
##==============================================================================
##
##  Project title: Modeling of childlessness in U.S
##
##  This code estimate proportion of childless mother by race/ethnicity and
##  nativity with a Janoshek curve using CPS data only. 
##
## 
##  Author: Benjamin-Samuel Schlüter
##  Date: March 2026
##==============================================================================
##
##  Notes
## ------
## 1. 
## 
##==============================================================================



## ----- LOAD PACKAGES ---------------------------------------------------------

# Install/load packages
library(tidyverse)
library(here)
library(rstan)
library(gridExtra)
library(tidybayes)





## ----- LOAD DATA -------------------------------------------------------------


# CPS by race and education, over 5y cohorts
df.cps.race.nat <- readRDS(
        here(
                "outputs",
                "data",
                "df_cps_female_5y_cohort_race_nat_corrected.rds"
        )
) 





## ----- TIDY DATA -------------------------------------------------------------

# Key dimensions in code
AGES <- 0:23
N.AGES <- length(AGES)
# NATIVITY NOT REALLY COLLECTED EARLIER
COHORTS <- seq(1965, 1985, 5)
N.COHORTS <- length(COHORTS)

df.stan <-
        df.cps.race.nat |> 
        filter(
                race_eth != "NH-Other",
                cohort5 %in% COHORTS,
                # Limit "wrong" group categorization in education
                age >= 22,
                age <= 45,
                # SE based on really few n leads to implausible SE 
                # and create issue in STAN
                p != 0,
                p < 0.99
        ) |> 
        mutate(
                logit_se = se / (p * (1 - p)), # Delta method
                logit_p = log(p / (1 - p)),
                # Create indexing var used in STAN to vectorize for 
                # group (race*edu), cohort, and age
                race_i = as.factor(race_eth) |> as.numeric(),
                # Year index starts at 1
                cohort5 = as.character(cohort5) |> as.numeric(),
                cohort_i = (cohort5 - (min(COHORTS) - 5)) / 5,
                # Age index: 1 is 22 (age[1]=0 corresponds to 22, age[2]=1 is 23yo)
                age_i = age - 21,
                # Create indexing var for education
                nat = factor(nat,
                             levels = c(
                                     "Native-Born",
                                     "Foreign-Born"
                             )),
                nat_i = as.numeric(nat),
                # Allows to index observed data later in code
                obs_i = 1
        ) |> 
        arrange(
                # !!! Order super important for STAN !!!
                # Has to match with indexing vars
                nat, race_eth, cohort5, age
        )





## ----- STAN: TS CHILDLESSNESS BY EDU & RACE ----------------------------------

# n.groups only consist of race_eth: only one education group
N.RACES <- length(unique(df.stan$race_eth))
N.NATS <- length(unique(df.stan$nat))
N.GROUPS <- N.RACES * N.NATS

# Create indexing for g
df.i.race.nat <- expand.grid(
        race = unique(df.stan$race_eth),
        nat = unique(df.stan$nat)
)

# Create indexing for phi in STAN (edu, race, cohort, age) 
df.i <- expand.grid(
        # !!!! Same order as order imposed above !!!!
        age_i = 1:N.AGES,
        cohort_i = 1:N.COHORTS,
        race_i = 1:N.RACES,
        nat_i = 1:N.NATS
) |> 
        mutate(
                # Create index for all combination of 
                # group, year, and t 
                i = row_number()
        )

# Index of each observed data for phi
obs_i <- df.i |> 
        left_join(
                df.stan |> 
                        dplyr::select(
                                nat_i, race_i, cohort_i, age_i, obs_i
                        ),
                by = c("nat_i", "race_i", "cohort_i", "age_i")
        ) |> 
        filter(
                !is.na(obs_i)
        ) |> 
        pull(i) 

# Index of education for each group
nat_g <- rep(unique(df.stan$nat_i), each = 3)

# Index of race for each group
race_g <- rep(unique(df.stan$race_i), times = length(unique(df.stan$nat_i)))

# STAN data
stan_data <- list(
        # Dimensions
        N = nrow(df.stan),
        A_all = N.AGES, # assume zero at 22 yo
        age = AGES, # assume zero at 22 yo s.t Janoschek(14)=0,
        T_all = N.COHORTS,
        G = N.GROUPS,
        obs_i = obs_i,
        # Careful !!
        # Structured as vector but order super important !!!
        # Needs to coincide with jano_ord in STAN (multiple to_vector() )
        logit_p = df.stan$logit_p,
        logit_se = df.stan$logit_se
)

# STAN set-up 
niter = 4000
nwarmup = niter/2
nchains = 4
nsim = (niter-nwarmup)*nchains
options(mc.cores = parallel::detectCores()-1)
pars <- c("jano", "phi", "omega", "gamma", "delta", "alpha",
          paste0("sigma_", c("o",
                             "g",
                             "d",
                             "a"))
)

# Initial values for pars
stanInit = function() {
        
        list(
                omega = matrix( runif(N.COHORTS * N.GROUPS, 0.7, 1), N.COHORTS, N.GROUPS ),
                gamma = matrix( runif(N.COHORTS * N.GROUPS, 0.001, 0.10), N.COHORTS, N.GROUPS ), # initially lower 0.005
                delta = matrix( runif(N.COHORTS * N.GROUPS, 0, 3), N.COHORTS, N.GROUPS ),
                alpha = matrix( runif(N.COHORTS * N.GROUPS, 0, 0.5), N.COHORTS, N.GROUPS )
        )
}

# Fit model  
fit = stan(here("codes", "stan", "p0_janoschek_hier_var_rw_cohort.stan"),
           data = stan_data,
           pars  = pars,
           init = stanInit,
           include = TRUE,
           iter = niter,
           warmup = nwarmup,
           chains = nchains,
           control=list(adapt_delta  = 0.9, 
                        max_treedepth= 12)
)
# no warnings

# Estimated prop. 
df.fit <- 
        fit |> 
        tidybayes::spread_draws(phi[g, age, cohort]) |> 
        tidybayes::median_qi(.width = 0.95) |> 
        mutate(
                age = age + 21,
                cohort5 = 5*cohort + (min(COHORTS) - 5),
                race_eth = df.i.race.nat$race[g],
                nat = df.i.race.nat$nat[g]
        ) |> 
        left_join(
                df.stan,
                by = c("cohort5", "race_eth", "nat", "age")
        )

# Save df
saveRDS(
        df.fit,
        here(
                "outputs",
                "data",
                "df_fit_totpooling_5ycohort_race_nat_janoschek.rds"
        ),
        compress = "xz"
)

