
##==============================================================================
##
##  Project title: Modeling of childlessness in U.S
##
##  This code estimate proportion of childless mother by education level
##  with a Janoshek curve using CPS and NSFG data. 
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
df.cps <- readRDS(
        here(
                "outputs",
                "data",
                "df_cps_female_5y_cohort_edu_corrected.rds"
        )
) |> 
        mutate(
                svy = "CPS"
        )

# NSFG by race and education, over 5y cohorts
df.nsfg <- readRDS(
        here(
                "outputs",
                "data",
                "df_nsfg_female_5y_cohort_edu.rds"
        )
) |> 
        filter(
                # Cohorts for which we were able to obtain
                # sufficient data
                cohort5 %in% seq(1950, 1995, 5)  
        ) |> 
        mutate(
                svy = "NSFG"
        )





## ----- TIDY DATA -------------------------------------------------------------

# Key dimensions in code
AGES <- 0:23
N.AGES <- length(AGES)
# POSSIBLE TO BROADEN TIME WINDOW
COHORTS <- seq(1950, 1995, 5)
N.COHORTS <- length(COHORTS)

df.stan <-
        bind_rows(
                df.cps,
                df.nsfg
        ) |> 
        filter(
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
                cohort5 = as.character(cohort5) |> as.numeric(),
                cohort_i = (cohort5 - (min(COHORTS) - 5)) / 5,
                # Age index: 1 is 21 (age[1]=0 corresponds to 21, age[2]=1 is 22yo)
                age_i = age - 21,
                # Create indexing var for education
                edu = factor(edu,
                             levels = c(
                                     "Less than high school",
                                     "High school degree or equivalent",
                                     "Some College",
                                     "At least four year College Degree"
                             )),
                edu_i = as.numeric(edu),
                # Create indexing variable for survey
                svy = factor(svy,
                             levels = c(
                                     "CPS",
                                     "NSFG"
                             )),
                svy_i = as.numeric(svy),
                # Allows to index observed data later in code
                obs_i = 1
        ) |> 
        arrange(
                # !!! Order super important for STAN !!!
                # Has to match with indexing vars
                svy, edu, cohort5, age
        ) 





## ----- STAN: TS CHILDLESSNESS BY EDU & RACE ----------------------------------

# n.groups only consist of race_eth: only one education group
N.EDUS <- length(unique(df.stan$edu))
N.GROUPS <- N.EDUS

# Create indexing for g
df.i.race.edu <- expand.grid(
        edu = unique(df.stan$edu)
)

# Create indexing for phi in STAN (edu, race, cohort, age) 
df.i <- expand.grid(
        # !!!! Same order as order imposed above !!!!
        age_i = 1:N.AGES,
        cohort_i = 1:N.COHORTS,
        edu_i = 1:N.EDUS
) |> 
        mutate(
                # Create index for all combination of 
                # group, year, and t 
                i = row_number()
        )

# Index of each observed data in each survey for phi
obs_i <- lapply(c("CPS", "NSFG"), function(s) {
        
        df.i |> 
                left_join(
                        df.stan |> 
                                filter(
                                        svy == s
                                ) |> 
                                dplyr::select(
                                        edu_i, cohort_i, age_i, obs_i
                                ),
                        by = c("edu_i", "cohort_i", "age_i")
                ) |> 
                filter(
                        !is.na(obs_i)
                ) |> 
                pull(i)
})
obs_i <- unlist(obs_i)

# Index of education for each group
edu_g <- unique(df.stan$edu_i)

# STAN data
stan_data <- list(
        # Dimensions
        N = nrow(df.stan),
        A_all = N.AGES, 
        age = AGES, 
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
                gamma = matrix( runif(N.COHORTS * N.GROUPS, 0.001, 0.10), N.COHORTS, N.GROUPS ),
                delta = matrix( runif(N.COHORTS * N.GROUPS, 0, 3), N.COHORTS, N.GROUPS ), # initially lower bound was 1
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
                edu = df.i.race.edu$edu[g]
        ) |> 
        full_join(
                df.stan,
                by = c("cohort5", "edu", "age")
        )
# Save df
saveRDS(
        df.fit,
        here(
                "outputs",
                "data",
                "df_fit_totpooling_5ycohort_edu_janoschek.rds"
        ),
        compress = "xz"
)

