
##==============================================================================
##
##  Project title: Modeling of childlessness in U.S
##
##  This code tidy the extracted CPS data set from IPUMS.
##
## 
##  Author: Benjamin-Samuel Schlüter
##  Date: March 2026
##==============================================================================
##
##  Notes
## ------
## 1. 
## 2. 
## 
##==============================================================================



## ===== LOAD PACKAGES =========================================================

library(tidyverse)
library(rvest)
library(here)
library(survey)
library(SAScii)
library(readr)
library(haven)





## ===== LOAD & TIDY CPS DATA ==================================================

# Years in which the fertility and mariage supplement has been collected
years.fert.sup <- c(1976, 1977, 1979,
                    1980:1984, 1986:1988,
                    seq(1990, 1994, 2), 1995, 1998, 
                    seq(2000, 2024, 2)) 
# Load CPS data (extract from IPUMS)
# read_csv allows to open compressed .gz 
data.cps <- read_csv(
        here(
                "inputs",
                "data", 
                "cps_00013.csv.gz"
        )
) |> 
        rename_all(
                .funs = tolower
        ) |> 
        # Keep useful variables
        dplyr::select(
                year, month, serial,pernum, # identification
                momloc, momloc2, poploc, poploc2, frever,# parenthood
                wtfinl, # weights,
                educ, # education attainment
                age, sex, race, hispan, nativity # demographic
        ) |> 
        filter(
                # Question on children only asked to women
                sex == 2,
                # Not all years have the fertility supplement
                year %in% years.fert.sup
        ) |> 
        mutate(
                # Create HH for survey design
                hhid = paste0(serial, year) |> as.numeric(),
                # Create abridged race 
                race_eth = case_when(
                        race == 100 & hispan == 0 ~ "NH-White",
                        race == 200 & hispan == 0 ~ "NH-Black",
                        !(race %in% c(100, 200)) & hispan == 0 ~ "NH-Other",
                        hispan != 0 ~ "Hispanic"
                ),
                # Create education attainment
                # https://murraylax.org/rtutorials/ipumsdata.html
                edu = case_when(
                        educ > 0 & educ < 73 ~ "Less than high school",
                        educ == 73 ~ "High school degree or equivalent",
                        educ > 73 & educ < 111 ~ "Some College",
                        # Merged Bachelor and Post-graduate
                        # P-G was a really small group
                        educ >= 111 & educ <= 125 ~ "At least four year College Degree",
                        TRUE ~ NA
                ),
                # Create nativity: native born vs foreign born
                nat = case_when(
                        nativity >= 1 & nativity <= 4 ~ "Native-Born",
                        nativity == 5 ~ "Foreign-Born",
                        TRUE ~ NA
                )
        ) 


# Data since before 2012 need to be corrected (see Census Bureau report)
# and don't consistently collect info on women: Subset women aged 15-45.
# Data since 2012 has new edit process (see Census Bureau report)
# and consistently collect info on women aged 15-50.
data.cps.2012_2024 <- 
        data.cps |>
        filter(
                year >= 2012,
                age >= 15,
                # change in sample universe
                case_when(
                        year < 2012 & age < 45 ~ TRUE,
                        year >= 2012 & age <= 50 ~ TRUE,
                        TRUE ~ FALSE
                ),
                # Remove when info on frever is not collected
                frever != 999
        ) |> 
        mutate(
                # Prop mother
                p = ifelse(frever == 0, 0, 1),
                # Categorical variable for counts with survey()
                mother = ifelse(frever == 0, "No", "Yes")
        )

# Correct the data before 2012
# Utilization of information from the household rosters both to allocate data 
# in cases of non-response, and to validate responses (see Census Bureau report).
data.cps.1976_2010 <- 
        data.cps |> 
        filter(
                
                year < 2012
        ) 

# Correct frever according to relation to child(ren)
correct.frever <- function(d, ...) {
        
        # Who is a mother
        find <- d$momloc != 0 | d$momloc2 != 0 
        # How many time are they listed parent
        n <- table(c(d$momloc[find], d$momloc2[find]))
        # Do not account for 0 (construction of "find" will always contain a 0)
        n <- tail(n, -1)
        id <- as.numeric(names(n))
        # Store
        d$frever_cor[d$pernum %in% id] <- n
        
        return(d)
}

# Apply function and get frever_cor using momloc
data.cps.cor.1976_2010 <- 
        data.cps.1976_2010 |> 
        mutate(
                # Never frever based on hh roster
                frever_cor = 0
        ) |> 
        group_by(
                year, month, serial
        ) |> 
        group_modify(correct.frever) |> 
        mutate(
                frever_merge = case_when(
                        # non-response so use frever_cor
                        frever == 999 ~ frever_cor,
                        # frever == 0 but possible that child(ren) recorded in hh
                        # otherwise frever_cor is also = 0
                        frever == 0 ~ frever_cor,
                        # frever is > 0 so use it (frever_cor might miss children
                        # that left the hh)
                        TRUE ~ frever
                )
        ) |> 
        ungroup() |> 
        mutate(
                # Prop mother
                p = ifelse(frever_merge == 0, 0, 1),
                # Categorical variable for counts with survey()
                mother = ifelse(frever_merge == 0, "No", "Yes")
        ) |> 
        filter(
                # Years 2008 and 2010 only interviewed women aged 15-44.
                # This step has to be done after applying the function otherwise
                # function doesn't work.
                age >= 15, 
                age < 45
        )

# Combine the two data frames replacing the uncorrected years
# with the corrected ones.
data.cps.cor <- 
        bind_rows(
                data.cps.2012_2024,
                data.cps.cor.1976_2010,
        ) |> 
        # Create cohort 
        mutate(
                cohort = year - age,
                cohort5 = cut(cohort,
                           breaks = seq(1930, 2010, 5),
                           labels =seq(1930, 2009, 5),
                           right = FALSE)
        )

# Clean environment
rm(data.cps)
rm(data.cps.2012_2024)
rm(data.cps.cor.1976_2010)

# Compute proportions of mother and SE accounting for weights.
# Complex survey design.
# One data set keeps NH-Other when modeling at national level
cps_design <- 
        svydesign( 
                id = ~ hhid , 
                data = data.cps.cor , 
                weights = ~ wtfinl
        )

# Weighted motherhood prop. by age, AND 5y COHORT
df.cps.p.5y.cohort.cor <- tibble(
        svyby( ~ p , ~ cohort5 + age, cps_design , svymean )
) |> 
        mutate(
                l95 = p - (1.96 * se),
                u95 = p + (1.96 *se)
        ) |> 
        arrange(
                cohort5, age
        )

# Weighted motherhood prop. by age, education AND 5y COHORT
df.cps.p.5y.cohort.edu.cor <- tibble(
        svyby( ~ p , ~ cohort5 + edu + age, cps_design , svymean )
) |> 
        mutate(
                l95 = p - (1.96 * se),
                u95 = p + (1.96 *se)
        ) |> 
        arrange(
                cohort5, edu, age
        )


# Weighted motherhood prop. by age, race_eth, AND 5y COHORT
df.cps.p.5y.cohort.race.cor <- tibble(
        svyby( ~ p , ~ cohort5 + race_eth + age, cps_design , svymean )
) |> 
        mutate(
                l95 = p - (1.96 * se),
                u95 = p + (1.96 *se)
        ) |> 
        arrange(
                cohort5, race_eth, age
        )

# Weighted motherhood prop. by age, race, edu, AND 5y COHORT
df.cps.p.5y.cohort.race.edu.cor <- tibble(
        svyby( ~ p , ~ cohort5 + race_eth + edu + age, cps_design , svymean )
) |> 
        mutate(
                l95 = p - (1.96 * se),
                u95 = p + (1.96 *se)
        ) |> 
        arrange(
                cohort5, race_eth, edu, age
        )

# Weighted motherhood prop. by age, race, nativity, AND 5y COHORT
df.cps.p.5y.cohort.race.nat.cor <- tibble(
        svyby( ~ p , ~ cohort5 + race_eth + nat + age, cps_design , svymean )
) |> 
        mutate(
                l95 = p - (1.96 * se),
                u95 = p + (1.96 *se)
        ) |> 
        arrange(
                cohort5, race_eth, nat, age
        )





## ===== SAVE DATA =============================================================

saveRDS(
        df.cps.p.5y.cohort.cor,
        here(
                "outputs",
                "data",
                "df_cps_female_5y_cohort_corrected.rds"
        ),
        compress = "xz"
)

saveRDS(
        df.cps.p.5y.cohort.edu.cor,
        here(
                "outputs",
                "data",
                "df_cps_female_5y_cohort_edu_corrected.rds"
        ),
        compress = "xz"
)

saveRDS(
        df.cps.p.5y.cohort.race.cor,
        here(
                "outputs",
                "data",
                "df_cps_female_5y_cohort_race_corrected.rds"
        ),
        compress = "xz"
)

saveRDS(
        df.cps.p.5y.cohort.race.edu.cor,
        here(
                "outputs",
                "data",
                "df_cps_female_5y_cohort_race_edu_corrected.rds"
        ),
        compress = "xz"
)

saveRDS(
        df.cps.p.5y.cohort.race.nat.cor,
        here(
                "outputs",
                "data",
                "df_cps_female_5y_cohort_race_nat_corrected.rds"
        ),
        compress = "xz"
)


