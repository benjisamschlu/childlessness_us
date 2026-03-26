
##==============================================================================
##
##  Project title: Modeling of childlessness in U.S
##
##  This code downloads and tidy NSFG data by race/ethnicity and education.
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
library(rstan)
library(gridExtra)





## ===== LOAD & TIDY NSFG DATA =================================================

# Different years leads to different ways of downloading data

## ===== LOAD DATA FROM 1995

file_name <- "FemRespData.dat"
# Url to download data
dat_url <- paste0(
        "https://ftp.cdc.gov/pub/Health_Statistics/NCHS/Datasets/NSFG/",
        "1995",
        file_name
)
sas_positions <-
        data.frame(
                # Rename panel into cluster for harmonization
                "varname" = c("drop1", "age", "drop2", "hieduc", "drop3","parity", "drop4", "hisprace","drop5", "poverty", "drop6", "strata", "cluster", "wgt", "drop7"),
                "width" = c(10, 2, 10879, 2, 396, 2, 1018, 1, 4, 3, 29, 2, 1, 10, 705),
                "column_types" = c("c", "d", "c", "d", "c", "d", "c", "d", "c", "d", "c", "d", "d", "d", "c") 
        )
# Load data
nsfg_tbl <-
        read_fwf(
                dat_url ,
                fwf_widths( 
                        abs( sas_positions[ , 'width' ] ), 
                        col_names = sas_positions[ , 'varname' ] 
                ) ,
                col_types = paste0( sas_positions[ , 'column_types' ] , collapse = "" ) ,
                na = c( "" , "." )
        )
data.nsfg.1995 <- data.frame( nsfg_tbl ) |> 
        dplyr::select(
                -starts_with("drop")
        ) |> 
        rename(
                "edu" = hieduc,
                "race_eth" = hisprace,
                "income" = poverty
        ) |> 
        filter(
                # Remove age that shouldn't be surveyed
                age >= 15,
                age < 45
        ) |> 
        mutate(
                # Allows binding with other periods
                cluster = as.character(cluster),
                # Var for period
                period = "1995"
        ) 

## ===== LOAD DATA FROM 2002-2019

# Code taken from
# https://asdfree.com/national-survey-of-family-growth-nsfg.html

dwld.period.2002_19 <- c("2002", "2006_2010", 
                         "2011_2013", "2013_2015", "2015_2017", "2017_2019")

# Load data from period 2002-2019
# Some warning() due to parse.SAScii() but checked and all good.
list.data.nsfg.2002_19 <- lapply(dwld.period.2002_19,
                                 function(p) {
                                         
                                         # File name changes over the years
                                         if (p == "2002") {
                                                 file_name <- "FemResp.dat"
                                         } else if (p == "2006_2010") { 
                                                 file_name <- "_FemResp.dat"
                                         } else {
                                                 file_name <- "_FemRespData.dat"
                                         }
                                         
                                         # Url to download data
                                         dat_url <- paste0(
                                                 "https://ftp.cdc.gov/pub/Health_Statistics/NCHS/Datasets/NSFG/",
                                                 p,
                                                 file_name
                                         )
                                         
                                         # 2002 has a different format and no sas file (.sps instead)
                                         if (p  == "2002") {
                                                 
                                                 sas_url <-
                                                         file.path(dirname(dat_url) , 
                                                                   paste0("spss/",
                                                                          p,
                                                                          "FemRespSetup.sps") )
                                                 sas_positions <- asciiSetupReader::parse_setup(sas_url)$setup
                                                 sas_positions <- 
                                                         sas_positions |> 
                                                         mutate(
                                                                 width = (end - begin) + 1,
                                                                 divisor = 1,
                                                                 varname = tolower(column_number),
                                                                 # Only considers variable used later
                                                                 char = ifelse(varname %in% c("age_r", "hisprace", "hieduc", "poverty","parity", "secu", "sest", "finalwgt") , FALSE, TRUE),
                                                                 column_types = ifelse(char, "c", "d")
                                                         ) |> 
                                                         dplyr::select(varname, width, char, divisor, column_types)
                                                 
                                         } else {
                                                 
                                                 if (p == "2006_2010") { 
                                                         
                                                         sas_positions <- 
                                                                 parse.SAScii(here("inputs", "data", "2006_2010_FemRespSetup.sas"))
                                                         
                                                 } else {
                                                         sas_url <-
                                                                 file.path(dirname(dat_url) , 
                                                                           paste0("sas/",
                                                                                  p,
                                                                                  "_FemRespSetup.sas") )
                                                         sas_positions <-
                                                                 parse.SAScii( sas_url )
                                                         
                                                 }
                                                 
                                                 sas_positions[ , 'varname' ] <-
                                                         tolower( sas_positions[ , 'varname' ] )
                                                 
                                                 sas_positions[ , 'column_types' ] <-
                                                         ifelse( sas_positions[ , 'char' ] , "c" , "d" )
                                                 
                                         }
                                         
                                         # Load data
                                         nsfg_tbl <-
                                                 read_fwf(
                                                         dat_url ,
                                                         fwf_widths( 
                                                                 abs( sas_positions[ , 'width' ] ) , 
                                                                 col_names = sas_positions[ , 'varname' ] 
                                                         ) ,
                                                         col_types = paste0( sas_positions[ , 'column_types' ] , collapse = "" ) ,
                                                         na = c( "" , "." )
                                                 )
                                         
                                         # Different race/ethnicity varname according to the period
                                         if (p  == "2002") {
                                                 secu <- "secu_r"
                                                 raceeth <- "hisprace"
                                         } else { 
                                                 secu <- "secu"
                                                 raceeth <- "hisprace2"
                                         }
                                         # Different weight names according to the period
                                         if (p == "2002") {
                                                 wgt_var <- "finalwgt"
                                         } else if (p == "2006_2010") {
                                                 wgt_var <- "wgtq1q16"
                                         } else { 
                                                 wgt_var <- paste0("wgt", p)
                                         }
                                         
                                         # Different age bounds according to the period
                                         if (p %in% c("2015_2017", "2017_2019")) {
                                                 age_bnd <- 50
                                         } else { 
                                                 age_bnd <- 45
                                         }
                                         
                                         # Select variables of interest
                                         nsfg_df <- data.frame( nsfg_tbl ) |> 
                                                 dplyr::select(
                                                         age = age_r, 
                                                         race_eth = !!sym(raceeth),
                                                         edu = hieduc,
                                                         income = poverty,
                                                         parity, 
                                                         cluster = !!sym(secu), 
                                                         strata = sest, 
                                                         wgt = !!sym(wgt_var)
                                                 ) |> 
                                                 filter(
                                                         # Remove age that shouldn't be surveyed
                                                         age < age_bnd
                                                 ) |> 
                                                 mutate(
                                                         # Var for period
                                                         period = p
                                                 )
                                 }
)
data.nsfg.2002_19 <- do.call("rbind", list.data.nsfg.2002_19)

# Last wave of NSFG subject to critisicm, not used in current analysis.

## ===== MERGE ALL PERIODS AND TIDY 

data.nsfg.2002_23 <- 
        bind_rows(
                data.nsfg.1995,
                data.nsfg.2002_19
        ) |> 
        mutate(
                # Prop mother
                p = ifelse(parity == 0, 0, 1),
                year = case_when(
                        period == "1995" ~ 1995,
                        period == "2002" ~ 2002,
                        period == "2006_2010" ~ 2008,
                        period == "2011_2013" ~ 2012,
                        period == "2013_2015" ~ 2014,
                        period == "2015_2017" ~ 2016,
                        period == "2017_2019" ~ 2018,
                        period == "2022_2023" ~ 2023
                ),
                cohort = year - age,
                cohort5 = cut(cohort,
                              breaks = seq(1930, 2010, 5),
                              labels =seq(1930, 2009, 5),
                              right = FALSE),
                # Name abbridged race
                race_eth = case_when(
                        race_eth == 1 ~ "Hispanic",
                        race_eth == 2 ~ "NH-White",
                        race_eth == 3 ~ "NH-Black",
                        race_eth == 4 ~ "NH-Other"
                ),
                edu = case_when(
                        edu < 9 ~ "Less than high school",
                        edu == 9 ~ "High school degree or equivalent",
                        edu == 10 ~ "Some College",
                        edu >= 11 ~ "At least four year College Degree",
                        TRUE ~ NA
                ),
                income = case_when(
                        income < 100 ~ "Poor",
                        income >=100 & income < 200 ~ "Near-poor",
                        income >= 200 & income < 300 ~ "Lower-middle",
                        income >= 300 ~ "Middle/high",
                        TRUE ~ NA
                )
        )



# Complex survey design
nsfg_design <- 
        svydesign( 
                id = ~ cluster, 
                strata = ~ strata, 
                data = data.nsfg.2002_23, 
                weights = ~ wgt, 
                nest = TRUE 
        )

# Weighted motherhood prop. by age AND 5y cohort at US level
df.nsfg.p.5y.cohort <- tibble(
        svyby( ~ p , ~ cohort5 + age, nsfg_design , svymean )
) |>
        mutate(
                l95 = p - (1.96 * se),
                u95 = p + (1.96 *se)
        ) |>
        arrange(
                cohort5, age
        )

# Weighted motherhood prop. by age, edu, AND 5y COHORT
df.nsfg.p.5y.cohort.edu <- tibble(
        svyby( ~ p , ~ cohort5 + edu + age, nsfg_design , svymean )
) |> 
        mutate(
                l95 = p - (1.96 * se),
                u95 = p + (1.96 *se)
        ) |> 
        arrange(
                cohort5, edu, age
        )

# Weighted motherhood prop. by age, race AND 5y COHORT
df.nsfg.p.5y.cohort.race <- tibble(
        svyby( ~ p , ~ cohort5 + race_eth + age, nsfg_design , svymean )
) |> 
        mutate(
                l95 = p - (1.96 * se),
                u95 = p + (1.96 *se)
        ) |> 
        arrange(
                cohort5, race_eth, age
        )


# Weighted motherhood prop. by age, race, edu, AND 5y COHORT
df.nsfg.p.5y.cohort.race.edu <- tibble(
        svyby( ~ p , ~ cohort5 + race_eth + edu + age, nsfg_design , svymean )
) |> 
        mutate(
                l95 = p - (1.96 * se),
                u95 = p + (1.96 *se)
        ) |> 
        arrange(
                cohort5, race_eth, edu, age
        )





## ===== SAVE DATA =============================================================

saveRDS(
        df.nsfg.p.5y.cohort,
        here(
                "outputs",
                "data",
                "df_nsfg_female_5y_cohort.rds"
        ),
        compress = "xz"
)

saveRDS(
        df.nsfg.p.5y.cohort.edu,
        here(
                "outputs",
                "data",
                "df_nsfg_female_5y_cohort_edu.rds"
        ),
        compress = "xz"
)

saveRDS(
        df.nsfg.p.5y.cohort.race,
        here(
                "outputs",
                "data",
                "df_nsfg_female_5y_cohort_race.rds"
        ),
        compress = "xz"
)

saveRDS(
        df.nsfg.p.5y.cohort.race.edu,
        here(
                "outputs",
                "data",
                "df_nsfg_female_5y_cohort_race_edu.rds"
        ),
        compress = "xz"
)

