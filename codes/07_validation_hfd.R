
##==============================================================================
##
##  Project title: Modeling of childlessness in U.S
##
##  This code compares our childlessness estimates with what can be derived
##  from HMD data.
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
library(HMDHFDplus)





## ----- LOAD DATA -------------------------------------------------------------

df.fit.5ycohort <- readRDS(
        here(
                "outputs",
                "data",
                "df_fit_totpooling_5ycohort_janoschek.rds"
        )
)

# Exposure by parity in US from Human Fertility Database.
# Look at cohort 1950-54
df.hfd <- readHFDweb("USA","exposRRpa") |> 
        mutate(
                across(E0x:E4px, ~ as.numeric(.x)),
                cohort = Year - Age,
                cohort5 = cut(cohort,
                              breaks = seq(1915, 2025, 5),
                              labels =seq(1915, 2020, 5),
                              right = FALSE)
) |> 
        filter(
                Age >= 22,
                Age <= 45,
                cohort5 %in% 1950
        ) %>% 
        # Get percentage of women in each parity
        rowwise() %>%
        mutate(
                E = sum(across(starts_with("E")), na.rm = T)
        ) |> 
        ungroup() %>% 
        summarise(
                .by = c(Age, cohort5),
                
                E = sum(E),
                E0x = sum(E0x)
        ) |> 
        mutate(
                phi = (E0x / E) * 100
        )


df.validation <- bind_rows(
        
        df.fit.5ycohort |> 
                filter(
                        cohort5 == 1950
                ) %>% 
                mutate(
                        across(phi:.upper, ~ .x * 100),
                        cohort5 = factor(cohort5),
                        type = "Bayesian parametric model"
                ) |> 
                dplyr::select(age, cohort5, phi, .lower, .upper, type),
        df.hfd |> 
                mutate(
                        type = "Human Fertility Database"
                ) |> 
                dplyr::select(age = Age, cohort5, phi, type)
)

# Save df
saveRDS(
        df.validation,
        here(
                "outputs",
                "data",
                "df_validation_hfd.rds"
        ),
        compress = "xz"
)

