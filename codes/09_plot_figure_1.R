
##==============================================================================
##
##  Project title: Modeling of childlessness in U.S
##
##  This code generates figure 1 of manuscript. 
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





## ----- LOAD DATA -------------------------------------------------------------

df.fit.5ycohort <- readRDS(
        here(
                "outputs",
                "data",
                "df_fit_totpooling_5ycohort_janoschek.rds"
        )
)





## ----- FIGURE 1 --------------------------------------------------------------

df.fit.5ycohort |> 
        filter(
                cohort5 %in% c(1950, 1980)
        ) %>% 
        mutate(
                # Convert to %
                across(phi:.upper, ~ .x * 100),
                across(c(p, l95, u95), ~ (1 - .x) * 100),
                cohort5 = factor(cohort5)
        ) |> 
        ggplot(aes(x = age,
                   y = phi,
                   ymin = .upper, 
                   ymax = .lower,
                   group = cohort5,
                   col = cohort5
        )) +
        geom_ribbon(col = NA,
                    alpha = .3) +
        geom_line(linewidth = 1) +
        geom_point(aes(y = p, shape = svy),
                   alpha = .4) +
        theme_bw() +
        theme(#legend.position = c(0.75, 0.75),
                legend.position = "none",
                legend.direction = "horizontal",
                legend.title = element_blank(),
                legend.text = element_text(size = 14),
                panel.border = element_blank(),
                axis.line = element_line(colour = "black"),
                axis.text = element_text(size = 13),
                axis.title = element_text(size = 14)) +
        scale_color_manual(values = c("1950" = "blue4",
                                      #"2015" = "deepskyblue4",
                                      "1980" = "deepskyblue")) +
        scale_fill_manual(values = c("1950" = "blue4",
                                     #"2015" = "deepskyblue4",
                                     "1980" = "deepskyblue")) +
        scale_y_continuous(limits = c(0, 100)) +
        scale_x_continuous(limits = c(20, 45),
                           breaks = seq(20, 50, 10)) +
        labs(y = "Childless women (%)",
             x = "Age")

ggsave(
        filename = here(
                "outputs",
                "figures",
                "fig1.pdf"
        ),
        width = 12, height = 9
)
ggsave(
        filename = here(
                "outputs",
                "figures",
                "fig1.jpg"
        ),
        width = 12, height = 9
)

