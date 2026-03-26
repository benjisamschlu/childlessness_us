
##==============================================================================
##
##  Project title: Modeling of childlessness in U.S
##
##  This code generates figure A3 of manuscript. 
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


df.fit.race.nat <- readRDS(
        here(
                "outputs",
                "data",
                "df_fit_totpooling_5ycohort_race_nat_janoschek.rds"
        )
)





## ----- FIGURE A3 -------------------------------------------------------------

df.fit.race.nat |> 
        filter(
                cohort5 %in% seq(1940, 1980, 5),
                
        ) %>% 
        mutate(
                # Convert to %
                across(phi:.upper, ~ .x * 100),
                across(c(p, l95, u95), ~ (1 - .x) * 100),
                cohort5 = case_when(
                        cohort5 == 1965 ~ "Cohorts 1965-69",
                        cohort5 == 1970 ~ "Cohorts 1970-74",
                        cohort5 == 1975 ~ "Cohorts 1975-79",
                        cohort5 == 1980 ~ "Cohorts 1980-84",
                        cohort5 == 1985 ~ "Cohorts 1985-89"
                )
        ) |> 
        ggplot(aes(x = age,
                   y = phi,
                   ymin = .upper, 
                   ymax = .lower,
                   linetype = nat,
                   col = nat,
                   group = nat
        )) +
        facet_grid(race_eth ~ cohort5) +
        geom_ribbon(aes(fill = nat,),
                    col = NA,
                    alpha = .3) +
        geom_line(aes(col = nat))+
        geom_point(aes(y = p),
                   alpha = .3) +
        theme_bw() +
        theme(legend.position = "bottom",
              legend.direction = "horizontal",
              legend.title = element_blank(),
              legend.text = element_text(size = 14),
              strip.background = element_blank(),
              strip.text = element_text(face = "bold", size = 13),
              panel.border = element_blank(),
              #panel.grid.major = element_blank(), 
              #panel.grid.minor = element_blank(),
              panel.background = element_blank(), 
              axis.line = element_line(colour = "black"),
              axis.text = element_text(size = 13),
              axis.title = element_text(size = 14)) +
        scale_color_manual(values = c("Native-Born" = "blue4",
                                      #"2015" = "deepskyblue4",
                                      "Foreign-Born" = "deepskyblue")) +
        scale_fill_manual(values = c("Native-Born" = "blue4",
                                     #"2015" = "deepskyblue4",
                                     "Foreign-Born" = "deepskyblue")) +
        scale_y_continuous(limits = c(0, 100)) +
        scale_x_continuous(limits = c(20, 45),
                           breaks = seq(20, 50, 10)) +
        scale_shape_discrete(na.translate = F) + 
        labs(y = "Childless women (%)",
             x = "Age") 

ggsave(
        filename = here(
                "outputs",
                "figures",
                "figA3.pdf"
        ),
        width = 12, height = 9
)
ggsave(
        filename = here(
                "outputs",
                "figures",
                "figA3.jpg"
        ),
        width = 12, height = 9
)
