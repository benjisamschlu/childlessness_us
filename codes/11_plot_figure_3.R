
##==============================================================================
##
##  Project title: Modeling of childlessness in U.S
##
##  This code generates figure 3 of manuscript. 
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

df.fit.edu <- readRDS(
        here(
                "outputs",
                "data",
                "df_fit_totpooling_5ycohort_edu_janoschek.rds"
        )
) |> 
        mutate(
                race_eth = "Total"
        )

df.fit <- readRDS(
        here(
                "outputs",
                "data",
                "df_fit_totpooling_5ycohort_race_edu_janoschek.rds"
        )
)




## ----- FIGURE 3 --------------------------------------------------------------

bind_rows(
        df.fit.edu,
        df.fit
) |> 
        filter(
                (age == 30) | (age == 45 & cohort5 %in% seq(1950, 1980, 5))
        ) %>%
        mutate(
                # Convert to %
                across(phi:.upper, ~ .x * 100),
                age = case_when(
                        age == 30 ~ "30 years old",
                        age == 45 ~ "45 years old"
                )
        ) |> 
        ggplot(aes(x = cohort5,
                   y = phi,
                   ymin = .upper, 
                   ymax = .lower,
                   fill = edu,
                   col = edu,
                   linetype = edu,
                   group = edu
        )) +
        facet_grid(age ~ race_eth,
                   scales = "free_y") +
        geom_ribbon(col = NA,
                    alpha = .2) +
        geom_line(linewidth = 1)+
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
        scale_linetype_manual(values = c("Less than high school" = "dotdash",
                                         "High school degree or equivalent" = "dotted",
                                         "Some College" = "solid",
                                         "At least four year College Degree" = "dashed")
        ) +
        scale_color_manual(values = c("Less than high school" = "darkorange",
                                      "High school degree or equivalent" = "chartreuse4",
                                      "Some College" = "deepskyblue3",
                                      "At least four year College Degree" = "red4")
        ) +
        scale_fill_manual(values = c("Less than high school" = "darkorange",
                                     "High school degree or equivalent" = "chartreuse4",
                                     "Some College" = "deepskyblue3",
                                     "At least four year College Degree" = "red4")
        ) +
        labs(y = "Childless women (%)",
             x = "Five-years Cohort")    

ggsave(
        filename = here(
                "outputs",
                "figures",
                "fig3.pdf"
        ),
        width = 12, height = 9
)

ggsave(
        filename = here(
                "outputs",
                "figures",
                "fig3.jpg"
        ),
        width = 12, height = 9
)
