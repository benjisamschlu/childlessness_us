
##==============================================================================
##
##  Project title: Modeling of childlessness in U.S
##
##  This code generates figure 2 of manuscript. 
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




## ----- FIGURE 2 --------------------------------------------------------------

bind_rows(
        df.fit.edu,
        df.fit
) |> 
        filter(
                cohort5 == 1980
        ) %>%
        mutate(
                # Convert to %
                across(phi:.upper, ~ .x * 100),
                across(c(p, l95, u95), ~ (1 - .x) * 100)
        ) |> 
        ggplot(aes(x = age,
                   y = phi,
                   ymin = .upper, 
                   ymax = .lower,
                   fill = edu,
                   col = edu,
                   linetype = edu,
                   group = edu
        )) +
        facet_wrap(~ race_eth) +
        geom_ribbon(col = NA,
                    alpha = .3) +
        geom_line(linewidth = 1)+
        geom_point(aes(y = p, shape = svy),
                   alpha = .4) +
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
        scale_y_continuous(limits = c(0, 100)) +
        scale_x_continuous(limits = c(20, 45),
                           breaks = seq(20, 50, 10)) +
        scale_shape_discrete(na.translate = F) + 
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
             x = "Age") +
        # Create a 2 rows legend
        guides(shape=guide_legend(nrow=2,byrow=TRUE),
               linetype=guide_legend(nrow=2,byrow=TRUE))

ggsave(
        filename = here(
                "outputs",
                "figures",
                "fig2.pdf"
        ),
        width = 12, height = 9
)
ggsave(
        filename = here(
                "outputs",
                "figures",
                "fig2.jpg"
        ),
        width = 12, height = 9
)


