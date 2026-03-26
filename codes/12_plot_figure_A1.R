
##==============================================================================
##
##  Project title: Modeling of childlessness in U.S
##
##  This code generates figure A1 of manuscript. 
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

df.fit.race <- readRDS(
        here(
                "outputs",
                "data",
                "df_fit_totpooling_5ycohort_race_janoschek.rds"
        )
)




## ----- FIGURE A1 -------------------------------------------------------------

df.fit.race %>% 
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
                   group = race_eth,
                   col = race_eth,
                   fill = race_eth,
                   linetype = race_eth,
                   ymin = .upper, 
                   ymax = .lower
        )) +
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
        scale_linetype_manual(values = c("NH-Black" = "dashed",
                                         "Hispanic" = "solid",
                                         "NH-White" = "longdash")
        ) +
        scale_color_manual(values = c("NH-Black" = "darkorange",
                                      "Hispanic" = "deepskyblue3",
                                      "NH-White" = "darkgray")
        ) +
        scale_fill_manual(values = c("NH-Black" = "darkorange",
                                     "Hispanic" = "deepskyblue3",
                                     "NH-White" = "darkgray")
        ) +
        scale_shape_discrete(na.translate = F) + 
        labs(y = "Childless women (%)",
             x = "Age") 

ggsave(
        filename = here(
                "outputs",
                "figures",
                "figA1.pdf"
        ),
        width = 12, height = 9
)
ggsave(
        filename = here(
                "outputs",
                "figures",
                "figA1.jpg"
        ),
        width = 12, height = 9
)
