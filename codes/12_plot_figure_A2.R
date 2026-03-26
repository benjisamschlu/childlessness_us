
##==============================================================================
##
##  Project title: Modeling of childlessness in U.S
##
##  This code generates figure A2 of manuscript. 
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

df.validation <- readRDS(
        here(
                "outputs",
                "data",
                "df_validation_hfd.rds"
        )
)





## ----- FIGURE A2 -------------------------------------------------------------

df.validation |> 
        ggplot(aes(x = age,
                   y = phi,
                   ymin = .upper, 
                   ymax = .lower,
                   group = type,
                   linetype = type
        )) +
        geom_ribbon(col = NA,
                    alpha = .3) +
        geom_line(linewidth = 1) +
        theme_bw() +
        theme(legend.position = "bottom",
                legend.direction = "horizontal",
                legend.title = element_blank(),
                legend.text = element_text(size = 14),
                panel.border = element_blank(),
                axis.line = element_line(colour = "black"),
                axis.text = element_text(size = 13),
                axis.title = element_text(size = 14)) +
        scale_y_continuous(limits = c(0, 100)) +
        scale_x_continuous(limits = c(20, 45),
                           breaks = seq(20, 50, 10)) +
        labs(y = "Childless women (%)",
             x = "Age")

ggsave(
        filename = here(
                "outputs",
                "figures",
                "figA2.pdf"
        ),
        width = 12, height = 9
)
ggsave(
        filename = here(
                "outputs",
                "figures",
                "figA2.jpg"
        ),
        width = 12, height = 9
)
