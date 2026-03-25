
##==============================================================================
##
##  Project title:
##
##  Description of what the code does.
##
## 
##  Author: Benjamin Schlüter
##  Date: Month Year
##==============================================================================
##
##  Notes
## ------
## 1. 
## 2.
## 
##==============================================================================

rm(list = ls())




## LOAD PACKAGES ---------------------------------------------------------------

# Install/load packages
packages <- c("tidyverse", "here")
for(p in packages){
        if(!require(p,character.only = TRUE)) install.packages(p)
        library(p,character.only = TRUE)
}



## FUNCTIONS -------------------------------------------------------------------


# source(here("codes", "functions", "fct_name.R"))



## LOAD DATA -------------------------------------------------------------------

df <- readRDS(here("inputs", 
                   "data_private", 
                   "data_name.file_type")) 



## KEY DIMENSIONS OF CODE -----------------------------------------------------



## CLEAN DATA ------------------------------------------------------------------



## SECTION 1 ===================================================================


## SECTION 2 ===================================================================



