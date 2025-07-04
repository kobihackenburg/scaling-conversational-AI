
# Master script for reproducing all figures and tables

# Libraries
library(tidyverse)
library(metafor)
library(broom)
library(estimatr)
library(ggrepel)
library(cowplot)
library(tidybayes)
library(brms)
library(mgcv)
library(modelr)
library(ggdist)
library(kableExtra)
library(patchwork)
library(readxl)
library(showtext)
library(sysfonts)
library(ggnewscale)
set.seed(42)

# List scripts
r_files <- 
  list.files("analysis_code/") %>% 
  purrr::discard(~str_detect(.x, "function|random_forest"))

# Run scripts
# Approximate run time on a 2022 Macbook pro: 30 minutes
t0_base <- Sys.time()
imap(r_files, 
     function(.x, .y) {
       source(paste0("analysis_code/", .x))
       print(paste0("**** R script ", .y, " of ", length(r_files), " complete: ", r_files[.y], " ****"))
       Sys.sleep(5)
     })
Sys.time() - t0_base

# Run random forest analysis separately to avoid package conflicts breaking things above

# Libraries
library(fixest)
library(tidymodels)

# Run
# Approximate run time on a 2022 Macbook pro: 5 minutes
source("analysis_code/8_random_forest_analysis.R")

