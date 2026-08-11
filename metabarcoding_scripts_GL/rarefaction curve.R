# Grace Lewin
# jul 21 2025

# make a rarefaction curve to determine sample size for each species

library(tidyverse) 
library(here)
library(janitor)
library(SpadeR)

###### IMPORT DATA

# import presence absence data (common name)
gaviota_pres_abs <- read_csv(here("data", "grouped_prey_sites_host.csv"))

###### TRANSPOSE PRESENCE/ABSENCE DATAFRAME

#transpose presence/absence dataframe and set the column names as Sample ID
gav_pres_abs_t <- data.frame(t(gaviota_pres_abs[ , -1]))

#and set the column names as Sample ID
colnames(gav_pres_abs_t) <- gaviota_pres_abs$sample_id

### using An's vegan cheatsheet

# libraries
library(tidyverse)
library(vegan)
library(ggvegan)

# data
# bird communities
# birds <- read_csv(here::here("data", "bird-comm.csv")) %>% 
#   column_to_rownames("site")
# 
# # environmental variables
# env <- read_csv(here::here("data", "env-var.csv"))

# set up a "metadata" frame - will be useful for plotting later!
site_type <- env %>% 
  select(site, landtype)

