###
### Alison C. Ketz 4/15/2024
###
### Survival Analysis Moose in Alphabet Hills
###

###########################################################
### Preliminaries
###########################################################

rm(list = ls())

setwd("~/Documents/red_tree_voles/scripts/rtv_occu/02_timevary")

library(tidyverse)
library(sf)
library(terra)
library(splines)
library(MetBrewer)
library(gridExtra)
library(xtable)
library(nimble)
library(MCMCvis)
library(readxl)
library(coda)

###########################################################
### Load and Clean Data
###########################################################

source("02_load_clean_data.R")

###############################################################
### Format data for model fitting
### setting up preliminary constants
###############################################################

source("03_preliminaries.R")

###########################################################
### Run model
###########################################################

source("04_run_model_yr_occ.R")

###########################################################
### Post processing
###########################################################

source("05_post_process.R")
