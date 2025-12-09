library(shiny)
library(shinyjs)
library(ggplot2)
library(bslib)
library(dplyr)
library(waiter)
devtools::source_url("https://raw.githubusercontent.com/Tom-Jenkins/utility_scripts/master/TJ_genind2genepop_function.R")
#devtools::source_url("https://raw.githubusercontent.com/Tom-Jenkins/utility_scripts/master/TJ_genind2structure_function.R")
source("functions.R", local = TRUE)
useShinyjs()

# load plink
#plink_path <- Sys.which("plink")
#if (plink_path == "") plink_path <- "/usr/local/bin/plink"
plink_path <- "plink/plink.exe"

# double check
bcftools_path <- Sys.which("bcftools")
if (bcftools_path == "") bcftools_path <- "/usr/local/bin/bcftools"


