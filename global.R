library(shiny)
library(shinyjs)
library(ggplot2)
library(bslib)
library(dplyr)
library(waiter)

if (!requireNamespace("rphast", quietly = TRUE)) {
   devtools::install_github("CshlSiepelLab/RPHAST")
}

library(rphast)

if (!requireNamespace("data.table", quietly = TRUE)) {
   install.packages("data.table")
}

library(data.table)

devtools::source_url("https://raw.githubusercontent.com/Tom-Jenkins/utility_scripts/master/TJ_genind2genepop_function.R")


plink_path <- "plink/plink.exe"
plink2_path <- "plink/plink2.exe"

bcftools_path <- Sys.which("bcftools")
if (bcftools_path == "") bcftools_path <- "/usr/local/bin/bcftools"


