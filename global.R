required_cran <- c(
   "bslib", "dplyr", "shiny", "shinyjs", "shinydashboard",
   "shinybusy", "tools", "utils", "zip", "vcfR", "janitor", "tibble", 
   "readr", "readxl", "tidyselect", "tidypopgen", "purrr", "stats", "tidyr",
   "adegenet", "poppr", "plyr", "QurvE", "ggplot2", "forcats", "ggrepel",
   "ade4", "pegas", "hierfstat", "openxlsx", "RColorBrewer", "BiocManager", 
   "tinytex", "seqinr", "ape",   "phangorn", "miscTools", "stringr", 
   "gridExtra", "BarcodingR", "data.table", "waiter"
)

required_bioc <- c(
   "Biostrings", "pwalign", "msa", "DECIPHER", "ggtree"
)

required_git <- c("CshlSiepelLab/RPHAST", "sa-lee/starmie")

git_package_names <- c("rphast", "starmie")

install_missing_cran_packages <- function(package){
   missing_packages <- package[!(package %in% installed.packages()[, "Package"])]
   if (length(missing_packages) > 0){
      install.packages(missing_packages, dependencies = TRUE, repos = "https://cloud.r-project.org")
   }
}

install_missing_bioc_packages <- function(package){
   if (!requireNamespace("BiocManager", quietly = TRUE)){
      install.packages("BiocManager", repos = "https://cloud.r-project.org")
   }
   missing_packages <- package[!(package %in% installed.packages()[, "Package"])]
   if (length(missing_packages) > 0){
      BiocManager::install(missing_packages, update = FALSE, ask = FALSE)
   }
}

install_missing_git_packages <- function(repos, package){
   missing_packages <- repos[!(package %in% installed.packages()[, "Package"])]
   if (length(missing_packages) > 0){
      if (!requireNamespace("remotes", quietly = TRUE)){
         install.packages("remotes", repos = "https://cloud.r-project.org")
      }
      remotes::install_github(missing_packages, upgrade = "never", dependencies = TRUE)
   }
}

install_missing_cran_packages(required_cran)
install_missing_bioc_packages(required_bioc)
install_missing_git_packages(required_git, git_package_names)
lapply(c(required_cran, required_bioc, git_package_names), library, character.only = TRUE)


devtools::source_url("https://raw.githubusercontent.com/Tom-Jenkins/utility_scripts/master/TJ_genind2genepop_function.R")

plink_path <- "plink/plink.exe"
plink2_path <- "plink/plink2.exe"

bcftools_path <- Sys.which("bcftools")
if (bcftools_path == "") bcftools_path <- "/usr/local/bin/bcftools"



