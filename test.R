# snpR for Fst and HWE calculations

## First convert to snpR dataclass
## Accepts filepath of vcf or vcf.gz, genepop, and PLINK files
##library(remotes)
##remotes::install_github("hemstrow/snpR")
test = snpR::import.snpR.data("www/sample_hgdp.vcf")
# add sample.meta by loading in files 
# upload file with SNP info and metadata?

## Fst Calc
fst = snpR::calc_pairwise_fst(test, "Pop")


# Will just merge this with existing pop stat calculation
# assuming it was converted to genind

snpR_object <- function(csv_file){
   library(dplyr)
   # clean
   csv_file <- lapply(csv_file2, function(x) gsub("/", "", x, fixed = TRUE))
   csv_file <- as.data.frame(csv_file)
   
   # SHORTEN THIS
   csv_file <- lapply(csv_file, function(x) gsub("-", "", x, fixed = TRUE))
   csv_file <- as.data.frame(csv_file)
   csv_file <- lapply(csv_file, function(x) gsub(".", "", x, fixed = TRUE))
   csv_file <- as.data.frame(csv_file)
   csv_file <- lapply(csv_file, function(x) gsub("_", "", x, fixed = TRUE))
   csv_file <- as.data.frame(csv_file)
   csv_file <- lapply(csv_file, function(x) gsub(" ", "", x, fixed = TRUE))
   csv_file <- as.data.frame(csv_file)
   csv_file[,1] <- trimws(csv_file[,1])

   # Transpose data
   # Remove pop column
   file_raw = csv_file[,-2]
   file = data.frame(t(file_raw))
   colnames(file) = file[1,]
   file_gt = file[-1,]
   file_snps = data.frame("N", rownames(file_gt))
   file_snps <- file_snps %>%
      rename(Chr = 1, SNPs = 2)
   
   file_meta = data.frame(
      pop = csv_file[,1],
      fam = csv_file[,2],
      stringsAsFactors = FALSE
   )
   
   snpR_obj = snpR::import.snpR.data(file_gt, snp.meta = file_snps, sample.meta = file_meta)
   
}

calculate_basic_stats <- function(csv_file){
   
   # Transpose csv_file
   genos = csv_file[,-c(1,2)]
   snp_
   
}


