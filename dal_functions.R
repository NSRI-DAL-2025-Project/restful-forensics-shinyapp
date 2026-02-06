source("global.R")
#===========================
# UNPACK COMPRESSED FILES
#===========================

unpack_input_file <- function(files, output.dir = output.dir){
   
   if(!file.exists(files)){
      stop("File does not exist in the working directory")
   } else {
      if(tools::file_ext(files) == "zip"){
         utils::unzip(files, 
                      files = NULL, 
                      list = FALSE, 
                      overwrite = TRUE, 
                      exdir = file.path(output.dir, "unpacked"))
         
         data_path <- file.path(output.dir, "unpacked")
         data_files <- list.files(path = data_path, full.names = TRUE)
      } else if(tools::file_ext(files) == "tar"){ 
         
         untar(files, files = NULL, list = FALSE, exdir = file.path(output.dir, "unpacked"))
         
         new <- list.files(path = file.path(output.dir, "unpacked"), recursive = TRUE, full.names = TRUE)
         file.copy(from = new, to = file.path(output.dir, "unpacked"), overwrite = TRUE)
         
         data_path <- file.path(output.dir, "unpacked")
         data_files <- list.files(path = data_path, full.names = TRUE)
      } else {
         stop("Not a zipped file. Accepted are zipped (.zip) and tar (.tar) files")
      }
      
      return(list(data_path = data_path, data_files = data_files))
   }
}

#============================
# Convert to PLINK
# Description: Returns file path containing plink files
#============================

convert_to_plink <- function(input.file, output.dir, plink_path = plink_path) {
   file_extension <- tools::file_ext(input.file)
   output_file <- file.path(output.dir, "converted_to_plink")
   
   if (grepl("\\.vcf\\.gz$", input.file)) {
      cmd <- paste(plink_path, "--vcf", input.file, "--make-bed --out", output_file)
   } else if (file_extension == "vcf") {
      cmd <- paste(plink_path, "--vcf", input.file, "--make-bed --out", output_file)
   } else if (file_extension == "bcf") {
      cmd <- paste(plink_path, "--bcf", input.file, "--make-bed --out", output_file)
   } else {
      stop("Unsupported file type. Please provide a VCF, VCF.GZ, or BCF.")
   }
   
   system(cmd)
   return(output_file)
}


#============================
# Convert BCF to VCF
#

bcf_to_vcf <- function(input.file, output.dir, plink_path = plink_path){
   output_file <- file.path(output.dir, "tovcf")
   
   if (file_extension == "bcf"){
      command <- stringr::str_c(
         plink_path, " --bcf ", input.file,
         " --const-fid 0 --cow --keep-allele-order --allow-no-sex --allow-extra-chr",
         " --recode vcf --out ", output_file
      )
      system(command)
   } else {
      stop("Input is not a BCF file.")
   }
   return(output_file)
}

#============================
# Convert PLINK to VCF

plink_to_vcf <- function(bed.file, bim.file, fam.file, output.dir, plink_path = plink_path){
   output_file <- file.path(output.dir, "tovcf")
   
   command <- stringr::str_c(
      plink_path, " --bed ", bed.file,
      " --bim ", bim.file, " --fam ", fam.file,
      " --const-fid 0 --cow --keep-allele-order --allow-no-sex --allow-extra-chr",
      " --recode vcf --out ", outputVCF
   )
   
   system(command)
   return(output_file)
}

#============================
# VCF TO FASTA

vcf_to_fasta <- function(vcf_file, reference, bcftools_path, output.dir){
   output_file <- file.path(output.dir, "consensus.fa")
   
   cmd <-  stringr::str_c(
      bcftools_path, "consensus -f ", reference, " ", vcf_file, " -o ", output_file
   )
   
   system(cmd)
   return(output_file)
   
}

#============================
# Load files

# Load VCF/VCF.GZ
# Description: Generates a dataframe with markers as columns

load_vcf_files <- function(files, output.dir = NULL){
   library(dplyr)
   
   if (tools::file_ext(vcf) == "vcf") {
      vcf_object <- vcfR::read.vcfR(vcf, verbose = FALSE)
      genotypes <- vcfR::extract.gt(vcf_object, return.alleles = TRUE)
      columns <- as.data.frame(vcfR::getFIX(vcf_object))
      ID <- columns$ID
      raw_df <- data.frame(ID, genotypes)
      
      final_df <- data.frame(t(raw_df)) %>%
         janitor::row_to_names(row_number = 1) %>%
         tibble::rownames_to_column(var = "Sample")
      
   } else if (tools::file_ext(vcf) == ".gz") {
      utils::untar(vcf, exdir = output.dir)
      
      wb <- list.files(path = file.path(output.dir), pattern = ".vcf$", full.names = TRUE)
      dflist <- lapply(wb, function(x) {
         vcf_obj <- vcfR::read.vcfR(x, verbose = FALSE)
         genotypes <- vcfR::extract.gt(vcf_obj, return.alleles = TRUE)
         columns <- as.data.frame(vcfR::getFIX(vcf_obj))
         ID <- columns$ID
         temp_df <- data.frame(ID, genotypes)
         
         data.frame(t(temp_df)) %>%
            janitor::row_to_names(row_number = 1) %>%
            tibble::rownames_to_column(var = "Sample")
      })
      
      final_df <- dplyr::bind_rows(dflist, .id = "source", .fill = TRUE)
      
   } else {
      stop("Unsupported file type for VCF input.")
   }
   
   return(final_df)
}

# Merge VCF Files
merge_vcf_files <- function(output.dir = output.dir, merged.file) {
   vcf_files <- list.files(output.dir, pattern = "*.vcf$", full.names = TRUE)
   
   if (length(vcf_files) == 0) {
      stop("No extracted VCF files found for merging.")
   }
   
   merge_command <- c("concat", vcf_files, "-o", file.path(output.dir, merged.file))
   system2("bcftools", args = merge_command)
   
   return(file.path(output.dir, merged.file))
}

# Load CSV/XLSX files
load_csv_xlsx_files <- function(input) {
   if (tools::file_ext(input) == "csv") {
      return(readr::read_csv(input))
   } else if (tools::file_ext(input) == "xlsx") {
      return(readxl::read_excel(input))
   } else if (tools::file_ext(input) == "txt") {
      return(read.table(input, quote="\"", comment.char=""))
   } else {
      stop("Input file should be in csv, txt, or xlsx format.")
   }
}

#============================
# VCF FILE CONVERSION TO CSV
# 
# Description: Function converts VCF and/or VCF.GZ to CSV files containing
# sample names in the first column and marker information (A/T) in succeeding cols
# and merges it with population metadata (Superpopulation/Continental level).
#
# Last revised 06 August 2025
# Issue: Pop added at the end of the file
# Revised: 16 January 2026, incorporated helper functions
#============================

vcf_to_csv <- function(files, ref = NULL, output.dir = NULL) {
   library(dplyr)
   library(purrr)
   
   extension <- tools::file_ext(files)
   
   if (extension %in% c("vcf", "gz")){
      raw_file <- load_vcf_files(files, output.dir = output.dir)
      raw_file <- raw_file %>%
         rename(Sample = 1)
   } else {
      stop("Input is not a VCF file.")
   } 
   
   final_df <- raw_file
   
   # Ensure reference is provided
   if (is.null(ref) || ref == "") stop("Reference should be provided")
   
   # Merge or assign population
   if (file.exists(ref)) {
      ref_data <- load_csv_xlsx_files(ref)
      ref_data <- ref_data %>%
         rename(Sample = 1, Population = 2)
      ref_data <- ref_data[,1:2]
      final_df <- left_join(raw_file, ref_data, by = "Sample")
   } else if (is.character(ref)) {
      final_df$Population <- ref
   } else {
      stop("Invalid reference input: must be a file path or string.")
   }
   
   # solution to the pop and sample organization (06 August 2025)
   sample <- final_df[,1]
   nosample <- final_df[,-1]
   data_cols <- ncol(nosample) - 1 #number of data cols
   total_cols <- as.integer(ncol(final_df)) #number of total cols
   data_gt <- final_df[, 2:data_cols] #subset data
   pop <- final_df[,total_cols] # subset pop
   final_df <- bind_cols(sample, pop, data_gt)
   final_df <- final_df %>%
      rename(Sample = 1, Population = 2)
   
   return(final_df)
}

#============================
# CSV to VCF
# Note that the strand where the ref and alt are based are compatible with the data on the CSV copy

to_binary <- function(df, markers = marker.file){ # marker.file requires txt/csv [1] rsID [2] REF [3] ALT
   library(dplyr)
   
   df <- as.data.frame(df)
   rownames(df) <- paste(df[,1], "id", sep = "_")
   data <- df[,-c(1,2)]
   data <- data.frame(t(data))
   row_name <- data.frame(rownames(data))
   data <- lapply(data, function(x) gsub("/", "", x, fixed = TRUE))
   data <- as.data.frame(data)
   revised_data <- data.frame(row_name, data)
   revised_data <- revised_data %>%
      rename(ID = 1)
   
   marker <- as.data.frame(markers)
   marker <- marker[,-c(2,3,4)]
   marker <- marker[,1:3]
   marker <- marker %>% mutate(across(tidyselect::everything(), ~ case_when(
      . == "A" ~ "AA",
      . == "T" ~ "TT",
      . == "C" ~ "CC",
      . == "G" ~ "GG",
      TRUE ~ .x)))
   marker <- marker %>%
      rename(ID = 1, REF = 2, ALT = 3)
   
   df_marker <- merge(marker, revised_data, by = "ID", all.x = TRUE)
   df_marker <- df_marker %>%
      mutate(across(ends_with("_id"),
                    ~ case_when(.x == df_marker$REF ~ 0,
                                .x == df_marker$ALT ~ 2)))
   
   df_marker[is.na(df_marker)] <- 1
   
   final_df <- df_marker[,-c(1,2,3)]
   final_df <- data.frame(t(final_df))
   return(final_df)
}

# CSV to tidypopgen's gen_tibble object
# Assumes matrix is a data frame
csv_to_gentibble <- function(file, loci.meta = loci.meta){
   library(dplyr)
   
   df <- load_csv_xlsx_files(file)
   meta <- df[,1:2]
   meta <- meta %>%
      rename(id = 1, population = 2)
   
   loci_meta <- load_csv_xlsx_files(loci.meta)
   loci_meta <- loci_meta %>%
      rename(name = 1, chromosome = 2, position = 3, genetic_dist = 4, allele_ref = 5, allele_alt = 6)
   
   geno <- to_binary(df, markers = loci_meta)
   geno <- as.matrix(geno)
   
   gentibble <- tidypopgen::gen_tibble(
      x = geno,
      loci = loci_meta,
      indiv_meta = meta,
      valid_alleles = c("A", "T", "C", "G"),
      quiet = TRUE
   )
   
   return(gentibble)
}

#============================
# UAS Files to CSV

uas_to_csv <- function(files = files, population = pop_file, reference = FALSE, output.dir = output.dir){
   library(dplyr)
   library(purrr)
   
   if (!file.exists(files)){
      stop("File does not exist in the working directory")
   } else {
      files_raw <- unpack_input_file(files, output.dir = output.dir)
   }
   
   data_files <- files_raw$data_files
   data_list <- data_files
   all.list <- list()
   
   for (x in data_list) {
      all.list[[x]] = readxl::read_excel(x,
                                         sheet = 1,
                                         col_names = TRUE,
                                         row.names(data_files))}
   
   new_colnames <- c("Sample", "ID", "Allele")
   dflist_new <- lapply(all.list, setNames, new_colnames)
   
   dflist_corrected <- lapply(
      dflist_new, 
      function(x){ 
         stats::aggregate(Allele ~ Sample + ID, x, paste, collapse = "/")
      })
   
   df_list <- purrr::map(dflist_corrected, ~(tidyr::pivot_wider(.x,
                                                                names_from= Sample,
                                                                values_from = Allele)))
   
   merged <- df_list %>% purrr::reduce(full_join, by= "ID")
   id <- merged$ID 
   
   #transpose
   correct_alleles <- merged
   correct_alleles <- data.frame(t(correct_alleles))
   names(correct_alleles) <- correct_alleles[1,] 
   correct_alleles <- correct_alleles[-1,] 
   
   samples <- data.frame(colnames(merged[,-1]))
   corrected <- correct_alleles
   
   Samples <- rownames(corrected)
   corrected <- data.frame(Samples, corrected)
   
   if (reference == FALSE){
      return(corrected)
   } else {
      pop_data <- load_csv_xlsx_files(population)
      pop_data <- pop_data %>%
         rename(Sample = 1, Population = 2)
      
      matched <- corrected %>% dplyr::left_join(pop_data, by = "Sample")
      data_length <- as.integer(ncol(corrected) - 1)
      data_matched <- matched[,2:data_length]
      meta_begin <- as.integer(ncol(corrected) + 1)
      meta <- matched[,meta_begin:ncol(matched)]
      
      final_df <- dplyr::bind_cols(matched$Sample, meta, data_matched)
      names(final_df)[names(final_df) == "matched$Sample"] <- "Sample"
      return(final_df)
   }
}

#==========================
# Convert to Genind
convert_to_genind_str <- function(file) {
   library(dplyr) 
   
   file <- file %>%
      rename(Ind = 1, Pop = 2)
   
   names(file)[names(file) == "Ind"] <- "Ind2"
   
   file$Ind <- rownames(file)
   file2 <- file
   Ind <- file$Ind
   data <- file[2:ncol(file)-1]
   file <- data.frame(Ind, data)
   
   ind <- as.character(file$Ind)
   pop <- as.character(file$Pop)
   
   fsnps_geno <- file[,3:ncol(file)]
   
   fsnps_gen <- adegenet::df2genind(fsnps_geno, 
                                    ind.names = ind, 
                                    pop = pop, 
                                    sep = "/", 
                                    NA.char = "N", 
                                    ploidy = 2, 
                                    type = "codom")
   
   
   fsnps_gen@pop <- as.factor(file$Pop)
   
   return(list(fsnps_gen = fsnps_gen, 
               new_file = file2))
}

convert_to_genind <- function(file) {
   
   file <- file %>%
      rename(Ind = 1, Pop = 2)
   
   ind <- as.character(file$Ind)
   pop <- as.character(file$Pop)
   fsnps_geno <- file[, 3:ncol(file)]
   
   fsnps_gen <- adegenet::df2genind(fsnps_geno, 
                                    ind.names = ind, 
                                    pop = pop, 
                                    sep = "/", 
                                    NA.char = "N", 
                                    ploidy = 2, 
                                    type = "codom")
   
   
   fsnps_gen@pop <- as.factor(file$Pop)
   
   return(fsnps_gen)
}

#==========================
# Clean Data
clean_input_data <- function(file) {
   library(dplyr)
   
   file1 <- lapply(file, function(x) gsub("|", "/", x, fixed = TRUE))
   file1 <- as.data.frame(file)
   file1[is.na(file1)] <- "N"
   
   file1 <- file1 %>%
      mutate(across(everything(), ~ case_when(
         . == "N/A" ~ "N",
         . == "NA" ~ "N",
         TRUE ~ .x))) %>%
      rename(Ind = 1, Pop = 2)
   
   file1 <- as.data.frame(file1)
   
   return(file1)
}

#==========================
# To STRUCTURE

revise_structure_file <- function(file, output.dir, system = "Windows"){
   
   fsnps_gen_sub <- poppr::popsub(file)
   path <- file.path(output.dir, "structure_file.str")
   
   if (system == "Windows"){
      genind2structure2(fsnps_gen_sub, file = path, pops = TRUE, unix = FALSE)
   } else if(system == "Linux"){
      genind2structure2(fsnps_gen_sub, file = path, pops = TRUE, markers = TRUE, unix = TRUE)
      system(paste("tr '\t' ' '", shQuote(path), ">", shQuote(path)))
      system(paste("sed -e 's/ /\t/2' -e 's/ /\t/1'", shQuote(path), ">", shQuote(path)))
   } 
   
   return(path)
}

#============================
# FILE CONVERSION TO SNIPPER 
# 
# Description: Function combines genetic data files (VCF/CSV/XLSX) containing
# sample names in the first column and marker information (A/T) in succeeding cols
# and its population metadata (Superpopulation/Continental level and population level).
# This creates an output compatible specific to SNIPPER
#
# Last revised 24 June 2025
#============================

to_snipper <- function(input,
                      references,
                      target.pop = TRUE,
                      population.name = NULL,
                      markers = snps){
   library(dplyr)
   library(purrr)
   
   file_extension <- tools::file_ext(input)
   
   if (file_extension %in% c("csv", "xlsx")){
      input.file <- load_csv_xlsx_files(input)
   } else if (file_extension == "vcf"){
      input.file <- load_vcf_files(input)
   } else {
      stop("Not an xlsx, csv, or vcf file.")
   }
   
   tosnipper <- lapply(
      input.file,
      function(x){
         gsub(pattern = "/", replacement = "", x = x, fixed = TRUE)
      }
   )
   
   tosnipper <- as.data.frame(tosnipper)
   
   if(class(tosnipper$Sample) != "character"){
      tosnipper$Sample <-  as.character(tosnipper$Sample)
   }
   
   reference <- load_csv_xlsx_files(references)
   reference <- reference %>%
      rename(Sample = 1)
   
   if(class(reference$Sample) != "character"){
      reference$Sample <-  as.character(reference$Sample)
   }
   
   matched <- tosnipper %>% dplyr::left_join(reference, by = "Sample")
   last.col <- as.integer(ncol(matched))
   sec.last <- last.col - 1
   Superpop <- as.data.frame(matched[,last.col])
   Population <- as.data.frame(matched[,sec.last])
   Sample <- as.data.frame(matched$Sample)
   data <- as.data.frame(matched[,2:ncol(matched)-1])
   drops <- "Sample"
   data <- data[,!(names(data) %in% drops)]
   
   to_excel <- dplyr::bind_cols(Population, Superpop, Sample, data)
   names(to_excel)[names(to_excel) == "matched[, last.col]"] <- "Superpop"
   names(to_excel)[names(to_excel) == "matched[, sec.last]"] <- "Population"
   names(to_excel)[names(to_excel) == "matched$Sample"] <- "Sample"
   
   tosnpr_split <- split(to_excel, to_excel$Population)
   tosnpr_split <- tosnpr_split %>% map(`rownames<-`, NULL)
   tosnpr_split <- lapply(
      tosnpr_split,
      function(x){
         x$No <- rownames(x)
         as.data.frame(x)
         data <- as.data.frame(x[,2:ncol(x)-2])
         Sample <- x[,1]
         x <- dplyr::bind_cols(x$No, Sample, data)
      }
   )
   
   merged <- plyr::ldply(tosnpr_split, data.frame)
   merged <- merged[,-c(1,3)]
   
   if(target.pop == TRUE){
      target <- merged[merged$Population == population.name,]
      target$snpr <- "0"
      non.target <- merged[merged$Population != population.name,]
      non.target$snpr <- "1"
      merged2 <- dplyr::bind_rows(target, non.target)
   } else if(target.pop == FALSE) {
      merged2 <- merged
      merged2$snpr <- "1"
   } else{
      stop("Parameter target.pop is not stated.")
   }
   
   sample.count <- as.integer(nrow(merged2))
   pop.only <- as.data.frame(merged2$Superpop)
   pop.only <- pop.only[!duplicated(pop.only), ]
   pop.only <- as.data.frame(pop.only)
   pop.count <- as.integer(nrow(pop.only))
   merged3 <- merged2[,-2]
   merged3 <- as.data.frame(merged3)
   names(merged3)[names(merged3) == "...1"] <- sample.count
   names(merged3)[names(merged3) == "Superpop"] <- markers
   names(merged3)[names(merged3) == "Sample"] <- pop.count
   names(merged3)[names(merged3) == "snpr"] <- ""
   
   merged3 <- rbind(NA, merged3)
   merged3 <- rbind(NA, merged3)
   merged3 <- rbind(NA, merged3)
   merged3 <- rbind(NA, merged3)
   
   return(merged3)
   
}

#============================
# MARKER EXTRACTION
# 
# Description: Function extracts SNPs by their rsID or position information
# using PLINK
#
# Last revised 31 October 2025
#============================
extract_markers <- function(input_type,
                           input.file = NULL, 
                            snps.list = NULL, 
                            pos.list = NULL, 
                            bed.file = NULL, 
                            bim.file = NULL, 
                            fam.file = NULL,
                            output.dir, 
                            merged.file,
                            plink_path) {
   
   if (!dir.exists(output.dir)) {
      dir.create(output.dir, recursive = TRUE)
   }
   
   plink_files_given <- !is.null(bed.file) || !is.null(bim.file) || !is.null(fam.file)
   vcf_given <- !is.null(input.file)
   
   if (plink_files_given && vcf_given){
      stop("Please provide either a single input file (VCF/VCF.GZ/BCF) or PLINK files (bed, bim, fam)")
   }
   
   if (!is.null(snps.list)) {
      return(extract_by_ID(snps.list, input_type, input.file,
                           bed.file, bim.file, fam.file,
                           output.dir, merged.file, plink_path))
   } else if (!is.null(pos.list)){
      return(extract_by_pos(pos.list, input_type, input.file,
                            bed.file, bim.file, fam.file,
                            output.dir, merged.file, plink_path))
   } else {
      stop("Provide SNPs name (rsID) or list of POS")
   }
}


extract_by_ID <- function(snps.list, input_type, input.file,
                          bed.file, bim.file, fam.file,
                          output.dir, merged.file,
                          plink_path) {
   
   file_extracted <- file.path(output.dir, merged.file)
   
   if (input_type %in% c("vcf", "bcf")){
      cmd <- paste(
         shQuote(plink_path),
         paste0("--", input_type), shQuote(input.file),
         "--extract", shQuote(snps.list),
         "--keep-allele-order --allow-no-sex --allow-extra-chr --recode vcf --out", shQuote(file_extracted)
      )
   } else {
      cmd <- paste(
         shQuote(plink_path),
         "--bed", shQuote(bed.file),
         "--bim", shQuote(bim.file),
         "--fam", shQuote(fam.file),
         "--extract", shQuote(snps.list),
         "--keep-allele-order --allow-no-sex --allow-extra-chr --recode vcf --out", shQuote(file_extracted)
      )
   }
   system(cmd)
   return(paste0(file_extracted, ".vcf"))
}


extract_by_pos <- function(pos.list, 
                           input_type,
                           input.file,
                           bed.file, bim.file, fam.file,
                           output.dir, 
                           merged.file,
                           plink_path){
   
   for (i in seq_len(nrow(pos.list))){
      chr_num <- pos.list[i, 2]
      start_bp <- pos.list[i, 3]
      
      filename_base <- paste0("chr", chr_num, "_", start_bp - 1)
      output_file <- file.path(output.dir, filename_base)
      
      if (input_type %in% c("vcf", "bcf")){
         cmd <- paste(
            shQuote(plink_path),
            paste0("--", input_type), shQuote(input.file),
            "--chr", chr_num,
            "--from-bp", start_bp,
            "--to-bp", start_bp,
            "--keep-allele-order --allow-extra-chr --recode vcf",
            "--out", shQuote(output_file)
         )
      } else {
         cmd <- paste(
            shQuote(plink_path),
            "--bed", shQuote(bed.file),
            "--bim", shQuote(bim.file),
            "--fam", shQuote(fam.file),
            "--chr", chr_num,
            "--from-bp", start_bp,
            "--to-bp", start_bp,
            "--keep-allele-order --allow-extra-chr --recode vcf",
            "--out", shQuote(output_file)
         )
      }
      system(cmd)
   }
   final_vcf <- merge_vcf_files(output.dir, merged.file)
   return(final_vcf)
}


extract_POStoID <- function(pos.list,
                            input_type,
                            input.file = NULL,
                            bed.file = NULL, 
                            bim.file = NULL, 
                            fam.file = NULL,
                            output.dir,
                            plink_path){
   
   # generate a text file outside
   list_plink <- file.path(output.dir, "plink_files.txt")
   list_ID_names <- file.path(output.dir, "ID_names.txt")
   
   if (file.exists(list_plink)) file.remove(list_plink)
   if (file.exists(list_ID_names)) file.remove(list_ID_names)
   
   for (i in seq_len(nrow(pos.list))) {
      rsID <- pos.list[i, 1]
      chr_num <- pos.list[i, 2]
      start_bp <- pos.list[i, 3]
      
      # 1. Extraction
      filename_base <- paste0("chr", chr_num, "_", start_bp - 1)
      output_file <- file.path(output.dir, filename_base)
      
      if (input_type %in% c("vcf", "bcf")) {
         extract_args <- paste(
            shQuote(plink_path),
            paste0("--", input_type), shQuote(input.file),
            "--cow",
            "--chr", chr_num,
            "--from-bp", start_bp,
            "--to-bp", start_bp,
            "--make-bed",
            "--out", shQuote(output_file)
         )
      } else {
         extract_args <- paste(
            shQuote(plink_path),
            "--bed", shQuote(bed.file),
            "--bim", shQuote(bim.file),
            "--fam", shQuote(fam.file),
            "--cow",
            "--chr", chr_num,
            "--from-bp", start_bp,
            "--to-bp", start_bp,
            "--make-bed",
            "--out", shQuote(output_file)
         )
      }
      system(extract_args)

      
      # 2. Adding rsID
      bed_file <- paste0(output_file, ".bed")
      bim_file <- paste0(output_file, ".bim")
      fam_file <- paste0(output_file, ".fam")
      
      if (!file.exists(bed_file)){
         warning("PLINK did not produce expected files for ", rsID)
         next
      }
      
      revised <- file.path(output.dir, rsID)
      add_rsid <- paste(
         shQuote(plink_path),
         "--bed", shQuote(bed_file),
         "--bim", shQuote(bim_file),
         "--fam", shQuote(fam_file),
         "--set-missing-var-ids @:#", rsID,
         "--make-bed",
         "--out", shQuote(revised)
      )
      message("Running: ", add_rsid)
      system(add_rsid)
      
      # 2.1. Generate files
      plink_lines <- paste(paste0(revised, ".bed"),
                           paste0(revised, ".bim"),
                           paste0(revised, ".fam"),
                           sep = "\t"
                           )
      write(plink_lines, file = list_plink, append = TRUE)
      
      # 2.2 Append rsID to ID names
      rsid_line <- paste0("chr", chr_num, ":", start_bp, ":", rsID, "\t", rsID)
      write(rsid_line, file = list_ID_names, append = TRUE)
   }
   
   # 3. Merge PLINK files
   merged_file <- file.path(output.dir, "merged")
   merged_plink <- paste(
      shQuote(plink_path),
      "--merge-list", shQuote(list_plink),
      "--recode vcf",
      "--keep-allele-order",
      "--out", shQuote(merged_file)
      )
   message("Running: ", merged_plink)
   system(merged_file)
   
   # 4. Rename rsID
   input_vcf <- paste0(merged_file, ".vcf")
   corrected_ID <- file.path(output.dir, "renamed_ID")
   updated_plink <- paste(
      shQuote(plink_path),
      "--vcf", shQuote(input_vcf),
      "--update-name", shQuote(list_ID_names),
      "--recode vcf",
      "--out", shQuote(corrected_ID)
   )
   system(updated_plink)
   
   return(paste0(corrected_ID, ".vcf"))
}




#============================
# CONCORDANCE ANALYSIS
# 
# Description: Compares genetic data information generated by different sequencing techniques.
# It compares the genetic data of the same individual from technique 1 and technique 2.  
# Input file should be in CSV or XLSX format.
#
# Last revised 16 October 2025: Corrected discordant and concordant counts
#============================


calc_concordance <- function(file1, file2, haplotypes = FALSE){
   library(dplyr)
   
   if(!file.exists(file1)){
      stop("First file does not exist in the working directory")
   } else {
      file1 <- load_csv_xlsx_files(file1)
   }
   
   if(!file.exists(file2)){
      stop("Second file does not exist in the working directory")
   } else {
      file2 <- load_csv_xlsx_files(file2)
   }
   
   file1 <- file1 %>%
      rename(Ind = 1)
   file2 <- file2 %>%
      rename(Ind = 1)
   
   file_list <- list(file1, file2)
   overlaps <- as.list(intersect(file1$Ind, file2$Ind))
   
   if (is.null(overlaps)){
      stop("Samples names from the two files are different.")
   }
   
   file_list2 <- lapply(
      file_list,
      function(x){
         x[x$Ind %in% overlaps, ]
      }
   )
   
   file_list3 <- lapply(
      file_list2,
      function(x){
         library(dplyr)
         data.frame(t(x)) %>%
            janitor::row_to_names(row_number = 1) %>%
            tibble::rownames_to_column(., var = "markers") 
      }
   )
   
   overlaps <- as.character(overlaps)
   markers1 = file_list3[[1]]$markers
   markers2 = file_list3[[2]]$markers
   
   file_list4 <- lapply(
      file_list3,
      function(x){
         relocate(x, any_of(overlaps)) 
      }
   )
   
   file_list4[[1]]$markers = markers1
   file_list4[[2]]$markers = markers2
   merged <- file_list4 %>% purrr::reduce(full_join, by= "markers")
   ID <- merged$markers
   merged <- clean_input_data(merged)
   
   if(haplotypes == TRUE){
      print("Assuming the data are haplotypes.")
   } else if(haplotypes == FALSE){
      merged <- merged %>% mutate(across(tidyselect::everything(), ~ case_when(
         . == "A" ~ "A/A",
         . == "T" ~ "T/T",
         . == "C" ~ "C/C",
         . == "G" ~ "G/G",
         . == "T/C" ~ "C/T",
         . == "T/G" ~ "G/T",
         . == "T/A" ~ "A/T",
         . == "G/A" ~ "A/G",
         . == "C/G" ~ "G/C",
         . == "C/A" ~ "A/C",
         TRUE ~ .x)))
   } else {
      stop("Parameter haplotype is required.")
   }
   
   overlap1 <- merged %>% select(dplyr::ends_with(".x"))
   overlap2 <- merged %>% select(dplyr::ends_with(".y"))
   
   for_conc <- QurvE::zipFastener(overlap1, overlap2, along = 2)
   for_conc2 <- data.frame(ID, for_conc)
   for_conc2[is.na(for_conc2)] <- "N"
   names(for_conc2) <-  sub('^X', '', names(for_conc2))
   
   return(for_conc2)
}

plot_concordance <- function(dataframe){
   library(ggplot2)
   
   concordance <- bind_cols(dataframe %>%
                               tidyr::gather(var, val, -matches("(.x$|ID)")) %>%
                               select(ID,val), dataframe %>%
                               tidyr::gather(var2, val2, -matches("(*.y$|ID)")) %>%
                               select(val2)) %>%
      add_count(ID) %>%
      group_by(ID) %>%
      summarise(
         Total = paste((ncol(dataframe) - 1)/2),
         Incomparable = paste(sum(val == "N" | val2 == "N")),
         Concordant = paste(sum(val == val2 & val != "N" & val2 != "N")),
         Discordant = paste(((ncol(dataframe) - 1)/2) - (sum(val == val2 & val != "N" & val2 != "N") + sum(val == "N" | val2 == "N")))) %>%
      left_join(dataframe, by = c("ID" = "ID")) 
   
   pivot <- concordance[,1]
   pivot2 <- concordance[,3:5]
   pivot <- data.frame(pivot, pivot2)
   pivot <- pivot %>%
      tidyr::pivot_longer(!ID,
                          names_to = 'Condition',
                          values_to = 'Count'
      )
   
   Count <- as.integer(pivot$Count)
   rsID <- pivot$ID
   Condition <- pivot$Condition
   
   visual <- data.frame(rsID, Count, Condition)
   
   plot_conc <- visual %>%
      arrange(Count) %>%
      arrange(Condition) %>%
      mutate(rsID = forcats::fct_inorder(rsID)) %>%
      ggplot(aes(fill = Condition, x= rsID, y=Count)) +
      geom_bar(position = "stack", stat = "identity") +
      theme(
         axis.text.x = element_text(
            angle = 90,
            vjust = .3), 
         panel.background = element_blank()) +
      scale_fill_manual(values= c("#1ca7ec",
                                  "#fb7a8e",
                                  "#1f2f98"
      ))
   
   return(list(
      results = concordance,
      plot = plot_conc 
   ))
}


#============================
# Depth Plot
# 
# Description: Generates depth of coverage plot available only when using VCF files.
#
# Last revised 31 October 2025
#============================

depth_from_vcf <- function(vcf, 
                           output.dir, 
                           reference, 
                           palette = NULL, 
                           width = 10, 
                           height = 8, 
                           dpi = 300){
   library(dplyr)
   
   vcf.file <- vcfR::read.vcfR(vcf)
   depth <- vcfR::extract.gt(vcf.file, element = "DP", as.numeric = TRUE)
   depth <- as.data.frame(t(depth))
   depth$Sample <- rownames(depth)
   
   depth_long <- depth %>% tidyr::pivot_longer(
      !Sample,
      names_to = "rsID",
      values_to = "Depth"
   )
   
   # reference assumes there are only two columns: 
   # [1] assumes it has the same key as one either the sample or marker name similar to VCF files
   # [2] the data to be highlighted 
   if(!is.null(reference)){
      ref <- load_csv_xlsx_files(reference)
      ref <- ref %>%
         rename(Sample = 1, highlight = 2)
      depth_long <- depth_long %>% dplyr::left_join(ref, by = "Sample")
      fill2 = depth_long$highlight
   } else {
      fill2 = NULL
   }
   
   # plot of depth per marker
   # to-do: slant the rsID labels
   p_rsid <- ggplot2::ggplot(depth_long, ggplot2::aes(x=rsID, y = Depth, fill = fill2)) +
      ggplot2::geom_boxplot() +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = .4)) +
      ggplot2::scale_fill_brewer(palette = palette)
   
   out_dp_rsid <- file.path(output.dir, "Depth_marker.png")
   ggsave(out_dp_rsid, plot = p_rsid, width = width, height = height, dpi = dpi)
   
   # plot of depth per sample
   p_sample <- ggplot2::ggplot(depth_long, ggplot2::aes(x=Sample, y = Depth, fill = fill2)) +
      ggplot2::geom_boxplot() +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = .4)) +
      ggplot2::scale_fill_brewer(palette = palette)
   
   out_dp_sample <- file.path(output.dir, "Depth_samples.png")
   ggsave(out_dp_sample, plot = p_sample, width = width, height = height, dpi = dpi)
   
   return(list(
      plot_marker = out_dp_rsid,
      plot_sample = out_dp_sample
   ))
}

compute_pop_stats <- function(fsnps_gen) {
   library(dplyr)
   library(ade4)
   library(adegenet)
   devtools::source_url("https://raw.githubusercontent.com/Tom-Jenkins/utility_scripts/master/TJ_genind2genepop_function.R")
   
   mar_matrix <- hierfstat::allelic.richness(hierfstat::genind2hierfstat(fsnps_gen))$Ar %>%
      apply(MARGIN = 2, FUN = mean) %>%
      round(digits = 3)
   mar_list <- as.data.frame(mar_matrix)
   
   basic_fsnps <- hierfstat::basic.stats(fsnps_gen, diploid = TRUE)
   Ho <- apply(basic_fsnps$Ho, 2, mean, na.rm = TRUE) %>% round(2)
   He <- apply(basic_fsnps$Hs, 2, mean, na.rm = TRUE) %>% round(2)
   heterozygosity_df <- data.frame(Population = names(Ho), Ho, He) %>%
      tidyr::pivot_longer(cols = c("Ho", "He"), names_to = "Variable", values_to = "Value")
   
   # T-Test between Heterozygosities
   ttest_df <- data.frame(
      Locus = rownames(basic_fsnps$perloc),
      basic_fsnps$perloc,
      row.names = NULL
   )
   
   # Inbreeding Coefficient
   fis_values <- apply(basic_fsnps$Fis, 2, mean, na.rm = TRUE) %>%
      round(3)
   fis_df <- data.frame(Population = names(fis_values), Fis = fis_values)
   
   # Allele frequencies
   fsnps_gpop <- adegenet::genind2genpop(fsnps_gen)
   allele_freqs <- t(adegenet::makefreq(fsnps_gpop, quiet = FALSE, missing = NA)) %>%
      as.data.frame()
   
   return(list(
      mar_list = mar_list,
      heterozygosity = heterozygosity_df,
      ttest = ttest_df,
      inbreeding_coeff = fis_df,
      allele_frequencies = allele_freqs
   ))
}

compute_hwe <- function(fsnps_gen) {
   # Hardy-Weinberg Equilibrium (List for export)
   fsnps_hwe <- as.numeric(round(pegas::hw.test(fsnps_gen, B = 1000), 6)) 
   
   # Chi-square test (Matrix for export, Data Frame for ggplot)
   fsnps_hwe_test <- data.frame(sapply(adegenet::seppop(fsnps_gen), 
                                       function(ls) pegas::hw.test(ls, B=0)[,3]))
   
   fsnps_hwe_chisq_matrix <- as.matrix(fsnps_hwe_test)
   fsnps_hwe_chisq_df <- as.data.frame(t(fsnps_hwe_chisq_matrix)) %>% tibble::rownames_to_column("Population")
   
   return(list(
      hw_summary = fsnps_hwe,
      hw_matrix = fsnps_hwe_test,
      hw_dataframe = fsnps_hwe_chisq_df
   ))
}


compute_fst <- function(fsnps_gen) {
   # Pairwise Fst matrix using Weir & Cockerham 1984 method
   fst_matrix_raw <- hierfstat::genet.dist(fsnps_gen, method = "WC84") %>%
      round(3)
   
   fst_list <- if (length(fst_matrix_raw) == 0) {
      list(message = "No Fst values calculated")
   } else {
      as.list(as.matrix(fst_matrix_raw))
   }
   
   fst_df <- as.data.frame(as.matrix(fst_matrix_raw)) %>%
      tibble::rownames_to_column("Site1") %>%
      tidyr::pivot_longer(cols = -Site1, names_to = "Site2", values_to = "Fst")
   
   return(list(
      fst_matrix = fst_list,   
      fst_dataframe = fst_df   
   ))
}


plot_heterozygosity <- function(Het_fsnps_df, out_dir) {
   library(ggplot2)
   out_path <- file.path(out_dir, "heterozygosity_plot.png")
   
   Het_fsnps_df$Variable <- as.factor(Het_fsnps_df$Variable)
   
   p <- ggplot(Het_fsnps_df, aes(x = Population, y = Value, fill = Variable)) +
      geom_bar(stat = "identity", position = position_dodge(width = 0.6), colour = "black") +
      scale_y_continuous(expand = c(0, 0), limits = c(0, 0.5)) +
      scale_fill_manual(
         values = c("royalblue", "#bdbdbd"),
         labels = c(expression(italic("H")[o]), expression(italic("H")[e]))
      ) +
      labs(y = "Heterozygosity") +
      theme(axis.text.x = element_text(size = 10, angle = 90, vjust = 0.5, face = "bold"))
   
   ggsave(out_path, plot = p, width = 9, dpi = 300)
   return(out_path)
}

plot_fst <- function(fst_df, out_dir) {
   library(ggplot2)
   out_path <- file.path(out_dir, "fst_heatmap.png")
   
   p <- ggplot(fst_df, aes(x = Site1, y = Site2, fill = Fst, label = round(Fst, 3))) +
      geom_tile(color = "black") +
      geom_text(aes(label = round(Fst, 3)), size = 3, color = "black") +
      scale_fill_gradient2(
         low = "blue", mid = "pink", high = "red",
         midpoint = max(fst_df$Fst, na.rm = TRUE) / 2
      ) +
      labs(x = "Site 1", y = "Site 2", fill = "Fst") +
      theme_minimal(base_size = 11) +
      theme(
         axis.text = element_text(face = "bold"),
         axis.text.x = element_text(angle = 45, hjust = 1)
      )
   
   ggsave(out_path, plot = p, width = 8, height = 6, dpi = 300)
   return(out_path)
}


export_pop_results <- function(stats_matrix, hw_matrix, fst_matrix, dir = tempdir()) {
   timestamp <- format(Sys.time(), "%Y%m%d_%H%M")
   out_file <- file.path(dir, paste0("population-statistics-results_", timestamp, ".xlsx"))
   
   datasets <- list(
      "Mean Allelic Richness"     = as.data.frame(stats_matrix$mar_list),
      "Heterozygosities"          = as.data.frame(stats_matrix$heterozygosity),
      "T-test per Locus"          = as.data.frame(stats_matrix$ttest),
      "Inbreeding Coefficient"    = as.data.frame(stats_matrix$inbreeding_coeff),
      "Allele Frequencies"        = as.data.frame(stats_matrix$allele_frequencies),
      "Hardy-Weinberg Equilibrium"= as.data.frame(hw_matrix$hw_summary),
      "Chi-square Test Results"   = as.data.frame(hw_matrix$hwe_matrix),
      "Pairwise Fst Matrix"       = as.data.frame(fst_matrix$fst_matrix)
   )
   
   openxlsx::write.xlsx(datasets, file = out_file)
   return(out_file)
}

#============================
# PRINCIPAL COMPONENT ANALYSIS
# 
# Description: Performs exploratory analysis using PCA.
# It accepts a CSV/XLSX file containing sample names in the first column, 
# population data on the second, and genetic data on the third col.
#
# Last revised 04 August 2025
#============================

compute_pca <- function(fsnps_gen) {
   library(ade4)
   library(adegenet)
   library(stats)
   
   x <- tab(fsnps_gen, NA.method = "mean")
   set.seed(9999)
   pca1 <- dudi.pca(x, scannf = FALSE, scale = FALSE, nf = 7)
   
   percent <- pca1$eig/sum(pca1$eig)*100
   
   ind_coords <- as.data.frame(pca1$li)
   colnames(ind_coords) <- paste0("PC", seq_len(ncol(ind_coords)))
   ind_coords$Ind <- adegenet::indNames(fsnps_gen)
   ind_coords$Site <- fsnps_gen@pop
   
   centroid <- stats::aggregate(ind_coords[, -c(ncol(ind_coords), ncol(ind_coords)-1)], 
                                by = list(ind_coords$Site), 
                                FUN = mean)
   colnames(centroid)[1] <- "Site"
   centroid <- as.data.frame(centroid)
   
   return(list(pca1 = pca1, percent = percent, ind_coords = ind_coords, centroid = centroid))
}

get_labels <- function(fsnps_gen, use_default, input_labels = NULL, input_colors = NULL, input_shapes = NULL) {
   library(ade4)
   library(adegenet)
   
   if (use_default) {
      labels <- levels(as.factor(fsnps_gen@pop))
      n <- as.integer(length(labels))
      colors <- RColorBrewer::brewer.pal(min(n, 9), name = "Set1")
      shapes <- rep(21:25, length.out = n)
   } else {
      labels <- input_labels
      colors <- input_colors
      shapes <- input_shapes
      
      if (is.null(labels) || is.null(colors) || is.null(shapes)) {
         stop("Custom labels and colors must be provided if use_default is FALSE.")
      }
      if (length(labels) != length(colors)) {
         stop("Number of labels and colors must match.")
      }
      if (length(labels) != length(shapes)) {
         stop("Number of labels and shapes must match.")
      }
   }
   return(list(labels = labels, colors = colors, shapes = shapes))
}

plot_pca <- function(ind_coords, centroid, percent, labels_colors, width = 8, height = 8, pc_x = 1, pc_y = 2) {
   library(ggplot2)
   library(ggrepel)
   library(stats)
   
   # Ensure data frames
   if (!is.data.frame(ind_coords)) ind_coords <- as.data.frame(ind_coords)
   if (!is.data.frame(centroid)) centroid <- as.data.frame(centroid)
   
   # Axis labels
   xlab <- paste("PC", pc_x, " (", format(round(percent[pc_x], 1), nsmall = 1), "%)", sep = "")
   ylab <- paste("PC", pc_y, " (", format(round(percent[pc_y], 1), nsmall = 1), "%)", sep = "")
   
   # Theme
   ggtheme <- theme(
      axis.text.y = element_text(colour = "black", size = 12),
      axis.text.x = element_text(colour = "black", size = 12),
      axis.title = element_text(colour = "black", size = 12),
      panel.border = element_rect(colour = "black", fill = NA, size = 1),
      panel.background = element_blank(),
      plot.title = element_text(hjust = 0.5, size = 15)
   )
   
   # Plot
   plot <- ggplot(ind_coords, aes(x = ind_coords[[paste0("PC", pc_x)]],
                                  y = ind_coords[[paste0("PC", pc_y)]],
                                  fill = Site,
                                  shape = Site)) +
      geom_hline(yintercept = 0) +
      geom_vline(xintercept = 0) +
      geom_point(aes(shape = Site, fill = Site), color = "black", size = 3, show.legend = FALSE) + # shape is changed
      geom_label_repel(data = centroid,
                       aes(x = centroid[[paste0("PC", pc_x)]],
                           y = centroid[[paste0("PC", pc_y)]],
                           label = Site,
                           fill = Site),
                       color = "black",
                       size = 4, show.legend = FALSE) +
      scale_fill_manual(values = labels_colors$colors) +
      #scale_color_manual(values = labels_colors$colors) +
      scale_shape_manual(values = labels_colors$shapes) +
      labs(x = xlab, y = ylab) +
      ggtheme
   
   return(plot)
}

#============================
# STRUCTURE ANALYSIS
# 
# Description: Explores population structure and ancestry inference.
# It accepts a CSV/XLSX file containing sample names in the first column, 
# population data on the second, and genetic data on the third col.
#
# Last revised 24 August 2025
#============================

clean_input_data_str <- function(file) {
   library(dplyr)
   
   file <- clean_input_data(file)
   
   # For Plotting
   populations_df <- file[,1:2]
   colnames(populations_df) <- c("Label", "Population")
   populations_df$Label <- rownames(populations_df)
   
   ### Change pop to numeric - for STRUCTURE
   pop_df <- as.data.frame(file$Pop) %>%
      dplyr::rename(pops = 1)
   
   # Get total no. of pops
   pop_df_unique <- as.data.frame(pop_df[!duplicated(pop_df), ]) %>%
      dplyr::rename(pops = 1)
   # add row names as numbers
   pop_df_unique$num <- rownames(pop_df_unique)
   # write.csv(pop_df_unique, file = "population_order.csv") # RETURN THIS FOR DOWNLOAD
   
   # replace the pops in the original df (pop_df) with the numbers
   pop_df_corr <- left_join(pop_df, pop_df_unique, by = "pops") 
   pops <- pop_df_corr$num
   
   ### Change Ind to numeric
   ind_only <- as.data.frame(file$Ind)
   ind_only$num <- rownames(ind_only)
   
   ind <- as.character(ind_only$num)
   pop <- as.character(pops)
   geno <- file[, 3:ncol(file)]
   
   genind_obj <- adegenet::df2genind(geno, ind.names = ind, pop = pop, sep = "/", NA.char = "N", ploidy = 2, type = "codom")
   genind_obj@pop <- as.factor(pop)
   
   return(list(
      fsnps_gen = genind_obj,
      populations = pop_df_unique,
      pop_labels = populations_df
   )) 
}

q_matrices <- function(dir){
   output <- list.files(path = dir, pattern = "\\_f$", full.names = TRUE)
   output_list <- lapply(output, function(filepath){
      lines <- readLines(filepath)
      
      start <- grep("Inferred ancestry of individuals", lines) + 2
      end <- grep("^Estimated Allele Frequencies in each cluster", lines)[1] - 1
      
      qmatrices <- lines[start:end]
      
      qmatrices_data <- do.call(rbind, lapply(qmatrices, function(line){
         section <- unlist(strsplit(line, ":"))
         
         if(length(section) == 2) {
            props <- as.numeric(strsplit(trimws(section[2]), "\\s+")[[1]])
            return(props)
         } else {
            return(NULL)
         }
      }))
      return(qmatrices_data)
   })
   #names(output_list) <- sapply(output_list, function(n) as.character(output$k))
   names(output_list) <- basename(output)
   return(output_list)
}

###############################
# Multiple Sequence Alignment #
###############################

# READING FASTA FILES

read_fasta <- function(zipped, directory){
   utils::unzip(zipped, 
                files = NULL, 
                list = FALSE, 
                overwrite = TRUE, 
                exdir = file.path(directory, "fasta_files"))
   
   data_path <- file.path(directory, "fasta_files")
   fasta_patterns <- paste("\\.fasta$", "\\.fa$", "\\.fna$", "\\fas$", sep = "|")
   fasta_files <- list.files(path = data_path, pattern = fasta_patterns, full.names = TRUE)
   dna_sequences <- Biostrings::readDNAStringSet(fasta_files)
   return(dna_sequences)
}

# ALIGNMENT

msa_results <- function(files, algorithm, directory){
   # Creating Substitution Matrix
   personal_matrix <- pwalign::nucleotideSubstitutionMatrix(match = 1, mismatch = 0, baseOnly = FALSE, type = "DNA")
   gap_penalty <- -2
   personal_matrix <- rbind(personal_matrix, "-" = gap_penalty)
   personal_matrix <- cbind(personal_matrix, "-" = gap_penalty)
   colnames(personal_matrix) <- c("A", "C", "G", "T", "M", "R", "W", "S", "Y", "K", "V", "H", "D", "B", "N", "-")
   rownames(personal_matrix) <-  c("A", "C", "G", "T", "M", "R", "W", "S", "Y", "K", "V", "H", "D", "B", "N", "-")
   personal_matrix <- as.matrix(personal_matrix)
   
   # perform msa
   aligned_sequences <- msa::msa(files,substitutionMatrix = personal_matrix, method = algorithm) # ClustalW, ClustalOmega, MUSCLE
   
   # calculate alignment score
   alignment_scores <- msa::msaConservationScore(aligned_sequences, substitutionMatrix = personal_matrix)
   
   # Post-processing
   aligned_dnastrings <- msa::msaConvert(aligned_sequences, type = "seqinr::alignment")
   aligned_dnastrings <- Biostrings::DNAStringSet(setNames(aligned_dnastrings$seq, aligned_dnastrings$nam))
   
   adjusted <- DECIPHER::AdjustAlignment(aligned_dnastrings)
   staggered <- DECIPHER::StaggerAlignment(adjusted)
   
   filename3 <- file.path(paste0(directory, "aligned_seqs.pdf"))
   msa::msaPrettyPrint(aligned_sequences, file = filename3,
                       output="pdf", showNames= "left", showLogo = "none", askForOverwrite = FALSE)
   
   return(list(
      alignment_msa = aligned_sequences,
      scores = alignment_scores,
      aligned_adjusted = adjusted,
      aligned_staggered = staggered,
      pdf = filename3
   ))
}

build_nj_tree <- function(alignment, outgroup = NULL, seed = 123, model = model){
   library(ape)
   library(ggtree)
   
   bins <- ape::as.DNAbin(alignment)
   distance <- ape::dist.dna(bins, model = model)
   nj_tree <- ape::nj(distance)
   
   if (!is.null(outgroup) && outgroup %in% nj_tree$tip.label){
      nj_tree <- ape::root(nj_tree, outgroup = outgroup)
   }
   
   nj_tree <- ape::ladderize(nj_tree)
   num_sites <- ncol(bins)
   if (num_sites < 10){
      warning("Alignment has fewer than 10 sites. Skipping bootstrap.")
      tree_plot <- ggtree(nj_tree, branch.length = "none") +
         theme_tree2() +
         geom_tiplab() +
         ggtitle("NJ Tree")
      return(tree_plot)
   } 
   
   # Bootstrapping
   set.seed(seed)
   boots <- ape::boot.phylo(nj_tree, bins, 
                            FUN = function(x){
                               tree <- ape::nj(ape::dist.dna(x, model = model))
                               if (!is.null(outgroup) && outgroup %in% tree$tip.label){
                                  tree <- ape::root(tree, outgroup = outgroup)
                               }
                               ape::ladderize(tree)
                            }, rooted = TRUE
   )
   
   boots[is.na(boots)] <- 0
   nj_tree$node.label <- as.character(boots)
   tree_plot <- ggtree(nj_tree, branch.length = "none") +
      theme_tree2() +
      geom_tiplab() +
      geom_text2(aes(subset = !isTip, label = label), hjust = -0.3) +
      ggtitle("NJ Tree") +
      xlim(0, 20)
   
   return(tree_plot)
}

build_upgma_tree <- function(alignment, outgroup = NULL, seed =123, model = model){
   library(ape)
   
   bins <- ape::as.DNAbin(alignment)
   distance <- ape::dist.dna(bins, model = model)
   upgma_tree <- upgma(distance)
   
   if (!is.null(outgroup) && outgroup %in% upgma_tree$tip.label){
      upgma_tree <- ape::root(upgma_tree, outgroup = outgroup)
   }
   
   upgma_tree <- ape::ladderize(upgma_tree)
   num_sites <- ncol(bins)
   if (num_sites < 10){
      warning("Alignment has fewer than 10 sites. Skipping bootstrap.")
      
      tree_plot <- ggtree(upgma_tree, branch.length = "none") +
         theme_tree2() +
         geom_tiplab() +
         ggtitle("UPGMA Tree")
      
      return(tree_plot)
   }
   
   set.seed(seed)
   boots <- ape::boot.phylo(upgma_tree, bins, 
                            FUN = function(x){
                               tree <- ape::nj(ape::dist.dna(x, model = model))
                               if (!is.null(outgroup) && outgroup %in% tree$tip.label){
                                  tree <- ape::root(tree, outgroup = outgroup)
                               }
                               ape::ladderize(tree)
                            }, rooted = TRUE, 
   )
   
   boots[is.na(boots)] <- 0
   upgma_tree$node.label <- as.character(boots)
   tree_plot <- ggtree(upgma_tree, branch.length = "none") +
      theme_tree2() +
      geom_tiplab() +
      geom_text2(aes(subset = !isTip, label = label), hjust = -0.3) +
      ggtitle("UPGMA Tree") +
      xlim(0, 20)
   
   return(tree_plot)
}


build_max_parsimony <- function(alignment, outgroup = NULL, seed = 123, directory){
   library(phangorn)
   library(ape)
   
   bins <- ape::as.DNAbin(alignment)
   phy <- phangorn::phyDat(bins, type = "DNA")
   dm <- dist.ml(phy)
   start_tree <- NJ(dm)
   parsimony_tree <- optim.parsimony(start_tree, phy)
   
   # Rooting
   if (!is.null(outgroup) && outgroup %in% parsimony_tree$tip.label){
      parsimony_tree <- root(parsimony_tree, outgroup = outgroup, resolve.root = TRUE)
   } 
   
   # boostrapping
   set.seed(seed)
   bs_pars <- bootstrap.phyDat(phy, \(x) optim.parsimony(NJ(dist.ml(x)), x))
   
   # plot
   filename <- paste(directory, "parsimony_tree.png")
   png(filename, width = 800, height = 600)
   plotBS(parsimony_tree, bs_pars, main = "Parsimony Tree")
   dev.off()
   
   return(filename)
}

build_ml_tree <- function(alignment, 
                          outgroup = NULL, 
                          seed = 123, 
                          bs_reps = 100,
                          directory){
   library(phangorn)
   library(ape)
   
   bins <- ape::as.DNAbin(alignment)
   phy <- phyDat(bins, type = "DNA")
   
   dm <- dist.ml(phy)
   start_tree <- NJ(dm)
   
   fit <- pml(start_tree, data = phy)
   
   # find best-fit model
   model_test <- modelTest(phy, tree = start_tree)
   best_model <- model_test$Model[which.min(model_test$BIC)]
   best_model <- sub("\\+.*", "", best_model)
   fit_opt <- optim.pml(fit, model = best_model, optGamma = TRUE, optInv = TRUE, rearrangement = "stochastic")
   
   tree <- fit_opt$tree
   if (!is.null(outgroup) && outgroup %in% tree$tip.label){
      tree <- root(tree, outgroup = outgroup, resolve.root=TRUE)
   } 
   
   # bootstrapping
   set.seed(seed)
   bs <- bootstrap.pml(fit_opt, bs = bs_reps, optNni = TRUE)
   
   filename <-  paste(directory, "ml_tree.png")
   png(filename, width = 800, height = 600)
   plotBS(tree, bs, main = paste("ML Tree (", best_model, ")"))
   dev.off()
   return(list(
      best_model = best_model,
      filename = filename
   ))
}

##################
# CLASSIFICATION #
##################

calculate_naive_bayes <- function(file){
   library(e1071)
   library(caret)
   
   data_fsnps <- load_csv_xlsx_files(file)
   data_fsnps <- data_fsnps %>%
      rename(Sample = 1, Pop = 2)
   data_fsnps[] <- lapply(data_fsnps, factor)
   predictors = !grepl("Pop",colnames(data_fsnps))
   label = "Pop"
   data_fsnps <- as.data.frame(data_fsnps)
   
   # training the naive bayes classifier using a leave-one-out cross validation method
   res = lapply(1:nrow(data_fsnps),function(i){
      fit = naiveBayes(y=factor(data_fsnps[-i,label]),
                       x=as.matrix(data_fsnps[-i,predictors]))
      data.frame(label=data_fsnps[i,label],
                 pred = predict(fit,as.matrix(data_fsnps[i,predictors],nrow=1))
      )
   })
   
   # summarise predictions
   res = do.call(rbind,res)
   
   # prepare the confusion matrix from the results
   confMatrix = confusionMatrix(res$pred,data_fsnps$Pop, mode = "everything")
   
   # convert to table, get pred and ref values
   pred = as.data.frame(confMatrix$table)
   
   predWide = tidyr::pivot_wider(data = pred,
                                 names_from = Reference,
                                 values_from = Freq)
   
   # access accuracy values
   predStat = as.data.frame(confMatrix$overall)
   
   # get other stats
   otherStat = as.data.frame(confMatrix$byClass)
   
   return(list(
      predTable = predWide,
      predStat = predStat,
      otherStat = otherStat
   ))
}

