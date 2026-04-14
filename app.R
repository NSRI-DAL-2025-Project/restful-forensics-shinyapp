library(bslib)
library(dplyr)
library(shiny)
library(shinyjs)
library(shinydashboard)
library(shinybusy)
source("functions.R", local = TRUE)
source("dal_functions.R", local = TRUE)
source("global.R", local = TRUE)

options(shiny.maxRequestSize = 5000*1024^2)

ui <- dashboardPage(
   dashboardHeader(
      title = div(
         tags$span("restFUL Forensics",
                   style = "font-family: Carme, sans-serif; font-size: 26px; color: #ffffff; vertical-align: middle; padding-left: 0px;")
      ),
      titleWidth = 300,
      tags$li(
         class = "dropdown",
         style = "font-family: 'Carmen', sans-serif; padding: 10px;",
         actionButton("refreshApp", "Refresh App", icon = icon("rotate"))
      )
   ),
   dashboardSidebar(
      width = 300,
      sidebarMenu(
         id = "tabs",
         tags$head(tags$style("img {max-width: 100%; height: auto; }")),
         tags$img(src = "readme/full.png", height = "auto", width = "300px", height = "100px"),
         menuItem("Homepage", tabName = "dashboard", icon = icon("dashboard")),
         menuItem("File Conversion", tabName = "FileConv", icon = icon("arrows-rotate")),
         menuItem("SNP Data Extraction", tabName = "markerExtract", icon = icon("dna")),
         menuItem("Filtering", tabName = "FilterTab", icon = icon("filter")),
         menuItem("Exploratory Analysis", tabName = "PCAtab", icon = icon("magnifying-glass-location")),
         menuItem("Population Summary Statistics", tabName = "PopStatistics", icon = icon("users-gear")),
         menuItem("Population Structure Analysis", tabName = "PopStructure", icon = icon("square-poll-vertical")),
         menuItem("Forensic Parameters", tabName = "ForensicParams", icon = icon("magnifying-glass")), # addition 12 March 2026
         menuItem("Forensic DNA Inference", tabName = "Classification", icon = icon("diagram-project")),
         menuItem("DNA Barcoding", icon = icon("chart-bar"),
                  menuSubItem("Multiple Sequence Alignment", tabName = "MSAtab"),
                  menuSubItem("Phylogenetic Tree Analysis", tabName = "PhylogenAnalysis"),
                  menuSubItem("Barcoding", tabName = "BarcodingTab")
                  ),
         menuItem("References", tabName = "AppRef", icon = icon("book-bookmark")),
         menuItem("About", tabName = "About", icon = icon("building-user"))
      ),
      h5("© 2025 DNA Analysis Laboratory, Natural Sciences Research Institute, University of the Philippines Diliman. All rights reserved.")

   ),
   dashboardBody(
      useShinyjs(),
      tags$head(
         includeCSS("www/custom.css") 
      ),
      shinybusy::add_busy_spinner(spin = "fading-circle", position = "full-page"),
      tags$style(HTML("
                      .shinybusy-overlay {
                      background-color: rgba(0,0,0,0.25) !important;
                      }
                      ")),
      tabItems(
         tabItem(tabName = "dashboard",
                 fluidRow(
                    tabBox(
                       title = "Introduction",
                       side = "right",
                       width = 12,
                       h4("restFUL forensics is a toolkit dedicated for the forensic analysis of 
                          single nucleotide polymorphisms (SNPs) and DNA barcodes. It compiles 
                          reference population datasets extracted from publicly available databases 
                          for direct evaluation of forensic marker panels. It allows the user to 
                          analyze their dataset with compiled reference datasets, perform exploratory 
                          data analysis (i.e. principal component analysis), and calculate population 
                          summary statistics and forensic parameters. This version also contains modules 
                          for population structure analysis (i.e. STRUCTURE), forensic DNA 
                          inference/classification using ancestry or phenotype-informative SNPs, and DNA 
                          barcoding (i.e. multiple sequencing alignment).")
                    )
                 ),
                 fluidRow(
                    tabBox(
                       title = "Overview of Features",
                       width = 12,
                       tabPanel("Workplan",
                     
                     # Zoom features adapted from: https://forum.posit.co/t/zoom-in-zoom-out-in-r-shiny-while-working-with-images/183567
                          div(style = "display:flex; justify-content: space-evenly; margin-bottom:10px;"),
                       actionButton("smaller_workplan", "-"),
                       actionButton("bigger_workplan", "+"),
                       div(style = "overflow:auto; text-align:center;",
                           uiOutput("workplanImg"))),
                       tabPanel("File Conversion Options",
                                div(style = "display:flex; justify-content: space-evenly; margin-bottom:10px;"),
                                actionButton("smaller_fc", "-"),
                                actionButton("bigger_fc", "+"),
                                div(style = "overflow:auto; text-align:center;",
                                    uiOutput("fileConvTable"))
                       )
                    )
                 )
         ),
         
         tabItem(tabName = "FileConv",
                 tabsetPanel(
                    tabPanel("Convert Files",
                             fluidRow(
                                box(
                                   title = "File Conversion Options",
                                   width = 5,
                                   radioButtons("inputType1", "Choose starting file type",
                                                choices = c("VCF file" = "vcf1",
                                                            "BCF file" = "bcf1",
                                                            "PLINK files (.bed/.bim/.fam)" = "plink1",
                                                            "CSV file" = "csv1")),
                                   
                                   conditionalPanel(
                                      condition = "input.inputType1 == 'vcf1'",
                                      fileInput("VCFFile", "Upload VCF File", accept = c(".vcf")),
                                      radioButtons("inputType2_vcf", "Choose final file type",
                                                   choices = c("PLINK files (.bed/.bim/.fam)" = "plink2",
                                                               "CSV file" = "csv2",
                                                               "FASTA file" = "fasta")),
                                      
                                      conditionalPanel(
                                         condition = "input.inputType2_vcf == 'fasta'",
                                         fileInput("FASTARef", "Reference sequence in FASTA format.", accept = c(".fasta", ".faa", ".fas"))
                                      ),
                                      
                                      conditionalPanel(
                                         condition = "input.inputType2_vcf == 'csv2'",
                                         radioButtons("poptype_vcf", "Do samples come from a single population?",
                                                      choices = c("Yes" = "single", "No" = "multiplepop")),
                                         conditionalPanel(
                                            condition = "input.poptype_vcf == 'multiplepop'",
                                            fileInput("multiplepop_vcf", "Reference file with sample ID and population", accept = c(".xlsx", ".csv")),
                                            helpText("*Accepts XLSX and CSV files"),
                                            
                                            # --- Breakdowns
                                            radioButtons("breakdown_vcf", "Calculate population breakdown?",
                                                         choices = c("Yes" = "yesbreakdown_vcf", "No" = "nobreakdown_vcf"), selected = "No"),
                                            conditionalPanel(
                                               condition = "input.breakdown_vcf == 'yesbreakdown_vcf'",
                                               helpText("Specify column name to serve as a basis for the summary count."),
                                               textAreaInput("breakdown_column_vcf", "Enter column name", rows = 1)
                                               
                                            )
                                         ),
                                         conditionalPanel(
                                            condition = "input.poptype_vcf == 'single'",
                                            textAreaInput("typePop_vcf", "Enter population", rows = 1)
                                         )
                                      ) # end of conditional for csv2
                                   ),
                                   
                                   conditionalPanel(
                                      condition = "input.inputType1 == 'bcf1'",
                                      fileInput("BCFFile", "Upload BCF File", accept = c(".bcf")),
                                      radioButtons("inputType2_bcf", "Choose final file type",
                                                   choices = c("VCF file" = "vcf2",
                                                               "PLINK files (.bed/.bim/.fam)" = "plink2",
                                                               "CSV file" = "csv2")),
                                      
                                      conditionalPanel(
                                         condition = "input.inputType2_bcf == 'csv2'",
                                         radioButtons("poptype_bcf", "Do samples come from a single population?",
                                                      choices = c("Yes" = "single", "No" = "multiplepop")),
                                         conditionalPanel(
                                            condition = "input.poptype_bcf == 'multiplepop'",
                                            fileInput("multiplepop_bcf", "Reference file with sample ID and population", accept = c(".xlsx", ".csv")),
                                            helpText("*Accepts XLSX and CSV files"),
                                            
                                            # --- Breakdowns
                                            radioButtons("breakdown_bcf", "Calculate population breakdown?",
                                                         choices = c("Yes" = "yesbreakdown_bcf", "No" = "nobreakdown_bcf"), selected = "No"),
                                            conditionalPanel(
                                               condition = "input.breakdown_bcf == 'yesbreakdown_bcf'",
                                               helpText("Specify column name to serve as a basis for the summary count."),
                                               textAreaInput("breakdown_column_bcf", "Enter column name", rows = 1)
                                               
                                            )
                                         ),
                                         conditionalPanel(
                                            condition = "input.poptype_bcf == 'single'",
                                            textAreaInput("typePop_bcf", "Enter population", rows = 1)
                                         )
                                      )
                                   ),
                                   
                                   conditionalPanel(
                                      condition = "input.inputType1 == 'plink1'",
                                      fileInput("bedFile", "Upload BED File", accept = c(".bed")),
                                      fileInput("bimFile", "Upload BIM File", accept = c(".bim")),
                                      fileInput("famFile", "Upload FAM File", accept = c(".fam")),
                                      radioButtons("inputType2_plink", "Choose final file type",
                                                   choices = c("VCF file" = "vcf2",
                                                               "CSV file" = "csv2")),
                                      
                                      conditionalPanel(
                                         condition = "input.inputType2_plink == 'csv2'",
                                         radioButtons("poptype_plink", "Do samples come from a single population?",
                                                      choices = c("Yes" = "single", "No" = "multiplepop")),
                                         conditionalPanel(
                                            condition = "input.poptype_plink == 'multiplepop'",
                                            fileInput("multiplepop_plink", "Reference file with sample ID and population", accept = c(".xlsx", ".csv")),
                                            helpText("*Accepts XLSX and CSV files"),
                                            
                                            
                                            # --- Breakdowns
                                            radioButtons("breakdown_plink", "Calculate population breakdown?",
                                                         choices = c("Yes" = "yesbreakdown_plink", "No" = "nobreakdown_plink"), selected = "No"),
                                            conditionalPanel(
                                               condition = "input.breakdown_plink == 'yesbreakdown_plink'",
                                               helpText("Specify column name to serve as a basis for the summary count."),
                                               textAreaInput("breakdown_column_plink", "Enter column name", rows = 1)
                                               
                                            )
                                         ),
                                         conditionalPanel(
                                            condition = "input.poptype_plink == 'single'",
                                            textAreaInput("typePop_plink", "Enter population", rows = 1)
                                         )
                                      )
                                   ),
                                   
                                   conditionalPanel(
                                      condition = "input.inputType1 == 'csv1'",
                                      p("This feature automatically converts a CSV file to VCF"),
                                      fileInput("CSVFile", "Upload CSV File", accept = c(".csv")),
                                      fileInput("lociMetaFile", "Upload loci/marker information", accept = c(".xlsx", ".csv"))
                                   ),
                                   
                                   actionButton("ConvertFILES", "Convert files", icon = icon("file-csv"))
                                ),
                                
                                tabBox(
                                   tabPanel("Instructions",
                                            h4("This interconverts common genetic files and formats with or without population information."),
                                            p(strong("Input file/s:")),
                                            p("Required: VCF, BCF, or PLINK (.bed, .bim, .fam) files. It also accepts zipped files as long as it contains the same file type."),
                                            p("Optional input files:"),
                                            tags$ul(
                                               tags$li("(to .csv) .xlsx/.csv/.txt file containing metadata (sample ID and population). All columns will be merged to the genotype data. Remove unnecessary columns before running 'Convert files'."),
                                               tags$li("(to .csv) Single-line text indicating the 'Column name' to be used as basis for the calculation of population breakdown."),
                                               tags$li("(.vcf to FASTA) Reference sequence in FASTA (.fasta, .fas, .faa) format."),
                                               tags$li("(.csv to .vcf) Marker information with the following columns: [1] SNP, [2] CHR, [3] POS, [4] Genetic distance, [5] REF Allele [6] ALT Allele.")
                                            ),
                                            p(strong("Expected output file/s:"), "VCF, PLINK, or CSV file."),
                                            br(),
                                            p("Maximum accepted file size: 5GB. It is recommended to split files with sizes larger than 5GB into multiple smaller files.")
                                   ),
                                   tabPanel("Sample Input Format/s", 
                                            h4("To convert to a CSV file with population metadata:"),
                                            h4("This is a sample reference file. Only the first two columns are used."),
                                            DT::dataTableOutput("ExampleRefFile"),
                                            br(),
                                            h4("For CSV to VCF conversion, a separate file on marker information is needed."),
                                            h4("See the following formats:"),
                                            h4("Required CSV format:"),
                                            DT::dataTableOutput("ExampleCSVFormat"),
                                            h4("Required marker info format:"),
                                            DT::dataTableOutput("markerInfoFormat")
                                   ),
                                   tabPanel("Download Sample Files", 
                                            tags$a("A. Sample VCF", href = "sample_hgdp.vcf", download = "sample_hgdp.vcf"),
                                            br(),
                                            tags$a("B. Sample CSV file (for VCF conversion)", href = "sample.csv", download = "sample.csv"),
                                            br(),
                                            tags$a("C. Sample marker metadata file (for CSV-VCF conversion)", href = "marker_info.csv", download = "marker_info.csv")
                                   )
                                )
                             ), # end of first fluid row
                             fluidRow(
                                tabBox(
                                   title = "Conversion Results",
                                   width = 12,
                                   tabPanel(
                                      title = "Preview CSV File and Download Output",
                                      div(
                                         style = "overflow-x: auto;",
                                         DT::dataTableOutput("previewTable")
                                      ),
                                      br(),
                                      uiOutput("downloadVCF_UI"),
                                      uiOutput("downloadCSV_UI"),
                                      uiOutput("downloadFASTA_UI"),
                                      uiOutput("downloadPLINK_UI")
                                   ),
                                   tabPanel(
                                      title = "(to CSV) View Population Breakdown",
                                      div(
                                         style = "overflow-x: auto;",
                                         DT::dataTableOutput("previewTableBreakdown")
                                      )
                                   )
                                )   
                             )
                    ),
                    
                    tabPanel("ForenSeq Conversion",
                             fluidRow(
                                box(
                                   fileInput("uas_zip", "Upload ZIP or TAR file",
                                             accept = c(".zip", ".tar")),
                                   helpText("*Accepts compressed files containing XLSX files."),
                                   fileInput("ref_file", "Optional Reference File (CSV or XLSX)",
                                             accept = c(".csv", ".xlsx")),
                                   actionButton("run_uas2csv", "Run Conversion")
                                ),
                                tabBox(
                                   tabPanel("Instructions",
                                            h4("This converts zipped ",
                                              tags$a("ForenSeq UAS",
                                                     href="https://verogen.com/products/universal-analysis-software/",
                                                     target="_blank"), "phenotype reports into a single file in a wide format."),
                                            p("This section builds upon the work of Ms. Maeviviene Sosing as part of the Filipino Genomes Research Program 2"),
                                            p(strong("Input file/s:"), "Compressed folder (.zip or .tar) containing .xlsx files."),
                                            p(strong("Expected output file/s:"), "Single CSV file (merged .xlsx files).")
                                   ),
                                   tabPanel("Sample Input Format/s", 
                                            h4("Sample input file. All alleles of available SNPs per sample are listed in a long format."),
                                            tableOutput("exampleXLSX")
                                   )#,
                                   #--------- Confidential sample data
                                 #  tabPanel("Download Sample Zipped File", 
                                 #           tags$h4("Downloadable Sample"),
                                 #           tags$ul(
                                 #              tags$a("Sample zipped file", href = "www/sample_forenseq.zip", download = NA))
                                   #)
                                )
                             ),
                             fluidRow(
                                tabBox(
                                   title = "Conversion Results",
                                   width = 12,
                                   tabPanel(
                                      title = "Preview and Download",
                                      div(
                                         style = "overflow-x: auto;", 
                                         DT::dataTableOutput("previewTableUAS")
                                      ),
                                      br(),
                                      uiOutput("downloadUAScsv_UI")
                                   )
                                )
                             )
                    ), # end of forenseq conversion
                    
                    tabPanel("Convert to SNIPPER analysis-ready file",
                             fluidRow(
                                box(
                                   width = 6,
                                   fileInput("convertFile", "Upload File", accept = c(".xlsx", ".csv")),
                                   checkboxInput("refProvided", "Reference populations included in the file?", value = TRUE),
                                   helpText("See 'sample input formats' for guidance. If 'TRUE', assuming population metadata is included."),
                                   conditionalPanel(
                                      condition = "input.refProvided == false",
                                      fileInput("refFile", "Upload Reference File", accept = c(".xlsx", ".csv"))
                                   ),
                                   checkboxInput("targetPop", "Classify a Target Population?", value = FALSE),
                                   conditionalPanel(
                                      condition = "input.targetPop == true",
                                      textInput("targetPopName", "Target Population Name"),
                                      helpText("Indicate population name to classify. Population should exist in the input file.")
                                   ),
                                   actionButton("convertBtn", "Convert Format", icon = icon("arrow-up-right-from-square"))
                                ), # end of input tabbox
                                tabBox(
                                   tabPanel("Instructions",
                                            p("The ",
                                               tags$a("SNIPPER app suite",
                                                      href="https://mathgene.usc.es/snipper/index.php",
                                                      target="_blank"), " is a web portal for classification of individuals into phenotypes and biogeographical ancestry (BGA), developed and hosted by UniversIDade de Santiago de Compostela. SNIPPER is a companion site to several ",
                                               tags$a("publications",
                                                      href="https://mathgene.usc.es/snipper4/papers.php",
                                                      target = "_blank"
                                               )
                                            ),
                                            br(),
                                            h4("This converts CSV or Excel (.xlsx) files to a SNIPPER-compatible file"),
                                            p(strong("Input file/s:"), "CSV or Excel (.xlsx) file."),
                                            p(strong("Parameter/s:"), "(optional) Target population name for classification."),
                                            p(strong("Expected output file/s:"), "Excel file (.xlsx)")
                                   ),
                                   tabPanel("Sample Input Format/s",
                                            h4("Sample Input File"),
                                            p("Format if input file does", strong("not"), "contain population metadata:"),
                                            DT::dataTableOutput("exampleTableSnipper1"),
                                            br(),
                                            p("Format if input file", strong("contains"), "population metadata:"),
                                            DT::dataTableOutput("exampleTableSnipper2"),
                                            br(),
                                            h4("Sample reference file"),
                                            DT::dataTableOutput("exampleRefSnipper")
                                   ),
                                   tabPanel("Download Sample File",
                                            h4("Downloadable Sample"),
                                                tags$ul(
                                                   tags$a("Sample zipped file", href = "sample_snipper.csv", download = "sample_snipper.csv"))
                                            )
                                ) # end of tabbox for instruction
                             ), # end of fluidrow
                             fluidRow(
                                tabBox(
                                   title = "Conversion Result",
                                   width = 12,
                                   tabPanel(
                                      title = "Preview and Download",
                                      div(
                                         style = "overflow-x: auto;",
                                         DT::dataTableOutput("previewTableSNIPPER")
                                      ),
                                      br(),
                                      uiOutput("downloadSNIPPER")
                                   )
                                )
                             )
                    ), # end of tabpanel for snipper
                    
                    tabPanel("To STRUCTURE file",
                             fluidRow(
                                box(
                                   width = 6,
                                   fileInput("tostrFile", "Upload CSV/XLSX file", accept = c(".xlsx", ".csv")),
                                   helpText("Use the 'Convert files to CSV' file if using VCF, BCF, or PLINK files. Population data is necessary."),
                                   radioButtons("systemFile", "Choose the operating system where STRUCTURE v2.3.4 is installed",
                                                choices = c("Linux" = "Linux", "Windows" = "Windows")),
                                   actionButton("csv2str", "Generate STRUCTURE File", icon = icon("arrow-up-right-from-square"))
                                ),
                                tabBox(
                                   tabPanel("Instructions",
                                            p(tags$a("STRUCTURE",
                                                     href="https://web.stanford.edu/group/pritchardlab/structure.html",
                                                     target="_blank"), " is a free software package for investigating population structure using 
                                              multi-locus genotype data (Stephen and Donnelly, 2000; Falush et al., 2003; 
                                              Falush et al., 2007; Hubisz et al., 2009).",
                                              tags$a("publications",
                                                     href="https://mathgene.usc.es/snipper4/papers.php",
                                                     target = "_blank"
                                              )
                                            ),
                                            br(),
                                            p("This converts CSV files to STRUCTURE-compatible files. This module was tested on STRUCTURE version 2.3.4."),
                                            p(strong("Input file/s:"), "CSV file containing marker and population data.
                                              Each row should represent multi-locus data for an individual sample."),
                                            p(strong("Parameter/s:"), "User's operating system (for STRUCTURE input compatibility)"),
                                            p(strong("Expected output file/s:")),
                                            tags$ul(
                                               tags$li("structure (.str) file"),
                                               tags$li("revised input file")
                                            ),
                                            br(),
                                            p("STRUCTURE generally can't handle sample labels with alphabets, the function converts sample labels to their associated row number."),
                                            p("For users who opt to use STRUCTURE via the terminal or GUI, instructions can be found here: ",
                                              tags$a("STRUCTURE v2.3.4 documentation",
                                                     href="https://web.stanford.edu/group/pritchardlab/structure_software/release_versions/v2.3.4/html/structure.html",
                                                     target="_blank"))
                                   ),
                                   tabPanel("Sample Input Format",
                                            DT::dataTableOutput("examplePop_STRUI")
                                            )
                                ) # end of tabbox
                             ), # end of fluidrow
                             fluidRow(
                                tabBox(
                                   title = "Conversion Result",
                                   width = 12,
                                   tabPanel(
                                      title = "Preview and Download",
                                      tableOutput("revisedCSV"),
                                      tableOutput("strFile"),
                                      br(),
                                      uiOutput("downloadrevised_UI"),
                                      uiOutput("downloadSTRfile_UI")
                                   )
                                )
                             )
                    ) # end of tabpanel for structure
                 )
         ),
         
         tabItem(tabName = "markerExtract",
                 tabsetPanel(
                    tabPanel("SNP Data Extraction",
                             fluidRow(
                                box(
                                   width = 5,
                                   radioButtons("inputFileType", "A. Input file format",
                                                choices = c("VCF/VCF.GZ/BCF", "PLINK"), inline = TRUE),
                                   
                                   conditionalPanel(
                                      condition = "input.inputFileType == 'VCF/VCF.GZ/BCF'",
                                      fileInput("markerFile", "Upload Genotype (VCF, VCF.GZ, or BCF File)", accept = c(".vcf", ".bcf", ".vcf.gz"))
                                   ),
                                   
                                   conditionalPanel(
                                      condition = "input.inputFileType == 'PLINK'",
                                      fileInput("bedFile", "PLINK BED file", accept = c(".bed")),
                                      fileInput("bimFile", "PLINK BIM file", accept = c(".bim")),
                                      fileInput("famFile", "PLINK FAM file", accept = c(".fam")),
                                      helpText("If multiple PLINK files will be used, upload as a zipped file."),
                                      fileInput("zippedPLINK", "Zipped PLINK Files", accept = c(".zip", ".tar"))
                                   ),
                                   actionButton("validateBtn", "Validate Input File Format", icon = icon("check")),
                                   uiOutput("markerOptionsUI") 
                                ),
                                
                                tabBox(
                                   tabPanel("Instructions",
                                            h4("This extracts SNPs based on reference SNP cluster ID (rsID) or GRCh37/GRCh38 position using PLINK 2.0 (Chang et al., 2015)."),
                                            p(strong("Input file/s:")),
                                            p("(1) VCF, BCF, or PLINK (.bed, .bim, .fam) files."),
                                            p("(2) Markers/position list — type rsIDs manually, upload a list, or use a POS .txt/.csv file."),
                                            p("Position list format:"),
                                            tags$ul(
                                               tags$li("[1] rsID/marker name"),
                                               tags$li("[2] Chromosome number"),
                                               tags$li("[3] Position (bp)")
                                            ),
                                            p(strong("Expected output file:"), "VCF File"),
                                            br(),
                                            p("Maximum accepted file size: 5GB. It is recommended to split files with sizes larger than 5GB into multiple smaller files.")
                                   ),
                                   tabPanel("Sample Input Format",
                                            h4("rsID Format"),
                                            tableOutput("examplersID"),
                                            h4("Position Format"),
                                            tableOutput("examplePOS")
                                   ),
                                   tabPanel("Download Sample Files",
                                            h4("Sample Files"),
                                            tags$ul(
                                               tags$a("Sample VCF file", href = "sample_hgdp.vcf", download = "sample_hgdp.vcf"),
                                               br(),
                                               tags$a("Sample marker metadata file", href = "marker_info.csv", download = "marker_info.csv")
                                            )
                                   )
                                )
                             ), # end fluidRow
                             fluidRow(
                                tabBox(
                                   width = 12, 
                                   tabPanel(
                                      title = "Detected rsIDs",
                                      DT::DTOutput("variantTable")
                                   ),
                                   tabPanel(
                                      title = "Extraction Results",
                                      uiOutput("downloadVCF_UI"),
                                      helpText("Note: File can appear as vCard files, it is still a VCF file.")
                                   )
                                )
                             )
                            ), # end extraction
                    
                    # SNP Extraction: Concordance Analysis
                    tabPanel("Concordance Analysis",
                             fluidRow(
                                box(
                                   fileInput("concordanceFile1", "Upload File A", accept = c(".xlsx", ".csv")),
                                   fileInput("concordanceFile2", "Upload File B", accept = c(".xlsx", ".csv")),
                                   checkboxInput("isHaplotype", "Treat data as haplotypes", value = FALSE),
                                   actionButton("compareBtn", "Run Concordance Analysis", icon = icon("play"))
                                ), # end of first tabbox
                                tabBox(
                                   tabPanel("Instructions",
                                            h4("This performs concordance analysis between files/datasets with overlapping samples."),
                                            p(strong("Input file/s:"), "Two .csv or .xlsx files with the same data format (i.e. same columns)."),
                                            p(strong("Parameter/s:"), "Indicate if using haplotypes."),
                                            p(strong("Expected output/s:")),
                                            tags$ul(
                                               tags$li("Concordance table (.xlsx)"),
                                               tags$li("Concordance plot (.png)")
                                            )
                                   ), # end of tP instructions
                                   tabPanel("Sample Input Format/s",
                                            h4("File Format (for concordance)"),
                                            tableOutput("exampleTable")
                                   ), # end of tP sample files 
                                   tabPanel("Download Sample Files",
                                            h4("Sample Files"),
                                            tags$ul(
                                               tags$a("Sample CSV file (1)", href = "sample1_for_concordance.csv", download = "sample1_for_concordance.csv"),
                                               br(),
                                               tags$a("Sample CSV file (2)", href = "sample2_for_concordance.csv", download = "sample2_for_concordance.csv")
                                            )
                                   )
                                ) # end of second tabbox
                             ), # end of fluidrow
                             fluidRow(
                                tabBox(
                                   title = "Concordance Results",
                                   width = 12,
                                   tabPanel("Summary Table",
                                      div(
                                         style = "overflow-x: auto;",
                                         DT::dataTableOutput("concordanceResults")
                                      ),
                                      br(),
                                      uiOutput("downloadConcordance_UI")
                                   ),
                                   tabPanel("Concordance Plot",
                                      div(
                                         style = "overflow-x: auto;",
                                         plotOutput("concordancePlot", height = "600px"),
                                         br(),
                                         uiOutput("downloadConcordancePlot_UI")
                                      )
                                   )
                                )
                             )
                    ) # end of tabpanel for concordance
                 ) # tabsetpanel
         ),
         tabItem(tabName = "FilterTab",
                 fluidRow(
                    box(
                       title = "Upload Files",
                       #fileInput("forFilter", "Upload VCF/BCF/PLINK files", accept = c(".vcf", ".bcf", ".vcf.gz", ".zip", ".tar")),
                       radioButtons("inputFileTypeFilter", "A. Input file format",
                                    choices = c("VCF/VCF.GZ/BCF", "PLINK"), inline = TRUE),
                       
                       conditionalPanel(
                          condition = "input.inputFileTypeFilter == 'VCF/VCF.GZ/BCF'",
                          fileInput("markerFileFilter", "Upload Genotype (VCF, VCF.GZ, or BCF File)", accept = c(".vcf", ".bcf", ".vcf.gz"))
                       ),
                       
                       conditionalPanel(
                          condition = "input.inputFileTypeFilter == 'PLINK'",
                          fileInput("bedFileFilter", "PLINK BED file", accept = c(".bed")),
                          fileInput("bimFileFilter", "PLINK BIM file", accept = c(".bim")),
                          fileInput("famFileFilter", "PLINK FAM file", accept = c(".fam"))
                       ),
                       
                       fileInput("highlightRef", "Optional Reference file for highlighting (CSV/XLSX)", accept = c(".xlsx", ".csv")),
                       checkboxInput("enableDP", "Plot Depth of Coverage", value = TRUE),
                       helpText("Depth of Coverage Plot only available if using a VCF file."),
                       selectInput("colorPalette", "Color Palette", choices = rownames(RColorBrewer::brewer.pal.info), selected = "Set2"),
                       hr(),
                       
                       h4("PLINK 2.0 Filtering Options"),
                       checkboxInput("filterIndiv", "Filter Individuals (--mind)", value = FALSE),
                       helpText("Exclude individuals with a missing genotype rate greater than the threshold"),
                       conditionalPanel("input.filterIndiv == true",
                                        numericInput("mindThresh", "Missingness Threshold (--mind)", value = 0.1, min = 0, max = 1, step = 0.01)
                       ),
                       checkboxInput("filterVariant", "Filter Variants (--geno)", value = FALSE),
                       helpText("Exclude SNPs with a missing genotype rate greater than the threshold."),
                       conditionalPanel("input.filterVariant == true",
                                        numericInput("genoThresh", "Missingness Threshold (--geno)", value = 0.1, min = 0, max = 1, step = 0.01)
                       ),
                       checkboxInput("filterAllele", "Filter Variants (--maf)", value = FALSE),
                       helpText("Exclude SNPs with a minor allele frequency less than the threshold."),
                       conditionalPanel("input.filterAllele == true",
                                        numericInput("mafThresh == true", "Minor Allele Frequency Threshold (--maf)", value = 0.1)
                       ),
                       checkboxInput("filterQuality", "Filter by Quality (--qual-threshold)", value = FALSE),
                       helpText("Exclude variants with quality scores below the threshold."),
                       conditionalPanel("input.filterQuality == true",
                                        numericInput("qualThresh == true", "Quality Score Threshold (--qual-threshold)", value = 5)
                       ),
                       checkboxInput("filterHWE", "Filter Variants (--hwe)", value = FALSE),
                       helpText("Exclude SNPs deviating from the Hardy-Weinberg Equilibrium."),
                       conditionalPanel("input.filterHWE == true",
                                        numericInput("qualHWE == true", "Hardy-Weinberg equilibrium exact test p-value Threshold (--hwe)", value = 0.000001, min = 0.0000000001),
                                        numericInput("kval == true", "K parameter (Greer et al. 2024) to adjust p-value threshold", value = 0.001)
                       ),
                       checkboxInput("filterLD", "Filter Variants (--indep-pairwise)", value = FALSE),
                       helpText("Prune markers in approximate linkage equilibrium with each other."),
                       conditionalPanel("input.filterLD == true",
                                        numericInput("ldWindow", "Window Size (kb)", value = 500, min = 1, step = 1),
                                        numericInput("ldStep", "Step Size (variants)", value = 50, min = 1, step = 1),
                                        numericInput("ldR2", "r2 Threshold", value = 0.2, min = 0, max = 1, step = 0.01)
                       ),
                       checkboxInput("cutoffKing", "Filter based on Relationships (--king-cutoff)", value = FALSE),
                       helpText("Exclude a member of a pair with a kinship coefficient greater than the threshold. Use '0.354' to screen for monozygotic twins and duplicate amples, '0.177' for 1st-degree, '0.0884' for 2nd-degree, and '0.0442' for 3rd-degree relationships."),
                       conditionalPanel("input.cutoffKing == true",
                                        selectInput("kingThresh", "Kinship Coefficient", choices = c("0.354", "0.177", "0.0884", "0.0442"), selected = "0.354")
                                        ),
                       textInput("customFilter", "Additional PLINK flags", placeholder = "--keep filestokeep.txt"),
                       fileInput("extraFile1", "Optional file for first flag", accept = c(".txt", ".ped", ".psam", ".pheno", ".xlsx", ".csv")),
                       fileInput("extraFile2", "Optional file for second flag", accept = c(".txt", ".ped", ".psam", ".pheno", ".xlsx", ".csv")),
                       helpText("Upload extra files only if required by additional PLINK flags."),
                       actionButton("calcDP", "Run Filtering & Plotting", icon = icon("filter")),
                       textOutput("filterWarning")
                    ),
                    tabBox(
                       tabPanel("Instructions",
                                h4("This filters individuals and variants using standard options in PLINK 2.0 (Chang et al., 2015)."),
                                p(strong("Input file/s:"), ".vcf, .vcf.gz, .bcf, or PLINK files"),
                                p(strong("Parameter/s:")),
                                tags$ul(
                                   tags$li("--mind [value]"),
                                   tags$li("--geno [value]"),
                                   tags$li("--maf [value]"),
                                   tags$li("--qual-threshold [value]"),
                                   tags$li("--hwe [value]"),
                                   tags$li("--indep-pairwise [value]"),
                                   tags$li("--king-cutoff [value]"),
                                   tags$li("Other additional PLINK flags")
                                ),
                                p(strong("Expected output/s:")),
                                tags$ul(
                                   tags$li("VCF file"),
                                   tags$li("Depth of Coverage Plots")
                                ),
                                br(),
                                p("Standard filtering flags are indicated. For other PLINK flags, see the following for options to be specified in the 'Additional PLINK flags' text box: ",
                                  tags$a("PLINK 2.0 Documentation",
                                         href="https://www.cog-genomics.org/plink/2.0/",
                                         target="_blank"))
                       ),
                       tabPanel("Download sample files",
                                tags$a("Sample VCF", href = "sample_hgdp.vcf", download = "sample_hgdp.vcf")
                       )
                    ), # end of tabBox for instructions
                    tabBox(
                       tabPanel("PLINK Commands Preview",
                                verbatimTextOutput("plinkCommandPreview"),
                       ),
                       tabPanel("Depth Plots",
                                imageOutput("depthMarkerPlot"),
                                imageOutput("depthSamplePlot")
                       ),
                       tabPanel("Download Files",
                                uiOutput("depthMarkerPlot_UI"),
                                uiOutput("depthSamplePlot_UI"),
                                uiOutput("downloadFilteredFile_UI")
                       )
                    )
                 ) # end of fluid row
         ), # end of tabItem for filtering
         
         tabItem(tabName = "MSAtab",
               #  tabsetPanel(
                    tabPanel("Multiple Sequence Alignment",
                             fluidRow(
                                box(
                                   fileInput("fastaFile", "Upload zipped FASTA files", accept = c(".zip", ".tar")),
                                   radioButtons("substitutionMatrix", "Choose Substitution Matrix for MSA",
                                                choices = c("ClustalW" = "ClustalW", "ClustalOmega" = "ClustalOmega", "MUSCLE" = "Muscle")),
                                   actionButton("runMSA", "Align", icon = icon("align-justify")),
                                   br(),
                                   selectInput("msaDownloadType", "Choose alignment (FASTA and PDF) version to download:",
                                               choices = c(
                                                  "Initial" = "initial",
                                                  "Adjusted" = "adjusted",
                                                  "Staggered" = "staggered"
                                               ),
                                               selected = "initial")
                                ), 
                                tabBox(
                                   tabPanel("Instructions",
                                            h4("This performs multiple sequence alignment using the msa R package (Bodenhofer et al., 2015) 
                                               and post-processing using DECIPHER (Wright, 2015)."),
                                            p(strong("Input file/s:"), "Zipped folder (.zip) containing FASTA files."),
                                            p(strong("Parameter/s:"), "Substitution matrix for the alignment (ClustalW, ClustalOmega, MUSCLE)"),
                                            p(strong("Expected output/s:")),
                                            tags$ul(
                                               tags$li("Aligned sequences"),
                                               tags$li("Alignment scores"),
                                               tags$li("Alignment PDF")
                                            ),
                                            p("PDF of the alignment is automatically downloaded in the working directory of the repository.
                                              This is a native feature of the msaPrettyPrint() function."),
                                            br(),
                                            p("The aligned sequences can be used in the tab ",
                                              tags$a(actionLink("tophylogentab", "'Phylogenetic Tree Analysis'."))
                                              )
                                   ),
                                   tabPanel("Download Sample File",
                                            h4("Sample Files"),
                                            tags$ul(
                                               tags$a("Sample zipped FASTA file", href = "lacto2.zip", download = "lacto2.zip")),
                                   )
                                ),
                                tabBox(
                                   width = 12,
                                   tabPanel("Preview of Alignments",
                                            msaR::msaROutput("msaView", width = "100%"),
                                            br(),
                                            verbatimTextOutput("initialAlignmentText"),
                                            br(),
                                            verbatimTextOutput("adjustedAlignmentText"),
                                            br(),
                                            verbatimTextOutput("staggeredAlignmentText"),
                                            br(),
                                            verbatimTextOutput("alignmentScoresPreview")
                                   ),
                                   tabPanel("Download Results",
                                            uiOutput("downloadAlignedFASTA_UI"),
                                            uiOutput("downloadAlignmentScores_UI"),
                                            downloadButton("downloadAlignmentPDF", "Download Alignment in PDF"),
                                            p("Note that the PDF is automatically downloaded in the same folder/working directory. This is a built in feature of the msa R package.")
                                   )
                                )
                             ) # end of fluid row
                    ) # end of tabPanel
         ), # end of tabset for msa

         tabItem(tabName = "PhylogenAnalysis",
                    tabPanel("Phylogenetic Tree Analysis",
                             fluidRow(
                                box(
                                   checkboxInput("uploadMSA", "Use results from the MSA tab?", value = FALSE),
                                   
                                   conditionalPanel(
                                     condition = "input.uploadMSA == true",
                                     selectInput("treeAlignmentType", "Use alignment for tree construction:",
                                                 choices = c(
                                                    "Initial" = "initial",
                                                    "Adjusted" = "adjusted",
                                                    "Staggered" = "staggered"
                                                 ),
                                                 selected = "initial"
                                     )
                                   ),
                                   conditionalPanel(
                                      condition = "input.uploadMSA == false",
                                      fileInput("msaFileforPhylogen", "Upload MSA file"),
                                      helpText("Accepted MSA formats: .msa, .fasta, .msf, .aln, .faa, .fas")
                                   ),
                                   
                                   selectInput("treeType", "Choose Method for Tree Construction",
                                               choices = c("NJ", "UPGMA", "Parsimony", "Maximum Likelihood")),
                                   
                                   conditionalPanel(
                                      condition = "input.treeType == 'NJ' || input.treeType == 'UPGMA'",
                                      selectInput("model", "Choose Substitution Model",
                                                  choices = c("N", "TS", "TV", "JC69", "K80", "F81", "K81", "F84", "BH87", "T92", "TN93", "GG95"),
                                                  selected = "K80")
                                   ),
                                   
                                   conditionalPanel(
                                      condition = "input.treeType == 'Maximum Likelihood'",
                                      textInput("boostrapSamples", "Set number of bootstrap samples", placeholder = "100")
                                   ),
                                   
                                   textInput("outgroup", "Outgroup (optional)", placeholder = "e.g. Sample1"),
                                   textInput("seed", "Set Seed Value", placeholder = "123"),
                                   actionButton("buildTree", "Build Tree", icon = icon("tree"))
                                ),
                                tabBox(
                                   title = "Instructions",
                                   p("This performs phylogenetic tree reconstruction using ape (Paradis and Strimmer, 2004) and phangorn (Schliep, 2011) R packages
                                      on multiple sequence alignments. Outputs generated from the ",
                                    tags$a(actionLink("tomsatab", "'Multiple Sequence Alignment'")), " tab are also accepted."
                                      ),
                                   p("Approaches to tree construction are Neighbor Joining (NJ), Unweight Pair Group Method using Arithmetic averages (UPGMA), Maximum Parsimony, and Maximum Likelihood. Check the assumptions and constraints of each approach ",
                                     tags$a("(Zou et al., 2024)",
                                            href="https://pmc.ncbi.nlm.nih.gov/articles/PMC11117635/",
                                            target = "_blank"
                                     ), "."
                                     ),
                                   p(strong("Input file"), "used is the alignment output from the Multiple Sequence Alignment tab. There is an option of using the raw, adjusted, or staggered alignment for tree construction."),
                                   p(strong("Parameters"), "vary based on the method."),
                                   p(strong("Expected output"), "is the phylogenetic tree in PNG format.")
                                ),
                                tabBox(
                                   tabPanel("View Results",
                                            h4("Phylogenetic Tree"),
                                            uiOutput("downloadTree_UI"),
                                            p("This tab uses the multiple sequence alignment performed in the previous tab."),
                                            uiOutput("treeImage")
                                   )
                                )
                             )
                    )
         ), # end of tabitem for phylogen
         tabItem(tabName = "BarcodingTab",
                    tabPanel("Barcoding",
                             tabsetPanel(
                                tabPanel("Species Identification",
                                         fluidRow(
                                            box(
                                               fileInput("refBarcoding", "Upload Aligned Reference Sequences"),
                                               fileInput("queBarcoding", "Upload Aligned Query Sequences"),
                                               helpText("The reference and query sequences should have the same length."),
                                               checkboxInput("kmerSelect", "Use k-mer method?", value = FALSE),
                                               conditionalPanel("input.kmerSelect == false",
                                                                selectInput("barcodingMethod", "Select method to train model and infer membership:",
                                                                            choices = c("fuzzyId", "bpNewTraining", "bpNewTrainingOnly", "bpUseTrained", "Bayesian"),
                                                                            selected = "bpNewTraining")
                                               ),
                                               conditionalPanel("input.kmerSelect == true",
                                                                radioButtons("kmerType", "Choose Method",
                                                                             choices = c("Fuzzy-set Method and kmer", "BP-based Method and kmer")),
                                                                conditionalPanel("input.kmerType == 'Fuzzy-set Method and kmer'",
                                                                                 numericInput("kmerValue", "K-mer value", value = 1, min = 0),
                                                                                 checkboxInput("optimizationKMER", "Use different kmer length?", value = FALSE)
                                                                ),
                                                                conditionalPanel("input.kmerType == 'BP-based Method and kmer'",
                                                                                 numericInput("kmerValue", "K-mer value", value = 1, min = 0),
                                                                                 checkboxInput("builtModel", "Use built model", value = FALSE),
                                                                                 numericInput("lrValue", "Parameter for weight decay", value = 0.00005),
                                                                                 numericInput("maxitValue", "Maximum number of iterations", value = 1000000)
                                                                )
                                               ), # end of conditional panel if kmerselect = true
                                               actionButton("identifySpecies", "Identify Species", icon = icon("magnifying-glass"))
                                            ),
                                            tabBox(
                                               title = "Instructions",
                                               h4("This performs species identification using the R package 'BarcodingR' (Zhang et al., 2016)."),
                                               p(strong("Input file/s:")),
                                               tags$ul(
                                                  tags$li("Aligned reference sequences (.msa, .fasta, .msf, .aln, .faa, .fas)"),
                                                  tags$li("Aligned query sequences (.msa, .fasta, .msf, .aln, .faa, .fas)")
                                               ),
                                               p(strong("Parameter/s:")),
                                               tags$ul(
                                                  tags$li("(without kmer method) Training model: bpNewTraining, fuzzyId, bpNewTrainingOnly, bpUsedTrained, or Bayesian"),
                                                  tags$li("(with kmer method) Fuzzy-set Method or BP-based method")
                                               )
                                            ),
                                            tabBox(
                                               verbatimTextOutput("identificationResult")
                                            )
                                         )
                                ), # end of first tabPanel
                                tabPanel("Optimize kmer values",
                                         fluidRow(
                                            box(
                                               title = "Optimization Options",
                                               width = 6,
                                               fileInput("optimizeKmerRef", "Upload reference dataset"),
                                               numericInput("maxKmer", "Length of maximum kmer value", value = 5, min = 2),
                                               actionButton("calOptimumKmer", "Identify Optimum kmer value", icon = icon("upload"))
                                            ),
                                            tabBox(
                                               title = "Instructions",
                                               width = 6,
                                               tabPanel("Overview",
                                                        h4("This calculates the optimal kmer values using BarcodingR (Zhang et al., 2016)"),
                                                        p(strong("Input file/s:"), "Aligned sequences of the reference dataset (FASTA file)"),
                                                        p(strong("Parameter/s:"), "Length of maximum kmer value"),
                                                        p(strong("Expected output file:"), "Kmer plot (.png)")
                                               )
                                            )
                                         ),
                                         fluidRow(
                                            tabBox(
                                               title = "Results",
                                               width = 12,
                                               tabPanel("Outputs",
                                                        verbatimTextOutput("kmerResult"),
                                                        imageOutput("kmerPlot"),
                                                        uiOutput("downloadKmerPlot_UI")
                                               )
                                            )
                                         )
                                ),
                                tabPanel("Barcoding Gap",
                                         fluidRow(
                                            box(
                                               title = "Gap Calculation Options",
                                               width = 6,
                                               fileInput("barcodeRef", "Upload reference dataset"),
                                               selectInput("gapModel", "Choose Distance",
                                                           choices = c("raw", "K80", "euclidean"),
                                                           selected = "raw"),
                                               actionButton("gapBarcodes", "Calculate gap", icon = icon("arrows-left-right-to-line"))
                                            ),
                                            tabBox(
                                               title = "Instructions",
                                               width = 6,
                                               tabPanel("Overview",
                                                        h4("This calculate the barcoding gap using BarcodingR (Zhang et al., 2016)"),
                                                        p(strong("Input file:"), "VCF file"),
                                                        p(strong("Parameter/s:"), "Distance (raw, K80, euclidean)"),
                                                        p(strong("Expected output file:"), "Barcoding gap plot (.png)")
                                               )
                                            )
                                         ),
                                         fluidRow(
                                            tabBox(
                                               title = "Results",
                                               width = 12,
                                               tabPanel("Outputs",
                                                        verbatimTextOutput("barcodingResult"),
                                                        imageOutput("BarcodingGapPlot"),
                                                        uiOutput("downloadGapPlot_UI")
                                               )
                                            )
                                         )
                                ),
                                tabPanel("Evaluate Barcodes",
                                         fluidRow(
                                            box(
                                               fileInput("barcode1", "Upload Barcode 1"),
                                               fileInput("barcode2", "Upload Barcode 2"),
                                               numericInput("kmer1", "Length of kmer for barcode 1", value = 5, min = 1),
                                               numericInput("kmer2", "Length of kmer for barcode 2", value = 5, min = 1),
                                               actionButton("evalBarcodes", "Evaluate Barcodes", icon = icon("code-compare"))
                                            ),
                                            tabBox(
                                               title = "Instructions",
                                               h4("This evaluate barcodes using species identification success rate criteria (Zhang et al., 2016)"),
                                               p(strong("Input file/s:"), ".csv or .xlsx."),
                                               p(strong("Parameter/s:"), "Length of kmer for barcode 1 and barcode 2 (separate)")
                                            ),
                                            tabBox(
                                               tableOutput("evalBarcodesResult")
                                            )
                                         )
                                ),
                                tabPanel("Species Membership Value (TDR)",
                                         fluidRow(
                                            box(
                                               p("Calculate the TDR2 value"),
                                               fileInput("oneSpe", "Upload DNA seq from a single query species"),
                                               fileInput("queSpe", "Upload DNA seq from different samples"),
                                               numericInput("bootValue1", "Bootstrap value for query species", value = 10, min = 1),
                                               numericInput("bootValue2", "Bootstrap value for reference samples", value = 10, min = 1),
                                               actionButton("calculateTDR2", "Calculate", icon = icon("calculator"))
                                            ),
                                            tabBox(
                                               title = "Instructions",
                                               h4("This calculates the Species Membership Value in terms of 
                                                  Two-Dimensional non-parametric resampling (TDR) using BarcodingR (Zhang et al., 2016)"),
                                               p(strong("Input file/s:"), "CSV file with marker and population data."),
                                               p(strong("Parameter/s:"), "Boostrap value for query and reference samples.")
                                            ),
                                            tabBox(
                                               verbatimTextOutput("tdrValues")
                                            )
                                         )
                                )
                             ) # end of tabsetpanel
                    )
         ), # end of tabset item
                # ) # end of tabsetPanel
         #), # end of tab item for 
         
         tabItem(tabName = "PopStatistics",
                 fluidRow(
                    box(
                       fileInput("popStatsFile", "Upload CSV or XLSX Dataset", accept = c(".zip", ".tar")),
                       actionButton("runPopStats", "Analyze", icon = icon("magnifying-glass-chart")),
                       uiOutput("downloadStatsXLSX_UI")
                    ),
                    tabBox(
                       tabPanel("Instructions",
                                div(
                                   style = "height: 40vh; overflow-y:scroll;",
                                   uiOutput("popstatRef")
                                )
                       ),
                       tabPanel("Sample Input Format/s",
                                h4("Example: Population File Format"),
                                DT::dataTableOutput("examplePop_UI")
                       ),
                       tabPanel("Download Sample Files",
                                h4("Sample File"),
                                tags$ul(
                                   tags$a("Sample CSV file", href = "sample.csv", download = "sample.csv")
                                )
                       )
                    )
                 ), # end of fluid row
                 fluidRow(
                    tabBox(
                       width = 12,
                       tabPanel("1. Private Alleles",
                                h4("Private Alleles Summary"),
                                DT::dataTableOutput("privateAlleleTable")
                       ),
                       tabPanel("2. Mean Allelic Richness",
                                h4("Mean Allelic Richness per site"),
                                DT::dataTableOutput("meanallelic")
                       ),
                       tabPanel("3. Heterozygosity",
                                h4("Observed vs Expected Heterozygosity"),
                                DT::dataTableOutput("heterozygosity_table"),
                                br(),
                                h4("Heterozygosity Plot"),
                                imageOutput("heterozygosity_plot"),
                                uiOutput("downloadHeterozygosityPlot_UI")
                       ),
                       tabPanel("4. Inbreeding Coefficients",
                                h4("Inbreeding Coefficient by Population"),
                                DT::dataTableOutput("inbreeding_table")
                       ),
                       tabPanel("5. Allele Frequencies",
                                h4("Allele Frequency Table"),
                                DT::dataTableOutput("allele_freq_table")
                       ),
                       tabPanel("6. Hardy-Weinberg Equilibrium",
                                h4("HWE P-value Summary"),
                                DT::dataTableOutput("hwe_summary_text"),
                                h4("Population-wise HWE Chi-Square Table"),
                                DT::dataTableOutput("hwe_chisq_table")
                       ),
                       tabPanel("7. Fst Values",
                                h4("Pairwise Fst Matrix"),
                                uiOutput("fstMatrixUI"),
                                h4("Tidy Pairwise Fst Data"),
                                DT::dataTableOutput("fstDfTable"),
                                br(),
                                h4("Fst Heatmap"),
                                imageOutput("fst_heatmap_plot", width = "100%")
                       ),
                    )
                 )
         ), # end of tabsetpanel
         
         tabItem(tabName = "PCAtab", 
                 fluidRow(
                    box(
                       fileInput("pcaFile", "Upload SNP Data (in CSV or XLSX) for PCA", accept = c(".csv", ".txt", ".xlsx")),
                       checkboxInput("useDefaultColors", "Use Default Colors and Labels", TRUE),
                       conditionalPanel(
                          condition = "!input.useDefaultColors",
                          fileInput("pcaLabels", "Upload PCA Labels (TXT)", accept = c(".csv", ".txt", ".xlsx")),
                          fileInput("colorPalette", "Upload Color Palette (TXT)", accept = c(".csv", ".txt", ".xlsx")),
                          fileInput("shapeList", "Upload desired point shapes (TXT)", accept = c(".csv", ".txt", ".xlsx")),
                          p("The order of the colors would match the order of PCA labels")
                       ),
                       br(),
                       numericInput("pcX", "PC Axis X", value = 1, min = 1),
                       numericInput("pcY", "PC Axis Y", value = 2, min = 1),
                       actionButton("runPCA", "Run PCA Analysis", icon = icon("play"))
                    ),
                    tabBox(
                       tabPanel("Instructions",
                                h4("This runs principal component analysis using the ade4 (Dray and Dufour, 2007) package in R."),
                                br(),
                                p(strong("Input file:"), "CSV or XLSX file and color labels (optional)"),
                                p(strong("Optional additional input file/s:")),
                                tags$ul(
                                   tags$li("PCA labels (.txt)"),
                                   tags$li("Color palette (.txt)"),
                                   tags$li("Desired point shapes (.txt)")
                                ),
                                p(strong("Expected output file:"), "PNG plots")
                       ),
                       tabPanel("Sample Input Format/s",
                                h4("Example: PCA Input Format"),
                                tableOutput("examplePCA")
                       ),
                       tabPanel("Download sample files",
                                h4("Sample File"),
                                tags$ul(
                                   tags$a("Sample file", href = "sample.csv", download = "sample.csv")
                                )
                       )
                    )
                 ),
                 fluidRow(
                    tabBox(
                       title = "PCA Results",
                       width = 12,
                       tabPanel("Plots",
                        plotOutput("barPlot"),
                       plotOutput("pcaPlot"),
                       downloadButton("downloadbarPlot", "Download Bar Plot"),
                       downloadButton("downloadPCAPlot", "Download PCA Plot"))
                    )
                 )
         ),
         
         tabItem(tabName = "PopStructure",
                 fluidRow(
                    box(
                       fileInput("structureFile", "Upload Input File (CSV/XLSX)", accept = c(".csv", ".xlsx")),
                       helpText("Input file should be similar to the output of the 'Convert to CSV' tab under 'File Conversion'"),
                       numericInput("kMin", "Min K", value = 2, min = 1),
                       numericInput("kMax", "Max K", value = 5, min = 1),
                       numericInput("numKRep", "Replicates per K", value = 5, min = 1),
                       numericInput("burnin", "Burn-in Period", value = 1000),
                       numericInput("numreps", "MCMC Reps After Burn-in", value = 10000),
                       checkboxInput("noadmix", "No Admixture Model", value = FALSE),
                       checkboxInput("phased", "Phased Genotype", value = FALSE),
                       numericInput("ploidy", "Ploidy Level", value = 2),
                       checkboxInput("linkage", "Use Linkage Model", value = FALSE),
                       actionButton("runStructure", "Run STRUCTURE", icon = icon("play")),
                       uiOutput("downloadButtons")
                    ),
                    tabBox(
                       tabPanel("Instructions",
                                p(tags$a("STRUCTURE",
                                         href="https://web.stanford.edu/group/pritchardlab/structure.html",
                                         target="_blank"), " is a free software package for investigating population structure using 
                                              multi-locus genotype data (Stephen and Donnelly, 2000; Falush et al., 2003; 
                                              Falush et al., 2007; Hubisz et al., 2009)."
                                ),
                                p("This runs STRUCTURE using the package without front-end and allows immediate 
                                   visualization of results using revised functions based on the ",
                                   tags$a("starmie",
                                          href="https://github.com/sa-lee/starmie",
                                          target = "_blank"
                                   ), "R package. This generates STRUCTURE input files and qmatrices files compatible for
                                   other visualization programs such as pong (Behr et al., 2016)"),
                                p("Some functions were revised and adapted from the strataG and dartR packages such as 'gl.run.structure', '.structureParseQmat', 'structureRead', and 'utils.structure.evanno'"),
                                #h4("Generate STRUCTURE input files and pong compatible files. Visualize the possible results"),
                                p(strong("Input file:"), "CSV or XLSX file"),
                                p(strong("Expected output file:"), "Zipped qmatrices, individual files, and PNG plots"),
                                p("See ",
                                  tags$a("STRUCTURE v2.3.4 Documentation",
                                         href="https://web.stanford.edu/group/pritchardlab/structure_software/release_versions/v2.3.4/structure_doc.pdf",
                                         target="_blank"))
                       ),
                       tabPanel("Sample Input File",
                              DT::dataTableOutput("examplePop_STR2UI")
                       ),
                       tabPanel("Download Sample File",
                                h4("Download Sample File"),
                                tags$ul(
                                   tags$a("Sample CSV file", href = "sample.csv", download = "sample.csv")
                                )     
                       )
                    )
                 ), # end of fluidrow
                 fluidRow(
                    tabBox(
                       width = 12,
                       title = "STRUCTURE Results",
                       h4("STRUCTURE Visualization"),
                       p("NOTE: The plots are expected to take some time to load."),
                       imageOutput("structurePlotPreview")
                    )
                 )
         ),
         
         tabItem(tabName = "ForensicParams",
                  tabsetPanel(                 
                     tabPanel("individual identity SNPs",    
                        fluidRow(
                             box(
                                fileInput("iisnpsFile", "Upload Reference File", accept = c(".csv", ".xlsx")),
                                helpText("See 'Sample Input File' for accepted formats. Frequency table and genotype files are accepted."),
                                checkboxInput("floorCeiling", "Use 5/2n rule in calculating genotype frequency?", FALSE),
                                conditionalPanel(
                                   condition = "input.floorCeiling",
                                   numericInput("totalPop", "Total individuals/samples", value = 100, min = 10,)
                                ),
                                checkboxInput("matchProfile", "Calculate RMP for a profile?", FALSE),
                                conditionalPanel(
                                   condition = "input.matchProfile",
                                   fileInput("fileProfile", "Upload Profile", accept = c(".csv", ".xlsx", ".txt")),
                                   numericInput("thetaValue", "Theta Value", min = 0, value = 0.01, max = 1)
                                ),
                                actionButton("calcIISNPs", "Calculate", icon = icon("calculator")),
                                uiOutput("downloadMetrics_UI"),
                                uiOutput("downloadRMP_UI")
                             ), # end of box
                             tabBox(
                                tabPanel("Instructions",
                                   h4("This calculates forensic parameters specific for individual identity SNPs"),
                                   tags$ul(
                                      tags$li("Random match probability (PM)"),
                                      tags$li("Power of discrimination (PD)"),
                                      tags$li("Polymorphism Information Content (PIC)"),
                                      tags$li("Power of Exclusion (PE)"),
                                      tags$li("Typical Paternity Index (TPI)")
                                   ),
                                   p(strong("Input file:"), "CSV or XLSX file in genotype format or as an allele frequency table"),
                                   p(strong("Expected output file:"), "CSV file"),
                                   br(),
                                   p("Guidelines on statistical calculations for casework: ",
                                     tags$a("Guidelines (for STR):",
                                            href="https://dfs.dc.gov/sites/default/files/dc/sites/dfs/page_content/attachments/FBS22%20-%20STR%20Statistical%20Calculations%20Guidelines.pdf",
                                            target="_blank")), 
                                   p("Guidelines and interpretations: ",
                                     tags$a("Based on the STRAF book",
                                            href="https://agouy.github.io/straf_book/forensic-parameters.html",
                                            target="_blank")),
                                ),
                                tabPanel("Sample Input File",
                                         h4("Acceptable file inputs: genotype files or an allele frequency table:"),
                                         p("Genotype file"),
                                         DT::dataTableOutput("referenceData_UI"),
                                         p("Allele frequency table"),
                                         DT::dataTableOutput("afSample_UI"),
                                         h4("Sample profile to match"),
                                         DT::dataTableOutput("profileSample_UI")
                                ),
                                tabPanel("Download Sample Files",
                                         h4("Download Sample File"),
                                         tags$ul(
                                            tags$a("Sample CSV file", href = "sample.csv", download = "sample.csv")
                                         ),
                                         tags$ul(
                                            tags$a("Sample Allele Frequency Table", href = "pop_stat.xlsx", download = "pop_stat.xlsx")
                                         )
                                )
                             )
                          ), # end of fluid row
                          fluidRow(
                             tabBox(
                                width = 12, 
                                tabPanel("Overall Forensic Params",
                                   selectInput("selected_pop", "Select Population", choices = NULL),
                                   div(
                                      style = "overflow-x: auto;",
                                      DT::dataTableOutput("popTable")
                                   )
                                ),
                                tabPanel("Genotype Frequencies",
                                         selectInput("selected_pop_gt", "Select Population", choices = NULL),
                                         div(
                                            style = "overflow-x: auto;",
                                            DT::dataTableOutput("genotypeFreqs_UI")
                                         )
                                )
                             ) # end of tabBox
                          )
                          ) # end of tabpanel
            )
         ), # end of tabItem
         
         tabItem(tabName = "Classification",
                 fluidRow(
                    box(
                       fileInput("forPredFile", "Upload CSV file", accept = c(".csv", ".xlsx")),
                       actionButton("runNaiveBayes", "Classify", icon = icon("align-justify")),
                       uiOutput("downloadClassification_UI")
                    ),
                    tabBox(
                       tabPanel("Instructions",
                                p("This performs a NaÏve Bayes classification and leave-one-out cross-validation 
                                  of individuals with SNP data (ancestry- or phenotype- informative) using the ", 
                                  tags$a("e1071",
                                         href="https://cran.r-project.org/web/packages/e1071/index.html",
                                         target="_blank"),
                                  
                                  " and ", 
                                  tags$a("caret",
                                         href="https://cran.r-project.org/web/packages/caret/index.html",
                                         target="_blank"),
                                  " R packages."),
                               
                                p("This tool may be used for rapid assessment of marker sets and training datasets. For
                                  more comprehensive forensic DNA inference of an individual, it is recommended to use this tool
                                  in conjunction with", 
                                  tags$a(actionLink("topcatab", "PCA")),
                                  tags$a(actionLink("tostructuretab", "STRUCTURE")), "and",
                                  tags$a("SNIPPER",
                                         href="https://mathgene.usc.es/snipper/index.php",
                                         target="_blank"),
                                  ". Multiple resources are available detailing the limiation of each classification method
                                   (Barash et al., 2024)"),
                                
                                p(strong("Input file/s:"), "CSV file containing training data or merged training and test data."),
                                p(strong("Expected output file:"), "XLSX file")
                       ), 
                       tabPanel("Sample Input File",
                                DT::dataTableOutput("classificationRef_UI")
                       ),
                       tabPanel("Download Sample File",
                                h4("Download Sample File"),
                                tags$ul(
                                   tags$a("Sample CSV file", href = "sample.csv", download = "sample.csv")
                                )
                       )
                    )
                 ), # end of fluid row
                 fluidRow(
                     tabBox(
                              width = 12,
                       tabPanel("Prediction Table",
                                verbatimTextOutput("predictionTableResult")
                       ),
                       tabPanel("Statistics by Population",
                                verbatimTextOutput("statbyClassResult")
                       ),
                       tabPanel("Overall Statistics",
                                verbatimTextOutput("overallStatResult")
                       )
                       
                    )
                 )
         ),
         tabItem(
            tabName = "AppRef",
            tabBox(
               width = 12,
               tabPanel("References",
                        div(
                          style = "height: 80vh; overflow-y:scroll;",
                          uiOutput("referenceTexts")
                        )
               )
            )
         ), # end of another tab item
         tabItem(
            tabName = "About",
            fluidRow(
               tabBox(
                  title = tagList(icon("book-open-reader"), "Background"),
                  width = 12,
                  #height = "250px",
                  h4("This is based on the preliminary work on ancestry marker analysis at the DNA Analysis Laboratory, Natural Sciences Research Institute, University of the Philippines Diliman."),
                  
                  p(strong("Primary Developer:"), "Leda Celeste Samin (DNA-NSRI-UPD)"),
                  p(strong("Project Leader:"), "Nelvie Fatima Jane Soliven (DNA-NSRI-UPD)"),
                  p(strong("Contributors and Collaborators:")),
                  p("Melvin Ambrocio Matias (Institute of Biology - UPD)"),
                  p("Jazelyn Salvador (DNA-NSRI-UPD)"),
                  p("Maria Corazon De Ungria (DNA-NSRI-UPD)"),
                  p("Frederick Delfin (DNA-NSRI-UPD)"),
               )),
            fluidRow(
               tabBox(
                  title = tagList(icon("wallet"), "Funding"),
                  width = 6,
                  height = "250px",
                  h4("The project is funded by the Natural Sciences Research Institute at the University of the Philippines Diliman"),
                  div(tags$img(
                     src = "funding.png",
                     width = "200px"
                  ),
                  style = "text-align: center;"
                  )
               ),
               tabBox(
                  title = tagList(icon("address-book"), "Contact"),
                  width = 6,
                  height = "250px",
                  h4(strong("Project Leader: "), "nasoliven@up.edu.ph"),
                  h4(strong("Institutional Contact: "), "dnalab.updiliman@up.edu.ph"),
                  h4(strong("Location:"), "Miranda Hall, Natural Sciences Research Institute, University of the Philippines Diliman, Quezon City, Philippines")
                      )
            )
         ) 
      )
   )
)

##########
# Server #
##########

server <- function(input, output, session){

   observeEvent(input$refreshApp, {
      session$reload()
   })
   
   
   zoom <- reactiveVal(1)
   
   observeEvent(input$smaller_workplan, {
      zoom(max(0.5, zoom() - 0.1))
   })
   
   observeEvent(input$bigger_workplan, {
      zoom(min(3, zoom() + 0.1))
   })
   
   output$workplanImg <- renderUI({
      tags$img(
         src = "readme/chart.png",
         style = paste0(
            "transform: scale(", zoom(), ");",
            "transform-origin: top center;",
            "transition: transform 0.2s;"
         )
      )
   })
   
   
   observeEvent(input$smaller_fc, {
      zoom(max(0.5, zoom() - 0.1))
   })
   
   observeEvent(input$bigger_fc, {
      zoom(min(3, zoom() + 0.1))
   })
   
   output$fileConvTable <- renderUI({
      tags$img(
         src = "fileconv.png",
         style = paste0(
            "transform: scale(", zoom(), ");",
            "transform-origin: top center;",
            "transition: transform 0.2s;"
         )
      )
   })
   
   output$referenceTexts <- renderUI({
      p_lists <- list(
         p("Pritchard, J.K., Stephens, M., & Donnelly, P. (2000). Inference of Population Structure Using Multilocus Genotype Data. Genetics Society of America, 155, 945-959.",
           tags$a("https://doi.org/10.1093/genetics/155.2.945",
                  href="https://doi.org/10.1093/genetics/155.2.945",
                  target="_blank")),
         p("Falush, D., Stephens, M., & Pritchard, J.K. (2003). Inference of Population Structure Using Multilocus Genotype Data: Linked Loci and Correlated Allele Frequencies. Genetics Society of America, 164, 1567-1587.",
           tags$a("https://doi.org/10.1093/genetics/164.4.1567",
                  href="https://doi.org/10.1093/genetics/164.4.1567",
                  target="_blank")),
         p("Falush, D., Stephens, M., & Pritchard, J.K. (2007). Inference of population structure using multilocus genotype data: dominant markers and null alleles. Molecular Ecology Notes, 7(4), 574-578.",
           tags$a("https://doi.org/10.1111/j.1471-8286.2007.01758.x",
                  href="https://doi.org/10.1111/j.1471-8286.2007.01758.x",
                  target="_blank")),
         p("Hubisz, M. J., Falush, D., Stephens, M., & Pritchard, J. K. (2009). Inferring weak population structure with the assistance of sample group information. Molecular ecology resources, 9(5), 1322–1332.",
           tags$a("https://doi.org/10.1111/j.1755-0998.2009.02591.x",
                  href="https://doi.org/10.1111/j.1755-0998.2009.02591.x",
                  target="_blank")),
         p("Purcell, S., Neale, B., Todd-Brown, K., Thomas, L., Ferreira, M. A., Bender, D., Maller, J., Sklar, P., de Bakker, P. I., Daly, M. J., & Sham, P. C. (2007). PLINK: a tool set for whole-genome association and population-based linkage analyses. American journal of human genetics, 81(3), 559–575.",
           tags$a("https://doi.org/10.1086/519795",
                  href="https://doi.org/10.1086/519795",
                  target="_blank")),
         p("Chang, C.C., Chow, C.C., Tellier, L.C., Vattikuti, S., Purcell, S.M., & Lee, J.J. (2015). Second-generation PLINK: rising to the challenge of larger and richer datasets. GigaScience, 4(7).",
           tags$a("https://doi.org/10.1186/s13742-015-0047-8",
                  href="https://doi.org/10.1186/s13742-015-0047-8",
                  target="_blank")),
         p("Dray, S., & Dufour, A.-B. (2007). The ade4 Package: Implementing the Duality Diagram for Ecologists. Journal of Statistical Software, 22(4), 1–20.",
           tags$a("https://doi.org/10.18637/jss.v022.i04",
                  href="https://doi.org/10.18637/jss.v022.i04",
                  target="_blank")),
         p("Kamvar, Z.N., Tabima, J.F., & Grunwald, N.J. (2014). Poppr: an R package for genetic analysis of populations with clonal, partially clonal, and/or sexual reproduction. PeerJ (2), e281,",
           tags$a("https://doi.org/10.7717/peerj.281",
                  href="https://doi.org/10.7717/peerj.281",
                  target="_blank")),
         p("Goudet, J. (2004). hierfstat, a package for r to compute and test hierarchical F-statistics. Molecular Ecology Notes, 5(1), 184-186.",
           tags$a("https://doi.org/10.1111/j.1471-8286.2004.00828.x",
                  href="https://doi.org/10.1111/j.1471-8286.2004.00828.x",
                  target="_blank")),
         p("Jombart, T. (2008). adegenet: a R package for the multivariate analysis of genetic markers. Bioinformatics, 24(11), 1403-1405.",
           tags$a("https://doi.org/10.1093/bioinformatics/btn129",
                  href="https://doi.org/10.1093/bioinformatics/btn129",
                  target="_blank")),
         p("Jombart, T., & Ahmed, I. (2011). adegenet 1.3-1: new tools for the analysis of genome-wide SNP data. Bioinformatics (Oxford, England), 27(21), 3070–3071.",
           tags$a("https://doi.org/10.1093/bioinformatics/btr521",
                  href="https://doi.org/10.1093/bioinformatics/btr521",
                  target="_blank")),
         p("Paradis E. (2010). pegas: an R package for population genetics with an integrated-modular approach. Bioinformatics (Oxford, England), 26(3), 419–420.",
           tags$a("https://doi.org/10.1093/bioinformatics/btp696",
                  href="https://doi.org/10.1093/bioinformatics/btp696",
                  target="_blank")),
         p("Petit, R.J., El Mousadik, A., & Pons, O. (1998). Identifying populations for conservation on the basis of genetic markers. Conservation Biology, 12:844-855"),
         p("Foulley, J.F., & Ollivier, L. (2005). Estimating allelic richness and its diversity. Livestock Science, 101:150-158.",
           tags$a("https://doi.org/10.1016/j.livprodsci.2005.10.021",
                  href="https://doi.org/10.1016/j.livprodsci.2005.10.021",
                  target="_blank")),
         p("Nei, M. (1978). Estimation of average heterozygosity and genetic distance from a small number of individuals. Genetics:89:583-590.",
           tags$a("https://doi.org/10.1093/genetics/89.3.583",
                  href="https://doi.org/10.1093/genetics/89.3.583",
                  target="_blank")),
         p("Rousset, F. (2002). Inbreeding and relatedness coefficients: what do they measure? Heredity, 88:371-380."),
         p("Rezaei, N., & Hedayat, M. (2013). Allele Frequency. Brenner's Encyclopedia of Genetics (Second Edition).",
           tags$a("https://doi.org/10.1016/B978-0-12-374984-0.00032-2",
                  href="https://doi.org/10.1016/B978-0-12-374984-0.00032-2",
                  target="_blank")),
         p("Tiret, L., & Cambien, F. (1995). Departure from Hardy-Weinberg equilibrium should be systematically tested in studies of association between genetic markers and disease. Circulation, 92(11):3364-3365."),
         p("Weir, B.S., & Cockerham, C.C. (1984). Estimating F-statistics for the analysis of population structure. Evolution; International Journal of Organic Evolution, 38(6): 1358-1370.",
           tags$a("https://doi.org/10.1111/j.1558-5646.1984.tb05657.x",
                  href="https://doi.org/10.1111/j.1558-5646.1984.tb05657.x",
                  target="_blank")),
         p("Gouy, A., & Zieger, M. (n.d.). The STRAF Book: Statistical Forensics made easy.",
           tags$a("https://agouy.github.io/straf_book/index.html",
                  href="https://agouy.github.io/straf_book/index.html",
                  target="_blank")),
         p("Dimitriadou, E., Hornik, K., Leisch, F., Meyer, D., & Weingessel, A. (2009). E1071: Misc Functions of the Department of Statistics (E1071), TU Wien.",
           tags$a("https://www.researchgate.net/publication/221678005_E1071_Misc_Functions_of_the_Department_of_Statistics_E1071_TU_Wien",
                  href="https://www.researchgate.net/publication/221678005_E1071_Misc_Functions_of_the_Department_of_Statistics_E1071_TU_Wien",
                  target="_blank")),
         p("Kuhn, M. (2008). Building Predictive Models in R Using the caret Package. Journal of Statistical Software, 28(5), 1–26.",
           tags$a("https://doi.org/10.18637/jss.v028.i05",
                  href="https://doi.org/10.18637/jss.v028.i05",
                  target="_blank")),
         p("Bodenhofer, U., Bonatesta, E., Horejš-Kainrath, C., & Hochreiter, S. (2015). msa: an R package for multiple sequence alignment. Bioinformatics (Oxford, England), 31(24), 3997–3999.",
           tags$a("https://doi.org/10.1093/bioinformatics/btv494",
                  href="https://doi.org/10.1093/bioinformatics/btv494",
                  target="_blank")),
         p("Zou, Y., Zhang, Z., Zeng, Y., Hu, H., Hao, Y., Huang, S., & Li, B. (2024). Common Methods for Phylogenetic Tree Construction and Their Implementation in R. Bioengineering (Basel, Switzerland), 11(5), 480.",
           tags$a("https://doi.org/10.3390/bioengineering11050480",
                  href="https://doi.org/10.3390/bioengineering11050480",
                  target="_blank")),
         p("Wright E. S. (2015). DECIPHER: harnessing local sequence context to improve protein multiple sequence alignment. BMC bioinformatics, 16, 322.",
           tags$a("https://doi.org/10.1186/s12859-015-0749-z",
                  href="https://doi.org/10.1186/s12859-015-0749-z",
                  target="_blank")),
         p("Paradis, E., & Schliep, K. (2019). ape 5.0: an environment for modern phylogenetics and evolutionary analyses in R. Bioinformatics (Oxford, England), 35(3), 526–528.",
           tags$a("https://doi.org/10.1093/bioinformatics/bty633",
                  href="https://doi.org/10.1093/bioinformatics/bty633",
                  target="_blank")),
         p("Schliep, K.P. (2011). phangorn: phylogenetic analysis in R. Bioinformatics, 27(4), 592-593.",
           tags$a("https://doi.org/10.1093/bioinformatics/btq706",
                  href="https://doi.org/10.1093/bioinformatics/btq706",
                  target="_blank")),
         p("Zhang, A., Hao, M., Yang, C., & Shi, Z. (2016). BarcodingR: an integrated r package for species identification using DNA barcodes. Methods in Ecology and Evolution, 8, 627-634.",
           tags$a("https://doi.org/10.1111/2041-210X.12682",
                  href="https://doi.org/10.1111/2041-210X.12682",
                  target="_blank")),
         p("Behr, A.A., Liu, K.Z., Liu-Fang, G., Nakka, P., & Ramachandran, S. (2016). pong: fast analysis and visualization of latent clusters in population genetic data. Bioinformatics, 32(18), 2817-2823.",
           tags$a("https://doi.org/10.1093/bioinformatics/btw327",
                  href="https://doi.org/10.1093/bioinformatics/btw327",
                  target="_blank")),
         p("Gruber, B., Unmack, P. J., Berry, O. F., & Georges, A. (2018). dartr: An r package to facilitate analysis of SNP data generated from reduced representation genome sequencing. Molecular ecology resources, 18(3), 691–699.",
           tags$a("https://doi.org/10.1111/1755-0998.12745",
                  href="https://doi.org/10.1111/1755-0998.12745",
                  target="_blank")),
         p("Archer, F. I., Adams, P. E., & Schneiders, B. B. (2017). stratag: An r package for manipulating, summarizing and analysing population genetic data. Molecular ecology resources, 17(1), 5–11.",
           tags$a("https://doi.org/10.1111/1755-0998.12559",
                  href="https://doi.org/10.1111/1755-0998.12559",
                  target="_blank")),
         p("Barash, M., McNevin, D., Fedorenko, V., & Giverts, P. (2024). Machine learning applications in forensic DNA profiling: A critical review. Forensic Science International: Genetics (69).",
           tags$a("https://doi.org/10.1016/j.fsigen.2023.102994",
                  href="https://doi.org/10.1016/j.fsigen.2023.102994",
                  target="_blank"))
         
      )
      
      do.call(tagList, p_lists)
   })
   
   output$popstatRef <- renderUI({
      p_lists <- list(
         p("Calculation of common population statistics:"),
         tags$ul(
            tags$li("Private alleles [1] calculated using the poppr R package"),
            tags$li("Mean Allelic Richness [2] using the hierfstat R package"),
            tags$li("Heterozygosity [3] using the hierfstat R package"),
            tags$li("Inbreeding Coefficient [4] using the hierfstat R package"),
            tags$li("Allele frequency [5] using the adegenet R package"),
            tags$li("Hardy-Weinberg equilibrium [6] using the pegas R package"),
            tags$li("FST values [7] using the hierfstat R package")
         ),
         br(),
         p(strong("Input file:"), "CSV or .xlsx file"),
         p(strong("Expected output files:")),
         tags$ul(
            tags$li(".xlsx file with all results"),
            tags$li("Heterozygosity Plot (.png)"),
            tags$li("Fst Plots (.png)")
         ),
         br(),
         h5("To learn more about the statistics:"),
         h5("References"),
         p("[1] Petit, R.J., El Mousadik, A., and Pons, O. (1998). Identifying populations for conservation on the basis of genetic markers. Conservation Biology, 12:844-855"),
         p("[2] Foulley, J.F., and Ollivier, L. (2005). Estimating allelic richness and its diversity. Livestock Science, 101:150-158. https://doi.org/10.1016/j.livprodsci.2005.10.021"),
         p("[3] Nei, M. (1978). Estimation of average heterozygosity and genetic distance from a small number of individuals. Genetics:89:583-590. https://doi.org/10.1093/genetics/89.3.583"),
         p("[4] Rousset, F. (2002). Inbreeding and relatedness coefficients: what do they measure? Heredity, 88:371-380."),
         p("[5] Rezaei, N., and Hedayat, M. (2013). Allele Frequency. Brenner's Encyclopedia of Genetics (Second Edition). https://doi.org/10.1016/B978-0-12-374984-0.00032-2"),
         p("[6] Tiret, L., and Cambien, F. (1995). Departure from Hardy-Weinberg equilibrium should be systematically tested in studies of association between genetic markers and disease. Circulation, 92(11):3364-3365."),
         p("[7] Weir, B.S., and Cockerham, C.C. (1984). Estimating F-statistics for the analysis of population structure. Evolution; International Journal of Organic Evolution, 38(6): 1358-1370. https://doi.org/10.1111/j.1558-5646.1984.tb05657.x")
         
      )
      
      do.call(tagList, p_lists)
   })
   
   observeEvent(input$tophylogentab, {
      updateTabItems(session, "tabs", selected = "PhylogenAnalysis")
   })
   
   observeEvent(input$tomsatab, {
      updateTabItems(session, "tabs", selected = "MSAtab")
   })
   
   observeEvent(input$topcatab, {
      updateTabItems(session, "tabs", selected = "PCAtab")
   })
   
   observeEvent(input$tostructuretab, {
      updateTabItems(session, "tabs", selected = "PopStructure")
   })
   
   #================= FILE CONVERSION =====================#
   convertedVCF <- reactiveVal(NULL)
   convertedFASTA <- reactiveVal(NULL)
   convertedCSV <- reactiveVal(NULL)
   convertedPLINK <- reactiveVal(NULL)
   convertedBreakdown <- reactiveVal(NULL)
   
   observe({
      hasfile <-!is.null(input$VCFFile) || !is.null(input$BCFFile) || !is.null(input$CSVFile) || (!is.null(input$bedFile) && !is.null(input$bimFile) && !is.null(input$famFile))
      
      breakdown_selected <- !is.null(input$breakdown_vcf) || !is.null(input$breakdown_bcf) || !is.null(input$breakdown_plink)
      breakdown_ready <- !breakdown_selected || 
                     (!is.null(input$breakdown_column_vcf) || !is.null(input$breakdown_column_bcf) || !is.null(input$breakdown_column_plink))

      
      toggleState("ConvertFILES", condition = hasfile && breakdown_ready)
   })
   
   
   exampleRefCSV <- data.frame(
         Sample.Name = c("sample1", "sample2", "sample3", "sample4", "..."),
         Population = c("Malaysia", "Mexico", "Greece", "South Korea", "..."),
         Superpopulation = c("Southeast Asia", "North and South America", "Europe", "East Asia", "...")
      )
   
   exampleCSVFile <- data.frame(
         Sample.Name = c("sample1", "sample2", "sample3", "sample4", "..."),
         Population = c("Malaysia", "Mexico", "Greece", "South Korea", "..."),
         rs01 = c("G/T", "G/A", "C/A", "A/A", "..."),
         rs02 = c("C/C", "C/C", "G/C", "G/G", "..." ),
         "..." = c("...", "...", "...", "...", "...")
      )
   marker_info_format <- data.frame(
         SNP = c("rs01", "rs02", "rs03", "rs04", "..."),
         chromosome = c("chr1", "chr4", "chr5", "chr5", "..."),
         position = c("1004", "90986", "5768", "9384982", "..."),
         genetic_distance = c("0", "0", "0", "0", "..."),
         ref_allele = c("A", "T", "G", "G", "C"),
         alt_allele = c("T", "A", "C", "C", "G")
      )
   
   output$ExampleRefFile <- DT::renderDataTable({
      req(exampleRefCSV)
      exampleRefCSV
   }, options = list(
      scrollX = TRUE,
      pageLength = 5
   )
   )

   output$ExampleCSVFormat <- DT::renderDataTable({
      req(exampleCSVFile)
      marker_info_format
   }, options = list(
      scrollX = TRUE,
      pageLength = 5
   )
   )
   
   output$markerInfoFormat <- DT::renderDataTable({
      req(marker_info_format)
      marker_info_format
   }, options = list(
      scrollX = TRUE,
      pageLength = 5
   )
   )
   
   output.dir <- tempdir()
   timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
   outputName <- paste0("converted_", timestamp, ".csv")
   
   output_type <- function(input) {
      switch(input$inputType1,
             "vcf1"   = input$inputType2_vcf,
             "bcf1"   = input$inputType2_bcf,
             "plink1" = input$inputType2_plink,
             "csv1"   = "vcf2"
      )
   }
   
   getRefValue <- function(input) {
      outputType <- output_type(input)
      
      if (outputType != "csv2") {
         return(NULL)
      }
      
      if (input$inputType1 == "vcf1") {
         req(input$poptype_vcf)
         if (input$poptype_vcf == "multiplepop") {
            req(input$multiplepop_vcf)
            return(input$multiplepop_vcf$datapath)
         } else {
            req(input$typePop_vcf)
            return(input$typePop_vcf)
         }
      } else if (input$inputType1 == "bcf1") {
         req(input$poptype_bcf)
         if (input$poptype_bcf == "multiplepop") {
            req(input$multiplepop_bcf)
            return(input$multiplepop_bcf$datapath)
         } else {
            req(input$typePop_bcf)
            return(input$typePop_bcf)
         }
      } else if (input$inputType1 == "plink1") {
         req(input$poptype_plink)
         if (input$poptype_plink == "multiplepop") {
            req(input$multiplepop_plink)
            return(input$multiplepop_plink$datapath)
         } else {
            req(input$typePop_plink)
            return(input$typePop_plink)
         }
      }
      return(NULL)
   }
   
   observeEvent(input$ConvertFILES, {
      req(input$ConvertFILES)
     # 
     # Sys.sleep(1.5)
      disable("ConvertFILES")
      
      outputType <- output_type(input)
      req(outputType)
      
      refValue <- if (outputType == "csv2"){
         getRefValue(input)
      } else {
         NULL
      }
      
      names_text <- paste(output.dir, "list.txt")
      merged_file <- file.path(output.dir, "merged")
      
      #-------------------------- VCF
      if (input$inputType1 == "vcf1") {
         req(input$VCFFile)
         vcf_ext <- tools::file_ext(input$VCFFile$name)
         
         if (vcf_ext %in% c("zip", "tar")) {
            unpacked_files <- unpack_input_file(input$VCFFile$datapath, output.dir)
            
            data_list <- unpacked_files$data_files
            all.list <- list()
            
            
            # Convert each file to plink and create a list of output files
            for (x in data_list) {
               revised <- substr(x, 1, nchar(x)-4)
               file_name <- convert_to_plink(x, output.dir, plink_path, name = revised)
               plink_lines <- paste(paste0(file_name, ".bed"),
                                    paste0(file_name, ".bim"),
                                    paste0(file_name, ".fam"),
                                    sep = "\t"
               )
               write(plink_lines, file = names_text, append = TRUE)
            }
            
            # merge all the plink files
            merged_plink <- paste(
               shQuote(plink_path),
               "--merge-list", shQuote(names_text),
               "--recode vcf",
               "--keep-allele-order",
               "--out", shQuote(merged_file)
            )
            system(merged_plink)
            
         }
         
         vcf_file <- if (vcf_ext %in% c("zip", "tar")) {
            paste(merged_file, ".vcf")
         } else { input$VCFFile$datapath }
         
         if (outputType == "csv2") {
            
            csv_file <- vcf_to_csv(
               vcf_file, 
               ref = refValue, 
               output.dir = output.dir
               )
            convertedCSV(csv_file)
            output$downloadConvertedCSV <- downloadHandler(
               filename = function() { outputName },
               content = function(file) { 
                  readr::write_csv(convertedCSV(), file) 
                  }
            )
            
            if (!is.null(input$breakdown_vcf)) {
               req(input$breakdown_column_vcf)
               breakdown_results <- pop_breakdown(csv_file, input$breakdown_column_vcf)
               convertedBreakdown(breakdown_results)
            }
            
         }
         
         if (outputType == "fasta") {
            req(input$FASTARef)
            
            converted.file <- vcf_to_fasta(
               vcf_file, 
               input$FASTARef$datapath,
               bcftools_path = bcftools_path, 
               output.dir = output.dir)
            
            convertedFASTA(converted.file)
            output$downloadConvertedFASTA <- downloadHandler(
               filename = function() { "consensus.fa" },
               content = function(file) { file.copy(convertedFASTA(), file) }
            )
         }
         
         if (outputType == "plink2") {
            converted.file <- convert_to_plink(vcf_file, output.dir = output.dir, plink_path = plink_path)
            convertedPLINK(converted.file)
            output$downloadConvertedPLINK <- downloadHandler(
               filename = function() { "convertedtoPLINK.zip" },
               content = function(file) {
                  data_files <- list.files(path = output.dir, pattern = "^converted_to_plink.", full.names = TRUE)
                  zip::zipr(zipfile = file, files = data_files)
               },
               contentType = "application/zip"
            )
         }
         
         enable("ConvertFILES")
      }
      
      #------------------------ BCF
      if (input$inputType1 == "bcf1") {
         req(input$BCFFile)
         bcf_ext <- tools::file_ext(input$VCFFile$name)
         
         if (bcf_ext %in% c("zip", "tar")) {
            unpacked_files <- unpack_input_file(input$VCFFile$datapath, output.dir)
            
            data_list <- unpacked_files$data_files

            # Convert each file to plink and create a list of output files
            for (x in data_list) {
               revised <- substr(x, 1, nchar(x)-4)
               file_name <- convert_to_plink(x, output.dir, plink_path, name = revised)
               plink_lines <- paste(paste0(file_name, ".bed"),
                                    paste0(file_name, ".bim"),
                                    paste0(file_name, ".fam"),
                                    sep = "\t"
               )
               write(plink_lines, file = names_text, append = TRUE)
            }
            
            # merge all the plink files
            merged_plink <- paste(
               shQuote(plink_path),
               "--merge-list", shQuote(names_text),
               "--recode vcf",
               "--keep-allele-order",
               "--out", shQuote(merged_file)
            )
            system(merged_plink)
         }
         
         bcf_file <- if (bcf_ext %in% c("zip", "tar")) {
            paste(merged_file, ".vcf")
         } else { input$BCFFile$datapath }
         
         
         if (outputType == "vcf2" & bcf_ext == "bcf") {
            converted.file <- bcf_to_vcf(bcf_file, output.dir = output.dir, plink_path = plink_path)
            convertedVCF(converted.file)
            output$downloadConvertedVCF <- downloadHandler(
               filename = function() { "bcftovcf.vcf" },
               content = function(file) { file.copy(convertedVCF(), file) }
            )
         } else if (outputType == "vcf2" & bcf_ext != "bcf"){
            convertedVCF(bcf_file)
            output$downloadConvertedVCF <- downloadHandler(
               filename = function() { "bcftovcf.vcf" },
               content = function(file) { file.copy(convertedVCF(), file) }
            )
         }
         
         if (outputType == "csv2" & bcf_ext == "bcf") {
            file2 <- bcf_to_vcf(bcf_file, output.dir = output.dir)
            csv_file <- vcf_to_csv(file2, ref = refValue, output.dir = output.dir)
            convertedCSV(csv_file)
            output$downloadConvertedCSV <- downloadHandler(
               filename = function() { outputName },
               content = function(file) { readr::write_csv(convertedCSV(), file) }
            )
            
            if (!is.null(input$breakdown_bcf)) {
               req(input$breakdown_column_bcf)
               breakdown_results <- pop_breakdown(csv_file, input$breakdown_column_bcf)
               convertedBreakdown(breakdown_results)
            }
         } else if (outputType == "csv2" & bcf_ext != "bcf") {
            csv_file <- vcf_to_csv(bcf_file, ref = refValue, output.dir = output.dir)
            convertedCSV(csv_file)
            output$downloadConvertedCSV <- downloadHandler(
               filename = function() { outputName },
               content = function(file) { readr::write_csv(convertedCSV(), file) }
            )
            
            if (!is.null(input$breakdown_bcf)) {
               req(input$breakdown_column_bcf)
               breakdown_results <- pop_breakdown(csv_file, input$breakdown_column_bcf)
               convertedBreakdown(breakdown_results)
            }
         }
         
         if (outputType == "plink2") {
            converted.file <- convert_to_plink(bcf_file, output.dir = output.dir, plink_path = plink_path)
            convertedPLINK(converted.file)
            
            output$downloadConvertedPLINK <- downloadHandler(
               filename = function() { "convertedtoPLINK.zip" },
               content = function(file) {
                  data_files <- list.files(path = output.dir, pattern = "^converted_to_plink.", full.names = TRUE)
                  zip::zipr(zipfile = file, files = data_files)
               },
               contentType = "application/zip"
            )
         }
         
         enable("ConvertFILES")
      }
      
      #---------------------- PLINK
      if (input$inputType1 == "plink1") {
         req((input$bedFile & input$bimFile & input$famFile) || input$zippedPLINK)
         
         if (!is.null(input$zippedPLINK)) {
            zipped_files <- unpack_input_file(input$zippedPLINK$datapath)
            file_names <- zipped_files$data_files
            
            for (x in file_names) {
               revised <- substr(x, 1, nchar(x)-4)
               plink_lines <- paste(paste0(revised, ".bed"),
                                    paste0(revised, ".bim"),
                                    paste0(revised, ".fam"),
                                    sep = "\t"
               )
               write(plink_lines, file = names_text, append = TRUE)
            }
            
            # merge all the plink files
            merged_plink <- paste(
               shQuote(plink_path),
               "--merge-list", shQuote(names_text),
               "--make-bed",
               "--out", shQuote(merged_file)
            )
            system(merged_plink)
         }
         
         if (!is.null(input$zippedPLINK)) {
            bed_file <- paste(merged_file, ".bed")
            bim_file <- paste(merged_file, ".bim")
            fam_file <- paste(merged_file, ".fam")
         } else { 
            bed_file <- input$bedFile$datapath
            bim_file <- input$bimFile$datapath
            fam_file <- input$famFile$datapath
            }
         
         converted.file <- plink_to_vcf(bed_file, bim_file, fam_file, output.dir = output.dir)
         
         if (outputType == "vcf2") {
            convertedVCF(converted.file)
            output$downloadConvertedVCF <- downloadHandler(
               filename = function() { "tovcf.vcf" },
               content = function(file) { file.copy(convertedVCF(), file) }
            )
         }
         
         if (outputType == "csv2") {
            csv_file <- vcf_to_csv(converted.file, ref = refValue, output.dir = output.dir)
            convertedCSV(csv_file)
            output$downloadConvertedCSV <- downloadHandler(
               filename = function() { outputName },
               content = function(file) { readr::write_csv(convertedCSV(), file) }
            )
            if (!is.null(input$breakdown_plink)) {
               req(input$breakdown_column_plink)
               breakdown_results <- pop_breakdown(csv_file, input$breakdown_column_plink)
               convertedBreakdown(breakdown_results)
            }
         }
         
         enable("ConvertFILES")
      }
      
      #------------------ CSV 
      if (input$inputType1 == "csv1") {
         req(input$CSVFile, input$lociMetaFile)
         csv_ext <- tools::file_ext(input$CSVFile$name)
         
         if (csv_ext %in% c("zip", "tar")){
            unpacked_files <- unpack_input_file(input$CSVFile$datapath, output.dir)
            file_names <- unpacked_files$data_files
            all.list <- list()
            
            for (x in file_names) {
               all.list[[x]] = load_csv_xlsx_files(x)
            }
            
            csv_merged <- data.table::rbindlist(all.list, fill = TRUE)
         }
         
         csv_file <- if (csv_ext %in% c("zip", "tar")){
            csv_merged
         } else {input$CSVFile$datapath}
         
         file2 <- csv_to_gentibble(csv_file, loci.meta = input$lociMetaFile$datapath)
         converted.file <- tidypopgen::gt_as_vcf(file2, file = "tovcf.vcf")
         convertedVCF(converted.file)
         output$downloadConvertedVCF <- downloadHandler(
            filename = function() { "tovcf.vcf" },
            content = function(file) { file.copy(convertedVCF(), file) }
         )
         enable("ConvertFILES")
         
      }
   }
   )
   
   output$previewTable <- DT::renderDataTable({
      req(convertedCSV())
      convertedCSV()
   }, options = list(
      scrollX = TRUE,
      pageLength = 10
   )
   )
   
   output$previewTableBreakdown <- DT::renderDataTable({
      req(convertedBreakdown())
      convertedBreakdown()
   }, options = list(
      scrollX = TRUE,
      pageLength = 10
   )
   )
   
   output$downloadCSV_UI <- renderUI({
      req(convertedCSV())
      downloadButton("downloadConvertedCSV", "Download CSV File")
   })
   
   output$downloadVCF_UI <- renderUI({
      req(convertedVCF())
      downloadButton("downloadConvertedVCF", "Download VCF File")
   })
   
   output$downloadFASTA_UI <- renderUI({
      req(convertedFASTA())
      downloadButton("downloadConvertedFASTA", "Download FASTA File")
   })
   
   output$downloadPLINK_UI <- renderUI({
      req(convertedPLINK())
      downloadButton("downloadConvertedPLINK", "Download PLINK File")
   })
   
   #================== ForenSeq to CSV ====================# 
   
   convertedUAS <- reactiveVal(NULL)
   observe({
      shinyjs::toggleState("run_uas2csv", !is.null(input$uas_zip))
   })
   
   
   output$exampleXLSX <- renderTable({
      data.frame(
         Sample.Name = c("sample1","sample1", "sample1", "sample1", "sample1", "sample1", "sample2", "sample3", "sample3", "sample3", "..."),
         Locus = c("rs01", "rs01", "rs02", "rs02", "rs02", "rs03", "rs01", "rs01", "rs02", "rs03", "..."),
         Allele = c("A", "T", "C", "A", "G", "T", "A", "T", "G", "A", "...")
      )
   })
   
   
   temp_dir <- tempdir()
   
   observeEvent(input$run_uas2csv, {
      req(input$uas_zip)
      
      #Sys.sleep(1.5)
      disable("run_uas2csv")
      
      input_path <- file.path(temp_dir, input$uas_zip$name)
      file.copy(input$uas_zip$datapath, input_path, overwrite = TRUE)
      
      # Handle optional reference input
      ref_value <- NULL
      use_reference <- FALSE
      
      if (!is.null(input$ref_file)) {
         ref_value <- input$ref_file$datapath
         use_reference <- TRUE
      }
      
      withProgress(message = "Converting file...", value = 0, {
         tryCatch({
            widened.file <- uas_to_csv(files = input_path,
                                       population = ref_value,
                                       reference = use_reference,
                                       dir = temp_dir)
            convertedUAS(widened.file)
            
            enable("run_uas2csv")
            showNotification("Conversion complete!", type = "message")
            
         }, error = function(e) {
            showNotification(paste("Error:", e$message), type = "error")
            enable("run_uas2csv")
         })
      })
      
   }) # end of observe Event
   
   outputName <- "merged_typed_data.csv"
   
   output$downloadUAScsv <- downloadHandler(
      filename = function() {
         outputName
      },
      content = function(file) {
         readr::write_csv(convertedUAS(), file)
      }
   )
   
   
   output$downloadUAScsv_UI <- renderUI({
      req(convertedUAS())
      downloadButton("downloadUAScsv", "Download CSV File")
   })
   
   output$previewTableUAS <- DT::renderDataTable({
      req(convertedUAS())
      convertedUAS()
   }, options = list(
      scrollX = TRUE,
      pageLength = 10
   )
   )
   
   #==================== To SNIPPER =======================#
   
   exampleTableSnipper1 <- data.frame(
         Ind = c("sample1", "sample2", "sample3", "sample4", "..."),
         rs101 = c("A/A", "A/T", "T/T", "A/T", "..."),
         rs102 = c("G/C", "G/C", "G/G", "G/C", "..."),
         rs103 = c("C/C", "C/G", "G/G", "G/G", "..."),
         rs_n = c("...", "...", "...", "...", "...")
      )
   output$exampleTableSnipper1 <- DT::renderDataTable({
      req(exampleTableSnipper1)
      exampleTableSnipper1
   }, options = list(
      scrollX = TRUE,
      pageLength = 10
   )
   )
   
   exampleTableSnipper2 <- data.frame(
      Ind = c("sample1", "sample2", "sample3", "sample4", "..."),
      Pop = c("sub_pop1", "sub_pop2", "sub_pop3", "sub_pop4", "..."),
      Superpop = c("region1", "region1", "region2", "region3", "..."),
      rs101 = c("A/A", "A/T", "T/T", "A/T", "..."),
      rs102 = c("G/C", "G/C", "G/G", "G/C", "..."),
      rs103 = c("C/C", "C/G", "G/G", "G/G", "..."),
      rs_n = c("...", "...", "...", "...", "...")
   )
   output$exampleTableSnipper2 <- DT::renderDataTable({
      req(exampleTableSnipper2)
      exampleTableSnipper2
   }, options = list(
      scrollX = TRUE,
      pageLength = 10
   )
   )
   
   exampleRefSnipper <- data.frame(
         Sample.Name = c("sample1", "sample2", "sample3", "sample4", "..."),
         Population = c("Malaysia", "Mexico", "Greece", "South Korea", "..."),
         Superpopulation = c("Southeast Asia", "North and South America", "Europe", "East Asia", "...")
      )
   output$exampleRefSnipper <- DT::renderDataTable({
      req(exampleRefSnipper)
      exampleRefSnipper
   }, options = list(
      scrollX = TRUE,
      pageLength = 10
   )
   )
   
   convertedSNIPPER <- reactiveVal(NULL)
   outputName <- "snipper.xlsx"
   
   observe({
      hasFile <- !is.null(input$convertFile) && nrow(input$convertFile) > 0
      ready <- isTRUE(hasFile)
      shinyjs::toggleState("convertBtn", ready)
   })
   
   observeEvent(input$convertBtn, {
      
      Sys.sleep(1.5)
      
      
      disable("convertBtn")
      
      tosnipper_file <- load_csv_xlsx_files(input$convertFile$datapath)
      tosnipper_file <- dplyr::rename(tosnipper_file, Sample = 1)
      
      if (!is.null(input$refProvided)) {
         inputPath <- tosnipper_file[,-c(2,3)]
         refPath <- tosnipper_file[,2:3]
      } else {
         inputPath <- tosnipper_file
         refPath <- load_csv_xlsx_files(input$refFile$datapath)
      }
      
      refPath <- dplyr::rename(refPath, Sample = 1)
      
      targetSet <- input$targetPop
      targetName <- if (targetSet) input$targetPopName else NULL
      
      inputData <- colnames(inputPath)
      numMarkers <- length(inputData) - 1
      
      withProgress(message = "Converting to SNIPPER-analysis ready file...", value = 0, {
         snipper.file <- tryCatch({
            
            to_snipper(input = inputPath,
                       references = refPath,
                       target.pop = targetSet,
                       population.name = targetName,
                       markers = numMarkers)
            
         }, error = function(e){
            showNotification(paste("Conversion failed:", e$message), type = "error")
            NULL
         })
         
         if (!is.null(snipper.file)) {
            convertedSNIPPER(snipper.file)
            enable("convertBtn")
         }
         enable("convertBtn")
      })
      
   }) # end of observe Event
   
   output$downloadConverted <- downloadHandler(
      filename = function() { outputName },
      content = function(file) {
         openxlsx::write.xlsx(convertedSNIPPER(), file)
      }
   )
   
   output$downloadSNIPPER <- renderUI({
      req(convertedSNIPPER())
      downloadButton("downloadConverted", "Download SNIPPER-ready file")
   })
   
   output$previewTableSNIPPER <- DT::renderDataTable({
      req(convertedSNIPPER())
      convertedSNIPPER()
   }, options = list(
      scrollX = TRUE,
      pageLength = 10
   )
   )
  
   #==================== CSV to STR =======================#
   examplePop_STR <-data.frame(
      Sample = c("Sample1", "Sample2", "Sample3", "Sample4", "..."),
      Population = c("POP1", "POP2", "POP3", "POP4", "..."),
      rs101 = c("A/A", "A/T", "A/A", "T/T", "..."),
      rs102 = c("G/G", "C/C", "G/C", "G/G", "..."),
      rs_n = c("...", "...", "...", "...", "...")
   )
   
   
   output$examplePop_STRUI <- DT::renderDataTable({
      req(examplePop_STR)
      examplePop_STR
   }, options = list(
      scrollX = TRUE,
      pageLength = 5
   )
   )
   
   observe({
      toggleState("csv2str", !is.null(input$tostrFile) && !is.null(input$systemFile))
   })
   
   csv_revised <- reactiveVal(NULL)
   strconvert <- reactiveVal(NULL)
   str_file <- reactiveVal(NULL)
   
   observeEvent(input$csv2str, {
      
      Sys.sleep(1.5)
      
      disable("csv2str")
      
      withProgress(message = "Analyzing files...", value = 0, {
         tryCatch({
            req(input$tostrFile$datapath, input$systemFile)
            
            csv_file <- load_csv_xlsx_files(input$tostrFile$datapath)
            genind <- convert_to_genind_str(csv_file)
            csv_revised(genind$new_file)
            strconvert(genind$fsnps_gen)
            
            directory <- tempdir()
            str_path <- revise_structure_file(strconvert(), directory, system = input$systemFile)
            str_file(str_path)
         }, error = function(e) {
            showNotification(paste("Error during STRUCTURE conversion", e$message), type = "error", duration = 20)
         }, finally = {
            enable("csv2str")
         }) # end of try catch 
      })
      shinyjs::enable("csv2str")
      
   })
   
   output$revisedCSV <- DT::renderDataTable({
      req(csv_revised())
      csv_revised()
   }, options = list(pageLength = 10, scrollX = TRUE))
   
   # read the str file
   output$strFile <- DT::renderDataTable({
      req(str_file())
      str_lines <- readLines(str_file(), n = 20)
      str_df <- data.frame(Line = seq_along(str_lines), Content = str_lines)
      str_df
   }, options = list(pageLength = 10, scrollX = TRUE))
   
   output$downloadrevised <- downloadHandler(
      filename = function() {"revised_input.csv"},
      content = function(file) {
         req(csv_revised())
         readr::write_csv(csv_revised(), file)
      }
   )
   
   output$downloadSTRfile <- downloadHandler(
      filename = function() { "structure_file.str"},
      content = function(file){
         req(str_file())
         file.copy(str_file(), file) # double check if reactive object works
      }
   )
   
   output$downloadrevised_UI <- renderUI({
      req(csv_revised())
      downloadButton("downloadrevised", "Download Revised CSV File")
   })
   
   output$downloadSTRfile_UI <- renderUI({
      req(str_file())
      downloadButton("downloadSTRfile", "Download .str File")
   })
   

   #=============== CONCORDANCE ANALYSIS ==================#
   observe({
      toggleState("compareBtn", !is.null(input$concordanceFile1) && !is.null(input$concordanceFile2))
   })
   
   concordanceResult <- reactiveVal(NULL)
   concordancePlotPath <- reactiveVal(NULL)
   
   observeEvent(input$compareBtn, {
      
      Sys.sleep(1.5)
      
      disable("compareBtn")
      
      withProgress(message = "Analyzing files...", value = 0, {
         tryCatch({
            
            req(input$concordanceFile1$datapath, input$concordanceFile2$datapath)
            
            haplo_flag <- input$isHaplotype
            file1_path <- input$concordanceFile1$datapath
            file2_path <- input$concordanceFile2$datapath
            
            result <- calc_concordance(file1_path, file2_path, haplotypes = haplo_flag)
            plot <- plot_concordance(result)
            
            enable("compareBtn")
            
            concordanceResult(result)
            concordancePlotPath(plot)
            
            showNotification("Concordance analysis complete, rendering outputs.", type = "message", duration = 30)
            print(Sys.time())
         }, error = function(e) {
            showNotification(paste("Error during analysis:", e$message), type = "error", duration = 10)
         })
      }) # end of withprogress
      
   }) # end of observe event
   
   output$concordanceResults <- DT::renderDataTable({
      req(concordanceResult())
      concordanceResult()
   }, options = list(
      scrollX = TRUE,
      pageLength = 10
   )
   )
   
   output$concordancePlot <- renderPlot({
      req(concordancePlotPath())
      concordancePlotPath()
   })
   
   output$downloadConcordance <- downloadHandler(
      filename = function() {"concordance.csv"},
      content = function(file) {
         readr::write_csv(concordanceResult(), file)
      }
   )
   
   output$downloadConcordancePlot <- downloadHandler(
      filename = function() {
         paste0("concordance_plot_", Sys.Date(), ".png")
      },
      content = function(file) {
         req(concordancePlotPath())
         ggsave(file, plot = concordancePlotPath(), width = 8, height = 6, dpi = 600)
         
      }, contentType = "image/png"
   )
   
   output$downloadConcordance_UI <- renderUI({
      req(concordanceResult())
      downloadButton("downloadConcordance", "Download Concordance Results")
   })
   
   output$downloadConcordancePlot_UI <- renderUI({
      req(concordancePlotPath())
      downloadButton("downloadConcordancePlot", "Download Concordance Plot")
   })
   
   
   #================ MARKER EXTRACTION ====================#
   output$examplersID <- renderTable({
      data.frame(
         rsID = c("rs101", "rs102", "rs103", "rs104", "...")
      )
   })
   
   output$examplePOS <- renderTable({
      data.frame(
         rsID = c("rs01", "rs04", "..."),
         chromosome = c("1", "2", "..."),
         position = c("104500", "205300", "...")
      )
   })
   
   output$exampleTable <- renderTable({
      data.frame(
         Ind = c("sample1", "sample2", "sample3", "..."),
         rs101 = c("A/A", "A/T", "T/T", "..."),
         rs102 = c("G/C", "G/C", "G/G", "..."),
         rs103 = c("C/C", "C/G", "G/G", "..."),
         rs_n = c("...", "...", "...", "...")
      )
   })
   
   # START MARKER EXTRACTION
   variant_ids <- reactiveVal(NULL)
   rsID_available <- reactiveVal(FALSE)
   extracted_file <- reactiveVal(NULL)
   
   observe({
      toggleState("validateBtn", (!is.null(input$markerFile) ||
                                     (!is.null(input$bedFile) && !is.null(input$bimFile) && !is.null(input$famFile))))
   })
   
   observeEvent(input$validateBtn, {
      req(input$markerFile)  
      
      temp_snplist <- file.path(tempdir(), "temp_snplist.txt")
      
      input_type <- if (!is.null(input$markerFile)) {
         if (grepl("\\.bcf$", input$markerFile$name, ignore.case = TRUE)) "bcf"
         else "vcf"
      } else {
         "plink"
      }
      
      snps <- character(0)
      
      if(input_type %in% c("vcf","bcf")){
         cmd <- paste(
            shQuote(plink2_path),
            paste0("--", input_type), shQuote(input$markerFile$datapath),
            "--write-snplist allow-dups",
            "--out", shQuote(temp_snplist)
         )
         system(cmd)
         
         snps <- tryCatch(readLines(paste0(temp_snplist, ".snplist")),
                          error = function(e) character(0))
         
         # Deduplicate automatically
         snps <- unique(snps)
         
         if(length(snps) == 0) snps <- "."
      }
      
      variant_ids(unique(snps))
      rsID_available(length(snps) > 1 || snps[1] != ".")
   })
   
   output$markerOptionsUI <- renderUI({
      req(variant_ids())
      
      if(rsID_available()){
         tagList(
            h4("Detected rsIDs:"),
            shinycssloaders::withSpinner(DT::DTOutput("variantTable"), type = 4),
            radioButtons("markerType", "Choose Marker Type",
                         choices = c("rsID", "pos"), inline = TRUE),
            conditionalPanel(
               condition = "input.markerType == 'rsID'",
               radioButtons("rsIDInputType", "rsID Input", choices = c("manual", "upload")),
               conditionalPanel(
                  condition = "input.rsIDInputType == 'manual'",
                  textAreaInput("typedrsIDs", "Enter rsIDs (one per line)", rows = 5)
               ),
               conditionalPanel(
                  condition = "input.rsIDInputType == 'upload'",
                  fileInput("markerList1", "Upload rsID List File")
               )
            ),
            conditionalPanel(
               condition = "input.markerType == 'pos'",
               fileInput("markerList2", "Upload POS List (.csv, .xlsx)"),
               checkboxInput("addrsID", "Add marker information/rsID to output?", value = FALSE)
            ),
            shinyjs::disabled(actionButton("extractBtn", "Run Marker Extraction", icon = icon("play")))
         )
      } else {
         tagList(
            h4("No rsIDs detected. Extraction will require a POS list."),
            fileInput("markerList2", "Upload POS List (.csv, .xlsx)"),
            checkboxInput("addrsID", "Add marker information/rsID to output?", value = FALSE),
            shinyjs::disabled(actionButton("extractBtn", "Run Marker Extraction", icon = icon("play")))
         )
      }
   })

   output$variantTable <- DT::renderDT({
      snps <- variant_ids()
      req(!is.null(snps))
      # Filter out placeholder if you want to display nothing when no rsIDs
      display_snps <- if(all(snps == ".")) character(0) else snps
      
      DT::datatable(data.frame(Variant_ID = display_snps),
                    options = list(scrollX = TRUE, pageLength = 10))
   })
   
   can_extract <- reactive({
      if(!rsID_available()){
         !is.null(input$markerList2)
      } else {
         if(is.null(input$markerType)) return(FALSE)
         
         if(input$markerType == "rsID"){
            if(is.null(input$rsIDInputType)) return(FALSE)
            if(input$rsIDInputType == "manual"){
               nzchar(trimws(input$typedrsIDs))
            } else if(input$rsIDInputType == "upload"){
               !is.null(input$markerList1)
            } else FALSE
         } else if(input$markerType == "pos"){
            !is.null(input$markerList2)
         } else FALSE
      }
   })
   
   observe({
      shinyjs::toggleState("extractBtn", condition = can_extract())
   })
   
   observeEvent(input$extractBtn, {
      
      disable("extractBtn")
      
      
      temp_dir <- tempdir()
      
      tryCatch({
         
         pgen_prefix <- file.path(temp_dir, "input_pgen")
         
         if (!is.null(input$markerFile)) {
            input_file <- input$markerFile$datapath
            
            converted_to_plink2(input_file, isplink = FALSE, plink_path = plink2_path, name = pgen_prefix)
            
         } else {
            
            bed_prefix <- tools::file_path_sans_ext(input$bedFile$datapath)
            converted_to_plink2(bed_prefix, isplink = TRUE, plink_path = plink2_path, name = pgen_prefix)

         }
         
         merged_name <- "extracted_markers"
         
         if (rsID_available() && input$markerType == "rsID") {
            
            snps_list <- tempfile(fileext = ".txt")
            if (input$rsIDInputType == "manual") {
               
               rsIDs <- trimws(unlist(strsplit(input$typedrsIDs, "\n")))
               rsIDs <- rsIDs[nzchar(rsIDs)]
               
               writeLines(rsIDs, snps_list)
               
            } else {
               df <- load_csv_xlsx_files(input$markerList1$datapath)
               writeLines(as.character(df[[1]]), snps_list)
            }
            
            extracted <- extract_by_ID_pgen(
               pgen_prefix = pgen_prefix,
               snps_list = snps_list,
               output_dir = temp_dir,
               merged_file = merged_name,
               plink_path = plink2_path
            )
         } else {
         
            req(input$markerList2)
            pos_list <- as.data.frame(load_csv_xlsx_files(input$markerList2$datapath))
            
            if (ncol(pos_list) < 3)
               stop("Position file must contain: rsID, chr, pos")
            
            colnames(pos_list)[1:3] <- c("rsID","chr","pos")
            range_file <- create_range_file(pos_list, temp_dir)
            
            if (isTRUE(input$addrsID)) {
               
               extracted <- extract_POStoID_pgen(
                  pos_list = pos_list,
                  pgen_prefix = pgen_prefix,
                  output_dir = temp_dir,
                  plink_path = plink2_path
               )
               
            } else {
               
               out_prefix <- file.path(temp_dir, merged_name)
               cmd_extract <- paste(
                  shQuote(plink2_path),
                  "--pfile", shQuote(pgen_prefix),
                  "--extract range", shQuote(range_file$range_file),
                  "--export vcf",
                  "--out", shQuote(out_prefix)
               )
               
               system(cmd_extract)
               extracted <- paste0(out_prefix, ".vcf")
               
               if (!file.exists(extracted))
                  stop("PLINK extraction failed: no VCF generated.")
            }
         }
         extracted_file(extracted)
         
         showNotification(
            "Marker extraction completed successfully.",
            type = "message"
         )
         
      },
      error = function(e) {
         showNotification(
            paste("Extraction error:", e$message),
            type = "error",
            duration = 10
         )
      },
      finally = {
         
         enable("extractBtn")
      })
      
   })
   
   
   output$downloadVCF <- downloadHandler(
      filename = function() {
         paste0("extracted_markers_", Sys.Date(), ".vcf")
      },
      content = function(file) {
         req(extracted_file())
         file.copy(extracted_file(), file, overwrite = TRUE)
      }
   )
   
   output$downloadVCF_UI <- renderUI({
      req(extracted_file())
      downloadButton("downloadVCF", "Download Extracted File (VCF)")
   })
   

   #===================== FILTERING =======================#
   observe({
      hasFile <- !is.null(input$markerFileFilter)
      anyFilter <- input$filterIndiv || input$filterVariant || input$filterAllele || input$filterQuality || input$filterHWE || input$filterLD
      shinyjs::toggleState("calcDP", condition = hasFile && anyFilter)
   })
   
   output$filterWarning <- renderText({
      if (!(input$filterIndiv || input$filterVariant || input$filterAllele || input$filterQuality || input$filterHWE || input$filterLD)){
         "Please select at least one filtering option."
      } else {
         ""
      }
   })
   
   # reset buttons with new file
   observeEvent(input$markerFileFilter, {
      ext <- tools::file_ext(input$markerFileFilter$name)
      if (tolower(ext) == "vcf"){
         shinyjs::enable("enableDp")
      } else {
         updateCheckboxInput(inputId = "enableDP", value = FALSE)
         shinyjs::disable("enableDP")
      }
      
   })
   
   temp_dir <- tempdir()
   depth_outputs <- reactiveVal(NULL)
   filtered_plink_file <- reactiveVal(NULL)
   
   observeEvent(input$calcDP, {
      req(input$markerFileFilter)
      
      Sys.sleep(1.5)
      
      vcf_path <- input$markerFileFilter$datapath
      ref_path <- if (!is.null(input$highlightRef)) input$highlightRef$datapath else NULL
      palette <-  if (!is.null(input$colorPalette)) input$colorPalette else NULL
      
      ext <- tools::file_ext(input$markerFileFilter$name)
      
      if (tolower(ext) == "vcf" && input$enableDP){
         dp <- depth_from_vcf(
            vcf = vcf_path,
            output.dir = temp_dir,
            reference = ref_path,
            palette = palette
         )
         depth_outputs(dp)
      }
      
      # CHECK THE FILE EXTENSIONS
      input_type <- if (!is.null(input$markerFileFilter)) {
         if (grepl("\\.bcf$", input$markerFileFilter$name, ignore.case = TRUE)) "bcf"
         else "vcf"
      } else {
         "plink"
      }
      
      # convert
      pgen_prefix <- file.path(temp_dir, "converted_to_plink2")
      if (input_type %in% c("vcf", "bcf")) {
         converted_to_plink2(input$markerFileFilter$datapath, isplink = FALSE, plink_path = plink2_path, name = pgen_prefix)
      } else {
         bed_prefix <- tools::file_path_sans_ext(input$bedFileFilter$datapath)
         converted_to_plink2(bed_prefix, isplink = TRUE, plink_path = plink2_path, name = pgen_prefix)
      }
      
      # Revised 14 November 2025 to first convert files to PLINK before filtering
      input_file <- convert_to_plink(input$markerFileFilter$datapath, temp_dir)
      
      # for plink filtering
      plink_cmds <- c(shQuote(plink2_path), "--pfile", shQuote(pgen_prefix), "--out", file.path(temp_dir, "filtered"))
      
      if (input$filterIndiv){
         plink_cmds <- c(plink_cmds, "--mind", input$mindThresh)
      }
      if (input$filterVariant){
         plink_cmds <- c(plink_cmds, "--geno", input$genoThresh)
      }
      if (input$filterAllele){
         plink_cmds <- c(plink_cmds, "--maf", input$mafThresh)
      }
      if (input$filterQuality){
         plink_cmds <- c(plink_cmds, "--qual-threshold", input$qualThresh)
      }
      if (input$filterHWE){
         plink_cmds <- c(plink_cmds, "--hwe", input$qualHWE, input$kval)
      }
      if (input$filterLD){
         plink_cmds <- c(plink_cmds, "--indep-pairwise", input$ldWindow, input$ldStep, input$ldR2)
      }
      if (input$cutoffKing){
         plink_cmds <- c(plink_cmds, "--king-cutoff", input$kingThresh)
      }
      
      # for custom flags
      custom_flag <- strsplit(trimws(input$customFilter), "\\s+")[[1]]
      if (length(custom_flag) == 1 && custom_flag[1] == "") custom_flag <-  NULL
      
      if (!is.null(custom_flag) && length(custom_flag) >= 2){
         if (!is.null(input$extraFile1) && length(custom_flags) >=2){
            custom_flag[2] <- shQuote(input$extraFile1$datapath)
         }
         
         if (!is.null(input$extraFile2) && length(custom_flags) >=4){
            custom_flag[4] <- shQuote(input$extraFile2$datapath)
         }
         
         plink_cmds <- c(plink_cmds, custom_flags)}
      
      system(paste(plink_cmds, collapse = " "))
      
      filtered_path <- file.path(temp_dir, "filtered.vcf")
      if (file.exists(filtered_path)){
         filtered_plink_file(filtered_path)
         shinyjs::enable("downloadFilteredFile")
      }
      
      output$plinkCommandPreview <- renderText({
         paste("plink", paste(plink_cmds, collapse = " "))
      })
      
      
   }) # end of observe events
   
   output$depthMarkerPlot <- renderImage({
      req(depth_outputs())
      list(src = depth_outputs()$plot_marker, contentType = "image/png", width = "100%")
   }, deleteFile = FALSE)
   
   output$depthSamplePlot <- renderImage({
      req(depth_outputs())
      list(src = depth_outputs()$plot_sample, contentType = "image/png", width = "100%")
   }, deleteFile = FALSE)
   
   output$downloadFilteredFile <- downloadHandler(
      filename = function(){"filtered.vcf"},
      content = function(file){
         req(filtered_plink_file())
         file.copy(filtered_plink_file(), file)
      }
   )
   
   output$downloadDepthMarkerPlot <- downloadHandler(
      filename = function() {
         "Depth_marker.png"
      },
      content = function(file) {
         req(depth_outputs())
         file.copy(depth_outputs()$plot_marker, file)
      }
   )
   
   output$downloadDepthSamplePlot <- downloadHandler(
      filename = function() {
         "Depth_samples.png"
      },
      content = function(file) {
         req(depth_outputs())
         file.copy(depth_outputs()$plot_sample, file)
      }
   )
   
   output$depthMarkerPlot_UI <- renderUI({
      req(depth_outputs()$plot_marker)
      downloadButton("downloadDepthMarkerPlot", "Download Marker Depth Plot")
   })
   
   output$depthSamplePlot_UI <- renderUI({
      req(depth_outputs()$plot_sample)
      downloadButton("downloadDepthSamplePlot", "Download Sample Depth Plot")
   })
   
   output$downloadFilteredFile_UI <- renderUI({
      req(filtered_plink_file())
      downloadButton("downloadFilteredFile", "Download Filtered File")
   })
   

   #============ MULTIPLE SEQUENCE ALIGNMENT ==============#
   fasta_data <- reactiveVal(NULL)
   alignment_msa <- reactiveVal(NULL)
   alignment_scores <- reactiveVal(NULL)
   alignment_adjusted <- reactiveVal(NULL)
   alignment_staggered <- reactiveVal(NULL)
   directory <- tempdir()
   
   observeEvent(input$runMSA, {
      req(input$fastaFile)
      
      Sys.sleep(1.5)
      fasta <- read_fasta(input$fastaFile$datapath, directory)
      fasta_data(fasta)

      aligned <- calc_msa(fasta, 
                             algorithm = input$substitutionMatrix)
      
      alignment_msa(aligned$alignment)
      alignment_scores(aligned$scores)
      alignment_adjusted(aligned$adjusted)
      alignment_staggered(aligned$staggered)
      
      
   }) # end of observe event for run msa
   
   output$msaView <- msaR::renderMsaR({
      req(alignment_msa())
      msaR::msaR(alignment_msa())
   })
   
   output$adjustedAlignmentText <- renderPrint({
      req(alignment_adjusted())
      alignment_adjusted()
   })
   
   output$staggeredAlignmentText <- renderPrint({
      req(alignment_staggered())
      alignment_staggered()
   })
   
   output$alignmentScoresPreview <- renderPrint({
      req(alignment_scores())
      alignment_scores()
   })
   
   
   # Changed writeLines to seqinr::write.fasta
   # 20 Nov 2025 used bios2mds to download msa to fasta
   output$downloadAlignedFASTA <- downloadHandler(
      filename = function() {paste0("aligned", input$msaDownloadType, "_sequence.fasta")},
      content = function(file){
         alignment <- switch(input$msaDownloadType,
                             initial = alignment_msa(),
                             adjusted = alignment_adjusted(),
                             staggered = alignment_staggered())

         aligned <- msa::msaConvert(alignment, "bios2mds::align")
         bios2mds::export.fasta(aligned, outfile = file, open = "w")
      }
   )
   
   output$downloadAlignmentScores <- downloadHandler(
      filename = function() { "alignment_scores.txt"},
      content = function(file){
         writeLines(utils::capture.output(print(alignment_scores())), file)   
      }
   )
   
   output$downloadAlignmentPDF <- downloadHandler(
      filename = function(){
         "aligned_seqs.pdf"
      },
      contentType = "application/pdf",
      content = function(file){
         alignment <- switch(input$msaDownloadType,
                             initial = alignment_msa(),
                             adjusted = alignment_adjusted(),
                             staggered = alignment_staggered())
         
         pdf_name = "aligned_seqs.pdf"
         msa::msaPrettyPrint(alignment, file = pdf_name,
                             output="pdf", showNames= "left", showLogo = "none", askForOverwrite = FALSE)
         file.copy(pdf_name, file, overwrite = TRUE)
         
      }
   )
   
   
   output$downloadAlignedFASTA_UI <- renderUI({
      alignment <- switch(input$msaDownloadType,
                          initial = alignment_msa(),
                          adjusted = alignment_adjusted(),
                          staggered = alignment_staggered())
      req(alignment)
      downloadButton("downloadAlignedFASTA", "Download Aligned Sequences")
   })
   output$downloadAlignmentScores_UI <- renderUI({
      req(alignment_scores())
      downloadButton("downloadAlignmentScores", "Download Alignment Scores")
   })

   
   #=========== PHYLOGENETIC TREE CONSTRUCTION ============#
   tree_plot <- reactiveVal(NULL)
   tree_path <- reactiveVal(NULL)
   tree_model <- reactiveVal(NULL)
   
   observeEvent(input$buildTree, {
      Sys.sleep(1.5)
      disable("buildTree")
      
      if (!is.null(msaFileforPhylogen)) {
         req(alignment_msa())
         alignment_file <- alignment_msa()
      } else {
         file_ext_ref <- tools::file_ext(input$msaFileforPhylogen$name)
         if (file_ext_ref == "msa") {
            alignment_file <- Biostrings::ReadDNAStringSet(input$msaFileforPhylogen$datapath)
         } else if (file_ext_ref %in% c("fasta", "msf", "aln")) {
            if (file_ext_ref == "aln") { file_ext_ref <- "clustal" } 
            alignment_file <- seqinr::read.alignment(file = input$msaFileforPhylogen$datapath, format = file_ext_ref)
         }
      }

      withProgress(message = "Building phylogenetic tree...", value = 0, {
         tryCatch({
            tree_type <- input$treeType
            outgroup <- input$outgroup
            bs <- input$boostrapSamples
            model <- input$model
            aligned <- alignment_file
            directory <- tempdir()
            
            if (!is.null(input$seed)){
               seed <- input$seed
            } else {
               seed <- "123"
            }
            
            if (tree_type == "NJ"){
               plot_obj <- build_nj_tree(aligned, outgroup = outgroup, model = model, seed = seed)
               tree_plot(plot_obj)
               tree_path(NULL)
               tree_model(paste("NJ C", model, ")", sep = ""))
            } else if (tree_type == "UPGMA"){
               plot_obj <- build_upgma_tree(aligned, outgroup = outgroup, model = model, seed = seed)
               tree_plot(plot_obj)
               tree_path(NULL)
               tree_model(paste("UPGMA (", model, ")", sep=""))
            } else if (tree_type == "Parsimony"){
               path <- build_max_parsimony(aligned, outgroup = outgroup, directory = directory, seed = seed)
               tree_plot(NULL)
               tree_path(path)
               tree_model("Parsimony")
            } else if (tree_type == "Maximum Likelihood"){
               results <- build_ml_tree(aligned, outgroup=outgroup, directory = directory, seed = seed, bs = bs)
               tree_plot(NULL)
               tree_path(results$filename)
               tree_model(results$best_model)
            }
            showNotification("Tree construction complete.", type = "message")

         },error = function(e){
            showNotification(paste("Error during tree construction:", e$message), type = "error", duration = 20)
         }) # end of try catch
         
         enable("buildTree")
      }) # end of with progress
      
      
   }) # end of observe event tree
   
   output$treeImage <- renderUI({
      if (!is.null(tree_plot())){
         plotOutput("treePlot")
      } else if (!is.null(tree_path())) {
         imageOutput("treePNG")
      }
   })
   
   output$treePlot <- renderPlot({
      req(tree_plot())
      tree_plot()
   })
   
   output$treePNG <- renderImage({
      req(tree_path())
      list(src = tree_path(), contentType = "image/png", width = 800, height = 600)
   }, deleteFile = FALSE)
   
   output$downloadTree <- downloadHandler(
      filename = function() {"tree.png"},
      content = function(file){
         if (!is.null(tree_plot())) {
            ggsave(file, plot = tree_plot(), width = 8, height = 6, dpi = 600)
         } else if (!is.null(tree_path())) {
            file.copy(tree_path(), file)
         }
      }
   )
   
   output$downloadTree_UI <- renderUI({
      if (!is.null(tree_plot()) || !is.null(req(tree_path()))){
      downloadButton("downloadTree", "Download Phylogenetic Tree")}
   })
   
   
   #===================== BARCODING =======================#
   #------------------ Species identification
   observe({ 
      refReady = !is.null(input$refBarcoding)
      queReady = !is.null(input$queBarcoding)
      
      toggleState("identifySpecies", refReady && queReady)
   })
   
   resultIdentity <- reactiveVal(NULL)
   refseq <- reactiveVal(NULL)
   queseq <- reactiveVal(NULL)
   
   observeEvent(input$identifySpecies, {
      disable("identifySpecies")
      
      Sys.sleep(1.5)
      
      req(input$refBarcoding)
      req(input$queBarcoding)
      
      # read file
      # 13 March 2026: removed rphast
      file_ext_ref <- tools::file_ext(input$refBarcoding$name)
      if (file_ext_ref == "msa") {
         barcoding_ref <- Biostrings::ReadDNAStringSet(input$refBarcoding$datapath)
      } else if (file_ext_ref %in% c("fasta", "msf", "aln")) {
         if (file_ext_ref == "aln") { file_ext_ref <- "clustal" } 
         barcoding_ref <- seqinr::read.alignment(file = input$refBarcoding$datapath, format = file_ext_ref)
      }
      
      file_ext_que <- tools::file_ext(input$queBarcoding$name)
      if (file_ext_que == "msa") {
         barcoding_que <- Biostrings::ReadDNAStringSet(input$queBarcoding$datapath)
      } else if (file_ext_que %in% c("fasta", "msf", "aln")) {
         if (file_ext_que == "aln") { file_ext_que <- "clustal" } 
         barcoding_que <- seqinr::read.alignment(file = input$queBarcoding$datapath, format = file_ext_que)
      }
      
      ref_seq <- ape::as.DNAbin(as.character(barcoding_ref))
      que_seq <- ape::as.DNAbin(as.character(barcoding_que))
      refseq(ref_seq)
      queseq(que_seq)
      
      # If not using kmer method
      if (input.kmerSelect == "false"){
         result_identity <- BarcodingR::barcoding.spe.identify(refseq(), queseq(), method = input$barcodingMethod)
      }
      
      if (input.kmerType == 'Fuzzy-set Method and kmer'){
         req(input$kmerValue)
         req(input$optimizationKMER)
         
         result_identity <- BarcodingR::barcoding.spe.identify2(refseq(), queseq(), kmer = input$kmerValue, optimization = input$optimizationKMER)
         
      } else if (input.kmerType == 'BP-based Method and kmer') {
         req(input$kmerValue)
         req(input$builtModel)
         req(input$lrValue)
         req(input$maxitValue)
         result_identity <- BarcodingR::bbsik(refseq(), queseq(), kmer = input$kmerValue, UseBuiltModel = input$builtModel, lr = input$lrValue, maxit = input$maxitValue)
      }
      
      resultIdentity(result_identity)
      
   })
   
   output$identificationResult <- renderPrint({
      req(resultIdentity())
      resultIdentity()
   })
   
   #----------------- Optimize kmer values
   observe({
      fileReady <- !is.null(input$optimizeKmerRef)
      toggleState("calOptimumKmer", fileReady)
   })
   
   kmerFile <- reactiveVal(NULL)
   optimalKmer <- reactiveVal(NULL)
   
   observeEvent(input$calOptimumKmer, {
      disable("calOptimumKmer")
      req(input$optimizeKmerRef)
      
      Sys.sleep(1.5)
      
      barcoding_ref <- rphast::read.msa(input$optimizeKmerRef$datapath, format = rphast::guess.format.msa(input$optimizeKmerRef$datapath, method = "content"))
      kmer_File <- ape::as.DNAbin(as.character(barcoding_ref))
      optimal_Kmer <- BarcodingR::optimize.kmer(kmerFile, max.kmer = input$maxKmer)
      kmerFile(kmer_File)
      optimalKmer(optimal_Kmer)
      
      
   }) # end of observe event
   
   # download
   output$kmerResult <- renderPrint({
      req(optimalKmer())
      as.data.frame(optimalKmer())
   })
   
   output$kmerPlot <- renderImage({
      req(kmerFile())
      req(input$maxKmer)
      BarcodingR::optimize.kmer(kmerFile(), max.kmer = input$maxKmer)
   }, deleteFile = FALSE)
   
   output$downloadKmerPlot <- downloadHandler(
      filename = function(){
         paste0("optimum_kmer_", Sys.Date(), ".png")
      },
      content = function(file){
         req(kmerFile())
         req(input$maxKmer)
         BarcodingR::optimize.kmer(kmerFile(), max.kmer = input$maxKmer)
      }, contentType = "image/png"
   )
   
   output$downloadKmerPlot_UI <- renderUI({
      req(kmerFile())
      downloadButton("downloadKmerPlot", "Download Kmer Plot")
   })
   
   #---------------------- barcoding gap
   observe({
      gapReady <- !is.null(input$barcodeRef)
      toggleState("gapBarcodes", gapReady)
   })
   
   refBarcode <- reactiveVal(NULL)
   barcodeGap <- reactiveVal(NULL)
   
   observeEvent(input$gapBarcodes, {
      disable("gapBarcodes")
      req(input$barcodeRef)
      
      Sys.sleep(1.5)
      
      barcoding_ref <- rphast::read.msa(input$barcodeRef$datapath, format = rphast::guess.format.msa(input$barcodeRef$datapath, method = "content"))
      ref_Barcode <- ape::as.DNAbin(as.character(barcoding_ref))
      gap <- BarcodingR::barcoding.gap(refBarcode, dist = input$gapModel)
      refBarcode(ref_Barcode)
      barcodeGap(gap)
      
   }) # end of observe event
   
   # download
   output$barcodingResult <- renderPrint({
      req(barcodeGap())
      as.data.frame(barcodeGap())
   })
   
   output$BarcodingGapPlot <- renderImage({
      req(refBarcode())
      req(refseq())
      req(input$gapModel)
      BarcodingR::barcoding.gap(refseq(), dist = input$gapModel)
   }, deleteFile = FALSE)
   
   output$downloadGapPlot <- downloadHandler(
      filename = function(){
         paste0("barcoding_gap_", Sys.Date(), ".png")
      },
      content = function(file){
         req(refBarcode())
         req(input$gapModel)
         BarcodingR::barcoding.gap(refBarcode(), dist = input$gapModel)
      }, contentType = "image/png"
   )
   
   output$downloadGapPlot_UI <- renderUI({
      req(refBarcode())
      downloadButton("downloadGapPlot", "Download Gap Plot")
   })
   
   #--------------------------- barcodes eval
   observe({ 
      barcode1 = !is.null(input$barcode1)
      barcode2 = !is.null(input$barcode2)
      toggleState("evalBarcodes", barcode1 && barcode2)
   })
   
   resultBarcodes <- reactiveVal(NULL)
   
   observeEvent(input$evalBarcodes, {
      disable("evalBarcodes")
      req(input$barcode1)
      req(input$barcode2)
      
      Sys.sleep(1.5)
      
      b1 <- rphast::read.msa(input$barcode1$datapath, format = rphast::guess.format.msa(input$barcode1$datapath, method = "content"))
      b2 <- rphast::read.msa(input$barcode2$datapath, format = rphast::guess.format.msa(input$barcode2$datapath, method = "content"))
      barcode1 <- ape::as.DNAbin(as.character(b1))
      barcode2 <- ape::as.DNAbin(as.character(b2))
      
      # convert to dataframe to download
      result <- BarcodingR::barcodes.eval(barcode1, barcode2, kmer1 = kmer1, kmer2 = kmer2)
      resultBarcodes(result)
      
   }) # end of observe event
   
   output$evalBarcodesResult <- renderTable({
      req(resultBarcodes())
      result2 <- as.data.frame(resultBarcodes())
      result2
   })
   
   #---------------------------- tdr2
   observe({
      file1Ready <- !is.null(input$oneSpe)
      file2Ready <- !is.null(input$queSpe)
      toggleState("calculateTDR2", file1Ready && file2Ready)
   })
   
   queTDR <- reactiveVal(NULL)
   refTDR <- reactiveVal(NULL)
   
   observeEvent(input$calculateTDR2, {
      disable(calculateTDR2)
      req(input$oneSpe)
      req(input$queSpe)
      
      Sys.sleep(1.5)
      
      que <- rphast::read.msa(input$oneSpe$datapath, format = rphast::guess.format.msa(input$oneSpe$datapath, method = "content"))
      ref <- rphast::read.msa(input$queSpe$datapath, format = rphast::guess.format.msa(input$queSpe$datapath, method = "content"))
      
      query <- ape::as.DNAbin(as.character(que))
      reference <- ape::as.DNAbin(as.character(ref))
      
      queTDR(query)
      refTDR(reference)
      
   })
   
   # issue with results, it prints and not stores
   output$tdrValues <- renderPrint({
      req(queTDR())
      req(refTDR())
      req(input$bootValue1)
      req(input$bootValue2)
      BarcodingR::TDR2(queTDR(), refTDR(), boot = input$bootValue1, boot2 = input$bootValue2)
   })
   
   
   #=============== POPULATION STATISTICS =================#
   examplePop <-data.frame(
         Sample = c("Sample1", "Sample2", "Sample3", "Sample4", "..."),
         Population = c("POP1", "POP2", "POP3", "POP4", "..."),
         rs101 = c("A/A", "A/T", "A/A", "T/T", "..."),
         rs102 = c("G/G", "C/C", "G/C", "G/G", "..."),
         rs_n = c("...", "...", "...", "...", "...")
      )
   
   output$examplePop_UI <- DT::renderDataTable({
      req(examplePop)
      examplePop
   }, options = list(
      scrollX = TRUE,
      pageLength = 5
   )
   )
   
   observe({
      file_ready <- !is.null(input$popStatsFile)
      shinyjs::toggleState("runPopStats", condition = file_ready)
   })
   
   privAlleles <- reactiveVal(NULL)
   popStats <- reactiveVal(NULL)
   hardyWeinberg <- reactiveVal(NULL)
   fstStats <- reactiveVal(NULL)
   fstData <- reactiveVal(NULL)
   afData <- reactiveVal(NULL)
   
   statsMatrix <- reactiveVal(NULL)
   hwMatrix <- reactiveVal(NULL)
   fstMatrix <- reactiveVal(NULL)
   
   observeEvent(input$runPopStats, {
      disable("runPopStats")
      req(input$popStatsFile)
      
      Sys.sleep(1.5)
      
      fsnps_gen <- reactive({
         req(input$popStatsFile)
         df <- load_csv_xlsx_files(input$popStatsFile$datapath)
         cleaned <- clean_input_data(df)
         convert_to_genind(cleaned)
      })
      
      withProgress(message = "Running population analysis...", value = 0, {
         
         tryCatch({
            req(fsnps_gen())
            
            incProgress(0.4, detail = "Computing private alleles...")
            
            priv_alleles <- poppr::private_alleles(fsnps_gen())
            if (is.null(priv_alleles)) {priv_alleles <- list(message = "No private alleles detected")
            } else {
               priv_alleles <- data.frame(rownames(priv_alleles), priv_alleles)
               priv_alleles <- dplyr::rename(priv_alleles, Pop = 1)
               rownames(priv_alleles) <- NULL
            }
            privAlleles(priv_alleles)
            
            incProgress(0.6, detail = "Computing population statistics...")
            population_stats <- compute_pop_stats(fsnps_gen())
            popStats(population_stats)
            
            ## AF
            af_stats <- compute_af(fsnps_gen())
            afData(af_stats)
            
            ## HWE
            incProgress(0.8, detail = "Running HWE and FST calculations...")
            hardy_weinberg_stats <- compute_hwe(fsnps_gen())
            hardyWeinberg(hardy_weinberg_stats)
            
            ## FST
            incProgress(1.0, detail = "Still running HWE and FST calculations...")
            
            fst_stats <- compute_fst(fsnps_gen())
            fstStats(fst_stats)
            
            
            fst_data <- fstStats()$fst_dataframe
            fstData(fst_data)
            
            showNotification("Rendering outputs, this might take some time...", type = "message", duration = 30)
            print(Sys.time())
            
            enable("runPopStats")
         }, error = function(e) {
            showNotification(paste("Population stats error:", e$message), type = "error")
            enable("runPopStats")
         })
         
         stats_matrix <- compute_pop_stats(fsnps_gen())
         hw_matrix <- compute_hwe(fsnps_gen())
         fst_matrix <- compute_fst(fsnps_gen())
         
         statsMatrix(stats_matrix)
         hwMatrix(hw_matrix)
         fstMatrix(fst_matrix)
      }) #end of withProgress
      
      
   })
   
   output$privateAlleleTable <- DT::renderDataTable({
      as.data.frame(privAlleles())
   }, options = list(pageLength = 10, scrollX = TRUE))
   
   output$meanallelic <- DT::renderDataTable({
      popStats()$mar_list
   })
   
   # heterozygosity
   output$heterozygosity_table <- DT::renderDataTable({
      popStats()$heterozygosity
   })
   
   output$heterozygosity_plot <- renderImage({
      req(popStats()$heterozygosity)
      
      plot_path <- plot_heterozygosity(
         Het_fsnps_df = popStats()$heterozygosity,
         out_dir = tempdir()
      )
      
      list(
         src = plot_path,
         contentType = "image/png",
         alt = "Heterozygosity Plot",
         width = "100%"
      )
   }, deleteFile = TRUE)
   
   output$inbreeding_table <- DT::renderDataTable({
      popStats()$inbreeding_coeff
   })
   
   output$ttest_table <- DT::renderDataTable({
      popStats()$ttest
   })
   
   output$allele_freq_table <- DT::renderDataTable({
      afData()
   }, options = list(scrollX = TRUE))
   
   
   output$hwe_summary_text <- DT::renderDataTable({
      hardyWeinberg()$hw_summary
   }, options = list(scrollX = TRUE))
   
   output$hwe_chisq_table <- DT::renderDataTable({
      hardyWeinberg()$hw_dataframe
   }, options = list(scrollX = TRUE))
   
   output$fstMatrixUI <- renderUI({
      fst <- fstStats()$fsnps_fst_matrix
      if (is.list(fst) && "message" %in% names(fst)) {
         tags$p(style = "color:gray;", fst$message)
      } else {
         DT::dataTableOutput("fstMatrixTable")
      }
   })
   
   output$fstMatrixTable <- DT::renderDataTable({
      matrix_data <- matrix(unlist(fstStats()$fst_matrix),
                            nrow = sqrt(length(fstStats()$fst_matrix)),
                            byrow = TRUE)
      rownames(matrix_data) <- colnames(matrix_data) <- attr(fsnps_gen(), "pop.names")
      as.data.frame(matrix_data)
   }, options = list(scrollX = TRUE))
   
   output$fstDfTable <- DT::renderDataTable({
      fstStats()$fst_dataframe
   })
   
   
   output$fst_heatmap_plot <- renderImage({
      req(fstData())
      
      plot_path <- plot_fst(
         fst_df = fstData(),
         out_dir = tempdir()
      )
      
      list(
         src = plot_path,
         contentType = "image/png",
         alt = "FST Heatmap",
         width = "100%"
      )
   }, deleteFile = TRUE)
   
   # download heterozygosity plot
   output$downloadHeterozygosityPlot <- downloadHandler(
      filename = function() {
         "heterozygosity_plot.png"
      },
      content = function(file) {
         plot_path <- plot_heterozygosity(
            Het_fsnps_df = popStats()$heterozygosity,
            out_dir = tempdir()
         )
         file.copy(plot_path, file)
      }
   )
   
   output$downloadHeterozygosityPlot_UI <- renderUI({
      downloadButton("downloadHeterozygosityPlot", "Download Heterozygosity Plot")
   })
   
   output$downloadFstHeatmap <- downloadHandler(
      filename = function() {
         "fst_heatmap.png"
      },
      content = function(file) {
         plot_path <- plot_fst(
            fst_df = fstData(),
            out_dir = tempdir()
         )
         file.copy(plot_path, file)
      }
   )
   
   output$downloadFstHeatmap_UI <- renderUI({
      downloadButton("downloadFstHeatmap", "Download Fst Plot")
   })
   
   ## download all results
   output$downloadStatsXLSX <- downloadHandler(
      filename = function() {
         timestamp <- format(Sys.time(), "%Y%m%d_%H%M")
         paste0("population-statistics-results_", timestamp, ".xlsx")
      },
      content = function(file) {
         req(statsMatrix(), hwMatrix(), fstMatrix(), privAlleles(), afData())
         path <- export_pop_results(allele_freq = afData(),
                                    priv_alleles = privAlleles(), 
                                    stats_matrix = statsMatrix(), 
                                    hw_matrix = hwMatrix(), 
                                    fst_matrix = fstMatrix(), dir = tempdir())
         
         file.copy(path, file)
         #openxlsx::write.xlsx(path, file = filename)
      }
   )
   
   output$downloadStatsXLSX_UI <- renderUI({
      req(statsMatrix(), hwMatrix(), fstMatrix(), privAlleles())
      downloadButton("downloadStatsXLSX", "Download Results (excel)")
   })
   
   #=============== FORENSIC PARAMETERS ===================#
   referenceData <-data.frame(
      Sample = c("Sample1", "Sample2", "Sample3", "Sample4", "..."),
      Population = c("POP1", "POP2", "POP3", "POP4", "..."),
      rs101 = c("A/A", "A/T", "A/A", "T/T", "..."),
      rs102 = c("G/G", "C/C", "G/C", "G/G", "..."),
      rs_n = c("...", "...", "...", "...", "...")
   )
   
   afSample <-data.frame(
      markers = c("rs101.A", "rs101.T", "rs102.C", "rs102.G", "..."),
      POP1 = c("0.18518", "0.81481", ".77777", "0.22222", "..."),
      POP2 = c("0.89285", "0.10714", "0.89285", "0.10714", "..."),
      POP3 = c("0.15789", "0.84210", "0.87894", "0.12105", "..."),
      POPn = c("...", "...", "...", "...", "...")
   )
   
   profileSample <-data.frame(
      markers = c("rs101", "rs102", "rs103", "rs104", "..."),
      profile = c("A/T", "G/C", "G/A", "T/T", "...")
   )
   
   output$referenceData_UI <- DT::renderDataTable({
      req(referenceData)
      referenceData
   }, options = list(
      scrollX = TRUE,
      pageLength = 5
   )
   )

   output$afSample_UI <- DT::renderDataTable({
      req(afSample)
      afSample
   }, options = list(
      scrollX = TRUE,
      pageLength = 5
   )
   )
   
   output$profileSample_UI <- DT::renderDataTable({
      req(profileSample)
      profileSample
   }, options = list(
      scrollX = TRUE,
      pageLength = 5
   )
   )
   
   # reactive storage
   results_rv <- reactiveValues(
      overall_metrics = NULL,
      pop_metrics = NULL,
      rmp_value = NULL
   )
   genotype_freqs <- reactiveVal(NULL)
   gt_freq_all <- reactiveVal(NULL)
   gt_freq_pop <- reactiveVal(NULL)
   snpsFile <- reactiveVal(NULL)
   
   # toggle action button only if a file is uploaded
   observe({
      shinyjs::toggleState("calcIISNPs", !is.null(input$iisnpsFile))
   })

   
   # main calculation
   observeEvent(input$calcIISNPs, {
      shinyjs::disable("calcIISNPs")
      req(input$iisnpsFile)
      
      fileUploaded <- load_csv_xlsx_files(input$iisnpsFile$datapath)
      data_type <- evaluate_file(fileUploaded)
      snpsFile(fileUploaded)
      
      computed_af <- NULL
      pop <- NULL
      
      if (data_type == "gts") {
         file <- clean_input_data(snpsFile())
         file <- convert_to_genind(file)
         computed_af <- compute_af(file)
         pop <- nrow(file)
      } else if (data_type == "freqs") {
         computed_af <- snpsFile()
      }
      
      # optional profile
      profile_df <- NULL
      theta <- 0
      pop <- NULL
      if (!is.null(input$fileProfile)){
         profile_df <- load_csv_xlsx_files(input$fileProfile$datapath)
         theta <- input$thetaValue
      }
      
      if (!is.null(input$floorCeiling)){
         pop <- input$totalPop 
      }
      
      # compute genotype frequencies
      gt_freqs <- calc_genotype_freq(computed_af, pop = pop)
      gt_freq_all(gt_freqs$gt_complete)
      gt_freq_pop(gt_freqs$gt_by_pop)
      
      # calculate all forensic metrics
      res <- calc_iisnps_params(gt_freq_all(), profile = profile_df, theta = theta)
      
      
      # save results
      if (!is.null(profile_df)){
         results_rv$rmp_value <- res$RMP_profile
         results_rv$overall_metrics <- res$marker_metrics
         results_rv$pop_metrices <- NULL
      } else {
         results_rv$overall_metrics <- res$overall
         results_rv$pop_metrics <- res$by_population
         results_rv$rmp_value <- NULL
      }
      
      shinyjs::enable("calcIISNPs")
   })
   
   observe({
      req(results_rv$pop_metrics, results_rv$overall_metrics)
      
      updateSelectInput(
         session,
         "selected_pop",
         choices = c(
            "Overall",
            names(results_rv$pop_metrics))
      )
   })
   
   observe({
      req(gt_freq_pop())
      
      updateSelectInput(
         session,
         "selected_pop_gt",
         choices = c(
            "Overall",
            names(gt_freq_pop()))
      )
   })
   
   # render marker metrics table
   output$genotypeFreqs_UI <- DT::renderDataTable({
      req(gt_freq_pop(), gt_freq_all())
      
      if (input$selected_pop == "Overall"){
         req(gt_freq_all())
         DT::datatable(gt_freq_all(), rownames = FALSE)
      } else {
         req(gt_freq_pop())
         DT::datatable(gt_freq_pop()[[input$selected_pop_gt]],
                       rownames = FALSE)
      }
   })
   
   output$popTable <- DT::renderDataTable({
      req(input$selected_pop)
      
      if (input$selected_pop == "Overall"){
         req(results_rv$overall_metrics)
         DT::datatable(results_rv$overall_metrics, rownames = FALSE)
      } else {
         req(results_rv$pop_metrics)
         DT::datatable(results_rv$pop_metrics[[input$selected_pop]],
                       rownames = FALSE)
      }
   })
   
   # download handlers
   output$downloadMetrics <- downloadHandler(
      filename = function() { paste0("forensic_metrics_", Sys.Date(), ".xlsx") },
      content = function(file) {
         req(results_rv$overall_metrics, results_rv$pop_metrics, gt_freq_pop(), gt_freq_all())
         sheets <- list()
         sheets[["Forensic Params (FP)"]] <- results_rv$overall_metrics
         pop_sheets <- results_rv$pop_metrics
         names(pop_sheets) <- paste0("FP_", substr(gsub("[^A-Za-z0-9]", "_",
                                                        names(pop_sheets)), 1, 25))
         sheets <- c(sheets, pop_sheets)
         
         sheets[["Genotype Frequency (GF)"]] <- gt_freq_all()
         gt_pop <- gt_freq_pop()
         names(gt_pop) <- paste0("FP_", substr(gsub("[^A-Za-z0-9]", "_",
                                                        names(gt_pop)), 1, 25))
         sheets <- c(sheets, gt_pop)
         writexl::write_xlsx(sheets, path = file)
      }
   )
   
   output$downloadRMP <- downloadHandler(
      filename = function() { paste0("RMP_profile_", Sys.Date(), ".csv") },
      content = function(file) {
         req(results_rv$rmp_value)
         write.csv(data.frame(RMP = results_rv$rmp_value), file, row.names = FALSE)
      }
   )
   
   # render UI for downloads
   output$downloadMetrics_UI <- renderUI({
      req(results_rv$overall_metrics)
      downloadButton("downloadMetrics", "Download Forensic Parameters")
   })
   
   output$downloadRMP_UI <- renderUI({
      req(results_rv$rmp_value)
      downloadButton("downloadRMP", "Download RMP")
   })
   
   
   #======================= PCA ===========================#
   PCAResults <- reactiveVal(NULL)
   LabelColors <- reactiveVal(NULL)
   output$examplePCA <- renderTable({
      data.frame(
         Sample = c("Sample1", "Sample2", "Sample3", "Sample4", "..."),
         Population = c("POP1", "POP2", "POP3", "POP4", "..."),
         rs101 = c("A/A", "A/T", "A/A", "T/T", "..."),
         rs102 = c("G/G", "C/C", "G/C", "G/G", "..."),
         rs_n = c("...", "...", "...", "...", "...")
      )
   })
   
   observe({
      hasDataFile <- !is.null(input$pcaFile)
      hasLabelsFile <- !is.null(input$pcaLabels)
      hasColorFile <- !is.null(input$colorPalette)
      hasShapesFile <- !is.null(input$shapeList)
      
      usingDefaults <- input$useDefaultColors
      
      readyForPCA <- hasDataFile && 
         (usingDefaults || (hasLabelsFile && hasColorFile && hasShapesFile))
      
      toggleState("runPCA", readyForPCA)
   })
   
   observeEvent(input$runPCA, {
      disable("runPCA")
      req(input$pcaFile)
      
      
      withProgress(message = "Running PCA...", {
         tryCatch({
            incProgress(0.2, detail = "Loading input file...")
            df <- load_csv_xlsx_files(input$pcaFile$datapath)
            cleaned <- clean_input_data(df)
            fsnps_gen <- convert_to_genind(cleaned)
            
            incProgress(0.4, detail = "Preparing color and label sets...")
            
            # Read custom labels/colors if needed
            labels <- NULL
            colors <- NULL
            shapes <- NULL
            
            if (!input$useDefaultColors) {
               req(input$pcaLabels, input$colorPalette)
               
               labels <- readLines(input$pcaLabels$datapath)
               colors <- readLines(input$colorPalette$datapath)
               shapes <- readLines(input$shapeList$datapath)
               
               # Optional: trim whitespace, ensure length match
               labels <- trimws(labels)
               colors <- trimws(colors)
               shapes <- trimws(shapes)
               
               if (length(labels) != length(colors) || length(labels) != length(shapes)) {
                  stop("Mismatch between number of labels and colors/shapes.")
               }
            }
            
            labels_colors <- get_labels(
               fsnps_gen = fsnps_gen,
               use_default = input$useDefaultColors,
               input_labels = labels,
               input_colors = colors,
               input_shapes = shapes
            )
            LabelColors(labels_colors)
            
            incProgress(0.6, detail = "Computing PCA...")
            pca_results1 <- compute_pca(fsnps_gen)
            PCAResults(pca_results1)

            output$barPlot <- renderPlot({
               req(PCAResults())
               
               barplot(PCAResults()$percent, 
                           ylab = "Genetic variance explained by eigenvectors (%)", ylim = c(0,25),
                           names.arg = round(PCAResults()$percent, 1))
            })
            
            incProgress(0.8, detail = "Rendering PCA plot...")
            
            output$pcaPlot <- renderPlot({
               req(PCAResults(), LabelColors())
               
               p <- plot_pca(
                  ind_coords = PCAResults()$ind_coords,
                  centroid = PCAResults()$centroid,
                  percent = PCAResults()$percent,
                  labels_colors = LabelColors(),
                  pc_x = input$pcX,
                  pc_y = input$pcY
               )
               print(p)
            })
            
            enable("runPCA")
         }, error = function(e) {
            showNotification(paste("PCA Error:", e$message), type = "error")
            enable("runPCA")
         })
      })
      
   })
   
   output$downloadbarPlot <- downloadHandler(
      filename = function() {
         paste0("bar_plot_", Sys.Date(), ".png")
      },
      content = function(file) {
         png(file, width = 800, height = 800, res = 300)
         graphics::barplot(
            PCAResults()$percent,
            ylab = "Genetic variance explained by eigenvectors (%)",
            ylim = c(0, 25),
            names.arg = round(PCAResults()$percent, 1)
         )
         dev.off()
      },
      contentType = "image/png"
   )
   
   output$downloadPCAPlot <- downloadHandler(
      filename = function() {
         paste0("pca_plot_", Sys.Date(), ".png")
      },
      content = function(file) {
         plot <- plot_pca(
            ind_coords = PCAResults()$ind_coords,
            centroid = PCAResults()$centroid,
            percent = PCAResults()$percent,
            labels_colors = LabelColors(),
            pc_x = input$pcX,
            pc_y = input$pcY
         )
         
         # Save directly to the requested file path
         ggsave(filename = file, plot = plot, width = 8, height = 8, dpi = 600)
      },
      contentType = "image/png"
   )
   
   #================= STRUCTURE ANALYSIS ==================#
   examplePop_STR2 <-data.frame(
      Sample = c("Sample1", "Sample2", "Sample3", "Sample4", "..."),
      Population = c("POP1", "POP2", "POP3", "POP4", "..."),
      rs101 = c("A/A", "A/T", "A/A", "T/T", "..."),
      rs102 = c("G/G", "C/C", "G/C", "G/G", "..."),
      rs_n = c("...", "...", "...", "...", "...")
   )
   
   
   output$examplePop_STR2UI <- DT::renderDataTable({
      req(examplePop_STR2)
      examplePop_STR2
   }, options = list(
      scrollX = TRUE,
      pageLength = 5
   )
   )
   
   observe({
      file_ready <- !is.null(input$structureFile)
      shinyjs::toggleState("runStructure", condition = file_ready)
   })
   
   analysis_done <- reactiveVal(FALSE)
   structure_result <- reactiveVal(NULL)
   qmatrices_result <- reactiveVal(NULL)
   structure_plot_paths <- reactiveVal(NULL)
   output_dir <- tempdir()
   out_path <- file.path(output_dir, "structure_input.str")
   
   observeEvent(input$runStructure, {
      disable("runStructure")
      req(input$structureFile)
      Sys.sleep(1.5)
      
      withProgress(message = "Running STRUCTURE analysis...", {
         incProgress(0.2, detail = "Loading input file...")
         df <- load_csv_xlsx_files(input$structureFile$datapath)
         fsnps_gen <- clean_input_data_str(df)
         
         incProgress(0.4, detail = "Converting to STRUCTURE file...")
         
         structure_df <- to_structure(fsnps_gen$fsnps_gen, include_pop = TRUE)
         structure_df[] <- lapply(structure_df, function(col) as.numeric(as.character(col)))
         
         write.table(structure_df, file = out_path, quote = FALSE, sep = " ",
                     row.names = FALSE, col.names = FALSE)
         
         incProgress(0.6, detail = "Running STRUCTURE analysis...")
         result <- running_structure(out_path,
                                     k.range = input$kMin:input$kMax,
                                     num.k.rep = input$numKRep,
                                     burnin = input$burnin,
                                     numreps = input$numreps,
                                     noadmix = input$noadmix,
                                     phased = input$phased,
                                     ploidy = input$ploidy,
                                     linkage = input$linkage,
                                     structure_path = "structure/structure.exe",
                                     output_dir = output_dir)
         
         structure_result(list(
            output_dir = output_dir,
            plot_paths = result$plot.paths
         ))
         
         incProgress(0.8, detail = "Extracting q matrices...")
         qmatrices_result(q_matrices(result$plot.paths))
         
         incProgress(1.0, detail = "Plotting...")
         populations_df <- fsnps_gen$pop_labels  
         str_files <- list.files(output_dir, pattern = "_f$", full.names = TRUE)
         str_data <- lapply(str_files, starmie::loadStructure)
         
         plot_paths <- lapply(str_data, function(structure_obj){
            file_name <- file.path(output_dir, paste0(structure_obj$K, "_plot.png"))
            gg <- plotQ(structure_obj, populations_df, outfile = file_name)
            ggplot2::ggsave(file_name, plot = gg, width = 12, height = 10, dpi = 600)
            file_name
         })
         
         structure_plot_paths(plot_paths)
         enable("runStructure")
         
      }) # end of with progress
      
      analysis_done(TRUE)
      
   }) # end of observe event for structure
   
   output$downloadLogs <- downloadHandler(
      filename = function() {
         paste0("structure_logs_", Sys.Date(), ".zip")
      },
      content = function(file) {
         log_files <- list.files(output_dir, pattern = "_log.*$", full.names = TRUE)
         if (length(log_files) == 0) return(NULL)
         
         log_files_renamed <- sapply(log_files, function(f) {
            ext <- tools::file_ext(f)
            if (ext == "") {
               new_f <- paste0(f, ".txt")
               file.rename(f, new_f)
               return(new_f)
            }
            return(f)
         })
         zip::zipr(zipfile = file, files = log_files_renamed, recurse = FALSE)
      },
      contentType = "application/zip"
   )
   
   output$downloadFOutputs <- downloadHandler(
      filename = function() {
         paste0("structure_outputs_", Sys.Date(), ".zip")
      },
      content = function(file) {
         f_files <- list.files(output_dir, full.names = TRUE)
         f_files <- f_files[grepl("_f", basename(f_files))]        
         if (length(f_files) == 0) return(NULL)
         zip::zipr(zipfile = file, files = f_files)
      },
      contentType = "application/zip"
   )
   
   output$downloadQMatrixTxtZip <- downloadHandler(
      filename = function() {
         paste0("q_matrices_", Sys.Date(), ".zip")
      },
      content = function(file) {
         req(qmatrices_result())
         q_files <- tempfile()
         dir.create(q_files)
         lapply(names(qmatrices_result()), function(name) {
            mat <- qmatrices_result()[[name]]
            if (!is.null(mat)) {
               write.table(mat, file.path(q_files, paste0(name, ".txt")),
                           row.names = FALSE, col.names = FALSE, quote = FALSE, sep = "\t")
            }
         })
         zip::zipr(zipfile = file, files = list.files(q_files, full.names = TRUE))
      },
      contentType = "application/zip"
   )
   
   output$downloadStructurePlots <- downloadHandler(
      filename = function() {
         paste0("structure_plots_", Sys.Date(), ".zip")
      },
      content = function(file) {
         req(structure_plot_paths())
         plot_files <- list.files(output_dir, pattern = "\\.png$", full.names = TRUE)
         zip::zipr(zipfile = file, files = plot_files)
      },
      contentType = "application/zip"
   )
   
   output$structurePlotPreview <- renderImage({
      req(structure_plot_paths())
      list(
         src = structure_plot_paths()[[1]],
         contentType = "image/png",
         alt = "STRUCTURE Plot Preview",
         width = "100%"
      )
   }, deleteFile = FALSE)
   
   output$downloadButtons <- renderUI({
      req(analysis_done())
      tagList(
         downloadButton("downloadLogs", "Download Log Files (.zip)"),
         downloadButton("downloadFOutputs", "Download STRUCTURE _f Files (.zip)"),
         downloadButton("downloadQMatrixTxtZip", "Download Q Matrices (.zip)"),
         downloadButton("downloadStructurePlots", "Download STRUCTURE Plots (.zip)")
      )
   })
   
   #================== CLASSIFICATION =====================#
   classificationRef <-data.frame(
      Sample = c("Sample1", "Sample2", "Sample3", "Sample4", "..."),
      Population = c("POP1", "POP2", "POP3", "POP4", "..."),
      rs101 = c("A/A", "A/T", "A/A", "T/T", "..."),
      rs102 = c("G/G", "C/C", "G/C", "G/G", "..."),
      rs_n = c("...", "...", "...", "...", "...")
   )
   
   
   output$classificationRef_UI <- DT::renderDataTable({
      req(classificationRef)
      classificationRef
   }, options = list(
      scrollX = TRUE,
      pageLength = 5
   )
   )
   
   observe({
      file_ready <- !is.null(input$forPredFile)
      shinyjs::toggleState("runNaiveBayes", condition = file_ready)
   })
   
   predResults <- reactiveVal(NULL)
   
   observeEvent(input$runNaiveBayes, {
      disable("runNaiveBayes")
      req(input$forPredFile)
      
      
      Sys.sleep(1.5)
      
      result <- calculate_naive_bayes(input$forPredFile$datapath)
      predResults(result)
      
      
   }) # end of observe event
   
   output$predictionTableResult <- renderPrint({
      req(predResults())
      predResults()$predTable
   })
   
   output$statbyClassResult <- renderPrint({
      req(predResults())
      predResults()$otherStat
   })
   
   output$overallStatResult <- renderPrint({
      req(predResults())
      predResults()$predStat
   })
   
   output$downloadClassification <- downloadHandler(
      filename = function(){
         timestamp <- format(Sys.time(), "%Y%m%d_%H%M")
         paste0("classification-results_", timestamp, ".xlsx")
      },
      content = function(file){
         dataset = list(
            "Table" = as.data.frame(predResults()$predTable),
            "Stats per Class" = as.data.frame(predResults()$otherStat),
            "Overall Stats" = as.data.frame(predResults()$predStat)
         )
         
         openxlsx::write.xlsx(dataset, file = file)
      }
   )
   
   output$downloadClassification_UI <- renderUI({
      req(predResults())
      downloadButton("downloadClassification", "Download Results")
   })
}

shinyApp(ui, server)