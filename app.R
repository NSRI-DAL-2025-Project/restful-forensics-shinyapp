library(bslib)
library(dplyr)
library(shiny)
library(shinycssloaders)
library(shinyjs)
library(shinydashboard)

source("functions.R", local = TRUE)
source("dal_functions.R", local = TRUE)
source("global.R", local = TRUE)

options(shiny.maxRequestSize = 2000*1024^2)

ui <- dashboardPage(
   dashboardHeader(
      title = div(
         #tags$img(src = "logo.png", height = "10px", style = "display: inline-block; vertical-align: middle;"),
         tags$span("restFUL Forensics",
                   style = "font-family: Carme, sans-serif; font-size: 26px; color: #ffffff; vertical-align: middle; padding-left: 0px;")
      ),
      titleWidth = 300
   ),
   dashboardSidebar(
      width = 300,

      sidebarMenu(
         menuItem("Homepage", tabName = "dashboard", icon = icon("dashboard")),
         menuItem("Data Pre-processing", tabName = "DataPreProcess", icon = icon("gears"), startExpanded = TRUE,
                  menuSubItem("🔄 File Conversion", tabName = "FileConv"),
                  menuSubItem("🧬 SNP Extraction", tabName = "markerExtract"),
                  menuSubItem("🔽 Filtering", tabName = "FilterTab")
         ), # End of menu item for data pre-processing
         menuItem("Data Processing", tabName = "DataProcess", icon = icon("diagram-project"), startExpanded = TRUE,
                  menuSubItem("📑 DNA Barcoding", tabName = "DNABarcoding"),
                  menuSubItem("📝 Forensic Summary Statistics", tabName = "PopStatistics"),
                  menuSubItem("🔍 Exploratory Analysis", tabName = "PCAtab"),
                  menuSubItem("📊 Population Structure Analysis", tabName = "PopStructure"),
                  menuSubItem("🪪 Forensic Ancestry Inference", tabName = "Classification")
         ), # End of menu item for data processing
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
      tabItems(
         tabItem(tabName = "dashboard",
                 fluidRow(
                    tabBox(
                       width = 12,
                       tags$head(tags$style("img {max-width: 100%; height: auto; }")),
                       div(tags$img(
                          src = "readme/fulllogo.png",
                          width = "600px"
                       ),
                       style = "text-align: center;"
                       )
                    )
                 ), # end of fluid row
                 fluidRow(
                    tabBox(
                       title = "Introduction",
                       width = 12,
                       h4("restFUL forensics is a toolkit dedicated for the forensic analysis of single nucleotide polymorphisms (SNPs) and DNA barcodes. It compiles population datasets extracted from publicly available databases for direct analysis of forensic marker panels.")
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
                    # First tab: Convert Files
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
                                      fileInput("VCFFile", "Upload VCF File"),
                                      radioButtons("inputType2_vcf", "Choose final file type",
                                                   choices = c("PLINK files (.bed/.bim/.fam)" = "plink2",
                                                               "CSV file" = "csv2",
                                                               "FASTA file" = "fasta")),
                                      
                                      conditionalPanel(
                                         condition = "input.inputType2_vcf == 'fasta'",
                                         fileInput("FASTARef", "Reference sequence in FASTA format.")
                                      ),
                                      
                                      conditionalPanel(
                                         condition = "input.inputType2_vcf == 'csv2'",
                                         radioButtons("poptype_vcf", "Do samples come from a single population?",
                                                      choices = c("Yes" = "single", "No" = "multiplepop")),
                                         conditionalPanel(
                                            condition = "input.poptype_vcf == 'multiplepop'",
                                            fileInput("multiplepop_vcf", "Reference file with sample ID and population"),
                                            helpText("*Accepts XLSX and CSV files")
                                         ),
                                         conditionalPanel(
                                            condition = "input.poptype_vcf == 'single'",
                                            textAreaInput("typePop_vcf", "Enter population", rows = 1)
                                         ),
                                         
                                         # --- Breakdowns
                                         radioButtons("breakdown_vcf", "Calculate population breakdown?",
                                                      choices = c("Yes" = "yesbreakdown_vcf", "No" = "nobreakdown_vcf")),
                                         conditionalPanel(
                                            condition = "input.breakdown_vcf == 'yesbreakdown_vcf'",
                                            helpText("Specify column name to serve as a basis for the summary count."),
                                            textAreaInput("breakdown_column_vcf", "Enter column name", rows = 1)
                                            
                                         )
                                         
                                      ) # end of conditional for csv2
                                   ),
                                   
                                   conditionalPanel(
                                      condition = "input.inputType1 == 'bcf1'",
                                      fileInput("BCFFile", "Upload BCF File"),
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
                                            fileInput("multiplepop_bcf", "Reference file with sample ID and population"),
                                            helpText("*Accepts XLSX and CSV files")
                                         ),
                                         conditionalPanel(
                                            condition = "input.poptype_bcf == 'single'",
                                            textAreaInput("typePop_bcf", "Enter population", rows = 1)
                                         ),
                                         
                                         # --- Breakdowns
                                         radioButtons("breakdown_bcf", "Calculate population breakdown?",
                                                      choices = c("Yes" = "yesbreakdown_bcf", "No" = "nobreakdown_bcf")),
                                         conditionalPanel(
                                            condition = "input.breakdown_bcf == 'yesbreakdown_bcf'",
                                            helpText("Specify column name to serve as a basis for the summary count."),
                                            textAreaInput("breakdown_column_bcf", "Enter column name", rows = 1)
                                            
                                         )
                                      )
                                   ),
                                   
                                   conditionalPanel(
                                      condition = "input.inputType1 == 'plink1'",
                                      fileInput("bedFile", "Upload BED File"),
                                      fileInput("bimFile", "Upload BIM File"),
                                      fileInput("famFile", "Upload FAM File"),
                                      radioButtons("inputType2_plink", "Choose final file type",
                                                   choices = c("VCF file" = "vcf2",
                                                               "CSV file" = "csv2")),
                                      
                                      conditionalPanel(
                                         condition = "input.inputType2_plink == 'csv2'",
                                         radioButtons("poptype_plink", "Do samples come from a single population?",
                                                      choices = c("Yes" = "single", "No" = "multiplepop")),
                                         conditionalPanel(
                                            condition = "input.poptype_plink == 'multiplepop'",
                                            fileInput("multiplepop_plink", "Reference file with sample ID and population"),
                                            helpText("*Accepts XLSX and CSV files")
                                         ),
                                         conditionalPanel(
                                            condition = "input.poptype_plink == 'single'",
                                            textAreaInput("typePop_plink", "Enter population", rows = 1)
                                         ),
                                         
                                         # --- Breakdowns
                                         radioButtons("breakdown_plink", "Calculate population breakdown?",
                                                      choices = c("Yes" = "yesbreakdown_plink", "No" = "nobreakdown_plink")),
                                         conditionalPanel(
                                            condition = "input.breakdown_plink == 'yesbreakdown_plink'",
                                            helpText("Specify column name to serve as a basis for the summary count."),
                                            textAreaInput("breakdown_column_plink", "Enter column name", rows = 1)
                                            
                                         )
                                      )
                                   ),
                                   
                                   conditionalPanel(
                                      condition = "input.inputType1 == 'csv1'",
                                      p("This feature automatically converts a CSV file to VCF"),
                                      fileInput("CSVFile", "Upload CSV File"),
                                      fileInput("lociMetaFile", "Upload loci/marker information")
                                   ),
                                   
                                   actionButton("ConvertFILES", "Convert files", icon = icon("file-csv"))
                                ),
                                
                                tabBox(
                                   tabPanel("Instructions", 
                                            h4("This tab intercqonverts common genetic files and to CSV with population information."),
                                            p(strong("Input file/s:")),
                                            p("Required: VCF, BCF, or PLINK (.bed, .bim, .fam) files. It also accepts zipped files as long as it contains the same file type."),
                                            p("Optional input files:"),
                                            tags$ul(
                                               tags$li("(to CSV) XLSX/CSV/TXT file containing metadata. All columns will be merged to the genotype data. Remove unnecessary columns before running 'Convert'."),
                                               tags$li("(to CSV) Single-line text indicating the 'Column name' to be used as basis for the calculation of population breakdown."),
                                               tags$li("(VCF to FASTA) Reference sequence in FASTA format."),
                                               tags$li("(CSV to VCF) Marker information with the following columns: [1] SNP, [2] CHR, [3] POS, [4] Genetic distance, [5] REF Allele [6] ALT Allele")
                                            ),
                                            p(strong("Expected output file/s:"), "VCF, PLINK, or CSV file."),
                                            br(),
                                   ),
                                   tabPanel("Sample Input Format/s", 
                                            h4("To convert to a CSV file with population metadata:"),
                                            h4("This is a sample reference file. Only the first two columns are used."),
                                            DT::dataTableOutput("ExampleRefFile"),
                                            #tableOutput("exampleRefCSV"),
                                            br(),
                                            h4("For CSV to VCF conversion, a separate file on marker information is needed."),
                                            h4("See the following formats:"),
                                            h4("Required CSV format:"),
                                            DT::dataTableOutput("ExampleCSVFormat"),
                                            #tableOutput("exampleCSVFile"),
                                            h4("Required marker info format:"),
                                            DT::dataTableOutput("markerInfoFormat")
                                            #tableOutput("exampleMarkerInfo")
                                              
                                   ),
                                   tabPanel("Download Sample Files", 
                                            tags$a("A. Sample VCF", href="www/sample_hgdp.vcf"),
                                            br(),
                                            tags$a("B. Sample CSV file (for VCF conversion)", href = "www/sample.csv", download = NA),
                                            br(),
                                            tags$a("C. Sample marker metadata file (for CSV-VCF conversion)", href = "www/marker_info.csv", download = NA)
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
                                         DT::dataTableOutput("previewTable") %>% shinycssloaders::withSpinner(color = "blue")
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
                                         DT::dataTableOutput("previewTableBreakdown") %>% shinycssloaders::withSpinner(color = "blue")
                                      )
                                   )
                                )   
                             )
                    ),
                    
                    tabPanel("ForenSeq Conversion",
                             fluidRow(
                                tabBox(
                                   fileInput("uas_zip", "Upload ZIP or TAR file",
                                             accept = c(".zip", ".tar")),
                                   helpText("*Accepts compressed files containing XLSX files."),
                                   fileInput("ref_file", "Optional Reference File (CSV or XLSX)",
                                             accept = c(".csv", ".xlsx")),
                                   actionButton("run_uas2csv", "Run Conversion")
                                ),
                                tabBox(
                                   #title = "Instructions",
                                   tabPanel("Instructions", 
                                            h4("This tab converts zipped ForenSeq UAS outputs to a single file in a wide format."),
                                            p("This section builds upon the work of Ms. Maeviviene Sosing as part of the Filipino Genomes Research Program 2"),
                                            p(strong("Input file/s:"), "Compressed folder (.zip or .tar) of XLSX files."),
                                            p(strong("Expected output file/s:"), "Single CSV file (merged XLSX files).")
                                   ),
                                   tabPanel("Sample Input Format/s", 
                                            h4("Sample input file. All alleles of available SNPs per sample are listed in a long format."),
                                            tableOutput("exampleXLSX")
                                   ),
                                   tabPanel("Download Sample Zipped File", 
                                            tags$h4("Downloadable Sample"),
                                            tags$ul(
                                               tags$a("Sample zipped file", href = "www/sample_forenseq.zip", download = NA))
                                   )
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
                                         DT::dataTableOutput("previewTableUAS") %>% shinycssloaders::withSpinner(color = "blue")
                                      ),
                                      br(),
                                      uiOutput("downloadUAScsv_UI")
                                   )
                                )
                             )
                    ), # end of forenseq conversion
                    
                    tabPanel("Convert to SNIPPER-analysis ready file",
                             fluidRow(
                                tabBox(
                                   width = 6,
                                   fileInput("convertFile", "Upload File"),
                                   helpText("*Accepts VCF, XLSX, and CSV files"),
                                   fileInput("refFile", "Upload Reference File"),
                                   helpText("*Accepts XLSX and CSV files"),
                                   checkboxInput("targetPop", "Subset Target Population?", value = FALSE),
                                   textInput("targetPopName", "Target Population Name"),
                                   actionButton("convertBtn", "Convert Format", icon = icon("arrow-up-right-from-square"))
                                ), # end of input tabbox
                                tabBox(
                                   tabPanel("Instructions",
                                            h4("This converts CSV or XLSX files to a SNIPPER-compatible file"),
                                            p(strong("Input file/s:"), "CSV or XLSX file."),
                                            p(strong("Parameter/s:"), "(optional) Target population name for classification"),
                                            p(strong("Expected output file/s:"), "XLSX file."),
                                            br(),
                                            p("SNIPPER tool for sample classification: ",
                                              tags$a("SNIPPER tool",
                                                     href="https://mathgene.usc.es/snipper/index.php",
                                                     target="_blank"))
                                   ),
                                   tabPanel("Sample Input Format/s",
                                            h4("Sample Input File"),
                                            tableOutput("exampleTableSnipper"),
                                            h4("Sample reference file"),
                                            tableOutput("exampleRefSnipper")
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
                                         DT::dataTableOutput("previewTableSNIPPER") %>% shinycssloaders::withSpinner(color = "blue")
                                      ),
                                      br(),
                                      uiOutput("downloadSNIPPER")
                                   )
                                )
                             )
                    ), # end of tabpanel for snipper
                    
                    tabPanel("CSV to STRUCTURE file",
                             fluidRow(
                                tabBox(
                                   width = 6,
                                   fileInput("tostrFile", "Upload CSV file"),
                                   helpText("Use the 'Convert files to CSV' file if using VCF, BCF, or PLINK files. Population data is necessary."),
                                   radioButtons("systemFile", "Choose the operating system where STRUCTURE v2.3.4 is installed",
                                                choices = c("Linux" = "Linux", "Windows" = "Windows")),
                                   actionButton("csv2str", "Generate STRUCTURE File", icon = icon("arrow-up-right-from-square"))
                                ),
                                tabBox(
                                   tabPanel("Instructions",
                                            h4("Convert CSV files to STRUCTURE files."),
                                            p(strong("Input file/s:"), "CSV file with marker and population data."),
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
                    tabPanel("SNP Extraction",
                             fluidRow(
                                box(
                                   width = 5,
                                   radioButtons("inputFileType", "A. Input file format",
                                                choices = c("VCF/VCF.GZ/BCF", "PLINK"), inline = TRUE),
                                   
                                   conditionalPanel(
                                      condition = "input.inputFileType == 'VCF/VCF.GZ/BCF'",
                                      fileInput("markerFile", "Upload Genotype (VCF, VCF.GZ, or BCF File)")
                                   ),
                                   
                                   conditionalPanel(
                                      condition = "input.inputFileType == 'PLINK'",
                                      fileInput("bedFile", "PLINK BED file"),
                                      fileInput("bimFile", "PLINK BIM file"),
                                      fileInput("famFile", "PLINK FAM file"),
                                      helpText("If multiple PLINK files will be used, upload as a zipped file."),
                                      fileInput("zippedPLINK", "Zipped PLINK Files")
                                   ),
                                   
                                   radioButtons("markerType", "B. Choose Marker Type",
                                                choices = c("rsid", "pos"), inline = TRUE),
                                   
                                   conditionalPanel(
                                      condition = "input.markerType == 'rsid'",
                                      radioButtons("rsidInputType", "RSID Input",
                                                   choices = c("manual", "upload")),
                                      
                                      conditionalPanel(
                                         condition = "input.rsidInputType == 'manual'",
                                         textAreaInput("typedRSIDs", "Enter RSIDs (one per line)", rows = 5)
                                      ),
                                      conditionalPanel(
                                         condition = "input.rsidInputType == 'upload'",
                                         fileInput("markerList1", "Upload RSID List File")
                                      )
                                   ),
                                   
                                   conditionalPanel(
                                      condition = "input.markerType == 'pos'",
                                      fileInput("markerList2", "Upload POS List (.csv, .xlsx)"),
                                      checkboxInput("addRSID", "C. Add marker information/rsID to VCF (output) file?", value = FALSE),
                                      helpText("Some files have no rsID information. Check the box to add rsID to output file")
                                   ),
                                   
                                   actionButton("extractBtn", "Run Marker Extraction", icon = icon("play"))
                                   
                                ),
                                tabBox(
                                   tabPanel("Instructions",
                                            h4("Extract SNPs based on rsID (marker identification) or GRCh37/GRCh38 position"),
                                            p(strong("Input file/s:")),
                                            p("(1) VCF, BCF, or PLINK (.bed, .bim, .fam) files."),
                                            p("(2) Markers/position list — you may type rsIDs manually, upload a list, or use a POS txt file."),
                                            p("The position list (txt file) should include:"),
                                            tags$ul(
                                               tags$li("[1] rsID/marker name"),
                                               tags$li("[2] Chromosome number (integer)"),
                                               tags$li("[3] Position in GRCh37 or GRCh38 (integer)")
                                            ),
                                            p(strong("Parameter/s:"), "Filtering options"),
                                            p(strong("Expected output file/s:"), "VCF file")
                                   ), # end of tabpanel
                                   tabPanel("Sample Input Format/s",
                                            h4("rsID Format"),
                                            tableOutput("exampleRSID"),
                                            h4("Position Format"),
                                            tableOutput("examplePOS")
                                   ),
                                   tabPanel("Download Sample Files",
                                            h4("Sample File/s:"),
                                            tags$ul(
                                               tags$a("A. Sample VCF file", href = "www/sample_hgdp.vcf", download = NA),
                                               br(),
                                               tags$a("B. Sample marker metadata file (to generate rsID names)", href = "www/marker_info.csv", download = NA)
                                            )
                                   ) # end of tabpanel download sample input
                                ) # end of tabbox
                             ), # end of fluid row
                             fluidRow(
                                tabBox(
                                   title = "Extraction Results",
                                   uiOutput("downloadExtracted_UI"),
                                   helpText("Note: Some systems mislabel .vcf files as contact files. This is a reminder that the file is a genomic VCF.")
                                )
                             )
                    ), # end of tab panel for extraction
                    
                    # SNP Extraction: Concordance Analysis
                    tabPanel("Concordance Analysis",
                             fluidRow(
                                tabBox(
                                   fileInput("concordanceFile1", "Upload File A"),
                                   fileInput("concordanceFile2", "Upload File B"),
                                   checkboxInput("isHaplotype", "Treat data as haplotypes", value = FALSE),
                                   actionButton("compareBtn", "Run Concordance Analysis", icon = icon("play"))
                                ), # end of first tabbox
                                tabBox(
                                   tabPanel("Instructions",
                                            h4("Concordance analysis between files with the same samples"),
                                            p(strong("Input file/s:"), "Two CSV or XLSX files."),
                                            p(strong("Parameter/s:"), "Indicate if using haplotypes"),
                                            p(strong("Expected output/s:")),
                                            tags$ul(
                                               tags$li("Concordance table"),
                                               tags$li("Concordance plot")
                                            )
                                   ), # end of tP instructions
                                   tabPanel("Sample Input Format/s",
                                            h4("File Format (for concordance)"),
                                            tableOutput("exampleTable")
                                   ), # end of tP sample files 
                                   tabPanel("Download Sample Files",
                                            h4("Sample Files"),
                                            tags$ul(
                                               tags$a("Sample CSV file (1)", href = "www/sample1_for.concordance.csv", download = NA),
                                               br(),
                                               tags$a("Sample CSV file (2)", href = "www/sample1_for.concordance.csv", download = NA)
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
                                         DT::dataTableOutput("concordanceResults") %>% shinycssloaders::withSpinner(color = "blue")
                                      ),
                                      br(),
                                      uiOutput("downloadConcordance_UI")
                                   ),
                                   tabPanel("Concordance Plot",
                                      div(
                                         style = "overflow-x: auto;",
                                         plotOutput("concordancePlot", height = "600px") %>% shinycssloaders::withSpinner(color = "blue"),
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
                       fileInput("forFilter", "Upload VCF/BCF/PLINK files"),
                       fileInput("highlightRef", "Optional Reference file for highlighting (CSV/XLSX)"),
                       checkboxInput("enableDP", "Plot Depth of Coverage", value = TRUE),
                       helpText("Depth of Coverage Plot only available if using a VCF file."),
                       selectInput("colorPalette", "Color Palette", choices = rownames(RColorBrewer::brewer.pal.info), selected = "Set2"),
                       hr(),
                       
                       h4("PLINK 1.9 Filtering Options"),
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
                                        numericInput("qualHWE == true", "Hardy-Weinberg equilibrium exact test p-value Threshold (--hwe)", value = 0.000001, min = 0.0000000001)
                       ),
                       checkboxInput("filterLD", "Filter Variants (--indep-pairwise)", value = FALSE),
                       helpText("Prune markers in approximate linkage equilibrium with each other."),
                       conditionalPanel("input.filterLD == true",
                                        numericInput("ldWindow", "Window Size (kb)", value = 500, min = 1, step = 1),
                                        numericInput("ldStep", "Step Size (variants)", value = 50, min = 1, step = 1),
                                        numericInput("ldR2", "r2 Threshold", value = 0.2, min = 0, max = 1, step = 0.01)
                       ),
                       textInput("customFilter", "Additional PLINK flags", placeholder = "--keep filestokeep.txt"),
                       fileInput("extraFile1", "Optional file for first flag", accept = c(".txt", ".ped", ".psam", ".pheno")),
                       fileInput("extraFile2", "Optional file for second flag", accept = c(".txt", ".ped", ".psam", ".pheno")),
                       helpText("Upload extra files only if required by additional PLINK flags."),
                       actionButton("calcDP", "Run Filtering & Plotting", icon = icon("filter")),
                       textOutput("filterWarning")
                    ),
                    tabBox(
                       tabPanel("Instructions",
                                h4("Filter individuals and variants using standard options in PLINK 1.9."),
                                p(strong("Input file/s:"), "VCF file"),
                                p(strong("Parameter/s:")),
                                tags$ul(
                                   tags$li("--mind [value]"),
                                   tags$li("--geno [value]"),
                                   tags$li("--maf [value]"),
                                   tags$li("--qual-threshold [value]"),
                                   tags$li("--hwe [value]"),
                                   tags$li("--indep-pairwise [value]"),
                                   tags$li("Other additional PLINK flags")
                                ),
                                p(strong("Expected output/s:")),
                                tags$ul(
                                   tags$li("VCF file"),
                                   tags$li("Depth of Coverage Plots")
                                ),
                                br(),
                                p("Standard filtering flags are indicated. For other PLINK flags, see the following for options to be specified in the 'Additional PLINK flags' text box: ",
                                  tags$a("PLINK 1.9 Documentation",
                                         href="https://www.cog-genomics.org/plink/",
                                         target="_blank"))
                       ),
                       tabPanel("Download sample files",
                                tags$a("Sample VCF", href="www/sample_hgdp.vcf")
                       )
                    ), # end of tabBox for instructions
                    tabBox(
                       tabPanel("PLINK Commands Preview",
                                verbatimTextOutput("plinkCommandPreview"),
                       ),
                       tabPanel("Depth Plots",
                                imageOutput("depthMarkerPlot") %>% shinycssloaders::withSpinner(color = "blue"),
                                imageOutput("depthSamplePlot") %>% shinycssloaders::withSpinner(color = "blue")
                       ),
                       tabPanel("Download Files",
                                uiOutput("depthMarkerPlot_UI"),
                                uiOutput("depthSamplePlot_UI"),
                                uiOutput("downloadFilteredFile_UI")
                       )
                    )
                 ) # end of fluid row
         ), # end of tabItem for filtering
         
         tabItem(tabName = "DNABarcoding",
                 tabsetPanel(
                    tabPanel("Multiple Sequence Alignment",
                             fluidRow(
                                box(
                                   fileInput("fastaFile", "Upload zipped FASTA files"),
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
                                            h4("Perform multiple sequence alignment using the msa R package."),
                                            p("Post-processing of alignment is performed using the DECIPHER package in R."),
                                            p(strong("Input file/s:"), "Zipped folder of FASTA files."),
                                            p(strong("Parameter/s:"), "Substitution matrix for the alignment (ClustalW, ClustalOmega, MUSCLE)"),
                                            p(strong("Expected output/s:")),
                                            tags$ul(
                                               tags$li("Aligned sequences"),
                                               tags$li("Alignment scores"),
                                               tags$li("Alignment PDF"),
                                               p("Note: Alignment PDF is only available via the Dockerized application.")
                                            ),
                                            br(),
                                            p("The aligned sequences can be used in the tab 'Phylogenetic Tree'")
                                   ),
                                   tabPanel("Download Sample File",
                                            h4("Sample Files"),
                                            tags$ul(
                                               tags$a("Sample zipped FASTA file", href = "www/lactobacillus/lacto2.zip", download = NA)),
                                   )
                                ),
                                tabBox(
                                   width = 12,
                                   tabPanel("Preview of Alignments",
                                            msaR::msaROutput("msaView", width = "100%"),
                                            br(),
                                            verbatimTextOutput("initialAlignmentText") %>% shinycssloaders::withSpinner(color = "blue"),
                                            br(),
                                            verbatimTextOutput("adjustedAlignmentText") %>% shinycssloaders::withSpinner(color = "blue"),
                                            br(),
                                            verbatimTextOutput("staggeredAlignmentText") %>% shinycssloaders::withSpinner(color = "blue"),
                                            br(),
                                            verbatimTextOutput("alignmentScoresPreview") %>% shinycssloaders::withSpinner(color = "blue")
                                   ),
                                   tabPanel("Download Results",
                                            uiOutput("downloadAlignedFASTA_UI"),
                                            uiOutput("downloadAlignmentScores_UI"),
                                            downloadButton("downloadAlignmentPDF", "Download Alignment in PDF"),
                                            p("Note that the PDF is automatically downloaded in the same folder/working directory. This is a built in feature of the msa R package.")
                                   )
                                )
                             ) # end of fluid row
                    ), # end of tabPanel
                    tabPanel("Phylogenetic Tree Analysis",
                             fluidRow(
                                box(
                                   selectInput("treeAlignmentType", "Use alignment for tree construction:",
                                               choices = c(
                                                  "Initial" = "initial",
                                                  "Adjusted" = "adjusted",
                                                  "Staggered" = "staggered"
                                               ),
                                               selected = "initial"
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
                                   h4("Perform phylogenetic tree reconstruction using ape and phangorn R packages."),
                                   p("Approaches to tree construction are NJ, UPGMA, Maximum Parsimony, and Maximum Likelihood. Check the assumptions and constraints of each approach [1]."),
                                   p(strong("Input file"), "used is the alignment output from the MSA tab. There is an option of using the raw, adjusted, or staggered alignment for tree construction."),
                                   p(strong("Parameters"), "vary based on the method."),
                                   p(strong("Expected output"), "is the phylogenetic tree in PNG format.")
                                ),
                                tabBox(
                                   tabPanel("View Results",
                                            h4("Phylogenetic Tree"),
                                            p("This tab uses the multiple sequence alignment performed in the previous tab."),
                                            uiOutput("treeImage") %>% shinycssloaders::withSpinner(color = "blue")
                                   ),
                                   tabPanel("Download Results",
                                            uiOutput("downloadTree_UI"),
                                            uiOutput("downloadAll_UI")  
                                   )
                                )
                             )
                    ), 
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
                                               h4("This tab performs species identification using the R package 'BarcodingR'."),
                                               p(strong("Input file/s:")),
                                               tags$ul(
                                                  tags$li("Aligned reference sequences"),
                                                  tags$li("Aligned query sequences")
                                               ),
                                               p(strong("Parameter/s:")),
                                               tags$ul(
                                                  tags$li("(without kmer method) Training model: bpNewTraining, fuzzyId, bpNewTrainingOnly, bpUsedTrained, or Bayesian"),
                                                  tags$li("(with kmer method) Fuzzy-set Method or BP-based method")
                                               )
                                            ),
                                            tabBox(
                                               verbatimTextOutput("identificationResult") %>% shinycssloaders::withSpinner(color = "blue")
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
                                                        h4("Calculate the optimal kmer values"),
                                                        p(strong("Input file/s:"), "Aligned sequences of the reference dataset (FASTA)"),
                                                        p(strong("Parameter/s:"), "Length of maximum kmer value"),
                                                        p(strong("Expected output file:"), "Kmer plot")
                                               )
                                            )
                                         ),
                                         fluidRow(
                                            tabBox(
                                               title = "Results",
                                               width = 12,
                                               tabPanel("Outputs",
                                                        verbatimTextOutput("kmerResult") %>% shinycssloaders::withSpinner(color = "blue"),
                                                        imageOutput("kmerPlot") %>% shinycssloaders::withSpinner(color = "blue"),
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
                                                        h4("Calculate the barcoding gap"),
                                                        p(strong("Input file:"), "VCF file"),
                                                        p(strong("Parameter/s:"), "Distance (raw, K80, euclidean)"),
                                                        p(strong("Expected output file:"), "Barcoding gap plot")
                                               )
                                            )
                                         ),
                                         fluidRow(
                                            tabBox(
                                               title = "Results",
                                               width = 12,
                                               tabPanel("Outputs",
                                                        verbatimTextOutput("barcodingResult") %>% shinycssloaders::withSpinner(color = "blue"),
                                                        imageOutput("BarcodingGapPlot") %>% shinycssloaders::withSpinner(color = "blue"),
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
                                               h4("Evaluate Barcodes"),
                                               p(strong("Input file/s:"), "CSV or XLSX file."),
                                               p(strong("Parameter/s:"), "Length of kmer for barcode 1 and barcode 2 (separate)")
                                            ),
                                            tabBox(
                                               tableOutput("evalBarcodesResult") %>% shinycssloaders::withSpinner(color = "blue")
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
                                               h4("Species Membership Value (TDR)"),
                                               p(strong("Input file/s:"), "CSV file with marker and population data."),
                                               p(strong("Parameter/s:"), "Boostrap value for query and reference samples.")
                                            ),
                                            tabBox(
                                               verbatimTextOutput("tdrValues") %>% shinycssloaders::withSpinner(color = "blue")
                                            )
                                         )
                                )
                             ) # end of tabsetpanel
                    )
                 ) # end of tabsetPanel
         ), # end of tab item for 
         
         tabItem(tabName = "PopStatistics",
                 fluidRow(
                    box(
                       fileInput("popStatsFile", "Upload CSV or XLSX Dataset"),
                       actionButton("runPopStats", "Analyze", icon = icon("magnifying-glass-chart")),
                       uiOutput("downloadStatsXLSX_UI")
                    ),
                    tabBox(
                       tabPanel("Private Alleles",
                                h4("Private Alleles Summary"),
                                DT::dataTableOutput("privateAlleleTable") %>% shinycssloaders::withSpinner(color = "blue")
                       ),
                       tabPanel("Mean Allelic Richness",
                                h4("Mean Allelic Richness per site"),
                                DT::dataTableOutput("meanallelic") %>% shinycssloaders::withSpinner(color = "blue")
                       ),
                       tabPanel("Heterozygosity",
                                h4("Observed vs Expected Heterozygosity"),
                                DT::dataTableOutput("heterozygosity_table") %>% shinycssloaders::withSpinner(color = "blue"),
                                br(),
                                h4("Heterozygosity Plot"),
                                imageOutput("heterozygosity_plot") %>% shinycssloaders::withSpinner(color = "blue"),
                                uiOutput("downloadHeterozygosityPlot_UI")
                       ),
                       tabPanel("Inbreeding Coefficients",
                                h4("Inbreeding Coefficient by Population"),
                                DT::dataTableOutput("inbreeding_table") %>% shinycssloaders::withSpinner(color = "blue")
                       ),
                       tabPanel("Allele Frequencies",
                                h4("Allele Frequency Table"),
                                DT::dataTableOutput("allele_freq_table") %>% shinycssloaders::withSpinner(color = "blue")
                       ),
                       tabPanel("Hardy-Weinberg Equilibrium",
                                h4("HWE P-value Summary"),
                                uiOutput("hwe_summary") %>% shinycssloaders::withSpinner(color = "blue"),
                                h4("Population-wise HWE Chi-Square Table"),
                                DT::dataTableOutput("hwe_chisq_table") %>% shinycssloaders::withSpinner(color = "blue")
                       ),
                       tabPanel("Fst Values",
                                h4("Pairwise Fst Matrix"),
                                uiOutput("fstMatrixUI") %>% shinycssloaders::withSpinner(color = "blue"),
                                h4("Tidy Pairwise Fst Data"),
                                DT::dataTableOutput("fstDfTable") %>% shinycssloaders::withSpinner(color = "blue"),
                                br(),
                                h4("Fst Heatmap"),
                                imageOutput("fst_heatmap_plot", width = "100%") %>% shinycssloaders::withSpinner(color = "blue")
                       ),
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
                                tableOutput("examplePop")
                       ),
                       tabPanel("Download Sample Files",
                                h4("Sample File"),
                                tags$ul(
                                   tags$a("Sample CSV file", href = "www/sample.csv", download = NA)
                                )
                       )
                    )
                 ) # end of fluid row
         ), # end of tabsetpanel
         
         tabItem(tabName = "PCAtab", 
                 fluidRow(
                    box(
                       fileInput("pcaFile", "Upload SNP Data (in CSV or XLSX) for PCA", accept = c(".csv", ".txt")),
                       checkboxInput("useDefaultColors", "Use Default Colors and Labels", TRUE),
                       conditionalPanel(
                          condition = "!input.useDefaultColors",
                          fileInput("pcaLabels", "Upload PCA Labels (TXT)"),
                          fileInput("colorPalette", "Upload Color Palette (TXT)"),
                          fileInput("shapeList", "Upload desired point shapes (TXT)"),
                          p("The order of the colors would match the order of PCA labels")
                       ),
                       br(),
                       numericInput("pcX", "PC Axis X", value = 1, min = 1),
                       numericInput("pcY", "PC Axis Y", value = 2, min = 1),
                       actionButton("runPCA", "Run PCA Analysis", icon = icon("play"))
                    ),
                    tabBox(
                       tabPanel("Instructions",
                                h4("Run principal component analysis using the ade4 R package."),
                                br(),
                                p(strong("Input file:"), "CSV or XLSX file and color labels (optional)"),
                                p(strong("Expected output file:"), "PNG plots")
                       ),
                       tabPanel("Sample Input Format/s",
                                h4("Example: PCA Input Format"),
                                tableOutput("examplePCA")
                       ),
                       tabPanel("Download sample files",
                                h4("Sample File"),
                                tags$ul(
                                   tags$a("Sample file", href = "www/sample.csv", download = NA)
                                )
                       )
                    ),
                    tabBox(
                       title = "PCA Results",
                       plotOutput("barPlot"),
                       plotOutput("pcaPlot"),
                       downloadButton("downloadbarPlot", "Download Bar Plot"),
                       downloadButton("downloadPCAPlot", "Download PCA Plot")
                    )
                 )
         ),
         
         tabItem(tabName = "PopStructure",
                 fluidRow(
                    box(
                       fileInput("structureFile", "Upload Input File (CSV/XLSX)"),
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
                       actionButton("runStructure", "Run STRUCTURE", icon = icon("play")) 
                    ),
                    tabBox(
                       tabPanel("Instructions",
                                p("Some functions were revised and adapted from the strataG and dartR packages such as 'gl.run.structure', '.structureParseQmat', 'structureRead', and 'utils.structure.evanno'"),
                                h4("Generate STRUCTURE input files and pong compatible files. Visualize the possible results"),
                                p(strong("Input file:"), "CSV or XLSX file"),
                                p(strong("Expected output file:"), "Zipped qmatrices, individual files, and PNG plots"),
                                p("See ",
                                  tags$a("STRUCTURE v2.3.4 Documentation",
                                         href="https://web.stanford.edu/group/pritchardlab/structure_software/release_versions/v2.3.4/structure_doc.pdf",
                                         target="_blank"))
                       ),
                       tabPanel("Download Sample File",
                                h4("Download Sample File"),
                                tags$ul(
                                   tags$a("Sample CSV file", href = "www/sample.csv", download = NA)
                                )     
                       )
                    ),
                    tabBox(
                       title = "STRUCTURE Results",
                       uiOutput("downloadButtons"),
                       h4("STRUCTURE Visualization"),
                       p("NOTE: The plots are expected to take some time to load."),
                       imageOutput("structurePlotPreview") %>% shinycssloaders::withSpinner(color = "blue")
                    )
                 )
                 
         ),
         
         tabItem(tabName = "Classification",
                 fluidRow(
                    box(
                       fileInput("forPredFile", "Upload CSV file"),
                       actionButton("runNaiveBayes", "Classify", icon = icon("align-justify")),
                       uiOutput("downloadClassification_UI")
                    ),
                    tabBox(
                       tabPanel("Instructions",
                                h4("Classify individuals using Naive Bayes from the e1071 and caret R packages."),
                                p(strong("Input file/s:"), "CSV file"),
                                p(strong("Expected output file:"), "XLSX file")
                       ), 
                       tabPanel("Download Sample File",
                                h4("Download Sample File"),
                                tags$ul(
                                   tags$a("Sample CSV file", href = "www/sample.csv", download = NA)
                                )
                       )
                    ),
                    tabBox(
                       tabPanel("Prediction Table",
                                verbatimTextOutput("predictionTableResult") %>% shinycssloaders::withSpinner(color = "blue")
                       ),
                       tabPanel("Statistics by Population",
                                verbatimTextOutput("statbyClassResult") %>% shinycssloaders::withSpinner(color = "blue")
                       ),
                       tabPanel("Overall Statistics",
                                verbatimTextOutput("overallStatResult") %>% shinycssloaders::withSpinner(color = "blue")
                       )
                       
                    )
                 )
         ),
         tabItem(
            tabName = "AppRef",
            tabBox(
               #title = "References",
               width = 12,
               #side = "right",
               tabPanel("References",
                        div(
                          style = "height: 50vh; overflow-y:scroll;",
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
                  width = 6,
                  height = "250px",
                  #h3("Background"),
                  p("The", strong("restFUL forensics"), "toolkit is an output of the project", strong("Development and validation of an automated web-based tool for efficient genomic marker extraction to assist in genomic research.")),
                  p("This is based on the preliminary work on ancestry marker analysis at the DNA Analysis Laboratory.")
               ),
               tabBox(
                  title = tagList(icon("people-line"), "Authors"),
                  width = 6,
                  height = "250px",
                  #h3("Authors"),
                  p("Nelvie Fatima Jane Soliven from the DNA Analysis Laboratory."),
                  p("Leda Celeste Samin from the DNA Analysis Laboratory."),
                  p("Melvin Ambrocio Matias from the Institute of Biology at the University of the Philippines Diliman.")
               )),
            fluidRow(
               tabBox(
                  title = tagList(icon("wallet"), "Funding"),
                  width = 6,
                  height = "250px",
                  #h3("Funding"),
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
         p("Bodenhofer, U., Bonatesta, E., Horejš-Kainrath, C., & Hochreiter, S. (2015). msa: an R package for multiple sequence alignment. Bioinformatics (Oxford, England), 31(24), 3997–3999.",
           tags$a("https://doi.org/10.1093/bioinformatics/btv494",
                  href="https://doi.org/10.1093/bioinformatics/btv494",
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
         p("Dray, S., & Dufour, A.-B. (2007). The ade4 Package: Implementing the Duality Diagram for Ecologists. Journal of Statistical Software, 22(4), 1–20.",
           tags$a("https://doi.org/10.18637/jss.v022.i04",
                  href="https://doi.org/10.18637/jss.v022.i04",
                  target="_blank")),
         p("Gruber, B., Unmack, P. J., Berry, O. F., & Georges, A. (2018). dartr: An r package to facilitate analysis of SNP data generated from reduced representation genome sequencing. Molecular ecology resources, 18(3), 691–699.",
           tags$a("https://doi.org/10.1111/1755-0998.12745",
                  href="https://doi.org/10.1111/1755-0998.12745",
                  target="_blank")),
         p("Archer, F. I., Adams, P. E., & Schneiders, B. B. (2017). stratag: An r package for manipulating, summarizing and analysing population genetic data. Molecular ecology resources, 17(1), 5–11.",
           tags$a("https://doi.org/10.1111/1755-0998.12559",
                  href="https://doi.org/10.1111/1755-0998.12559",
                  target="_blank")),
         p("Dimitriadou, E., Hornik, K., Leisch, F., Meyer, D., & Weingessel, A. (2009). E1071: Misc Functions of the Department of Statistics (E1071), TU Wien.",
           tags$a("https://www.researchgate.net/publication/221678005_E1071_Misc_Functions_of_the_Department_of_Statistics_E1071_TU_Wien",
                  href="https://www.researchgate.net/publication/221678005_E1071_Misc_Functions_of_the_Department_of_Statistics_E1071_TU_Wien",
                  target="_blank")),
         p("Kuhn, M. (2008). Building Predictive Models in R Using the caret Package. Journal of Statistical Software, 28(5), 1–26.",
           tags$a("https://doi.org/10.18637/jss.v028.i05",
                  href="https://doi.org/10.18637/jss.v028.i05",
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
         p(strong("Input file:"), "CSV or XLSX file"),
         p(strong("Expected output files:")),
         tags$ul(
            tags$li("XLSX file with all results"),
            tags$li("Heterozygosity Plot"),
            tags$li("Fst Plots")
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
   
   
   
   # FILE CONVERSION #
   convertedVCF <- reactiveVal(NULL)
   convertedFASTA <- reactiveVal(NULL)
   convertedCSV <- reactiveVal(NULL)
   convertedPLINK <- reactiveVal(NULL)
   convertedBreakdown <- reactiveVal(NULL)
   
   observe({
      toggleState("ConvertFILES", !is.null(input$VCFFile) || !is.null(input$BCFFile) || !is.null(input$CSVFile) || (!is.null(input$bedFile) && !is.null(input$bimFile) && !is.null(input$famFile)))
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
      showPageSpinner()
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
      
      #============= VCF ===============#
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
      }
      
      #============= BCF ===============#
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
      }
      
      #============= PLINK ===============#
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
      }
      
      #============= CSV ===============#
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
      }
      
      enable("ConvertFILES")
      hidePageSpinner()
   })
   
   
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
   
   ### ForenSeq to CSV
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
      showPageSpinner()
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
      hidePageSpinner()
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
   
   
   ### For SNIPPER
   output$exampleTableSnipper <- renderTable({
      data.frame(
         Ind = c("sample1", "sample2", "sample3", "sample4", "..."),
         rs101 = c("A/A", "A/T", "T/T", "A/T", "..."),
         rs102 = c("G/C", "G/C", "G/G", "G/C", "..."),
         rs103 = c("C/C", "C/G", "G/G", "G/G", "..."),
         rs_n = c("...", "...", "...", "...", "...")
      )
   })
   
   output$exampleRefSnipper <- renderTable({
      data.frame(
         Sample.Name = c("sample1", "sample2", "sample3", "sample4", "..."),
         Population = c("Malaysia", "Mexico", "Greece", "South Korea", "..."),
         Superpopulation = c("Southeast Asia", "North and South America", "Europe", "East Asia", "...")
      )
   })
   
   convertedSNIPPER <- reactiveVal(NULL)
   outputName <- "snipper.xlsx"
   
   observe({
      hasFile <- !is.null(input$convertFile) && nrow(input$convertFile) > 0
      hasRef  <- !is.null(input$refFile) && nrow(input$refFile) > 0
      
      hasTarget <- TRUE
      if (isTRUE(input$targetPop)) {
         hasTarget <- !is.null(input$targetPopName) && nzchar(trimws(input$targetPopName))
      }
      
      ready <- isTRUE(hasFile) && isTRUE(hasRef) && isTRUE(hasTarget)
      
      shinyjs::toggleState("convertBtn", ready)
   })
   
   observeEvent(input$convertBtn, {
      showPageSpinner()
      Sys.sleep(1.5)
      hidePageSpinner()
      
      disable("convertBtn")
      
      inputPath <- input$convertFile$datapath
      refPath <- input$refFile$datapath
      targetSet <- input$targetPop
      targetName <- if (targetSet) input$targetPopName else NULL
      
      inputData <- read.csv(inputPath, header = TRUE)
      numMarkers <- ncol(inputData) - 2
      

      
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
  
   # CSV TO STR #
   observe({
      toggleState("csv2str", !is.null(input$tostrFile) && !is.null(input$systemFile))
   })
   
   csv_revised <- reactiveVal(NULL)
   strconvert <- reactiveVal(NULL)
   str_file <- reactiveVal(NULL)
   
   observeEvent(input$csv2str, {
      showPageSpinner()
      Sys.sleep(1.5)
      
      disable("csv2str")
      
      withProgress(message = "Analyzing files...", value = 0, {
         tryCatch({
            req(input$tostrFile$datapath, input$systemFile)
            
            csv_file <- load_input_file(input$tostrFile$datapath)
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
      hidePageSpinner()
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
   
   ## Concordance Analysis
   observe({
      toggleState("compareBtn", !is.null(input$concordanceFile1) && !is.null(input$concordanceFile2))
   })
   
   concordanceResult <- reactiveVal(NULL)
   concordancePlotPath <- reactiveVal(NULL)
   
   observeEvent(input$compareBtn, {
      showPageSpinner()
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
      hidePageSpinner()
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
   
   
   ## MARKER EXTRACTION
   output$exampleRSID <- renderTable({
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
   extracted_file <- reactiveVal(NULL)
   
   observe({
      isFileUploaded <- !is.null(input$markerFile) || (!is.null(input$bedFile) && !is.null(input$bimFile) && !is.null(input$famFile))
      
      isRSIDReady <- input$markerType == "rsid" && (
         (input$rsidInputType == "manual" && nzchar(input$typedRSIDs)) ||
            (input$rsidInputType == "upload" && !is.null(input$markerList1))
      )
      
      isPOSReady <- input$markerType == "pos" && !is.null(input$markerList2)
      
      toggleState("extractBtn", isFileUploaded && (isRSIDReady || isPOSReady))
   })
   
   
   observeEvent(input$extractBtn, {
      disable("extractBtn")
      showPageSpinner()
      Sys.sleep(1.5)
      
      temp_dir <- tempdir()
      
      withProgress(message = "Extracting markers...", value = 0, {
         tryCatch({
            snps_list <- if (input$markerType == "rsid") {
               if (input$rsidInputType == "manual") {
                  temp <- tempfile(fileext = ".txt")
                  writeLines(strsplit(input$typedRSIDs, "\n")[[1]], temp)
                  temp
               } else if (!is.null(input$markerList1)) {
                  input$markerList1$datapath
               }
            } else NULL
            
            pos_list <- if (input$markerType == "pos" && !is.null(input$markerList2)) {
               load_csv_xlsx_files(input$markerList2$datapath)
            } else NULL
            
            addSNPinfo <- !is.null(input$addRSID)
            
            input_type <- if (!is.null(input$markerFile)){
               if (grepl("\\.bcf$", input$markerFile$name, ignore.case = TRUE)) {
                  "bcf"
               } else if (grepl("\\.vcf.gz$", input$markerFile$name, ignore.case = TRUE)) {
                  "vcf"
               } else {
                  stop("Unsupported input file format.")
               }
            } else {
               "plink"
            }
            
            if (addSNPinfo) {
               extracted_markers <- extract_POStoID(
                  pos.list = pos_list,
                  input_type = input_type,
                  input.file = if (input_type %in% c("vcf", "bcf")) input$markerFile$datapath else NULL,
                  bed.file = input$bedFile$datapath,
                  bim.file = input$bimFile$datapath,
                  fam.file = input$famFile$datapath,
                  output.dir  = temp_dir,
                  plink_path  = plink_path
               )
            } else {
               extracted_markers <- extract_markers(
                  input_type = input_type,
                  input.file  = if (input_type %in% c("vcf", "bcf")) input$markerFile$datapath else NULL,
                  snps.list = snps_list,
                  pos.list = pos_list,
                  bed.file = input$bedFile$datapath,
                  bim.file = input$bimFile$datapath,
                  fam.file = input$famFile$datapath,
                  output.dir  = temp_dir,
                  merged.file = "final_merged.vcf",
                  plink_path  = plink_path
               )
            }
            extracted_file(extracted_markers)
            showNotification("VCF file successfully extracted and ready for download!", type = "message")
            enable("extractBtn")
         }, error = function(e) {
            showNotification(paste("Error:", e$message), type = "error")
            enable("extractBtn")
         })
      })
      hidePageSpinner()
   })
   output$downloadExtracted <- downloadHandler(
      filename = function() { "final_merged.vcf" },
      content = function(file) {
         req(extracted_file())
         file.copy(extracted_file(), file)
      },
      contentType = "text/vcf"
   )
   
   output$downloadExtracted_UI <- renderUI({
      req(extracted_file())
      downloadButton("downloadExtracted", "Download Extracted Markers (VCF)")
   })
   
   #############
   # Filtering #
   #############
   # enable run button only if there's input files and at least one filter
   observe({
      hasFile <- !is.null(input$forFilter)
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
   observeEvent(input$forFilter, {
      ext <- tools::file_ext(input$forFilter$name)
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
      req(input$forFilter)
      showPageSpinner()
      Sys.sleep(1.5)
      
      vcf_path <- input$forFilter$datapath
      ref_path <- if (!is.null(input$highlightRef)) input$highlightRef$datapath else NULL
      palette <-  if (!is.null(input$colorPalette)) input$colorPalette else NULL
      
      ext <- tools::file_ext(input$forFilter$name)
      
      if (tolower(ext) == "vcf" && input$enableDP){
         dp <- depth_from_vcf(
            vcf = vcf_path,
            output.dir = temp_dir,
            reference = ref_path,
            palette = palette
         )
         depth_outputs(dp)
      }
      
      # Revised 14 November 2025 to first convert files to PLINK before filtering
      input_file <- convert_to_plink(input$forFilter$datapath, temp_dir)
      
      # for plink filtering
      plink_cmds <- c("plink", "--bfile", shQuote(paste(temp_dir, "converted_to_plink")), "--out", file.path(temp_dir, "filtered"))
      
      if (input$filterIndiv){
         plink_cmds <- c(plink_cmds, "--mind", input$mindThresh)
      }
      if (input$filterVariant){
         plink_cmds <- c(plink_cmds, "--mind", input$genoThresh)
      }
      if (input$filterAllele){
         plink_cmds <- c(plink_cmds, "--mind", input$mafThresh)
      }
      if (input$filterQuality){
         plink_cmds <- c(plink_cmds, "--mind", input$qualThresh)
      }
      if (input$filterHWE){
         plink_cmds <- c(plink_cmds, "--mind", input$qualHWE)
      }
      if (input$filterLD){
         plink_cmds <- c(plink_cmds, "--mind", input$ldWindow, input$ldStep, input$ldR2)
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
      
      hidePageSpinner()
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
   
   #######
   # MSA #
   #######

   fasta_data <- reactiveVal(NULL)
   alignment_msa <- reactiveVal(NULL)
   alignment_scores <- reactiveVal(NULL)
   alignment_adjusted <- reactiveVal(NULL)
   alignment_staggered <- reactiveVal(NULL)
   directory <- tempdir()
   
   observeEvent(input$runMSA, {
      req(input$fastaFile)
      showPageSpinner()
      Sys.sleep(1.5)
      fasta <- read_fasta(input$fastaFile$datapath, directory)
      fasta_data(fasta)

      aligned <- calc_msa(fasta, 
                             algorithm = input$substitutionMatrix)
      
      alignment_msa(aligned$alignment)
      alignment_scores(aligned$scores)
      alignment_adjusted(aligned$adjusted)
      alignment_staggered(aligned$staggered)
      
      hidePageSpinner()
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

         #writeLines(utils::capture.output(print(alignment)), file)
         #seqinr::write.fasta(sequences = as.list(alignment), names = getSequence(alignment), file.out = filename)
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
   
   #filename3 <- paste0(directory, "/aligned_seqs.pdf")
   
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

   
   # Phylogenetic Tree Construction
   tree_plot <- reactiveVal(NULL)
   tree_path <- reactiveVal(NULL)
   tree_model <- reactiveVal(NULL)
   
   observeEvent(input$buildTree, {
      req(aligned_data())
      disable("buildTree")
      showPageSpinner()
      Sys.sleep(1.5)
      
      withProgress(message = "Building phylogenetic tree...", value = 0, {
         tryCatch({
            tree_type <- input$treeType
            outgroup <- input$outgroup
            bs <- input$boostrapSamples
            model <- input$model
            aligned <- aligned_data()
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
      
      hidePageSpinner()
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
   
   output$downloadAll <- downloadHandler(
      filename = function() {"phylo_outputs.zip"},
      content = function(file){
         tmp_fasta <- tempfile(fileext = ".fasta")
         tmp_scores <- tempfile(fileext = ".txt")
         tmp_pdf <- alignment_pdf()
         tmp_tree <- if(!is.null(tree_plot())){
            tmp <- tempfile(fileext=".png")
            ggsave(tmp, plot = tree_plot(), width = 8, height = 6, dpi = 300)
            tmp
         } else {
            tree_path()
         }
         
         writeLines(capture.output(print(aligned_data())), tmp_fasta)
         writeLines(capture.output(print(alignment_scores())), tmp_scores)
         
         zip::zip(file, files = c(tmp_fasta, tmp_scores, tmp_pdf, tmp_tree))
      }
   )
   
   
   output$downloadTree_UI <- renderUI({
      req(tree_plot()) || req(tree_path())
      downloadButton("downloadTree", "Download Phylogenetic Tree")
   })
   
   output$downloadAll_UI <- renderUI({
      req(tree_plot()) || req(tree_path())
      downloadButton("downloadAll", "Download All Outputs")
   })

   
   # BARCODING

   # Species identification
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
      showPageSpinner()
      Sys.sleep(1.5)
      
      req(input$refBarcoding)
      req(input$queBarcoding)
      
      # read file
      # 13 Jan 2026: added "guess.format.msa" and "$datapath" for input files
      barcoding_ref <- rphast::read.msa(input$refBarcoding$datapath, format = guess.format.msa(input$refBarcoding$datapath, method = "content"))
      barcoding_que <- rphast::read.msa(input$queBarcoding$datapath, format = guess.format.msa(input$refBarcoding$datapath, method = "content"))
      
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
      hidePageSpinner()
   })
   
   output$identificationResult <- renderPrint({
      req(resultIdentity())
      resultIdentity()
   })
   
   # Optimize kmer values
   observe({
      fileReady <- !is.null(input$optimizeKmerRef)
      toggleState("calOptimumKmer", fileReady)
   })
   
   kmerFile <- reactiveVal(NULL)
   optimalKmer <- reactiveVal(NULL)
   
   observeEvent(input$calOptimumKmer, {
      disable("calOptimumKmer")
      req(input$optimizeKmerRef)
      showPageSpinner()
      Sys.sleep(1.5)
      
      barcoding_ref <- rphast::read.msa(input$optimizeKmerRef$datapath, format = rphast::guess.format.msa(input$optimizeKmerRef$datapath, method = "content"))
      kmer_File <- ape::as.DNAbin(as.character(barcoding_ref))
      optimal_Kmer <- BarcodingR::optimize.kmer(kmerFile, max.kmer = input$maxKmer)
      kmerFile(kmer_File)
      optimalKmer(optimal_Kmer)
      
      hidePageSpinner()
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
   
   # barcoding gap
   observe({
      gapReady <- !is.null(input$barcodeRef)
      toggleState("gapBarcodes", gapReady)
   })
   
   refBarcode <- reactiveVal(NULL)
   barcodeGap <- reactiveVal(NULL)
   
   observeEvent(input$gapBarcodes, {
      disable("gapBarcodes")
      req(input$barcodeRef)
      showPageSpinner()
      Sys.sleep(1.5)
      
      barcoding_ref <- rphast::read.msa(input$barcodeRef$datapath, format = rphast::guess.format.msa(input$barcodeRef$datapath, method = "content"))
      ref_Barcode <- ape::as.DNAbin(as.character(barcoding_ref))
      gap <- BarcodingR::barcoding.gap(refBarcode, dist = input$gapModel)
      refBarcode(ref_Barcode)
      barcodeGap(gap)
      hidePageSpinner()
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
   
   # barcodes eval
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
      showPageSpinner()
      Sys.sleep(1.5)
      
      b1 <- rphast::read.msa(input$barcode1$datapath, format = rphast::guess.format.msa(input$barcode1$datapath, method = "content"))
      b2 <- rphast::read.msa(input$barcode2$datapath, format = rphast::guess.format.msa(input$barcode2$datapath, method = "content"))
      barcode1 <- ape::as.DNAbin(as.character(b1))
      barcode2 <- ape::as.DNAbin(as.character(b2))
      
      # convert to dataframe to download
      result <- BarcodingR::barcodes.eval(barcode1, barcode2, kmer1 = kmer1, kmer2 = kmer2)
      resultBarcodes(result)
      hidePageSpinner()
   }) # end of observe event
   
   output$evalBarcodesResult <- renderTable({
      req(resultBarcodes())
      result2 <- as.data.frame(resultBarcodes())
      result2
   })
   
   # tdr2
   
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
      showPageSpinner()
      Sys.sleep(1.5)
      
      que <- rphast::read.msa(input$oneSpe$datapath, format = rphast::guess.format.msa(input$oneSpe$datapath, method = "content"))
      ref <- rphast::read.msa(input$queSpe$datapath, format = rphast::guess.format.msa(input$queSpe$datapath, method = "content"))
      
      query <- ape::as.DNAbin(as.character(que))
      reference <- ape::as.DNAbin(as.character(ref))
      
      queTDR(query)
      refTDR(reference)
      hidePageSpinner()
   })
   
   # issue with results, it prints and not stores
   output$tdrValues <- renderPrint({
      req(queTDR())
      req(refTDR())
      req(input$bootValue1)
      req(input$bootValue2)
      BarcodingR::TDR2(queTDR(), refTDR(), boot = input$bootValue1, boot2 = input$bootValue2)
   })
   
   
   # POP STAT
   output$examplePop <- renderTable({
      data.frame(
         Sample = c("Sample1", "Sample2", "Sample3", "Sample4", "..."),
         Population = c("POP1", "POP2", "POP3", "POP4", "..."),
         rs101 = c("A/A", "A/T", "A/A", "T/T", "..."),
         rs102 = c("G/G", "C/C", "G/C", "G/G", "..."),
         rs_n = c("...", "...", "...", "...", "...")
      )
   })
   
   observe({
      file_ready <- !is.null(input$popStatsFile)
      shinyjs::toggleState("runPopStats", condition = file_ready)
   })
   
   privAlleles <- reactiveVal(NULL)
   popStats <- reactiveVal(NULL)
   hardyWeinberg <- reactiveVal(NULL)
   fstStats <- reactiveVal(NULL)
   fstData <- reactiveVal(NULL)
   
   statsMatrix <- reactiveVal(NULL)
   hwMatrix <- reactiveVal(NULL)
   fstMatrix <- reactiveVal(NULL)
   
   observeEvent(input$runPopStats, {
      disable("runPopStats")
      req(input$popStatsFile)
      showPageSpinner()
      Sys.sleep(1.5)
      
      fsnps_gen <- reactive({
         req(input$popStatsFile)
         df <- load_input_file(input$popStatsFile$datapath)
         cleaned <- clean_input_data(df)
         convert_to_genind(cleaned)
      })
      
      withProgress(message = "Running population analysis...", value = 0, {
         
         tryCatch({
            req(fsnps_gen())
            
            incProgress(0.4, detail = "Computing private alleles...")
            
            priv_alleles <- poppr::private_alleles(fsnps_gen())
            if (is.null(priv_alleles)) priv_alleles <- list(message = "No private alleles detected")
            privAlleles(priv_alleles)
            
            incProgress(0.6, detail = "Computing population statistics...")
            population_stats <- compute_pop_stats(fsnps_gen())
            popStats(population_stats)
            
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
      
      hidePageSpinner()
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
      popStats()$allele_frequencies
   }, options = list(scrollX = TRUE))
   
   output$hwe_summary <- renderUI({
      pvals <- hardyWeinberg()$hw_summary
      tagList(
         h4("HWE P-values across loci"),
         verbatimTextOutput("hwe_summary_text")
      )
   })
   
   output$hwe_summary_text <- renderText({
      paste0("P-values: ", paste(round(hardyWeinberg()$hw_summary, 4), collapse = ", "))
   })
   
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
         req(statsMatrix(), hwMatrix(), fstMatrix(), privAlleles())
         path <- export_pop_results(privAlleles(), statsMatrix(), hwMatrix(), fstMatrix(), dir = tempdir())
         
         file.copy(path, file)
         #openxlsx::write.xlsx(path, file = filename)
      }
   )
   
   output$downloadStatsXLSX_UI <- renderUI({
      req(statsMatrix(), hwMatrix(), fstMatrix(), privAlleles())
      downloadButton("downloadStatsXLSX", "Download Results (excel)")
   })
   
   # PCA
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
      
      readyForPCA <- hasDataFile && (usingDefaults || (hasLabelsFile && hasColorFile && hasShapesFile))
      
      toggleState("runPCA", readyForPCA)
   })
   
   observeEvent(input$runPCA, {
      disable("runPCA")
      req(input$pcaFile)
      showPageSpinner()
      Sys.sleep(1.5)
      
      withProgress(message = "Running PCA...", {
         tryCatch({
            incProgress(0.2, detail = "Loading input file...")
            df <- load_input_file(input$pcaFile$datapath)
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
               
               if (length(labels) != length(colors) && length(labels) != length(shapes)) {
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
            
            incProgress(0.6, detail = "Computing PCA...")
            pca_results <- compute_pca(fsnps_gen)
            
            # to add under ui
            # add an option to download
            output$barPlot <- renderPlot({
               # identify percent of variance explained per component
               graphics::barplot(pca_results$percent, 
                                 ylab = "Genetic variance explained by eigenvectors (%)", ylim = c(0,25),
                                 names.arg = round(pca_results$percent, 1))
            })
            
            incProgress(0.8, detail = "Rendering PCA plot...")
            
            output$pcaPlot <- renderPlot({
               plot_pca(
                  ind_coords = pca_results$ind_coords,
                  centroid = pca_results$centroid,
                  percent = pca_results$percent,
                  labels_colors = labels_colors,
                  pc_x = input$pcX,
                  pc_y = input$pcY
               )
            })
            
            enable("runPCA")
         }, error = function(e) {
            showNotification(paste("PCA Error:", e$message), type = "error")
            enable("runPCA")
         })
      })
      hidePageSpinner()
   })
   
   output$downloadbarPlot <- downloadHandler(
      filename = function() {
         paste0("bar_plot_", Sys.Date(), ".png")
      },
      content = function(file) {
         png(file, width = 800, height = 800, res = 300)
         graphics::barplot(
            pca_results$percent,
            ylab = "Genetic variance explained by eigenvectors (%)",
            ylim = c(0, 25),
            names.arg = round(pca_results$percent, 1)
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
            ind_coords = pca_results$ind_coords,
            centroid = pca_results$centroid,
            percent = pca_results$percent,
            labels_colors = labels_colors,
            pc_x = input$pcX,
            pc_y = input$pcY
         )
         
         # Save directly to the requested file path
         ggsave(filename = file, plot = plot, width = 8, height = 8, dpi = 600)
      },
      contentType = "image/png"
   )
   
   ## STRUCTURE ANALYSIS
   observe({
      file_ready <- !is.null(input$structureFile)
      shinyjs::toggleState("runStructure", condition = file_ready)
   })
   
   structure_result <- reactiveVal(NULL)
   qmatrices_result <- reactiveVal(NULL)
   structure_plot_paths <- reactiveVal(NULL)
   output_dir <- tempdir()
   out_path <- file.path(output_dir, "structure_input.str")
   
   observeEvent(input$runStructure, {
      disable("runStructure")
      req(input$structureFile)
      showPageSpinner()
      Sys.sleep(1.5)
      
      withProgress(message = "Running STRUCTURE analysis...", {
         incProgress(0.2, detail = "Loading input file...")
         df <- load_input_file(input$structureFile$datapath)
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
      
      hidePageSpinner()
   }) # end of observe event for structure
   
   output$downloadLogs <- downloadHandler(
      filename = function() {
         paste0("structure_logs_", Sys.Date(), ".zip")
      },
      content = function(file) {
         log_files <- list.file(output_dir, pattern = "_log$", full.names = TRUE)
         if (length(log_files) == 0) return(NULL)
         zip::zipr(zipfile = file, files = log_files)
      },
      contentType = "application/zip"
   )
   
   output$downloadFOutputs <- downloadHandler(
      filename = function() {
         paste0("structure_outputs_", Sys.Date(), ".zip")
      },
      content = function(file) {
         f_files <- list.files(output_dir, pattern = "_f$", full.names = TRUE)
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
         req(qmatrix_zip_path())
         temp_dir <- tempfile()
         dir.create(temp_dir)
         lapply(names(qmatrices_result()), function(name){
            matrix_data <- qmatrices_result()[[name]]
            if (!is.data.frame(matrix_data)) return(NULL)
            write.table(matrix_data, file = file.path(temp_dir, paste0(name, ".txt")),
                        row.names = FALSE, col.names = FALSE, quote = FALSE, sep = "\t")
         })
         zip::zipr(zipfile = file, files = list.files(temp_dir, full.names = TRUE))
      },
      contentType = "application/zip"
   )
   
   output$downloadStructurePlots <- downloadHandler(
      filename = function() {
         paste0("structure_plots_", Sys.Date(), ".zip")
      },
      content = function(file) {
         req(structure_plot_paths())
         zip::zipr(zipfile = file, files = structure_plot_paths())
      },
      contentType = "application/zip"
   )
   
   output$structurePlotPreview <- renderImage({
      req(structure_plot_paths())
      list(
         src = structure_plots()[[1]],
         contentType = "image/png",
         alt = "STRUCTURE Plot Preview",
         width = "100%"
      )
   }, deleteFile = FALSE)
   
   output$downloadButtons <- renderUI({
      req(structure_plot_paths(), qmatrices_result(), structure_result())
      tagList(
         downloadButton("downloadLogs", "Download Log Files (.zip)"),
         br(), br(),
         downloadButton("downloadFOutputs", "Download STRUCTURE _f Files (.zip)"),
         br(), br(),
         downloadButton("downloadQMatrixTxtZip", "Download Q Matrices (.zip)"),
         br(), br(),
         downloadButton("downloadStructurePlots", "Download STRUCTURE Plots (.zip)")
      )
   })
   
   # Prediction #
   observe({
      file_ready <- !is.null(input$forPredFile)
      shinyjs::toggleState("runNaiveBayes", condition = file_ready)
   })
   
   predResults <- reactiveVal(NULL)
   
   observeEvent(input$runNaiveBayes, {
      disable("runNaiveBayes")
      req(input$forPredFile)
      
      showPageSpinner()
      Sys.sleep(1.5)
      
      result <- calculate_naive_bayes(input$forPredFile$datapath)
      predResults(result)
      
      hidePageSpinner()
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
         
         openxlsx::write.xlsx(datasets, file = file)
      }
   )
   
   output$downloadClassification_UI <- renderUI({
      req(predResults())
      downloadButton("downloadClassification", "Download Results")
   })
}

shinyApp(ui, server)