library(shiny)
library(shinydashboard)
library(dplyr)
library(bslib)
library(dplyr)
library(shiny)
library(shinyjs)
library(shinycssloaders)

ui <- dashboardPage(
   dashboardHeader(
      title = div(
         tags$img(src = "logo.png", height = "30px", style = "display: inline-block; vertical-align: middle;"),
         tags$span("RESTful Forensics",
                   style = "font-family: Carme, sans-serif; font-size: 26px; color: #FFFFFF; vertical-align: middle; padding-left: 0px;")
      ),
      titleWidth = 300
   ), 
   dashboardSidebar(
      width = 300,
      sidebarMenu(
         menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
         menuItem("Data Pre-processing", tabName = "DataPreProcess", icon = icon("gears"), startExpanded = TRUE,
                  menuSubItem("File Conversion", tabName = "FileConv"),
                  menuSubItem("SNP Extraction", tabName = "markerExtract"),
                  menuSubItem("Filtering", tabName = "FilterTab")
         ), # End of menu item for data pre-processing
         menuItem("Data Processing", tabName = "DataProcess", icon = icon("diagram-project"), startExpanded = TRUE,
                  menuSubItem("DNA Barcoding", tabName = "DNABarcoding"),
                  menuSubItem("Forensic Summary Statistics", tabName = "PopStatistics"),
                  menuSubItem("Exploratory Analysis", tabName = "PCA"),
                  menuSubItem("Population Structure Analysis", tabName = "PopStructure"),
                  menuSubItem("Forensic Ancestry Inference", tabName = "Classification")
                  
         ), # End of menu item for data processing
         menuItem("References", tabName = "AppRef", icon = icon("book-bookmark")),
         menuItem("About", tabName = "About", icon = icon("building-user"))
      ),
      textOutput("res")
   ),
   dashboardBody(
      tabItems(
         tabItem(tabName = "dashboard",
                 h2("Dashboard tab content")
         ),
         
         tabItem(tabName = "FileConv",
                 tabsetPanel(
                    # First tab: Convert Files
                    tabPanel("Convert Files",
                             tabsetPanel(
                                # VCF Conversion
                                tabPanel("VCF Conversion",
                                         fluidRow(
                                            tabBox(
                                               title = "VCF Options",
                                               width = 12,
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
                                                     fileInput("multiplepop_vcf", "Input reference file with sample ID and population"),
                                                     helpText("*Accepts XLSX and CSV files")
                                                  ),
                                                  conditionalPanel(
                                                     condition = "input.poptype_vcf == 'single'",
                                                     textAreaInput("typePop_vcf", "Enter population", rows = 1)
                                                  )
                                               ),
                                               actionButton("ConvertVCF", "Convert VCF", icon = icon("file-csv"))
                                            )
                                         )
                                ),
                                
                                # BCF Conversion
                                tabPanel("BCF Conversion",
                                         fluidRow(
                                            tabBox(
                                               title = "BCF Options",
                                               width = 12,
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
                                                     fileInput("multiplepop_bcf", "Input reference file with sample ID and population"),
                                                     helpText("*Accepts XLSX and CSV files")
                                                  ),
                                                  conditionalPanel(
                                                     condition = "input.poptype_bcf == 'single'",
                                                     textAreaInput("typePop_bcf", "Enter population", rows = 1)
                                                  )
                                               ),
                                               actionButton("ConvertBCF", "Convert BCF", icon = icon("file-csv"))
                                            )
                                         )
                                ),
                                
                                # PLINK Conversion
                                tabPanel("PLINK Conversion",
                                         fluidRow(
                                            tabBox(
                                               title = "PLINK Options",
                                               width = 12,
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
                                                     fileInput("multiplepop_plink", "Input reference file with sample ID and population"),
                                                     helpText("*Accepts XLSX and CSV files")
                                                  ),
                                                  conditionalPanel(
                                                     condition = "input.poptype_plink == 'single'",
                                                     textAreaInput("typePop_plink", "Enter population", rows = 1)
                                                  )
                                               ),
                                               actionButton("ConvertPLINK", "Convert PLINK", icon = icon("file-csv"))
                                            )
                                         )
                                ),
                                
                                # CSV Conversion
                                tabPanel("CSV Conversion",
                                         fluidRow(
                                            tabBox(
                                               title = "CSV Options",
                                               width = 12,
                                               p("This feature automatically converts a CSV file to VCF"),
                                               fileInput("CSVFile", "Upload CSV File"),
                                               fileInput("lociMetaFile", "Upload loci/marker information"),
                                               radioButtons("poptypeCSV", "Do samples come from a single population?",
                                                            choices = c("Yes" = "single", "No" = "multiplepop")),
                                               conditionalPanel(
                                                  condition = "input.poptypeCSV == 'multiplepop'",
                                                  fileInput("multiplepop_csv", "Input reference file with sample ID and population"),
                                                  helpText("*Accepts XLSX and CSV files")
                                               ),
                                               conditionalPanel(
                                                  condition = "input.poptypeCSV == 'single'",
                                                  textAreaInput("typePop_csv", "Enter population", rows = 1)
                                               ),
                                               actionButton("ConvertCSV", "Convert CSV", icon = icon("file-csv"))
                                            )
                                         )
                                )
                             ), # end of tabsetPanel
                             
                             # Instructions block (shared for all conversions)
                             fluidRow(
                                tabBox(
                                   tabPanel("Instructions", 
                                            h4("This tab interconverts common genetic files and to CSV with population information."),
                                            p("Input file/s:"), 
                                            tags$ul(
                                               tags$li("VCF, BCF, or PLINK (.bed, .bim, .fam) files."),
                                               tags$li("(to CSV) Population data (XLSX/CSV/TXT)."),
                                               tags$li("(VCF to FASTA) Reference sequence in FASTA format."),
                                               tags$li("(CSV to VCF) Marker information with the following columns: [1] SNP, [2] CHR, [3] POS, [4] Genetic distance, [5] REF Allele [6] ALT Allele")
                                            ),
                                            p("Expected output file/s: VCF, PLINK, or CSV file.")
                                   ),
                                   tabPanel("Sample Reference File", 
                                            h5("To convert to a CSV file with population metadata:"),
                                            tableOutput("exampleRefCSV"),
                                            h5("Required CSV format:"),
                                            tableOutput("exampleCSVFile"),
                                            h5("Required marker info format:"),
                                            tableOutput("exampleMarkerInfo")
                                   ),
                                   tabPanel("Download sample files", 
                                            tags$a("A. Sample VCF", href="www/sample_hgdp.vcf"),
                                            br(),
                                            tags$a("B. Sample CSV file (for VCF conversion)", href = "www/sample.csv", download = NA),
                                            br(),
                                            tags$a("C. Sample marker metadata file (for CSV-VCF conversion)", href = "www/marker_info.csv", download = NA)
                                   )
                                )
                             ),
                             
                             # Conversion Results block (shared for all conversions)
                             fluidRow(
                                tabBox(
                                   title = "Conversion Results",
                                   tableOutput("previewTable") %>% withSpinner(color = "blue"),
                                   uiOutput("downloadVCF_UI"),
                                   uiOutput("downloadCSV_UI"),
                                   uiOutput("downloadFASTA_UI"),
                                   uiOutput("downloadPLINK_UI")
                                )
                             )
                    ),
                    
                    # Second tab: ForenSeq Conversion
                    tabPanel("ForenSeq Conversion",
                             fluidRow(
                                tabBox(
                                   fileInput("uas_zip", "Upload ZIP or TAR file",
                                             accept = c(".zip", ".tar")),
                                   helpText("*Accepts compressed files containing XLSX files."),
                                   fileInput("ref_file", "Optional Reference File (CSV or XLSX)",
                                             accept = c(".csv", ".xlsx")),
                                   actionButton("run_uas2csv", "Run Conversion"),
                                   br(), br(),
                                   downloadButton("downloadUAScsv", "Download Converted CSV")
                                ),
                                tabBox(
                                   #title = "Instructions",
                                   tabPanel("Instructions", 
                                            h4("This tab converts zipped ForenSeq UAS outputs to a single file in a wide format."),
                                            p("This section builds upon the work of Ms. Maeviviene Sosing as part of the Filipino Genomes Research Program 2"),
                                            p("Input file/s: Compressed folder (.zip or .tar) of XLSX files."),
                                            p("Expected output file/s: Single CSV file (merged XLSX files).")
                                   ),
                                   tabPanel("Sample Input File", 
                                            h5("Sample input file. All alleles of available SNPs per sample are listed in a long format."),
                                            tableOutput("exampleXLSX")
                                   ),
                                   tabPanel("Download Sample Zipped File", 
                                            tags$h4("Downloadable Sample"),
                                            tags$ul(
                                               tags$a("Sample zipped file", href = "www/sample_forenseq.zip", download = NA))
                                   )
                                ),
                                tabBox(
                                   title = "Preview of Output",
                                   tableOutput("previewTableUAS") %>% withSpinner(color = "blue")
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
                                            h5("This converts CSV or XLSX files to a SNIPPER-compatible file"),
                                            p("Input file/s: CSV or XLSX file."),
                                            p("Parameter/s: (optional) Target population name for classification"),
                                            p("Expected output file/s: XLSX file."),
                                            p("SNIPPER tool for sample classification: https://mathgene.usc.es/snipper/index.php")
                                   ),
                                   tabPanel(
                                      h5("Sample input file."),
                                      tableOutput("exampleTableSnipper"),
                                      h5("Sample reference file"),
                                      tableOutput("exampleRefSnipper")
                                   )
                                ), # end of tabbox for instruction
                                tabBox(
                                   h4("Preview of Converted SNIPPER Data"),
                                   tableOutput("previewTableSNIPPER") %>% withSpinner(color = "blue")
                                )
                             ) # end of fluidrow
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
                                            h5("Convert CSV files to STRUCTURE files."),
                                            p("Input file/s: CSV file with marker and population data."),
                                            p("Parameter/s: User's operating system (for STRUCTURE input compatibility)"),
                                            p("Expected output file/s:"),
                                            tags$ul(
                                               tags$li("structure (.str) file"),
                                               tags$li("revised input file")
                                            ),
                                            p("STRUCTURE generally can't handle sample labels with alphabets, the function converts sample labels to their associated row number."),
                                            p("For users who opt to use STRUCTURE via the terminal or GUI, instructions can be found here: ",
                                              tags$a("STRUCTURE v2.3.4 documentation",
                                                     href="https://web.stanford.edu/group/pritchardlab/structure_software/release_versions/v2.3.4/html/structure.html",
                                                     target="_blank"))
                                            
                                   )
                                ), # end of tabbox
                                tabBox(
                                   h4("Preview of output files"),
                                   tableOutput("revisedCSV"),
                                   tableOutput("strFile"),
                                   downloadButton("downloadrevised", "Download Revised CSV file"),
                                   downloadButton("downloadSTRfile", "Download STR file")
                                )
                             ) # end of fluidrow
                    ) # end of tabpanel for structure
                 )
         ),
         
         tabItem(tabName = "subitem1", "Sub-item 1 tab content"),
         tabItem(tabName = "subitem2", "Sub-item 2 tab content")
      )
   ) # end of dashboard body
)

server <- function(input, output, session) {
   output$res <- renderText({
      req(input$sidebarItemExpanded)
      paste("Expanded menuItem:", input$sidebarItemExpanded)
   })
}

shinyApp(ui, server)