library(shiny)
library(shinydashboard)

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
         tabItem("dashboard", "Dashboard tab content"),
         tabItem("FileConv",
                 tabsetPanel(
                    tabPanel("Convert Files",
                             fluidRow(
                                tabBox(
                                   title = "Load input files",
                                   radioButtons("inputType1", "Choose starting file type",
                                                choices = c("VCF file" = "vcf1", "BCF file" = "bcf1", "PLINK files (.bed/.bim/.fam)" = "plink1", "CSV file" = "csv1")),
                                   
                                   conditionalPanel(
                                      condition = "input.inputType1 == 'vcf1'",
                                      fileInput("VCFFile", "Upload VCF File"),
                                      radioButtons("inputType2", "Choose final file type",
                                                   choices = c("PLINK files (.bed/.bim/.fam)" = "plink2", "CSV file" = "csv2", "FASTA file" = "fasta")),
                                      
                                      conditionalPanel(
                                         condition = "input.inputType2 == 'fasta'",
                                         fileInput("FASTARef", "Reference sequence in FASTA format.")
                                      ),
                                      
                                      conditionalPanel(
                                         condition = "input.inputType2 == 'csv2'",
                                         radioButtons("poptype", "Do samples come from a single population?",
                                                      choices = c("Yes" = "single", "No" = "multiplepop")),
                                         conditionalPanel(
                                            condition = "input.poptype == 'multiplepop'",
                                            fileInput("multiplepop", "Input reference file with sample ID and population"),
                                            helpText("*Accepts XLSX and CSV files")
                                         ),
                                         
                                         conditionalPanel(
                                            condition = "input.poptype == 'single'",
                                            textAreaInput("typePop", "Enter population", rows = 1)
                                         )
                                      )
                                   ), 
                                   
                                   conditionalPanel(
                                      condition = "input.inputType1 == 'bcf1'",
                                      fileInput("BCFFile", "Upload BCF File"),
                                      radioButtons("inputType2", "Choose final file type",
                                                   choices = c("VCF file" = "vcf2", "PLINK files (.bed/.bim/.fam)" = "plink2", "CSV file" = "csv2")),
                                      
                                      conditionalPanel(
                                         condition = "input.inputType2 == 'csv2'",
                                         radioButtons("poptype", "Do samples come from a single population?",
                                                      choices = c("Yes" = "single", "No" = "multiplepop")),
                                         conditionalPanel(
                                            condition = "input.poptype == 'multiplepop'",
                                            fileInput("multiplepop", "Input reference file with sample ID and population"),
                                            helpText("*Accepts XLSX and CSV files")
                                         ),
                                         
                                         conditionalPanel(
                                            condition = "input.poptype == 'single'",
                                            textAreaInput("typePop", "Enter population", rows = 1)
                                         )
                                      )
                                   ),
                                   
                                   conditionalPanel(
                                      condition = "input.inputType1 == 'plink1'",
                                      fileInput("bedFile", "Upload BED File"),
                                      fileInput("bimFile", "Upload BIM File"),
                                      fileInput("famFile", "Upload FAM File"),
                                      radioButtons("inputType2", "Choose final file type",
                                                   choices = c("VCF file" = "vcf2", "CSV file" = "csv2")),
                                      
                                      conditionalPanel(
                                         condition = "input.inputType2 == 'csv2'",
                                         radioButtons("poptype", "Do samples come from a single population?",
                                                      choices = c("Yes" = "single", "No" = "multiplepop")),
                                         conditionalPanel(
                                            condition = "input.poptype == 'multiplepop'",
                                            fileInput("multiplepop", "Input reference file with sample ID and population"),
                                            helpText("*Accepts XLSX and CSV files")
                                         ),
                                         
                                         conditionalPanel(
                                            condition = "input.poptype == 'single'",
                                            textAreaInput("typePop", "Enter population", rows = 1)
                                         )
                                      )
                                   ),
                                   
                                   conditionalPanel(
                                      condition = "input.inputType1 == 'csv1'",
                                      p("This feature automatically converts a CSV file to VCF"),
                                      fileInput("CSVFile", "Upload CSV File"),
                                      fileInput("lociMetaFile", "Upload loci/marker information"),
                                      radioButtons("poptypeCSV", "Do samples come from a single population?",
                                                   choices = c("Yes" = "single", "No" = "multiplepop")),
                                      
                                      conditionalPanel(
                                         condition = "input.poptypeCSV == 'multiplepop'",
                                         fileInput("multiplepop", "Input reference file with sample ID and population"),
                                         helpText("*Accepts XLSX and CSV files")
                                      ),
                                      
                                      conditionalPanel(
                                         condition = "input.poptypeCSV == 'single'",
                                         textAreaInput("typePop", "Enter population", rows = 1)
                                      )
                                   ),
                                   actionButton("ConvertFILES", "Convert files", icon = icon("file-csv"))
                                ),
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
                                   p("Expected output file/s: VCF, PLINK, or CSV file."),
                                   br()
                                   ), # end of tabpanel 1
                                 tabPanel("Sample Reference File",
                                    h5("To convert to a CSV file with population metadata:"),
                                    h5("This is a sample reference file. Only the first two columns are used."),
                                    tableOutput("exampleRefCSV"),
                                    br(),
                                    h5("For CSV to VCF conversion, a separate file on marker information is needed."),
                                    h5("See the following formats:"),
                                    h5("Required CSV format:"),
                                    tableOutput("exampleCSVFile"),
                                    h5("Required marker info format:"),
                                    tableOutput("exampleMarkerInfo")
                                 ), # end of tabpanel 2
                                 tabPanel("Download sample files",
                                          h4("Sample Files"),
                                          tags$ul(
                                             tags$a("A. Sample VCF file", href = "www/sample_hgdp.vcf", download = NA),
                                             br(),
                                             tags$a("B. Sample CSV file (for VCF conversion)", href = "www/sample.csv", download = NA),
                                             br(),
                                             tags$a("C. Sample marker metadata file (for CSV-VCF conversion)", href = "www/marker_info.csv", download = NA)
                                          )
                                          )
                                ), # end of tabbox
                                tabBox(
                                   width = "300px",
                                   tableOutput("previewTable") %>% withSpinner(color = "blue"),
                                   uiOutput("downloadVCF_UI"),
                                   uiOutput("downloadCSV_UI"),
                                   uiOutput("downloadFASTA_UI"),
                                   uiOutput("downloadPLINK_UI")
                                ) # end of another tabbox
                             ) # end of fluidrow
                    ), # end of tabPanel for file conv
                    tabPanel(
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
                          ), # end of 1st tabbox
                          tabBox(
                             tabPanel("Instructions",
                                      h4("This tab converts zipped ForenSeq UAS outputs to a single file in a wide format."),
                                      p("This section builds upon the work of Ms. Maeviviene Sosing as part of the Filipino Genomes Research Program 2"),
                                      p("Input file/s: Compressed folder (.zip or .tar) of XLSX files."),
                                      p("Expected output file/s: Single CSV file (merged XLSX files).")
                             ), # instructions
                             tabPanel("Sample Input File",
                                      h5("Sample input file. All alleles of available SNPs per sample are listed in a long format."),
                                      tableOutput("exampleXLSX")),
                             tabPanel("Download Sample Zipped File",
                                      tags$h4("Downloadable Sample"),
                                      tags$ul(
                                      tags$a("Sample zipped file", href = "www/sample_forenseq.zip", download = NA))
                             ) # sample of required format
                          ),
                          tabBox(
                             h4("Preview of Output"),
                             tableOutput("previewTableUAS") %>% withSpinner(color = "blue")
                          )
                       ) # end of fluid row
                    ) # end of tabPanel for widen forenseq
                 ) # end of tabset panel
         ), # end of file conv tabItem
         tabItem("subitem1", "Sub-item 1 tab content"),
         tabItem("subitem2", "Sub-item 2 tab content") 
      )
   )
)

server <- function(input, output, session) {
   output$res <- renderText({
      req(input$sidebarItemExpanded)
      paste("Expanded menuItem:", input$sidebarItemExpanded)
   })
}

shinyApp(ui, server)