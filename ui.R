library(shiny)
library(shinydashboard)

##############
# UI: Header #
##############
header <- dashboardHeader(
   title = div(
      tags$img(src = "logo.png", height = "30px", style = "display: inline-block; vertical-align: middle;"),
      tags$span("RESTful Forensics",
                style = "font-family: Carme, sans-serif; font-size: 26px; color: #92b2e4; vertical-align: middle; padding-left: 0px;")
   ),
   titleWidth = 300
   )

###############
# UI: Sidebar #
###############

sidebar <- dashboardSidebar(
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
)
############
# UI: Body #
############

body <- dashboardBody(
   tabItems(
      tabItem("dashboard"), # end of dashboard # ADD MORE CONTENT
     # tabItem("DataPreProcess"),
      tabItem("FileConv",
              tabPanel("Convert Files",
                 fluidRow(
                    tabBox(
                       title = "Instructions",
                       h4("This tab interconverts common genetic files and to CSV with population information."),
                       p("Input file/s:"), 
                       tags$ul(
                          tags$li("VCF, BCF, or PLINK (.bed, .bim, .fam) files."),
                          tags$li("(Optional) Population data (XLSX/CSV/TXT)."),
                          tags$li("Optional) Reference sequence in FASTA format (for VCF to FASTA conversion)"),
                          tags$li("(Optional) Marker information (for CSV to VCF conversion) with the following columns: [1] SNP, [2] CHR, [3] POS, [4] Genetic distance, [5] REF Allele [6] ALT Allele")
                       ),
                       p("Expected output file/s: VCF, PLINK, or CSV file."),
                       br(),
                       h4("Sample Files"),
                       tags$ul(
                          tags$a("A. Sample VCF file", href = "www/sample_hgdp.vcf", download = NA),
                          br(),
                          tags$a("B. Sample CSV file (for VCF conversion)", href = "www/sample.csv", download = NA),
                          br(),
                          tags$a("C. Sample marker metadata file (for CSV-VCF conversion)", href = "www/marker_info.csv", download = NA)
                       )
                    ),
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
                       fluidRow(
                          column(6,
                                 h5("To convert to a CSV file with population metadata:"),
                                 h5("This is a sample reference file. Only the first two columns are used."),
                                 tableOutput("exampleRefCSV")),
                          column(6,
                                 h5("For CSV to VCF conversion, a separate file on marker information is needed."),
                                 h5("See the following formats:"),
                                 h5("Required CSV format:"),
                                 tableOutput("exampleCSVFile"),
                                 h5("Required marker info format:"),
                                 tableOutput("exampleMarkerInfo")
                          )
                       ), # end of fluidRow
                       tableOutput("previewTable") %>% withSpinner(color = "blue"),
                       uiOutput("downloadVCF_UI"),
                       uiOutput("downloadCSV_UI"),
                       uiOutput("downloadFASTA_UI"),
                       uiOutput("downloadPLINK_UI")
                    ) # end of another tabbox
                 ) # end of fluidrow
              ) # end of tabPanel

              ), # end of file conversion tab
      tabItem("subitem2", "Sub-item 2 tab content") 
   )
)




##########
# Server #
##########