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
                  menuSubItem("🔄 File Conversion", tabName = "FileConv"),
                  menuSubItem("🧬 SNP Extraction", tabName = "markerExtract"),
                  menuSubItem("🔽 Filtering", tabName = "FilterTab")
         ), # End of menu item for data pre-processing
         menuItem("Data Processing", tabName = "DataProcess", icon = icon("diagram-project"), startExpanded = TRUE,
                  menuSubItem("📑 DNA Barcoding", tabName = "DNABarcoding"),
                  menuSubItem("📝 Forensic Summary Statistics", tabName = "PopStatistics"),
                  menuSubItem("🔍 Exploratory Analysis", tabName = "PCA"),
                  menuSubItem("📊 Population Structure Analysis", tabName = "PopStructure"),
                  menuSubItem("🪪 Forensic Ancestry Inference", tabName = "Classification")
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
                             fluidRow(
                                box(
                                   title = "File Conversion Options",
                                   width = 5,
                                   radioButtons("inputType1", "Choose starting file type",
                                                choices = c("VCF file" = "vcf1",
                                                            "BCF file" = "bcf1",
                                                            "PLINK files (.bed/.bim/.fam)" = "plink1",
                                                            "CSV file" = "csv1")),
                                   
                                   # VCF options
                                   conditionalPanel(
                                      condition = "input.inputType1 == 'vcf1'",
                                      fileInput("VCFFile", "Upload VCF File"),
                                      radioButtons("inputType2", "Choose final file type",
                                                   choices = c("PLINK files (.bed/.bim/.fam)" = "plink2",
                                                               "CSV file" = "csv2",
                                                               "FASTA file" = "fasta")),
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
                                   
                                   # BCF options
                                   conditionalPanel(
                                      condition = "input.inputType1 == 'bcf1'",
                                      fileInput("BCFFile", "Upload BCF File"),
                                      radioButtons("inputType2", "Choose final file type",
                                                   choices = c("VCF file" = "vcf2",
                                                               "PLINK files (.bed/.bim/.fam)" = "plink2",
                                                               "CSV file" = "csv2")),
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
                                   
                                   # PLINK options
                                   conditionalPanel(
                                      condition = "input.inputType1 == 'plink1'",
                                      fileInput("bedFile", "Upload BED File"),
                                      fileInput("bimFile", "Upload BIM File"),
                                      fileInput("famFile", "Upload FAM File"),
                                      radioButtons("inputType2", "Choose final file type",
                                                   choices = c("VCF file" = "vcf2",
                                                               "CSV file" = "csv2")),
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
                                   
                                   # CSV options
                                   conditionalPanel(
                                      condition = "input.inputType1 == 'csv1'",
                                      p("This feature automatically converts a CSV file to VCF"),
                                      fileInput("CSVFile", "Upload CSV File"),
                                      fileInput("lociMetaFile", "Upload loci/marker information")
                                   ),
                                   actionButton("ConvertFILES", "Convert files", icon = icon("file-csv"))
                                ), # end of box
                                tabBox(
                                   tabPanel("Instructions", 
                                            h5("This tab intercqonverts common genetic files and to CSV with population information."),
                                            p("Input file/s:"),
                                            tags$ul(
                                               tags$li("VCF, BCF, or PLINK (.bed, .bim, .fam) files."),
                                               tags$li("(to CSV) Population data (XLSX/CSV/TXT)."),
                                               tags$li("(VCF to FASTA) Reference sequence in FASTA format."),
                                               tags$li("(CSV to VCF) Marker information with the following columns: [1] SNP, [2] CHR, [3] POS, [4] Genetic distance, [5] REF Allele [6] ALT Allele")
                                            ),
                                            p("Expected output file/s: VCF, PLINK, or CSV file."),
                                            br(),
                                   ),
                                   tabPanel("Sample Input Format/s", 
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
                                   ),
                                   tabPanel("Download Sample Files", 
                                            tags$a("A. Sample VCF", href="www/sample_hgdp.vcf"),
                                            br(),
                                            tags$a("B. Sample CSV file (for VCF conversion)", href = "www/sample.csv", download = NA),
                                            br(),
                                            tags$a("C. Sample marker metadata file (for CSV-VCF conversion)", href = "www/marker_info.csv", download = NA)
                                   )
                                ),
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
                                            h5("This tab converts zipped ForenSeq UAS outputs to a single file in a wide format."),
                                            p("This section builds upon the work of Ms. Maeviviene Sosing as part of the Filipino Genomes Research Program 2"),
                                            p("Input file/s: Compressed folder (.zip or .tar) of XLSX files."),
                                            p("Expected output file/s: Single CSV file (merged XLSX files).")
                                            ),
                                   tabPanel("Sample Input Format/s", 
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
                                      p("SNIPPER tool for sample classification: ",
                                        tags$a("SNIPPER tool",
                                               href="https://mathgene.usc.es/snipper/index.php",
                                               target="_blank"))
                             ),
                             tabPanel("Sample Input Format/s",
                                h5("Sample Input File"),
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
                                                fileInput("famFile", "PLINK FAM file")
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
                                            h5("Extract SNPs based on rsID (marker identification) or GRCh37/GRCh38 position"),
                                            p("Input file/s:"),
                                            p("(1) VCF, BCF, or PLINK (.bed, .bim, .fam) files."),
                                            p("(2) Markers/position list — you may type rsIDs manually, upload a list, or use a POS txt file."),
                                            p("The position list (txt file) should include:"),
                                            tags$ul(
                                               tags$li("[1] Chromosome number (integer)"),
                                               tags$li("[2] Starting base-pair position (integer)"),
                                               tags$li("[3] Final base-pair position (integer)")
                                            ),
                                            p("Parameter/s: Filtering options"),
                                            p("Expected output file/s: VCF file")
                                   ), # end of tabpanel
                                   tabPanel("Sample Input Format/s",
                                      h5("rsID Format"),
                                      tableOutput("exampleRSID"),
                                      h5("Position Format"),
                                      tableOutput("examplePOS")
                                   ),
                                   tabPanel("Download Sample Files",
                                            h5("Sample File/s:"),
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
                                      downloadButton("downloadExtracted", "Download Extracted VCF"),
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
                                h5("Concordance analysis between files with the same samples"),
                                p("Input file/s: Two CSV or XLSX files."),
                                p("Parameter/s: Indicate if using haplotypes"),
                                p("Expected output/s:"),
                                tags$ul(
                                   tags$li("Concordance table"),
                                   tags$li("Concordance plot")
                                )
                             ), # end of tP instructions
                             tabPanel("Sample Input Format/s",
                                h5("File Format (for concordance)"),
                                tableOutput("exampleTable")
                             ), # end of tP sample files 
                             tabPanel("Download Sample Files",
                                h5("Sample Files"),
                                tags$ul(
                                   tags$a("Sample CSV file (1)", href = "www/sample1_for.concordance.csv", download = NA),
                                   br(),
                                   tags$a("Sample CSV file (2)", href = "www/sample1_for.concordance.csv", download = NA)
                                )
                             )
                          ), # end of second tabbox
                          tabBox(
                             tabPanel("Summary Table",
                                h4("Concordance Summary Table"),
                                tableOutput("concordanceResults") %>% withSpinner(color = "blue"),
                                downloadButton("downloadConcordance", "Download Concordance Results")
                             ),
                             tabPanel("Concordance Plot",
                                 h4("Concordance Plot"),
                                 plotOutput("concordancePlot", height = "600px") %>% withSpinner(color = "blue"),
                                 downloadButton("downloadConcordancePlot", "Download Plot")
                             )
                          )
                       ) # end of fluidrow
                    ) # end of tabpanel for concordance
                          ) # tabsetpanel
         ),
         tabItem(tabName = "FilterTab",
                  fluidRow(
                     tabBox(
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
                          p("Input file/s: VCF file"),
                          p("Parameter/s:"),
                          tags$ul(
                             tags$li("--mind [value]"),
                             tags$li("--geno [value]"),
                             tags$li("--maf [value]"),
                             tags$li("--qual-threshold [value]"),
                             tags$li("--hwe [value]"),
                             tags$li("--indep-pairwise [value]"),
                             tags$li("Other additional PLINK flags")
                          ),
                          p("Expected output/s:"),
                          tags$ul(
                             tags$li("VCF file"),
                             tags$li("Depth of Coverage Plots")
                          ),
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
                              imageOutput("depthMarkerPlot") %>% withSpinner(color = "blue"),
                              imageOutput("depthSamplePlot") %>% withSpinner(color = "blue")
                        ),
                        tabPanel("Download Files",
                              downloadButton("downloadFilteredFile", "Download Filtered File"),
                              downloadButton("downloadDepthPlots", "Download Plots")
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
                                 p("Input file/s: Zipped folder of FASTA files."),
                                 p("Parameter/s: Substitution matrix for the alignment (ClustalW, ClustalOmega, MUSCLE)"),
                                 p("Expected output/s:"),
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
                                h5("Sample Files"),
                                tags$ul(
                                   tags$a("Sample zipped FASTA file", href = "www/lactobacillus/lacto2.zip", download = NA)),
                             )
                          ),
                          tabBox(
                             tabPanel("Preview of Alignments",
                                verbatimTextOutput("initialAlignmentText") %>% withSpinner(color = "blue"),
                                br(),
                                verbatimTextOutput("adjustedAlignmentText") %>% withSpinner(color = "blue"),
                                br(),
                                verbatimTextOutput("staggeredAlignmentText") %>% withSpinner(color = "blue"),
                                br(),
                                verbatimTextOutput("alignmentScoresPreview") %>% withSpinner(color = "blue")
                             ),
                             tabPanel("Download Results",
                                 downloadButton("downloadAlignedFASTA", "Download Aligned Sequences"),
                                 downloadButton("downloadAlignmentScores", "Download Alignment Scores"),
                                 downloadButton("downloadAlignmentPDF", "Download Alignment PDF")
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
                           h5("Perform phylogenetic tree reconstruction using ape and phangorn R packages."),
                           p("Approaches to tree construction are NJ, UPGMA, Maximum Parsimony, and Maximum Likelihood. Check the assumptions and constraints of each approach [1]."),
                           p("Input file used is the alignment output from the MSA tab. There is an option of using the raw, adjusted, or staggered alignment for tree construction."),
                           p("Parameters vary based on the method."),
                           p("Expected output is the phylogenetic tree in PNG format.")
                          ),
                          tabBox(
                             tabPanel("View Results",
                                      h4("Phylogenetic Tree"),
                                      p("This tab uses the multiple sequence alignment performed in the previous tab."),
                                      uiOutput("treeImage") %>% withSpinner(color = "blue")
                             ),
                             tabPanel("Download Results",
                                      downloadButton("downloadTree", "Download Tree"),
                                      downloadButton("downloadAll", "Download All Outputs")  
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
                                   h5("This tab performs species identification using the R package 'BarcodingR'."),
                                   p("Input file/s:"),
                                   tags$ul(
                                      tags$li("Aligned reference sequences"),
                                      tags$li("Aligned query sequences")
                                   ),
                                   p("Parameter/s:"),
                                   tags$ul(
                                      tags$li("(without kmer method) Training model: bpNewTraining, fuzzyId, bpNewTrainingOnly, bpUsedTrained, or Bayesian"),
                                      tags$li("(with kmer method) Fuzzy-set Method or BP-based method")
                                   )
                                ),
                                tabBox(
                                   verbatimTextOutput("identificationResult") %>% withSpinner(color = "blue")
                                )
                             )
                          ), # end of first tabPanel
                          
                          tabPanel("Optimize kmer values",
                             fluidRow(
                                tabBox(
                                   fileInput("optimizeKmerRef", "Upload reference dataset"),
                                   numericInput("maxKmer", "Length of maximum kmer value", value = "5", min = 2),
                                   actionButton("calOptimumKmer", "Identify Optimum kmer value", icon = icon("upload"))
                                ),
                                tabBox(
                                   h5("Calculate the optimal kmer values"),
                                   p("Input file/s: Aligned sequences of the reference dataset (FASTA)"),
                                   p("Parameter/s: Length of maximum kmer value"),
                                   p("Expected output file: Kmer plot")
                                ),
                                tabBox(
                                   verbatimTextOutput("kmerResult") %>% withSpinner(color = "blue"),
                                   imageOutput("kmerPlot") %>% withSpinner(color = "blue"),
                                   downloadHandler("downloadKmerPlot", "Download Plot") 
                                )
                             )
                          ),
                          tabPanel("Barcoding Gap",
                             fluidRow(
                                tabBox(
                                   fileInput("barcodeRef", "Upload reference dataset"),
                                   selectInput("gapModel", "Choose Distance",
                                               choices = c("raw", "K80", "euclidean"),
                                               selected = "raw"),
                                   actionButton("gapBarcodes", "Calculate gap", icon = icon("arrows-left-right-to-line"))
                                ),
                                tabBox(
                                   h5("Calculate the barcoding gap"),
                                   p("Input file: VCF file"),
                                   p("Parameter/s: Distance (raw, K80, euclidean)"),
                                   p("Expected output file: Barcoding gap plot")
                                ),
                                tabBox(
                                   verbatimTextOutput("barcodingResult") %>% withSpinner(color = "blue"),
                                   imageOutput("BarcodingGapPlot") %>% withSpinner(color = "blue"),
                                   downloadHandler("downloadGapPlot", "Download Barcoding Gap Plot")
                                )
                             )
                          ),
                          tabPanel("Evaluate Barcodes",
                             fluidRow(
                                tabBox(
                                   fileInput("barcode1", "Upload Barcode 1"),
                                   fileInput("barcode2", "Upload Barcode 2"),
                                   numericInput("kmer1", "Length of kmer for barcode 1", value = 5, min = 1),
                                   numericInput("kmer2", "Length of kmer for barcode 2", value = 5, min = 1),
                                   actionButton("evalBarcodes", "Evaluate Barcodes", icon = icon("code-compare"))
                                ),
                                tabBox(
                                   h5("Evaluate Barcodes"),
                                   p("Input file/s: CSV or XLSX file."),
                                   p("Parameter/s: Length of kmer for barcode 1 and barcode 2 (separate)")
                                ),
                                tabBox(
                                   tableOutput("evalBarcodesResult") %>% withSpinner(color = "blue")
                                )
                             )
                          ),
                          tabPanel("Species Membership Value (TDR)",
                             fluidRow(
                                tabBox(
                                   p("Calculate the TDR2 value"),
                                   fileInput("oneSpe", "Upload DNA seq from a single query species"),
                                   fileInput("queSpe", "Upload DNA seq from different samples"),
                                   numericInput("bootValue1", "Bootstrap value for query species", value = 10, min = 1),
                                   numericInput("bootValue2", "Bootstrap value for reference samples", value = 10, min = 1),
                                   actionButton("calculateTDR2", "Calculate", icon = icon("calculator"))
                                ),
                                tabBox(
                                   h5("Species Membership Value (TDR)"),
                                   p("Input file/s: CSV file with marker and population data."),
                                   p("Parameter/s: Boostrap value for query and reference samples.")
                                ),
                                tabBox(
                                   verbatimTextOutput("tdrValues") %>% withSpinner(color = "blue")
                                )
                             )
                          )
                       ) # end of tabsetpanel
                    )
                 ) # end of tabsetPanel
                 ), # end of tab item for 
         tabItem(tabName = "adfa", "sfda")
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