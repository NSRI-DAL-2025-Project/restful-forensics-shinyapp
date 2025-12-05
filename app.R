# to-do: add a notice that server will disconnect after a certain time
# to-do: reset file uploads
library(bslib)
library(shinyjs)
source("functions.R", local = TRUE)
source("global.R")
#shiny::addResourcePath('www', '/srv/shiny-server/restful-forensics/www') # for docker

ui <- tagList(
   useShinyjs(),
   waiter::waiter_preloader(
      html = waiter::spin_fading_circles(),
      color = "rgba(255,255,255,0.6)"
   ),
   tags$head(
      tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css2?family=Carme&display=swap"),
      tags$style(HTML("
            body {font-family: 'Carme';}
            .navbar-nav > li:nth-child(1) > a { background-color: transparent !important; }
            .navbar-nav > li:nth-child(2) > a { background-color: transparent !important; }
            .navbar-nav > li:nth-child(3) > a { background-color: #aec8d9 !important; }
            .navbar-nav > li:nth-child(4) > a { background-color: #94b8d0 !important; }
            .navbar-nav > li:nth-child(5) > a { background-color: #75a2bf !important; }
            .navbar-nav > li:nth-child(6) > a { background-color: #5e8cad !important; }
            .navbar-nav > li:nth-child(7) > a { background-color: #487aa7 !important; }
            .navbar-nav > li:nth-child(8) > a { background-color: #326f9e !important; }
            .navbar-nav > li:nth-child(9) > a { background-color: #295b8a !important; }
            .navbar-nav > li:nth-child(10) > a { background-color: #174978 !important; }
            .navbar-nav > li:nth-child(11) > a { background-color: #003366 !important; }
      
            .clickable-card {
                border: 1px solid #ccc;
                border-radius: 6px;
                padding: 15px;
                margin-bottom: 10px;
                box-shadow: 2px 2px 6px rgba(0,0,0,0.1);
                cursor: pointer;
                background-color: #f9f9f9;
              }
              .card-header {
                font-weight: bold;
                font-size: 20px;
                color: #1c4e80;
              }
              .card-body {
                display: none;
                margin-top: 20px;
                margin-bottom: 20px;
                font-size: 14px;
                color: #333;
              }
              
              .inner-card {
                background-color: #eef5fb;
                border: 1px solid #bdd3eb;
                border-radius: 6px;
                padding: 12px;
                margin-top: 20px;
                margin-bottom: 20px;
              }
              .inner-card h5 {
                margin-top: 0;
                font-size: 15px;
                color: #1c4e80;
              }
      
               .waiter-overlay{
                         backdrop-filter: blur(4px);
                      } 
          "))
   ), # end of tags$head
   tags$script(HTML("
            $(document).on('click', '.clickable-card', function() {
              $(this).find('.card-body').slideToggle('fast');
            });
          ")),
   
   navbarPage(
      
      title = div(
         #tags$img(src = "www/logo.png", height = "30px", style = "display: inline-block; vertical-align: middle;"), ############ UNCOMMENT OUT IF USING DOCKER
         tags$img(src = "logo.png", height = "30px", style = "display: inline-block; vertical-align: middle;"),
         tags$span("RESTful Forensics",
                   style = "font-family: Carme, sans-serif; font-size: 26px; color: #92b2e4; vertical-align: middle; padding-left: 0px;") #,
         #tags$div(
         #   style = "position: fixed; bottom: 0, width: 100%; background-color: transparent; padding: 8px; text-align: center; font-size: 10px; color: #666;",
         #   HTML("&copy; 2025 DNA Analysis Laboratory, Natural Sciences Research Institute, University of the Philippines Diliman. All rights reserved.")
         #)
      ), # end of title
      
      tabPanel(
         title = HTML("<span style = 'color:#000000 ;'>Homepage</span>"),
         
         div(
            class = "card",
            style = "margin: 30px; box-shadow: 0 5 px 10px rgba(0,0,0,0.08); padding: 20px",
            
            div(
               class = "card-header",
               style = "font-size: 20px; font-weight: bold; margin-bottom: 15px;",
               "From the Authors:"
            ),
            
            div(
               class = "card-text",
               style = "font-size: 16px; line-height: 1.6;",
               p("This application is a compilation of the work on ancestry informative markers by the DNA Analysis Laboratory with an ongoing effort to expand to other marker types."),
               br(),
               p("This project is led by Nelvie Fatima Jane Soliven. For inquiries, contact nasoliven@up.edu.ph.")
            )
         )
      ), # end of tab panel for homepage
      
      
      
      ## 1. Instructions Tab
      tabPanel(title = HTML("<span style = 'color:#000000;'>Instructions</span>"),
               h4("Instructions page (typical workflow for marker analysis)"),
               p("Click each section for the specific instructions."),
               fluidPage(
                  
                  div(class = "clickable-card",
                      div(class = "card-header", "🔄 File Conversion"),
                      div(class = "card-body",
                          h4("Convert various genetic data structures to commonly used input files."),
                          
                          div(class = "inner-card",
                              h5("A. Convert files to CSV and add population info"),
                              p("Input file/s:"), 
                              tags$ul(
                                 tags$li("VCF, BCF, or PLINK (.bed, .bim, .fam) files."),
                                 tags$li("(Optional) Population data in XLSX or CSV")                          
                              ),
                              p("Parameter/s: Metadata on sample population"),
                              p("Expected output file/s: CSV file.")
                          ),
                          div(class = "inner-card",
                              h5("B. Convert ForenSeq UAS outputs to wide format"),
                              p("Input file/s: Compressed folder (.zip or .tar) of XLSX files."),
                              p("Expected output file/s: Single CSV file (merged XLSX files)."),
                              br(),
                              p("This section builds upon the work of Ms. Maeviviene Sosing as part of the Filipino Genomes Research Program 2"),
                          ),
                          div(class = "inner-card",
                              h5("C. Convert VCF files to FASTA"),
                              p("Input file/s: VCF file"),
                              p("Expected output file/s: FASTA file.")
                          ), 
                          div(class = "inner-card",
                              h5("D. Convert CSV or XLSX files to a SNIPPER-compatible file"),
                              p("Input file/s: CSV or XLSX file."),
                              p("Parameter/s: (optional) Target population name for classification"),
                              p("Expected output file/s: XLSX file."),
                              p("SNIPPER tool for sample classification: https://mathgene.usc.es/snipper/index.php")
                          ),
                          div(class = "inner-card",
                              h5("E. Convert CSV files to STRUCTURE files."),
                              p("Input file/s: CSV file with marker and population data."),
                              p("Parameter/s: User's operating system (for STRUCTURE input compatibility)"),
                              p("Expected output file/s:"),
                              tags$ul(
                                 tags$li("structure (.str) file"),
                                 tags$li("revised input file")
                              ),
                              br(),
                              p("STRUCTURE generally can't handle sample labels with alphabets, the function would convert sample labels to their associated row number."),
                              p("For users who opt to use STRUCTURE via the terminal or using the GUI, instructions can be found here: https://web.stanford.edu/group/pritchardlab/structure_software/release_versions/v2.3.4/html/structure.html")
                          )
                      )
                  ),
                  ## extraction
                  div(class = "clickable-card",
                      div(class = "card-header", "🧬 SNP Extraction"),
                      div(class = "card-body",
                          h4("Extract markers using PLINK v1.9 from sequenced data and perform concordance analysis."),
                          p("See https://www.cog-genomics.org/plink/ for filtering options."),
                          
                          div(class = "inner-card",
                              h5("A. Extract SNPs based on rsID (marker identification) or GRCh37/GRCh38 position"),
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
                          ),
                          
                          div(class = "inner-card",
                              h5("B. Concordance analysis between files with the same samples"),
                              p("Input file/s: Two CSV or XLSX files."),
                              p("Parameter/s: Indicate if using haplotypes"),
                              p("Expected output/s:"),
                              tags$ul(
                                 tags$li("Concordance table"),
                                 tags$li("Concordance plot")
                              )
                          )
                      )
                  ), # end of div for tab2
                  div(class = "clickable-card",
                      div(class = "card-header", "🔽 Filtering"),
                      div(class = "card-body",
                          h4("Filter individuals and variants using standard options in PLINK 1.9."),
                          p("Standard filtering flags are indicated. For other PLINK flags, see https://www.cog-genomics.org/plink/ for options to be specified in the 'Additional PLINK flags' text box."),
                          br(),
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
                          )
                      )
                  ), # end of div
                  
                  div(class = "clickable-card",
                      div(class = "card-header", "↔️ Multiple Sequence Alignment"),
                      div(class = "card-body",
                          h4("Perform multiple sequence alignment using the msa R package."),
                          p("Post-processing of alignment is performed using the DECIPHER package in R. https://bioconductor.org/packages/devel/bioc/vignettes/DECIPHER/inst/doc/ArtOfAlignmentInR.pdf"),
                          br(),
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
                      )
                  ),
                  
                  div(class = "clickable-card",
                      div(class = "card-header", "🌲 Phylogenetic Tree"),
                      div(class = "card-body",
                          h4("Perform phylogenetic tree reconstruction using ape and phangorn R packages."),
                          p("Approaches to tree construction are NJ, UPGMA, Maximum Parsimony, and Maximum Likelihood. Check the assumptions and constraints of each approach [1]."),
                          br(),
                          p("Input file used is the alignment output from the MSA tab. There is an option of using the raw, adjusted, or staggered alignment for tree construction."),
                          p("Parameters vary based on the method."),
                          p("Expected output is the phylogenetic tree in PNG format."),
                          br(), br(),
                          h5("References"),
                          p("[1] Zuo, Y., Zhang, Z., Zeng, Y., Hu, H., Hao, Y., Huang, S., and Li, B. (2024). Common methods for Phylogenetic Tree Construction and Their Implementation in R. Bioengineering(Basel), 11(5): 480. https://doi.org/10.3390/bioengineering11050480")
                      )
                  ),
                  div(class = "clickable-card",
                      div(class = "card-header", "📑 Barcoding"),
                      div(class = "card-body",
                          p("Perform DNA barcoding using the R package 'BarcodingR'"),
                          div(class = "inner-card",
                              h5("A. Species Identification"),
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
                          div(class = "inner-card",
                              h5("B. Optimize kmer values"),
                              p("Input file/s: Aligned sequences of the reference dataset (FASTA)"),
                              p("Parameter/s: Length of maximum kmer value"),
                              p("Expected output file: Kmer plot"),
                              br(),
                              p("This section builds upon the work of Ms. Maeviviene Sosing as part of the Filipino Genomes Research Program 2"),
                          ),
                          div(class = "inner-card",
                              h5("C. Barcoding Gap"),
                              p("Input file: VCF file"),
                              p("Parameter/s: Distance (raw, K80, euclidean)"),
                              p("Expected output file: Barcoding gap plot")
                          ), 
                          div(class = "inner-card",
                              h5("D. Evaluate Barcodes"),
                              p("Input file/s: CSV or XLSX file."),
                              p("Parameter/s: Length of kmer for barcode 1 and barcode 2 (separate)")
                          ),
                          div(class = "inner-card",
                              h5("E. Species Membership Value (TDR)"),
                              p("Input file/s: CSV file with marker and population data."),
                              p("Parameter/s: Boostrap value for query and reference samples.")
                          )
                      ) # end of card body
                  ),
                  
                  ### POP Stat
                  div(class = "clickable-card",
                      div(class = "card-header", "📝 Population Statistics"),
                      div(class = "card-body",
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
                          p("Input file: CSV or XLSX file"),
                          p("Expected output files:"),
                          tags$ul(
                             tags$li("XLSX file with all results"),
                             tags$li("Heterozygosity Plot"),
                             tags$li("Fst Plots")
                          ),
                          br(), br(),
                          h5("References"),
                          p("[1] Petit, R.J., El Mousadik, A., and Pons, O. (1998). Identifying populations for conservation on the basis of genetic markers. Conservation Biology, 12:844-855"),
                          p("[2] Foulley, J.F., and Ollivier, L. (2005). Estimating allelic richness and its diversity. Livestock Science, 101:150-158. https://doi.org/10.1016/j.livprodsci.2005.10.021"),
                          p("[3] Nei, M. (1978). Estimation of average heterozygosity and genetic distance from a small number of individuals. Genetics:89:583-590. https://doi.org/10.1093/genetics/89.3.583"),
                          p("[4] Rousset, F. (2002). Inbreeding and relatedness coefficients: what do they measure? Heredity, 88:371-380."),
                          p("[5] Rezaei, N., and Hedayat, M. (2013). Allele Frequency. Brenner's Encyclopedia of Genetics (Second Edition). https://doi.org/10.1016/B978-0-12-374984-0.00032-2"),
                          p("[6] Tiret, L., and Cambien, F. (1995). Departure from Hardy-Weinberg equilibrium should be systematically tested in studies of association between genetic markers and disease. Circulation, 92(11):3364-3365."),
                          p("[7] Weir, B.S., and Cockerham, C.C. (1984). Estimating F-statistics for the analysis of population structure. Evolution; International Journal of Organic Evolution, 38(6): 1358-1370. https://doi.org/10.1111/j.1558-5646.1984.tb05657.x")
                      )
                  ), #end of div for popstat
                  
                  div(class = "clickable-card",
                      div(class = "card-header", "🔍 Exploratory Analysis"),
                      div(class = "card-body",
                          h4("Run principal component analysis using the ade4 R package."),
                          br(),
                          p("Input file: CSV or XLSX file and color labels (optional)"),
                          p("Expected output file: PNG plots"))
                  ), # end of div for pca
                  
                  div(class = "clickable-card",
                      div(class = "card-header", "📊 STRUCTURE Analysis"),
                      div(class = "card-body",
                          h4("Generate STRUCTURE input files and pong compatible files. Visualize the possible results"),
                          p("See https://web.stanford.edu/group/pritchardlab/structure_software/release_versions/v2.3.4/structure_doc.pdf for the documentation."),
                          br(),
                          p("Input file: CSV or XLSX file"),
                          p("Expected output file: Zipped qmatrices, individual files, and PNG plots")                          
                      ))
               ) # end of fluidpage
               
      ), # end of tab panel 
      
      ## FILE CONVERSION
      tabPanel(
         title = HTML("<span style = 'color:#ffffff;'>File Conversion</span>"),
         tabsetPanel(
            
            # Subtab 1: Convert to CSV
            tabPanel("Convert files to CSV",
                     useShinyjs(),
                     sidebarLayout(
                        sidebarPanel(
                           radioButtons("inputType", "Choose Input File Type",
                                        choices = c("VCF file" = "vcf", "BCF file" = "bcf", "PLINK files (.bed/.bim/.fam)" = "plink")),
                           
                           conditionalPanel(
                              condition = "input.inputType == 'vcf'",
                              fileInput("vcfFile", "Upload VCF File")
                           ),
                           # added 4 Aug (missed)
                           conditionalPanel(
                              condition = "input.inputType == 'bcf'",
                              fileInput("bcfFile", "Upload BCF File")
                           ),
                           
                           conditionalPanel(
                              condition = "input.inputType == 'plink'",
                              fileInput("bedFile", "Upload BED File"),
                              fileInput("bimFile", "Upload BIM File"),
                              fileInput("famFile", "Upload FAM File")
                           ),
                           
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
                           ),
                           
                           actionButton("convertCSV", "Convert File to CSV", icon = icon("file-csv"))
                           
                        ),
                        mainPanel(
                           tableOutput("previewTable"),
                           fluidRow(
                              column(6,
                                     h5("This is a sample reference file. Only the first two columns (sample and population information) are used."),
                                     tableOutput("exampleRefCSV")),
                              column(6,
                                     tags$h4("Sample File"),
                                     tags$ul(
                                        tags$a("Sample VCF file", href = "www/sample_hgdp.vcf", download = NA)
                                     )  
                              )
                           ), # end of fluidRow
                           downloadButton("downloadConvertedCSV", "Download Converted CSV")
                        ) # end of main panel
                     )
            ),
            
            tabPanel("Widen ForenSeq UAS files",
                     useShinyjs(),
                     sidebarLayout(
                        sidebarPanel(
                           fileInput("uas_zip", "Upload ZIP or TAR file",
                                     accept = c(".zip", ".tar")),
                           helpText("*Accepts compressed files containing XLSX files."),
                           fileInput("ref_file", "Optional Reference File (CSV or XLSX)",
                                     accept = c(".csv", ".xlsx")),
                           actionButton("run_uas2csv", "Run Conversion"),
                           br(), br(),
                           downloadButton("downloadUAScsv", "Download Converted CSV")
                        ),
                        mainPanel(
                           h4("Preview of Output"),
                           tableOutput("previewTableUAS"),
                           fluidRow(
                              column(6,
                                     h5("Sample input file. All alleles of available SNPs per sample are listed in a long format."),
                                     tableOutput("exampleXLSX")),
                              column(6,
                                     tags$h4("Downloadable Sample"),
                                     tags$ul(
                                        tags$a("Sample zipped file", href = "www/sample_forenseq.zip", download = NA)
                                     )  
                              )
                           ) # end of fluidRow
                        ) #end of mainpanel
                     )
            ), #end of tabpanel
            
            tabPanel("VCF to FASTA",
                     useShinyjs(),
                     sidebarLayout(
                        sidebarPanel(
                           fileInput("vcfFile", "Upload VCF file"),
                           fileInput("refFasta", "Upload reference file genome"),
                           actionButton("converttoFasta", "Convert to FASTA", icon = icon("file-invoice"))
                        ), # end of sidebarPanel
                        mainPanel(
                           verbatimTextOutput("fastaPreview"),
                           conditionalPanel(
                              condition = "output.readyFasta",
                              downloadButton("downloadFasta", "Download FASTA file")
                           )
                        ) # end of mainpanel
                     ) # end of sidebarLayout
                     
            ), # end of tabPanel for VCF to fasta
            
            tabPanel("Convert to SNIPPER-analysis ready file",
                     useShinyjs(),
                     sidebarLayout(
                        sidebarPanel(
                           fileInput("convertFile", "Upload File"),
                           helpText("*Accepts VCF, XLSX, and CSV files"),
                           fileInput("refFile", "Upload Reference File"),
                           helpText("*Accepts XLSX and CSV files"),
                           
                           checkboxInput("targetPop", "Subset Target Population?", value = FALSE),
                           textInput("targetPopName", "Target Population Name"),
                           actionButton("convertBtn", "Convert Format", icon = icon("arrow-up-right-from-square"))
                        ),
                        mainPanel(
                           h4("Preview of Converted SNIPPER Data"),
                           tableOutput("previewTableSNIPPER"),
                           fluidRow(
                              column(6,
                                     h5("Sample input file."),
                                     tableOutput("exampleTableSnipper")),
                              column(6, 
                                     h5("Sample reference file"),
                                     tableOutput("exampleRefSnipper")),
                              column(6,
                                     tags$h4("Downloadable Sample"),
                                     tags$ul(
                                        tags$a("Sample file", href = "www/sample.csv", download = NA)
                                     )  
                              )
                           ) # end of fluidRow
                        ) # end of mainpanel
                     )
            ),
            
            tabPanel("CSV to STRUCTURE file",
                     useShinyjs(),
                     sidebarLayout(
                        sidebarPanel(
                           fileInput("tostrFile", "Upload CSV file"),
                           helpText("Use the 'Convert files to CSV' file if using VCF, BCF, or PLINK files. Population data is necessary."),
                           radioButtons("systemFile", "Choose the operating system where STRUCTURE v2.3.4 is installed",
                                        choices = c("Linux" = "Linux", "Windows" = "Windows")),
                           actionButton("csv2str", "Generate STRUCTURE File", icon = icon("arrow-up-right-from-square"))   
                        ),
                        mainPanel(
                           h4("Preview of output files"),
                           fluidRow(
                              column(6,
                                     h5("Revised input file"),
                                     tableOutput("revisedCSV") # REVISE
                              ),
                              column(6,
                                     h5("Preview of str file"),
                                     tableOutput("strFile") # REVISE
                              )
                           ),
                           br(), br(),
                           downloadButton("downloadrevised", "Download Revised CSV file"),
                           downloadButton("downloadSTRfile", "Download STR file")
                           
                        ) # end of mainpanel
                     )
            ) # end of tabpanel for csv to structure file
            
         )
      ), # end of tabpanel
      
      ## MARKER EXTRACTION
      tabPanel(title = HTML("<span style = 'color:#ffffff;'>SNP Extraction</span>"),
               tabsetPanel(
                  tabPanel("SNP Extraction",
                           useShinyjs(),
                           sidebarLayout(
                              sidebarPanel(
                                 tabPanel("Marker Extraction",
                                          fluidPage(
                                             fileInput("markerFile", "Upload Genotype (VCF, BCF or PLINK) File"),
                                             
                                             radioButtons("markerType", "Choose Marker Type",
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
                                                fileInput("markerList2", "Upload POS List (.csv, .xlsx)")
                                             ),
                                             
                                             fileInput("bedFile", "PLINK BED file (optional)"),
                                             fileInput("bimFile", "PLINK BIM file (optional)"),
                                             fileInput("famFile", "PLINK FAM file (optional)"),
                                             textAreaInput("plink_args",
                                                           label = "Additional PLINK Arguments",
                                                           placeholder = "--maf 0.05 --geno 0.1",
                                                           rows = 3,
                                                           width = "100%"),
                                             helpText("See https://www.cog-genomics.org/plink/ for options."),
                                             
                                             actionButton("extractBtn", "Run Marker Extraction", icon = icon("play"))
                                             
                                          )
                                 )
                              ),
                              mainPanel(
                                 h4("Example Input Formats"),
                                 fluidRow(
                                    column(6,
                                           h5("rsID Format"),
                                           tableOutput("exampleRSID")
                                    ),
                                    column(6,
                                           h5("Position Format"),
                                           tableOutput("examplePOS")
                                    )
                                 ),
                                 downloadButton("downloadExtracted", "Download Extracted VCF"),
                                 helpText("Note: Some systems mislabel .vcf files as contact files. This is a reminder that the file is a genomic VCF.")
                              ) # end of mainpanel
                           )
                  ),
                  tabPanel(HTML("<span style = 'color:#000000;'>Concordance Analysis</span>"),
                           useShinyjs(),
                           sidebarLayout(
                              sidebarPanel(
                                 fileInput("concordanceFile1", "Upload File A"),
                                 fileInput("concordanceFile2", "Upload File B"),
                                 checkboxInput("isHaplotype", "Treat data as haplotypes", value = FALSE),
                                 actionButton("compareBtn", "Run Concordance Analysis", icon = icon("play"))
                              ),
                              mainPanel(
                                 h4("Example Input Formats"),
                                 fluidRow(
                                    column(6,
                                           h5("File Format (for concordance)"),
                                           tableOutput("exampleTable")
                                    )
                                 ),
                                 hr(),
                                 h4("Concordance Summary Table"),
                                 tableOutput("concordanceResults"),
                                 hr(),
                                 h4("Concordance Plot"),
                                 plotOutput("concordancePlot", height = "600px"),
                                 hr(),
                                 downloadButton("downloadConcordance", "Download Concordance Results"),
                                 downloadButton("downloadConcordancePlot", "Download Plot")
                              )
                           )
                  )
               )
      ), # end of tabpanel
      
      #############
      # FILTERING #
      #############
      
      tabPanel(HTML("<span style = 'color:#ffffff;'> Filtering </span>"),
               sidebarLayout(
                  sidebarPanel(
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
                  ), # end of side panel
                  mainPanel(
                     verbatimTextOutput("plinkCommandPreview"),
                     h4("Depth of Coverage Plots"),
                     imageOutput("depthMarkerPlot"),
                     imageOutput("depthSamplePlot"),
                     br(),
                     downloadButton("downloadFilteredFile", "Download Filtered File"),
                     downloadButton("downloadDepthPlots", "Download Plots")
                  ) # end of mainPanel
                  
               ) # sidebar layout
      ), # end of tab panel for filtering
      
      ###########
      # ADD MSA #
      ###########
      
      tabPanel(HTML("<span style = 'color:#ffffff;'> Multiple Sequence Alignment</span>"),
               sidebarLayout(
                  sidebarPanel(
                     #useShinyjs(),
                     fileInput("fastaFile", "Upload zipped FASTA files"),
                     radioButtons("substitutionMatrix", "Choose Substitution Matrix for MSA",
                                  choices = c("ClustalW" = "ClustalW", "ClustalOmega" = "ClustalOmega", "MUSCLE" = "Muscle")),
                     br(),
                     actionButton("runMSA", "Align", icon = icon("align-justify")),
                     selectInput("msaDownloadType", "Choose alignment (FASTA and PDF) version to download:",
                                 choices = c(
                                    "Initial" = "initial",
                                    "Adjusted" = "adjusted",
                                    "Staggered" = "staggered"
                                 ),
                                 selected = "initial"),
                     downloadButton("downloadAlignedFASTA", "Download Aligned Sequences"),
                     br(), br(),
                     downloadButton("downloadAlignmentScores", "Download Alignment Scores"),
                     br(), br(),
                     downloadButton("downloadAlignmentPDF", "Download Alignment PDF")
                     
                  ), #tabPanel
                  mainPanel(
                     verbatimTextOutput("initialAlignmentText"),
                     br(),
                     verbatimTextOutput("adjustedAlignmentText"),
                     br(),
                     verbatimTextOutput("staggeredAlignmentText"),
                     br(),
                     verbatimTextOutput("alignmentScoresPreview")
                  ) # end of mainPanel
               ) # end of sidebarLayout
               
      ), # end of tabpanel for MSA
      
      # Phylogenetic Tree Construction
      tabPanel(HTML("<span style='color:#ffffff;'>Phylogenetic Tree</span>"),
               sidebarLayout(
                  sidebarPanel(
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
                     actionButton("buildTree", "Build Tree", icon = icon("tree")),
                     br(), br(),
                     downloadButton("downloadTree", "Download Tree"),
                     downloadButton("downloadAll", "Download All Outputs")
                  ),
                  
                  mainPanel(
                     h4("Aligned Sequences Preview"),
                     verbatimTextOutput("treeAlignmentPreview"),
                     br(),
                     h4("Phylogenetic Tree"),
                     uiOutput("treeImage")
                  )
                  
               ) # end of sidebar layout
      ), # end of tab panel
      
      
      #############
      # BARCODING #
      #############   
      tabPanel(HTML("<span style = 'color:#ffffff;'>Barcoding</span>"),
               tabsetPanel(
                  tabPanel("Species Identification",
                           sidebarPanel(
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
                           ), # end of sidebar Panel
                           mainPanel(
                              verbatimTextOutput("identificationResult")
                           )
                  ), # end of first tab Panel
                  tabPanel("Optimize kmer values",
                           sidebarPanel(
                              p("Calculate the optimal kmer value that can be used for relevant calculations."),
                              fileInput("optimizeKmerRef", "Upload reference dataset"),
                              numericInput("maxKmer", "Length of maximum kmer value", value = "5", min = 2),
                              actionButton("calOptimumKmer", "Identify Optimum kmer value", icon = icon("upload"))
                           ), # end of sidebar panel
                           mainPanel(
                              verbatimTextOutput("kmerResult"),
                              imageOutput("kmerPlot"),
                              downloadHandler("downloadKmerPlot", "Download Plot")
                           )
                           
                  ), # end of second tab panel
                  tabPanel("Barcoding Gap",
                           sidebarPanel(
                              fileInput("barcodeRef", "Upload reference dataset"),
                              selectInput("gapModel", "Choose Distance",
                                          choices = c("raw", "K80", "euclidean"),
                                          selected = "raw"),
                              actionButton("gapBarcodes", "Calculate gap", icon = icon("arrows-left-right-to-line"))
                              
                           ), # end of sidebar panel
                           mainPanel(
                              verbatimTextOutput("barcodingResult"),
                              imageOutput("BarcodingGapPlot"),
                              downloadHandler("downloadGapPlot", "Download Barcoding Gap Plot")
                           )
                  ), #end of third tab panel
                  tabPanel("Evaluate Barcodes",
                           sidebarPanel(
                              fileInput("barcode1", "Upload Barcode 1"),
                              fileInput("barcode2", "Upload Barcode 2"),
                              numericInput("kmer1", "Length of kmer for barcode 1", value = 5, min = 1),
                              numericInput("kmer2", "Length of kmer for barcode 2", value = 5, min = 1),
                              actionButton("evalBarcodes", "Evaluate Barcodes", icon = icon("code-compare"))
                           ), # end of sidebar panel
                           mainPanel(
                              tableOutput("evalBarcodesResult")
                           )
                  ), #end of third tab panel
                  tabPanel("Species Membership Value (TDR)",
                           sidebarPanel(
                              p("Calculate the TDR2 value"),
                              fileInput("oneSpe", "Upload DNA seq from a single query species"),
                              fileInput("queSpe", "Upload DNA seq from different samples"),
                              numericInput("bootValue1", "Bootstrap value for query species", value = 10, min = 1),
                              numericInput("bootValue2", "Bootstrap value for reference samples", value = 10, min = 1),
                              actionButton("calculateTDR2", "Calculate", icon = icon("calculator"))
                           ), # end of sidebar panel
                           mainPanel(
                              verbatimTextOutput("tdrValues")
                           )
                  ) # end of fourth tab panel
               ) # end of first tabset panel
      ), # end of tabpanel
      
      #########################
      # Population Statistics #
      #########################
      
      tabPanel(HTML("<span style = 'color:#ffffff;'>Population Statistics</span>"),
               tabsetPanel(
                  sidebarPanel("Perform Analysis",
                               useShinyjs(),
                               fileInput("popStatsFile", "Upload CSV or XLSX Dataset"),
                               actionButton("runPopStats", "Analyze", icon = icon("magnifying-glass-chart")),
                               downloadButton("downloadStatsXLSX", "Download Results (Excel)"),
                               
                               hr(),
                               h4("Example: Population File Format"),
                               tableOutput("examplePop"),
                               tags$h4("Sample File"),
                               tags$ul(
                                  tags$a("Sample file", href = "www/sample.csv", download = NA)
                               )
                  ), 
                  tabPanel("1 Private Alleles",
                           h4("Private Alleles Summary"),
                           uiOutput("privateAllelePlot")
                  ),
                  tabPanel("2 Mean Allelic Richness",
                           h4("Mean Allelic Richness per site"),
                           DT::dataTableOutput("meanallelic")
                  ),
                  tabPanel("3 Heterozygosity",
                           h4("Observed vs Expected Heterozygosity"),
                           DT::dataTableOutput("heterozygosity_table"),
                           hr(),
                           h4("Heterozygosity Plot"),
                           imageOutput("heterozygosity_plot"),
                           downloadButton("downloadHeterozygosityPlot", "Download Plot")
                  ),
                  tabPanel("4 Inbreeding Coefficients",
                           h4("Inbreeding Coefficient by Population"),
                           DT::dataTableOutput("inbreeding_table")
                  ),
                  tabPanel("5 Allele Frequencies",
                           h4("Allele Frequency Table"),
                           DT::dataTableOutput("allele_freq_table")
                  ),
                  tabPanel("6 Hardy-Weinberg Equilibrium",
                           h4("HWE P-value Summary"),
                           uiOutput("hwe_summary"),
                           h4("Population-wise HWE Chi-Square Table"),
                           DT::dataTableOutput("hwe_chisq_table")
                  ),
                  tabPanel("7 Fst Values",
                           h4("Pairwise Fst Matrix"),
                           uiOutput("fstMatrixUI"),  
                           h4("Tidy Pairwise Fst Data"),
                           DT::dataTableOutput("fstDfTable"),
                           hr(),
                           h4("Fst Heatmap"),
                           imageOutput("fst_heatmap_plot", width = "100%") 
                           
                  )
               )
      ), # end of tabpanel
      
      #######
      # PCA #
      #######
      
      tabPanel(HTML("<span style = 'color:#ffffff;'>Exploratory Analysis</span>"),
               sidebarLayout(
                  sidebarPanel(
                     fileInput("pcaFile", "Upload SNP Data (in CSV or XLSX) for PCA", accept = c(".csv", ".txt")),
                     checkboxInput("useDefaultColors", "Use Default Colors and Labels", TRUE),
                     conditionalPanel(
                        condition = "!input.useDefaultColors",
                        fileInput("pcaLabels", "Upload PCA Labels"),
                        fileInput("colorPalette", "Upload Color Palette")
                     ),
                     numericInput("pcX", "PC Axis X", value = 1, min = 1),
                     numericInput("pcY", "PC Axis Y", value = 2, min = 1),
                     actionButton("runPCA", "Run PCA Analysis", icon = icon("play"))
                  ),
                  mainPanel(
                     h4("Example: PCA Input Format"),
                     tableOutput("examplePCA"),
                     tags$h4("Sample File"),
                     tags$ul(
                        tags$a("Sample file", href = "www/sample.csv", download = NA)
                     ),
                     
                     hr(),
                     plotOutput("barPlot"),
                     downloadButton("downloadbarPlot", "Download Bar Plot"),
                     hr(),
                     plotOutput("pcaPlot"),
                     downloadButton("downloadPCAPlot", "Download PCA Plot")
                     #hr(),
                  )
               )
      ), # end of tabpanel
      
      ######################
      # STRUCTURE Analysis #
      ######################
      
      tabPanel(
         HTML("<span style='color:#ffffff;'>STRUCTURE Analysis</span>"),
         sidebarLayout(
            sidebarPanel(
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
               actionButton("runStructure", "Run STRUCTURE", icon = icon("play")),
               uiOutput("downloadButtons")
            ),
            mainPanel(
               tags$h4("Download Sample File"),
               tags$ul(
                  tags$a("Sample file", href = "www/sample.csv", download = NA)
               ),
               h4("STRUCTURE Visualization"),
               imageOutput("structurePlotPreview"),
               br(),
               p("NOTE: The plots are expected to take some time to load."),
               br(), br(),
               p("Some functions were revised and adapted from the strataG and dartR packages such as 'gl.run.structure', '.structureParseQmat', 'structureRead', and 'utils.structure.evanno'"),
               h3("References"),
               p("Archer, F., Adams, P., and Schneiders, B. (2016). strataG: an R package for manipulating, summarizing, and analyzing population genetic data. Molecular Ecology Resources 17: 5-11. https://doi.org/10.1111/1755-0998.12559"),
               p("Mijangos, J.L., Gruber, B., Berry, O., Pacioni, C., and Georges, A. (2022). dartR v2: An accessible genetic analysis platform for conservation, ecology, and agriculture. Methods in Ecology and Evolution. https://doi.org/10.1111/2041-210X.13918"),
               p("Gruber, B., Unmack, P.J., Berry, O.F., and Georges, A. (2018). dartR: an R package to facilitate analysis of SNP data generated from reduced representation genome sequencing. Molecular Ecology Resources 18:691-699. https://doi.org/10.1111/1755-0998.12745"),
               p("Jenkins, T. (2018). R function to export a genind object in STRUCTURE format. [Source code]. GitHub. https://raw.githubusercontent.com/Tom-Jenkins/utility_scripts/master/TJ_genind2structure_function.R")
               
            ) # end of mainpanel
         )
      ), #end of tabpanel
      p("© 2025 DNA Analysis Laboratory, Natural Sciences Research Institute, University of the Philippines Diliman. All rights reserved."),
      div(
         style = "position: fixed; bottom: 0, width: 100%; background-color: transparent; padding: 8px; text-align: center; font-size: 10px; color: #666;"
      )
   )
)
# TO ADD
server <- function(input, output, session) {
   
   #lastAction <- reactiveVal(Sys.time())
   
   # timer
   #observe({
   #invalidateLater(600000, session)  # 5 minutes
   
   # set cleanup time by 5 mins
   #   if (difftime(Sys.time(), lastAction(), units = "secs") > 300) {
   #      # resets
   #      convertedCSV
   #      convertedSNIPPER
   #      convertedUAS
   #      concordanceResult(NULL)
   #      concordancePlotPath(NULL)
   #      fsnps_gen(NULL)
   #      population_stats(NULL)
   #      hardy_weinberg_stats(NULL)
   #      fst_stats(NULL)
   
   # this would reset the ui
   #      shinyjs::reset("formPanel")
   
   # to clean the files
   #      unlink(tempdir(), recursive = TRUE)
   
   #      showNotification("Session cleaned due to inactivity.", type = "message")
   #   }
   #})
   
   
   ## Store plots for viewing and downloading
   plots <- reactiveValues(pca = NULL, het = NULL, fst = NULL)
   status <- reactiveVal("Waiting for input...")
   
   # FILE CONVERSION #
   convertedCSV <- reactiveVal(NULL)
   
   output$exampleRefCSV <- renderTable({
      data.frame(
         Sample.Name = c("sample1", "sample2", "sample3", "sample4", "..."),
         Population = c("Malaysia", "Mexico", "Greece", "South Korea", "..."),
         Superpopulation = c("Southeast Asia", "North and South America", "Europe", "East Asia", "...")
      )
   })
   
   observe({
      file_ready <- FALSE
      pop_ready <- FALSE
      
      # Check file inputs
      if (input$inputType == "vcf") {
         file_ready <- !is.null(input$vcfFile)
      } else if (input$inputType == "plink") {
         file_ready <- !is.null(input$bedFile) &&
            !is.null(input$bimFile) &&
            !is.null(input$famFile)
      } else if (input$inputType == "bcf") {
         file_ready <- !is.null(input$bcfFile)
      }
      
      # Check population input
      if (input$poptype == "multiplepop") {
         pop_ready <- !is.null(input$multiplepop)
      } else if (input$poptype == "single") {
         pop_ready <- nchar(trimws(input$typePop)) > 0
      }
      
      # Enable or disable convert button
      if (file_ready && pop_ready) {
         shinyjs::enable("convertCSV")
      } else {
         shinyjs::disable("convertCSV")
      }
   })
   
   observeEvent(input$convertCSV, {
      req(input$convertCSV)
      
      #lastAction(Sys.time())
      
      disable("convertCSV")
      waiter_show(html = spin_fading_circles(), color = "#ffffff")
      
      outputDir <- tempdir()
      timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
      outputName <- paste0("converted_", timestamp, ".csv")
      
      
      # Set reference input
      refValue <- if (input$poptype == "multiplepop") {
         input$multiplepop$datapath
      } else {
         input$typePop
      }
      
      if (input$inputType == "vcf") {
         ext <- tools::file_ext(input$vcfFile$name)
         if (ext %in% c("vcf", "gz")) {
            vcfPath <- input$vcfFile$datapath
            
            csv_result <- vcftocsv(vcf = vcfPath, ref = refValue)
            convertedCSV(csv_result)
         } else {
            showNotification("Unsupported VCF format. Please upload a .vcf or .vcf.gz file.", type = "error")
            enable("convertCSV")
            return()
         }
         
      } else if (input$inputType == "bcf") {
         temp_output <- file.path(outputDir, "bcftovcf")
         command <- stringr::str_c(
            plink_path, " --bcf ", input$bcfFile$datapath,
            " --const-fid 0 --cow --keep-allele-order --allow-no-sex --allow-extra-chr",
            " --recode vcf --out ", temp_output
         )
         
         exit_code <- system(command)
         if (exit_code != 0){
            showNotification("Plink conversion failed", type = "error")
            enable("convertCSV")
            return()
         }
         
         vcfPath <- paste0(temp_output, ".vcf")
         csv_result <- vcftocsv(vcf = vcfPath, ref = refValue)
         convertedCSV(csv_result)
         
      } else if (input$inputType == "plink") {
         bed <- input$bedFile$datapath
         bim <- input$bimFile$datapath
         fam <- input$famFile$datapath
         
         outputVCF <- file.path(outputDir, "plink2vcf")
         command <- stringr::str_c(
            plink_path, " --bed ", bed,
            " --bim ", bim, " --fam ", fam,
            " --const-fid 0 --cow --keep-allele-order --allow-no-sex --allow-extra-chr",
            " --recode vcf --out ", outputVCF
         )
         
         exit_code <- system(command)
         if (exit_code != 0){
            showNotification("Plink conversion failed", type = "error")
            enable("convertCSV")
            return()
         }
         
         vcfPath <- paste0(outputVCF, ".vcf")
         csv_result <- vcftocsv(vcf = vcfPath, ref = refValue)
         convertedCSV(csv_result)
      }
      
      enable("convertCSV")
      
      output$downloadConvertedCSV <- downloadHandler(
         filename = function() { outputName },
         content = function(file) {
            readr::write_csv(convertedCSV(), file)
         }
      )
      
      output$previewTable <- renderTable({
         req(convertedCSV())
         head(convertedCSV(), 10)
      })
      
      waiter_hide()
   }) # end of observeEvent
   
   
   # CONVERT TO FASTA #
   
   observe({
      fasta_ready <- !is.null(input$vcfFile)
      shinyjs::toggleState("converttoFasta", condition = fasta_ready)
   })
   
   observeEvent(input$converttoFasta, {
      req(input$vcfFile, input$refFasta)
      
      disable("converttoFasta")
      temp_dir <- tempdir()
      
      waiter_show(html = spin_fading_circles(), color = "#ffffff")
      
      fasta_path <- vcf_to_fasta(
         vcf_file = input$vcfFile$datapath,
         reference = input$refFasta$datapath,
         bcftools_path = bcftools_path,
         directory = temp_dir
      )
      
      output$downloadFasta <- downloadHandler(
         filename = function() { "consensus.fa"},
         content = function(file){
            file.copy(fasta_path, file)
         }
      )
      
      output$readyFasta <- reactive({ file.exists(fasta_path)})
      outputOptions(output, "readyFasta", suspendWhenHidden = FALSE)
      
      output$fastaPreview <- renderText({
         req(file.exists(fasta_path))
         paste(readLines(fasta_path, n = 10), collapse = "\n")
      })
      
      shinyjs::enable("converttoFasta")
      waiter_hide()
   }) # end of observe event convert to FASTA
   
   
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
   
   observe({
      hasFile <- !is.null(input$convertFile)
      hasRef <- !is.null(input$refFile)
      
      if (input$targetPop) {
         hasTarget <- nchar(trimws(input$targetPopName)) > 0
      } else {
         hasTarget <- TRUE  # Only required if checkbox is checked
      }
      
      ready <- hasFile && hasRef && hasTarget
      
      if (ready) {
         shinyjs::enable("convertBtn")
      } else {
         shinyjs::disable("convertBtn")
      }
   })
   
   observeEvent(input$convertBtn, {
      #lastAction(Sys.time())
      
      disable("convertBtn")
      waiter_show(html = spin_fading_circles(), color = "#ffffff")
      
      inputPath <- input$convertFile$datapath
      refPath <- input$refFile$datapath
      targetSet <- input$targetPop
      targetName <- if (targetSet) input$targetPopName else NULL
      
      inputData <- read.csv(inputPath, header = TRUE)
      numMarkers <- ncol(inputData) - 2
      
      outputName <- "snipper.xlsx"
      
      withProgress(message = "Converting to SNIPPER-analysis ready file...", value = 0, {
         
         snipper.file <- tryCatch({
            
            tosnipper(input = inputPath,
                      references = refPath,
                      target.pop = targetSet,
                      population.name = targetName,
                      markers = numMarkers)
            
            waiter_hide()
         }, error = function(e){
            showNotification(paste("Conversion failed:", e$message), type = "error")
            NULL
         })
         
         if (!is.null(snipper.file)) {
            convertedSNIPPER(snipper.file)
            enable("convertBtn")
            
            output$downloadConverted <- downloadHandler(
               filename = function() { outputName },
               content = function(file) {
                  openxlsx::write.xlsx(convertedSNIPPER(), file)
               }
            )
            
            output$previewTableSNIPPER <- renderTable({
               req(convertedSNIPPER())
               head(convertedSNIPPER(), 10)
            })
         }
         
      })
      
   }) # end of observe Event
   
   
   
   ### UAS to CSV
   convertedUAS <- reactiveVal(NULL)
   
   output$exampleXLSX <- renderTable({
      data.frame(
         Sample.Name = c("sample1","sample1", "sample1", "sample1", "sample1", "sample1", "sample2", "sample3", "sample3", "sample3", "..."),
         Locus = c("rs01", "rs01", "rs02", "rs02", "rs02", "rs03", "rs01", "rs01", "rs02", "rs03", "..."),
         Allele = c("A", "T", "C", "A", "G", "T", "A", "T", "G", "A", "...")
      )
   })
   
   observe({
      zip_ready <- !is.null(input$uas_zip)
      shinyjs::toggleState("run_uas2csv", condition = zip_ready)
   })
   
   observeEvent(input$run_uas2csv, {
      #lastAction(Sys.time())
      
      disable("run_uas2csv")
      waiter_show(html = spin_fading_circles(), color = "#ffffff")
      
      req(input$uas_zip)
      
      temp_dir <- tempdir()
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
            widened.file <- uas2csv(files = input_path,
                                    population = ref_value,
                                    reference = use_reference,
                                    dir = temp_dir)
            convertedUAS(widened.file)
            
            enable("run_uas2csv")
            
            outputName <- "01_merged_typed_data.csv"
            
            output$downloadUAScsv <- downloadHandler(
               filename = function() {
                  outputName
               },
               content = function(file) {
                  readr::write_csv(convertedUAS(), file)
               }
            )
            
            output$previewTableUAS <- renderTable({
               req(convertedUAS())
               head(convertedUAS(), 10)  # Preview top 10 rows
            })
            
            waiter_hide()
            showNotification("Conversion complete!", type = "message")
         }, error = function(e) {
            showNotification(paste("Error:", e$message), type = "error")
            enable("run_uas2csv")
         })
      })
      
   }) # end of observe Event
   
   
   
   # CSV TO STR #
   shinyjs::disable("downloadrevised")
   shinyjs::disable("downloadSTRfile")
   
   observe({
      toggleState("csv2str", !is.null(input$tostrFile) && !is.null(input$systemFile))
   })
   
   csv_revised <- reactiveVal(NULL)
   strconvert <- reactiveVal(NULL)
   str_file <- reactiveVal(NULL)
   
   observeEvent(input$csv2str, {
      disable("csv2str")
      shinyjs::disable("downloadrevised")
      shinyjs::disable("downloadSTRfile")
      waiter_show(html = spin_fading_circles(), color = "#ffffff")
      
      withProgress(message = "Analyzing files...", value = 0, {
         tryCatch({
            req(input$tostrFile$datapath, input$systemFile)
            
            csv_file <- load_input_file(input$tostrFile$datapath)
            genind <- convert_to_genind_str(csv_file)
            csv_revised(genind$new_file)
            strconvert(genind$fsnps_gen)
            
            directory <- tempdir()
            str_path <- to_structure_file(strconvert(), directory, system = input$systemFile)
            str_file(str_path)
            
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
            
            shinyjs::enable("downloadrevised")
            shinyjs::enable("downloadSTRfile")
            
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
                  file.copy(str_path, file)
               }
            )
         }, error = function(e) {
            showNotification(paste("Error during STRUCTURE conversion", e$message), type = "error", duration = 20)
         }, finally = {
            enable("csv2str")
            waiter_hide()
         }) # end of try catch 
      })
   })
   
   
   ## Concordance Analysis
   observe({
      toggleState("compareBtn", !is.null(input$concordanceFile1) && !is.null(input$concordanceFile2))
   })
   
   concordanceResult <- reactiveVal(NULL)
   concordancePlotPath <- reactiveVal(NULL)
   
   observeEvent(input$compareBtn, {
      #lastAction(Sys.time())
      
      disable("compareBtn")
      waiter_show(html = spin_fading_circles(), color = "#ffffff")
      
      withProgress(message = "Analyzing files...", value = 0, {
         tryCatch({
            
            req(input$concordanceFile1$datapath, input$concordanceFile2$datapath)
            
            haplo_flag <- input$isHaplotype
            file1_path <- input$concordanceFile1$datapath
            file2_path <- input$concordanceFile2$datapath
            
            result <- concordance(file1_path, file2_path, haplotypes = haplo_flag)
            
            enable("compareBtn")
            
            concordanceResult(result$results)
            concordancePlotPath(result$plot)
            
            output$concordanceResults <- renderTable({
               req(concordanceResult())
               concordanceResult()
            })
            
            output$concordancePlot <- renderPlot({
               req(concordancePlotPath())
               concordancePlotPath()
            })
            
            showNotification("Concordance analysis complete, rendering outputs.", type = "message", duration = 30)
            print(Sys.time())
            
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
            
            waiter_hide()
         }, error = function(e) {
            showNotification(paste("Error during analysis:", e$message), type = "error", duration = 10)
         })
      }) # end of withprogress
      
   }) # end of observe event
   
   ## MARKER EXTRACTION
   output$exampleRSID <- renderTable({
      data.frame(
         rsID = c("rs101", "rs102", "rs103", "rs104", "...")
      )
   })
   
   output$examplePOS <- renderTable({
      data.frame(
         Chromosome = c("1", "2", "..."),
         Start_BP = c("104500", "205300", "..."),
         End_BP = c("104700", "205700", "...")
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
   shinyjs::disable("downloadExtracted")
   observe({
      isFileUploaded <- !is.null(input$markerFile)
      
      isRSIDReady <- input$markerType == "rsid" && (
         (input$rsidInputType == "manual" && nzchar(input$typedRSIDs)) ||
            (input$rsidInputType == "upload" && !is.null(input$markerList1))
      )
      
      isPOSReady <- input$markerType == "pos" && !is.null(input$markerList2)
      
      toggleState("extractBtn", isFileUploaded && (isRSIDReady || isPOSReady))
   })
   
   
   observeEvent(input$extractBtn, {
      disable("extractBtn")
      req(input$markerFile)
      
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
               ext <- tools::file_ext(input$markerList2$name)
               if (ext == "csv") read.csv(input$markerList2$datapath, header = FALSE)
               else if (ext %in% c("xlsx", "xls")) readxl::read_excel(input$markerList2$datapath, col_names = FALSE)
               else {
                  showNotification("Invalid file type", type = "error")
                  return(NULL)
               }
            } else NULL
            
            
            # load other parameters
            plink_args <- if (!is.null(input$plink_args) && nzchar(input$plink_args)) {
               strsplit(input$plink_args, "\\s+")[[1]]
            } else NULL
            
            
            temp_dir <- tempdir()
            
            extracted_markers <- extract_markers(
               input.file  = input$markerFile$datapath,
               snps.list = snps_list,
               pos.list = pos_list,
               bed.file = input$bedFile$datapath,
               bim.file = input$bimFile$datapath,
               fam.file = input$famFile$datapath,
               plink_args = plink_args,
               output.dir  = temp_dir,
               merged.file = "final_merged.vcf",
               plink_path  = plink_path
            )
            
            extracted_file(extracted_markers)
            showNotification("VCF file successfully extracted and ready for download!", type = "message")
            enable("extractBtn")
            shinyjs::enable("downloadExtracted")
            waiter_hide()
         }, error = function(e) {
            showNotification(paste("Error:", e$message), type = "error")
            enable("extractBtn")
         })
         
      })
      
   })
   output$downloadExtracted <- downloadHandler(
      filename = function() { "final_merged.zip" },
      content = function(file) {
         req(extracted_file())
         file.copy(extracted_file(), file)
      },
      contentType = "text/plain"
   )
   
   #############
   # Filtering #
   #############
   shinyjs::disable("downloadDepthPlots")
   shinyjs::disable("downloadFilteredFile")
   
   # enable run button only if there's input files and at least one filter
   observe({
      hasFile <- !is.null(input$forFilter)
      anyFilter <- input$filterIndiv || input$filterVariant || input$filterAllele || input$filterQuality || input$filterHWE || input$filterLD
      shinyjs::toggleState("calcDP", condition = hasFile && anyFilter)
   })
   
   output$filterWarning <- renderText({
      if (is.null(input$forFilter)){
         "Please upload a VCF file."
      } else if (!(input$filterIndiv || input$filterVariant || input$filterAllele || input$filterQuality || input$filterHWE || input$filterLD)){
         "Please select at least one filtering option."
      } else {
         ""
      }
   })
   
   # reset buttons with new file
   observeEvent(input$forFilter, {
      shinyjs::disable("downloadDepthPlots")
      shinyjs::disable("downloadFilteredFile")
      
      ext <- tools::file_ext(input$forFilter$name)
      if (tolower(ext) == "vcf"){
         shinyjs::enable("enableDp")
      } else {
         updateCheckboxInput(inputId = "enableDP", value = FALSE)
         shinyjs::disable("enableDP")
      }
   })
   
   depth_outputs <- reactiveVal(NULL)
   filtered_plink_file <- reactiveVal(NULL)
   
   observeEvent(input$calcDP, {
      req(input$forFilter)
      temp_dir <- tempdir()
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
         shinyjs::enable("downloadDepthPlots")
      }
      
      # Revised 14 November 2025 to first convert files to PLINK before filtering
      input_file <- convert_to_plink(input$forFilter$datapath, temp_dir)
      
      # for plink filtering
      plink_cmds <- c("plink", "--bfile", shQuote(input_file), "--out", file.path(temp_dir, "filtered"))
      
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
   
   #######
   # MSA #
   #######
   
   shinyjs::disable("downloadAlignedFASTA")
   shinyjs::disable("downloadAlignmentScores")
   shinyjs::disable("downloadAlignmentPDF")
   shinyjs::disable("downloadTree")
   shinyjs::disable("downloadAll")
   
   fasta_data <- reactiveVal(NULL)
   aligned_data <- reactiveVal(NULL)
   alignment_scores <- reactiveVal(NULL)
   alignment_pdf <- reactiveVal(NULL)
   alignment_adjusted <- reactiveVal(NULL)
   alignment_staggered <- reactiveVal(NULL)
   directory <- tempdir()
   
   observeEvent(input$fastaFILE, {
      shinyjs::disable("downloadAlignedFASTA")
      shinyjs::disable("downloadAlignmentScores")
      shinyjs::disable("downloadAlignmentPDF")
   })
   
   observeEvent(input$runMSA, {
      req(input$fastaFile)
      
      fasta_data(read_fasta(input$fastaFile$datapath, directory = directory))
      aligned <- msa_results(fasta_data(), algorithm = input$substitutionMatrix, directory = directory)
      aligned_data(aligned$alignment)
      alignment_scores(aligned$scores)
      alignment_pdf(aligned$pdf)
      alignment_adjusted(aligned$aligned_adjusted)
      alignment_staggered(aligned$aligned_staggered)
      
      shinyjs::enable("downloadAlignedFASTA")
      shinyjs::enable("downloadAlignmentScores")
      shinyjs::enable("downloadAlignmentPDF")
      
   }) # end of observe event for run msa
   
   output$initialAlignmentText <- renderPrint({
      req(aligned_data())
      aligned_data()
      #aligned <- aligned_data()
      #withMathJax(HTML(msa::msaPrettyPrint(aligned, output="asis", showNames = "left", showLogo = "none",
      #               shadingMode = "similar", showLegend = FALSE, shadingColors = "blues", askForOverwrite = FALSE)))
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
                             initial = aligned_data(),
                             adjusted = alignment_adjusted(),
                             staggered = alignment_staggered())
         req(alignment) 
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
      filename = function(){#"aligned_seqs.pdf"
         file.path(paste0(directory, "aligned_seqs.pdf"))
      },
      content = function(file){
         req(alignment_pdf())
         file.copy(alignment_pdf(), file)
      }
   )
   
   
   # Phylogenetic Tree Construction
   tree_plot <- reactiveVal(NULL)
   tree_path <- reactiveVal(NULL)
   tree_model <- reactiveVal(NULL)
   
   observeEvent(input$buildTree, {
      req(aligned_data())
      
      disable("buildTree")
      waiter_show()
      
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
            
            shinyjs::enable("downloadTree")
            shinyjs::enable("downloadAll")
         },error = function(e){
            showNotification(paste("Error during tree construction:", e$message), type = "error", duration = 20)
         }) # end of try catch
         
         enable("buildTree")
         waiter_hide()
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
   
   # BARCODING
   
   # Species identification
   observe({ 
      refReady = !is.null(input$refBarcoding)
      queReady = !is.null(input$queBarcoding)
      
      toggleState("identifySpecies", refReady && queReady)
   })
   
   observeEvent(input$identifySpecies, {
      disable("identifySpecies")
      req(input$refBarcoding)
      req(input$queBarcoding)
      
      # read file
      barcoding_ref <- rphast::read.msa(input$refBarcoding)
      barcoding_que <- rphast::read.msa(input$queBarcoding)
      
      refseq <- ape::as.DNAbin(as.character(barcoding_ref))
      queseq <- ape::as.DNAbin(as.character(barcoding_que))
      
      # If not using kmer method
      if (input.kmerSelect == "false"){
         result_identity <- BarcodingR::barcoding.spe.identify(refseq, queseq, method = input$barcodingMethod)
      }
      
      if (input.kmerType == 'Fuzzy-set Method and kmer'){
         req(input$kmerValue)
         req(input$optimizationKMER)
         
         result_identity <- BarcodingR::barcoding.spe.identify2(refseq, queseq, kmer = input$kmerValue, optimization = input$optimizationKMER)
         
      } else if (input.kmerType == 'BP-based Method and kmer') {
         req(input$kmerValue)
         req(input$builtModel)
         req(input$lrValue)
         req(input$maxitValue)
         result_identity <- BarcodingR::bbsik(refseq, queseq, kmer = input$kmerValue, UseBuiltModel = input$builtModel, lr = input$lrValue, maxit = input$maxitValue)
      }
      
      output$identificationResult <- renderPrint({
         req(result_identity)
         result_identity
      })
   })
   
   # Optimize kmer values
   observe({
      fileReady <- !is.null(input$optimizeKmerRef)
      toggleState("calOptimumKmer", fileReady)
   })
   
   observeEvent(input$calOptimumKmer, {
      disable("calOptimumKmer")
      req(input$optimizeKmerRef)
      
      barcoding_ref <- rphast::read.msa(input$optimizeKmerRef)
      kmerFile <- ape::as.DNAbin(as.character(barcoding_ref))
      optimalKmer <- BarcodingR::optimize.kmer(kmerFile, max.kmer = input$maxKmer)
      
      # download
      output$kmerResult <- renderPrint({
         req(optimalKmer)
         as.data.frame(optimalKmer)
      })
      
      output$kmerPlot <- renderImage({
         req(kmerFile)
         req(input$maxKmer)
         BarcodingR::optimize.kmer(kmerFile, max.kmer = input$maxKmer)
      }, deleteFile = FALSE)
      
      output$downloadKmerPlot <- downloadHandler(
         filename = function(){
            paste0("optimum_kmer_", Sys.Date(), ".png")
         },
         content = function(file){
            req(kmerFile)
            req(input$maxKmer)
            BarcodingR::optimize.kmer(kmerFile, max.kmer = input$maxKmer)
         }, contentType = "image/png"
      )
      
   }) # end of observe event
   
   # barcoding gap
   observe({
      gapReady <- !is.null(input$barcodeRef)
      toggleState("gapBarcodes", gapReady)
   })
   
   observeEvent(input$gapBarcodes, {
      disable("gapBarcodes")
      req(input$barcodeRef)
      
      barcoding_ref <- rphast::read.msa(input$barcodeRef)
      refBarcode <- ape::as.DNAbin(as.character(barcoding_ref))
      gap <- BarcodingR::barcoding.gap(refBarcode, dist = input$gapModel)
      
      # download
      output$barcodingResult <- renderPrint({
         req(gap)
         as.data.frame(gap)
      })
      
      output$BarcodingGapPlot <- renderImage({
         req(refBarcode)
         req(input$gapModel)
         BarcodingR::barcoding.gap(refseq, dist = input$gapModel)
      }, deleteFile = FALSE)
      
      output$downloadGapPlot <- downloadHandler(
         filename = function(){
            paste0("barcoding_gap_", Sys.Date(), ".png")
         },
         content = function(file){
            req(refBarcode)
            req(input$gapModel)
            BarcodingR::barcoding.gap(refBarcode, dist = input$gapModel)
         }, contentType = "image/png"
      )
      
      
   }) # end of observe event
   
   # barcodes eval
   observe({ 
      barcode1 = !is.null(input$barcode1)
      barcode2 = !is.null(input$barcode2)
      toggleState("evalBarcodes", barcode1 && barcode2)
   })
   
   observeEvent(input$evalBarcodes, {
      disable("evalBarcodes")
      req(input$barcode1)
      req(input$barcode2)
      
      b1 <- rphast::read.msa(input$barcode1)
      b2 <- rphast::read.msa(input$barcode2)
      barcode1 <- ape::as.DNAbin(as.character(b1))
      barcode2 <- ape::as.DNAbin(as.character(b2))
      
      # convert to dataframe to download
      result <- BarcodingR::barcodes.eval(barcode1, barcode2, kmer1 = kmer1, kmer2 = kmer2)
      
      output$evalBarcodesResult <- renderTable({
         req(result)
         result2 <- as.data.frame(result)
         result2
      })
      
   }) # end of observe event
   
   # tdr2
   
   observe({
      file1Ready <- !is.null(input$oneSpe)
      file2Ready <- !is.null(input$queSpe)
      toggleState("calculateTDR2", file1Ready && file2Ready)
   })
   
   observeEvent(input$calculateTDR2, {
      disable(calculateTDR2)
      req(input$oneSpe)
      req(input$queSpe)
      
      que <- rphast::read.msa(input$oneSpe)
      ref <- rphast::read.msa(input$queSpe)
      
      query <- ape::as.DNAbin(as.character(que))
      reference <- ape::as.DNAbin(as.character(ref))
      
      # issue with results, it prints and not stores
      output$tdrValues <- renderPrint({
         req(query)
         req(reference)
         req(input$bootValue1)
         req(input$bootValue2)
         BarcodingR::TDR2(query, reference, boot = input$bootValue1, boot2 = input$bootValue2)
      })
   })
   
   ###
   
   
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
   
   observeEvent(input$runPopStats, {
      #lastAction(Sys.time())
      
      disable("runPopStats")
      waiter_show(html = spin_fading_circles(), color = "#ffffff")
      
      req(input$popStatsFile)
      
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
            
            output$privateAlleleTable <- DT::renderDataTable({
               as.data.frame(priv_alleles)
            }, options = list(pageLength = 10, scrollX = TRUE))
            
            incProgress(0.6, detail = "Computing population statistics...")
            population_stats <- reactive({
               req(fsnps_gen())
               compute_population_stats(fsnps_gen())
            })
            
            output$meanallelic <- DT::renderDataTable({
               population_stats()$mar_list
            })
            
            # heterozygosity
            output$heterozygosity_table <- DT::renderDataTable({
               population_stats()$heterozygosity
            })
            
            output$heterozygosity_plot <- renderImage({
               req(population_stats()$heterozygosity)
               
               plot_path <- plot_heterozygosity(
                  Het_fsnps_df = population_stats()$heterozygosity,
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
               population_stats()$inbreeding_coeff
            })
            
            output$ttest_table <- DT::renderDataTable({
               population_stats()$ttest
            })
            
            output$allele_freq_table <- DT::renderDataTable({
               population_stats()$allele_frequencies
            }, options = list(scrollX = TRUE))
            
            #output$privateAllelePlot <- renderUI({
            #   if (is.list(priv_alleles) && "message" %in% names(priv_alleles)) {
            #      tags$div(style = "color:gray; font-style:italic; margin-top:10px;",
            #               priv_alleles$message)
            #   } else {
            #      # Display as a nicely formatted table
            #      DT::dataTableOutput("privateAlleleTable")
            #   }
            #})
            
            ## HWE
            incProgress(0.8, detail = "Running HWE and FST calculations...")
            hardy_weinberg_stats <- reactive({
               req(fsnps_gen())
               compute_hardy_weinberg(fsnps_gen())
            })
            
            output$hwe_summary <- renderUI({
               pvals <- hardy_weinberg_stats()$hw_summary
               tagList(
                  h4("HWE P-values across loci"),
                  verbatimTextOutput("hwe_summary_text")
               )
            })
            
            output$hwe_summary_text <- renderText({
               paste0("P-values: ", paste(round(hardy_weinberg_stats()$hw_summary, 4), collapse = ", "))
            })
            
            output$hwe_chisq_table <- DT::renderDataTable({
               hardy_weinberg_stats()$hw_dataframe
            }, options = list(scrollX = TRUE))
            
            ## FST
            incProgress(1.0, detail = "Still running HWE and FST calculations...")
            
            fst_stats <- reactive({
               req(fsnps_gen())
               compute_fst(fsnps_gen())
            })
            
            fst_data <- reactive({
               req(fst_stats())
               fst_stats()$fst_dataframe
            })
            
            showNotification("Rendering outputs, this might take some time...", type = "message", duration = 30)
            print(Sys.time())
            
            output$fstMatrixUI <- renderUI({
               fst <- fst_stats()$fsnps_fst_matrix
               if (is.list(fst) && "message" %in% names(fst)) {
                  tags$p(style = "color:gray;", fst$message)
               } else {
                  DT::dataTableOutput("fstMatrixTable")
               }
            })
            
            output$fstMatrixTable <- DT::renderDataTable({
               matrix_data <- matrix(unlist(fst_stats()$fst_matrix),
                                     nrow = sqrt(length(fst_stats()$fst_matrix)),
                                     byrow = TRUE)
               rownames(matrix_data) <- colnames(matrix_data) <- attr(fsnps_gen(), "pop.names")
               as.data.frame(matrix_data)
            }, options = list(scrollX = TRUE))
            
            output$fstDfTable <- DT::renderDataTable({
               fst_stats()$fst_dataframe
            })
            
            
            output$fst_heatmap_plot <- renderImage({
               req(fst_data())
               
               plot_path <- plot_fst_heatmap(
                  fst_df = fst_data(),
                  out_dir = tempdir()
               )
               
               list(
                  src = plot_path,
                  contentType = "image/png",
                  alt = "FST Heatmap",
                  width = "100%"
               )
            }, deleteFile = TRUE)
            
            
            enable("runPopStats")
            
            # download heterozygosity plot
            output$downloadHeterozygosityPlot <- downloadHandler(
               filename = function() {
                  "heterozygosity_plot.png"
               },
               content = function(file) {
                  plot_path <- plot_heterozygosity(
                     Het_fsnps_df = population_stats()$heterozygosity,
                     out_dir = tempdir()
                  )
                  file.copy(plot_path, file)
               }
            )
            
            output$downloadFstHeatmap <- downloadHandler(
               filename = function() {
                  "fst_heatmap.png"
               },
               content = function(file) {
                  plot_path <- plot_fst_heatmap_static(
                     fst_df = fst_data(),
                     out_dir = tempdir()
                  )
                  file.copy(plot_path, file)
               }
            )
            
            ## download all results
            stats_matrix <- compute_population_stats(fsnps_gen())
            hw_matrix <- compute_hardy_weinberg(fsnps_gen())
            fst_matrix <- compute_fst(fsnps_gen())
            
            output$downloadStatsXLSX <- downloadHandler(
               filename = function() {
                  timestamp <- format(Sys.time(), "%Y%m%d_%H%M")
                  paste0("population-statistics-results_", timestamp, ".xlsx")
               },
               content = function(file) {
                  req(stats_matrix, hw_matrix, fst_matrix)
                  path <- export_results(priv_alleles, stats_matrix, hw_matrix, fst_matrix, dir = tempdir())
                  
                  file.copy(path, file)
                  #openxlsx::write.xlsx(path, file = filename)
               }
            )
            waiter_hide()
         }, error = function(e) {
            showNotification(paste("Population stats error:", e$message), type = "error")
            enable("runPopStats")
         })
      }) #end of withProgress
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
      
      usingDefaults <- input$useDefaultColors
      
      readyForPCA <- hasDataFile && (usingDefaults || (hasLabelsFile && hasColorFile))
      
      toggleState("runPCA", readyForPCA)
   })
   
   observeEvent(input$runPCA, {
      #lastAction(Sys.time())
      
      disable("runPCA")
      waiter_show(html = spin_fading_circles(), color = "#ffffff")
      
      req(input$pcaFile)
      
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
            
            if (!input$useDefaultColors) {
               req(input$pcaLabels, input$colorPalette)
               
               labels <- readLines(input$pcaLabels$datapath)
               colors <- readLines(input$colorPalette$datapath)
               
               # Optional: trim whitespace, ensure length match
               labels <- trimws(labels)
               colors <- trimws(colors)
               
               if (length(labels) != length(colors)) {
                  stop("Mismatch between number of labels and colors.")
               }
            }
            
            labels_colors <- get_colors_labels(
               fsnps_gen = fsnps_gen,
               use_default = input$useDefaultColors,
               input_labels = labels,
               input_colors = colors
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
            
            waiter_hide()
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
   
   observeEvent(input$runStructure, {
      disable("runStructure")
      waiter_show(html = spin_fading_circles(), color = "#ffffff")
      
      req(input$structureFile)
      
      withProgress(message = "Running STRUCTURE analysis...", {
         incProgress(0.2, detail = "Loading input file...")
         df <- load_input_file(input$structureFile$datapath)
         fsnps_gen <- clean_input_data_str(df)
         
         
         incProgress(0.4, detail = "Converting to STRUCTURE file...")
         output_dir <- tempdir()
         dir.create(output_dir, showWarnings = FALSE)
         out_path <- file.path(output_dir, "structure_input.str")
         
         structure_df <- to_structure(fsnps_gen$fsnps_gen, include_pop = TRUE)
         structure_df[] <- lapply(structure_df, function(col) as.numeric(as.character(col)))
         
         write.table(structure_df, file = out_path, quote = FALSE, sep = " ",
                     row.names = FALSE, col.names = FALSE)
         
         #cat("STRUCTURE input preview:\n", paste(readLines(out_path, n = 5), collapse = "\n"))
         #structure_file <- out_path
         
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
         waiter_hide()
         
         
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
      }) # end of with progress
      
      
   }) # end of observe event for structure
   
}

shinyApp(ui, server)