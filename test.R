tabItem(tabName = "FileConv",
        tabsetPanel(
           # --- VCF Tab ---
           tabPanel("VCF Conversion",
                       box(
                          title = "VCF Options",
                          width = 5,
                          fileInput("VCFFile", "Upload VCF File"),
                          radioButtons("vcfOutputType", "Choose final file type",
                                       choices = c("PLINK files (.bed/.bim/.fam)" = "plink",
                                                   "CSV file" = "csv",
                                                   "FASTA file" = "fasta")),
                          conditionalPanel(
                             condition = "input.vcfOutputType == 'fasta'",
                             fileInput("FASTARef", "Reference sequence in FASTA format.")
                          ),
                          conditionalPanel(
                             condition = "input.vcfOutputType == 'csv'",
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
                             )
                          ),
                          actionButton("ConvertVCF", "Convert VCF", icon = icon("file-csv"))
                       )
           ),
           
           # --- BCF Tab ---
           tabPanel("BCF Conversion",
                       box(
                          title = "BCF Options",
                          width = 5,
                          fileInput("BCFFile", "Upload BCF File"),
                          radioButtons("bcfOutputType", "Choose final file type",
                                       choices = c("VCF file" = "vcf",
                                                   "PLINK files (.bed/.bim/.fam)" = "plink",
                                                   "CSV file" = "csv")),
                          conditionalPanel(
                             condition = "input.bcfOutputType == 'csv'",
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
                             )
                          ),
                          actionButton("ConvertBCF", "Convert BCF", icon = icon("file-csv"))
                       )
                    
           ),
           
           # --- PLINK Tab ---
           tabPanel("PLINK Conversion",
                       box(
                          title = "PLINK Options",
                          width = 5,
                          fileInput("bedFile", "Upload BED File"),
                          fileInput("bimFile", "Upload BIM File"),
                          fileInput("famFile", "Upload FAM File"),
                          radioButtons("plinkOutputType", "Choose final file type",
                                       choices = c("VCF file" = "vcf",
                                                   "CSV file" = "csv")),
                          conditionalPanel(
                             condition = "input.plinkOutputType == 'csv'",
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
                             )
                          ),
                          actionButton("ConvertPLINK", "Convert PLINK", icon = icon("file-csv"))
                       )
                    
           ),
           
           # --- CSV Tab ---
           tabPanel("CSV Conversion",
                    fluidRow(
                       box(
                          title = "CSV Options",
                          width = 5,
                          fileInput("CSVFile", "Upload CSV File"),
                          fileInput("lociMetaFile", "Upload loci/marker information"),
                          actionButton("ConvertCSV", "Convert CSV", icon = icon("file-csv"))
                       )
                    )
           )
        )
)