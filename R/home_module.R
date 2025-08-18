# R/home_module.R

#' Home Tab UI
#'
#' This function defines the user interface for the main "Home" tab.
#' It includes all the input controls for selecting analysis types and uploading data.
#'
#' @param id A unique identifier for the module.
#'
homeUI <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)
  
  # UI is now a flex container with three direct children panels
  div(class = "home-container",
      
      # --- Panel 1: User Inputs (Left) ---
      div(class = "overlay-panel",
          radioButtons(
            ns("analysis_mode"),
            "Select Analysis Workflow",
            choices = c(
              "Experimental Design" = "eda",
              "Stability Analysis" = "stability",
              "Multivariate Analysis" = "multivariate",
              "Mating Design" = "mating"
            ),
            selected = "eda"
          ),
          fileInput(ns("file"), "Upload CSV File", accept = c("text/csv", ".csv")),
          
          # --- Conditional UI for Stability Analysis ---
          conditionalPanel(
            condition = paste0("input['", ns("analysis_mode"), "'] == 'stability'"),
            selectInput(
              ns("stab_subtype"), "Select The Stability Analysis?",
              choices = c("AMMI Analysis" = "ammi", "GGE Biplot" = "gge")
            )
          ),
          
          # --- Conditional UI for Multivariate Analysis (Updated) ---
          conditionalPanel(
            condition = paste0("input['", ns("analysis_mode"), "'] == 'multivariate'"),
            selectInput(
              ns("multi_subtype"), "Select The Multivariate Analysis",
              choices = c("Principal Component Analysis (PCA)" = "pca", 
                          "Correlation Analysis" = "correlation", 
                          "Path Analysis" = "path")
            )
          ),
          
          # --- Conditional UI for Experimental Design ---
          conditionalPanel(
            condition = paste0("input['", ns("analysis_mode"), "'] == 'eda'"),
            selectInput(ns("design"), "Select Experimental Design",
                        choices = c("Alpha Lattice", "RCBD", "CRD", "Augmented RCBD"))
          ),
          
          # --- Conditional UI for Mating Design ---
          conditionalPanel(
            condition = paste0("input['", ns("analysis_mode"), "'] == 'mating'"),
            selectInput(
              ns("md_mating_design"), "Select Mating Design",
              choices = c(
                "Griffing Method I (Full Diallel: Parents, F1s, Reciprocals)" = "griffing_m1",
                "Griffing Method II (Parents & F1s, No Reciprocals)" = "griffing_m2",
                "Griffing Method III (F1s & Reciprocals, No Parents)" = "griffing_m3",
                "Griffing Method IV (F1s Only, No Parents, No Reciprocals)" = "griffing_m4",
                "Partial Diallel" = "diallel_partial",
                "Line x Tester" = "line_tester"
              )
            )
          ),
          actionButton(ns("go_to_analysis"), "Proceed to Analysis", class = "btn btn-primary"),
          br(),br(),
          tags$div(style = "font-size: 11px; text-align: center;",
                   HTML("Developed by <b>Dr. Abhijith K P</b><br>
             Scientist (Genetics and Plant Breeding), ICAR-IARI Assam<br>
             <a href='mailto:abhijithkpgen@gmail.com'>abhijithkpgen@gmail.com</a>"),
                   br(), br(),
                   tags$div(
                     class = "custom-footer",
                     HTML("Copyright &copy; 2025 Abhijith Krishnan. Released under the <a href='https://github.com/abhijithkpgen/PBAT/blob/main/LICENSE' target='_blank'>MIT License</a>.")
                   )
          )
      ),
      
      # --- Panel 2: Workflow Overview (Middle) ---
      div(class = "overlay-panel",
          h4(tags$b("Application Workflow Overview")),
          p("PbAT is an end-to-end pipeline for statistical and multivariate analysis of plant breeding data. Follow these steps:"),
          tags$ol(
            tags$li(tags$b("Upload Data:"), " Upload your experimental data in CSV format."),
            tags$li(tags$b("Select Analysis Type:"), " Choose your desired analysis workflow."),
            tags$li(tags$b("Map Data Columns:"), " Assign the appropriate columns to enable correct analysis."),
            tags$li(tags$b("Run Analysis:"), tags$ul(
              tags$li(tags$b("Experimental Designs:"), " Generate summary statistics, ANOVA, BLUEs/BLUPs, diagnostics, and post-hoc tests."),
              tags$li(tags$b("Stability Analysis:"), " Perform AMMI and GGE biplot analysis for GxE interaction."),
              tags$li(tags$b("Multivariate Analysis:"), " Perform PCA, correlation, and path analysis."),
              tags$li(tags$b("Mating Design Analysis:"), " Analyze Diallel and Line x Tester designs.")
            )),
            tags$li(tags$b("Review & Download Results:"), " Download all results as publication-ready files for reporting or further analysis.")
          ),
          tags$p(style = "font-size: 11px; margin-top: 10px;",
                 "PbAT is designed for intuitive data analysis without needing expertise in R. ",
                 tags$b("Sample datasets and detailed help are available in the Help & Guide tab.")
          )
      ),
      
      # --- Panel 3: Run Locally (Right) ---
      div(class = "overlay-panel installation-box",
          tags$div(style = "text-align: center;",
                   tags$img(src = "www/spinner2.gif", height = "150px", style = "margin-bottom: 5px; border: none;")
          ),
          h4(icon("laptop-code"), " Run Locally in R"),
          p("You can install and run this application on your own computer directly within R or RStudio. This is a great option for offline use or for analyzing sensitive data."),
          tags$a(href = "https://github.com/abhijithkpgen/PBAT", 
                 target = "_blank", 
                 class = "btn btn-primary", 
                 style = "color: white !important; margin-right: 10px;", 
                 icon("github"), " View on GitHub"),
          tags$a(href = "https://github.com/abhijithkpgen/PBAT/archive/refs/tags/v1.0.5.tar.gz", 
                 class = "btn btn-secondary", 
                 style = "color: white !important;", 
                 icon("download"), " Download v1.0.5"),
          
          tags$details(
            tags$summary(icon("terminal"), " Click to view Installation Instructions"),
            tags$pre(
              tags$code(
                HTML(
                  '# Install devtools if not already installed<br>',
                  'if (!require("devtools")) install.packages("devtools")<br><br>',
                  '# Install PbAT from GitHub<br>',
                  'devtools::install_github("abhijithkpgen/PBAT")<br><br>',
                  '# Load the library<br>',
                  'library(PBAT)<br><br>',
                  '# Run the application<br>',
                  'run_app()'
                )
              )
            )
          )
      )
  )
}


#' Home Tab Server
#'
#' This function defines the server-side logic for the Home tab. Its main
#' role is to listen for the user's request to proceed, gather all the inputs,
#' and return them as a reactive value to the main app.
#'
#' @param id A unique identifier for the module.
#' @return A reactive list containing the uploaded data and user selections.
#'
homeServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    results_to_return <- reactiveVal()
    observeEvent(input$go_to_analysis, {
      req(input$file)
      df <- readr::read_csv(input$file$datapath, na = c("", "NA"), show_col_types = FALSE)
      settings <- list(
        file_data = df,
        analysis_mode = input$analysis_mode,
        design = input$design,
        stab_subtype = input$stab_subtype,
        multi_subtype = input$multi_subtype,
        mating_design = input$md_mating_design,
        trigger = runif(1)
      )
      results_to_return(settings)
    })
    return(results_to_return)
  })
}
