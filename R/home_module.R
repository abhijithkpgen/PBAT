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

  # UI code from the original "Home" tabPanel
  div(class = "home-container",
    # Left Panel: User Inputs
    div(class = "overlay-panel",
      radioButtons(
        ns("analysis_mode"), # All input/output IDs must be wrapped in ns()
        "Select Analysis Type",
        choices = c(
          "Experimental Design " = "eda",
          "Multivariate Analysis (PCA / GGE / Correlation / Path)" = "multivariate",
          "Mating Design " = "mating"
        ),
        selected = "eda"
      ),
      div(class = "compact-row",
        div(class = "half-width", textInput(ns("user_name"), "Name")),
        div(class = "half-width", textInput(ns("user_email"), "Email"))
      ),
      textInput(ns("user_institute"), "Institute / Organization"),
      fileInput(ns("file"), "Upload CSV File", accept = ".csv"),

      # Conditional Panels must use the namespaced input condition
      conditionalPanel(
        condition = paste0("input['", ns("analysis_mode"), "'] == 'multivariate'"),
        selectInput(
          ns("multi_subtype"),
          "Which Multivariate Analysis?",
          choices = c( "AMMI Analysis" = "ammi", 
            "GGE Biplot" = "gge", "Principal Component Analysis (PCA)" = "pca",
            "Correlation Analysis" = "correlation", "Path Analysis" = "path"
          )
        )
      ),
      conditionalPanel(
        condition = paste0("input['", ns("analysis_mode"), "'] == 'eda'"),
        selectInput(ns("design"), "Experimental Design",
                    choices = c("Alpha Lattice", "RCBD", "CRD", "Augmented RCBD"))
      ),
      conditionalPanel(
        condition = paste0("input['", ns("analysis_mode"), "'] == 'mating'"),
        selectInput(
          ns("md_mating_design"),
          "Select Mating Design",
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
        br(), br(), br(), br(), br(), br(), br(), br(),
        tags$div(
          class = "custom-footer",
          HTML("Copyright &copy; 2025 Abhijith Krishnan. Released under the <a href='https://github.com/abhijithkpgen/PBAT/blob/main/LICENSE' target='_blank'>MIT License</a>.")
        )
      )
    ),
    # Right Panel: Workflow Overview (Static content requires no changes)
    div(class = "overlay-panel",
      h4(tags$b("Application Workflow Overview")),
      tags$p("PbAT: Plant Breeding Analytical Tools is an end-to-end pipeline for statistical and multivariate analysis of plant breeding data. Follow these steps:"),
      tags$ol(
        tags$li(tags$b("Upload Data:"), " Upload your experimental data in CSV format. Include columns for genotype, location, replication, block (if applicable), and trait(s)."),
        tags$li(tags$b("Select Analysis Type:"), " Choose Experimental Design Analysis, Multivariate Analysis, or Mating Design Analysis depending on your workflow."),
        tags$li(tags$b("Map Data Columns:"), " Assign the appropriate columns (genotype, location, replication, block, etc.) when prompted, to enable correct analysis."),
        tags$li(tags$b("Run Analysis:"), tags$ul(
          tags$li(tags$b(" Experimental Designs:"), " Generate summary statistics, ANOVA, heritability (H^2), BLUEs/BLUPs (combined and location-wise), diagnostics, and post-hoc tests. Visualize results with boxplots, QQ plots, and interaction plots."),
          tags$li(tags$b(" Multivariate Analysis:"), " Perform PCA, GGE biplot , correlation, and path analysis on selected traits. Run analyses in standalone mode or linked to your EDA results."),
          tags$li(tags$b(" Mating Design Analysis:"), " Analyze Diallel (Griffing Methods I-IV, PartialDiallel Designs) and  Line x Tester. Obtain GCA/SCA effects, ANOVA tables, and variance components.")
        )),
        tags$li(tags$b(" Review & Download Results:"), " All results-interactive tables, plots, and summaries-can be downloaded as publication-ready PDF/CSV ZIP files for reporting or further analysis.")
      ),
      tags$p(style = "font-size: 11px; margin-top: 10px;",
        "PbAT is designed for plant breeders and researchers to analyze data intuitively without needing knowledge in R. ",
        tags$b("Sample datasets and detailed help are available in the Help & Guide tab.")
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

    # This reactiveVal will hold the data and settings to be returned to the main app.
    results_to_return <- reactiveVal()

    # This observer watches for the "Proceed to Analysis" button click within this module.
    observeEvent(input$go_to_analysis, {
      # 1. Ensure a file has been uploaded before proceeding.
      req(input$file)

      # 2. Read the uploaded CSV file.
      df <- readr::read_csv(input$file$datapath, na = c("", "NA"), show_col_types = FALSE)

      # 3. Bundle all the inputs from this module's UI into a single list.
      settings <- list(
        file_data = df,
        analysis_mode = input$analysis_mode,
        design = input$design,
        multi_subtype = input$multi_subtype,
        mating_design = input$md_mating_design,
        trigger = runif(1) # A random number to ensure this reactive always fires
      )

      # 4. Set the reactiveVal with our results list.
      # The main app server will be listening for this change.
      results_to_return(settings)
    })

    # 5. CRITICAL: Return the reactiveVal.
    # This makes the data and settings from this module available to the main app.
    return(results_to_return)
  })
}