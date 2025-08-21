# R/home_module.R

homeUI <- function(id) {
  ns <- NS(id)
  
  div(class = "home-container",
      
      div(class = "overlay-panel",
          radioButtons(
            ns("analysis_mode"),
            "Select Analysis Workflow",
            choices = c(
              "Design Your Trial" = "design_exp",
              "Trait Explorer" = "trait_explorer",
              "Experimental Design" = "eda",
              "Stability Analysis" = "stability",
              "Multivariate Analysis" = "multivariate",
              "Mating Design" = "mating"
            ),
            selected = "design_exp"
          ),
          
          # --- Conditional Panel for Trial Design Inputs ---
          conditionalPanel(
            condition = paste0("input['", ns("analysis_mode"), "'] == 'design_exp'"),
            
            # This selectInput controls the file inputs below
            selectInput(ns("design_type_home"), "Select Design Type",
                        choices = c("Randomized Complete Block Design (RCBD)" = "rcbd",
                                    "Augmented RCBD" = "augmented",
                                    "Alpha Lattice Design" = "alpha")),
            
            # File input for RCBD and Alpha Lattice designs
            conditionalPanel(
              condition = "input.design_type_home == 'rcbd' || input.design_type_home == 'alpha'", ns = ns,
              fileInput(ns("geno_file_std_home"), "Upload Genotypes CSV (Single Column)", accept = ".csv")
            ),
            
            # File input for Augmented RCBD
            conditionalPanel(
              condition = "input.design_type_home == 'augmented'", ns = ns,
              fileInput(ns("geno_file_aug_home"), "Upload Genotypes CSV (Two Columns: Test, Check)", accept = ".csv")
            )
          ),
          
          # --- Conditional File Input for all OTHER analysis modes ---
          conditionalPanel(
            condition = paste0("input['", ns("analysis_mode"), "'] != 'design_exp'"),
            fileInput(ns("file"), "Upload CSV File for Analysis", accept = c("text/csv", ".csv"))
          ),
          
          # --- Other conditional UIs ---
          conditionalPanel(
            condition = paste0("input['", ns("analysis_mode"), "'] == 'trait_explorer'"),
            selectInput(ns("explorer_type"), "Select Explorer Type", choices = c("Spatial Trait Explorer" = "spatial", "Data Curation & Outlier Analysis" = "curation"))
          ),
          conditionalPanel(
            condition = paste0("input['", ns("analysis_mode"), "'] == 'stability'"),
            selectInput(ns("stab_subtype"), "Select The Stability Analysis?", choices = c("AMMI Analysis" = "ammi", "GGE Biplot" = "gge"))
          ),
          conditionalPanel(
            condition = paste0("input['", ns("analysis_mode"), "'] == 'multivariate'"),
            selectInput(ns("multi_subtype"), "Select The Multivariate Analysis", choices = c("Principal Component Analysis (PCA)" = "pca", "Correlation Analysis" = "correlation", "Path Analysis" = "path"))
          ),
          conditionalPanel(
            condition = paste0("input['", ns("analysis_mode"), "'] == 'eda'"),
            selectInput(ns("design"), "Select Experimental Design", choices = c("Alpha Lattice", "RCBD", "CRD", "Augmented RCBD"))
          ),
          conditionalPanel(
            condition = paste0("input['", ns("analysis_mode"), "'] == 'mating'"),
            selectInput(ns("md_mating_design"), "Select Mating Design", choices = c("Griffing Method I (Full Diallel: Parents, F1s, Reciprocals)" = "griffing_m1", "Griffing Method II (Parents & F1s, No Reciprocals)" = "griffing_m2", "Griffing Method III (F1s & Reciprocals, No Parents)" = "griffing_m3", "Griffing Method IV (F1s Only, No Parents, No Reciprocals)" = "griffing_m4", "Partial Diallel" = "diallel_partial", "Line x Tester" = "line_tester"))
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
      
      # This is now a dynamic UI output
      uiOutput(ns("workflow_overview_ui")),
      
      div(class = "overlay-panel installation-box",
          tags$div(style = "text-align: center;",
                   tags$img(src = "www/spinner2.gif", height = "150px", style = "margin-bottom: 5px; border: none;")
          ),
          h4(icon("laptop-code"), " Run Locally in R"),
          p("You can install and run this application on your own computer directly within R or RStudio. This is a great option for offline use or for analyzing large data sets."),
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

homeServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    results_to_return <- reactiveVal()
    observeEvent(input$go_to_analysis, {
      
      mode <- input$analysis_mode
      df <- NULL
      geno_df <- NULL
      
      if (mode != "design_exp") {
        req(input$file)
        df <- readr::read_csv(input$file$datapath, na = c("", "NA"), show_col_types = FALSE)
      } else {
        if (input$design_type_home %in% c("rcbd", "alpha")) {
          req(input$geno_file_std_home)
          geno_df <- readr::read_csv(input$geno_file_std_home$datapath, show_col_types = FALSE)
        } else if (input$design_type_home == "augmented") {
          req(input$geno_file_aug_home)
          geno_df <- readr::read_csv(input$geno_file_aug_home$datapath, show_col_types = FALSE)
        }
      }
      
      settings <- list(
        file_data = df,
        geno_data = geno_df,
        analysis_mode = mode,
        design_type_home = input$design_type_home,
        explorer_type = input$explorer_type,
        design = input$design,
        stab_subtype = input$stab_subtype,
        multi_subtype = input$multi_subtype,
        mating_design = input$md_mating_design,
        trigger = runif(1)
      )
      results_to_return(settings)
    })
    
    # --- NEW: Dynamic Workflow Overview Panel ---
    output$workflow_overview_ui <- renderUI({
      
      mode <- input$analysis_mode
      
      panel_content <- switch(
        mode,
        "design_exp" = tagList(
          h4(tags$b("Design Your Trial Workflow")),
          p("This workflow helps you create randomized layouts for common experimental designs before going to the field."),
          tags$ol(
            tags$li(tags$b("Select Design Type:"), "Choose from RCBD, Augmented RCBD, or Alpha Lattice."),
            tags$li(tags$b("Upload Genotypes:"), "Provide a CSV file with your test (and check) genotype names."),
            tags$li(tags$b("Set Parameters:"), "Specify the number of replications, blocks, etc."),
            tags$li(tags$b("Generate & Download:"), "Create and visualize the randomized field plan and download the field book as a CSV.")
          )
        ),
        "trait_explorer" = tagList(
          h4(tags$b("Trait Explorer Workflow")),
          p("This workflow is for pre-analysis data visualization and cleaning."),
          tags$ol(
            tags$li(tags$b("Upload Data:"), "Provide your raw field data CSV."),
            tags$li(tags$b("Select Explorer Type:"),
                    tags$ul(
                      tags$li(tags$b("Spatial Explorer:"), "Visualize the distribution of a trait across the physical layout of your field to identify spatial trends or gradients."),
                      tags$li(tags$b("Data Curation:"), "Automatically detect potential outliers in your trait data using statistical methods (IQR, SD, etc.).")
                    )
            ),
            tags$li(tags$b("Map Columns & Run:"), "Assign columns and generate plots or reports."),
            tags$li(tags$b("Curate & Re-upload:"), "Download the outlier report, clean your original dataset, and re-upload the curated file for further analysis.")
          )
        ),
        "eda" = tagList(
          h4(tags$b("Experimental Design Analysis Workflow")),
          p("Analyze data from a completed trial to evaluate genotype performance."),
          tags$ol(
            tags$li(tags$b("Upload Data:"), "Provide your collected trial data."),
            tags$li(tags$b("Select Design:"), "Choose the design you used (e.g., RCBD, Alpha Lattice)."),
            tags$li(tags$b("Map Columns:"), "Assign columns for genotype, block, traits, etc."),
            tags$li(tags$b("Run Analysis:"), "Generate descriptive statistics, ANOVA, variance components, heritability, and diagnostic plots."),
            tags$li(tags$b("Calculate Estimates:"), "Compute Best Linear Unbiased Estimates (BLUEs) for fixed models or Predictors (BLUPs) for random models."),
            tags$li(tags$b("Download:"), "Export all tables and plots in a publication-ready format.")
          )
        ),
        "stability" = tagList(
          h4(tags$b("Stability Analysis Workflow")),
          p("Analyze genotype performance across multiple environments (Genotype x Environment interaction)."),
          tags$ol(
            tags$li(tags$b("Upload Data:"), "Provide data from a multi-environment trial."),
            tags$li(tags$b("Select Analysis:"), "Choose between AMMI or GGE Biplot analysis."),
            tags$li(tags$b("Map Columns:"), "Assign columns for genotype, environment, replication, and the trait of interest."),
            tags$li(tags$b("Run & Interpret:"), "Generate biplots to visualize GxE patterns, identify stable/adapted genotypes, and understand the 'which-won-where' scenario."),
            tags$li(tags$b("Download:"), "Export stability indices, ANOVA tables, and high-quality biplots.")
          )
        ),
        "multivariate" = tagList(
          h4(tags$b("Multivariate Analysis Workflow")),
          p("Explore the relationships between multiple traits."),
          tags$ol(
            tags$li(tags$b("Upload Data:"), "Provide your trial data with multiple measured traits."),
            tags$li(tags$b("Select Analysis:"), "Choose PCA, Correlation, or Path Analysis."),
            tags$li(tags$b("Select Traits:"), "Choose two or more traits for the analysis."),
            tags$li(tags$b("Run & Visualize:"), "Generate plots (e.g., PCA biplots, correlograms, path diagrams) to understand trait associations, identify patterns, and determine direct/indirect effects."),
            tags$li(tags$b("Download:"), "Export all plots and underlying data tables.")
          )
        ),
        "mating" = tagList(
          h4(tags$b("Mating Design Analysis Workflow")),
          p("Analyze data from specific crossing schemes to estimate genetic parameters."),
          tags$ol(
            tags$li(tags$b("Upload Data:"), "Provide your data from a mating design experiment."),
            tags$li(tags$b("Select Design:"), "Choose from Line x Tester or various Diallel methods."),
            tags$li(tags$b("Map Columns:"), "Assign columns for parents (or lines/testers), replication, and traits."),
            tags$li(tags$b("Run Analysis:"), "Calculate ANOVA and estimate General (GCA) and Specific (SCA) Combining Ability effects."),
            tags$li(tags$b("Interpret Results:"), "Identify the best general combiners for breeding programs and the best specific crosses for hybrid development.")
          )
        )
      )
      
      div(class = "overlay-panel", panel_content)
    })
    
    return(results_to_return)
  })
}
