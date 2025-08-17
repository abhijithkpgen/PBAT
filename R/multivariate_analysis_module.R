# multivariate_analysis_module.R
#
# This file contains the UI and Server logic for the updated Multivariate Analysis module.
# AMMI and GGE analyses have been moved to the new Stability Analysis module.

# Required Libraries
library(shiny)
library(dplyr)
library(FactoMineR)
library(factoextra)
library(corrplot)
library(PerformanceAnalytics)
library(lavaan)
library(semPlot)
library(DT)

# ===================================================================
# MODULE UI FUNCTION
# ===================================================================
multivariate_analysis_ui <- function(id) {
  ns <- NS(id)
  
  tabPanel("Multivariate Analysis",
           sidebarLayout(
             sidebarPanel(
               width = 3,
               uiOutput(ns("multi_sidebar"))
             ),
             mainPanel(
               width = 9,
               uiOutput(ns("multi_mainpanel"))
             )
           )
  )
}

# ===================================================================
# MODULE SERVER FUNCTION
# ===================================================================
multivariate_analysis_server <- function(id, shared_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # --- Reactive Values to store results ---
    pca_results  <- reactiveVal(NULL)
    path_results <- reactiveVal(NULL)
    corr_data    <- reactiveVal(NULL)
    
    # --- Reactives to access shared data ---
    multi_file_data <- reactive({ shared_data$file_data })
    multi_subtype_selected <- reactive({ shared_data$multi_subtype })
    
    # --- Dynamic Sidebar UI ---
    output$multi_sidebar <- renderUI({
      req(multi_subtype_selected(), multi_file_data())
      
      df <- multi_file_data()
      choices_numeric <- names(df)[sapply(df, is.numeric)]
      
      step_block <- function(label, ...) {
        tags$div(tags$b(label), br(), ..., style = "margin-bottom: 20px;")
      }
      
      div(style = "color: #142850; font-size: 15px;",
          if (multi_subtype_selected() == "pca") {
            tagList(
              step_block("Step 1: Select Traits for PCA",
                         checkboxGroupInput(ns("multi_pca_traits"), "Traits for PCA", choices = choices_numeric)
              ),
              step_block("Step 2: Run and Download",
                         actionButton(ns("multi_run_pca"), "Run PCA", class = "btn btn-success"),
                         uiOutput(ns("multi_pca_status")), br(),
                         downloadButton(ns("multi_download"), "Download Results (ZIP)", class = "btn btn-primary")
              )
            )
          } else if (multi_subtype_selected() == "correlation") {
            tagList(
              step_block("Step 1: Select Traits for Correlation",
                         checkboxGroupInput(ns("multi_corr_traits"), "Traits for Correlation", choices = choices_numeric)
              ),
              step_block("Step 2: Run and Download",
                         actionButton(ns("multi_run_corr"), "Run Correlation Analysis", class = "btn btn-success"),
                         uiOutput(ns("multi_corr_status")), br(),
                         downloadButton(ns("multi_download"), "Download Results (ZIP)", class = "btn btn-primary")
              )
            )
          } else if (multi_subtype_selected() == "path") {
            tagList(
              step_block("Step 1: Select Dependent and Independent Traits",
                         selectInput(ns("multi_path_dep"), "Dependent Trait (Y)", choices = choices_numeric),
                         selectInput(ns("multi_path_indep"), "Independent Traits (X)",
                                     choices = setdiff(choices_numeric, input$multi_path_dep),
                                     multiple = TRUE)
              ),
              step_block("Step 2: Run and Download",
                         actionButton(ns("multi_run_path"), "Run Path Analysis", class = "btn btn-success"),
                         uiOutput(ns("multi_path_status")), br(),
                         downloadButton(ns("multi_download"), "Download Results (ZIP)", class = "btn btn-primary")
              )
            )
          }
      )
    })
    
    # --- Dynamic Main Panel UI ---
    output$multi_mainpanel <- renderUI({
      req(multi_subtype_selected())
      if (multi_subtype_selected() == "pca") {
        tabsetPanel(
          tabPanel("Individual Biplot", plotOutput(ns("multi_pca_biplot"))),
          tabPanel("Scree Plot", plotOutput(ns("multi_pca_scree"))),
          tabPanel("Variable Contributions", plotOutput(ns("multi_pca_varcontrib"))),
          tabPanel("Summary Table", tableOutput(ns("multi_pca_eigen")))
        )
      } else if (multi_subtype_selected() == "correlation") {
        tabsetPanel(
          tabPanel("Correlation Matrix Plot", plotOutput(ns("multi_corr_corrplot"))),
          tabPanel("Pairs Plot", plotOutput(ns("multi_corr_pairsplot")))
        )
      } else if (multi_subtype_selected() == "path") {
        tabsetPanel(
          tabPanel("Path Diagram", plotOutput(ns("multi_path_diagram"))),
          tabPanel("Path Coefficients Table", tableOutput(ns("multi_path_coef"))),
          tabPanel("Model Fit Summary", verbatimTextOutput(ns("multi_path_fit")))
        )
      }
    })
    
    # --- Observers for Path Analysis Trait Selection ---
    observeEvent(input$multi_path_dep, {
      req(multi_subtype_selected() == "path", multi_file_data())
      df <- multi_file_data()
      choices_numeric <- names(df)[sapply(df, is.numeric)]
      indep_choices <- setdiff(choices_numeric, input$multi_path_dep)
      updateSelectInput(session, "multi_path_indep", choices = indep_choices)
    })
    
    # --- Analysis Logic ---
    
    # PCA
    observeEvent(input$multi_run_pca, {
      req(multi_file_data(), input$multi_pca_traits)
      df <- multi_file_data()
      df_pca_numeric <- df %>%
        dplyr::select(all_of(input$multi_pca_traits)) %>%
        na.omit()
      
      tryCatch({
        res.pca <- FactoMineR::PCA(df_pca_numeric, graph = FALSE)
        pca_results(list(pca = res.pca))
        output$multi_pca_biplot <- renderPlot({ fviz_pca_ind(res.pca, repel = TRUE) })
        output$multi_pca_scree <- renderPlot({ fviz_screeplot(res.pca, addlabels = TRUE) })
        output$multi_pca_varcontrib <- renderPlot({ fviz_pca_var(res.pca) })
        output$multi_pca_eigen <- renderTable({ round(factoextra::get_eigenvalue(res.pca), 2) }, rownames = TRUE)
        output$multi_pca_status <- renderUI({ span(style = "color: green;", icon("check"), " PCA Completed") })
      }, error = function(e) {
        showModal(modalDialog(title = "PCA Error", e$message))
      })
    })
    
    # Correlation
    observeEvent(input$multi_run_corr, {
      req(multi_file_data(), input$multi_corr_traits)
      df <- multi_file_data()
      df_corr <- df %>%
        dplyr::select(all_of(input$multi_corr_traits)) %>%
        na.omit()
      corr_data(df_corr)
      
      tryCatch({
        output$multi_corr_corrplot <- renderPlot({
          corrplot(cor(df_corr), method = "number", type = "upper", order = "hclust")
        })
        output$multi_corr_pairsplot <- renderPlot({
          PerformanceAnalytics::chart.Correlation(df_corr, histogram = FALSE)
        })
        output$multi_corr_status <- renderUI({ span(style = "color: green;", icon("check"), " Correlation Completed") })
      }, error = function(e) {
        showModal(modalDialog(title = "Correlation Error", e$message))
      })
    })
    
    # Path Analysis
    observeEvent(input$multi_run_path, {
      req(multi_file_data(), input$multi_path_dep, input$multi_path_indep)
      df <- multi_file_data()
      dep <- input$multi_path_dep
      indep <- input$multi_path_indep
      
      if (length(indep) < 2) {
        showModal(modalDialog(title = "Selection Error", "Please select at least two Independent Traits."))
        return()
      }
      
      df_path <- df[, c(dep, indep), drop = FALSE] %>% na.omit()
      model_str <- paste0(dep, " ~ ", paste(indep, collapse = " + "))
      
      tryCatch({
        fit <- lavaan::sem(model_str, data = df_path, meanstructure = TRUE)
        path_results(fit)
        
        output$multi_path_diagram <- renderPlot({
          semPlot::semPaths(fit, whatLabels = "std", layout = "tree", 
                            edge.label.cex = 1.1, sizeMan = 6,
                            fade = FALSE, posCol = "darkgreen", negCol = "darkred")
        })
        output$multi_path_coef <- renderTable({
          subset(lavaan::parameterEstimates(fit, standardized = TRUE), op == "~")
        }, rownames = FALSE)
        output$multi_path_fit <- renderPrint({
          summary(fit, fit.measures = TRUE, standardized = TRUE)
        })
        output$multi_path_status <- renderUI({ span(style = "color: green;", icon("check"), " Path Analysis Completed") })
      }, error = function(e) {
        showModal(modalDialog(title = "Path Analysis Error", e$message))
      })
    })
    
    # --- Download Handler ---
    output$multi_download <- downloadHandler(
      filename = function() {
        paste0("Multivariate_Results_", multi_subtype_selected(), "_", Sys.Date(), ".zip")
      },
      content = function(file) {
        tmp_dir <- tempdir()
        files <- c()
        
        # Save PCA results
        if (multi_subtype_selected() == "pca" && !is.null(pca_results()$pca)) {
          pca_obj <- pca_results()$pca
          pdf(file.path(tmp_dir, "PCA_Biplot.pdf")); print(fviz_pca_ind(pca_obj, repel = TRUE)); dev.off()
          pdf(file.path(tmp_dir, "PCA_Scree.pdf")); print(fviz_screeplot(pca_obj, addlabels = TRUE)); dev.off()
          pdf(file.path(tmp_dir, "PCA_Contributions.pdf")); print(fviz_pca_var(pca_obj)); dev.off()
          write.csv(factoextra::get_eigenvalue(pca_obj), file.path(tmp_dir, "PCA_Eigenvalues.csv"))
          files <- c(files, file.path(tmp_dir, "PCA_Biplot.pdf"), file.path(tmp_dir, "PCA_Scree.pdf"),
                     file.path(tmp_dir, "PCA_Contributions.pdf"), file.path(tmp_dir, "PCA_Eigenvalues.csv"))
        }
        
        # Save Correlation results
        if (multi_subtype_selected() == "correlation" && !is.null(corr_data())) {
          pdf(file.path(tmp_dir, "Correlation_MatrixPlot.pdf")); corrplot(cor(corr_data()), method="number", type="upper"); dev.off()
          pdf(file.path(tmp_dir, "Correlation_Pairs.pdf")); PerformanceAnalytics::chart.Correlation(corr_data(), histogram=FALSE); dev.off()
          write.csv(cor(corr_data()), file.path(tmp_dir, "Correlation_Matrix.csv"))
          files <- c(files, file.path(tmp_dir, "Correlation_MatrixPlot.pdf"), file.path(tmp_dir, "Correlation_Pairs.pdf"),
                     file.path(tmp_dir, "Correlation_Matrix.csv"))
        }
        
        # Save Path Analysis results
        if (multi_subtype_selected() == "path" && !is.null(path_results())) {
          fit <- path_results()
          pdf(file.path(tmp_dir, "Path_Diagram.pdf"))
          semPlot::semPaths(fit, "std", layout="tree", fade=FALSE, posCol="darkgreen", negCol="darkred")
          dev.off()
          write.csv(lavaan::parameterEstimates(fit, standardized=TRUE), file.path(tmp_dir, "Path_Coefficients.csv"))
          capture.output(summary(fit, fit.measures=TRUE), file = file.path(tmp_dir, "Path_Summary.txt"))
          files <- c(files, file.path(tmp_dir, "Path_Diagram.pdf"), file.path(tmp_dir, "Path_Coefficients.csv"),
                     file.path(tmp_dir, "Path_Summary.txt"))
        }
        
        zip::zip(zipfile = file, files = files, mode = "cherry-pick")
      }
    )
    
  })
}
