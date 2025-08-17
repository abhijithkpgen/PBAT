# stability_analysis_module.R
#
# This file contains the complete UI and Server logic for the new Stability Analysis module,
# featuring AMMI and GGE analyses for evaluating genotype performance across environments.

# Required Libraries
library(shiny)
library(dplyr)
library(metan)
library(DT)
library(colourpicker)

# ===================================================================
# MODULE UI FUNCTION
# ===================================================================
stability_analysis_ui <- function(id) {
  ns <- NS(id)
  
  tabPanel("Stability Analysis",
           sidebarLayout(
             sidebarPanel(
               width = 3,
               uiOutput(ns("stab_sidebar"))
             ),
             mainPanel(
               width = 9,
               uiOutput(ns("stab_mainpanel"))
             )
           )
  )
}

# ===================================================================
# MODULE SERVER FUNCTION
# ===================================================================
stability_analysis_server <- function(id, shared_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # --- Reactive Values to store results ---
    ammi_results <- reactiveVal(NULL)
    gge_results  <- reactiveVal(NULL)
    
    # --- Reactives to access shared data ---
    stab_file_data <- reactive({ shared_data$file_data })
    stab_subtype_selected <- reactive({ shared_data$stab_subtype })
    
    # --- Dynamic Sidebar UI ---
    output$stab_sidebar <- renderUI({
      req(stab_subtype_selected(), stab_file_data())
      
      df <- stab_file_data()
      choices_numeric <- names(df)[sapply(df, is.numeric)]
      choices_all <- names(df)
      
      step_block <- function(label, ...) {
        tags$div(tags$b(label), br(), ..., style = "margin-bottom: 20px;")
      }
      
      div(style = "color: #142850; font-size: 15px;",
          if (stab_subtype_selected() == "ammi") {
            tagList(
              step_block("Step 1: Select Columns for AMMI",
                         selectInput(ns("stab_ammi_genotype"), "Genotype Column", choices = choices_all),
                         selectInput(ns("stab_ammi_location"), "Location Column", choices = choices_all),
                         selectInput(ns("stab_ammi_rep"), "Replication Column", choices = choices_all)
              ),
              step_block("Step 2: Select Trait",
                         selectInput(ns("stab_ammi_trait"), "Trait", choices = choices_numeric)
              ),
              step_block("Step 3: Plot Options",
                         colourInput(ns("stab_ammi_col_gen"), "Genotype Color", value = "blue"),
                         colourInput(ns("stab_ammi_col_env"), "Environment Color", value = "red")
              ),
              step_block("Step 4: Run and Download",
                         actionButton(ns("stab_run_ammi"), "Run AMMI Analysis", class = "btn btn-success"),
                         uiOutput(ns("stab_ammi_status")), br(),
                         downloadButton(ns("stab_download"), "Download Results (ZIP)", class = "btn btn-primary")
              )
            )
          } else if (stab_subtype_selected() == "gge") {
            tagList(
              step_block("Step 1: Map Data Columns",
                         selectInput(ns("stab_gge_genotype"), "Genotype Column", choices = choices_all),
                         selectInput(ns("stab_gge_location"), "Location Column", choices = choices_all),
                         checkboxInput(ns("stab_gge_is_replicated"), "Is the data replicated?", value = FALSE),
                         conditionalPanel(
                           condition = paste0("input['", ns("stab_gge_is_replicated"), "']"),
                           selectInput(ns("stab_gge_rep"), "Replication Column", choices = choices_all)
                         )
              ),
              step_block("Step 2: Select Trait",
                         selectInput(ns("stab_gge_trait"), "Trait", choices = choices_numeric)
              ),
              step_block("Step 3: GGE Model Options",
                         selectInput(ns("stab_gge_centering"), "Centering Method", choices = c("none", "environment", "global"), selected = "environment"),
                         selectInput(ns("stab_gge_scaling"), "Scaling Method", choices = c("none", "sd"), selected = "sd"),
                         selectInput(ns("stab_gge_svp"), "SVP Method", choices = c("genotype", "environment", "symmetrical"), selected = "symmetrical")
              ),
              step_block("Step 4: Plot Options",
                         colourInput(ns("stab_gge_col_gen"), "Genotype Color", value = "blue"),
                         colourInput(ns("stab_gge_col_env"), "Environment Color", value = "red")
              ),
              step_block("Step 5: Run and Download",
                         actionButton(ns("stab_run_gge"), "Run GGE Biplot", class = "btn btn-success"),
                         uiOutput(ns("stab_gge_status")), br(),
                         downloadButton(ns("stab_download"), "Download Results (ZIP)", class = "btn btn-primary")
              )
            )
          }
      )
    })
    
    # --- Dynamic Main Panel UI ---
    output$stab_mainpanel <- renderUI({
      req(stab_subtype_selected())
      if (stab_subtype_selected() == "ammi") {
        tabsetPanel(
          tabPanel("AMMI ANOVA", verbatimTextOutput(ns("stab_ammi_anova"))),
          tabPanel("AMMI Biplot", plotOutput(ns("stab_ammi_biplot"))),
          tabPanel("Yield Stability Plot (WAASB)", plotOutput(ns("stab_ammi_waasb"))),
          tabPanel("AMMI Stability Ranks", DT::dataTableOutput(ns("stab_ammi_ranks")))
        )
      } else if (stab_subtype_selected() == "gge") {
        tabsetPanel(
          tabPanel("Which Won Where", plotOutput(ns("stab_gge_plot_type1"))),
          tabPanel("Mean vs Stability", plotOutput(ns("stab_gge_plot_type2"))),
          tabPanel("Representativeness vs Discriminativeness", plotOutput(ns("stab_gge_plot_type3")))
        )
      }
    })
    
    # --- Analysis Logic ---
    
    # AMMI Analysis
    observeEvent(input$stab_run_ammi, {
      req(stab_file_data(), input$stab_ammi_genotype, input$stab_ammi_location,
          input$stab_ammi_rep, input$stab_ammi_trait)
      
      df <- stab_file_data()
      df_ammi <- df %>%
        dplyr::select(
          gen  = all_of(input$stab_ammi_genotype),
          env  = all_of(input$stab_ammi_location),
          rep  = all_of(input$stab_ammi_rep),
          resp = all_of(input$stab_ammi_trait)
        ) %>% na.omit()
      
      tryCatch({
        ammi_fit  <- metan::performs_ammi(df_ammi, env = env, gen = gen, rep = rep, resp = resp, verbose = FALSE)
        waasb_fit <- metan::waasb(df_ammi, gen, env, rep, resp, verbose = FALSE)
        ammi_results(list(ammi = ammi_fit, waasb = waasb_fit))
        
        output$stab_ammi_anova <- renderPrint({ ammi_fit[[1]]$ANOVA })
        output$stab_ammi_biplot <- renderPlot({ 
          metan::plot_scores(ammi_fit, 
                             type = 2,
                             col.gen = input$stab_ammi_col_gen,
                             col.env = input$stab_ammi_col_env) 
        })
        output$stab_ammi_waasb <- renderPlot({ plot(waasb_fit) })
        
        output$stab_ammi_ranks <- DT::renderDataTable({
          stab_tbl <- metan::ammi_indexes(ammi_fit)[[1]]
          preferred <- c("GEN","Y","Y_R", "ASV","ASV_R","ssiASV", "SIPC","SIPC_R","ssiSIPC",
                         "EV","EV_R","ssiEV", "Za","Za_R","ssiZa", "ASTAB","ASTAB_R","ssiASTAB",
                         "ASI","WAASY","WAASB")
          if (any(grepl("^ssi", names(stab_tbl)))) {
            dplyr::select(stab_tbl, dplyr::any_of(preferred), dplyr::matches("^ssi"))
          } else {
            dplyr::select(stab_tbl, dplyr::any_of(preferred))
          }
        }, options = list(scrollX = TRUE))
        
        output$stab_ammi_status <- renderUI({ span(style = "color: green;", icon("check"), " AMMI Completed") })
      }, error = function(e) {
        showModal(modalDialog(title = "AMMI Error", e$message))
      })
    })
    
    # GGE Biplot
    observeEvent(input$stab_run_gge, {
      req(stab_file_data(), input$stab_gge_genotype, input$stab_gge_location, input$stab_gge_trait)
      df <- stab_file_data()
      
      # Prepare the data frame
      df_gge <- df %>%
        dplyr::select(
          gen = all_of(input$stab_gge_genotype),
          env = all_of(input$stab_gge_location),
          resp = all_of(input$stab_gge_trait),
          # Conditionally select replication column
          if (input$stab_gge_is_replicated) all_of(input$stab_gge_rep) else NULL
        ) %>%
        na.omit()
      
      # If data is replicated, compute means
      if (input$stab_gge_is_replicated) {
        df_gge <- df_gge %>%
          group_by(gen, env) %>%
          summarise(resp = mean(resp, na.rm = TRUE), .groups = "drop")
      }
      
      tryCatch({
        gge_model <- metan::gge(df_gge, 
                                env = env, 
                                gen = gen, 
                                resp = resp,
                                centering = input$stab_gge_centering,
                                scaling = input$stab_gge_scaling,
                                svp = input$stab_gge_svp)
        gge_results(gge_model)
        
        plot_gge_with_colors <- function(type) {
          plot(gge_model, 
               type = type,
               col.gen = input$stab_gge_col_gen,
               col.env = input$stab_gge_col_env)
        }
        
        output$stab_gge_plot_type1 <- renderPlot({ plot_gge_with_colors(3) })
        output$stab_gge_plot_type2 <- renderPlot({ plot_gge_with_colors(2) })
        output$stab_gge_plot_type3 <- renderPlot({ plot_gge_with_colors(4) })
        
        output$stab_gge_status <- renderUI({ span(style = "color: green;", icon("check"), " GGE Completed") })
      }, error = function(e) {
        showModal(modalDialog(title = "GGE Error", e$message))
      })
    })
    
    # --- Download Handler ---
    output$stab_download <- downloadHandler(
      filename = function() {
        paste0("Stability_Results_", stab_subtype_selected(), "_", Sys.Date(), ".zip")
      },
      content = function(file) {
        tmp_dir <- tempdir()
        files <- c()
        
        if (stab_subtype_selected() == "ammi" && !is.null(ammi_results())) {
          res <- ammi_results()
          capture.output(res$ammi[[1]]$ANOVA, file = file.path(tmp_dir, "AMMI_ANOVA.txt"))
          write.csv(metan::ammi_indexes(res$ammi)[[1]], file.path(tmp_dir, "AMMI_Stability_Ranks_Full.csv"), row.names = FALSE)
          
          # Save plots with user-defined colors
          pdf(file.path(tmp_dir, "AMMI_Biplot.pdf"))
          print(plot_scores(res$ammi, type = 2, col.gen = input$stab_ammi_col_gen, col.env = input$stab_ammi_col_env))
          dev.off()
          
          pdf(file.path(tmp_dir, "AMMI_WAASB_Plot.pdf")); print(plot(res$waasb)); dev.off()
          
          files <- c(files, file.path(tmp_dir, "AMMI_ANOVA.txt"), file.path(tmp_dir, "AMMI_Stability_Ranks_Full.csv"), 
                     file.path(tmp_dir, "AMMI_Biplot.pdf"), file.path(tmp_dir, "AMMI_WAASB_Plot.pdf"))
        }
        
        if (stab_subtype_selected() == "gge" && !is.null(gge_results())) {
          for (i in c(2, 3, 4)) {
            p_file <- file.path(tmp_dir, paste0("GGE_Plot_Type", i, ".pdf"))
            pdf(p_file, width = 7, height = 6)
            print(plot(gge_results(), type = i, col.gen = input$stab_gge_col_gen, col.env = input$stab_gge_col_env))
            dev.off()
            files <- c(files, p_file)
          }
        }
        
        zip::zipr(zipfile = file, files = files, recurse = FALSE)
      }
    )
    
  })
}
