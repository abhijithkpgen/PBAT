# R/design_experiment_module.R

# Required libraries for this module
library(shiny)
library(dplyr)
# library(DT) # DT is no longer needed
library(ggplot2)
library(plotly)

# ===================================================================
# CUSTOM DESIGN GENERATION FUNCTIONS
# ===================================================================

#' Generate a Randomized Complete Block Design (RCBD) Layout
#'
#' Creates an RCBD field book from a list of treatments and replications.
#'
#' @param treatments A character vector of treatment/genotype names.
#' @param reps The number of replications (blocks).
#' @return A data frame representing the field book.
custom_design_rcbd <- function(treatments, reps) {
  # Create the full list of plots for all replications
  full_design <- expand.grid(treatments = treatments, replication = 1:reps)
  
  # For each replication, randomize the order of treatments
  randomized_design <- full_design %>%
    group_by(replication) %>%
    mutate(treatments = sample(treatments)) %>%
    ungroup() %>%
    mutate(
      plots = 100 * replication + row_number() - (replication - 1) * length(treatments),
      block = replication # In a simple RCBD, blocks are equivalent to replications
    ) %>%
    select(plots, replication, block, treatments) %>%
    arrange(plots)
  
  return(randomized_design)
}


#' Generate an Augmented RCBD Layout (Robust Base R Version)
#'
#' Creates an Augmented RCBD field book using only base R to ensure robust randomization.
#'
#' @param test_treatments A character vector of the main treatments to be tested.
#' @param check_treatments A character vector of the check/control treatments.
#' @param blocks The total number of blocks in the experiment.
#' @return A data frame representing the field book.
custom_design_augmented_rcbd <- function(test_treatments, check_treatments, blocks) {
  t <- length(test_treatments)
  c <- length(check_treatments)
  
  # Add filler entries if necessary to ensure even distribution
  if (t %% blocks != 0) {
    n_fillers <- blocks - (t %% blocks)
    fillers <- paste0("Filler_", 1:n_fillers)
    test_treatments <- c(test_treatments, fillers)
    t <- length(test_treatments)
  }
  
  plots_per_block_test <- t / blocks
  
  # 1. Create a data frame with test entries assigned to blocks
  shuffled_tests <- sample(test_treatments)
  test_df <- data.frame(
    block = rep(1:blocks, each = plots_per_block_test),
    treatments = shuffled_tests,
    stringsAsFactors = FALSE
  )
  
  # 2. Create a data frame for the check treatments, replicated in each block
  check_df <- expand.grid(
    block = 1:blocks,
    treatments = check_treatments,
    stringsAsFactors = FALSE
  )
  
  # 3. Combine test and check data frames
  unrandomized_df <- rbind(test_df, check_df)
  
  # 4. For each block, shuffle the order of all treatments (test and checks together)
  randomized_list <- list()
  for (b in 1:blocks) {
    block_subset <- unrandomized_df[unrandomized_df$block == b, ]
    block_subset$treatments <- sample(block_subset$treatments)
    randomized_list[[b]] <- block_subset
  }
  
  # 5. Combine the randomized blocks back into one data frame
  field_book <- do.call(rbind, randomized_list)
  
  # 6. Assign plot numbers and Type, then finalize the order
  field_book <- field_book[order(field_book$block), ]
  field_book$plots <- 101:(100 + nrow(field_book))
  field_book$Type <- ifelse(field_book$treatments %in% check_treatments, "Check", "Test")
  
  field_book <- field_book[, c("plots", "block", "treatments", "Type")]
  
  return(field_book)
}


#' Generate an Alpha Lattice Design Layout
#'
#' Creates an Alpha Lattice design field book from scratch.
#'
#' @param treatments A character vector of treatment/genotype names.
#' @param k The number of plots per block (block size).
#' @param r The number of replications.
#' @return A data frame representing the field book.
custom_design_alpha <- function(treatments, k, r) {
  v <- length(treatments)
  if (v %% k != 0) {
    stop("The total number of treatments must be a multiple of the block size (k).")
  }
  
  b <- v / k # Number of blocks per replication
  
  # 1. Create a base plan (matrix of treatment numbers)
  base_plan <- matrix(1:v, nrow = k, byrow = TRUE)
  
  # 2. Generate the layout for all replications
  all_reps <- list()
  for (rep in 1:r) {
    if (rep == 1) {
      current_treatments <- sample(treatments)
    } else {
      permuted_indices <- as.vector(t(apply(base_plan, 1, sample)))
      current_treatments <- treatments[permuted_indices]
    }
    shuffled_blocks <- sample(1:b)
    rep_df <- data.frame(
      replication = rep,
      block = rep(shuffled_blocks, each = k),
      treatments = current_treatments
    )
    all_reps[[rep]] <- rep_df
  }
  
  # 3. Combine all replications and assign plot numbers
  field_book <- bind_rows(all_reps) %>%
    arrange(replication, block) %>%
    mutate(plots = 100 * replication + row_number() - (replication - 1) * v) %>%
    select(plots, replication, block, treatments) %>%
    arrange(plots)
  
  return(field_book)
}


# ===================================================================
# MODULE UI
# ===================================================================
designExperimentUI <- function(id) {
  ns <- NS(id)
  
  tabPanel("Design Your Trial",
           sidebarLayout(
             sidebarPanel(
               width = 3,
               h4("Trial Design Generator"),
               p("Select a design, map your genotype columns, set the parameters, and generate a field layout."),
               uiOutput(ns("design_sidebar_ui")) 
             ),
             mainPanel(
               width = 9,
               tabsetPanel(
                 tabPanel("Visualize Layout", 
                          h4("Interactive Field Plot"),
                          p("Hover over the plots to see details. Use the tools in the top-right corner to pan, zoom, and save a static image."),
                          plotly::plotlyOutput(ns("layout_plot"), height = "800px")
                 ),
                 tabPanel("Field Book", 
                          h4("Generated Field Layout Table"),
                          p("This table shows the randomized layout for your experiment. You can sort, search, and copy the data."),
                          tableOutput(ns("design_table")) # REPLACED DT::dataTableOutput
                 )
               )
             )
           )
  )
}

# ===================================================================
# MODULE SERVER
# ===================================================================
designExperimentServer <- function(id, home_inputs) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    rv <- reactiveValues(design_output = NULL, plot_object = NULL)
    
    geno_data <- reactive({
      req(home_inputs()$analysis_mode == "design_exp")
      home_inputs()$geno_data
    })
    
    active_design <- reactive({
      req(home_inputs()$analysis_mode == "design_exp")
      home_inputs()$design_type_home
    })
    
    output$design_sidebar_ui <- renderUI({
      df <- geno_data()
      design <- active_design()
      req(df, design)
      
      col_names <- names(df)
      
      tagList(
        h4("Step 1: Map Columns"),
        if (design %in% c("rcbd", "alpha")) {
          selectInput(ns("geno_col_std"), "Genotype Column", choices = col_names)
        },
        if (design == "augmented") {
          tagList(
            selectInput(ns("test_col_aug"), "Test Genotypes Column", choices = col_names, selected = col_names[1]),
            selectInput(ns("check_col_aug"), "Check Genotypes Column", choices = col_names, selected = col_names[2])
          )
        },
        hr(),
        h4("Step 2: Set Parameters"),
        if (design == "rcbd") {
          numericInput(ns("rcbd_reps"), "Number of Replications", value = 3, min = 2)
        } else if (design == "augmented") {
          tagList(
            numericInput(ns("aug_blocks"), "Number of Blocks", value = 5, min = 2),
            uiOutput(ns("aug_block_suggestion_ui"))
          )
        } else if (design == "alpha") {
          tagList(
            numericInput(ns("alpha_reps"), "Number of Replications", value = 2, min = 2),
            numericInput(ns("alpha_k"), "Number of Plots per Block (k)", value = 5, min = 2),
            helpText("Note: For Alpha Lattice, the total number of genotypes must be a multiple of 'k'.")
          )
        },
        hr(),
        h4("Step 3: Generate & Download"),
        actionButton(ns("generate_design"), "Generate Design", class = "btn-primary", style="width:100%;"),
        br(), br(),
        downloadButton(ns("download_csv"), "Download Field Book (CSV)", class="btn-success", style="width:100%;"),
        br(), br(),
        downloadButton(ns("download_pdf"), "Download Layout Plot (PDF)", class="btn-info", style="width:100%;")
      )
    })
    
    output$aug_block_suggestion_ui <- renderUI({
      design <- active_design()
      req(design == "augmented")
      df <- geno_data()
      req(df, input$test_col_aug, input$check_col_aug)
      
      n_test <- length(na.omit(df[[input$test_col_aug]]))
      n_check <- length(na.omit(df[[input$check_col_aug]]))
      
      # --- Suggestion for avoiding fillers ---
      get_factors <- function(n) {
        if (n <= 1) return(integer(0))
        x <- 1:floor(sqrt(n))
        factors <- x[n %% x == 0]
        unique(sort(c(factors, n/factors)))
      }
      
      divisors <- get_factors(n_test)
      suggested_divisors <- divisors[divisors > 1 & divisors <= (n_test/2)]
      
      suggestion_text_filler <- if (length(suggested_divisors) > 0) {
        paste("To avoid filler plots with", n_test, "test entries, use one of these block numbers:", paste(suggested_divisors, collapse=", "))
      } else {
        paste("With", n_test, "test entries, filler plots will likely be needed to ensure equal block sizes.")
      }
      
      # --- Suggestion for statistical power (degrees of freedom) ---
      min_df_error <- 12
      suggestion_text_stats <- NULL
      if (n_check > 1) {
        min_blocks_for_df <- ceiling(min_df_error / (n_check - 1)) + 1
        suggestion_text_stats <- paste0("For statistical validity with ", n_check, " checks, it is recommended to use at least ", min_blocks_for_df, " blocks to achieve a reliable error estimate (>=12 df).")
      } else {
        suggestion_text_stats <- "Note: With only one check entry, the error degrees of freedom cannot be estimated from checks alone. Consider adding more checks for a more robust analysis."
      }
      
      # Combine the suggestions into a single UI element
      tagList(
        helpText(HTML(paste0("<b>Layout Tip:</b> ", suggestion_text_filler))),
        helpText(HTML(paste0("<b>Statistical Tip:</b> ", suggestion_text_stats)))
      )
    })
    
    
    observeEvent(input$generate_design, {
      
      df <- geno_data()
      design <- active_design()
      req(df, design)
      
      gen_names <- character(0)
      check_names <- character(0)
      
      tryCatch({
        if (design %in% c("rcbd", "alpha")) {
          req(input$geno_col_std)
          gen_names <- na.omit(df[[input$geno_col_std]])
        } else if (design == "augmented") {
          req(input$test_col_aug, input$check_col_aug)
          gen_names <- na.omit(df[[input$test_col_aug]])
          check_names <- na.omit(df[[input$check_col_aug]])
          if (length(check_names) == 0) {
            stop("Augmented design requires at least one check genotype.")
          }
        }
        
        if (length(gen_names) == 0) {
          stop("No valid test genotypes were found in the mapped column.")
        }
        
        layout <- switch(
          design,
          "rcbd" = custom_design_rcbd(treatments = gen_names, reps = input$rcbd_reps),
          "augmented" = custom_design_augmented_rcbd(test_treatments = gen_names, check_treatments = check_names, blocks = input$aug_blocks),
          "alpha" = custom_design_alpha(treatments = gen_names, k = input$alpha_k, r = input$alpha_reps)
        )
        
        # 1. Standardize column names
        plot_data <- layout
        names(plot_data)[names(plot_data) == "plots"] <- "Plot"
        names(plot_data)[names(plot_data) == "treatments"] <- "Genotype"
        names(plot_data)[names(plot_data) == "replication"] <- "Replication"
        names(plot_data)[names(plot_data) == "block"] <- "Block"
        
        # 2. Create Row and Column coordinates using Base R
        if ("Block" %in% names(plot_data)) {
          grouping_cols <- c("Replication", "Block")[c("Replication", "Block") %in% names(plot_data)]
          plot_data <- do.call(rbind, by(plot_data, plot_data[, grouping_cols, drop = FALSE], function(sub_df) {
            sub_df$Column <- 1:nrow(sub_df)
            sub_df
          }))
          
          if ("Replication" %in% names(plot_data)) {
            plot_data$Row <- as.integer(factor(paste(plot_data$Replication, plot_data$Block, sep="-")))
          } else {
            plot_data$Row <- as.integer(factor(plot_data$Block))
          }
        } else {
          plot_data$Row <- 1
          plot_data$Column <- 1:nrow(plot_data)
        }
        
        # 3. Create labels
        plot_data$box_label <- if("Replication" %in% names(plot_data)) {
          paste0(plot_data$Genotype, "\n(R", plot_data$Replication, ")")
        } else {
          plot_data$Genotype
        }
        
        plot_data$tooltip <- paste0(
          "Plot: ", plot_data$Plot,
          "<br>Genotype: ", plot_data$Genotype,
          "<br>Row: ", plot_data$Row,
          "<br>Column: ", plot_data$Column,
          if ("Block" %in% names(plot_data)) paste("<br>Block:", plot_data$Block) else "",
          if ("Replication" %in% names(plot_data)) paste("<br>Replication:", plot_data$Replication) else ""
        )
        
        # 4. Save final table for output (using robust base R)
        final_df <- plot_data[order(plot_data$Plot), ]
        final_df$Plot_Num <- 1:nrow(final_df)
        
        # Define final columns explicitly to avoid errors
        if (design %in% c("rcbd", "alpha")) {
          final_cols <- c("Plot_Num", "Plot", "Row", "Column", "Replication", "Block", "Genotype")
        } else { # Augmented
          final_cols <- c("Plot_Num", "Plot", "Row", "Column", "Block", "Genotype", "Type")
        }
        # Ensure all selected columns actually exist before subsetting
        final_cols_exist <- final_cols[final_cols %in% names(final_df)]
        rv$design_output <- final_df[, final_cols_exist]
        
        # 5. Create plot object
        p <- ggplot(plot_data, aes(x = as.factor(Column), y = as.factor(Row), text = tooltip, fill = as.factor(Genotype))) +
          geom_tile(color = "white", size = 0.5, width = 0.95, height = 0.95) +
          geom_text(aes(label = box_label), size = 2.5, color = "black") +
          labs(title = paste("Field Layout for", toupper(design)), x = "Column", y = "Row", fill = "Genotype") +
          theme_minimal(base_size = 14) +
          theme(panel.grid = element_blank(), legend.position = "none")
        
        if (design == "augmented") {
          check_data <- plot_data[plot_data$Genotype %in% check_names, ]
          if (nrow(check_data) > 0) {
            p <- p + geom_tile(
              data = check_data, aes(color = "Check Plots"), 
              fill = NA, linewidth = 1.2, width = 0.95, height = 0.95, inherit.aes = TRUE
            ) + scale_color_manual(name = "", values = c("Check Plots" = "black"))
          }
        }
        
        rv$plot_object <- p
        showNotification("Design and plot generated successfully!", type = "message")
        
      }, error = function(e) {
        showModal(modalDialog(title = "Error Generating Design", e$message))
        rv$design_output <- NULL
        rv$plot_object <- NULL
      })
    })
    
    # --- Render Outputs ---
    output$design_table <- renderTable({ # REPLACED DT::renderDataTable
      req(rv$design_output)
      rv$design_output
    })
    
    output$layout_plot <- plotly::renderPlotly({
      req(rv$plot_object)
      plotly::ggplotly(rv$plot_object, tooltip = "text")
    })
    
    # --- Download Handlers ---
    output$download_csv <- downloadHandler(
      filename = function() {
        paste0(active_design(), "_field_book_", Sys.Date(), ".csv")
      },
      content = function(file) {
        req(rv$design_output)
        write.csv(rv$design_output, file, row.names = FALSE)
      }
    )
    
    output$download_pdf <- downloadHandler(
      filename = function() {
        paste0(active_design(), "_layout_plot_", Sys.Date(), ".pdf")
      },
      content = function(file) {
        req(rv$plot_object)
        ggsave(file, plot = rv$plot_object, device = "pdf", width = 11, height = 8.5, units = "in")
      }
    )
    
  })
}
