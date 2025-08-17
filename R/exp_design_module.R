# R/analysis_module.R

# ===================================================================
# -------- Block 1: Module UI Definition --------
# ===================================================================
#' Experimental Design Analysis UI
#'
#' This function defines the user interface for the two main analysis tabs ("Analysis 1" and "Analysis 2").
#' It returns a list of tabPanels to be inserted into the main app's navbarPage.
#'
#' @param id A unique identifier for the module.
#'
analysisUI <- function(id) {
  ns <- NS(id) # Create the namespace
  
  list(
    # --- UI for Analysis 1 Tab ---
    tabPanel("Analysis 1",
             sidebarLayout(
               sidebarPanel(width = 4, uiOutput(ns("dynamic_sidebar"))),
               mainPanel(id = ns("main_panel_eda"), uiOutput(ns("dynamic_mainpanel"))) 
             )
    ),
    # --- UI for Analysis 2 Tab ---
    tabPanel("Analysis 2",
             sidebarLayout(
               sidebarPanel(width = 4,
                            selectInput(ns("gge_trait"), "Trait for GGE Biplot", choices = NULL),
                            actionButton(ns("run_gge"), "Run GGE Biplot", class = "btn btn-success"),
                            uiOutput(ns("gge_status")),
                            hr(),
                            checkboxGroupInput(ns("multi_traits"), "Select Traits for PCA / Correlation", choices = NULL),
                            actionButton(ns("run_pca"), "Run PCA", class = "btn btn-primary"),
                            uiOutput(ns("pca_status")),
                            br(),
                            actionButton(ns("run_corr"), "Run Correlation Plot", class = "btn btn-secondary"),
                            uiOutput(ns("corr_status")),
                            downloadButton(ns("download_analysis2"), "Download Analysis 2 ZIP", class = "btn btn-success")
               ),
               mainPanel(
                 tabsetPanel(
                   id = ns("analysis2_tabs"),
                   tabPanel(" GGE Biplot",
                            h4(" Which Won Where"), plotOutput(ns("gge_plot_type1")),
                            h4(" Mean vs Stability"), plotOutput(ns("gge_plot_type2")),
                            h4(" Representativeness vs Discriminativeness"), plotOutput(ns("gge_plot_type3"))
                   ),
                   tabPanel(" PCA Plot",
                            h4(" PCA Individual Biplot"), plotOutput(ns("pca_plot_biplot")),
                            h4(" Scree Plot"), plotOutput(ns("pca_plot_scree")),
                            h4(" Variable Contributions"), plotOutput(ns("pca_plot_varcontrib")),
                            h4(" PCA Summary Table"), tableOutput(ns("pca_table_summary"))
                   ),
                   tabPanel("Correlation Plot",
                            plotOutput(ns("corr_plot1")),
                            hr(),
                            uiOutput(ns("corr_interpretation_ui"))
                   )
                 )
               )
             )
    )
  )
  
}

# ===================================================================
# -------- Block 2: Module Server Definition --------
# ===================================================================
#' Experimental Design Analysis Server
#'
#' Contains all server logic for the EDA tabs.
#'
#' @param id A unique identifier for the module.
#' @param home_inputs A reactive list from the home module.
#'
analysisServer <- function(id, home_inputs) {
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    # ===================================================================
    # -------- Section 2.1: Reactive Values & Data Input --------
    # ===================================================================
    raw_data <- reactiveVal(NULL)
    active_design <- reactiveVal(NULL)
    descriptive_results <- reactiveVal(NULL)
    model_results <- reactiveVal(NULL)
    gge_results <- reactiveVal(NULL)
    pca_results <- reactiveVal(NULL)
    crd_results <- reactiveValues() 
    go_to_analysis2 <- reactiveVal(0)
    corr_interpretation <- reactiveVal(NULL)
    
    observeEvent(home_inputs(), {
      req(home_inputs()$analysis_mode == "eda")
      raw_data(home_inputs()$file_data)
      active_design(home_inputs()$design)
    })
    
    # ===================================================================
    # -------- Section 2.2: Helper Functions (Self-Contained) --------
    # ===================================================================
    
    # Helper function to generate text summary for correlations
    generate_correlation_interpretation <- function(corr_matrix, p_value_matrix, alpha = 0.05) {
      significant_correlations <- list()
      # Define thresholds for strength
      strength_levels <- list(
        strong = 0.7,
        moderate = 0.4
      )
      
      # Iterate through the upper triangle of the matrix to avoid duplicates
      for (i in 1:(nrow(p_value_matrix) - 1)) {
        for (j in (i + 1):ncol(p_value_matrix)) {
          p_val <- p_value_matrix[i, j]
          if (!is.na(p_val) && p_val < alpha) {
            corr_val <- corr_matrix[i, j]
            var1 <- rownames(corr_matrix)[i]
            var2 <- colnames(corr_matrix)[j]
            
            # Determine direction and strength
            direction <- if (corr_val > 0) "positive" else "negative"
            strength <- if (abs(corr_val) >= strength_levels$strong) "strong" else if (abs(corr_val) >= strength_levels$moderate) "moderate" else "weak"
            
            # Format p-value for display
            p_text <- if (p_val < 0.001) "p < 0.001" else paste0("p = ", round(p_val, 3))
            
            # Create the interpretation sentence
            sentence <- paste0("A significant ", tags$b(strength), " ", tags$b(direction), " correlation was observed between '",
                               var1, "' and '", var2, "' (r = ", sprintf("%.2f", corr_val), ", ", p_text, ").")
            
            significant_correlations[[length(significant_correlations) + 1]] <- tags$li(HTML(sentence))
          }
        }
      }
      
      if (length(significant_correlations) == 0) {
        return(tags$p("No significant correlations were found at the p < 0.05 level."))
      } else {
        return(tagList(
          tags$h4("Key Findings:"),
          tags$ul(significant_correlations)
        ))
      }
    }
    
    # --- ADD THIS to Section 2.2: Helper Functions ---
    generate_crd_anova_interpretation <- function(anova_df) {
      # Filter for significant terms (excluding Residuals)
      significant_terms <- anova_df %>%
        filter(term != "Residuals", p.value < 0.05) %>%
        pull(term)
      
      # Construct the interpretation sentence
      interpretation <- if (length(significant_terms) > 0) {
        # Create a formatted string of significant terms
        terms_str <- paste0("<b>", significant_terms, "</b>", collapse = ", ")
        if (length(significant_terms) > 1) {
          # Nicer formatting for multiple terms
          last_term_index <- max(gregexpr(",", terms_str)[[1]])
          terms_str <- paste0(substr(terms_str, 1, last_term_index - 1), " and ", substr(terms_str, last_term_index + 2, nchar(terms_str)))
        }
        paste("The ANOVA shows that the following term(s) had a statistically significant effect on the trait (p < 0.05):", terms_str, ".")
      } else {
        "The ANOVA shows that none of the factors or their interactions had a statistically significant effect on the trait (p < 0.05)."
      }
      
      # Return the interpretation wrapped in styled HTML
      tags$div(class = "alert alert-light", style = "margin-top: 15px; border-left: 4px solid #007bff;", HTML(interpretation))
    }
    
    
    # --- Block E8 Part A: EQUATION REPOSITORY ---
    model_equations <- list(
      rcbd_single_fixed = "Model Formula: trait ~ Genotype + (1|Block)",
      rcbd_single_random = "Model Formula: trait ~ (1|Genotype) + (1|Block)",
      rcbd_multi_fixed = "Model Formula: trait ~ Genotype + (1|Environment) + (1|Genotype:Environment) + (1|Block:Environment)",
      rcbd_multi_random = "Model Formula: trait ~ (1|Genotype) + (1|Environment) + (1|Genotype:Environment) + (1|Block:Environment)",
      augmentedrcbd_single_fixed = "Model Formula: trait ~ Genotype + (1|Block)",
      augmentedrcbd_single_random = "Model Formula: trait ~ (1|Genotype) + (1|Block)",
      augmentedrcbd_multi_fixed = "Model Formula: trait ~ Genotype + (1|Environment) + (1|Genotype:Environment) + (1|Block:Environment)",
      augmentedrcbd_multi_random = "Model Formula: trait ~ (1|Genotype) + (1|Environment) + (1|Genotype:Environment) + (1|Block:Environment)",
      alphalattice_single_fixed = "Model Formula: trait ~ Genotype + (1|Replication) + (1|Block:Replication)",
      alphalattice_single_random = "Model Formula: trait ~ (1|Genotype) + (1|Replication) + (1|Block:Replication)",
      alphalattice_multi_fixed = "Model Formula: trait ~ Genotype + (1|Environment) + (1|Genotype:Environment) + (1|Replication:Environment) + (1|Block:Replication:Environment)",
      alphalattice_multi_random = "Model Formula: trait ~ (1|Genotype) + (1|Environment) + (1|Genotype:Environment) + (1|Replication:Environment) + (1|Block:Replication:Environment)",
      default = "Model information not available."
    )
    get_equation_key <- function(design, trial_type, model_type) { trial_str <- if (trial_type == "Multi Environment") "multi" else "single"; model_str <- tolower(model_type); key <- paste(design, trial_str, model_str, sep = "_"); return(key) }
    add_significance_stars <- function(p_values) { sapply(p_values, function(p) { if (is.na(p)) return(""); if (p < 0.001) return("***"); if (p < 0.01) return("**"); if (p < 0.05) return("*"); return("ns") }) }
    
    # --- Block E8 Part B: ANOVA FUNCTION ---
    extract_custom_anova <- function(df, trait, entry_col, block_col, rep_col, env_col, design) {
      tryCatch({
        bt <- function(x) paste0("`", x, "`"); base_formula <- paste(bt(trait), "~", bt(entry_col))
        if (!is.null(env_col)) { base_formula <- paste(base_formula, "+", bt(env_col), "+", bt(entry_col), ":", bt(env_col)) }
        if (design == "alphalattice") { req(rep_col); if (!is.null(env_col)) { base_formula <- paste(base_formula, "+", bt(env_col), "/", bt(rep_col), "/", bt(block_col)) } else { base_formula <- paste(base_formula, "+", bt(rep_col), "/", bt(block_col)) }
        } else { if (!is.null(env_col)) { base_formula <- paste(base_formula, "+", bt(env_col), "/", bt(block_col)) } else { base_formula <- paste(base_formula, "+", bt(block_col)) } }
        fit_anova <- lm(as.formula(base_formula), data = df)
        anova_raw <- as.data.frame(stats::anova(fit_anova)) %>% tibble::rownames_to_column("Source")
        anova_filtered <- anova_raw %>% dplyr::filter(Source == entry_col) %>%
          dplyr::rename(DF = "Df", SS = "Sum Sq", MSS = "Mean Sq", F_value = "F value", p.value = "Pr(>F)") %>%
          dplyr::mutate(Significance = add_significance_stars(p.value)) %>%
          dplyr::select(Source, DF, SS, MSS, F_value, p.value, Significance)
        return(anova_filtered %>% dplyr::mutate(across(where(is.numeric), ~ round(., 3))))
      }, error = function(e) { data.frame(Message = "ANOVA could not be computed.", Error = e$message) })
    }
    
    # --- Block E8 Part C: MAIN ANALYSIS ENGINE FUNCTION ---
    run_model_and_extract <- function(df, trait, vc_formula_str, random_formula_str, 
                                      model_type, entry_col, env_col, block_col, rep_col, design) {
      results <- list()
      
      round_df <- function(d) {
        if(is.data.frame(d) && ncol(d) > 0) {
          return(d %>% dplyr::mutate(across(where(is.numeric), ~ round(., 3))))
        }
        return(d)
      }
      
      process_var_comps <- function(fit_model) {
        var_corr_df <- as.data.frame(lme4::VarCorr(fit_model))
        simplified_df <- var_corr_df %>%
          dplyr::select(Source = grp, Variance = vcov) %>%
          dplyr::mutate(Source = ifelse(is.na(Source), "Residual", Source)) %>%
          dplyr::filter(!is.na(Variance)) %>%
          dplyr::mutate(Percentage = scales::percent(Variance / sum(Variance), accuracy = 0.1)) %>%
          dplyr::select(Source, Variance, Percentage)
        return(round_df(simplified_df))
      }
      
      process_lrt_table <- function(fit_model) {
        lrt_df <- tibble::rownames_to_column(as.data.frame(lmerTest::rand(fit_model)), "Source")
        lrt_df_modified <- lrt_df %>%
          dplyr::mutate(Source = ifelse(Source == "<none>", "Baseline Model (for comparison)", Source))
        return(round_df(lrt_df_modified))
      }
      
      generate_lrt_interpretation <- function(lrt_table, entry_col_name, env_col_name) {
        significant_effects <- lrt_table %>%
          dplyr::filter(`Pr(>Chisq)` < 0.05, Source != "Baseline Model (for comparison)") %>%
          dplyr::pull(Source)
        
        if (length(significant_effects) == 0) {
          return(HTML("The analysis suggests that none of the random effects significantly contribute to the model's fit (at the p < 0.05 level)."))
        }
        
        term_map <- list(
          "Replication" = "Replication",
          "Block" = "Block",
          "Rep" = "Replication"
        )
        term_map[[entry_col_name]] <- "Genotype"
        if(!is.null(env_col_name)) term_map[[env_col_name]] <- "Location (Environment)"
        if(!is.null(env_col_name)) term_map[[paste0(entry_col_name, ":", env_col_name)]] <- "Genotype x Environment interaction"
        if(!is.null(env_col_name)) term_map[[paste0("Rep", ":", env_col_name)]] <- "Replication within Environment"
        term_map[[paste0("Block", ":", "Rep")]] <- "Block within Replication"
        if(!is.null(env_col_name)) term_map[[paste0("Block", ":", env_col_name)]] <- "Block within Environment"
        if(!is.null(env_col_name)) term_map[[paste0("Block", ":Rep:", env_col_name)]] <- "Block within Replication & Environment"
        
        descriptive_names <- sapply(significant_effects, function(effect) {
          clean_effect <- gsub("[()1| ]", "", effect)
          mapped_name <- term_map[[clean_effect]]
          return(ifelse(is.null(mapped_name), clean_effect, paste0(mapped_name, " (", clean_effect, ")")))
        })
        
        format_effect_list <- function(effects) {
          n <- length(effects)
          if (n == 0) return("")
          if (n == 1) return(effects[1])
          if (n == 2) return(paste(effects, collapse = " and "))
          return(paste(paste(effects[1:(n-1)], collapse = ", "), effects[n], sep = ", and "))
        }
        
        effects_list_str <- format_effect_list(descriptive_names)
        interpretation <- paste0("The analysis indicates that the variance from the following random effect(s) is statistically significant (p < 0.05): ", tags$b(effects_list_str), ". This suggests that these sources of variation play a major role in trait expression.")
        return(HTML(interpretation))
      }
      
      generate_anova_interpretation <- function(anova_table) {
        if (nrow(anova_table) == 0 || is.null(anova_table$p.value)) return("")
        p_val <- anova_table$p.value[1]; if (is.na(p_val)) return("")
        significance_level <- anova_table$Significance[1]; p_val_text <- paste0("(p = ", round(p_val, 3), ")")
        if (p_val < 0.05) {
          signif_word <- switch(significance_level, "***" = "highly significant", "**" = "significant", "*" = "significant", "significant")
          interpretation <- paste("The ANOVA test shows a", tags$b(signif_word), "difference among genotypes", p_val_text, ". This indicates genuine genetic differences.")
        } else {
          interpretation <- paste("The ANOVA test shows", tags$b("no significant"), "difference among genotypes", p_val_text, ". Differences may be due to chance.")
        }
        return(HTML(interpretation))
      }
      
      if (model_type == "Fixed") {
        fit_vc <- lmerTest::lmer(as.formula(vc_formula_str), data = df)
        results$is_singular <- lme4::isSingular(fit_vc)
        results$singularity_message <- if (results$is_singular) "Warning: Model fit is singular." else ""
        results$anova_table <- extract_custom_anova(df, trait, entry_col, block_col, rep_col, env_col, design)
        results$anova_interpretation <- generate_anova_interpretation(results$anova_table)
        results$var_comps <- tryCatch({ process_var_comps(fit_vc) }, error = function(e) data.frame(Message="VarComps error"))
        lrt_table_obj <- tryCatch({ process_lrt_table(fit_vc) }, error = function(e) NULL)
        if(!is.null(lrt_table_obj)) {
          results$lrt_table <- lrt_table_obj
          results$lrt_interpretation <- generate_lrt_interpretation(lrt_table_obj, entry_col, env_col)
        } else {
          results$lrt_table <- data.frame(Message="LRT not computed."); results$lrt_interpretation <- ""
        }
        results$blue_table <- tryCatch({
          emm_comb <- emmeans::emmeans(fit_vc, specs = as.formula(paste0("~", entry_col))); 
          summary_comb <- as.data.frame(summary(emm_comb)) %>% dplyr::select(all_of(entry_col), BLUE_Combined = "emmean", SE_Combined = "SE"); 
          final_blue_table <- summary_comb
          if (!is.null(env_col) && input$env_wise) {
            env_levels <- unique(as.character(df[[env_col]])); 
            env_blues_list <- lapply(env_levels, function(env) {
              df_sub <- df[df[[env_col]] == env, ]; 
              bt_trait <- paste0("`", trait, "`"); 
              bt_entry_col <- paste0("`", entry_col, "`"); 
              bt_rep_col <- if(!is.null(rep_col)) paste0("`", rep_col, "`") else NULL; 
              bt_block_col <- paste0("`", block_col, "`")
              ss_formula_str <- if(design == "alphalattice") { paste(bt_trait, "~", bt_entry_col, "+ (1|", bt_rep_col, ") + (1|", bt_block_col, ":", bt_rep_col, ")") } else { paste(bt_trait, "~", bt_entry_col, "+ (1|", bt_block_col, ")") }
              fit_ss <- tryCatch(lmerTest::lmer(as.formula(ss_formula_str), data=df_sub), error=function(e) NULL)
              if(is.null(fit_ss)) return(NULL)
              emm_ss <- emmeans::emmeans(fit_ss, specs = as.formula(paste0("~", entry_col))); 
              as.data.frame(summary(emm_ss)) %>% dplyr::select(all_of(entry_col), !!paste0("BLUE_", env) := emmean)
            }); 
            env_blues_wide <- purrr::reduce(Filter(Negate(is.null), env_blues_list), dplyr::full_join, by = entry_col); 
            final_blue_table <- dplyr::left_join(summary_comb, env_blues_wide, by = entry_col)
          }
          round_df(final_blue_table)
        }, error = function(e) data.frame(Message="BLUEs could not be calculated.", Error = e$message))
        
      } else { # Random Model
        fit_rand <- lmerTest::lmer(as.formula(random_formula_str), data = df)
        results$is_singular <- lme4::isSingular(fit_rand)
        results$singularity_message <- if(results$is_singular) "Warning: Model fit is singular." else ""
        lrt_table_obj <- tryCatch({ process_lrt_table(fit_rand) }, error = function(e) NULL)
        if(!is.null(lrt_table_obj)) {
          results$lrt_table <- lrt_table_obj
          results$lrt_interpretation <- generate_lrt_interpretation(lrt_table_obj, entry_col, env_col)
        } else {
          results$lrt_table <- data.frame(Message="LRT not computed."); results$lrt_interpretation <- ""
        }
        results$var_comps <- tryCatch({ process_var_comps(fit_rand) }, error = function(e) data.frame(Message = "VarComps error"))
        results$blup_table <- tryCatch({
          intercept <- lme4::fixef(fit_rand)["(Intercept)"]; 
          blups_comb <- lme4::ranef(fit_rand)[[entry_col]]; 
          blups_comb$BLUP_Combined <- intercept + blups_comb[,1]
          blups_comb <- tibble::rownames_to_column(blups_comb, var=entry_col) %>% dplyr::select(all_of(entry_col), "BLUP_Combined"); 
          final_blup_table <- blups_comb
          if (!is.null(env_col) && input$env_wise) {
            gxe_term <- paste0(entry_col, ":", env_col); 
            blups_ind <- lme4::ranef(fit_rand)[[gxe_term]]; 
            blups_ind$BLUP <- intercept + blups_ind[,1]
            blups_ind <- tibble::rownames_to_column(blups_ind, var="Var") %>% tidyr::separate("Var", into=c(entry_col, env_col), sep=":"); 
            wide_blups <- blups_ind %>% dplyr::select(all_of(entry_col), all_of(env_col), "BLUP") %>% tidyr::pivot_wider(names_from=all_of(env_col), values_from="BLUP", names_prefix="BLUP_"); 
            final_blup_table <- dplyr::left_join(blups_comb, wide_blups, by = entry_col)
          }
          round_df(final_blup_table)
        }, error = function(e) data.frame(Message = "BLUPs could not be calculated.", Error = e$message))
      }
      return(results)
    }
    
    #==================================================================
    # -------- Section 2.3: Dynamic UI Rendering --------
    #==================================================================
    
    # --- Block E2: EDA Dynamic Sidebar UI ---
    output$dynamic_sidebar <- renderUI({
      df <- raw_data()
      design <- active_design()
      if (is.null(df) || is.null(design)) return(h4("Please select EDA on the Home page and proceed."))
      
      nms <- names(df)
      numeric_cols <- nms[sapply(df, is.numeric)]
      design_lwr <- tolower(gsub("\\s+", "", design))
      step_header <- function(step, txt) h4(div(style="margin-bottom:6px;margin-top:10px;color:#3a5a40;", paste0("Step ", step, ": ", txt)))
      
      if (design_lwr == "crd") {
        wellPanel(
          step_header(1, "Specify Number of Factors"),
          numericInput(ns("crd_n_factors"), NULL, value = 2, min = 1, max = 3, step = 1),
          uiOutput(ns("crd_factor_ui")),
          step_header(2, "Select Traits"),
          checkboxGroupInput(ns("crd_traits"), NULL, choices = numeric_cols),
          step_header(3, "Run Analysis"),
          actionButton(ns("crd_descriptive"), "Run Descriptive Summary", class = "btn btn-primary mb-2"),
          actionButton(ns("run_crd_anova"), "Run ANOVA + Post Hoc", class = "btn btn-primary mb-2"),
          actionButton(ns("run_crd_interact"), "Plot Interactions", class = "btn btn-info mb-2"),
          step_header(4, "Download Results"),
          downloadButton(ns("download_crd_zip"), "Download CRD Results (ZIP)", class = "btn btn-success")
        )
      } else if (design_lwr %in% c("augmentedrcbd", "rcbd", "alphalattice")) {
        wellPanel(
          step_header(1, "Select Trial Type"),
          selectInput(ns("trial_type"), NULL, choices = c("Single Environment", "Multi Environment"), selected = "Single Environment"),
          step_header(2, "Map Data Columns"),
          selectInput(ns("entry"),  "Genotype Column", choices = nms),
          if (design_lwr == "alphalattice") selectInput(ns("rep"), "Replication Column", choices = nms),
          selectInput(ns("block"),  "Block Column", choices = nms),
          conditionalPanel(
            condition = paste0("input['", ns("trial_type"), "'] == 'Multi Environment'"),
            selectInput(ns("env"), "Environment Column", choices = nms)
          ),
          if (design_lwr == "augmentedrcbd") {
            list(
              selectInput(ns("check_col"), "Check/Entry Type Column", choices = nms),
              helpText("Select the column indicating checks vs. test entries.")
            )
          },
          step_header(3, "Select Traits and Options"),
          checkboxGroupInput(ns("traits"), NULL, choices = numeric_cols, selected = numeric_cols[1]),
          selectInput(ns("palette"), "Boxplot Color Palette", choices = rownames(RColorBrewer::brewer.pal.info)),
          selectInput(ns("genotype_model"), "Genotype Model", choices = c("Fixed", "Random"), selected = "Fixed"),
          checkboxInput(ns("env_wise"), "Compute Environment-wise BLUE/BLUP?", value = TRUE),
          step_header(4, "Run Analysis"),
          actionButton(ns("run_descriptive"), "Run Descriptive Summary", class = "btn btn-primary mb-2"),
          uiOutput(ns("descriptive_status")),
          actionButton(ns("run_model"), "Run Model-Based Analysis", class = "btn btn-primary mb-2"),
          uiOutput(ns("model_status")),
          step_header(5, "Download Results"),
          downloadButton(ns("download_analysis1"), "Download Results ZIP", class = "btn btn-success")
        )
      }
    })
    
    # --- Block C1: CRD Factors UI ---
    output$crd_factor_ui <- renderUI({
      req(raw_data(), input$crd_n_factors)
      df <- raw_data()
      nfac <- as.numeric(input$crd_n_factors)
      nms <- names(df)
      # CRITICAL: The ID for each selectInput must be wrapped in ns()
      lapply(1:nfac, function(i) {
        selectInput(ns(paste0("crd_factor", i)), paste("Factor", i), choices = nms)
      })
    })
    
    # -------- Block C2: Dynamic Main Panel UI (Corrected for reactiveValues) -----------
    output$dynamic_mainpanel <- renderUI({
      design <- active_design()
      if (is.null(design)) return(NULL)
      
      if (toupper(design) == "CRD") {
        # THE FIX: Convert the reactiveValues object to a regular list
        # without calling it as a function.
        res <- reactiveValuesToList(crd_results)
        
        # If no results have been generated yet, show a message.
        if (length(res) == 0) {
          return(div(h4("No CRD results yet. Click a 'Run' button to begin.")))
        }
        
        # Build the UI by looping through the traits selected by the user.
        # This is more robust than trying to guess traits from the results list.
        tab_list <- lapply(req(input$crd_traits), function(trait) {
          trait_id <- make.names(trait)
          tabPanel(
            title = trait,
            tabsetPanel(
              tabPanel("Summary Table", DT::dataTableOutput(ns(paste0("crd_sum_", trait_id)))),
              tabPanel("ANOVA Table", DT::dataTableOutput(ns(paste0("crd_anova_", trait_id)))),
              tabPanel("DMRT Results", verbatimTextOutput(ns(paste0("crd_dmrt_", trait_id)))),
              tabPanel("Missing Combinations", DT::dataTableOutput(ns(paste0("crd_missing_", trait_id)))),
              tabPanel("Interaction Plots", uiOutput(ns(paste0("crd_interaction_panel_", trait_id))))
            )
          )
        })
        
        # Use do.call to correctly build the tabsetPanel
        return(do.call(tabsetPanel, c(list(id = ns("crd_trait_tabs")), tab_list)))
        
      } else {
        # This part for other designs remains unchanged.
        tabsetPanel(
          id = ns("result_tabs"),
          tabPanel("Descriptive Results", uiOutput(ns("descriptive_ui"))),
          tabPanel("Model Results", uiOutput(ns("model_ui")))
        )
      }
    })
    
    # ===================================================================
    # -------- Section 2.4: Server Logic for Analyses and Rendering --------
    # ===================================================================
    
    # --- Block E6: EDA Descriptive Analysis (Non-CRD) ---
    observeEvent(input$run_descriptive, {
      req(raw_data(), input$traits)
      
      make_safe <- function(x) gsub("[^A-Za-z0-9]", "_", x)
      df <- raw_data()
      trial_type <- if (!is.null(input$trial_type)) input$trial_type else "Single Environment"
      
      env_col <- NULL
      if (trial_type == "Multi Environment" && !is.null(input$env) && input$env %in% names(df)) {
        env_col <- input$env
      }
      
      results_list <- purrr::map(input$traits, function(trait) {
        df_trait <- df[!is.na(df[[trait]]), ]
        df_trait$Environment <- if (is.null(env_col)) factor("All") else as.factor(df_trait[[env_col]])
        
        summary_tbl <- df_trait %>%
          dplyr::group_by(Environment) %>%
          dplyr::summarise(
            Trait = trait,
            Mean = round(mean(.data[[trait]], na.rm = TRUE), 2),
            SD = round(sd(.data[[trait]], na.rm = TRUE), 2),
            SE = round(SD / sqrt(n()), 2),
            CV = round(100 * SD / Mean, 2),
            Min = round(min(.data[[trait]], na.rm = TRUE), 2),
            Max = round(max(.data[[trait]], na.rm = TRUE), 2),
            N = n()
          )
        
        boxplot <- ggplot(df_trait, aes(x = Environment, y = .data[[trait]], fill = Environment)) +
          geom_boxplot(color = "black", width = 0.5) +
          theme_minimal(base_size = 16) +
          labs(
            title = if (length(unique(df_trait$Environment)) == 1)
              paste("Boxplot of", trait)
            else
              paste("Boxplot of", trait, "by Environment"),
            y = trait
          ) +
          scale_fill_brewer(palette = input$palette)
        
        qqplots <- df_trait %>%
          dplyr::group_split(Environment) %>%
          purrr::map(~ ggplot(.x, aes(sample = .data[[trait]])) +
                       stat_qq() + stat_qq_line() +
                       ggtitle(paste("QQ -", unique(.x$Environment))) +
                       theme_minimal(base_size = 14)
          )
        
        list(summary = summary_tbl, boxplot = boxplot, qq = qqplots)
      })
      names(results_list) <- input$traits
      descriptive_results(results_list)
      
      output$descriptive_status <- renderUI({
        span(style = "color: green; font-weight: bold;", icon("check"), " Descriptive Analysis Completed (OK)")
      })
      shinyjs::enable("run_model")
      shinyjs::enable("download_analysis1")
      
      output$descriptive_ui <- renderUI({
        tabs <- purrr::map(names(results_list), function(trait) {
          safe_trait <- make_safe(trait)
          tabPanel(
            title = trait,
            h4("Summary Statistics"), tableOutput(ns(paste0("sum_", safe_trait))),
            h4("Boxplot"), plotOutput(ns(paste0("box_", safe_trait))),
            h4("QQ Plot(s)"),
            fluidRow(
              purrr::map(seq_along(results_list[[trait]]$qq), function(i) {
                column(4, plotOutput(ns(paste0("qq_", safe_trait, "_", i))))
              })
            )
          )
        })
        do.call(tabsetPanel, tabs)
      })
      
      purrr::walk2(names(results_list), results_list, function(trait, r) {
        safe_trait <- make_safe(trait)
        local({
          trait_local <- safe_trait
          r_local <- r
          output[[paste0("sum_", trait_local)]] <- renderTable(r_local$summary)
          output[[paste0("box_", trait_local)]] <- renderPlot(r_local$boxplot)
          purrr::walk2(seq_along(r_local$qq), r_local$qq, function(i, p) {
            i_local <- i; p_local <- p
            output[[paste0("qq_", trait_local, "_", i_local)]] <- renderPlot(p_local)
          })
        })
      })
      
      updateTabsetPanel(session, "result_tabs", selected = "Descriptive Results")
    })
    
    # --- Block E8 Part D: Model-Based Analysis (Non-CRD) ---
    observeEvent(input$run_model, {
      req(raw_data(), active_design(), !tolower(gsub("\\s+", "", active_design())) == "crd", input$traits, input$entry)
      # ADD THIS WAITER_SHOW CALL
      waiter::waiter_show(
        id = ns("main_panel_eda"), # Targets the main panel
        html = tagList(
          tags$img(src = "www/spinner3.gif", height = "170px"),
          h4("Running Model Analysis, this may take a while...", style = "color:white;")
        ),
        color = "white" 
      )
      withProgress(message = 'Running Model Analysis...', value = 0, {
        df <- raw_data(); design <- tolower(gsub("\\s+", "", active_design())); trial_type <- input$trial_type
        genotype_model <- input$genotype_model
        traits <- input$traits; entry_col <- make.names(input$entry); block_col <- make.names(input$block); rep_col <- if (!is.null(input$rep)) make.names(input$rep) else NULL; env_col <- if (trial_type == "Multi Environment" && !is.null(input$env)) make.names(input$env) else NULL
        names(df) <- make.names(names(df)); cols_to_factor <- c(entry_col, block_col, rep_col, env_col); for (col in cols_to_factor) { if (!is.null(col) && col %in% names(df)) df[[col]] <- as.factor(df[[col]]) }
        
        all_results <- list(); bt <- function(x) paste0("`", x, "`")
        for (i in seq_along(traits)) {
          original_trait_name <- traits[i]; trait <- make.names(original_trait_name)
          incProgress(1 / length(traits), detail = paste("Processing trait:", original_trait_name))
          if (!is.numeric(df[[trait]])) { showNotification(paste("Skipping non-numeric trait:", original_trait_name), type = "warning"); next }
          
          trait_results <- list(); fixed_formula_vc_str <- NULL; formula_random_str <- NULL
          if (design %in% c("rcbd", "augmentedrcbd")) { if (is.null(env_col)) { fixed_formula_vc_str <- paste(bt(trait), "~", bt(entry_col), "+ (1|", bt(block_col), ")"); formula_random_str <- paste(bt(trait), "~ (1|", bt(entry_col), ") + (1|", bt(block_col), ")") } else { fixed_formula_vc_str <- paste(bt(trait), "~", bt(entry_col), "+ (1|", bt(env_col), ") + (1|", bt(entry_col), ":", bt(env_col), ") + (1|", bt(block_col), ":", bt(env_col), ")"); formula_random_str <- paste(bt(trait), "~ (1|", bt(entry_col), ") + (1|", bt(env_col), ") + (1|", bt(entry_col), ":", bt(env_col), ") + (1|", bt(block_col), ":", bt(env_col), ")") } 
          } else if (design == "alphalattice") { req(rep_col); if (is.null(env_col)) { fixed_formula_vc_str <- paste(bt(trait), "~", bt(entry_col), "+ (1|", bt(rep_col), ") + (1|", bt(block_col), ":", bt(rep_col), ")"); formula_random_str <- paste(bt(trait), "~ (1|", bt(entry_col), ") + (1|", bt(rep_col), ") + (1|", bt(block_col), ":", bt(rep_col), ")") } else { fixed_formula_vc_str <- paste(bt(trait), "~", bt(entry_col), "+ (1|", bt(env_col), ") + (1|", bt(entry_col), ":", bt(env_col), ") + (1|", bt(rep_col), ":", bt(env_col), ") + (1|", bt(block_col), ":", bt(rep_col), ":", bt(env_col), ")"); formula_random_str <- paste(bt(trait), "~ (1|", bt(entry_col), ") + (1|", bt(env_col), ") + (1|", bt(entry_col), ":", bt(env_col), ") + (1|", bt(rep_col), ":", bt(env_col), ") + (1|", bt(block_col), ":", bt(rep_col), ":", bt(env_col), ")") } }
          
          if (genotype_model == "Fixed") {
            key <- get_equation_key(design, trial_type, "Fixed")
            other_res <- run_model_and_extract(df, trait, fixed_formula_vc_str, NULL, "Fixed", entry_col, env_col, block_col, rep_col, design)
            trait_results$Fixed <- c(list(equation_latex = model_equations[[key]] %||% model_equations$default), other_res)
          }
          if (genotype_model == "Random") {
            key <- get_equation_key(design, trial_type, "Random")
            res_rand <- run_model_and_extract(df, trait, NULL, formula_random_str, "Random", entry_col, env_col, block_col, rep_col, design)
            trait_results$Random <- c(list(equation_latex = model_equations[[key]] %||% model_equations$default), res_rand)
          }
          all_results[[original_trait_name]] <- trait_results
        }
        model_results(all_results)
        waiter::waiter_hide() 
      })
      showNotification(paste(toupper(active_design()), "model analysis complete."), type = "message")
      updateTabsetPanel(session, "result_tabs", selected = "Model Results")
    })
    
    # --- Block E8 Part E: UI Renderer for Model Results ---
    output$model_ui <- renderUI({
      req(model_results())
      results <- model_results(); if (length(results) == 0) return(h4("Run Analysis.", style="color:grey;"))
      
      create_explanation_ui <- function(id_prefix, content) {
        ns_prefix <- ns(id_prefix)
        link_id <- paste0("toggle_", ns_prefix); div_id <- paste0("div_", ns_prefix)
        tagList(actionLink(link_id, "Show/Hide Explanation", style = "font-size: 12px;"), shinyjs::hidden(div(id = div_id, class = "alert alert-info", style = "margin-top: 10px;", content)))
      }
      
      model_explanation_content <- function(model_type, design, trial_type) {
        design <- tolower(gsub("\\s+", "", design))
        fixed_effects <- character(0); random_effects <- c("Residual (error)")
        if(model_type == "Fixed") { fixed_effects <- c(fixed_effects, "Genotype") } else { random_effects <- c(random_effects, "Genotype") }
        if(design == "alphalattice") {
          random_effects <- c(random_effects, "Replication")
          if(trial_type != "Multi Environment") random_effects <- c(random_effects, "Block within Replication")
        } else { random_effects <- c(random_effects, "Block") }
        if(trial_type == "Multi Environment") {
          random_effects <- c(random_effects, "Environment", "Genotype x Environment Interaction")
          if(design == "alphalattice") {
            random_effects <- c(random_effects, "Replication within Environment", "Block within Replication & Environment")
          } else { random_effects <- c(random_effects, "Block within Environment") }
        }
        tagList(
          if (length(fixed_effects) > 0) { list(tags$b("Fixed Effects:"), tags$ul(lapply(sort(unique(fixed_effects)), tags$li))) },
          tags$b("Random Effects:"), tags$ul(lapply(sort(unique(random_effects)), tags$li))
        )
      }
      
      lrt_explanation_content <- tagList(
        tags$b("What is the Likelihood Ratio Test (LRT)?"), tags$p("The LRT tests if a random effect is statistically significant. A small p-value (Pr(>Chisq)) suggests the effect is important."), tags$b("What is the Baseline Model?"), tags$p("The 'Baseline Model (for comparison)' is the simpler model used as a starting point for the test.")
      )
      
      trait_tabs <- purrr::imap(results, ~{
        trait_name <- .y; trait_content <- .x; model_type_tabs <- list()
        if (!is.null(trait_content$Fixed)) {
          tid_prefix <- make.names(paste0(trait_name, "_fixed"))
          model_type_tabs$Fixed <- tabPanel("Fixed Model (Genotype)",
                                            h5("Model Equation", style="font-weight:bold;"), uiOutput(ns(paste0("equation_", tid_prefix))),
                                            create_explanation_ui(paste0("model_exp_", tid_prefix), model_explanation_content("Fixed", active_design(), input$trial_type)), 
                                            hr(),
                                            uiOutput(ns(paste0("singularity_warning_", tid_prefix))),
                                            h4("1. ANOVA Table (Genotype Only)"), DT::dataTableOutput(ns(paste0("anova_", tid_prefix))),
                                            uiOutput(ns(paste0("anova_interp_", tid_prefix))),
                                            h4("2. Likelihood Ratio Test (LRT)"), create_explanation_ui(paste0("lrt_exp_", tid_prefix), lrt_explanation_content), DT::dataTableOutput(ns(paste0("lrt_", tid_prefix))),
                                            uiOutput(ns(paste0("lrt_interp_", tid_prefix))), 
                                            h4("3. Variance Components"), DT::dataTableOutput(ns(paste0("varcomp_", tid_prefix))),
                                            h4("4. Best Linear Unbiased Estimates (BLUEs)"), DT::dataTableOutput(ns(paste0("blues_", tid_prefix)))
          )
        }
        if (!is.null(trait_content$Random)) {
          tid_prefix <- make.names(paste0(trait_name, "_random"))
          model_type_tabs$Random <- tabPanel("Random Model (Genotype)",
                                             h5("Model Equation", style="font-weight:bold;"), uiOutput(ns(paste0("equation_", tid_prefix))),
                                             create_explanation_ui(paste0("model_exp_", tid_prefix), model_explanation_content("Random", active_design(), input$trial_type)), 
                                             hr(),
                                             uiOutput(ns(paste0("singularity_warning_", tid_prefix))),
                                             h4("1. Likelihood Ratio Test (LRT)"), create_explanation_ui(paste0("lrt_exp_", tid_prefix), lrt_explanation_content), DT::dataTableOutput(ns(paste0("lrt_", tid_prefix))),
                                             uiOutput(ns(paste0("lrt_interp_", tid_prefix))),
                                             h4("2. Variance Components"), DT::dataTableOutput(ns(paste0("varcomp_", tid_prefix))),
                                             h4("3. Best Linear Unbiased Predictors (BLUPs)"), DT::dataTableOutput(ns(paste0("blups_", tid_prefix)))
          )
        }
        tabPanel(title = trait_name, do.call(tabsetPanel, unname(model_type_tabs)))
      })
      
      purrr::iwalk(results, ~{
        trait_name <- .y; trait_content <- .x
        if (!is.null(trait_content$Fixed)) {
          tid_prefix <- make.names(paste0(trait_name, "_fixed"))
          local({
            observeEvent(input[[paste0("toggle_model_exp_", tid_prefix)]], { shinyjs::toggle(id = paste0("div_", ns(paste0("model_exp_", tid_prefix))), anim = TRUE) })
            observeEvent(input[[paste0("toggle_lrt_exp_", tid_prefix)]], { shinyjs::toggle(id = paste0("div_", ns(paste0("lrt_exp_", tid_prefix))), anim = TRUE) })
            
            output[[paste0("anova_interp_", tid_prefix)]] <- renderUI({ req(trait_content$Fixed$anova_interpretation); tags$div(class="alert alert-light", style="margin-top:10px; border-left: 3px solid #142850;", trait_content$Fixed$anova_interpretation) })
            output[[paste0("lrt_interp_", tid_prefix)]] <- renderUI({ req(trait_content$Fixed$lrt_interpretation); tags$div(class="alert alert-light", style="margin-top:10px; border-left: 3px solid #142850;", trait_content$Fixed$lrt_interpretation) })
            output[[paste0("equation_", tid_prefix)]] <- renderUI({ req(trait_content$Fixed$equation_latex); p(trait_content$Fixed$equation_latex) })
            output[[paste0("singularity_warning_", tid_prefix)]] <- renderUI({ if(!is.null(trait_content$Fixed$is_singular) && trait_content$Fixed$is_singular) { tags$div(class = "alert alert-warning", trait_content$Fixed$singularity_message) } })
            output[[paste0("anova_", tid_prefix)]] <- DT::renderDataTable(trait_content$Fixed$anova_table, rownames = FALSE, options = list(scrollX = TRUE, pageLength = 5))
            output[[paste0("lrt_", tid_prefix)]] <- DT::renderDataTable(trait_content$Fixed$lrt_table, rownames = FALSE, options = list(scrollX = TRUE, pageLength = 5))
            output[[paste0("varcomp_", tid_prefix)]] <- DT::renderDataTable(trait_content$Fixed$var_comps, rownames = FALSE, options = list(scrollX = TRUE, pageLength = 5))
            output[[paste0("blues_", tid_prefix)]] <- DT::renderDataTable(trait_content$Fixed$blue_table, rownames = FALSE, options = list(scrollX = TRUE, pageLength = 10))
          })
        }
        if (!is.null(trait_content$Random)) {
          tid_prefix <- make.names(paste0(trait_name, "_random"))
          local({
            observeEvent(input[[paste0("toggle_model_exp_", tid_prefix)]], { shinyjs::toggle(id = paste0("div_", ns(paste0("model_exp_", tid_prefix))), anim = TRUE) })
            observeEvent(input[[paste0("toggle_lrt_exp_", tid_prefix)]], { shinyjs::toggle(id = paste0("div_", ns(paste0("lrt_exp_", tid_prefix))), anim = TRUE) })
            
            output[[paste0("equation_", tid_prefix)]] <- renderUI({ req(trait_content$Random$equation_latex); p(trait_content$Random$equation_latex) })
            output[[paste0("lrt_interp_", tid_prefix)]] <- renderUI({ req(trait_content$Random$lrt_interpretation); tags$div(class="alert alert-light", style="margin-top:10px; border-left: 3px solid #142850;", trait_content$Random$lrt_interpretation) })
            output[[paste0("singularity_warning_", tid_prefix)]] <- renderUI({ if(!is.null(trait_content$Random$is_singular) && trait_content$Random$is_singular) { tags$div(class = "alert alert-warning", trait_content$Random$singularity_message) } })
            output[[paste0("lrt_", tid_prefix)]] <- DT::renderDataTable(trait_content$Random$lrt_table, rownames = FALSE, options = list(scrollX = TRUE, pageLength = 5))
            output[[paste0("varcomp_", tid_prefix)]] <- DT::renderDataTable(trait_content$Random$var_comps, rownames = FALSE, options = list(scrollX = TRUE, pageLength = 5))
            output[[paste0("blups_", tid_prefix)]] <- DT::renderDataTable(trait_content$Random$blup_table, rownames = FALSE, options = list(scrollX = TRUE, pageLength = 10))
          })
        }
      })
      
      do.call(tabsetPanel, unname(trait_tabs))
    })
    
    # ===================================================================
    # -------- Section 2.4: CRD Analysis Logic (CORRECTED BLOCKS) --------
    # ===================================================================
    # -------- Block C4: CRD Descriptive Analysis (Corrected for reactiveValues) -----------
    observeEvent(input$crd_descriptive, {
      req(raw_data(), input$crd_traits, input$crd_n_factors)
      df <- raw_data(); nfac <- as.numeric(input$crd_n_factors)
      factor_names <- paste0("crd_factor", 1:nfac)
      factor_cols <- sapply(factor_names, function(x) input[[x]])
      
      # --- Original validation logic ---
      if (any(is.null(factor_cols) | factor_cols == "")) {
        showNotification("Please select all factors.", type = "error"); return(NULL)
      }
      if (any(!factor_cols %in% names(df))) {
        showNotification("Selected factors do not match data columns.", type = "error"); return(NULL)
      }
      for (fac in factor_cols) df[[fac]] <- as.factor(df[[fac]])
      
      for (trait in input$crd_traits) {
        local({
          trait_local <- trait; trait_id <- make.names(trait_local)
          
          summary_tbl <- df %>%
            group_by(across(all_of(factor_cols))) %>%
            summarise(
              mean = round(mean(.data[[trait_local]], na.rm = TRUE), 2),
              SE = round(sd(.data[[trait_local]], na.rm = TRUE)/sqrt(n()), 2),
              N = n(),
              .groups = "drop"
            )
          
          # Assign the result directly to a named slot (e.g., crd_results$summary_Trait1)
          crd_results[[paste0("summary_", trait_id)]] <- summary_tbl
          
          # Render the output immediately
          output[[paste0("crd_sum_", trait_id)]] <- DT::renderDataTable({
            req(crd_results[[paste0("summary_", trait_id)]])
          })
        })
      }
      showNotification("Descriptive summary complete.", type = "message")
    })
    
    
    # --- Block C5: ANOVA + Post-Hoc (Corrected for reactiveValues) ---
    observeEvent(input$run_crd_anova, {
      req(raw_data(), input$crd_traits, input$crd_n_factors > 0)
      df <- raw_data(); nfac <- as.numeric(input$crd_n_factors)
      factor_names <- paste0("crd_factor", 1:nfac)
      factor_cols <- na.omit(unname(sapply(factor_names, function(x) input[[x]])))
      
      # --- Original validation logic ---
      if (any(duplicated(factor_cols))) {
        showNotification("Please select unique factors for each Factor slot!", type = "error"); return(NULL)
      }
      if (length(factor_cols) == 0) {
        showNotification("Please select at least one valid factor.", type = "error"); return(NULL)
      }
      if (any(!factor_cols %in% names(df))) {
        showNotification("Selected factors do not match data columns.", type = "error"); return(NULL)
      }
      for (fac in factor_cols) df[[fac]] <- as.factor(df[[fac]])
      single_level_factors <- factor_cols[sapply(factor_cols, function(fac) length(unique(df[[fac]])) < 2)]
      if (length(single_level_factors) > 0) {
        msg <- paste("Some selected factors have only one level and will be ignored in ANOVA:", paste(single_level_factors, collapse = ", "))
        showNotification(msg, type = "error"); return(NULL)
      }
      
      withProgress(message = 'Running ANOVA + Post Hoc (DMRT)...', value = 0, {
        for (trait in input$crd_traits) {
          local({
            trait_local <- trait; trait_id <- make.names(trait_local)
            incProgress(1/length(input$crd_traits), detail = paste("Trait:", trait_local))
            
            # --- Original analysis variables and logic ---
            anova_str <- ""; dmrt_str <- ""; anova_df <- data.frame(); missing <- data.frame()
            
            if (!is.numeric(df[[trait_local]])) {
              anova_str <- paste("Trait", trait_local, "is not numeric. Cannot run ANOVA.")
              anova_df <- data.frame(Message = anova_str)
            } else {
              factors_bt <- sapply(factor_cols, function(x) if (grepl("[^A-Za-z0-9_.]", x)) paste0("`", x, "`") else x)
              formula_str <- if (length(factors_bt) == 1) { paste(trait_local, "~", factors_bt[1]) } else { paste(trait_local, "~", paste(factors_bt, collapse = " * ")) }
              fit <- tryCatch(aov(as.formula(formula_str), data = df), error = function(e) NULL)
              
              if (!is.null(fit)) {
                anova_df <- broom::tidy(fit)
                anova_str <- "Shapiro-Wilk Test for Normality of Residuals:\n"
                res <- tryCatch(residuals(fit), error = function(e) NULL)
                if (!is.null(res) && length(res) > 3) {
                  shapiro <- tryCatch(shapiro.test(res), error = function(e) NULL)
                  if (!is.null(shapiro)) anova_str <- paste0(anova_str, paste(capture.output(shapiro), collapse = "\n"), "\n")
                }
                if (requireNamespace("agricolae", quietly = TRUE)) {
                  for (ff in factors_bt) {
                    dmrt <- tryCatch(agricolae::duncan.test(fit, ff, group=TRUE), error = function(e) NULL)
                    if (!is.null(dmrt)) dmrt_str <- paste0(dmrt_str, "\nDMRT for ", ff, ":\n", paste(capture.output(dmrt$groups), collapse = "\n"), "\n")
                  }
                  if (length(factors_bt) > 1) {
                    all_interactions <- unlist(lapply(2:length(factors_bt), function(m) combn(factors_bt, m, simplify = FALSE)), recursive = FALSE)
                    for (inter in all_interactions) {
                      term <- paste(inter, collapse=":")
                      dmrt <- tryCatch(agricolae::duncan.test(fit, term, group=TRUE), error = function(e) NULL)
                      if (!is.null(dmrt)) dmrt_str <- paste0(dmrt_str, "\nDMRT for interaction ", term, ":\n", paste(capture.output(dmrt$groups), collapse = "\n"), "\n")
                    }
                  }
                } else { dmrt_str <- "\nagricolae package not available. DMRT not run.\n" }
              } else { anova_str <- "ANOVA failed."; anova_df <- data.frame(Message = anova_str) }
            }
            full_grid <- expand.grid(lapply(df[factor_cols], function(x) levels(x))); actual <- df[, factor_cols, drop = FALSE] %>% distinct()
            missing <- dplyr::anti_join(full_grid, actual, by = factor_cols)
            
            # Assign each result directly to its own slot
            crd_results[[paste0("anova_table_", trait_id)]] <- anova_df
            crd_results[[paste0("dmrt_text_", trait_id)]] <- dmrt_str
            crd_results[[paste0("report_", trait_id)]] <- paste(anova_str, "\n", dmrt_str)
            crd_results[[paste0("missing_combinations_", trait_id)]] <- missing
            
            # --- UI rendering logic ---
            output[[paste0("crd_anova_", trait_id)]] <- DT::renderDataTable({ req(crd_results[[paste0("anova_table_", trait_id)]]) })
            output[[paste0("crd_dmrt_", trait_id)]] <- renderPrint({ cat(crd_results[[paste0("dmrt_text_", trait_id)]]) })
            output[[paste0("crd_missing_", trait_id)]] <- DT::renderDataTable({ req(crd_results[[paste0("missing_combinations_", trait_id)]]) })
          })
        }
      })
      showNotification("ANOVA and Post-Hoc analysis complete.", type = "message")
    })
    
    
    # -------- Block C6: CRD Interaction Plots (DEFINITIVE FIX for reactiveValues) --------
    observeEvent(input$run_crd_interact, {
      req(raw_data(), input$crd_traits, input$crd_n_factors > 0)
      df <- raw_data(); nfac <- as.numeric(input$crd_n_factors)
      factor_names <- paste0("crd_factor", 1:nfac)
      factor_cols <- sapply(factor_names, function(x) input[[x]])
      
      # --- Original validation logic ---
      if (any(is.null(factor_cols) | factor_cols == "")) {
        showNotification("Please select all factors.", type = "error"); return(NULL)
      }
      if (any(!factor_cols %in% names(df))) {
        showNotification("Selected factors do not match data columns.", type = "error"); return(NULL)
      }
      for (fac in factor_cols) df[[fac]] <- as.factor(df[[fac]])
      
      for (trait in input$crd_traits) {
        local({
          trait_local <- trait; trait_id <- make.names(trait_local)
          plot_list <- list()
          
          # --- UI registration using ns() (Original Logic) ---
          output[[paste0("crd_interaction_panel_", trait_id)]] <- renderUI({
            ui_list <- list()
            if (nfac == 3) {
              ui_list <- list(
                tags$h4("Three-way Interaction"), plotOutput(ns(paste0("crd_interact_3way_", trait_id))),
                tags$h4("Two-way Interactions"), plotOutput(ns(paste0("crd_interact_2way_", trait_id, "_1"))),
                plotOutput(ns(paste0("crd_interact_2way_", trait_id, "_2"))), plotOutput(ns(paste0("crd_interact_2way_", trait_id, "_3")))
              )
            } else if (nfac == 2) {
              ui_list <- list(tags$h4("Two-way Interaction"), plotOutput(ns(paste0("crd_interact_2way_", trait_id, "_1"))))
            } else if (nfac == 1) {
              ui_list <- list(plotOutput(ns(paste0("crd_interact_single_", trait_id))))
            }
            do.call(tagList, ui_list)
          })
          
          # --- Original plot building logic ---
          if (nfac == 3) {
            fac1 <- factor_cols[1]; fac2 <- factor_cols[2]; fac3 <- factor_cols[3]
            df_summary_3way <- df %>% group_by(.data[[fac1]], .data[[fac2]], .data[[fac3]]) %>% summarise(mean_value = mean(.data[[trait_local]], na.rm=TRUE), .groups='drop')
            p3way <- ggplot(df_summary_3way, aes(x=.data[[fac1]], y=mean_value, color=.data[[fac3]], group=.data[[fac3]])) + geom_line() + geom_point() + facet_wrap(as.formula(paste("~", fac2))) + labs(title=paste("3-way Interaction:", trait_local), x=fac1, y=paste("Mean", trait_local), color=fac3) + theme_bw() + theme(axis.text.x = element_text(angle=45, hjust=1))
            plot_list[["3way"]] <- p3way
            output[[paste0("crd_interact_3way_", trait_id)]] <- renderPlot({ print(p3way) })
            all_pairs <- list(c(fac1, fac2), c(fac1, fac3), c(fac2, fac3))
            for (i in seq_along(all_pairs)) {
              xfac <- all_pairs[[i]][1]; linefac <- all_pairs[[i]][2]
              df_summary_2way <- df %>% group_by(.data[[xfac]], .data[[linefac]]) %>% summarise(mean_value = mean(.data[[trait_local]], na.rm=TRUE), .groups='drop')
              p2way <- ggplot(df_summary_2way, aes(x=.data[[xfac]], y=mean_value, color=.data[[linefac]], group=.data[[linefac]])) + geom_line() + geom_point() + labs(title=paste("2-way Interaction:", xfac, "x", linefac), x=xfac, y=paste("Mean", trait_local), color=linefac) + theme_bw() + theme(axis.text.x = element_text(angle=45, hjust=1))
              plot_list[[paste0("2way_", i)]] <- p2way
              output[[paste0("crd_interact_2way_", trait_id, "_", i)]] <- renderPlot({ print(p2way) })
            }
          } else if (nfac == 2) {
            xfac <- factor_cols[1]; linefac <- factor_cols[2]
            df_summary_2way <- df %>% group_by(.data[[xfac]], .data[[linefac]]) %>% summarise(mean_value = mean(.data[[trait_local]], na.rm=TRUE), .groups='drop')
            p2way <- ggplot(df_summary_2way, aes(x=.data[[xfac]], y=mean_value, color=.data[[linefac]], group=.data[[linefac]])) + geom_line() + geom_point() + labs(title=paste("2-way Interaction:", trait_local), x=xfac, y=paste("Mean", trait_local), color=linefac) + theme_bw() + theme(axis.text.x = element_text(angle=45, hjust=1))
            plot_list[["2way_1"]] <- p2way
            output[[paste0("crd_interact_2way_", trait_id, "_1")]] <- renderPlot({ print(p2way) })
          } else if (nfac == 1) {
            fac <- factor_cols[1]
            pbox <- ggplot(df, aes(x=.data[[fac]], y=.data[[trait_local]], fill=.data[[fac]])) + geom_boxplot() + labs(title=paste("Boxplot:", trait_local, "by", fac), x=fac, y=trait_local) + theme_bw() + theme(axis.text.x = element_text(angle=45, hjust=1), legend.position="none")
            plot_list[["boxplot"]] <- pbox
            output[[paste0("crd_interact_single_", trait_id)]] <- renderPlot({ print(pbox) })
          }
          
          # --- THE FIX: Directly assign the list of plots to its reactiveValues slot ---
          crd_results[[paste0("plots_", trait_id)]] <- plot_list
        })
      }
      showNotification("Interaction plots generated for all selected traits.", type = "message")
    })
    
    
    
    # --- Block E9: Update Analysis 2 Trait Selectors ---
    observe({
      mod_res <- model_results()
      if (is.null(mod_res) || length(mod_res) == 0) return(NULL)
      
      valid_fixed_results <- purrr::keep(mod_res, ~!is.null(.x$Fixed$blue_table) && is.data.frame(.x$Fixed$blue_table))
      if(length(valid_fixed_results) == 0) return(NULL)
      
      traits_with_blues <- names(purrr::keep(valid_fixed_results, ~"BLUE_Combined" %in% names(.x$Fixed$blue_table)))
      traits_with_locwise_blues <- names(purrr::keep(valid_fixed_results, ~ncol(.x$Fixed$blue_table) > 3))
      
      updateSelectInput(session, "gge_trait", choices = traits_with_locwise_blues, selected = if (length(traits_with_locwise_blues) > 0) traits_with_locwise_blues[[1]] else NULL)
      updateCheckboxGroupInput(session, "multi_traits", choices = traits_with_blues, selected = traits_with_blues)
    })
    
    # --- Block E12: Analysis 2 - GGE Biplot ---
    observeEvent(input$run_gge, {
      req(input$gge_trait, model_results())
      trait_data <- model_results()[[input$gge_trait]]$Fixed
      req(trait_data, trait_data$blue_table, is.data.frame(trait_data$blue_table))
      
      df_blue <- trait_data$blue_table
      # This input$entry will come from the ns("entry") in the dynamic sidebar
      entry_col_name <- make.names(input$entry) 
      
      df_gge <- df_blue %>%
        dplyr::select(gen = all_of(entry_col_name), starts_with("BLUE_")) %>%
        dplyr::select(-contains("BLUE_Combined"), -contains("SE_Combined")) %>%
        tidyr::pivot_longer(cols = -gen, names_to = "env", values_to = "resp", names_prefix = "BLUE_")
      
      tryCatch({
        gge_model <- metan::gge(df_gge, env = env, gen = gen, resp = resp, scaling = 1)
        gge_results(gge_model)
        output$gge_plot_type1 <- renderPlot({ plot(gge_model, type = 3) })
        output$gge_plot_type2 <- renderPlot({ plot(gge_model, type = 2) })
        output$gge_plot_type3 <- renderPlot({ plot(gge_model, type = 4) })
        output$gge_status <- renderUI({ span(style = "color: green; font-weight: bold;", icon("check"), " GGE Biplot Completed (OK)") })
        updateTabsetPanel(session, "analysis2_tabs", selected = " GGE Biplot")
      }, error = function(e) { showModal(modalDialog(title = "GGE Error", paste("Error in GGE analysis:", e$message), easyClose = TRUE)) })
    })
    
    # --- Block E13: Analysis 2 - PCA ---
    observeEvent(input$run_pca, {
      req(model_results(), input$multi_traits)
      entry_col_name <- make.names(input$entry)
      
      trait_df_list <- purrr::map(input$multi_traits, function(trait) {
        res <- model_results()[[trait]]$Fixed
        if (!is.null(res) && !is.null(res$blue_table) && "BLUE_Combined" %in% names(res$blue_table)) {
          res$blue_table %>% dplyr::select(all_of(entry_col_name), !!sym(trait) := BLUE_Combined)
        } else { NULL }
      }) %>% purrr::discard(is.null)
      
      if (length(trait_df_list) < 2) { showModal(modalDialog(title = "Insufficient Data", "PCA requires at least two traits with valid Combined BLUEs.", easyClose = TRUE)); return() }
      
      df_pca <- trait_df_list %>% purrr::reduce(full_join, by = entry_col_name)
      df_pca_numeric <- df_pca[, -1, drop = FALSE]
      rownames(df_pca_numeric) <- df_pca[[1]]
      
      tryCatch({
        res.pca <- FactoMineR::PCA(df_pca_numeric, graph = FALSE, ncp = 5)
        pca_results(list(pca = res.pca))
        output$pca_plot_biplot <- renderPlot({ fviz_pca_ind(res.pca, col.ind = "cos2", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), repel = TRUE) })
        output$pca_plot_scree <- renderPlot({ fviz_screeplot(res.pca, addlabels = TRUE) })
        output$pca_plot_varcontrib <- renderPlot({ fviz_pca_var(res.pca, col.var = "contrib", gradient.cols = c("white", "blue", "red")) })
        output$pca_table_summary <- renderTable({ round(factoextra::get_eigenvalue(res.pca), 2) }, rownames = TRUE)
        output$pca_status <- renderUI({ span(style = "color: green; font-weight: bold;", icon("check"), " PCA Completed (OK)") })
        updateTabsetPanel(session, "analysis2_tabs", selected = " PCA Plot")
      }, error = function(e) { showModal(modalDialog(title = "PCA Error", paste("Error in PCA analysis:", e$message), easyClose = TRUE)) })
    })
    
    # --- Block E14: Analysis 2 - Correlation ---
    observeEvent(input$run_corr, {
      req(pca_results())
      df_corr <- tryCatch({ pca_results()$pca$call$X }, error = function(e) NULL)
      if (is.null(df_corr)) {
        showModal(modalDialog(title = "Correlation Error", "PCA results missing. Please run PCA first.", easyClose = TRUE))
        return()
      }
      
      tryCatch({
        # Calculate correlation matrix and p-values
        corr_obj <- corrplot::cor.mtest(df_corr, conf.level = 0.95)
        corr_matrix <- cor(df_corr, use = "complete.obs")
        p_value_matrix <- corr_obj$p
        
        # Generate and store the interpretation
        interpretation_text <- generate_correlation_interpretation(corr_matrix, p_value_matrix)
        corr_interpretation(interpretation_text)
        
        # --- SIMPLIFIED AND CORRECTED PLOT RENDERING ---
        
        # Plot 1: A single, clear correlation matrix
        output$corr_plot1 <- renderPlot({
          corrplot(
            corr_matrix,
            method = "number",        # Use color to represent correlation strength
            addCoef.col = "black",   # Overlay coefficients in solid black text
            type = "upper",          
            order = "hclust",        
            p.mat = p_value_matrix,  # Use p-values to blank non-significant results
            sig.level = 0.05,        
            insig = "pch",         
            tl.col = "black",        # Text label color
            tl.srt = 45,             # Rotate text labels for readability
            number.cex = 0.9         # Adjust number size
          )
        })
        
        # Plot 2 has been removed from the logic.
        
        output$corr_status <- renderUI({
          span(style = "color: green; font-weight: bold;", icon("check"), " Correlation Completed (OK)")
        })
        updateTabsetPanel(session, "analysis2_tabs", selected = " Correlation Plot")
        
      }, error = function(e) {
        showModal(modalDialog(title = "Correlation Error", paste("Error in correlation plotting:", e$message), easyClose = TRUE))
      })
    })
    output$corr_interpretation_ui <- renderUI({
      req(corr_interpretation())
      div(style = "background-color: #f8f9fa; border-left: 5px solid #007bff; padding: 15px; border-radius: 5px;",
          corr_interpretation()
      )
    })
    #==================================================================
    # -------- Section 2.5: Download Handlers --------
    #==================================================================
    
    # -------- Block D1: Download Handler - CRD Results (Corrected for reactiveValues) --------
    output$download_crd_zip <- downloadHandler(
      filename = function() paste0("CRD_Results_", Sys.Date(), ".zip"),
      content = function(file) {
        # 1. Create a temporary directory.
        tmp_dir <- tempfile("crd_zip_")
        dir.create(tmp_dir)
        on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)
        
        files_to_zip <- c()
        # Access the reactiveValues object directly
        res <- crd_results
        sanitize <- function(x) gsub("[^A-Za-z0-9_]", "_", x)
        
        # 2. Iterate through the traits selected in the UI.
        for (trait in req(input$crd_traits)) {
          trait_id <- make.names(trait)
          tname <- sanitize(trait)
          
          # 3. Check for and save each result type using its unique key.
          
          # --- Save Summary CSV ---
          summary_key <- paste0("summary_", trait_id)
          if (!is.null(res[[summary_key]])) {
            fname <- file.path(tmp_dir, paste0(tname, "_summary.csv"))
            write.csv(res[[summary_key]], fname, row.names = FALSE)
            files_to_zip <- c(files_to_zip, fname)
          }
          
          # --- Save ANOVA CSV ---
          anova_key <- paste0("anova_table_", trait_id)
          if (!is.null(res[[anova_key]])) {
            fname <- file.path(tmp_dir, paste0(tname, "_anova.csv"))
            write.csv(res[[anova_key]], fname, row.names = FALSE)
            files_to_zip <- c(files_to_zip, fname)
          }
          
          # --- Save DMRT Text File ---
          dmrt_key <- paste0("dmrt_text_", trait_id)
          if (!is.null(res[[dmrt_key]]) && nzchar(res[[dmrt_key]])) {
            fname <- file.path(tmp_dir, paste0(tname, "_dmrt_results.txt"))
            writeLines(res[[dmrt_key]], fname)
            files_to_zip <- c(files_to_zip, fname)
          }
          
          # --- Save Missing Combinations CSV ---
          missing_key <- paste0("missing_combinations_", trait_id)
          if (!is.null(res[[missing_key]]) && nrow(res[[missing_key]]) > 0) {
            fname <- file.path(tmp_dir, paste0(tname, "_missing_combinations.csv"))
            write.csv(res[[missing_key]], fname, row.names = FALSE)
            files_to_zip <- c(files_to_zip, fname)
          }
          
          # --- Save Interaction Plots PDF ---
          plots_key <- paste0("plots_", trait_id)
          plot_list <- res[[plots_key]]
          if (!is.null(plot_list) && length(plot_list) > 0) {
            for (pname in names(plot_list)) {
              fname <- file.path(tmp_dir, paste0(tname, "_", sanitize(pname), "_interaction.pdf"))
              pdf(fname, width = 8, height = 5)
              print(plot_list[[pname]])
              dev.off()
              files_to_zip <- c(files_to_zip, fname)
            }
          }
        }
        
        # 4. Zip all the created files.
        zip::zip(zipfile = file, files = files_to_zip, mode = "cherry-pick")
      },
      contentType = "application/zip"
    )
    
    
    
    # -------- Block D2: Download Handler - Analysis 1 ZIP (Corrected) --------
    output$download_analysis1 <- downloadHandler(
      filename = function() {
        design_name <- tools::toTitleCase(active_design() %||% "Analysis")
        paste0("PbAT_", design_name, "_Results_", Sys.Date(), ".zip")
      },
      content = function(file) {
        tmp_dir <- tempfile("analysis1_zip_")
        dir.create(tmp_dir)
        on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)
        
        files_to_zip <- c()
        descriptive_list <- descriptive_results()
        model_list <- model_results()
        
        for (trait in names(model_list)) {
          tryCatch({
            # --- Save Descriptive Results ---
            if (!is.null(descriptive_list[[trait]])) {
              desc_res <- descriptive_list[[trait]]
              fname_sum <- file.path(tmp_dir, paste0(trait, "_descriptive_summary.csv"))
              write.csv(desc_res$summary, fname_sum, row.names = FALSE)
              
              fname_box <- file.path(tmp_dir, paste0(trait, "_boxplot.pdf"))
              pdf(fname_box, width = 11, height = 8.5); print(desc_res$boxplot); dev.off()
              
              if (!is.null(desc_res$qq) && length(desc_res$qq) > 0) {
                fname_qq <- file.path(tmp_dir, paste0(trait, "_qqplots.pdf"))
                pdf(fname_qq, width = 11, height = 8.5)
                for (p in desc_res$qq) { print(p) }
                dev.off()
                files_to_zip <- c(files_to_zip, fname_qq)
              }
              files_to_zip <- c(files_to_zip, fname_sum, fname_box)
            }
            
            # --- Save Model Results ---
            trait_model_res <- model_list[[trait]]
            if (!is.null(trait_model_res$Fixed)) {
              fixed_res <- trait_model_res$Fixed
              if (is.data.frame(fixed_res$anova_table)) { fname <- file.path(tmp_dir, paste0(trait, "_Fixed_ANOVA.csv")); write.csv(fixed_res$anova_table, fname, row.names=F); files_to_zip <- c(files_to_zip, fname) }
              if (is.data.frame(fixed_res$blue_table)) { fname <- file.path(tmp_dir, paste0(trait, "_Fixed_BLUEs.csv")); write.csv(fixed_res$blue_table, fname, row.names=F); files_to_zip <- c(files_to_zip, fname) }
              if (is.data.frame(fixed_res$lrt_table)) { fname <- file.path(tmp_dir, paste0(trait, "_Fixed_LRT.csv")); write.csv(fixed_res$lrt_table, fname, row.names=F); files_to_zip <- c(files_to_zip, fname) }
              if (is.data.frame(fixed_res$var_comps)) { fname <- file.path(tmp_dir, paste0(trait, "_Fixed_VarComps.csv")); write.csv(fixed_res$var_comps, fname, row.names=F); files_to_zip <- c(files_to_zip, fname) }
            }
            if (!is.null(trait_model_res$Random)) {
              rand_res <- trait_model_res$Random
              if (is.data.frame(rand_res$blup_table)) { fname <- file.path(tmp_dir, paste0(trait, "_Random_BLUPs.csv")); write.csv(rand_res$blup_table, fname, row.names=F); files_to_zip <- c(files_to_zip, fname) }
              if (is.data.frame(rand_res$lrt_table)) { fname <- file.path(tmp_dir, paste0(trait, "_Random_LRT.csv")); write.csv(rand_res$lrt_table, fname, row.names=F); files_to_zip <- c(files_to_zip, fname) }
              if (is.data.frame(rand_res$var_comps)) { fname <- file.path(tmp_dir, paste0(trait, "_Random_VarComps.csv")); write.csv(rand_res$var_comps, fname, row.names=F); files_to_zip <- c(files_to_zip, fname) }
            }
          }, error = function(e) { showNotification(paste("Error for trait", trait, ":", e$message), type = "warning") })
        }
        
        # CORRECTED FUNCTION CALL
        zip::zipr(zipfile = file, files = files_to_zip, recurse = FALSE)
      },
      contentType = "application/zip"
    )
    
    
    # -------- Block D3: Download Handler - Analysis 2 ZIP (FINAL CORRECTED) --------
    output$download_analysis2 <- downloadHandler(
      filename = function() paste0("PbAT_Analysis2_Results_", Sys.Date(), ".zip"),
      content = function(file) {
        # 1. Create a temporary directory.
        tmp_dir <- tempfile("analysis2_zip_")
        dir.create(tmp_dir)
        on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)
        
        files_to_zip <- c()
        
        tryCatch({
          # --- GGE Plots ---
          if (!is.null(gge_results())) {
            for (i in 1:4) {
              fname <- file.path(tmp_dir, paste0("GGE_Plot_Type", i, ".pdf"))
              pdf(fname, width = 7, height = 6); print(plot(gge_results(), type = i)); dev.off()
              files_to_zip <- c(files_to_zip, fname)
            }
          }
          # --- PCA Plots & Data ---
          if (!is.null(pca_results()$pca)) {
            pca_obj <- pca_results()$pca
            
            pdf(file.path(tmp_dir, "PCA_Biplot.pdf")); print(fviz_pca_ind(pca_obj, repel = TRUE)); dev.off()
            pdf(file.path(tmp_dir, "PCA_Scree.pdf")); print(fviz_screeplot(pca_obj, addlabels = TRUE)); dev.off()
            pdf(file.path(tmp_dir, "PCA_Contributions.pdf")); print(fviz_pca_var(pca_obj)); dev.off()
            write.csv(pca_obj$ind$coord, file.path(tmp_dir, "PCA_Coordinates.csv"))
            write.csv(factoextra::get_eigenvalue(pca_obj), file.path(tmp_dir, "PCA_Eigenvalues.csv"))
            
            files_to_zip <- c(files_to_zip, file.path(tmp_dir, "PCA_Biplot.pdf"), 
                              file.path(tmp_dir, "PCA_Scree.pdf"), file.path(tmp_dir, "PCA_Contributions.pdf"),
                              file.path(tmp_dir, "PCA_Coordinates.csv"), file.path(tmp_dir, "PCA_Eigenvalues.csv"))
          }
          # --- Correlation Plot & Data ---
          if (!is.null(pca_results()$pca$call$X)) {
            corr_data <- pca_results()$pca$call$X
            pdf(file.path(tmp_dir, "Correlation_MatrixPlot.pdf"), width=8, height=8)
            corrplot::corrplot(cor(corr_data, use="complete.obs"), method="number", type="upper")
            dev.off()
            
            pdf(file.path(tmp_dir, "Correlation_Pairs.pdf"))
            PerformanceAnalytics::chart.Correlation(corr_data, histogram = FALSE)
            dev.off()
            
            write.csv(cor(corr_data, use="complete.obs"), file.path(tmp_dir, "Correlation_Matrix.csv"))
            
            files_to_zip <- c(files_to_zip, file.path(tmp_dir, "Correlation_MatrixPlot.pdf"), 
                              file.path(tmp_dir, "Correlation_Pairs.pdf"), file.path(tmp_dir, "Correlation_Matrix.csv"))
          }
        }, error = function(e) {
          showNotification(paste("Error saving Analysis 2 results:", e$message), type = "error")
        })
        
        # 2. Zip the files using their full paths.
        zip::zip(zipfile = file, files = files_to_zip, mode = "cherry-pick")
      },
      contentType = "application/zip"
    )
    
  })
}