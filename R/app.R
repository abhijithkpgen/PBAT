# =================== UI Code ===================
# PbAT: Plant Breeding Analytical Tools (Unified Version)

# ===========================================================
# ----------- Block 1: Libraries and NavbarPage Setup --------
# ----------------------- LIBRARIES --------------------------
library(shiny)
library(shinyjs)
library(bslib)
library(readr)
library(dplyr)
library(ggplot2)
library(tibble)
library(purrr)
library(DT)
library(lme4)
library(lmerTest)
library(emmeans)
library(metan)
library(zip)
library(rlang)
library(broom)
library(corrplot)
library(FactoMineR)
library(factoextra)
library(tidyr)
library(PerformanceAnalytics)
library(car)
library(multcomp)
library(semPlot)
library(RColorBrewer)
library(waiter)
library(agricolae)
library(broom.mixed)


ui <- navbarPage(
title = div(
  style = "display: flex; align-items: center; gap: 16px;",
  tags$img(src = "LogoNobg.png", height = "60px", style = "margin-right: 8px;"),
  span("PbAT: Plant Breeding Analytical Tools  v1.0.1")
),   

  id = "main_tabs",
  theme = bs_theme(
    version = 4,
    bootswatch = "cerulean",
	
    primary = "#e17055",   # Soft orange
    secondary = "#00b894"  # Green
  ),

  header = tagList(
    useShinyjs(),
    waiter::use_waiter(),
    tags$head(tags$style(HTML("
      body {
        background: linear-gradient(135deg, #b0e0ff 0%, #fffbe0 100%);
        background-attachment: fixed;
        min-height: 100vh;
      }
      .home-container {
        display: flex;
        justify-content: center;
        align-items: flex-start;
        min-height: 85vh;
        gap: 40px;
        margin-top: 30px;
        background: none;
      }
      .overlay-panel {
        background-color: #142850 !important; /* Navy blue */
        padding: 20px;
        border-radius: 12px;
        box-shadow: 0 0 18px rgba(0,0,0,0.17);
        width: 400px;
        font-size: 13px;
        color: #fafcff !important;           /* Light text */
        border: 1px solid #223870;
      }
      .compact-row {
        display: flex;
        justify-content: space-between;
        gap: 10px;
      }
      .half-width { width: 49%; }
      .btn-primary {
        background-color: #00b894 !important;
        border: none !important;
      }
      .btn-primary:hover {
        background-color: #00cec9 !important;
      }
      /* --- Add this block for NAVBAR --- */
     .navbar, .navbar.navbar-dark, .navbar.bg-primary, .navbar-dark.bg-primary {
  background-color: #142850 !important;
  min-height: 70px !important;
  padding-top: 10px;
  padding-bottom: 10px;
  box-shadow: 0 2px 8px rgba(20, 40, 80, 0.08);
}
.navbar .navbar-brand {
  color: #fff !important;
  font-size: 1.45rem;
  font-weight: bold;
  letter-spacing: 0.5px;
  padding-top: 6px;
  padding-bottom: 6px;
}
.navbar .navbar-nav .nav-link {
  font-size: 1.1rem;
  padding-top: 14px !important;
  padding-bottom: 14px !important;
  /* --- Navy blue font for all sidebarPanel text --- */
.sidebarPanel,
.sidebarPanel label,
.sidebarPanel .control-label,
.sidebarPanel .form-control,
.sidebarPanel .selectize-input,
.sidebarPanel .checkbox,
.sidebarPanel .radio,
.sidebarPanel .checkbox label,
.sidebarPanel .radio label {
  color: #142850 !important;

}


    ")))
    
    
  ),


# ----------------- Block 2: Home Tab (UPDATED) -----------------
tabPanel("Home",
  div(class = "home-container",
    # Left Panel: User Inputs
    div(class = "overlay-panel",
      radioButtons(
        "analysis_mode",
        "Select Analysis Type",
        choices = c(
          "Experimental Design " = "eda",
          "Multivariate Analysis (PCA / GGE / Correlation / Path)" = "multivariate",
          "Mating Design " = "mating"
        ),
        selected = "eda"
      ),
      div(class = "compact-row",
        div(class = "half-width", textInput("user_name", "Name")),
        div(class = "half-width", textInput("user_email", "Email"))
      ),
      textInput("user_institute", "Institute / Organization"),
      fileInput("file", "Upload CSV File", accept = ".csv"),

      # Show Multivariate Analysis options if selected
      conditionalPanel(
        condition = "input.analysis_mode == 'multivariate'",
        selectInput(
          "multi_subtype",
          "Which Multivariate Analysis?",
          choices = c(
            "GGE Biplot" = "gge",
            "Principal Component Analysis (PCA)" = "pca",
            "Correlation Analysis" = "correlation",
            "Path Analysis" = "path"
          ),
          selected = "pca"
        )
      ),

      # Show design UI only for EDA
      conditionalPanel(
        condition = "input.analysis_mode == 'eda'",
        selectInput("design", "Experimental Design",
                    choices = c("Alpha Lattice", "RCBD", "CRD", "Augmented RCBD"))
      ),

      # Show Mating Design selection only for Mating Design Analysis
      conditionalPanel(
        condition = "input.analysis_mode == 'mating'",
        selectInput(
          "md_mating_design",
          "Select Mating Design",
          choices = c(
            "Griffing Method I (Full Diallel: Parents, F1s, Reciprocals)" = "griffing_m1",
            "Griffing Method II (Parents & F1s, No Reciprocals)" = "griffing_m2",
            "Griffing Method III (F1s & Reciprocals, No Parents)" = "griffing_m3",
            "Griffing Method IV (F1s Only, No Parents, No Reciprocals)" = "griffing_m4",
            "Partial Diallel" = "diallel_partial",
            "Line × Tester" = "line_tester"
          ),
          selected = "griffing_m1"
        )
      ),

      actionButton("go_to_analysis", "Proceed to Analysis", class = "btn btn-primary"),
      br(),br(),
      tags$div(style = "font-size: 11px; text-align: center;",
        HTML("Developed by <b>Dr. Abhijith K P</b><br>
              Scientist (Genetics and Plant Breeding), ICAR-IARI Assam<br>
              <a href='mailto:abhijithkpgen@gmail.com'>abhijithkpgen@gmail.com</a>"),
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        tags$div(
          class = "custom-footer",
          HTML("Copyright &copy; 2025 Abhijith Krishnan. Released under the <a href='https://github.com/abhijithkpgen/PBAT/blob/main/LICENSE' target='_blank'>MIT License</a>.")
        )
      )
    ),
    # Right Panel: Workflow Overview
    div(class = "overlay-panel",
      h4(tags$b("Application Workflow Overview")),
      tags$p("PbAT: Plant Breeding Analytical Tools is an end-to-end pipeline for statistical and multivariate analysis of plant breeding data. Follow these steps:"),
      tags$ol(
        tags$li(tags$b("Upload Data:"), " Upload your experimental data in CSV format. Include columns for genotype, location, replication, block (if applicable), and trait(s)."),
        tags$li(tags$b("Select Analysis Type:"), " Choose Experimental Design Analysis, Multivariate Analysis, or Mating Design Analysis depending on your workflow."),
        tags$li(tags$b("Map Data Columns:"), " Assign the appropriate columns (genotype, location, replication, block, etc.) when prompted, to enable correct analysis."),
        tags$li(tags$b("Run Analysis:"), tags$ul(
          tags$li(tags$b(" Experimental Designs:"), " Generate summary statistics, ANOVA, heritability (H²), BLUEs/BLUPs (combined and location-wise), diagnostics, and post-hoc tests. Visualize results with boxplots, QQ plots, and interaction plots."),
          tags$li(tags$b(" Multivariate Analysis:"), " Perform PCA, GGE biplot , correlation, and path analysis on selected traits. Run analyses in standalone mode or linked to your EDA results."),
          tags$li(tags$b(" Mating Design Analysis:"), " Analyze Diallel (Griffing Methods I–IV, PartialDiallel Designs) and  Line × Tester. Obtain GCA/SCA effects, ANOVA tables, and variance components.")
        )),
        tags$li(tags$b(" Review & Download Results:"), " All results—interactive tables, plots, and summaries—can be downloaded as publication-ready PDF/CSV ZIP files for reporting or further analysis.")
      ),
      tags$p(style = "font-size: 11px; margin-top: 10px;",
        "PbAT is designed for plant breeders and researchers to analyze data intuitively without needing knowledge in R. ",
        tags$b("Sample datasets and detailed help are available in the Help & Guide tab.")
      )
    )
  )
),


  # ----------------- Block 3: Analysis 1 Tab -----------------
  tabPanel("Analysis 1",
    sidebarLayout(
      sidebarPanel(width = 4,
        uiOutput("dynamic_sidebar")
      ),
      mainPanel(
        uiOutput("dynamic_mainpanel")
		
      )
    )
  ),

  # ----------------- Block 4: Analysis 2 Tab -----------------
  tabPanel("Analysis 2",
    sidebarLayout(
      sidebarPanel(width = 4,
        selectInput("gge_trait", "Trait for GGE Biplot", choices = NULL),
        actionButton("run_gge", "Run GGE Biplot", class = "btn btn-success"),
        uiOutput("gge_status"),
        hr(),
        checkboxGroupInput("multi_traits", "Select Traits for PCA / Correlation", choices = NULL),
        actionButton("run_pca", "Run PCA", class = "btn btn-primary"),
        uiOutput("pca_status"),
        br(),
        actionButton("run_corr", "Run Correlation Plot", class = "btn btn-secondary"),
        uiOutput("corr_status"),
        downloadButton("download_analysis2", "Download Analysis 2 ZIP", class = "btn btn-success")
      ),
      mainPanel(
        tabsetPanel(
          id = "analysis2_tabs",
          tabPanel(" GGE Biplot",
            h4(" Which Won Where"),
            plotOutput("gge_plot_type1"),
            h4(" Mean vs Stability"),
            plotOutput("gge_plot_type2"),
            h4(" Representativeness vs Discriminativeness"),
            plotOutput("gge_plot_type3")
          ),
          tabPanel(" PCA Plot",
            h4(" PCA Individual Biplot"),
            plotOutput("pca_plot_biplot"),
            h4(" Scree Plot"),
            plotOutput("pca_plot_scree"),
            h4(" Variable Contributions"),
            plotOutput("pca_plot_varcontrib"),
            h4(" PCA Summary Table"),
            tableOutput("pca_table_summary")
          ),
          tabPanel(" Correlation Plot",
            plotOutput("corr_plot1"),
            plotOutput("corr_plot2")
          )
        )
      )
    )
  ),

  # ----------------- Block 5: Multivariate Analysis Tab -----------------
  tabPanel("Multivariate Analysis",
    sidebarLayout(
      sidebarPanel(
        uiOutput("multi_sidebar")
      ),
      mainPanel(
        uiOutput("multi_mainpanel"),
      )
    )
  ),

 # =================== Mating Design Analysis UI (Block M-UI) ===================
tabPanel(
  "Mating Design Analysis",
  sidebarLayout(
    sidebarPanel(
      div(style = "color: #142850; font-size: 15px;",
        # ---- Step 1: Experimental Design ----
        tags$div(
          tags$b("Step 1: Choose Experimental Design"),
          br(),
          selectInput(
            "md_experimental_design",
            NULL,
            choices = c("RCBD"),
            selected = "RCBD"
          ),
          style = "margin-bottom: 18px;"
        ),
        # ---- Step 2: Location Handling ----
        tags$div(
          tags$b("Step 2: Select Location Type"),
          br(),
          radioButtons(
            "md_location_type",
            NULL,
            choices = c("Single Location"),
            selected = "Single Location"
          ),
          style = "margin-bottom: 18px;"
        ),
        # ---- Step 3: (Optional) Model Type Note ----
        conditionalPanel(
          condition = "['griffing_m1','griffing_m2','griffing_m3','griffing_m4'].includes(input.md_mating_design)",
          tags$div(
            style = "background-color: #eef6fa; color: #115370; padding: 10px; margin-bottom: 10px; border-radius: 7px; font-size: 13px; border-left: 4px solid #1d7ec5;",
            HTML("<b>Note:</b> All Griffing diallel analyses here use <b>fixed effects models</b> (as per Griffing, 1956). ")
          )
        ),
        # ---- Step 4: Dynamic Column Selectors ----
        tags$div(
          tags$b("Step 3: Select Columns"),
          br(),
          uiOutput("md_dynamic_columns"),
          style = "margin-bottom: 18px;"
        ),
        # ---- Step 5: Run Analysis ----
        tags$div(
          tags$b("Step 4: Run Analysis"),
          br(),
          actionButton(
            "md_run_analysis",
            "Run Mating Analysis",
            class = "btn btn-primary"
          ),
          style = "margin-bottom: 18px;"
        ),
        # ---- Step 6: Download Results ----
        tags$div(
          tags$b("Step 5: Download Results"),
          br(),
          downloadButton("md_download_results", "Download All Results", class = "btn btn-success"),
          style = "margin-bottom: 0px;"
        )
      )
    ),
    mainPanel(
      div(style = "color: #142850;",
        tabsetPanel(
          tabPanel(
            "ANOVA Table",
            tags$h4("ANOVA Table", style = "color: #142850;"),
            tableOutput("md_anova"),br(),
            # --- INTERPRETATIONS BLOCK ---
            tags$h4("Summary of Results", style = "color: #142850; margin-top: 15px;"),
            uiOutput("md_interpretations"),
            
            
            br(),
            conditionalPanel(
              condition = "input.md_mating_design == 'griffing_m2'",
              tags$h4("Classical ANOVA (Treatment, Rep, Residual)", style = "color: #142850;"),
              tableOutput("md_griffing2_anova")
            )
          ),
          tabPanel(
            "GCA Effects",
            conditionalPanel(
              condition = "input.md_mating_design == 'line_tester'",
              h4("Line GCA Effects", style = "color: #142850;"),
              tableOutput("md_gca_lines"),
              h4("Tester GCA Effects", style = "color: #142850;"),
              tableOutput("md_gca_testers")
            ),
            conditionalPanel(
              condition = "input.md_mating_design != 'line_tester'",
              tableOutput("md_gca")
            )
          ),
          tabPanel(
            "SCA Effects",
            h4("SCA Effects", style = "color: #142850;"),
            tableOutput("md_sca")
          )
        )
      )
    )
  )
),
# =================== End Block M-UI ===================


  # ----------------- Block 6: Help & Guide Tab -----------------
  tabPanel("Help & Guide",
    fluidPage(
      div(style = "padding: 30px;",
        h2("📘 Help & Guide",style = "color: #23272b;"),
       
        tags$h4("📂 Sample Format Downloads",style = "color: #23272b;"),
        tags$ul(
  tags$ul(
  tags$li(tags$a(href = "Alpha_lattice_sample.csv", "📥 Alpha Lattice Sample CSV", target = "_blank")),
  tags$li(tags$a(href = "Augmented_RCBD_Sample.csv", "📥 Augmented RCBD Sample CSV", target = "_blank")),
  tags$li(tags$a(href = "Diallel_Griffing_Method1_Sample.csv", "📥 Griffing Method I Diallel Sample CSV", target = "_blank")),
  tags$li(tags$a(href = "Diallel_Griffing_Method2_Sample.csv", "📥 Griffing Method II Diallel Sample CSV", target = "_blank")),
  tags$li(tags$a(href = "Diallel_Griffing_Method3_Sample.csv", "📥 Griffing Method III Diallel Sample CSV", target = "_blank")),
  tags$li(tags$a(href = "Diallel_Griffing_Method4_Sample.csv", "📥 Griffing Method IV Diallel Sample CSV", target = "_blank")),
  tags$li(tags$a(href = "Factorial_CRD_sample.csv", "📥 Factorial CRD Sample CSV", target = "_blank")),
  tags$li(tags$a(href = "Line_x_Tester_Sample.csv", "📥 Line × Tester Sample CSV", target = "_blank")),
  tags$li(tags$a(href = "Partial_diallel_dummy.csv", "📥 Partial Diallel Sample CSV", target = "_blank")),
   tags$li(tags$a(href = "Biplot_Sample_Format.csv", "📥 Biplot_Sample_Format CSV", target = "_blank")),
   tags$li(tags$a(href = "Mult_Variate_sample_format.csv", "📥 Multi variate Analysis Sample Format CSV", target = "_blank")),
  tags$li(tags$a(href = "RCBD_sample.csv", "📥 RCBD Sample CSV", target = "_blank"))
)
    )
  )
)))

server <- function(input, output, session) {
  
  # This is the new, robust function with a new name.
  add_significance_stars_robust <- function(df) {
    # Find the p-value column by checking for common names
    pval_col_name <- dplyr::case_when(
      "Pr(>F)" %in% names(df) ~ "Pr(>F)",
      "p_value" %in% names(df) ~ "p_value",
      "Pr(>|t|)" %in% names(df) ~ "Pr(>|t|)",
      TRUE ~ NA_character_
    )
    
    # If no p-value column is found, just return the original data frame
    if (is.na(pval_col_name)) {
      return(df)
    }
    
    p_values <- df[[pval_col_name]]
    
    # Use dplyr::case_when for a safe, vectorized way to assign stars
    stars <- dplyr::case_when(
      is.na(p_values)   ~ "",
      p_values < 0.001  ~ "***",
      p_values < 0.01   ~ "**",
      p_values < 0.05   ~ "*",
      p_values < 0.1    ~ ".",
      TRUE              ~ ""
    )
    
    # Add the stars to a new column named 'Signif'
    df$Signif <- stars
    return(df)
  }
  # Place this at the top of your server function
  shared_data <- reactiveValues(
    mating_design = NULL,
    file_data = NULL
  )
  
  # ===================================================================
# HOME TAB LISTENERS
# ===================================================================

# This observer "listens" for a file to be uploaded on the Home tab
observeEvent(input$home_file_input, { # <-- IMPORTANT: See note below
  
  # When a file is uploaded, this action runs.
  req(input$home_file_input$datapath)
  
  # It reads the CSV file and saves the resulting data frame in our shared storage.
  shared_data$file_data <- read.csv(input$home_file_input$datapath)
  
  # This message will appear in your R console to confirm it worked.
  print("Data file has been uploaded and stored successfully.")
})


# This observer "listens" for a change to the dropdown on the Home tab
observeEvent(input$home_design_selector, { # <-- IMPORTANT: See note below
  
  # When the user chooses a design, this action runs.
  # It takes the chosen value (e.g., "line_tester") and saves it in our shared storage.
  shared_data$mating_design <- input$home_design_selector
  
  # This message will appear in your R console to confirm it worked.
  print(paste("Design chosen on Home tab:", shared_data$mating_design))
})
# --------- Block 7: Reactive Values for Data Storage ----------

multi_subtype_selected <- reactiveVal(NULL)
multi_file_data         <- reactiveVal(NULL)
gge_results             <- reactiveVal(NULL)
raw_data                <- reactiveVal(NULL)
descriptive_results     <- reactiveVal(NULL)
model_results           <- reactiveVal(NULL)
crd_results             <- reactiveVal(list())   # <--- Retain only this line for crd_results
pca_results             <- reactiveVal(NULL)
path_results            <- reactiveVal(NULL)
crd_update_trigger      <- reactiveVal(0)
model_results_fixed <- reactiveVal()
model_results_random <- reactiveVal()


# --------- Dynamic Tab Navigation (Hide/Show Tabs) ----------

# Hide all analysis tabs (except Home and Help & Guide) at startup
observe({
  hideTab("main_tabs", "Analysis 1")
  hideTab("main_tabs", "Analysis 2")
  hideTab("main_tabs", "Multivariate Analysis")
  hideTab("main_tabs", "Mating Design Analysis")
})

# Show relevant tabs when user clicks "Proceed to Analysis"
observeEvent(input$go_to_analysis, {
  # Hide all analysis tabs first
  hideTab("main_tabs", "Analysis 1")
  hideTab("main_tabs", "Analysis 2")
  hideTab("main_tabs", "Multivariate Analysis")
  hideTab("main_tabs", "Mating Design Analysis")
  
  # Show and select the right tab(s) based on user's choice
  if (input$analysis_mode == "eda") {
    showTab("main_tabs", "Analysis 1")
    showTab("main_tabs", "Analysis 2")
    updateTabsetPanel(session, "main_tabs", selected = "Analysis 1")
  } else if (input$analysis_mode == "multivariate") {
    showTab("main_tabs", "Multivariate Analysis")
    updateTabsetPanel(session, "main_tabs", selected = "Multivariate Analysis")
  } else if (input$analysis_mode == "mating") {
    showTab("main_tabs", "Mating Design Analysis")
    updateTabsetPanel(session, "main_tabs", selected = "Mating Design Analysis")
  }
})

	# =================== Block M0: Reactive Values for Mating Design (Corrected) ===================


	# Holds the data uploaded via the CSV fileInput in the "Mating Design Analysis" tab
	md_file_data <- shiny::reactiveVal(NULL)

	# Holds the analysis results (after running Griffing, etc.)
	md_results <- shiny::reactiveVal(NULL)

	# =================== End Block M0 ===================


	# --- Block M1: Dynamic Sidebar for Mating Design (No file upload, uses Home tab data) ---

# Dynamic main column selectors for each mating design
output$md_dynamic_columns <- renderUI({
  df <- md_file_data()
  if (is.null(df)) 
    return(div(style = "color: #142850;", h5("Please upload a CSV on the Home tab to select columns.")))
  all_cols <- names(df)
  num_cols <- all_cols[sapply(df, is.numeric)]
  
  div(style = "color: #142850;",
    switch(input$md_mating_design,
      griffing_m1 =, griffing_m2 =, griffing_m3 =, griffing_m4 =,
      diallel_partial = tagList(
        selectInput("md_parent1", "Parent 1 Column", choices = all_cols),
        selectInput("md_parent2", "Parent 2 Column", choices = all_cols),
        selectInput("md_rep", "Replication Column", choices = all_cols),
        selectInput("md_trait", "Trait", choices = num_cols)
      ),
      line_tester = tagList(
        selectInput("md_line", "Line Column", choices = all_cols),
        selectInput("md_tester", "Tester Column", choices = all_cols),
        selectInput("md_rep", "Replication Column", choices = all_cols),
        selectInput("md_type", "Type Column", choices = all_cols, selected = "type"),
        selectInput("md_trait", "Trait", choices = num_cols)
      ),
      nc1 = tagList(
        selectInput("md_set", "Set Column", choices = all_cols),
        selectInput("md_male", "Male Parent Column", choices = all_cols),
        selectInput("md_female", "Female Parent Column", choices = all_cols),
        selectInput("md_rep", "Replication Column", choices = all_cols),
        selectInput("md_trait", "Trait", choices = num_cols)
      ),
      nc2 = tagList(
        selectInput("md_male", "Male Parent Column", choices = all_cols),
        selectInput("md_female", "Female Parent Column", choices = all_cols),
        selectInput("md_rep", "Replication Column", choices = all_cols),
        selectInput("md_trait", "Trait", choices = num_cols)
      ),
      nc3 = tagList(
        selectInput("md_set", "Set Column", choices = all_cols),
        selectInput("md_male", "Male Parent Column", choices = all_cols),
        selectInput("md_female", "Female Parent Column", choices = all_cols),
        selectInput("md_rep", "Replication Column", choices = all_cols),
        selectInput("md_trait", "Trait", choices = num_cols)
      ),
      h5("Select a mating design above.")
    )
  )
})

# =================== End Block M1 ===================


# =================== Block M2: Utility Functions for Mating Analysis ===================

# Set the diagonal (selfs) of a square matrix to NA (for diallel, Griffing, etc.)
set_parent_na <- function(mat) {
  idx <- intersect(rownames(mat), colnames(mat))
  for (i in idx) mat[i, i] <- NA
  mat
}

# Get all unique parent names from two columns (character vectors)
get_all_parents <- function(p1, p2) {
  unique(c(as.character(p1), as.character(p2)))
}

## ===================================================================
## --- CORRECTED GRIFFING METHOD FUNCTIONS (BLOCK X1) ---
## ===================================================================

##=== Block X1.1: Griffing Method I ===
griffing_method1 <- function(df, rep_col = "Rep", male_col = "Male", female_col = "Female", trait_col = "Trait", blk_col = NULL) {
  data_ab1 <- df
  data_ab1$Rep    <- as.factor(data_ab1[[rep_col]])
  if (!is.null(blk_col)) data_ab1$Blk <- as.factor(data_ab1[[blk_col]])
  data_ab1$Male   <- as.character(data_ab1[[male_col]])
  data_ab1$Female <- as.character(data_ab1[[female_col]])
  data_ab1$YVAR   <- as.numeric(as.character(data_ab1[[trait_col]]))
  data_ab1 <- data_ab1[!is.na(data_ab1$Male) & !is.na(data_ab1$Female) & !is.na(data_ab1$YVAR), ]
  
  data_ab1$Male <- factor(data_ab1$Male)
  data_ab1$Female <- factor(data_ab1$Female)
  
  bc <- nlevels(data_ab1$Rep)
  ptypes <- sort(unique(c(as.character(data_ab1$Male), as.character(data_ab1$Female))))
  p <- length(ptypes)
  means_df <- aggregate(YVAR ~ Male + Female, data = data_ab1, mean)
  means_df <- merge(means_df, expand.grid(Male = ptypes, Female = ptypes), all.y = TRUE)
  myMatrix <- matrix(NA, nrow = p, ncol = p, dimnames = list(ptypes, ptypes))
  for (i in 1:nrow(means_df)) {
    myMatrix[as.character(means_df$Male[i]), as.character(means_df$Female[i])] <- means_df$YVAR[i]
  }
  if (any(is.na(myMatrix))) stop("Missing values in means matrix for Griffing I. Ensure all p^2 crosses are present in the data.")
  modelg1 <- lm(YVAR ~ factor(Rep) + factor(paste(Male, Female, sep="_x_")), data = data_ab1)
  anmodel <- anova(modelg1); rownames(anmodel)[nrow(anmodel)] <- "Residual"
  MSEAD <- as.numeric(anmodel[nrow(anmodel), "Mean Sq"]); error_DF <- as.numeric(anmodel[nrow(anmodel), "Df"])
  
  Xi.    <- rowSums(myMatrix)
  X.j    <- colSums(myMatrix)
  Xbar   <- sum(myMatrix)
  acon   <- sum((Xi. + X.j)^2) / (2*p)
  ssgca <- acon - (2/(p^2))*(Xbar^2)
  sssca <- sum(myMatrix * (myMatrix + t(myMatrix)))/2 - acon + (Xbar^2)/(p^2)
  ssrecp <- sum((myMatrix - t(myMatrix))^2)/4
  ssmat <- sum((Xi. - X.j)^2)/(2*p)
  ssnomat <- ssrecp - ssmat
  Df <- c(p-1, p*(p-1)/2, p*(p-1)/2, p-1, (p-2)*(p-1)/2)
  SSS <- c(ssgca, sssca, ssrecp, ssmat, ssnomat)*bc
  MSSS <- SSS/Df
  names(SSS) <- names(MSSS) <- c("GCA", "SCA", "Reciprocal", "Maternal", "No Maternal")
  FVAL <- MSSS/MSEAD
  pval <- 1 - pf(FVAL, Df, error_DF)
  
  anova_diallel <- data.frame(Df=Df, `Sum Sq`=SSS, `Mean Sq`=MSSS, `F value`=FVAL, `Pr(>F)`=pval, row.names=names(SSS), check.names=FALSE)
  anova_diallel <- add_significance_stars_robust(anova_diallel)
  
  anova_error <- data.frame(Df=error_DF, `Sum Sq`=MSEAD*error_DF, `Mean Sq`=MSEAD, `F value`=NA, `Pr(>F)`=NA, Signif="", row.names="Residual", check.names=FALSE)
  anova_final <- rbind(anova_diallel, anova_error)
  
  gca <- (Xi. + X.j) / (2*p) - Xbar/(p^2)
  gca_se <- sqrt(((p-1) * MSEAD) / (2*p*p*bc))
  gca_df <- data.frame(Parent=ptypes, GCA=gca, SE=gca_se, T_value = gca/gca_se)
  gca_df$p_value <- 2 * pt(-abs(gca_df$T_value), df = Df[1])
  gca_df <- add_significance_stars_robust(gca_df)
  
  sca <- (myMatrix + t(myMatrix))/2 - (matrix(Xi. + X.j, nrow=p, ncol=p, byrow=TRUE) + matrix(Xi. + X.j, nrow=p, ncol=p, byrow=FALSE))/(2*p) + Xbar/(p^2)
  sca[lower.tri(sca)] <- NA
  sca_se <- sqrt(((p*p - 2*p + 2) * MSEAD) / (2*p*p*bc))
  sca_df <- data.frame(expand.grid(Female=ptypes, Male=ptypes), SCA=as.vector(sca))
  sca_df <- sca_df[!is.na(sca_df$SCA),]
  sca_df$SE <- sca_se
  sca_df$T_value <- sca_df$SCA / sca_df$SE
  sca_df$p_value <- 2 * pt(-abs(sca_df$T_value), df = Df[2])
  sca_df <- add_significance_stars_robust(sca_df)
  
  return(list(method="I", anova=anova_final, gca=gca_df, sca=sca_df))
}


##=== Block X1.2: Griffing Method II ===
griffing_method2 <- function(df, rep_col = "Rep", male_col = "Male", female_col = "Female", trait_col = "Trait", blk_col = NULL) {
  data_ab1 <- df
  data_ab1$Rep    <- as.factor(data_ab1[[rep_col]])
  if (!is.null(blk_col)) data_ab1$Blk <- as.factor(data_ab1[[blk_col]])
  data_ab1$Male   <- as.character(data_ab1[[male_col]])
  data_ab1$Female <- as.character(data_ab1[[female_col]])
  data_ab1$YVAR   <- as.numeric(as.character(data_ab1[[trait_col]]))
  data_ab1 <- data_ab1[!is.na(data_ab1$Male) & !is.na(data_ab1$Female) & !is.na(data_ab1$YVAR), ]
  
  data_ab1$Male <- factor(data_ab1$Male)
  data_ab1$Female <- factor(data_ab1$Female)
  
  bc <- nlevels(data_ab1$Rep)
  ptypes <- sort(unique(c(as.character(data_ab1$Male), as.character(data_ab1$Female))))
  p <- length(ptypes)
  data_ab1$Cross <- factor(paste(pmin(data_ab1$Female, data_ab1$Male), pmax(data_ab1$Female, data_ab1$Male), sep = "_x_"))
  
  means_df <- aggregate(YVAR ~ Male + Female, data = data_ab1, mean)
  means_df <- merge(means_df, expand.grid(Male = ptypes, Female = ptypes), all.y = TRUE)
  myMatrix <- matrix(NA, nrow = p, ncol = p, dimnames = list(ptypes, ptypes))
  for (i in 1:nrow(means_df)) {
    myMatrix[as.character(means_df$Male[i]), as.character(means_df$Female[i])] <- means_df$YVAR[i]
  }
  
  myMatrix[lower.tri(myMatrix, diag=FALSE)] <- t(myMatrix)[lower.tri(t(myMatrix), diag=FALSE)] # Symmetrize
  if (any(is.na(myMatrix[upper.tri(myMatrix, diag=TRUE)]))) stop("Missing values in means matrix for Griffing II. Ensure p(p+1)/2 crosses are present.")
  
  modelg <- lm(YVAR ~ Cross + Rep, data = data_ab1)
  anmodel <- anova(modelg)
  rownames(anmodel)[nrow(anmodel)] <- "Residual"
  MSEAD <- as.numeric(anmodel["Residual", "Mean Sq"])
  error_DF <- as.numeric(anmodel["Residual", "Df"])
  
  Xi. <- rowSums(myMatrix)
  Xbar <- sum(myMatrix[upper.tri(myMatrix, diag=TRUE)])
  acon <- sum((Xi. + diag(myMatrix))^2) / (p + 2)
  ssgca <- acon - (4 * Xbar^2) / (p * (p + 2))
  sssca <- sum((myMatrix[upper.tri(myMatrix, diag=TRUE)])^2) - acon + (2 * Xbar^2) / ((p + 1) * (p + 2))
  Df <- c(p - 1, p * (p - 1) / 2)
  SSS <- c(ssgca, sssca) * bc
  MSSS <- SSS / Df
  FVAL <- MSSS / MSEAD
  pval <- 1 - pf(FVAL, Df, error_DF)
  
  anova_diallel <- data.frame(Df=Df, `Sum Sq`=SSS, `Mean Sq`=MSSS, `F value`=FVAL, `Pr(>F)`=pval, row.names=c("GCA", "SCA"), check.names=FALSE)
  anova_diallel <- add_significance_stars_robust(anova_diallel)
  
  anova_error <- data.frame(Df=error_DF, `Sum Sq`=MSEAD*error_DF, `Mean Sq`=MSEAD, `F value`=NA, `Pr(>F)`=NA, Signif="", row.names="Residual", check.names=FALSE)
  anova_final <- rbind(anova_diallel, anova_error)
  
  gca <- (Xi. + diag(myMatrix) - 2 * Xbar / p) / (p + 2)
  gca_se <- sqrt(((p - 1) * MSEAD) / (p * (p + 2) * bc))
  gca_df <- data.frame(Parent=ptypes, GCA=gca, SE=gca_se, T_value=gca/gca_se)
  gca_df$p_value <- 2 * pt(-abs(gca_df$T_value), df = Df[1])
  gca_df <- add_significance_stars_robust(gca_df)
  
  sca_mat <- myMatrix - (matrix(Xi.+diag(myMatrix),nrow=p,ncol=p,byrow=TRUE) + matrix(Xi.+diag(myMatrix),nrow=p,ncol=p,byrow=FALSE))/(p+2) + 2*Xbar/((p+1)*(p+2))
  sca_mat[lower.tri(sca_mat)] <- NA
  sca_df <- expand.grid(Male=ptypes, Female=ptypes)
  sca_df$SCA <- as.vector(sca_mat)
  sca_df <- sca_df[!is.na(sca_df$SCA), ]
  sca_se <- sqrt(((p*p + p + 2) * MSEAD) / ((p + 1) * (p + 2) * bc))
  sca_df$SE <- sca_se
  sca_df$T_value <- sca_df$SCA / sca_df$SE
  sca_df$p_value <- 2 * pt(-abs(sca_df$T_value), df = Df[2])
  sca_df <- add_significance_stars_robust(sca_df)
  
  return(list(method="II", anova=anova_final, gca=gca_df, sca=sca_df))
}


## ===================================================================
## --- FINAL CORRECTED GRIFFING METHODS 3 & 4 ---
## ===================================================================

##=== Block X1.3: Griffing Method III ===
griffing_method3 <- function(df, rep_col = "Rep", male_col = "Male", female_col = "Female", trait_col = "Trait", blk_col = NULL) {
  data_ab1 <- df
  data_ab1$Rep    <- as.factor(data_ab1[[rep_col]])
  if (!is.null(blk_col)) data_ab1$Blk <- as.factor(data_ab1[[blk_col]])
  data_ab1$Male   <- as.character(data_ab1[[male_col]])
  data_ab1$Female <- as.character(data_ab1[[female_col]])
  data_ab1$YVAR   <- as.numeric(as.character(data_ab1[[trait_col]]))
  data_ab1 <- data_ab1[!is.na(data_ab1$Male) & !is.na(data_ab1$Female) & !is.na(data_ab1$YVAR), ]
  data_ab1 <- data_ab1[data_ab1$Male != data_ab1$Female, ] # Method 3 excludes selfs
  
  data_ab1$Male <- factor(data_ab1$Male)
  data_ab1$Female <- factor(data_ab1$Female)
  
  bc <- nlevels(data_ab1$Rep)
  ptypes <- sort(unique(c(as.character(data_ab1$Male), as.character(data_ab1$Female))))
  p <- length(ptypes)
  
  # Build means matrix using logic from reference code
  means_df <- aggregate(YVAR ~ Male + Female, data = data_ab1, mean)
  means_df <- merge(means_df, expand.grid(Male = ptypes, Female = ptypes), all.y = TRUE)
  myMatrix <- matrix(NA, nrow = p, ncol = p, dimnames = list(ptypes, ptypes))
  for (i in 1:nrow(means_df)) {
    myMatrix[means_df$Male[i], means_df$Female[i]] <- means_df$YVAR[i]
  }
  diag(myMatrix) <- NA
  
  # Check for missing values in off-diagonal
  if (any(is.na(myMatrix[upper.tri(myMatrix)]) | is.na(myMatrix[lower.tri(myMatrix)]))) {
    stop("Griffing Method III: Incomplete matrix; missing F1 or reciprocal data (excluding selfs).")
  }
  
  modelg <- lm(YVAR ~ factor(Rep) + factor(paste(Male, Female, sep = "_x_")), data = data_ab1)
  anmodel <- anova(modelg); rownames(anmodel)[nrow(anmodel)] <- "Residual"
  MSEAD <- as.numeric(anmodel[nrow(anmodel), "Mean Sq"]); error_DF <- as.numeric(anmodel[nrow(anmodel), "Df"])
  
  Xi.    <- rowSums(myMatrix, na.rm = TRUE)
  X.j    <- colSums(myMatrix, na.rm = TRUE)
  Xbar   <- sum(myMatrix, na.rm = TRUE)
  acon <- sum((Xi. + X.j)^2) / (2 * (p - 2))
  ssgca <- acon - (2 / (p * (p - 2))) * (Xbar^2)
  sssca <- sum((myMatrix + t(myMatrix))^2, na.rm=TRUE)/4 - acon + (Xbar^2)/((p-1)*(p-2))
  ssrecp <- sum((myMatrix - t(myMatrix))^2, na.rm=TRUE)/4
  ssmat <- sum((Xi. - X.j)^2) / (2 * p)
  ssnomat <- ssrecp - ssmat
  Df <- c((p - 1), (p * (p - 3) / 2), (p * (p - 1) / 2), (p - 1), ((p - 2) * (p - 1) / 2))
  SSS <- c(ssgca, sssca, ssrecp, ssmat, ssnomat) * bc
  MSSS <- SSS / Df
  names(SSS) <- names(MSSS) <- c("GCA", "SCA", "Reciprocal", "Maternal", "No Maternal")
  FVAL <- MSSS / MSEAD
  pval <- 1 - pf(FVAL, Df, error_DF)
  
  anova_diallel <- data.frame(Df=Df, `Sum Sq`=SSS, `Mean Sq`=MSSS, `F value`=FVAL, `Pr(>F)`=pval, row.names=names(SSS), check.names=FALSE)
  anova_diallel <- add_significance_stars_robust(anova_diallel)
  
  anova_error <- data.frame(Df=error_DF, `Sum Sq`=MSEAD*error_DF, `Mean Sq`=MSEAD, `F value`=NA, `Pr(>F)`=NA, Signif="", row.names="Residual", check.names=FALSE)
  anova_final <- rbind(anova_diallel, anova_error)
  
  gca <- (p * (Xi. + X.j) - 2 * Xbar) / (2 * p * (p - 2))
  gca_se <- sqrt(((p - 1) * MSEAD) / (2 * p * (p - 2) * bc))
  gca_df <- data.frame(Parent=ptypes, GCA=gca, SE=gca_se, T_value=gca/gca_se)
  gca_df$p_value <- 2 * pt(-abs(gca_df$T_value), df = Df[1])
  gca_df <- add_significance_stars_robust(gca_df)
  
  sca <- (myMatrix + t(myMatrix))/2 - (matrix(Xi.+X.j,nrow=p,ncol=p,byrow=TRUE) + matrix(Xi.+X.j,nrow=p,ncol=p,byrow=FALSE))/(2*(p-2)) + Xbar/((p-1)*(p-2))
  sca[lower.tri(sca, diag = TRUE)] <- NA
  sca_df <- expand.grid(Female=ptypes, Male=ptypes)
  sca_df$SCA <- as.vector(sca)
  sca_df <- sca_df[!is.na(sca_df$SCA), ]
  sca_se <- sqrt(((p - 3) * MSEAD) / (2 * (p - 1) * bc))
  sca_df$SE <- sca_se
  sca_df$T_value <- sca_df$SCA / sca_df$SE
  sca_df$p_value <- 2 * pt(-abs(sca_df$T_value), df = Df[2])
  sca_df <- add_significance_stars_robust(sca_df)
  
  return(list(method="III", anova=anova_final, gca=gca_df, sca=sca_df))
}


##=== Block X1.4: Griffing Method IV ===
griffing_method4 <- function(df, rep_col, male_col, female_col, trait_col, blk_col = NULL) {
  dat <- df[, c(rep_col, male_col, female_col, trait_col)]
  names(dat) <- c("Rep", "Male", "Female", "YVAR")
  dat$YVAR <- as.numeric(as.character(dat$YVAR))
  dat <- dat[!is.na(dat$Male) & !is.na(dat$Female) & !is.na(dat$YVAR), ]
  dat <- dat[dat$Male != dat$Female, ] # Method 4 excludes selfs
  
  dat$Rep <- factor(dat$Rep)
  dat$Cross <- factor(paste(pmin(dat$Female, dat$Male), pmax(dat$Female, dat$Male), sep = "_x_"))
  bc <- nlevels(dat$Rep)
  ptypes <- sort(unique(c(dat$Male, dat$Female)))
  p <- length(ptypes)
  
  # Build means matrix using logic from reference code
  means_df <- aggregate(YVAR ~ Male + Female, data = dat, mean)
  myMatrix <- matrix(NA, nrow = p, ncol = p, dimnames = list(ptypes, ptypes))
  for (i in 1:nrow(means_df)) {
    myMatrix[as.character(means_df$Male[i]), as.character(means_df$Female[i])] <- means_df$YVAR[i]
  }
  
  # Symmetrize the matrix for calculations
  myMatrix[lower.tri(myMatrix)] <- t(myMatrix)[lower.tri(t(myMatrix))]
  if (any(is.na(myMatrix[upper.tri(myMatrix)]))) {
    stop("Incomplete data: One or more cross combinations are missing for Method 4.")
  }
  
  model_g4 <- lm(YVAR ~ Cross + Rep, data = dat)
  anova_g4 <- anova(model_g4)
  MSEAD <- anova_g4["Residuals", "Mean Sq"]
  Randoms_Df <- anova_g4["Residuals", "Df"]
  
  diag(myMatrix) <- NA 
  Xi. <- rowSums(myMatrix, na.rm = TRUE)
  Xbar <- sum(myMatrix, na.rm = TRUE) / 2 
  
  ssgca <- (1/(p-2)) * sum(Xi.^2) - (4*Xbar^2)/(p*(p-2))
  sssca <- sum(myMatrix[upper.tri(myMatrix)]^2) - (1/(p-2))*sum(Xi.^2) + (2*Xbar^2)/((p-1)*(p-2))
  
  Df <- c(p - 1, p * (p - 3) / 2)
  SSS <- c(ssgca, sssca) * bc
  MSSS <- SSS / Df
  FVAL <- MSSS / MSEAD
  pval <- 1 - pf(FVAL, Df, Randoms_Df)
  
  anova_diallel <- data.frame(Df=Df, `Sum Sq`=SSS, `Mean Sq`=MSSS, `F value`=FVAL, `Pr(>F)`=pval, row.names=c("GCA", "SCA"), check.names=FALSE)
  anova_diallel <- add_significance_stars_robust(anova_diallel)
  
  anova_error <- data.frame(Df=Randoms_Df, `Sum Sq`=MSEAD*Randoms_Df, `Mean Sq`=MSEAD, `F value`=NA, `Pr(>F)`=NA, Signif="", row.names="Residual", check.names=FALSE)
  anova_final <- rbind(anova_diallel, anova_error)
  
  gcaeff <- (1/(p-2)) * (Xi. - (2 * Xbar / p))
  gca_se <- sqrt(((p-1)*MSEAD)/(bc*p*(p-2)))
  gca_tab <- data.frame(Parent=ptypes, GCA=gcaeff, SE=gca_se)
  gca_tab <- add_significance_stars_robust(gca_tab)
  
  mu_hat <- (2*Xbar)/(p*(p-1))
  gca_m1 <- matrix(gcaeff, nrow=p, ncol=p, byrow=TRUE)
  gca_m2 <- matrix(gcaeff, nrow=p, ncol=p, byrow=FALSE)
  scaeffmat <- myMatrix - gca_m1 - gca_m2 - mu_hat
  scaeffmat[lower.tri(scaeffmat, diag=TRUE)] <- NA
  
  sca_tab <- data.frame(expand.grid(Female=ptypes, Male=ptypes), SCA=as.vector(scaeffmat))
  sca_tab <- sca_tab[!is.na(sca_tab$SCA),]
  sca_tab$SE <- sqrt(((p-3)*MSEAD)/(bc*(p-1)))
  sca_tab <- add_significance_stars_robust(sca_tab)
  
  return(list(method="IV", anova=anova_final, gca=gca_tab, sca=sca_tab))
}
# =================== Block X1.5: Robust Partial Diallel Analysis ===================

diallel_partial_manual <- function(df, trait, p1, p2, rep) {
  Y <- df[[trait]]
  P1 <- as.character(df[[p1]])
  P2 <- as.character(df[[p2]])
  Rep <- as.factor(df[[rep]])
  
  mask <- !is.na(Y)
  Y <- Y[mask]; P1 <- P1[mask]; P2 <- P2[mask]; Rep <- Rep[mask]
  parents <- sort(unique(c(P1, P2)))
  n_par <- length(parents)
  mu <- mean(Y, na.rm = TRUE)
  n_obs <- length(Y)
  
  # ---- Calculate s: Crosses per parent ----
  s_tab <- table(c(P1, P2))
  s_vals <- as.numeric(s_tab)
  s <- mean(s_vals)
  is_balanced <- all(s_vals == s)
  if (!is_balanced) warning(
    paste0("[Partial Diallel] Unbalanced design detected: crosses per parent are not equal (s values: ",
           paste(unique(s_vals), collapse = ","), "). Results use average s = ", s, "."))
  
  # ---- Replication (assume all observed crosses have same # of reps) ----
  cross_ids <- paste(pmin(P1, P2), pmax(P1, P2), sep = ":")
  reps_per_cross <- as.numeric(median(table(cross_ids))) # 
  if (any(table(cross_ids) != reps_per_cross)) {
    warning("[Partial Diallel] Unbalanced replication detected across crosses! Using modal # of reps.")
  }
  
  # ---- GCA effects: Least squares (sum gi = 0 constraint) ----
  Xg <- matrix(0, nrow = n_obs, ncol = n_par)
  for (i in 1:n_obs) {
    Xg[i, which(parents == P1[i])] <- 1
    Xg[i, which(parents == P2[i])] <- 1
  }
  Xg_red <- Xg[, -n_par, drop = FALSE]
  Y_c <- Y - mu
  gca_hat_red <- solve(t(Xg_red) %*% Xg_red, t(Xg_red) %*% Y_c)
  gca_hat <- c(gca_hat_red, -sum(gca_hat_red))
  names(gca_hat) <- parents
  
  # ---- SCA effects: average SCA per unique cross ----
  cross_names_unique <- unique(cross_ids)
  sca_cross <- numeric(length(cross_names_unique))
  names(sca_cross) <- cross_names_unique
  for (cr_name in cross_names_unique) {
    rows <- which(cross_ids == cr_name)
    ptemp <- unlist(strsplit(cr_name, ":"))
    expected <- mu + gca_hat[ptemp[1]] + gca_hat[ptemp[2]]
    sca_cross[cr_name] <- mean(Y[rows]) - expected
  }
  # Fitted SCA for each observation
  sca_fitted_for_each_obs <- sca_cross[cross_ids]
  
  # ---- Replication effects ----
  rep_means <- tapply(Y, Rep, mean)
  rep_effects <- rep_means - mu
  rep_effect_for_each_obs <- rep_effects[as.character(Rep)]
  
  # ---- Fitted value and Error ----
  fitted_values <- mu + rep_effect_for_each_obs +
                   gca_hat[P1] + gca_hat[P2] +
                   sca_fitted_for_each_obs
  residuals <- Y - fitted_values
  
  # ---- Build ANOVA: Partition SS/df as in the paper ----
  # Replication SS/df
  ss_rep <- sum(table(Rep) * (rep_means - mu)^2)
  df_rep <- length(unique(Rep)) - 1
  
  # GCA SS/df: 2*s*sum(g_i^2) (Kempthorne & Curnow, Table 1)
  ss_gca <- 2 * s * sum(gca_hat^2)
  df_gca <- n_par - 1
  
  # SCA SS/df: sum over observed crosses (SCA^2 * #replicates)
  ss_sca <- sum(sca_cross^2) * reps_per_cross
  df_sca <- length(unique(cross_ids)) - n_par
  
  # Error: residual sum of squares
  ss_err <- sum(residuals^2)
  df_err <- n_obs - (df_rep + df_gca + df_sca + 1)
  
  # Total
  ss_total <- sum((Y - mu)^2)
  df_total <- n_obs - 1
  
  # Mean Squares
  ms_rep <- ss_rep / df_rep
  ms_gca <- ss_gca / df_gca
  ms_sca <- ss_sca / df_sca
  ms_err <- ss_err / df_err
  
  # F values (vs. error)
  f_gca <- ms_gca / ms_err
  f_sca <- ms_sca / ms_err
  f_rep <- ms_rep / ms_err
  
  # p-values
  p_gca <- pf(f_gca, df_gca, df_err, lower.tail = FALSE)
  p_sca <- pf(f_sca, df_sca, df_err, lower.tail = FALSE)
  p_rep <- pf(f_rep, df_rep, df_err, lower.tail = FALSE)
  
  # Significance stars
  stars <- function(p) {
    if (is.na(p)) return("")
    if (p < 0.001) "***" else if (p < 0.01) "**" else if (p < 0.05) "*" else if (p < 0.1) "." else ""
  }
  
  # ANOVA Table
  anova_tab <- data.frame(
    Source = c("Replication", "GCA", "SCA", "Error", "Total"),
    Df = c(df_rep, df_gca, df_sca, df_err, df_total),
    "Sum Sq" = c(ss_rep, ss_gca, ss_sca, ss_err, ss_total),
    "Mean Sq" = c(ms_rep, ms_gca, ms_sca, ms_err, NA),
    "F value" = c(f_rep, f_gca, f_sca, NA, NA),
    "Pr(>F)" = c(p_rep, p_gca, p_sca, NA, NA),
    Stars = sapply(c(p_rep, p_gca, p_sca, NA, NA), stars),
    check.names = FALSE
  )
  
  # ---- Variance components: Table 1 in Kempthorne & Curnow ----
  r <- reps_per_cross
  n <- n_par
  # s already defined above
  sigma2_sca <- (ms_sca - ms_err) / r
  sigma2_gca <- ((ms_gca - ms_sca) * (n - 1)) / (r * s * (n - 2))
  
  var_tab <- data.frame(
    Variance_Component = c("GCA", "SCA"),
    Value = c(sigma2_gca, sigma2_sca)
  )
  
  # Output GCA, SCA effects as data.frame
  gca_tab <- data.frame(Parent = parents, GCA = gca_hat)
  sca_tab <- data.frame(Cross = names(sca_cross), SCA = sca_cross)
  
  list(
    anova = anova_tab,
    gca = gca_tab,
    sca = sca_tab,
    var = var_tab
  )
}
# =================== End Block X1.5 ===================

# =============================================================================
# --- Ab_07-08-2025 Line x Tester Function (Corrected t-tests) ---
# =============================================================================


line_tester_manual <- function(data, line_col, tester_col, rep_col, trait_col, type_col) {
  
  # --- Internal Helper Function for Significance Stars ---
  get_stars <- function(p_values) {
    dplyr::case_when(
      is.na(p_values)   ~ "",
      p_values < 0.001  ~ "***",
      p_values < 0.01   ~ "**",
      p_values < 0.05   ~ "*",
      p_values < 0.1    ~ ".",
      TRUE              ~ ""
    )
  }
  
  # --- Main Analysis Block ---
  tryCatch({
    # 1. DATA PREPARATION AND VALIDATION
    df <- data.frame(
      Rep    = as.factor(data[[rep_col]]),
      Line   = as.factor(data[[line_col]]),
      Tester = as.factor(data[[tester_col]]),
      Type   = as.factor(data[[type_col]]),
      Y      = as.numeric(data[[trait_col]])
    )
    df <- na.omit(df)
    
    if (!any(df$Type == "cross")) {
      stop("The 'Type' column must contain entries with the exact value 'cross'.")
    }
    
    parents <- df[df$Type != "cross", ]
    crosses <- df[df$Type == "cross", ]
    crosses$Line   <- droplevels(crosses$Line)
    crosses$Tester <- droplevels(crosses$Tester)
    
    has_parents <- nrow(parents) > 0
    l <- nlevels(crosses$Line)
    t <- nlevels(crosses$Tester)
    
    if (l < 2 || t < 1) {
      stop("Analysis requires at least two lines and one tester in the cross data.")
    }
    
    # 2. ANOVA CALCULATIONS
    df$TreatmentID <- interaction(df$Line, df$Tester, df$Type, drop = TRUE)
    model_overall <- aov(Y ~ Rep + TreatmentID, data = df)
    anova_overall <- anova(model_overall)
    
    MS_Error <- anova_overall["Residuals", "Mean Sq"]
    DF_Error <- anova_overall["Residuals", "Df"]
    
    model_crosses <- aov(Y ~ Line * Tester, data = crosses)
    anova_crosses <- anova(model_crosses)
    
    DF_Parents <- 0; SS_Parents <- 0
    if (has_parents && nlevels(droplevels(parents$Line)) > 1) {
      parents$Parent <- droplevels(parents$Line)
      model_parents <- aov(Y ~ Parent, data = parents)
      anova_parents <- anova(model_parents)
      DF_Parents <- anova_parents["Parent", "Df"]
      SS_Parents <- anova_parents["Parent", "Sum Sq"]
    }
    
    DF_PvC <- 0; SS_PvC <- 0
    if (has_parents) {
      df$PvC <- factor(ifelse(df$Type == "cross", "Cross", "Parent"), levels = c("Parent", "Cross"))
      model_pvc <- aov(Y ~ PvC, data = df)
      anova_pvc <- anova(model_pvc)
      DF_PvC <- anova_pvc["PvC", "Df"]
      SS_PvC <- anova_pvc["PvC", "Sum Sq"]
    }
    
    # 3. ASSEMBLE ANOVA TABLE
    ss_crosses_total <- sum(anova_crosses[c("Line", "Tester", "Line:Tester"), "Sum Sq"])
    df_crosses_total <- sum(anova_crosses[c("Line", "Tester", "Line:Tester"), "Df"])
    
    source_names <- c("Replications", "Treatments", "  Parents", "  Parents vs. Crosses", "  Crosses",
                      "    Lines", "    Testers", "    Lines X Testers", "Error", "Total")
    
    DF <- c(anova_overall["Rep", "Df"], anova_overall["TreatmentID", "Df"], DF_Parents, DF_PvC, 
            df_crosses_total, anova_crosses["Line", "Df"], anova_crosses["Tester", "Df"], 
            anova_crosses["Line:Tester", "Df"], DF_Error, sum(anova_overall[, "Df"], na.rm = TRUE))
    
    SS <- c(anova_overall["Rep", "Sum Sq"], anova_overall["TreatmentID", "Sum Sq"], SS_Parents, SS_PvC,
            ss_crosses_total, anova_crosses["Line", "Sum Sq"], anova_crosses["Tester", "Sum Sq"], 
            anova_crosses["Line:Tester", "Sum Sq"], anova_overall["Residuals", "Sum Sq"], 
            sum(anova_overall[, "Sum Sq"], na.rm = TRUE))
    
    MS <- ifelse(DF > 0, SS / DF, 0)
    F_value <- MS / MS_Error
    P_value <- pf(F_value, DF, DF_Error, lower.tail = FALSE)
    
    anova_final <- data.frame(Source = source_names, Df = DF, `Sum Sq` = SS, `Mean Sq` = MS, 
                              `F value` = F_value, `Pr(>F)` = P_value, check.names = FALSE)
    
    anova_final[anova_final$Source == "Error", "Mean Sq"] <- MS_Error
    rows_to_blank <- c("Total")
    cols_to_blank <- c("Mean Sq", "F value", "Pr(>F)")
    anova_final[anova_final$Source %in% rows_to_blank, cols_to_blank] <- NA
    
    anova_final$Stars <- get_stars(anova_final$`Pr(>F)`)
    
    numeric_cols <- c("Sum Sq", "Mean Sq", "F value", "Pr(>F)")
    anova_final[numeric_cols] <- lapply(anova_final[numeric_cols], function(x) sprintf("%.2f", x))
    anova_final[is.na(anova_final) | anova_final == "NA"] <- ""
    
    # 4. CALCULATE GCA/SCA EFFECTS WITH CORRECT P-VALUES
    grand_mean <- mean(crosses$Y)
    
    # GCA for Lines
    emm_lines <- emmeans(model_crosses, ~ Line)
    summary_lines <- as.data.frame(summary(emm_lines))
    gca_lines_out <- data.frame(Line = summary_lines$Line, GCA = summary_lines$emmean - grand_mean, SE = summary_lines$SE)
    gca_lines_out$`t value` <- gca_lines_out$GCA / gca_lines_out$SE
    gca_lines_out$`Pr(>|t|)` <- 2 * pt(-abs(gca_lines_out$`t value`), df = DF_Error)
    gca_lines_out$Stars <- get_stars(gca_lines_out$`Pr(>|t|)`)
    
    # GCA for Testers
    emm_testers <- emmeans(model_crosses, ~ Tester)
    summary_testers <- as.data.frame(summary(emm_testers))
    gca_testers_out <- data.frame(Tester = summary_testers$Tester, GCA = summary_testers$emmean - grand_mean, SE = summary_testers$SE)
    gca_testers_out$`t value` <- gca_testers_out$GCA / gca_testers_out$SE
    gca_testers_out$`Pr(>|t|)` <- 2 * pt(-abs(gca_testers_out$`t value`), df = DF_Error)
    gca_testers_out$Stars <- get_stars(gca_testers_out$`Pr(>|t|)`)
    
    # SCA Effects
    emm_sca <- emmeans(model_crosses, ~ Line:Tester)
    summary_sca <- as.data.frame(summary(emm_sca))
    
    summary_sca <- merge(summary_sca, gca_lines_out[, c("Line", "GCA")], by = "Line")
    names(summary_sca)[names(summary_sca) == "GCA"] <- "GCA_Line"
    summary_sca <- merge(summary_sca, gca_testers_out[, c("Tester", "GCA")], by = "Tester")
    names(summary_sca)[names(summary_sca) == "GCA"] <- "GCA_Tester"
    
    summary_sca$SCA <- summary_sca$emmean - summary_sca$GCA_Line - summary_sca$GCA_Tester - grand_mean
    
    sca_out <- data.frame(Line = summary_sca$Line, Tester = summary_sca$Tester, SCA = summary_sca$SCA, SE = summary_sca$SE)
    sca_out$`t value` <- sca_out$SCA / sca_out$SE
    sca_out$`Pr(>|t|)` <- 2 * pt(-abs(sca_out$`t value`), df = DF_Error)
    sca_out$Stars <- get_stars(sca_out$`Pr(>|t|)`)
    
    # 5. RETURN ALL RESULTS
    list(
      anova_full  = anova_final,
      gca_lines   = gca_lines_out,
      gca_testers = gca_testers_out,
      sca         = sca_out
    )
    
  }, error = function(e) {
    list(error = paste("Line x Tester analysis failed:", e$message))
  })
}


# ---- BLOCK X2: Mating Design runner (Corrected Final Version) ----
run_mating_ui <- function(df, input) {
  
  design <- input$md_mating_design
  
  if (design == "diallel_partial") {
    req_cols <- c(input$md_trait, input$md_parent1, input$md_parent2, input$md_rep)
    if (any(!req_cols %in% names(df))) {
      return(list(error = paste("Missing column(s):", paste(req_cols[!req_cols %in% names(df)], collapse = ", "))))
    }
    res <- diallel_partial_manual(df, input$md_trait, input$md_parent1, input$md_parent2, input$md_rep)
    
  } else if (design == "line_tester") {
    req_cols <- c(input$md_line, input$md_tester, input$md_rep, input$md_trait, input$md_type)
    if (any(!req_cols %in% names(df))) {
      return(list(error = paste("Missing column(s):", paste(req_cols[!req_cols %in% names(df)], collapse = ", "))))
    }
    res <- line_tester_manual(df, input$md_line, input$md_tester, input$md_rep, input$md_trait, input$md_type)
    
  } else { # This block now handles all Griffing methods
    
    blk <- NULL # Block column is not used in this context
    
    req_cols <- c(input$md_trait, input$md_parent1, input$md_parent2, input$md_rep)
    if (any(!req_cols %in% names(df))) {
      return(list(error = paste("Missing column(s):", paste(req_cols[!req_cols %in% names(df)], collapse = ", "))))
    }
    
    # ** THE FIX IS HERE: Arguments are now named and in the correct order **
    if (design == "griffing_m1") {
      res <- griffing_method1(df, rep_col=input$md_rep, male_col=input$md_parent1, female_col=input$md_parent2, trait_col=input$md_trait, blk_col=blk)
    } else if (design == "griffing_m2") {
      df$Parent1_std <- pmin(as.character(df[[input$md_parent1]]), as.character(df[[input$md_parent2]]))
      df$Parent2_std <- pmax(as.character(df[[input$md_parent1]]), as.character(df[[input$md_parent2]]))
      res <- griffing_method2(df, rep_col=input$md_rep, male_col="Parent1_std", female_col="Parent2_std", trait_col=input$md_trait, blk_col=blk)
    } else if (design == "griffing_m3") {
      res <- griffing_method3(df, rep_col=input$md_rep, male_col=input$md_parent1, female_col=input$md_parent2, trait_col=input$md_trait, blk_col=blk)
    } else if (design == "griffing_m4") {
      res <- griffing_method4(df, rep_col=input$md_rep, male_col=input$md_parent1, female_col=input$md_parent2, trait_col=input$md_trait, blk_col=blk)
    }
  }
  
  return(res)
}

# ---- BLOCK X3: Server observe and table outputs (with Interpretations) ----

md_results <- reactiveVal(NULL)

observeEvent(input$md_run_analysis, {
  cat("\n============================================\n")
  cat("DEBUG: 'Run Mating Analysis' button clicked.\n")
  
  # Reset previous results
  md_results(NULL)
  
  # Check if data exists before proceeding
  req(md_file_data())
  cat("DEBUG: Data is available. Calling 'run_mating_ui'...\n")
  
  tryCatch({
    # Call the main runner function which selects the correct analysis
    res <- run_mating_ui(md_file_data(), input)
    
    cat("DEBUG: 'run_mating_ui' has finished. Checking the result object...\n")
    
    # Check if the result object contains a controlled error message
    if (!is.null(res$error)) {
      cat("ERROR: The analysis function returned a controlled error: ", res$error, "\n")
      showModal(modalDialog(title = "Analysis Error", res$error))
      md_results(NULL)
    } else {
      cat("SUCCESS: Analysis was successful. Storing results.\n")
      md_results(res) # Store the successful results
      showNotification("Mating Design Analysis Completed ✅", type = "message")
    }
    
  }, error = function(e) {
    # This block catches any UNEXPECTED crash during the analysis
    cat("FATAL ERROR: An unexpected error was caught by the top-level tryCatch.\n")
    cat("--> The error message is: ", e$message, "\n") # This is the key info we need
    
    showModal(modalDialog(
      title = "Fatal Analysis Error",
      # This is the message from your screenshot
      paste("Error in mating analysis:", e$message),
      easyClose = TRUE
    ))
    md_results(NULL)
  })
  cat("============================================\n\n")
})

# ----- ANOVA Table Output -----
output$md_anova <- renderTable({
  req(md_results())
  res <- md_results()
  # This handles output from both the new Line x Tester and other designs
  if (!is.null(res$anova_full)) {
    res$anova_full
  } else if (!is.null(res$anova)) {
    res$anova
  } else {
    NULL
  }
}, rownames = TRUE)

output$md_griffing2_anova <- renderTable({
  req(md_results())
  res <- md_results()
  # Show ONLY for Griffing II
  if (!is.null(res$griffing_anova) && isTRUE(res$method == "II")) {
    res$griffing_anova
  } else {
    NULL
  }
}, rownames = TRUE)


# ----- GCA Effects Output: smart display -----
# This is for non-Line-Tester designs. It is hidden by a conditionalPanel in the UI.
output$md_gca <- renderTable({
  req(md_results())
  res <- md_results()
  # Explicitly do nothing if the results are from Line x Tester
  if (!is.null(res$gca_lines) && !is.null(res$gca_testers)) {
    return(NULL)
  }
  # Render GCA for other designs (e.g., Griffing)
  if (!is.null(res$gca)) {
    return(res$gca)
  }
  return(NULL)
}, rownames = FALSE)

# For Line × Tester: GCA Effects - Lines
output$md_gca_lines <- renderTable({
  req(md_results())
  res <- md_results()
  if (!is.null(res$gca_lines)) res$gca_lines else NULL
}, rownames = FALSE)

# For Line × Tester: GCA Effects - Testers
output$md_gca_testers <- renderTable({
  req(md_results())
  res <- md_results()
  if (!is.null(res$gca_testers)) res$gca_testers else NULL
}, rownames = FALSE)

# ----- SCA Output -----
output$md_sca <- renderTable({
  req(md_results())
  res <- md_results()
  if (!is.null(res$sca)) res$sca else NULL
}, rownames = FALSE)

# ----- Variance Components Output (for NC3) -----
output$md_variance_components <- renderTable({
  req(md_results())
  res <- md_results()
  if (!is.null(res$variance_components)) res$variance_components else NULL
}, rownames = FALSE)

# =============================================================================
# --- Dynamic Interpretations UI ---
# =============================================================================
# This block generates a text summary of the analysis results.

output$md_interpretations <- renderUI({
  # Wait for the results to be ready
  req(md_results())
  res <- md_results()
  
  # Ensure it's a line x tester result and not an error
  if (!is.null(res$error) || is.null(res$anova_full)) {
    return(tags$p("Interpretations not generated "))
  }
  
  # --- Helper function to generate the text ---
  generate_interpretations <- function(results) {
    
    # Re-parse the formatted ANOVA table to get numeric p-values
    anova <- results$anova_full
    # Temporarily remove non-numeric columns for parsing
    stars_col <- anova$Stars
    source_col <- anova$Source
    anova$Stars <- NULL
    anova$Source <- NULL
    # Convert back to numeric, coercing blanks to NA
    anova <- data.frame(lapply(anova, function(x) as.numeric(as.character(x))))
    anova$Source <- source_col
    
    # Extract key p-values
    # Note the extra spaces to match the source names exactly
    p_lines   <- anova$`Pr..F.`[anova$Source == "    Lines"]
    p_testers <- anova$`Pr..F.`[anova$Source == "    Testers"]
    p_lxt     <- anova$`Pr..F.`[anova$Source == "    Lines X Testers"]
    
    # --- Build Interpretation List ---
    interpretations <- list()
    
    # 1. GCA Interpretation (Additive Effects)
    if (!is.na(p_lines) && p_lines < 0.05) {
      interpretations <- append(interpretations, list(tags$li(
        tags$b("Significant Line Effects:"), " The significant p-value for 'Lines' indicates that there are real, statistically significant differences in the general combining ability (GCA) among the lines. This suggests that some lines are consistently better parents for improving the trait."
      )))
    } else {
      interpretations <- append(interpretations, list(tags$li(
        tags$b("Non-Significant Line Effects:"), " The non-significant p-value for 'Lines' suggests that there are no significant differences in the general combining ability (GCA) among the lines."
      )))
    }
    
    if (!is.na(p_testers) && p_testers < 0.05) {
      interpretations <- append(interpretations, list(tags$li(
        tags$b("Significant Tester Effects:"), " The significant p-value for 'Testers' indicates that there are real differences in the GCA among the testers."
      )))
    }
    
    # 2. SCA Interpretation (Non-Additive Effects)
    if (!is.na(p_lxt) && p_lxt < 0.05) {
      interpretations <- append(interpretations, list(tags$li(
        tags$b("Significant SCA Effects:"), " The significant 'Lines X Testers' interaction indicates that specific combinations of lines and testers perform unexpectedly better or worse than predicted by their GCA alone. This highlights the importance of non-additive gene action (dominance/epistasis) for this trait."
      )))
    } else {
      interpretations <- append(interpretations, list(tags$li(
        tags$b("Non-Significant SCA Effects:"), " The non-significant 'Lines X Testers' interaction suggests that the performance of a cross can be reliably predicted by the GCA of its parents alone. Additive gene action is likely more important than non-additive effects for this trait."
      )))
    }
    
    # 3. Identify Best Combiners
    # Best General Combiner (Line)
    best_line <- results$gca_lines %>% filter(GCA == max(GCA))
    interpretations <- append(interpretations, list(tags$li(
      tags$b("Best General Combiner (Line):"), paste0(" The line '", best_line$Line, "' showed the highest positive GCA value (", sprintf("%.2f", best_line$GCA), "), making it the best parent for generally improving this trait across different combinations.")
    )))
    
    # Best Specific Combination (Cross)
    best_cross <- results$sca %>% filter(SCA == max(SCA))
    interpretations <- append(interpretations, list(tags$li(
      tags$b("Best Specific Combination:"), paste0(" The cross '", best_cross$Line, " x ", best_cross$Tester, "' had the highest positive SCA value (", sprintf("%.2f", best_cross$SCA), "), indicating outstanding performance due to specific, non-additive gene interactions.")
    )))
    
    return(tags$ul(style = "font-size: 15px; line-height: 1.6;", interpretations))
  }
  
  # --- Render the UI ---
  div(
    style = "background-color: #f8f9fa; border: 1px solid #dee2e6; border-radius: 8px; padding: 20px;",
    generate_interpretations(res)
  )
})


 # -------- Ab_02-06-2025--Block V1: Dynamic Multivariate Sidebar UI (Fully Fixed for Path Analysis) --------
output$multi_sidebar <- renderUI({
  req(multi_subtype_selected(), multi_file_data())

  df <- multi_file_data()
  choices_numeric <- names(df)[sapply(df, is.numeric)]
  choices_all <- names(df)
  choices_all_with_none <- c("None", choices_all)

  # Helper function for step labels and spacing
  step_block <- function(label, ...) {
    tags$div(
      tags$b(label),
      br(),
      ...,
      style = "margin-bottom: 20px;"
    )
  }

  # All UI inside a navy blue font div
  div(style = "color: #142850; font-size: 15px;",
    if (multi_subtype_selected() == "gge") {
      tagList(
        step_block("Step 1: Select Genotype & Location Columns",
          selectInput("multi_gge_genotype", "Genotype Column", choices = choices_all),
          selectInput("multi_gge_location", "Location Column", choices = choices_all)
        ),
        step_block("Step 2: Select Trait for GGE",
          selectInput("multi_gge_trait", "Trait", choices = choices_numeric)
        ),
        step_block("Step 3: Run and Download",
          actionButton("multi_run_gge", "Run GGE Biplot", class = "btn btn-success"),
          uiOutput("multi_gge_status"),
          br(),
          downloadButton("multi_gge_download", "Download GGE Plots (ZIP)", class = "btn btn-primary")
        )
      )
    } else if (multi_subtype_selected() == "pca") {
      tagList(
        step_block("Step 1: Select Genotype Column",
          selectInput("multi_pca_genotype", "Genotype Column", choices = choices_all)
        ),
        step_block("Step 2: Select Traits for PCA",
          checkboxGroupInput("multi_pca_traits", "Traits for PCA", choices = choices_numeric)
        ),
        step_block("Step 3: Run and Download",
          actionButton("multi_run_pca", "Run PCA", class = "btn btn-success"),
          uiOutput("multi_pca_status"),
          br(),
          downloadButton("multi_pca_download", "Download PCA Results (ZIP)", class = "btn btn-primary")
        )
      )
    } else if (multi_subtype_selected() == "correlation") {
      tagList(
        step_block("Step 1: Select Genotype Column",
          selectInput("multi_corr_genotype", "Genotype Column", choices = choices_all)
        ),
        step_block("Step 2: Select Traits for Correlation",
          checkboxGroupInput("multi_corr_traits", "Traits for Correlation", choices = choices_numeric)
        ),
        step_block("Step 3: Run and Download",
          actionButton("multi_run_corr", "Run Correlation Analysis", class = "btn btn-success"),
          uiOutput("multi_corr_status"),
          br(),
          downloadButton("multi_corr_download", "Download Correlation Results (ZIP)", class = "btn btn-primary")
        )
      )
    } else if (multi_subtype_selected() == "path") {
      tagList(
        step_block("Step 1: Select Genotype Column (optional)",
          selectInput(
            "multi_path_genotype",
            "Genotype Column (optional, for grouping/labeling)",
            choices = choices_all_with_none,
            selected = "None"
          )
        ),
        step_block("Step 2: Select Dependent and Independent Traits",
          selectInput(
            "multi_path_dep",
            "Dependent Trait (only one)",
            choices = choices_numeric,
            selected = isolate(input$multi_path_dep) %||% NULL
          ),
          selectInput(
            "multi_path_indep",
            "Independent Traits (select two or more)",
            choices = setdiff(choices_numeric, input$multi_path_dep),
            multiple = TRUE,
            selected = {
              # Only select if current selection valid, else NULL
              valid_indep <- setdiff(choices_numeric, input$multi_path_dep)
              sel <- isolate(input$multi_path_indep)
              if (is.null(sel) || !all(sel %in% valid_indep) || length(sel) < 2) NULL else sel
            }
          )
        ),
        step_block("Step 3: Run and Download",
          actionButton("multi_run_path", "Run Path Analysis", class = "btn btn-success"),
          uiOutput("multi_path_status"),
          br(),
          downloadButton("multi_path_download", "Download Path Analysis (ZIP)", class = "btn btn-primary")
        )
      )
    }
  )
})


# ---- Dynamic Observers for Path Trait Selection (ADD immediately after Block 8!) ----
observe({
  req(multi_subtype_selected() == "path")
  df <- multi_file_data()
  req(df)
  num_cols <- names(df)[sapply(df, is.numeric)]
  all_cols <- c("None", names(df))

  # Update options, but do not force a selection
  updateSelectInput(session, "multi_path_genotype",
                    choices = all_cols)
  updateSelectInput(session, "multi_path_dep",
                    choices = num_cols)
})

observeEvent(input$multi_path_dep, {
  req(multi_subtype_selected() == "path")
  df <- multi_file_data()
  req(df)
  num_cols <- names(df)[sapply(df, is.numeric)]
  indep_choices <- setdiff(num_cols, input$multi_path_dep)
  # Remove current selections if not valid or <2
  valid_sel <- intersect(isolate(input$multi_path_indep), indep_choices)
  if (length(valid_sel) < 2) valid_sel <- NULL
  updateSelectInput(session, "multi_path_indep",
                    choices = indep_choices,
                    selected = valid_sel)
})



  # -------- Block V2: Dynamic Multivariate Main Panel UI (Updated for Path Analysis) --------
output$multi_mainpanel <- renderUI({
  req(multi_subtype_selected())
  if (multi_subtype_selected() == "gge") {
    tabsetPanel(
      tabPanel("Which Won Where", plotOutput("multi_gge_plot_type1")),
      tabPanel("Mean vs Stability", plotOutput("multi_gge_plot_type2")),
      tabPanel("Representativeness vs Discriminativeness", plotOutput("multi_gge_plot_type3"))
    )
  } else if (multi_subtype_selected() == "pca") {
    tabsetPanel(
      tabPanel("Individual Biplot", plotOutput("multi_pca_biplot")),
      tabPanel("Scree Plot", plotOutput("multi_pca_scree")),
      tabPanel("Variable Contributions", plotOutput("multi_pca_varcontrib")),
      tabPanel("Summary Table", tableOutput("multi_pca_eigen"))
    )
  } else if (multi_subtype_selected() == "correlation") {
    tabsetPanel(
      tabPanel("Correlation Plot", plotOutput("multi_corr_corrplot")),
      tabPanel("Pairs Plot", plotOutput("multi_corr_pairsplot"))
    )
  } else if (multi_subtype_selected() == "path") {
    tabsetPanel(
      tabPanel("Path Diagram", plotOutput("multi_path_diagram")),
      tabPanel("Path Coefficients Table", tableOutput("multi_path_coef")),
      tabPanel("Model Fit Summary", verbatimTextOutput("multi_path_fit"))
    )
  }
})
# -------- Block V3: Standalone Multivariate GGE Biplot with Debugging -----------
observeEvent(input$multi_run_gge, {
  req(multi_file_data(), input$multi_gge_genotype, input$multi_gge_location, input$multi_gge_trait)
  
  df <- multi_file_data()

  message("\n\n--- Debugging GGE Run ---")
  message("Columns available: ", paste(names(df), collapse = ", "))
  message("Genotype selected: ", input$multi_gge_genotype)
  message("Location selected: ", input$multi_gge_location)
  message("Trait selected: ", input$multi_gge_trait)

  if (!(input$multi_gge_genotype %in% names(df))) {
    showModal(modalDialog(title = "Error", paste("Genotype column", input$multi_gge_genotype, "not found in data."), easyClose = TRUE))
    return()
  }
  if (!(input$multi_gge_location %in% names(df))) {
    showModal(modalDialog(title = "Error", paste("Location column", input$multi_gge_location, "not found in data."), easyClose = TRUE))
    return()
  }
  if (!(input$multi_gge_trait %in% names(df))) {
    showModal(modalDialog(title = "Error", paste("Trait column", input$multi_gge_trait, "not found in data."), easyClose = TRUE))
    return()
  }

  df_gge <- df %>%
    dplyr::select(gen = !!sym(input$multi_gge_genotype),
                  env = !!sym(input$multi_gge_location),
                  resp = !!sym(input$multi_gge_trait)) %>%
    dplyr::filter(!is.na(resp))

  message("Data subset for GGE has ", nrow(df_gge), " rows.")

  if (nrow(df_gge) == 0) {
    showModal(modalDialog(title = "Error", "No non-missing data for selected columns.", easyClose = TRUE))
    return()
  }

  tryCatch({
    gge_model <- metan::gge(df_gge, env = env, gen = gen, resp = resp, scaling = "sd")
    gge_results(gge_model)

    output$multi_gge_plot_type1 <- renderPlot({ plot(gge_model, type = 3) })
    output$multi_gge_plot_type2 <- renderPlot({ plot(gge_model, type = 2) })
    output$multi_gge_plot_type3 <- renderPlot({ plot(gge_model, type = 4) })

    output$multi_gge_status <- renderUI({
      span(style = "color: green; font-weight: bold;", icon("check"), " GGE Biplot Completed ✅")
    })

    message("GGE Biplot generated successfully!")
  }, error = function(e) {
    showModal(modalDialog(title = "Standalone GGE Error", paste("Error in GGE analysis:", e$message), easyClose = TRUE))
    message("Error encountered during GGE: ", e$message)
  })
})


  # -------- Block V4: Standalone PCA -----------
  observeEvent(input$multi_run_pca, {
    req(multi_file_data(), input$multi_pca_genotype, input$multi_pca_traits)
    df <- multi_file_data()

    if (!(input$multi_pca_genotype %in% names(df))) {
      showModal(modalDialog(title = "Error", paste("Genotype column", input$multi_pca_genotype, "not found in data."), easyClose = TRUE))
      return()
    }
    missing_traits <- setdiff(input$multi_pca_traits, names(df))
    if (length(missing_traits) > 0) {
      showModal(modalDialog(title = "Error", paste("Traits not found in data:", paste(missing_traits, collapse = ", ")), easyClose = TRUE))
      return()
    }

    df_pca <- df %>%
      dplyr::select(Genotype = !!sym(input$multi_pca_genotype), all_of(input$multi_pca_traits)) %>%
      dplyr::filter(!is.na(Genotype))

    rownames(df_pca) <- df_pca$Genotype
    df_pca <- df_pca[, input$multi_pca_traits, drop = FALSE]

    tryCatch({
      res.pca <- FactoMineR::PCA(df_pca, graph = FALSE)
      pca_results(list(pca = res.pca))
      output$multi_pca_biplot <- renderPlot({
        fviz_pca_ind(res.pca, col.ind = "cos2",
                     gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), repel = TRUE)
      })
      output$multi_pca_scree <- renderPlot({ fviz_screeplot(res.pca, addlabels = TRUE) })
      output$multi_pca_varcontrib <- renderPlot({
        fviz_pca_var(res.pca, col.var = "contrib", gradient.cols = c("white", "blue", "red"))
      })
      output$multi_pca_eigen <- renderTable({
        eigen_tbl <- factoextra::get_eigenvalue(res.pca)
        eigen_tbl <- round(eigen_tbl, 2)
        eigen_tbl
      }, rownames = TRUE)
      output$multi_pca_status <- renderUI({
        span(style = "color: green; font-weight: bold;", icon("check"), " PCA Completed ✅")
      })
    }, error = function(e) {
      showModal(modalDialog(
        title = "Standalone PCA Error",
        paste("Error in PCA:", e$message),
        easyClose = TRUE
      ))
    })
  })

 # -------- Block V5: Standalone Correlation with Debugging -----------
observeEvent(input$multi_run_corr, {
  req(multi_file_data(), input$multi_corr_genotype, input$multi_corr_traits)

  df <- multi_file_data()

  message("\n\n--- Debugging Correlation Run ---")
  message("Columns available: ", paste(names(df), collapse = ", "))
  message("Selected genotype column: ", input$multi_corr_genotype)
  message("Selected traits: ", paste(input$multi_corr_traits, collapse = ", "))

  if (!(input$multi_corr_genotype %in% names(df))) {
    showModal(modalDialog(title = "Error", paste("Genotype column", input$multi_corr_genotype, "not found in data."), easyClose = TRUE))
    return()
  }
  missing_traits <- setdiff(input$multi_corr_traits, names(df))
  if (length(missing_traits) > 0) {
    showModal(modalDialog(title = "Error", paste("Traits not found:", paste(missing_traits, collapse = ", ")), easyClose = TRUE))
    return()
  }

  df_corr <- df %>%
    dplyr::select(all_of(input$multi_corr_traits)) %>%
    dplyr::filter(complete.cases(.))

  message("Data subset for Correlation has ", nrow(df_corr), " rows.")

  tryCatch({
    output$multi_corr_corrplot <- renderPlot({
      corrplot(cor(df_corr), method = "number", type = "upper", order = "hclust",
               col = RColorBrewer::brewer.pal(8, "RdYlBu"))
    })
    output$multi_corr_pairsplot <- renderPlot({
      PerformanceAnalytics::chart.Correlation(df_corr, histogram = FALSE)
    })

    output$multi_corr_status <- renderUI({
      span(style = "color: green; font-weight: bold;", icon("check"), " Correlation Completed ✅")
    })

    message("Correlation plots generated successfully.")
  }, error = function(e) {
    showModal(modalDialog(title = "Correlation Error", paste("Error:", e$message), easyClose = TRUE))
    message("Error encountered during correlation: ", e$message)
  })
})
# -------- Block V6: Standalone Path Analysis (Debug + Default Colors) --------
observeEvent(input$multi_run_path, {
  message("===== [PathAnalysis] Triggered =====")
  req(multi_file_data(), input$multi_path_dep, input$multi_path_indep)
  
  df <- multi_file_data()
  genotype_col <- if (!is.null(input$multi_path_genotype) && input$multi_path_genotype != "None") input$multi_path_genotype else NULL
  dep <- input$multi_path_dep
  indep <- input$multi_path_indep
  message("[PathAnalysis] Dependent: ", dep)
  message("[PathAnalysis] Independents: ", paste(indep, collapse = ", "))
  message("[PathAnalysis] Genotype col: ", genotype_col)
  
  # Validate selection
  if (is.null(dep) || length(indep) < 2) {
    message("[PathAnalysis] Selection error: Not enough traits selected.")
    showModal(modalDialog(
      title = "Selection Error",
      "Please select exactly one Dependent Trait and at least two Independent Traits.",
      easyClose = TRUE
    ))
    return()
  }
  
  # Subset and clean data
  keep_vars <- unique(c(dep, indep, genotype_col))
  keep_vars <- keep_vars[!is.null(keep_vars) & keep_vars != ""]
  message("[PathAnalysis] Keeping variables: ", paste(keep_vars, collapse = ", "))
  df_path <- df[, keep_vars, drop = FALSE]
  df_path <- df_path[complete.cases(df_path), , drop = FALSE]
  message("[PathAnalysis] Cleaned data rows: ", nrow(df_path))
  message("[PathAnalysis] Cleaned data cols: ", paste(colnames(df_path), collapse = ", "))
  
  # Prepare lavaan model
  rhs <- paste(indep, collapse = " + ")
  model_str <- paste0(dep, " ~ ", rhs)
  message("[PathAnalysis] Model syntax: ", model_str)
  
  library(lavaan)
  fit <- tryCatch(
    lavaan::sem(model_str, data = df_path, meanstructure = TRUE),
    error = function(e) e
  )
  
  if (inherits(fit, "error")) {
    message("[PathAnalysis] Lavaan model error: ", fit$message)
    showModal(modalDialog(
      title = "Path Analysis Error",
      paste("Error in path analysis:", fit$message),
      easyClose = TRUE
    ))
    return()
  }
  
  # Node check for semPlot
  param_tab <- lavaan::parameterEstimates(fit)
  node_labels <- unique(c(param_tab$lhs, param_tab$rhs))
  node_labels <- node_labels[node_labels != ""]
  message("[PathAnalysis] Node labels in model: ", paste(node_labels, collapse = ", "))
  message("[PathAnalysis] Number of nodes: ", length(node_labels))
  
  # Path diagram: default colors
  output$multi_path_diagram <- renderPlot({
    message("[PathAnalysis] Entering semPaths plot...")
    tryCatch({
      semPlot::semPaths(
        fit,
        whatLabels = "std",
        layout = "tree",
        edge.label.cex = 1.1,
        sizeMan = 6,
        sizeLat = 6,
        mar = c(4,4,4,4),
        nCharNodes = 0 # no label abbreviation
        # No node.color: use package's default
      )
      message("[PathAnalysis] semPaths plot completed successfully.")
    }, error = function(e) {
      message("[PathAnalysis] semPaths error: ", e$message)
      plot.new()
      text(0.5, 0.5, paste("semPaths error:", e$message), col = "red")
    })
  })
  
  # Path Coefficients Table
  output$multi_path_coef <- renderTable({
    coef_tab <- lavaan::parameterEstimates(fit, standardized = TRUE)
    subset(coef_tab, op == "~")[, c("lhs", "op", "rhs", "est", "std.all", "pvalue")]
  }, rownames = FALSE)
  
  # Model Fit Summary
  output$multi_path_fit <- renderPrint({
    summary(fit, fit.measures = TRUE, standardized = TRUE)
  })
  
  # Status UI
  output$multi_path_status <- renderUI({
    span(style = "color: green; font-weight: bold;", icon("check"), " Path Analysis Completed ✅")
  })
  
  # Optionally: Save fit object for download ZIP
  path_results(fit)
  
  message("===== [PathAnalysis] Block complete =====")
})





# -------- Block E1: Handling File Upload and Mode Selection on Home Ab_09-03-2025-----------
observeEvent(input$go_to_analysis, {
  req(input$file)

  df <- readr::read_csv(input$file$datapath, na = c("", "NA"), show_col_types = FALSE)

  if (input$analysis_mode == "eda") {
    raw_data(df)
    updateTabsetPanel(session, "main_tabs", selected = "Analysis 1")
  } else if (input$analysis_mode == "multivariate") {
    multi_file_data(df)
    multi_subtype_selected(input$multi_subtype)
    updateTabsetPanel(session, "main_tabs", selected = "Multivariate Analysis")
  } else if (input$analysis_mode == "mating") {
    md_file_data(df)  # <--- FIXED: use the new name, not mating_file_data!
    updateTabsetPanel(session, "main_tabs", selected = "Mating Design Analysis")
  }
})



# -------- Block E2: EDA Dynamic Sidebar UI (Uniform Mapping for All Except CRD) --------
output$dynamic_sidebar <- renderUI({
  df <- raw_data()
  if (is.null(df) || is.null(input$design)) return(NULL)

  nms <- names(df)
  numeric_cols <- nms[sapply(df, is.numeric)]
  design <- tolower(gsub("\\s+", "", input$design))

  step_header <- function(step, txt) h4(div(style = "margin-bottom:6px;margin-top:10px;color:#3a5a40;", paste0("Step ", step, ": ", txt)))

  # --------- CRD: untouched logic ---------
  if (design == "crd") {
    wellPanel(
      step_header(1, "Specify Number of Factors"),
      numericInput("crd_n_factors", NULL, value = 2, min = 1, max = 3, step = 1),
      uiOutput("crd_factor_ui"),
      step_header(2, "Select Traits"),
      checkboxGroupInput("crd_traits", NULL, choices = numeric_cols),
      step_header(3, "Run Analysis"),
      actionButton("crd_descriptive", "Run Descriptive Summary", class = "btn btn-primary mb-2"),
      actionButton("run_crd_anova", "Run ANOVA + Post Hoc", class = "btn btn-primary mb-2"),
      actionButton("run_crd_interact", "Plot Interactions", class = "btn btn-info mb-2"),
      step_header(4, "Download Results"),
      downloadButton("download_crd_zip", "Download CRD Results (ZIP)", class = "btn btn-success")
    )

  # --------- Uniform mapping for all other designs ---------
  } else if (design %in% c("augmentedrcbd", "rcbd", "alphalattice")) {
    sidebar_fields <- list(
      step_header(1, "Select Trial Type"),
      selectInput("trial_type", NULL, choices = c("Single Environment", "Multi Environment"), selected = "Single Environment"),
      step_header(2, "Map Data Columns"),
      selectInput("entry",  "Genotype Column", choices = nms),
      if (design == "alphalattice")
        selectInput("rep", "Replication Column", choices = nms),
      selectInput("block",  "Block Column", choices = nms),
      conditionalPanel(
        condition = "input.trial_type == 'Multi Environment'",
        selectInput("env", "Environment Column", choices = nms)
      ),
      if (design == "augmentedrcbd")
        list(
          selectInput("check_col", "Check/Entry Type Column", choices = nms),
          helpText("Select the column indicating replicated checks (e.g., value = 'check') and unreplicated test entries (e.g., value = 'entry').")
        ),
      step_header(3, "Select Traits and Options"),
      checkboxGroupInput("traits", NULL, choices = numeric_cols, selected = numeric_cols[1:min(3, length(numeric_cols))]),
      selectInput("palette", "Boxplot Color Palette", choices = rownames(RColorBrewer::brewer.pal.info)),
      selectInput("genotype_model", "Genotype Model", choices = c("Fixed", "Random"), selected = "Fixed"),
      checkboxInput("env_wise", "Compute Environment-wise BLUE/BLUP (for multi-environment)?", value = TRUE),
      step_header(4, "Run Analysis"),
      actionButton("run_descriptive", "Run Descriptive Summary", class = "btn btn-primary mb-2"),
      uiOutput("descriptive_status"),
      actionButton("run_model", "Run Model-Based Analysis", class = "btn btn-primary mb-2"),
      uiOutput("model_status"),
      step_header(5, "Download Results"),
      downloadButton("download_analysis1", paste("Download", tools::toTitleCase(design), "Results ZIP"), class = "btn btn-success")
    )
    wellPanel(Filter(Negate(is.null), sidebar_fields))

  # --------- Fallback for unknown/custom designs ---------
  } else {
    wellPanel(
      h4("Unknown or Custom Design (showing all options)"),
      selectInput("entry", "Genotype Column", choices = nms),
      selectInput("env", "Environment Column", choices = nms),
      selectInput("rep", "Replication Column", choices = nms),
      selectInput("block", "Block Column", choices = nms),
      checkboxGroupInput("traits", "Select Traits", choices = numeric_cols),
      selectInput("palette", "Boxplot Color Palette", choices = rownames(RColorBrewer::brewer.pal.info)),
      selectInput("genotype_model", "Genotype Model", choices = c("Fixed", "Random"), selected = "Fixed"),
      checkboxInput("env_wise", "Compute Environment-wise BLUE/BLUP (for multi-environment)?", value = TRUE),
      actionButton("run_descriptive", "Run Descriptive Summary", class = "btn btn-primary mb-2"),
      uiOutput("descriptive_status"),
      actionButton("run_model", "Run Model-Based Analysis", class = "btn btn-primary mb-2"),
      uiOutput("model_status"),
      downloadButton("download_analysis1", "Download Analysis 1 ZIP", class = "btn btn-success")
    )
  }
})
# -------- End of Block E2 --------


  # -------- Block C1: CRD Factors UI -----------
  output$crd_factor_ui <- renderUI({
    req(raw_data(), input$crd_n_factors)
    df <- raw_data()
    nfac <- as.numeric(input$crd_n_factors)
    nms <- names(df)
    lapply(1:nfac, function(i) {
      selectInput(paste0("crd_factor", i), paste("Factor", i), choices = nms)
    })
  })

 # -------- Block C2: Dynamic Main Panel UI (CRD with Results, All Tabs) -----------

output$dynamic_mainpanel <- renderUI({
  design <- input$design
  if (is.null(design)) return(NULL)

  if (toupper(design) == "CRD") {
    req(crd_results())
    res <- crd_results()

    if (is.null(res) || length(res) == 0) {
      return(
        div(
          h4("No CRD results yet."),
          style = "color: #b02a37; padding: 2em 1em 1em 1em; background: #fff3cd; border-radius: 8px;"
        )
      )
    }

    tab_list <- lapply(names(res), function(trait) {
      tabPanel(
        title = trait,
        tabsetPanel(
          tabPanel("Summary Table", DT::dataTableOutput(paste0("crd_sum_", trait))),
          tabPanel("ANOVA Table", DT::dataTableOutput(paste0("crd_anova_", trait))),
          tabPanel("DMRT Results", verbatimTextOutput(paste0("crd_dmrt_", trait))),
          tabPanel("Missing Combinations", DT::dataTableOutput(paste0("crd_missing_", trait))),
          tabPanel("Interaction Plots", uiOutput(paste0("crd_interaction_panel_", trait)))
        )
      )
    })

    # CRITICAL: Use do.call, not tabsetPanel(..., tab_list)
    return(
      do.call(tabsetPanel, c(list(id = "crd_trait_tabs"), tab_list))
    )

  } else {
    tabsetPanel(
      id = "result_tabs",
      tabPanel("Descriptive Results", uiOutput("descriptive_ui")),
      tabPanel("Model Results", uiOutput("model_ui"))
    )
  }
})


  # -------- Block C3: Trial/Factor Mapping UI -----------
  output$trial_or_factor_type <- renderUI({
    if (input$analysis_mode == "eda") {
      if (input$design == "CRD") {
        numericInput("crd_n_factors", "Number of Factors", value = 2, min = 1, max = 3, step = 1)
      } else {
        selectInput("trial_type", "Trial Type", choices = c("Single Location", "Multi Location"), selected = "Single Location")
      }
    }
  })

# -------- Block C4: CRD Descriptive Analysis (Ab_07-06-2025) -----------

observeEvent(input$crd_descriptive, {
  req(raw_data(), input$crd_traits, input$crd_n_factors)
  df <- raw_data()
  nfac <- as.numeric(input$crd_n_factors)
  factor_names <- paste0("crd_factor", 1:nfac)
  factor_cols <- sapply(factor_names, function(x) input[[x]])

  # Validate columns
  if (any(is.null(factor_cols) | factor_cols == "")) {
    showNotification("Please select all factors.", type = "error")
    return(NULL)
  }
  if (any(!factor_cols %in% names(df))) {
    showNotification("Selected factors do not match data columns.", type = "error")
    return(NULL)
  }
  for (fac in factor_cols) df[[fac]] <- as.factor(df[[fac]])

  # -- THE FIX: Don't overwrite any analysis results! --
  current_results <- crd_results()
  if (is.null(current_results)) current_results <- list()

  for (trait in input$crd_traits) {
    if (!is.numeric(df[[trait]])) next
    summary_tbl <- df %>%
      group_by(across(all_of(factor_cols))) %>%
      summarise(
        mean = round(mean(.data[[trait]], na.rm = TRUE), 2),
        SE = round(sd(.data[[trait]], na.rm = TRUE)/sqrt(n()), 2),
        N = n(),
        .groups = "drop"
      )

    # Only update summary, preserve ALL other analysis results if present
    if (is.null(current_results[[trait]])) current_results[[trait]] <- list()
    old_trait <- current_results[[trait]]
    current_results[[trait]] <- modifyList(
      old_trait,
      list(summary = summary_tbl)
    )
  }

  # Save back to reactive value
  crd_results(current_results)
  showNotification("Descriptive summary complete.", type = "message")

  # Register UI outputs for summary, report, and interaction plots
  for (trait in input$crd_traits) {
    local({
      trait_local <- trait
      output[[paste0("crd_sum_", trait_local)]] <- DT::renderDataTable({
        tbl <- crd_results()[[trait_local]]$summary
        if (!is.null(tbl)) as.data.frame(tbl) else data.frame()
      })
      output[[paste0("crd_report_", trait_local)]] <- renderPrint({
        rep <- crd_results()[[trait_local]]$report
        if (!is.null(rep) && nzchar(rep)) cat(rep)
        else cat("Run ANOVA + Post Hoc to view report.")
      })
      # Render interaction plot outputs ONLY if they exist
      output[[paste0("crd_interaction_panel_", trait_local)]] <- renderUI({
        plots <- crd_results()[[trait_local]]$interaction_plots
        if (!is.null(plots) && length(plots) > 0) {
          plot_outputs <- lapply(names(plots), function(nm) {
            plotOutput(paste0("crd_interact_", nm, "_", trait_local))
          })
          do.call(tagList, plot_outputs)
        } else {
          h4("Run Plot Interactions to view plots.")
        }
      })
      # The actual plot renderers (this just registers, C6 sets the content!)
      plots <- crd_results()[[trait_local]]$interaction_plots
      if (!is.null(plots) && length(plots) > 0) {
        for (nm in names(plots)) {
          local({
            nm_local <- nm
            output[[paste0("crd_interact_", nm_local, "_", trait_local)]] <- renderPlot({
              p <- crd_results()[[trait_local]]$interaction_plots[[nm_local]]
              if (!is.null(p)) print(p)
            })
          })
        }
      }
    })
  }
})
# -------- End Block C4 -----------

# --- Block C5: Robust Factorial ANOVA + Diagnostics + Post Hoc (DMRT) WITH DataTable OUTPUT (CLEAN) ---
observeEvent(input$run_crd_anova, {
  req(raw_data(), input$crd_traits, input$crd_n_factors)
  df <- raw_data()
  nfac <- as.numeric(input$crd_n_factors)
  factor_names <- paste0("crd_factor", 1:nfac)
  factor_cols <- na.omit(unname(sapply(factor_names, function(x) input[[x]])))
  factor_cols <- factor_cols[!(is.na(factor_cols) | factor_cols == "" | factor_cols == " ")]

  # Defensive checks
  if (any(duplicated(factor_cols))) {
    showNotification("Please select unique factors for each Factor slot!", type = "error")
    return(NULL)
  }
  if (length(factor_cols) == 0) {
    showNotification("Please select at least one valid factor.", type = "error")
    return(NULL)
  }
  if (any(!factor_cols %in% names(df))) {
    showNotification("Selected factors do not match data columns.", type = "error")
    return(NULL)
  }
  for (fac in factor_cols) df[[fac]] <- as.factor(df[[fac]])

  single_level_factors <- factor_cols[sapply(factor_cols, function(fac) length(unique(df[[fac]])) < 2)]
  if (length(single_level_factors) > 0) {
    msg <- paste("Some selected factors have only one level and will be ignored in ANOVA:", paste(single_level_factors, collapse = ", "))
    showNotification(msg, type = "error")
    return(NULL)
  }

  traits <- input$crd_traits
  trait_results <- list()

  withProgress(message = 'Running Factorial ANOVA + Post Hoc (DMRT)...', value = 0, {
    for (trait in traits) {
      trait_local <- trait
      incProgress(1/length(traits), detail = paste("Trait:", trait_local))

      anova_str <- ""
      dmrt_str <- ""
      summary_tbl <- data.frame()
      anova_df <- data.frame()
      missing <- data.frame()

      if (!is.numeric(df[[trait_local]])) {
        anova_str <- paste("Trait", trait_local, "is not numeric. Cannot run ANOVA.")
        dmrt_str <- ""
      } else {
        # Build ANOVA
        factors_bt <- sapply(factor_cols, function(x) if (grepl("[^A-Za-z0-9_.]", x)) paste0("`", x, "`") else x)
        formula_str <- if (length(factors_bt) == 1) {
          paste(trait_local, "~", factors_bt[1])
        } else {
          paste(trait_local, "~", paste(factors_bt, collapse = " * "))
        }
        anova_formula <- as.formula(formula_str)
        fit <- tryCatch(aov(anova_formula, data = df), error = function(e) NULL)

        if (!is.null(fit)) {
          # ANOVA Table
          anova_tbl <- summary(fit)[[1]]
          anova_df <- as.data.frame(anova_tbl)
          anova_df$Effect <- rownames(anova_tbl)
          anova_df <- anova_df[, c("Effect", setdiff(names(anova_df), "Effect"))]
          numcols <- sapply(anova_df, is.numeric)
          anova_df[, numcols] <- lapply(anova_df[, numcols, drop=FALSE], round, 4)
          # Diagnostics
          anova_str <- "Shapiro-Wilk Test for Normality of Residuals:\n"
          res <- tryCatch(residuals(fit), error = function(e) NULL)
          if (!is.null(res) && length(res) > 3) {
            shapiro <- tryCatch(shapiro.test(res), error = function(e) NULL)
            if (!is.null(shapiro)) {
              anova_str <- paste0(anova_str, paste(capture.output(shapiro), collapse = "\n"), "\n")
            }
          }
          # DMRT
          if (requireNamespace("agricolae", quietly = TRUE)) {
            for (ff in factors_bt) {
              dmrt <- tryCatch(agricolae::duncan.test(fit, ff, group=TRUE), error = function(e) NULL)
              if (!is.null(dmrt)) {
                dmrt_str <- paste0(dmrt_str, "\nDMRT for ", ff, ":\n")
                dmrt_str <- paste0(dmrt_str, paste(capture.output(dmrt$groups), collapse = "\n"), "\n")
              }
            }
            if (length(factors_bt) > 1) {
              all_interactions <- unlist(lapply(2:length(factors_bt), function(m) combn(factors_bt, m, simplify = FALSE)), recursive = FALSE)
              for (inter in all_interactions) {
                term <- paste(inter, collapse=":")
                dmrt <- tryCatch(agricolae::duncan.test(fit, term, group=TRUE), error = function(e) NULL)
                if (!is.null(dmrt)) {
                  dmrt_str <- paste0(dmrt_str, "\nDMRT for interaction ", term, ":\n")
                  dmrt_str <- paste0(dmrt_str, paste(capture.output(dmrt$groups), collapse = "\n"), "\n")
                }
              }
            }
          } else {
            dmrt_str <- "\nagricolae package not available. DMRT not run.\n"
          }
        } else {
          anova_str <- "ANOVA failed: not enough data or structural problem."
          dmrt_str <- ""
          anova_df <- data.frame()
        }
      }

      # Save results
      summary_tbl <- df %>%
        group_by(across(all_of(factor_cols))) %>%
        summarise(
          mean = round(mean(.data[[trait_local]], na.rm = TRUE), 2),
          SE   = round(sd(.data[[trait_local]], na.rm = TRUE)/sqrt(n()), 2),
          N    = n(),
          .groups = "drop"
        )
      full_grid <- expand.grid(lapply(df[factor_cols], function(x) levels(x)))
      names(full_grid) <- factor_cols
      actual <- df[, factor_cols, drop = FALSE]
      for (col in factor_cols) actual[[col]] <- as.character(actual[[col]])
      for (col in factor_cols) full_grid[[col]] <- as.character(full_grid[[col]])
      missing <- dplyr::anti_join(full_grid, actual, by = factor_cols)

      trait_results[[trait_local]] <- list(
        anova_table = anova_df,
        dmrt_text   = dmrt_str,
        report      = paste(anova_str, "\n", dmrt_str),
        summary     = summary_tbl,
        missing_combinations = missing
      )

      # UI outputs
      output[[paste0("crd_anova_", trait_local)]] <- DT::renderDataTable({
        tbl <- trait_results[[trait_local]]$anova_table
        if (!is.null(tbl) && nrow(tbl) > 0) {
          DT::datatable(tbl, options = list(dom = 't', pageLength = nrow(tbl)), rownames = FALSE)
        } else {
          data.frame(Message = "No ANOVA results.")
        }
      })
      output[[paste0("crd_dmrt_", trait_local)]] <- renderPrint({
        txt <- trait_results[[trait_local]]$dmrt_text
        if (!is.null(txt) && nzchar(txt)) cat(txt)
        else cat("Run ANOVA + Post Hoc to view report.")
      })
      output[[paste0("crd_missing_", trait_local)]] <- DT::renderDataTable({
        miss <- trait_results[[trait_local]]$missing_combinations
        if (!is.null(miss) && nrow(miss) > 0) {
          DT::datatable(miss, options = list(dom = 't', pageLength = min(nrow(miss), 10)), rownames = FALSE)
        } else {
          data.frame(Message = "No missing combinations: data is complete for these factors.")
        }
      })
    }
    crd_results(trait_results)
  })
  showNotification("Factorial ANOVA with diagnostics and post hoc (DMRT) complete.", type = "message")
})
# --- End Block C5 ---




# -------- Block C6: CRD Interaction Plots (Ab_22-06-2025) -----------
observeEvent(input$run_crd_interact, {
  req(raw_data(), input$crd_traits, input$crd_n_factors)
  df <- raw_data()
  nfac <- as.numeric(input$crd_n_factors)
  factor_names <- paste0("crd_factor", 1:nfac)
  factor_cols <- sapply(factor_names, function(x) input[[x]])
  
  # Validate
  if (any(is.null(factor_cols) | factor_cols == "")) {
    showNotification("Please select all factors.", type = "error")
    return(NULL)
  }
  if (any(!factor_cols %in% names(df))) {
    showNotification("Selected factors do not match data columns.", type = "error")
    return(NULL)
  }
  for (fac in factor_cols) df[[fac]] <- as.factor(df[[fac]])
  
  traits <- input$crd_traits
  for (trait in traits) {
    trait_local <- trait
    plot_list <- list()
    
    # UI registration (does not build plots here, just makes UI)
    output[[paste0("crd_interaction_panel_", trait_local)]] <- renderUI({
      ui_list <- list()
      if (nfac == 3) {
        ui_list <- append(ui_list, list(
          tags$h4("Three-way Interaction"),
          plotOutput(paste0("crd_interact_3way_", trait_local)),
          tags$h4("Two-way Interactions"),
          plotOutput(paste0("crd_interact_2way_", trait_local, "_1")),
          plotOutput(paste0("crd_interact_2way_", trait_local, "_2")),
          plotOutput(paste0("crd_interact_2way_", trait_local, "_3"))
        ))
      } else if (nfac == 2) {
        ui_list <- append(ui_list, list(
          tags$h4("Two-way Interaction"),
          plotOutput(paste0("crd_interact_2way_", trait_local, "_1"))
        ))
      } else {
        ui_list <- append(ui_list, list(
          plotOutput(paste0("crd_interact_single_", trait_local))
        ))
      }
      do.call(tagList, ui_list)
    })
    
    # ------- Plot building and storing -------
    if (nfac == 3) {
      fac1 <- factor_cols[1]
      fac2 <- factor_cols[2]
      fac3 <- factor_cols[3]
      
      # 3-way interaction plot
      df_summary_3way <- df %>%
        group_by(.data[[fac1]], .data[[fac2]], .data[[fac3]]) %>%
        summarise(mean_value = mean(.data[[trait_local]], na.rm=TRUE), .groups='drop')
      p3way <- ggplot(df_summary_3way, aes(
        x = .data[[fac1]], y = mean_value,
        color = .data[[fac3]], group = .data[[fac3]]
      )) +
        geom_line() + geom_point() +
        facet_wrap(as.formula(paste("~", fac2))) +
        labs(
          title = paste("3-way Interaction:", trait_local, "~", fac1, "×", fac3, ", by", fac2),
          x = fac1, y = paste("Mean", trait_local), color = fac3
        ) +
        theme_bw() +
        theme(axis.text.x = element_text(angle=45, hjust=1))
      plot_list[["3way"]] <- p3way
      output[[paste0("crd_interact_3way_", trait_local)]] <- renderPlot({ print(p3way) })
      
      # All two-way pairs
      all_pairs <- list(
        c(fac1, fac2),
        c(fac1, fac3),
        c(fac2, fac3)
      )
      for (i in seq_along(all_pairs)) {
        xfac <- all_pairs[[i]][1]
        linefac <- all_pairs[[i]][2]
        df_summary_2way <- df %>%
          group_by(.data[[xfac]], .data[[linefac]]) %>%
          summarise(mean_value = mean(.data[[trait_local]], na.rm=TRUE), .groups='drop')
        p2way <- ggplot(df_summary_2way, aes(
          x = .data[[xfac]], y = mean_value,
          color = .data[[linefac]], group = .data[[linefac]]
        )) +
          geom_line() + geom_point() +
          labs(
            title = paste("2-way Interaction:", trait_local, "~", xfac, "×", linefac),
            x = xfac, y = paste("Mean", trait_local), color = linefac
          ) +
          theme_bw() +
          theme(axis.text.x = element_text(angle=45, hjust=1))
        plot_list[[paste0("2way_", i)]] <- p2way
        output[[paste0("crd_interact_2way_", trait_local, "_", i)]] <- renderPlot({ print(p2way) })
      }
    } else if (nfac == 2) {
      xfac <- factor_cols[1]
      linefac <- factor_cols[2]
      df_summary_2way <- df %>%
        group_by(.data[[xfac]], .data[[linefac]]) %>%
        summarise(mean_value = mean(.data[[trait_local]], na.rm=TRUE), .groups='drop')
      p2way <- ggplot(df_summary_2way, aes(
        x = .data[[xfac]], y = mean_value,
        color = .data[[linefac]], group = .data[[linefac]]
      )) +
        geom_line() + geom_point() +
        labs(
          title = paste("2-way Interaction:", trait_local, "~", xfac, "×", linefac),
          x = xfac, y = paste("Mean", trait_local), color = linefac
        ) +
        theme_bw() +
        theme(axis.text.x = element_text(angle=45, hjust=1))
      plot_list[["2way_1"]] <- p2way
      output[[paste0("crd_interact_2way_", trait_local, "_1")]] <- renderPlot({ print(p2way) })
    } else if (nfac == 1) {
      fac <- factor_cols[1]
      pbox <- ggplot(df, aes(x = .data[[fac]], y = .data[[trait_local]], fill = .data[[fac]])) +
        geom_boxplot() +
        labs(
          title = paste("Boxplot:", trait_local, "by", fac),
          x = fac, y = trait_local
        ) +
        theme_bw() +
        theme(axis.text.x = element_text(angle=45, hjust=1), legend.position = "none")
      plot_list[["boxplot"]] <- pbox
      output[[paste0("crd_interact_single_", trait_local)]] <- renderPlot({ print(pbox) })
    }
    
    # Store plot_list in crd_results immediately
    all_results <- crd_results()
    if (is.null(all_results[[trait_local]])) all_results[[trait_local]] <- list()
    all_results[[trait_local]]$interaction_plots <- plot_list
    crd_results(all_results)
  }
  showNotification("Interaction plots generated for all selected traits.", type = "message")
})
# ---- End Block C6 ----

# -------- Block E6: EDA Descriptive Analysis (Single or Multi-Environment) --------
observeEvent(input$run_descriptive, {
  req(raw_data())
  req(input$traits)
  
  make_safe <- function(x) gsub("[^A-Za-z0-9]", "_", x)
  df <- raw_data()
  trial_type <- if (!is.null(input$trial_type)) input$trial_type else "Single Environment"
  
  # Use the environment column ONLY if it's multi-environment and mapped
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
        N = n(),
        Missing = sum(is.na(df[[trait]])),
        Missing_Percent = round(100 * Missing / nrow(df), 2),
        .groups = "drop"
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
    span(style = "color: green; font-weight: bold;", icon("check"), " Descriptive Analysis Completed ✅")
  })
  shinyjs::enable("run_model")
  shinyjs::enable("download_analysis1")
  
  output$descriptive_ui <- renderUI({
    tabs <- purrr::map(names(results_list), function(trait) {
      safe_trait <- make_safe(trait)
      tabPanel(
        title = trait,
        h4("Summary Statistics"), tableOutput(paste0("sum_", safe_trait)),
        h4("Boxplot"), plotOutput(paste0("box_", safe_trait)),
        h4("QQ Plot(s)"),
        fluidRow(
          purrr::map(seq_along(results_list[[trait]]$qq), function(i) {
            column(4, plotOutput(paste0("qq_", safe_trait, "_", i)))
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
# -------- End Block E6 --------



# ===================================================================
# -------- Block E8: Final Version with Simplified Display (Ab_07-08-2025) --------
# ===================================================================


# --- A. EQUATION REPOSITORY (Simplified Plain Text Version) ---
# This list now ONLY contains the R formula.
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

# --- B. ANOVA FUNCTION --- (Unchanged from your version)
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

# --- C. MAIN ANALYSIS ENGINE FUNCTION --- (Unchanged from your version)
#' Run Mixed Model (with final user-defined interpretation)
run_model_and_extract <- function(df, trait, vc_formula_str, random_formula_str, 
                                  model_type, entry_col, env_col, block_col, rep_col, design) {
  results <- list()
  
  round_df <- function(d) { #... (unchanged)
    if(is.data.frame(d) && ncol(d) > 0) {
      return(d %>% dplyr::mutate(across(where(is.numeric), ~ round(., 3))))
    }
    return(d)
  }
  
  process_var_comps <- function(fit_model) { #... (unchanged)
    var_corr_df <- as.data.frame(lme4::VarCorr(fit_model))
    simplified_df <- var_corr_df %>%
      dplyr::select(Source = grp, Variance = vcov) %>%
      dplyr::mutate(Source = ifelse(is.na(Source), "Residual", Source)) %>%
      dplyr::filter(!is.na(Variance)) %>%
      dplyr::mutate(Percentage = scales::percent(Variance / sum(Variance), accuracy = 0.1)) %>%
      dplyr::select(Source, Variance, Percentage)
    return(round_df(simplified_df))
  }
  
  process_lrt_table <- function(fit_model) { #... (unchanged)
    lrt_df <- tibble::rownames_to_column(as.data.frame(lmerTest::rand(fit_model)), "Source")
    lrt_df_modified <- lrt_df %>%
      dplyr::mutate(Source = ifelse(Source == "<none>", "Baseline Model (for comparison)", Source))
    return(round_df(lrt_df_modified))
  }
  
  # <<< THIS HELPER FUNCTION HAS BEEN COMPLETELY REWRITTEN TO MATCH YOUR TEMPLATE >>>
  generate_lrt_interpretation <- function(lrt_table, entry_col_name, env_col_name) {
    significant_effects <- lrt_table %>%
      dplyr::filter(`Pr(>Chisq)` < 0.05, Source != "Baseline Model (for comparison)") %>%
      dplyr::pull(Source)
    
    if (length(significant_effects) == 0) {
      return(HTML("The analysis suggests that none of the random effects significantly contribute to the model's fit (at the p < 0.05 level)."))
    }
    
    # Define a map for descriptive names
    term_map <- list(
      "Replication" = "Replication",
      "Block" = "Block",
      "Rep" = "Replication" # Common abbreviation
    )
    term_map[[entry_col_name]] <- "Genotype"
    term_map[[env_col_name]] <- "Location (Environment)"
    term_map[[paste0(entry_col_name, ":", env_col_name)]] <- "Genotype × Environment interaction"
    term_map[[paste0("Rep", ":", env_col_name)]] <- "Replication within Environment"
    term_map[[paste0("Block", ":", "Rep")]] <- "Block within Replication"
    term_map[[paste0("Block", ":", env_col_name)]] <- "Block within Environment"
    term_map[[paste0("Block", ":Rep:", env_col_name)]] <- "Block within Replication & Environment"
    
    # Clean technical names and map to descriptive names
    descriptive_names <- sapply(significant_effects, function(effect) {
      clean_effect <- gsub("[()1| ]", "", effect)
      mapped_name <- term_map[[clean_effect]]
      return(ifelse(is.null(mapped_name), clean_effect, paste0(mapped_name, " (", clean_effect, ")")))
    })
    
    # Format the list into natural language (e.g., A, B, and C)
    format_effect_list <- function(effects) {
      n <- length(effects)
      if (n == 0) return("")
      if (n == 1) return(effects[1])
      if (n == 2) return(paste(effects, collapse = " and "))
      return(paste(paste(effects[1:(n-1)], collapse = ", "), effects[n], sep = ", and "))
    }
    
    effects_list_str <- format_effect_list(descriptive_names)
    
    # Build the final interpretation sentence using your template
    interpretation <- paste0(
      "The analysis indicates that the variance from the following random effect(s) is statistically significant (p < 0.05): ",
      tags$b(effects_list_str),
      ". This suggests that these sources of variation play a major role in trait expression."
    )
    
    return(HTML(interpretation))
  }
  
  generate_anova_interpretation <- function(anova_table) { #... (unchanged)
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
    lrt_table_obj <- tryCatch({ if(results$is_singular) NULL else process_lrt_table(fit_vc) }, error = function(e) NULL)
    if(!is.null(lrt_table_obj)) {
      results$lrt_table <- lrt_table_obj
      results$lrt_interpretation <- generate_lrt_interpretation(lrt_table_obj, entry_col, env_col)
    } else {
      results$lrt_table <- data.frame(Message="LRT not computed."); results$lrt_interpretation <- ""
    }
    results$blue_table <- tryCatch({
      #... (BLUEs logic unchanged) ...
      emm_comb <- emmeans::emmeans(fit_vc, specs = entry_col); summary_comb <- as.data.frame(summary(emm_comb)) %>% dplyr::select(all_of(entry_col), BLUE_Combined = "emmean", SE_Combined = "SE"); final_blue_table <- summary_comb
      if (!is.null(env_col)) {
        env_levels <- unique(as.character(df[[env_col]])); env_blues_list <- lapply(env_levels, function(env) {
          df_sub <- df[df[[env_col]] == env, ]; bt_trait <- paste0("`", trait, "`"); bt_entry_col <- paste0("`", entry_col, "`"); bt_rep_col <- paste0("`", rep_col, "`"); bt_block_col <- paste0("`", block_col, "`")
          ss_formula_str <- if(design == "alphalattice") { paste(bt_trait, "~", bt_entry_col, "+ (1|", bt_rep_col, ") + (1|", bt_block_col, ":", bt_rep_col, ")") } else { paste(bt_trait, "~", bt_entry_col, "+ (1|", bt_block_col, ")") }
          fit_ss <- tryCatch(lmerTest::lmer(as.formula(ss_formula_str), data=df_sub), error=function(e) NULL)
          if(is.null(fit_ss)) return(NULL)
          emm_ss <- emmeans::emmeans(fit_ss, specs = entry_col); as.data.frame(summary(emm_ss)) %>% dplyr::select(all_of(entry_col), !!paste0("BLUE_", env) := emmean)
        }); env_blues_wide <- purrr::reduce(Filter(Negate(is.null), env_blues_list), dplyr::full_join, by = entry_col); final_blue_table <- dplyr::left_join(summary_comb, env_blues_wide, by = entry_col)
      }
      round_df(final_blue_table)
    }, error = function(e) data.frame(Message="BLUEs could not be calculated."))
    
  } else { # Random Model
    fit_rand <- lmerTest::lmer(as.formula(random_formula_str), data = df)
    results$is_singular <- lme4::isSingular(fit_rand)
    results$singularity_message <- if(results$is_singular) "Warning: Model fit is singular." else ""
    lrt_table_obj <- tryCatch({ if (results$is_singular) NULL else process_lrt_table(fit_rand) }, error = function(e) NULL)
    if(!is.null(lrt_table_obj)) {
      results$lrt_table <- lrt_table_obj
      results$lrt_interpretation <- generate_lrt_interpretation(lrt_table_obj, entry_col, env_col)
    } else {
      results$lrt_table <- data.frame(Message="LRT not computed."); results$lrt_interpretation <- ""
    }
    results$var_comps <- tryCatch({ process_var_comps(fit_rand) }, error = function(e) data.frame(Message = "VarComps error"))
    results$blup_table <- tryCatch({
      #... (BLUPs logic unchanged) ...
      intercept <- lme4::fixef(fit_rand)["(Intercept)"]; blups_comb <- lme4::ranef(fit_rand)[[entry_col]]; blups_comb$BLUP_Combined <- intercept + blups_comb[,1]
      blups_comb <- tibble::rownames_to_column(blups_comb, var=entry_col) %>% dplyr::select(all_of(entry_col), "BLUP_Combined"); final_blup_table <- blups_comb
      if (!is.null(env_col)) {
        gxe_term <- paste0(entry_col, ":", env_col); blups_ind <- lme4::ranef(fit_rand)[[gxe_term]]; blups_ind$BLUP <- intercept + blups_ind[,1]
        blups_ind <- tibble::rownames_to_column(blups_ind, var="Var") %>% tidyr::separate("Var", into=c(entry_col, env_col), sep=":"); wide_blups <- blups_ind %>% dplyr::select(all_of(entry_col), all_of(env_col), "BLUP") %>% tidyr::pivot_wider(names_from=all_of(env_col), values_from="BLUP", names_prefix="BLUP_"); final_blup_table <- dplyr::left_join(blups_comb, wide_blups, by = entry_col)
      }
      round_df(final_blup_table)
    }, error = function(e) data.frame(Message = "BLUPs could not be calculated."))
  }
  return(results)
}

# --- D. MAIN OBSERVER ---
observeEvent(input$run_model, {
  req(raw_data(), input$design, !tolower(gsub("\\s+", "", input$design)) == "crd", input$traits, input$entry)
  withProgress(message = 'Running Model Analysis...', value = 0, {
    df <- raw_data(); design <- tolower(gsub("\\s+", "", input$design)); trial_type <- input$trial_type
    # <<< MODIFIED: Simplified the check as "Both" is no longer an option >>>
    genotype_model <- input$genotype_model
    traits <- input$traits; entry_col <- make.names(input$entry); block_col <- make.names(input$block); rep_col <- if (!is.null(input$rep)) make.names(input$rep) else NULL; env_col <- if (trial_type == "Multi Environment" && !is.null(input$env)) make.names(input$env) else NULL
    names(df) <- make.names(names(df)); cols_to_factor <- c(entry_col, block_col, rep_col, env_col); for (col in cols_to_factor) { if (!is.null(col) && col %in% names(df)) df[[col]] <- as.factor(df[[col]]) }
    all_results <- list(); bt <- function(x) paste0("`", x, "`")
    for (i in seq_along(traits)) {
      original_trait_name <- traits[i]; trait <- make.names(original_trait_name)
      incProgress(1 / length(traits), detail = paste("Processing trait:", original_trait_name))
      if (!is.numeric(df[[trait]])) { showNotification(paste("Skipping non-numeric trait:", original_trait_name), type = "warning"); next }
      trait_results <- list(); fixed_formula_vc_str <- NULL; formula_random_str <- NULL
      if (design %in% c("rcbd", "augmentedrcbd")) { if (is.null(env_col)) { fixed_formula_vc_str <- paste(bt(trait), "~", bt(entry_col), "+ (1|", bt(block_col), ")"); formula_random_str <- paste(bt(trait), "~ (1|", bt(entry_col), ") + (1|", bt(block_col), ")") } else { fixed_formula_vc_str <- paste(bt(trait), "~", bt(entry_col), "+ (1|", bt(env_col), ") + (1|", bt(entry_col), ":", bt(env_col), ") + (1|", bt(block_col), ":", bt(env_col), ")"); formula_random_str <- paste(bt(trait), "~ (1|", bt(entry_col), ") + (1|", bt(env_col), ") + (1|", bt(entry_col), ":", bt(env_col), ") + (1|", bt(block_col), ":", bt(env_col), ")") } } else if (design == "alphalattice") { req(rep_col); if (is.null(env_col)) { fixed_formula_vc_str <- paste(bt(trait), "~", bt(entry_col), "+ (1|", bt(rep_col), ") + (1|", bt(block_col), ":", bt(rep_col), ")"); formula_random_str <- paste(bt(trait), "~ (1|", bt(entry_col), ") + (1|", bt(rep_col), ") + (1|", bt(block_col), ":", bt(rep_col), ")") } else { fixed_formula_vc_str <- paste(bt(trait), "~", bt(entry_col), "+ (1|", bt(env_col), ") + (1|", bt(entry_col), ":", bt(env_col), ") + (1|", bt(rep_col), ":", bt(env_col), ") + (1|", bt(block_col), ":", bt(rep_col), ":", bt(env_col), ")"); formula_random_str <- paste(bt(trait), "~ (1|", bt(entry_col), ") + (1|", bt(env_col), ") + (1|", bt(entry_col), ":", bt(env_col), ") + (1|", bt(rep_col), ":", bt(env_col), ") + (1|", bt(block_col), ":", bt(rep_col), ":", bt(env_col), ")") } }
      
      # <<< MODIFIED: Simplified logic for Fixed/Random >>>
      if (genotype_model == "Fixed") {
        key <- get_equation_key(design, trial_type, "Fixed")
        anova_res <- extract_custom_anova(df, trait, entry_col, block_col, rep_col, env_col, design)
        other_res <- run_model_and_extract(df, trait, fixed_formula_vc_str, NULL, "Fixed", entry_col, env_col, block_col, rep_col, design)
        trait_results$Fixed <- c(list(anova_table = anova_res, equation_latex = model_equations[[key]] %||% model_equations$default), other_res)
      }
      if (genotype_model == "Random") {
        key <- get_equation_key(design, trial_type, "Random")
        res_rand <- run_model_and_extract(df, trait, NULL, formula_random_str, "Random", entry_col, env_col, block_col, rep_col, design)
        trait_results$Random <- c(list(equation_latex = model_equations[[key]] %||% model_equations$default), res_rand)
      }
      all_results[[original_trait_name]] <- trait_results
    }
    model_results(all_results)
  })
  showNotification(paste(toupper(input$design), "model analysis complete."), type = "message")
  updateTabsetPanel(session, "result_tabs", selected = "Model Results")
})

# --- E. UNIFIED UI RENDERER (with Simplified Explanation) ---
output$model_ui <- renderUI({
  req(model_results())
  results <- model_results(); if (length(results) == 0) return(h4("Run Analysis.", style="color:grey;"))
  
  create_explanation_ui <- function(id_prefix, content) {
    link_id <- paste0("toggle_", id_prefix); div_id <- paste0("div_", id_prefix)
    tagList(actionLink(link_id, "Show/Hide Explanation", style = "font-size: 12px;"), shinyjs::hidden(div(id = div_id, class = "alert alert-info", style = "margin-top: 10px;", content)))
  }
  
  # <<< MODIFIED: This is the new simplified explanation function >>>
  model_explanation_content <- function(model_type, design, trial_type) {
    design <- tolower(gsub("\\s+", "", design))
    fixed_effects <- character(0) # Start with an empty list
    random_effects <- c("Residual (error)")
    
    if(model_type == "Fixed") {
      fixed_effects <- c(fixed_effects, "Genotype")
    } else { # Random
      random_effects <- c(random_effects, "Genotype")
    }
    
    if(design == "alphalattice") {
      random_effects <- c(random_effects, "Replication")
      if(trial_type != "Multi Environment") random_effects <- c(random_effects, "Block within Replication")
    } else { # RCBD or Augmented RCBD
      random_effects <- c(random_effects, "Block")
    }
    
    if(trial_type == "Multi Environment") {
      random_effects <- c(random_effects, "Environment", "Genotype × Environment Interaction")
      if(design == "alphalattice") {
        random_effects <- c(random_effects, "Replication within Environment", "Block within Replication & Environment")
      } else { # RCBD or Augmented RCBD
        random_effects <- c(random_effects, "Block within Environment")
      }
    }
    
    tagList(
      if (length(fixed_effects) > 0) {
        list(tags$b("Fixed Effects:"), tags$ul(lapply(sort(unique(fixed_effects)), tags$li)))
      },
      tags$b("Random Effects:"),
      tags$ul(lapply(sort(unique(random_effects)), tags$li))
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
                                        withMathJax(),
                                        h5("Model Equation", style="font-weight:bold;"), uiOutput(paste0("equation_", tid_prefix)),
                                        
                                        # <<< MODIFIED: This line is updated to call the new function correctly >>>
                                        create_explanation_ui(paste0("model_exp_", tid_prefix), model_explanation_content("Fixed", input$design, input$trial_type)), 
                                        
                                        hr(),
                                        uiOutput(paste0("singularity_warning_", tid_prefix)),
                                        h4("1. ANOVA Table (Genotype Only)"), DT::dataTableOutput(paste0("anova_", tid_prefix)),
                                        uiOutput(paste0("anova_interp_", tid_prefix)),
                                        h4("2. Likelihood Ratio Test (LRT)"), create_explanation_ui(paste0("lrt_exp_", tid_prefix), lrt_explanation_content), DT::dataTableOutput(paste0("lrt_", tid_prefix)),
                                        uiOutput(paste0("lrt_interp_", tid_prefix)), 
                                        h4("3. Variance Components"), DT::dataTableOutput(paste0("varcomp_", tid_prefix)),
                                        h4("4. Best Linear Unbiased Estimates (BLUEs)"), DT::dataTableOutput(paste0("blues_", tid_prefix))
      )
    }
    if (!is.null(trait_content$Random)) {
      tid_prefix <- make.names(paste0(trait_name, "_random"))
      model_type_tabs$Random <- tabPanel("Random Model (Genotype)",
                                         withMathJax(),
                                         h5("Model Equation", style="font-weight:bold;"), uiOutput(paste0("equation_", tid_prefix)),
                                         
                                         # <<< MODIFIED: This line is updated to call the new function correctly >>>
                                         create_explanation_ui(paste0("model_exp_", tid_prefix), model_explanation_content("Random", input$design, input$trial_type)), 
                                         
                                         hr(),
                                         uiOutput(paste0("singularity_warning_", tid_prefix)),
                                         h4("1. Likelihood Ratio Test (LRT)"), create_explanation_ui(paste0("lrt_exp_", tid_prefix), lrt_explanation_content), DT::dataTableOutput(paste0("lrt_", tid_prefix)),
                                         uiOutput(paste0("lrt_interp_", tid_prefix)),
                                         h4("2. Variance Components"), DT::dataTableOutput(paste0("varcomp_", tid_prefix)),
                                         h4("3. Best Linear Unbiased Predictors (BLUPs)"), DT::dataTableOutput(paste0("blups_", tid_prefix))
      )
    }
    tabPanel(title = trait_name, do.call(tabsetPanel, unname(model_type_tabs)))
  })
  
  purrr::iwalk(results, ~{
    trait_name <- .y; trait_content <- .x
    if (!is.null(trait_content$Fixed)) {
      tid_prefix <- make.names(paste0(trait_name, "_fixed"))
      local({
        observeEvent(input[[paste0("toggle_model_exp_", tid_prefix)]], { shinyjs::toggle(id = paste0("div_model_exp_", tid_prefix), anim = TRUE) })
        observeEvent(input[[paste0("toggle_lrt_exp_", tid_prefix)]], { shinyjs::toggle(id = paste0("div_lrt_exp_", tid_prefix), anim = TRUE) })
        
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
        observeEvent(input[[paste0("toggle_model_exp_", tid_prefix)]], { shinyjs::toggle(id = paste0("div_model_exp_", tid_prefix), anim = TRUE) })
        observeEvent(input[[paste0("toggle_lrt_exp_", tid_prefix)]], { shinyjs::toggle(id = paste0("div_lrt_exp_", tid_prefix), anim = TRUE) })
        
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
# -------- CORRECTED BLOCKS FOR ANALYSIS 2 (E9, E12, E13, E14) (Ab_28-04-2025)--------
# ===================================================================

# -------- Block E9: Update Analysis 2 Trait Selectors (Corrected) --------
observe({
  mod_res <- model_results()
  if (is.null(mod_res) || length(mod_res) == 0) return(NULL)
  
  # Filter for traits that have a valid Fixed model result with a blue_table
  valid_fixed_results <- purrr::keep(mod_res, ~!is.null(.x$Fixed$blue_table) && is.data.frame(.x$Fixed$blue_table))
  
  if(length(valid_fixed_results) == 0) return(NULL)

  # Traits with Combined BLUEs (for PCA/correlation) have the 'BLUE_Combined' column
  traits_with_blues <- names(purrr::keep(valid_fixed_results, ~"BLUE_Combined" %in% names(.x$Fixed$blue_table)))
  
  # Traits with location-wise BLUEs have more than 3 columns (Geno, BLUE_Combined, SE_Combined, plus location columns)
  traits_with_locwise_blues <- names(purrr::keep(valid_fixed_results, ~ncol(.x$Fixed$blue_table) > 3))

  # Update GGE trait selector
  updateSelectInput(
    session, "gge_trait",
    choices = traits_with_locwise_blues,
    selected = if (length(traits_with_locwise_blues) > 0) traits_with_locwise_blues[[1]] else NULL
  )
  
  # Update PCA/Correlation trait selector
  updateCheckboxGroupInput(
    session, "multi_traits",
    choices = traits_with_blues,
    selected = traits_with_blues
  )
})


# -------- Block E12: Analysis 2 Tab - GGE Biplot (Corrected) -----------
observeEvent(input$run_gge, {
  req(input$gge_trait, model_results())
  
  # Correctly access the nested blue_table from the Fixed results
  trait_data <- model_results()[[input$gge_trait]]$Fixed
  
  if (is.null(trait_data) || is.null(trait_data$blue_table) || !is.data.frame(trait_data$blue_table)) {
    showModal(modalDialog(
      title = "Data Not Found",
      "Please run a Fixed or Both Model-Based Analysis for a multi-environment trial first.",
      easyClose = TRUE
    ))
    return()
  }
  df_blue <- trait_data$blue_table
  
  # Prepare data for GGE by pivoting the wide BLUEs table to a long format
  # Use the cleaned entry_col name from the model run
  entry_col_name <- make.names(input$entry)
  
  df_gge <- df_blue %>%
    dplyr::select(gen = all_of(entry_col_name), starts_with("BLUE_")) %>%
    dplyr::select(-contains("BLUE_Combined"), -contains("SE_Combined")) %>%
    tidyr::pivot_longer(
      cols = -gen,
      names_to = "env",
      values_to = "resp",
      names_prefix = "BLUE_"
    )

  tryCatch({
    gge_model <- metan::gge(df_gge, env = env, gen = gen, resp = resp, scaling = "sd")
    gge_results(gge_model)
    output$gge_plot_type1 <- renderPlot({ plot(gge_model, type = 3) })
    output$gge_plot_type2 <- renderPlot({ plot(gge_model, type = 2) })
    output$gge_plot_type3 <- renderPlot({ plot(gge_model, type = 4) })
    output$gge_status <- renderUI({ span(style = "color: green; font-weight: bold;", icon("check"), " GGE Biplot Completed ✅") })
    updateTabsetPanel(session, "analysis2_tabs", selected = " GGE Biplot")
  }, error = function(e) {
    showModal(modalDialog(title = "GGE Error", paste("Error in GGE analysis:", e$message), easyClose = TRUE))
  })
})

# -------- Block E13: Analysis 2 Tab - PCA (Corrected) -----------
observeEvent(input$run_pca, {
  req(model_results(), input$multi_traits)
  
  entry_col_name <- make.names(input$entry)
  
  # Correctly map over selected traits and extract the BLUE_Combined column
  trait_df_list <- purrr::map(input$multi_traits, function(trait) {
    res <- model_results()[[trait]]$Fixed
    if (!is.null(res) && !is.null(res$blue_table) && "BLUE_Combined" %in% names(res$blue_table)) {
      res$blue_table %>% dplyr::select(all_of(entry_col_name), !!sym(trait) := BLUE_Combined)
    } else {
      NULL
    }
  }) %>% purrr::discard(is.null)
  
  if (length(trait_df_list) < 2) {
    showModal(modalDialog(title = "Insufficient Data", "PCA requires at least two traits with valid Combined BLUEs.", easyClose = TRUE))
    return()
  }
  
  # Join the single-trait data frames into one wide data frame for PCA
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
    output$pca_status <- renderUI({ span(style = "color: green; font-weight: bold;", icon("check"), " PCA Completed ✅") })
    updateTabsetPanel(session, "analysis2_tabs", selected = " PCA Plot")
  }, error = function(e) {
    showModal(modalDialog(title = "PCA Error", paste("Error in PCA analysis:", e$message), easyClose = TRUE))
  })
})

# -------- Block E14: Analysis 2 Tab - Correlation (Corrected) -----------
observeEvent(input$run_corr, {
  req(pca_results())
  
  # Get the data from the PCA results to ensure consistency
  df_corr <- tryCatch({
    pca_results()$pca$call$X
  }, error = function(e) NULL)
  
  if (is.null(df_corr)) {
    showModal(modalDialog(title = "Correlation Error", "PCA results missing. Please run PCA first.", easyClose = TRUE))
    return()
  }
  
  tryCatch({
    output$corr_plot1 <- renderPlot({
      corrplot(cor(df_corr, use = "complete.obs"), method = "number", type = "upper", order = "hclust",
               col = RColorBrewer::brewer.pal(8, "RdYlBu"))
    })
    output$corr_plot2 <- renderPlot({
      PerformanceAnalytics::chart.Correlation(df_corr, histogram = TRUE, pch = 19)
    })
    output$corr_status <- renderUI({ span(style = "color: green; font-weight: bold;", icon("check"), " Correlation Completed ✅") })
    updateTabsetPanel(session, "analysis2_tabs", selected = " Correlation Plot")
  }, error = function(e) {
    showModal(modalDialog(title = "Correlation Error", paste("Error in correlation plotting:", e$message), easyClose = TRUE))
  })
})


# -------- Block D1: Download Handler - CRD Results ZIP (DEBUG/ROBUST VERSION) --------
output$download_crd_zip <- downloadHandler(
  filename = function() paste0("CRD_Results_", Sys.Date(), ".zip"),
  content = function(file) {
    # Use a unique temp directory for this session
    tmp_dir <- tempfile("crdzip_")
    dir.create(tmp_dir)
    files <- c()
    res <- crd_results()
    sanitize <- function(x) gsub("[^A-Za-z0-9_]", "_", x)
    
    for (trait in names(res)) {
      tname <- sanitize(trait)
      trait_res <- res[[trait]]
      
      cat("\n=== Trait:", tname, "===\n")
      # ---- Summary ----
      if (!is.null(trait_res$summary) && nrow(trait_res$summary) > 0) {
        sumfile <- file.path(tmp_dir, paste0(tname, "_summary.csv"))
        write.csv(trait_res$summary, sumfile, row.names = FALSE)
        files <- c(files, sumfile)
      }

      # ---- ANOVA Table ----
      if (!is.null(trait_res$anova_table) && nrow(trait_res$anova_table) > 0) {
        cat("ANOVA table preview:\n")
        print(head(trait_res$anova_table))
        anovafile <- file.path(tmp_dir, paste0(tname, "_anova.csv"))
        write.csv(trait_res$anova_table, anovafile, row.names = FALSE)
        files <- c(files, anovafile)
      } else {
        cat("No ANOVA table for", tname, "!\n")
      }

      # ---- DMRT Results ----
      if (!is.null(trait_res$dmrt_text) && nzchar(trimws(trait_res$dmrt_text))) {
        cat("DMRT text preview:\n")
        cat(substr(trait_res$dmrt_text, 1, 100), "...\n")
        dmrtfile <- file.path(tmp_dir, paste0(tname, "_dmrt_results.txt"))
        writeLines(trait_res$dmrt_text, dmrtfile)
        files <- c(files, dmrtfile)
      } else {
        cat("No DMRT results for", tname, "!\n")
      }

      # ---- Full diagnostics report (optional) ----
      if (!is.null(trait_res$report) && nzchar(trimws(trait_res$report))) {
        repfile <- file.path(tmp_dir, paste0(tname, "_anova_posthoc_report.txt"))
        writeLines(trait_res$report, repfile)
        files <- c(files, repfile)
      }

      # ---- Missing Combinations ----
      if (!is.null(trait_res$missing_combinations) && nrow(trait_res$missing_combinations) > 0) {
        missfile <- file.path(tmp_dir, paste0(tname, "_missing_combinations.csv"))
        write.csv(trait_res$missing_combinations, missfile, row.names = FALSE)
        files <- c(files, missfile)
      }

      # ---- Interaction Plots ----
      if (!is.null(trait_res$interaction_plots) && length(trait_res$interaction_plots) > 0) {
        for (pn in names(trait_res$interaction_plots)) {
          pdfname <- sanitize(pn)
          pdffile <- file.path(tmp_dir, paste0(tname, "_", pdfname, "_interaction.pdf"))
          pdf(pdffile, width = 8, height = 5)
          print(trait_res$interaction_plots[[pn]])
          dev.off()
          files <- c(files, pdffile)
        }
      }

      # ---- (Optional) Always write a test file per trait ----
      testfile <- file.path(tmp_dir, paste0(tname, "_TEST.txt"))
      writeLines("This is a test file for debugging the ZIP output.", testfile)
      files <- c(files, testfile)
    }
    
    # Print all files before zipping
    cat("\nFiles written for zip:\n")
    print(files)
    cat("Existence check:\n")
    print(file.exists(files))
    
    # Zip only these files
    zip::zip(zipfile = file, files = files, mode = "cherry-pick")
  }
)
# ---- End Block D1 ----

# -------- Block D2: Download Handler - Analysis 1 ZIP (Corrected) --------
output$download_analysis1 <- downloadHandler(
  filename = function() {
    # Use the selected design name in the filename for clarity
    design_name <- tools::toTitleCase(input$design)
    paste0("PbAT_", design_name, "_Results_", Sys.Date(), ".zip")
  },
  content = function(file) {
    # Setup a temporary directory to store files
    tmp_dir <- tempdir()
    files_to_zip <- c()
    
    # Get the results from the reactive values
    descriptive_list <- descriptive_results()
    model_list <- model_results()

    # Loop through each analyzed trait
    for (trait in names(model_list)) {
      
      # --- Save Descriptive Results (if they exist) ---
      if (!is.null(descriptive_list[[trait]])) {
        desc_res <- descriptive_list[[trait]]
        
        # Save summary CSV
        summary_file <- file.path(tmp_dir, paste0(trait, "_descriptive_summary.csv"))
        write.csv(desc_res$summary, summary_file, row.names = FALSE)
        files_to_zip <- c(files_to_zip, summary_file)
        
        # Save boxplot PDF
        box_file <- file.path(tmp_dir, paste0(trait, "_boxplot.pdf"))
        pdf(box_file, width = 11, height = 8.5)
        print(desc_res$boxplot)
        dev.off()
        files_to_zip <- c(files_to_zip, box_file)
        
        # Save QQ plots PDF
        qq_file <- file.path(tmp_dir, paste0(trait, "_qqplots.pdf"))
        pdf(qq_file, width = 11, height = 8.5)
        # Arrange multiple QQ plots on one page if there are many
        gridExtra::grid.arrange(grobs = desc_res$qq, ncol = 3)
        dev.off()
        files_to_zip <- c(files_to_zip, qq_file)
      }
      
      # --- Save Model Results (New Corrected Logic) ---
      trait_model_res <- model_list[[trait]]
      
      # 1. Save results from the FIXED model analysis (if it was run)
      if (!is.null(trait_model_res$Fixed)) {
        fixed_res <- trait_model_res$Fixed
        
        if (!is.null(fixed_res$anova_table) && is.data.frame(fixed_res$anova_table)) {
          file_path <- file.path(tmp_dir, paste0(trait, "_Fixed_ANOVA.csv"))
          write.csv(fixed_res$anova_table, file_path, row.names = FALSE)
          files_to_zip <- c(files_to_zip, file_path)
        }
        if (!is.null(fixed_res$blue_table) && is.data.frame(fixed_res$blue_table)) {
          file_path <- file.path(tmp_dir, paste0(trait, "_Fixed_BLUEs.csv"))
          write.csv(fixed_res$blue_table, file_path, row.names = FALSE)
          files_to_zip <- c(files_to_zip, file_path)
        }
        if (!is.null(fixed_res$lrt_table) && is.data.frame(fixed_res$lrt_table)) {
          file_path <- file.path(tmp_dir, paste0(trait, "_Fixed_LRT.csv"))
          write.csv(fixed_res$lrt_table, file_path, row.names = FALSE)
          files_to_zip <- c(files_to_zip, file_path)
        }
        if (!is.null(fixed_res$var_comps) && is.data.frame(fixed_res$var_comps)) {
          file_path <- file.path(tmp_dir, paste0(trait, "_Fixed_VarComps.csv"))
          write.csv(fixed_res$var_comps, file_path, row.names = FALSE)
          files_to_zip <- c(files_to_zip, file_path)
        }
      }
      
      # 2. Save results from the RANDOM model analysis (if it was run)
      if (!is.null(trait_model_res$Random)) {
        rand_res <- trait_model_res$Random
        
        if (!is.null(rand_res$blup_table) && is.data.frame(rand_res$blup_table)) {
          file_path <- file.path(tmp_dir, paste0(trait, "_Random_BLUPs.csv"))
          write.csv(rand_res$blup_table, file_path, row.names = FALSE)
          files_to_zip <- c(files_to_zip, file_path)
        }
        if (!is.null(rand_res$lrt_table) && is.data.frame(rand_res$lrt_table)) {
          file_path <- file.path(tmp_dir, paste0(trait, "_Random_LRT.csv"))
          write.csv(rand_res$lrt_table, file_path, row.names = FALSE)
          files_to_zip <- c(files_to_zip, file_path)
        }
        if (!is.null(rand_res$var_comps) && is.data.frame(rand_res$var_comps)) {
          file_path <- file.path(tmp_dir, paste0(trait, "_Random_VarComps.csv"))
          write.csv(rand_res$var_comps, file_path, row.names = FALSE)
          files_to_zip <- c(files_to_zip, file_path)
        }
      }
    }
    
    # Create the zip file
    zip::zip(zipfile = file, files = files_to_zip, mode = "cherry-pick")
  }
)
# -------- Block D3: Download Handler - Analysis 2 ZIP (Linked from EDA) --------
output$download_analysis2 <- downloadHandler(
  filename = function() {
    paste0("PbAT_Analysis2_", Sys.Date(), ".zip")
  },
  content = function(file) {
    tmp_dir <- tempdir()
    files <- c()

    gge_model <- tryCatch(gge_results(), error = function(e) NULL)
    if (!is.null(gge_model)) {
      for (i in c(2, 3, 4)) {
        gge_file <- file.path(tmp_dir, paste0("GGE_Type", i, ".pdf"))
        pdf(gge_file, width = 7, height = 6)
        print(plot(gge_model, type = i))
        dev.off()
        files <- c(files, gge_file)
      }
    }

    pca_obj <- tryCatch(pca_results()$pca, error = function(e) NULL)
    if (!is.null(pca_obj)) {
      pdf(file.path(tmp_dir, "PCA_Biplot.pdf"))
      print(fviz_pca_ind(pca_obj, col.ind = "cos2",
                        gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), repel = TRUE))
      dev.off()

      pdf(file.path(tmp_dir, "PCA_Scree.pdf"))
      print(fviz_screeplot(pca_obj, addlabels = TRUE))
      dev.off()

      pdf(file.path(tmp_dir, "PCA_Contributions.pdf"))
      print(fviz_pca_var(pca_obj, col.var = "contrib",
                        gradient.cols = c("white", "blue", "red")))
      dev.off()

      files <- c(
        files,
        file.path(tmp_dir, "PCA_Biplot.pdf"),
        file.path(tmp_dir, "PCA_Scree.pdf"),
        file.path(tmp_dir, "PCA_Contributions.pdf")
      )

      coords_file <- file.path(tmp_dir, "PCA_Coordinates.csv")
      write.csv(pca_obj$ind$coord, coords_file, row.names = TRUE)
      files <- c(files, coords_file)

      eigen_file <- file.path(tmp_dir, "PCA_Eigenvalues.csv")
      write.csv(factoextra::get_eigenvalue(pca_obj), eigen_file, row.names = TRUE)
      files <- c(files, eigen_file)
    }

    corr_data <- tryCatch(pca_obj$call$X, error = function(e) NULL)
    if (!is.null(corr_data)) {
      pdf(file.path(tmp_dir, "Correlation_Upper.pdf"))
      corrplot(cor(corr_data), method = "number", type = "upper", order = "hclust",
               col = RColorBrewer::brewer.pal(8, "RdYlBu"))
      dev.off()

      pdf(file.path(tmp_dir, "Correlation_Pairs.pdf"))
      PerformanceAnalytics::chart.Correlation(corr_data, histogram = FALSE)
      dev.off()

      corr_file <- file.path(tmp_dir, "Correlation_Matrix.csv")
      write.csv(cor(corr_data), corr_file)
      files <- c(
        files,
        file.path(tmp_dir, "Correlation_Upper.pdf"),
        file.path(tmp_dir, "Correlation_Pairs.pdf"),
        corr_file
      )
    }

    zip::zip(zipfile = file, files = files, mode = "cherry-pick")
  }
)

# -------- Block D4: Download Handler - Standalone Multivariate ZIP --------
output$multi_gge_download <- output$multi_pca_download <- output$multi_corr_download <- output$multi_path_download <-
  downloadHandler(
    filename = function() {
      paste0("Standalone_Multivariate_", Sys.Date(), ".zip")
    },
    content = function(file) {
      tmp_dir <- tempdir()
      files <- c()

      # GGE
      gge_model <- tryCatch(gge_results(), error = function(e) NULL)
      if (!is.null(gge_model)) {
        for (i in c(2, 3, 4)) {
          gge_file <- file.path(tmp_dir, paste0("MV_GGE_Type", i, ".pdf"))
          pdf(gge_file, width = 7, height = 6)
          print(plot(gge_model, type = i))
          dev.off()
          files <- c(files, gge_file)
        }
      }

      # PCA
      pca_obj <- tryCatch(pca_results()$pca, error = function(e) NULL)
      if (!is.null(pca_obj)) {
        pdf(file.path(tmp_dir, "MV_PCA_Biplot.pdf"))
        print(fviz_pca_ind(pca_obj, col.ind = "cos2",
                          gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), repel = TRUE))
        dev.off()

        pdf(file.path(tmp_dir, "MV_PCA_Scree.pdf"))
        print(fviz_screeplot(pca_obj, addlabels = TRUE))
        dev.off()

        pdf(file.path(tmp_dir, "MV_PCA_Contributions.pdf"))
        print(fviz_pca_var(pca_obj, col.var = "contrib",
                          gradient.cols = c("white", "blue", "red")))
        dev.off()

        files <- c(
          files,
          file.path(tmp_dir, "MV_PCA_Biplot.pdf"),
          file.path(tmp_dir, "MV_PCA_Scree.pdf"),
          file.path(tmp_dir, "MV_PCA_Contributions.pdf")
        )

        coords_file <- file.path(tmp_dir, "MV_PCA_Coordinates.csv")
        write.csv(pca_obj$ind$coord, coords_file, row.names = TRUE)
        files <- c(files, coords_file)

        eigen_file <- file.path(tmp_dir, "MV_PCA_Eigenvalues.csv")
        write.csv(factoextra::get_eigenvalue(pca_obj), eigen_file, row.names = TRUE)
        files <- c(files, eigen_file)
      }

      # Correlation
      corr_data <- tryCatch(pca_obj$call$X, error = function(e) NULL)
      if (!is.null(corr_data)) {
        pdf(file.path(tmp_dir, "MV_Correlation_Upper.pdf"))
        corrplot(cor(corr_data), method = "number", type = "upper", order = "hclust",
                 col = RColorBrewer::brewer.pal(8, "RdYlBu"))
        dev.off()

        pdf(file.path(tmp_dir, "MV_Correlation_Pairs.pdf"))
        PerformanceAnalytics::chart.Correlation(corr_data, histogram = FALSE)
        dev.off()

        corr_file <- file.path(tmp_dir, "MV_Correlation_Matrix.csv")
        write.csv(cor(corr_data), corr_file)
        files <- c(
          files,
          file.path(tmp_dir, "MV_Correlation_Upper.pdf"),
          file.path(tmp_dir, "MV_Correlation_Pairs.pdf"),
          corr_file
        )
      }

      # Path Analysis
      path_obj <- tryCatch(path_results(), error = function(e) NULL)
      if (!is.null(path_obj)) {
        # Standardized solution table
        path_table <- tryCatch(as.data.frame(lavaan::standardizedSolution(path_obj)), error = function(e) NULL)
        if (!is.null(path_table)) {
          write.csv(path_table, file.path(tmp_dir, "MV_Path_StandardizedSolution.csv"), row.names = FALSE)
          files <- c(files, file.path(tmp_dir, "MV_Path_StandardizedSolution.csv"))
        }

        # Path diagram (if available)
        if (requireNamespace("semPlot", quietly = TRUE)) {
          pdf(file.path(tmp_dir, "MV_Path_Diagram.pdf"))
          semPlot::semPaths(path_obj, "std", layout = "tree", nCharNodes = 0,
                            edge.label.cex = 1, whatLabels = "std", style = "ram", residuals = FALSE)
          dev.off()
          files <- c(files, file.path(tmp_dir, "MV_Path_Diagram.pdf"))
        }

        # Path model summary
        summary_file <- file.path(tmp_dir, "MV_Path_ModelSummary.txt")
        capture.output(
          print(lavaan::summary(path_obj, standardized = TRUE, fit.measures = TRUE)),
          file = summary_file
        )
        files <- c(files, summary_file)
      }

      zip::zip(zipfile = file, files = files, mode = "cherry-pick")
    }
  )
# -------- Block D5: Download Handler -Mating design-------------

output$md_download_results <- downloadHandler(
  filename = function() {
    paste0("Mating_Design_Results_", Sys.Date(), ".zip")
  },
  content = function(file) {
    tmp_dir <- tempdir()
    files <- c()
    res <- md_results()
    # ANOVA
    if (!is.null(res$anova)) {
      aov_file <- file.path(tmp_dir, "ANOVA_Table.csv")
      write.csv(cbind(Source = rownames(res$anova), res$anova), aov_file, row.names = FALSE)
      files <- c(files, aov_file)
    }
    # GCA Effects
    if (!is.null(res$gca)) {
      gca_file <- file.path(tmp_dir, "GCA_Effects.csv")
      write.csv(res$gca, gca_file, row.names = FALSE)
      files <- c(files, gca_file)
    }
    # SCA Effects
    if (!is.null(res$sca)) {
      sca_file <- file.path(tmp_dir, "SCA_Effects.csv")
      write.csv(res$sca, sca_file, row.names = FALSE)
      files <- c(files, sca_file)
    }
    # Means Matrix
    if (!is.null(res$means_matrix)) {
      mat_file <- file.path(tmp_dir, "Means_Matrix.csv")
      mat_df <- as.data.frame(res$means_matrix)
      write.csv(cbind(Parent = rownames(mat_df), mat_df), mat_file, row.names = FALSE)
      files <- c(files, mat_file)
    }
    # Variance Components
    if (!is.null(res$variance_components)) {
      varcomp_file <- file.path(tmp_dir, "Variance_Components.csv")
      write.csv(res$variance_components, varcomp_file, row.names = FALSE)
      files <- c(files, varcomp_file)
    }
    zip::zip(zipfile = file, files = files, mode = "cherry-pick")
  }
)

} # END SERVER FUNCTION

# Internal: Create PBAT Shiny app
pbat_app <- function() {
  shiny::shinyApp(ui, server)
}