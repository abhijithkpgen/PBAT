# ------------------------- LIBRARIES -------------------------
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

# ------------------------- UI -------------------------
ui <- navbarPage(
  title = "PbAT: Plant Breeding Analytical Tools",
  id = "main_tabs",
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  header = tagList(
    useShinyjs(),
    tags$head(tags$style(HTML("
      .home-container {
        display: flex;
        justify-content: center;
        align-items: flex-start;
        min-height: 85vh;
        gap: 40px;
        margin-top: 30px;
      }
      body {
          background-image: url('background2.jpg');
          background-size: cover;
          background-position: center;
          background-repeat: no-repeat;
          background-attachment: fixed;
        }
.overlay-panel {
        background-color: rgba(255,255,255,0.92);
        padding: 20px;
        border-radius: 10px;
        box-shadow: 0 0 12px rgba(0,0,0,0.3);
        width: 400px;
        font-size: 13px;
      }
      .compact-row {
        display: flex;
        justify-content: space-between;
        gap: 10px;
      }
      .half-width { width: 49%; }
    ")))
  ),

# -------------------- HOME TAB --------------------
tabPanel("Home",
  div(class = "home-container",
    # Left Panel: User Inputs
    div(class = "overlay-panel",
      div(class = "compact-row",
        div(class = "half-width", textInput("user_name", "Name")),
        div(class = "half-width", textInput("user_email", "Email"))
      ),
      textInput("user_institute", "Institute / Organization"),
      fileInput("file", "Upload CSV File", accept = ".csv"),
      selectInput("design", "Experimental Design", 
                  choices = c("Alpha Lattice", "RCBD", "CRD", "Augmented RCBD")),
      radioButtons("trial_type", "Trial Type", 
                   choices = c("Single Location", "Multi Location"), inline = TRUE),
      actionButton("go_to_analysis", "Proceed to Analysis", class = "btn btn-primary"),
      br(),
      tags$div(style = "font-size: 11px; text-align: center;",
        HTML("Developed by <b>Dr. Abhijith K P</b><br>
              Scientist (Genetics and Plant Breeding), ICAR-IARI Assam<br>
              <a href='mailto:abhijithkpgen@gmail.com'>abhijithkpgen@gmail.com</a>")
      )
    ),

    # Right Panel: Workflow Overview
    div(class = "overlay-panel",
      h4(tags$b("Application Workflow Overview")),
      tags$p("PbAT: Plant Breeding Analytical Tools is a complete statistical analysis pipeline for plant breeding experiments. Follow the steps below to perform your analysis:"),
      tags$ol(
        tags$li(tags$b("1. Upload Data:"), 
                " Upload your experimental data in CSV format. Make sure the dataset includes columns for genotype, location, replication, block (if applicable), and trait measurements."),
        tags$li(tags$b("2. Column Mapping:"), 
                " Assign the correct columns for genotype, location, replication, and blocks. This allows the app to run models correctly."),
        tags$li(tags$b("3. Descriptive & Model-Based Analysis:"), 
                " Generate summary statistics, ANOVA tables, heritability (H²), and BLUEs/BLUPs for each selected trait."),
        tags$li(tags$b("4. Multivariate Analysis:"), 
                " Perform PCA (Principal Component Analysis), GGE Biplots (Which-Won-Where, Stability, Discrimination), and Correlation analysis across traits."),
        tags$li(tags$b("5. Download Results:"), 
                " Export all analysis outputs including plots and tables in PDF/CSV format as ZIP files for reporting or publication.")
      ),
      tags$p(style = "font-size: 11px; margin-top: 10px;",
             "This tool was developed for plant breeders and researchers to analyze experimental data quickly and intuitively, without needing a knowledge in R.")
    )
  )
),



  # ------------------------- ANALYSIS 1 -------------------------
  tabPanel("Analysis 1",
    sidebarLayout(
      sidebarPanel(width = 4,
        uiOutput("mapping_ui"),
        uiOutput("trait_selector"),
        radioButtons("genotype_model", "Genotype as:",
                     choices = c("Fixed", "Random", "Both"), inline = TRUE),
        checkboxInput("location_wise", "Generate Location-wise BLUEs/BLUPs", value = TRUE),
        actionButton("run_descriptive", "Run Descriptive Analysis", class = "btn btn-info"),
        uiOutput("descriptive_status"),
        br(),
        actionButton("run_model", "Run Model-Based Analysis", class = "btn btn-warning", disabled = TRUE),
        uiOutput("model_status"),
        br(),
        selectInput("palette", "Boxplot Color Palette", choices = c("Set1", "Set2", "Dark2")),
        downloadButton("download_analysis1", "Download Analysis 1 ZIP", class = "btn btn-success")
      ),
      mainPanel(
        tabsetPanel(
          id = "result_tabs",
          tabPanel(" Descriptive Results", uiOutput("descriptive_ui")),
          tabPanel(" Model Results", uiOutput("model_ui"))
        )
      )
    )
  ),

  # ------------------------- ANALYSIS 2 -------------------------
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
)
,
          tabPanel(" PCA Plot",
  h4(" PCA Individual Biplot"),
  plotOutput("pca_plot_biplot"),
  h4(" Scree Plot"),
  plotOutput("pca_plot_scree"),
  h4(" Variable Contributions"),
  plotOutput("pca_plot_varcontrib"),
  h4(" PCA Summary Table"),
  tableOutput("pca_table_summary")
)
,
          tabPanel(" Correlation Plot", plotOutput("corr_plot1"), plotOutput("corr_plot2"))
        )
      )
    )
  ),

  # ------------------------- HELP -------------------------
 # -------------------- HELP TAB --------------------
  tabPanel("Help & Guide",
    fluidPage(
      div(style = "padding: 30px;",
        h2("📘 Help & Guide"),
        p("This tool supports Alpha Lattice, RCBD, CRD, and Augmented RCBD across single or multi-locations."),
        tags$h4("📂 Sample Format Downloads"),
        tags$ul(
          tags$li(tags$a(href = "Alphalattice_format.csv", "📥 Alpha Lattice CSV", target = "_blank")),
          tags$li(tags$a(href = "RCBD_format.csv", "📥 RCBD CSV", target = "_blank")),
          tags$li(tags$a(href = "Augmented_RCBD_format.csv", "📥 Augmented RCBD CSV", target = "_blank"))
        ),
        tags$h4("🧭 How to Use PbEd"),
        tags$ol(
          tags$li("Enter details & upload data in Home tab."),
          tags$li("Select design, trial type, and map required columns."),
          tags$li("Run Descriptive → Model-Based Analysis."),
          tags$li("Proceed to Analysis 2 for PCA, GGE, and Correlation."),
          tags$li("Download all results as ZIP.")
        )
      )
    )
  )
)
# ------------------------- SERVER -------------------------
server <- function(input, output, session) {
  raw_data <- reactiveVal(NULL)
  descriptive_results <- reactiveVal(NULL)
  model_results <- reactiveVal(NULL)
  gge_results <- reactiveVal(NULL)
  pca_results <- reactiveVal(NULL)

  # -------------------- FILE UPLOAD + COLUMN MAPPING --------------------
  observeEvent(input$go_to_analysis, {
    req(input$file)
    df <- read_csv(input$file$datapath, na = c("", "NA"), show_col_types = FALSE)
    raw_data(df)

    updateTabsetPanel(session, "main_tabs", selected = "Analysis 1")

    output$mapping_ui <- renderUI({
      tagList(
        if (input$trial_type == "Multi Location") selectInput("loc", "Location Column", choices = names(df)),
        if (input$design %in% c("Alpha Lattice", "RCBD")) selectInput("rep", "Replication Column", choices = names(df)),
        if (input$design %in% c("Alpha Lattice", "Augmented RCBD")) selectInput("block", "Block Column", choices = names(df)),
        selectInput("entry", "Genotype (Entry) Column", choices = names(df))
      )
    })

    output$trait_selector <- renderUI({
      num_vars <- names(df)[sapply(df, is.numeric)]
      exclude <- c(input$loc, input$rep, input$block, input$entry) %>% discard(is.null)
      traits <- setdiff(num_vars, exclude)
      checkboxGroupInput("traits", "Select Traits to Analyze", choices = traits)
    })

    updateSelectInput(session, "gge_trait", choices = names(df)[sapply(df, is.numeric)])
    updateCheckboxGroupInput(session, "multi_traits", choices = names(df)[sapply(df, is.numeric)])
  })

  # -------------------- DESCRIPTIVE + MODEL ANALYSIS --------------------

# ---- Descriptive Analysis ----
observeEvent(input$run_descriptive, {
  req(raw_data(), input$traits, input$entry)
  df <- raw_data()
  results_list <- map(input$traits, function(trait) {
    df_trait <- df[!is.na(df[[trait]]), ]
    df_trait[[input$entry]] <- as.factor(df_trait[[input$entry]])
    df_trait$Location <- if (!is.null(input$loc)) as.factor(df_trait[[input$loc]]) else "All"

    summary_tbl <- df_trait %>%
      group_by(Location) %>%
      summarise(
        Trait = trait,
        Mean = round(mean(.data[[trait]]), 2),
        SD = round(sd(.data[[trait]]), 2),
        SE = round(SD / sqrt(n()), 2),
        CV = round(100 * SD / Mean, 2),
        Min = round(min(.data[[trait]]), 2),
        Max = round(max(.data[[trait]]), 2),
        N = n(),
        Missing = sum(is.na(df[[trait]])),
        Missing_Percent = round(100 * Missing / nrow(df), 2),
        .groups = "drop"
      )

    boxplot <- ggplot(df_trait, aes(x = Location, y = .data[[trait]], fill = Location)) +
      geom_boxplot(color = "black") +
      theme_minimal() +
      labs(title = paste("Boxplot of", trait, "by Location"), y = trait) +
      scale_fill_brewer(palette = input$palette)

    qqplots <- df_trait %>%
      group_split(Location) %>%
      map(~ ggplot(.x, aes(sample = .data[[trait]])) +
            stat_qq() + stat_qq_line() +
            ggtitle(paste("QQ -", unique(.x$Location))) +
            theme_minimal())

    list(summary = summary_tbl, boxplot = boxplot, qq = qqplots)
  })

  names(results_list) <- input$traits
  descriptive_results(results_list)

  output$descriptive_status <- renderUI({
    span(style = "color: green; font-weight: bold;", icon("check"), " Descriptive Analysis Completed ✅")
  })

  shinyjs::enable("run_model")
  shinyjs::enable("download_zip1")

  output$descriptive_ui <- renderUI({
    tabs <- map(names(results_list), function(trait) {
      r <- results_list[[trait]]
      tabPanel(
        title = trait,
        h4(" Summary Statistics"), tableOutput(paste0("sum_", trait)),
        h4(" Boxplot"), plotOutput(paste0("box_", trait)),
        h4(" QQ Plot(s)"), fluidRow(map(seq_along(r$qq), function(i) {
          column(4, plotOutput(paste0("qq_", trait, "_", i)))
        }))
      )
    })
    do.call(tabsetPanel, tabs)
  })

  walk2(names(results_list), results_list, function(trait, r) {
    output[[paste0("sum_", trait)]] <- renderTable(r$summary)
    output[[paste0("box_", trait)]] <- renderPlot(r$boxplot)
    walk2(seq_along(r$qq), r$qq, function(i, p) {
      output[[paste0("qq_", trait, "_", i)]] <- renderPlot(p)
    })
  })

  updateTabsetPanel(session, "result_tabs", selected = " Descriptive Results")
})

# ---- Model-Based Analysis ----
observeEvent(input$run_model, {
  req(raw_data(), input$traits, input$entry)
  df <- raw_data()
  results <- list()

  for (trait in input$traits) {
    tryCatch({
      df_sub <- df %>% filter(!is.na(.data[[trait]]))
      df_sub[[input$entry]] <- as.factor(df_sub[[input$entry]])
      if (!is.null(input$loc)) df_sub[[input$loc]] <- as.factor(df_sub[[input$loc]])
      if (!is.null(input$rep)) df_sub[[input$rep]] <- as.factor(df_sub[[input$rep]])
      if (!is.null(input$block)) df_sub[[input$block]] <- as.factor(df_sub[[input$block]])

      locs <- if (!is.null(input$loc)) unique(df_sub[[input$loc]]) else "All"
      combined_label <- "Combined"

      # --- BLUPs ---
      blup_df <- data.frame(Genotype = levels(df_sub[[input$entry]]))
      if (input$genotype_model %in% c("Random", "Both")) {
        model_comb <- lmer(as.formula(paste0(trait, " ~ (1|", input$entry, ")")), data = df_sub)
        ran_comb <- ranef(model_comb)[[input$entry]]
        blup_comb <- rownames_to_column(ran_comb, var = "Genotype") %>%
          mutate(!!combined_label := round(`(Intercept)` + fixef(model_comb)[["(Intercept)"]], 2)) %>%
          select(Genotype, !!combined_label)
        blup_df <- full_join(blup_df, blup_comb, by = "Genotype")

        if (input$location_wise && input$trial_type == "Multi Location") {
          for (loc in locs) {
            d <- df_sub[df_sub[[input$loc]] == loc, ]
            model <- lmer(as.formula(paste0(trait, " ~ (1|", input$entry, ")")), data = d)
            ran <- ranef(model)[[input$entry]]
            blup_loc <- rownames_to_column(ran, var = "Genotype") %>%
              mutate(!!loc := round(`(Intercept)` + fixef(model)[["(Intercept)"]], 2)) %>%
              select(Genotype, !!loc)
            blup_df <- full_join(blup_df, blup_loc, by = "Genotype")
          }
        }
      }

      # --- BLUEs ---
      blue_df <- data.frame(Genotype = levels(df_sub[[input$entry]]))
      if (input$genotype_model %in% c("Fixed", "Both")) {
        model_comb <- lm(as.formula(paste0(trait, " ~ ", input$entry)), data = df_sub)
        blue_comb <- emmeans(model_comb, specs = input$entry) %>%
          as.data.frame() %>%
          select(Genotype = !!input$entry, emmean) %>%
          mutate(!!combined_label := round(emmean, 2)) %>%
          select(Genotype, !!combined_label)
        blue_df <- full_join(blue_df, blue_comb, by = "Genotype")

        if (input$location_wise && input$trial_type == "Multi Location") {
          for (loc in locs) {
            d <- df_sub[df_sub[[input$loc]] == loc, ]
            model <- lm(as.formula(paste0(trait, " ~ ", input$entry)), data = d)
            blue_loc <- emmeans(model, specs = input$entry) %>%
              as.data.frame() %>%
              select(Genotype = !!input$entry, emmean) %>%
              mutate(!!loc := round(emmean, 2)) %>%
              select(Genotype, !!loc)
            blue_df <- full_join(blue_df, blue_loc, by = "Genotype")
          }
        }
      }

      # --- ANOVA ---
      terms <- c()
      if (!is.null(input$loc)) terms <- c(terms, input$loc)
      terms <- c(terms, input$entry)
      if (!is.null(input$loc)) terms <- c(terms, paste0(input$loc, ":", input$entry))
      if (!is.null(input$rep)) terms <- c(terms, paste0(input$loc, ":", input$rep))
      if (!is.null(input$block)) terms <- c(terms, input$block)

      aov_model <- tryCatch({
        aov(as.formula(paste(trait, "~", paste(terms, collapse = "+"))), data = df_sub)
      }, error = function(e) NULL)

      anova_res <- if (!is.null(aov_model)) {
        broom::tidy(aov_model) %>%
          mutate(
            `Sum Sq` = round(sumsq, 2),
            `Mean Sq` = round(meansq, 2),
            `F value` = round(statistic, 2),
            `Pr(>F)` = round(p.value, 4),
            Stars = case_when(
              p.value < 0.001 ~ "***",
              p.value < 0.01 ~ "**",
              p.value < 0.05 ~ "*",
              p.value < 0.1 ~ ".",
              TRUE ~ ""
            )
          ) %>%
          select(Source = term, Df = df, `Sum Sq`, `Mean Sq`, `F value`, `Pr(>F)`, Stars)
      } else {
        data.frame()
      }

      model <- lmer(as.formula(paste0(trait, " ~ (1|", input$entry, ")")), data = df_sub)
      vc <- as.data.frame(VarCorr(model))
      g <- vc[vc$grp == input$entry, "vcov"]
      e <- attr(VarCorr(model), "sc")^2
      h2 <- round(g / (g + e), 2)

      results[[trait]] <- list(anova = anova_res, h2 = h2, blup = blup_df, blue = blue_df)
    }, error = function(e) {
      results[[trait]] <- list(error = e$message)
    })
  }

  model_results(results)

  output$model_status <- renderUI({
    span(style = "color: green; font-weight: bold;", icon("check"), " Model-Based Analysis Completed You can now proceed to the 'Analysis 2 tab ✅")
  })

  output$model_ui <- renderUI({
    tabs <- map(names(results), function(trait) {
      r <- results[[trait]]
      tabPanel(trait,
        h4(" ANOVA Table"), DTOutput(paste0("aov_", trait)),
        h4(" Heritability (H²)"), verbatimTextOutput(paste0("h2_", trait)),
        h4(" BLUEs (Location-wise + Combined)"), DTOutput(paste0("blue_", trait)),
        h4(" BLUPs (Location-wise + Combined)"), DTOutput(paste0("blup_", trait))
      )
    })
    do.call(tabsetPanel, tabs)
  })

  walk(names(results), function(trait) {
    r <- results[[trait]]
    output[[paste0("aov_", trait)]]  <- renderDT(datatable(r$anova))
    output[[paste0("h2_", trait)]]   <- renderPrint(r$h2)
    output[[paste0("blue_", trait)]] <- renderDT(datatable(r$blue))
    output[[paste0("blup_", trait)]] <- renderDT(datatable(r$blup))
  })

  updateTabsetPanel(session, "result_tabs", selected = " Model Results")
  shinyjs::enable("download_zip1")
})

  # -------------------- GGE BIPLOT --------------------
observeEvent(input$run_gge, {
  req(input$gge_trait)
  mod_res <- model_results()
  trait_data <- mod_res[[input$gge_trait]]

  if (is.null(trait_data) || is.null(trait_data$blue)) {
    showModal(modalDialog(
      title = "Location-wise BLUEs Required",
      "Please run Model-Based Analysis with:",
      tags$ul(
        tags$li("Genotype as: Fixed or Both"),
        tags$li("Location-wise BLUEs: Checked")
      ),
      easyClose = TRUE
    ))
    return()
  }

  df_blue <- trait_data$blue
  if (!"Combined" %in% colnames(df_blue)) {
    showModal(modalDialog(
      title = "BLUEs Missing",
      "No location-wise BLUEs found. Ensure appropriate model settings.",
      easyClose = TRUE
    ))
    return()
  }

  df_gge <- df_blue %>%
    select(-Combined) %>%
    pivot_longer(-Genotype, names_to = "env", values_to = "resp") %>%
    rename(gen = Genotype)

  gge_model <- metan::gge(df_gge, env = env, gen = gen, resp = resp)

  # Plot 1: Which Won Where
  output$gge_plot_type1 <- renderPlot({
    plot(gge_model, type = 3)
  })

  # Plot 2: Mean vs Stability
  output$gge_plot_type2 <- renderPlot({
    plot(gge_model, type = 2)
  })

  # Plot 3: Representativeness vs Discriminativeness
  output$gge_plot_type3 <- renderPlot({
    plot(gge_model, type = 4)
  })

  output$gge_status <- renderUI({
    span(style = "color: green; font-weight: bold;", icon("check"), " GGE Biplot Completed ✅")
  })

  updateTabsetPanel(session, "analysis2_tabs", selected = " GGE Biplot")
})

  # -------------------- PCA --------------------
  observeEvent(input$run_pca, {
    req(model_results(), input$multi_traits)

    trait_df <- map(input$multi_traits, function(trait) {
      bl <- model_results()[[trait]]$blue
      if (!is.null(bl)) bl %>% select(Genotype, Combined) else NULL
    }) %>% discard(is.null)

    if (length(trait_df) < length(input$multi_traits)) {
      showModal(modalDialog(
        title = "Missing BLUEs",
        "Some traits lack Combined BLUEs. Please re-run Model-Based Analysis.",
        easyClose = TRUE
      ))
      return()
    }

    df_pca <- reduce(trait_df, full_join, by = "Genotype")
    names(df_pca) <- c("Genotype", input$multi_traits)
    rownames(df_pca) <- df_pca$Genotype
    df_pca <- df_pca[, input$multi_traits]

    res.pca <- FactoMineR::PCA(df_pca, graph = FALSE)
    pca_results(list(pca = res.pca))

    output$pca_plot_biplot <- renderPlot({
      fviz_pca_ind(res.pca, col.ind = "cos2", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), repel = TRUE)
    })

    output$pca_plot_scree <- renderPlot({ fviz_screeplot(res.pca, addlabels = TRUE) })
    output$pca_plot_varcontrib <- renderPlot({
      fviz_pca_var(res.pca, col.var = "contrib", gradient.cols = c("white", "blue", "red"))
    })
output$pca_table_summary <- renderTable({
  eigen_tbl <- factoextra::get_eigenvalue(res.pca)
  eigen_tbl <- round(eigen_tbl, 2)
  eigen_tbl
}, rownames = TRUE)

    output$pca_status <- renderUI({
      span(style = "color: green; font-weight: bold;", icon("check"), " PCA Completed ✅")
    })

    updateTabsetPanel(session, "analysis2_tabs", selected = " PCA Plot")
  })

  # -------------------- CORRELATION --------------------
  observeEvent(input$run_corr, {
    req(pca_results())
    df_corr <- pca_results()$pca$call$X

    output$corr_plot1 <- renderPlot({
      corrplot(cor(df_corr), method = "number", type = "upper", order = "hclust",
               col = RColorBrewer::brewer.pal(8, "RdYlBu"))
    })

    output$corr_plot2 <- renderPlot({
      PerformanceAnalytics::chart.Correlation(df_corr, histogram = FALSE)
    })

    output$corr_status <- renderUI({
      span(style = "color: green; font-weight: bold;", icon("check"), " Correlation Completed ✅")
    })

    updateTabsetPanel(session, "analysis2_tabs", selected = " Correlation Plot")
  })
  
  output$download_analysis1 <- downloadHandler(
  filename = function() {
    paste0("PbAT_Analysis1_", Sys.Date(), ".zip")
  },
  content = function(file) {
    tmp_dir <- tempdir()
    files <- c()
    result_list <- descriptive_results()
    model_list <- model_results()

    for (trait in names(result_list)) {
      # --- Boxplot PDF ---
      box_file <- file.path(tmp_dir, paste0(trait, "_boxplot.pdf"))
      pdf(box_file)
      print(result_list[[trait]]$boxplot)
      dev.off()
      files <- c(files, box_file)

      # --- QQ Plots PDF ---
      qq_file <- file.path(tmp_dir, paste0(trait, "_qqplots.pdf"))
      pdf(qq_file)
      walk(result_list[[trait]]$qq, print)
      dev.off()
      files <- c(files, qq_file)

      # --- Summary Table CSV ---
      summary_file <- file.path(tmp_dir, paste0(trait, "_summary.csv"))
      write.csv(result_list[[trait]]$summary, summary_file, row.names = FALSE)
      files <- c(files, summary_file)

      # --- BLUEs CSV ---
      if (!is.null(model_list[[trait]]$blue)) {
        blue_file <- file.path(tmp_dir, paste0(trait, "_BLUEs.csv"))
        write.csv(model_list[[trait]]$blue, blue_file, row.names = FALSE)
        files <- c(files, blue_file)
      }

      # --- BLUPs CSV ---
      if (!is.null(model_list[[trait]]$blup)) {
        blup_file <- file.path(tmp_dir, paste0(trait, "_BLUPs.csv"))
        write.csv(model_list[[trait]]$blup, blup_file, row.names = FALSE)
        files <- c(files, blup_file)
      }

      # --- ANOVA Table CSV ---
      if (!is.null(model_list[[trait]]$anova)) {
        aov_file <- file.path(tmp_dir, paste0(trait, "_ANOVA.csv"))
        write.csv(model_list[[trait]]$anova, aov_file, row.names = FALSE)
        files <- c(files, aov_file)
      }
    }

    zip::zip(zipfile = file, files = files, mode = "cherry-pick")
  }
)

output$download_analysis2 <- downloadHandler(
  filename = function() {
    paste0("PbAT_Analysis2_", Sys.Date(), ".zip")
  },
  content = function(file) {
    tmp_dir <- tempdir()
    files <- c()

    # --- GGE Plots ---
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

    # --- PCA ---
    pca_obj <- tryCatch(pca_results()$pca, error = function(e) NULL)
    if (!is.null(pca_obj)) {
      # PCA biplot
      pdf(file.path(tmp_dir, "PCA_Biplot.pdf"))
      print(fviz_pca_ind(pca_obj, col.ind = "cos2", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), repel = TRUE))
      dev.off()
      # Scree
      pdf(file.path(tmp_dir, "PCA_Scree.pdf"))
      print(fviz_screeplot(pca_obj, addlabels = TRUE))
      dev.off()
      # Contributions
      pdf(file.path(tmp_dir, "PCA_Contributions.pdf"))
      print(fviz_pca_var(pca_obj, col.var = "contrib", gradient.cols = c("white", "blue", "red")))
      dev.off()

      files <- c(
        files,
        file.path(tmp_dir, "PCA_Biplot.pdf"),
        file.path(tmp_dir, "PCA_Scree.pdf"),
        file.path(tmp_dir, "PCA_Contributions.pdf")
      )

      # PCA coordinates
      coords_file <- file.path(tmp_dir, "PCA_Coordinates.csv")
      write.csv(pca_obj$ind$coord, coords_file, row.names = TRUE)
      files <- c(files, coords_file)

      # PCA eigenvalues
      eigen_file <- file.path(tmp_dir, "PCA_Eigenvalues.csv")
      write.csv(factoextra::get_eigenvalue(pca_obj), eigen_file, row.names = TRUE)
      files <- c(files, eigen_file)
    }

    # --- Correlation ---
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


}

# ------------------------- APP LAUNCH -------------------------
shinyApp(ui = ui, server = server)
