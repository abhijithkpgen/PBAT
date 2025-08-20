# R/package_imports.R

# This file centralizes all package imports for the PBAT application to ensure
# that all necessary functions are available and the NAMESPACE is managed correctly.

#' @keywords internal
#'
# Shiny and UI Packages
#' @importFrom shiny NS tabPanel sidebarLayout sidebarPanel uiOutput mainPanel fluidPage div h2 h3 h4 h5 p tags actionButton br checkboxGroupInput checkboxInput conditionalPanel downloadButton fileInput hr numericInput radioButtons reactive reactiveVal reactiveValues observeEvent renderUI req selectInput tabsetPanel updateNavbarPage updateTabsetPanel updateSelectInput showModal modalDialog need validate actionLink moduleServer HTML tagList wellPanel helpText reactiveValuesToList verbatimTextOutput span icon tableOutput plotOutput fluidRow column renderTable renderPlot withProgress incProgress showNotification observe updateCheckboxGroupInput renderPrint navbarPage navbarMenu downloadHandler
#' @importFrom shinyjs useShinyjs delay runjs toggle hidden
#' @importFrom bslib bs_theme page_navbar
#' @importFrom waiter use_waiter waiter_hide waiter_show spin_fading_circles waiter_show_on_load
#' @importFrom DT DTOutput renderDT datatable
#' @importFrom plotly plotlyOutput renderPlotly ggplotly
#' @importFrom htmlwidgets saveWidget
#' @importFrom colourpicker colourInput
#'
# Tidyverse and Data Manipulation
#' @importFrom dplyr %>% across all_of any_of arrange bind_rows contains distinct filter full_join group_by left_join mutate n pull recode select starts_with summarise ungroup row_number ntile if_else case_when rename where
#' @importFrom tidyr drop_na pivot_longer pivot_wider separate
#' @importFrom tibble as_tibble tibble rownames_to_column
#' @importFrom purrr map iwalk
#' @importFrom readr read_csv write_csv
#' @importFrom rlang .data := sym
#' @importFrom scales percent
#'
# Plotting and Visualization
#' @importFrom RColorBrewer brewer.pal.info
#' @importFrom corrplot corrplot
#' @importFrom factoextra fviz_pca_ind fviz_screeplot fviz_pca_var get_eigenvalue
#' @importFrom PerformanceAnalytics chart.Correlation
#' @importFrom semPlot semPaths
#' @importFrom ggplot2 ggplot aes geom_tile scale_fill_viridis_c labs theme_minimal theme element_blank element_text scale_color_manual scale_linetype_manual geom_boxplot geom_jitter geom_line geom_point facet_wrap theme_bw scale_fill_brewer geom_crossbar
#'
# Statistics and Modeling
#' @importFrom future plan multisession
#' @importFrom agricolae duncan.test
#' @importFrom broom tidy
#' @importFrom car Anova
#' @importFrom emmeans emmeans
#' @importFrom FactoMineR PCA
#' @importFrom lme4 isSingular
#' @importFrom lmerTest lmer
#' @importFrom lavaan sem parameterEstimates
#' @importFrom metan performs_ammi waasb ammi_indexes gge plot_scores
#' @importFrom stats aov as.formula na.omit p.adjust pf pnorm pt quantile residuals sd shapiro.test aggregate lm anova median runif cor
#'
# Base R and Utils
#' @importFrom utils capture.output combn write.csv
#' @importFrom grDevices dev.off pdf
#' @importFrom zip zipr
NULL
