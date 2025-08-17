# R/package_imports.R

# Centralize package imports for PBAT.
# This file centralizes imports to keep NAMESPACE tidy.

#' @keywords internal
#'
# Shiny and UI Packages
#' @importFrom shiny actionButton actionLink br checkboxGroupInput checkboxInput column conditionalPanel div downloadButton downloadHandler fileInput fluidPage fluidRow h1 h2 h3 h4 h5 helpText hr HTML icon incProgress mainPanel modalDialog moduleServer navbarMenu navbarPage NS numericInput observe observeEvent p plotOutput radioButtons reactive reactiveVal reactiveValues reactiveValuesToList removeModal renderPlot renderPrint renderTable renderUI req selectInput showModal showNotification showTab sidebarLayout sidebarPanel span tableOutput tabPanel tabsetPanel tagList tags textInput uiOutput updateCheckboxGroupInput updateNavbarPage updateSelectInput updateTabsetPanel validate verbatimTextOutput wellPanel withProgress need
#' @importFrom shinyjs useShinyjs runjs show hide toggle enable disable
#' @importFrom bslib bs_theme page_navbar
#' @importFrom waiter use_waiter waiter_hide waiter_show spin_fading_circles waiter_show_on_load
#' @importFrom DT DTOutput renderDT datatable renderDataTable dataTableOutput
#' @importFrom zip zipr
#' @importFrom colourpicker colourInput
#'
# Tidyverse and Data Manipulation
#' @importFrom dplyr %>% across all_of any_of arrange bind_rows contains distinct filter full_join group_by left_join mutate n pull recode select starts_with summarise summarise_all where
#' @importFrom tidyr drop_na pivot_longer pivot_wider separate
#' @importFrom tibble as_tibble tibble rownames_to_column
#' @importFrom purrr map map_chr map_dbl map_dfc map_dfr map_int map_lgl pmap some iwalk walk2 imap
#' @importFrom readr read_csv write_csv
#' @importFrom rlang .data := sym
#' @importFrom scales percent
#'
# Plotting and Visualization
#' @importFrom RColorBrewer brewer.pal.info brewer.pal
#' @importFrom corrplot corrplot
#' @importFrom factoextra fviz_ca_biplot fviz_cos2 fviz_pca_biplot fviz_pca_ind fviz_pca_var fviz_screeplot hcut get_eigenvalue
#' @importFrom PerformanceAnalytics chart.Correlation
#' @importFrom semPlot semPaths
#' @importFrom ggplot2 aes element_text facet_wrap geom_boxplot geom_line geom_point ggplot labs scale_fill_brewer theme theme_bw theme_minimal
#'
# Statistics and Modeling
#' @import promises
#' @import future
#' @importFrom agricolae HSD.test LSD.test duncan.test friedman kruskal kurtosis reg.homog skewness waerden.test
#' @importFrom broom tidy
#' @importFrom broom.mixed tidy
#' @importFrom car Anova leveneTest qqPlot
#' @importFrom emmeans emmeans contrast
#' @importFrom FactoMineR CA PCA
#' @importFrom lme4 VarCorr isSingular ranef fixef
#' @importFrom lmerTest lmer
#' @importFrom multcomp cld glht
#' @importFrom lavaan sem parameterEstimates
#' @importFrom metan plot_scores
#'
# Base R and Utils
#' @importFrom grDevices dev.off pdf
#' @importFrom utils install.packages
#' @importFrom stats aggregate anova aov as.formula cor lm median model.tables na.omit pf pt qf qt residuals runif sd shapiro.test update vcov
#' @importFrom utils capture.output combn head tail write.csv zip
NULL
