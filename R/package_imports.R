# Centralize package imports for PBAT.
# This file uses @importFrom for all packages to avoid namespace conflicts.

#' @keywords internal
#'
# Shiny and UI Packages
#' @importFrom shiny actionButton actionLink br checkboxGroupInput checkboxInput column conditionalPanel div downloadButton downloadHandler fileInput fluidPage fluidRow h1 h2 h3 h4 h5 helpText hr HTML icon incProgress mainPanel modalDialog moduleServer navbarMenu navbarPage NS numericInput observe observeEvent p plotOutput radioButtons reactive reactiveVal reactiveValues reactiveValuesToList removeModal renderPlot renderPrint renderTable renderUI req selectInput showModal showNotification showTab sidebarLayout sidebarPanel span tableOutput tabPanel tabsetPanel tagList tags textInput uiOutput updateCheckboxGroupInput updateNavbarPage updateSelectInput updateTabsetPanel validate verbatimTextOutput wellPanel withProgress need
#' @importFrom shinyjs useShinyjs runjs show hide toggle enable disable
#' @importFrom bslib bs_theme page_navbar
#' @importFrom waiter use_waiter waiter_hide waiter_show spin_fading_circles
#' @importFrom DT DTOutput renderDT datatable
#' @importFrom zip zipr
#'
# Tidyverse and Data Manipulation
#' @importFrom dplyr %>% across all_of arrange as_tibble bind_rows contains distinct filter full_join group_by left_join mutate n pull recode select starts_with summarise summarise_all where
#' @importFrom tidyr drop_na pivot_longer pivot_wider replace_na separate
#' @importFrom tibble as_tibble column_to_rownames remove_rownames rownames_to_column tibble
#' @importFrom purrr map map_chr map_dbl map_dfc map_dfr map_int map_lgl pmap some
#' @importFrom readr read_csv write_csv
#' @importFrom rlang .data := sym
#' @importFrom scales percent
#'
# Plotting and Visualization
#' @importFrom ggplot2 aes element_text facet_wrap geom_abline geom_bar geom_boxplot geom_col geom_hline geom_jitter geom_label geom_line geom_point geom_segment geom_text geom_tile geom_vline ggplot labs scale_fill_brewer theme theme_bw theme_classic theme_minimal xlab ylab stat_qq stat_qq_line#' @importFrom ggplot2 aes element_text facet_wrap geom_abline geom_bar geom_boxplot geom_col geom_hline geom_jitter geom_label geom_line geom_point geom_segment geom_text geom_tile geom_vline ggplot labs scale_fill_brewer theme theme_bw theme_classic theme_minimal xlab ylab stat_qq stat_qq_line ggtitle
#' @importFrom RColorBrewer brewer.pal
#' @importFrom corrplot corrplot
#' @importFrom factoextra fviz_ca_biplot fviz_cos2 fviz_pca_biplot fviz_pca_ind fviz_pca_var fviz_screeplot hcut
#' @importFrom PerformanceAnalytics chart.Correlation
#' @importFrom semPlot semPaths
#'
# Statistics and Modeling
#' @importFrom agricolae HSD.test LSD.test duncan.test friedman kruskal kurtosis reg.homog skewness waerden.test
#' @importFrom broom tidy
#' @importFrom broom.mixed tidy
#' @importFrom car Anova leveneTest qqPlot
#' @importFrom emmeans emmeans contrast
#' @importFrom FactoMineR CA PCA
#' @importFrom lme4 VarCorr
#' @importFrom lmerTest lmer
#' @importFrom metan anova_ind arrange_comb corr_coef env_strat GGE ge_effects ge_plot get_dist gge gv_ind hmgv_ind path_coeff plot_scores recode_factor res_ind waas waasb waasb_ind
#' @importFrom multcomp cld glht
#'
# Base R and Utils
#' @importFrom grDevices dev.off pdf
#' @importFrom stats aggregate anova aov as.formula cor lm median model.tables na.omit pf pt qf qt residuals runif sd shapiro.test update vcov
#' @importFrom utils capture.output combn head tail write.csv zip
NULL
