# ===================================================================
# FINAL INTEGRATED SHINY APPLICATION
# This app.R file loads all modules and connects them with a custom UI.
# UPDATE: Removed redundant library calls to rely on global.R
# ===================================================================

# ===================================================================
# 3. DEFINE THE USER INTERFACE (UI)
# ===================================================================
app_ui <- function() {
  navbarPage(
    title = div(
      style = "display: flex; align-items: center; gap: 16px;",
      tags$img(src = "www/LogoNobg.png", height = "60px", style = "margin-right: 8px;"),
      span("PbAT: Plant Breeding Analytical Tools  v1.0.1")
    ),
    id = "main_navbar", 
    theme = bs_theme(
      version = 4,
      bootswatch = "cerulean",
      primary = "#e17055",
      secondary = "#00b894"
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
        background-color: #142850 !important;
        padding: 20px;
        border-radius: 12px;
        box-shadow: 0 0 18px rgba(0,0,0,0.17);
        width: 400px;
        font-size: 13px;
        color: #fafcff !important;
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
      }
      .sidebarPanel, .sidebarPanel label, .sidebarPanel .control-label,
      .sidebarPanel .form-control, .sidebarPanel .selectize-input,
      .sidebarPanel .checkbox, .sidebarPanel .radio,
      .sidebarPanel .checkbox label, .sidebarPanel .radio label {
        color: #142850 !important;
      }
    ")))
    ),
  
    tabPanel("Home", homeUI(id = "home")),
  
    navbarMenu("Experimental Design",
               analysisUI(id = "eda")[[1]], 
               analysisUI(id = "eda")[[2]]
    ),
  
    mating_design_ui(id = "mating"),
  
    multivariate_analysis_ui(id = "multi"),
  
    tabPanel("Help & Guide",
             fluidPage(
               div(style = "padding: 30px;",
                   h2(" Help & Guide", style = "color: #23272b;"),
                   h4(" Sample Format Downloads", style = "color: #23272b;"),
                   p("Note: These files must be placed inside a 'www' folder in your app's directory to be downloadable."),
                   tags$ul(
                     tags$li(tags$a(href = "www/Alpha_lattice_sample.csv", "Alpha Lattice Sample CSV", download = NA, target = "_blank")),
                     tags$li(tags$a(href = "www/Augmented_RCBD_Sample.csv", "Augmented RCBD Sample CSV", download = NA, target = "_blank")),
                     tags$li(tags$a(href = "www/Diallel_Griffing_Method1_Sample.csv", "Griffing Method I Diallel Sample CSV", download = NA, target = "_blank")),
                     tags$li(tags$a(href = "www/Diallel_Griffing_Method2_Sample.csv", "Griffing Method II Diallel Sample CSV", download = NA, target = "_blank")),
                     tags$li(tags$a(href = "www/Diallel_Griffing_Method3_Sample.csv", "Griffing Method III Diallel Sample CSV", download = NA, target = "_blank")),
                     tags$li(tags$a(href = "www/Diallel_Griffing_Method4_Sample.csv", "Griffing Method IV Diallel Sample CSV", download = NA, target = "_blank")),
                     tags$li(tags$a(href = "www/Factorial_CRD_sample.csv", "Factorial CRD Sample CSV", download = NA, target = "_blank")),
                     tags$li(tags$a(href = "www/Line_x_Tester_Sample.csv", "Line x Tester Sample CSV", download = NA, target = "_blank")),
                     tags$li(tags$a(href = "www/Partial_diallel_dummy.csv", "Partial Diallel Sample CSV", download = NA, target = "_blank")),
                     tags$li(tags$a(href = "www/Biplot_Sample_Format.csv", "Biplot Sample Format CSV", download = NA, target = "_blank")),
                     tags$li(tags$a(href = "www/Mult_Variate_sample_format.csv", "Multivariate Analysis Sample Format CSV", download = NA, target = "_blank")),
                     tags$li(tags$a(href = "www/RCBD_sample.csv", "RCBD Sample CSV", download = NA, target = "_blank"))
                   )
               )
             )
    )
  )
}
