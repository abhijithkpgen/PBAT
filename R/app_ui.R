# ===================================================================
# FINAL INTEGRATED SHINY APPLICATION
# This app.R file loads all modules and connects them with a custom UI.
# UPDATE: Replaced non-ASCII emoji characters with text to pass CRAN checks.
# ===================================================================

# ===================================================================
# 3. DEFINE THE USER INTERFACE (UI)
# ===================================================================
app_ui <- function() {
  navbarPage(
    title = div(
      style = "display: flex; align-items: center; gap: 16px;",
      tags$img(src = "www/LogoNobg.png", height = "60px", style = "margin-right: 8px;"),
      span("PbAT: Plant Breeding Analytical Tools  v1.0.2")
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
        flex-wrap: wrap; /* Allows panels to wrap on smaller screens */
        min-height: 85vh;
        gap: 25px; /* Spacing between panels */
        margin-top: 30px;
        background: none;
      }
      .overlay-panel {
        background-color: #142850 !important;
        padding: 20px;
        border-radius: 12px;
        box-shadow: 0 0 18px rgba(0,0,0,0.17);
        flex: 1; /* Allow panels to grow */
        min-width: 340px; /* Minimum width before wrapping */
        max-width: 450px; /* Maximum width */
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
      
      /* Style for the new installation box */
      .installation-box {
        background-color: #f8f9fa !important;
        color: #142850 !important;
        border-left: 5px solid #00b894;
      }
      .installation-box h4 {
        color: #142850 !important;
        font-weight: bold;
      }
      .installation-box code {
        background-color: #e9ecef;
        color: #333;
        padding: 2px 4px;
        border-radius: 4px;
        font-size: 12px;
      }
      .installation-box pre {
        background-color: #e9ecef;
        padding: 15px;
        border-radius: 5px;
        border: 1px solid #ced4da;
        white-space: pre-wrap; /* Ensures code wraps */
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
                   h2("Help & Guide", style = "color: #23272b;"),
                   tabsetPanel(
                     id = "help_tabs",
                     tabPanel("Troubleshooting",
                              div(style = "padding-top: 20px;",
                                  h3("Troubleshooting Common Issues"),
                                  p("Encountering an issue? Most problems, especially with complex model analyses, are related to network connection timeouts during long calculations. Here are a few simple steps you can take to resolve common errors."),
                                  h4("Problem: Results Not Appearing or an Error Message (like 'AJAX error') After Running an Analysis"),
                                  p(HTML("<b>Cause:</b> This typically happens when the analysis takes a while to complete (10 seconds or more) and your browser's connection to the server times out, especially on a slower or less stable internet connection. The analysis likely finished successfully on the server, but the results didn't make it back to your browser.")),
                                  h4("Solutions (Try these in order):"),
                                  tags$ul(
                                    tags$li(HTML("<b>1. The Quick Refresh Trick:</b><br>This is the easiest and most common fix. If the results area is blank but the rest of the app is working, simply interact with the table controls. Click the <b>'Show X entries'</b> dropdown menu above the empty table area and select a different number. This sends a new, quick request to the server, which then correctly displays the results that were already calculated.")),
                                    tags$li(HTML("<b>2. Ensure a Stable Internet Connection:</b><br>Since these analyses involve sending data and waiting for results, a stable connection is key. If you are on a weak Wi-Fi signal, try moving closer to your router or connect to a more reliable network and try running the analysis again.")),
                                    tags$li(HTML("<b>3. Reduce the Analysis Workload:</b><br>The more traits you select, the longer the server needs to compute. If the analysis is still failing, try reducing the complexity by selecting fewer traits at a time.")),
                                    tags$li(HTML("<b>4. Be Patient:</b><br>A complex mixed-model analysis on a large dataset can take some time. After clicking 'Run,' please allow up to a minute for the server to process before assuming there is an error."))
                                  )
                              )
                     ),
                     tabPanel("Sample Data",
                              div(style = "padding-top: 20px;",
                                  h3("Sample Data Downloads"),
                                  p("Disclaimer: The example datasets provided in this application are simulated for demonstration purposes only. They do not represent actual experimental results and should not be used for research conclusions."),
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
                                    tags$li(tags$a(href = "www/AMMI_GGE_Sample_Data.csv", "Biplot Sample Format CSV", download = NA, target = "_blank")),
                                    tags$li(tags$a(href = "www/Mult_Variate_sample_format.csv", "Multivariate Analysis Sample Format CSV", download = NA, target = "_blank")),
                                    tags$li(tags$a(href = "www/RCBD_sample.csv", "RCBD Sample CSV", download = NA, target = "_blank"))
                                  )
                              )
                     )
                   )
               )
             )
    )
  )
}