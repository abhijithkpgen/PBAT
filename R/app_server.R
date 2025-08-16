#' @importFrom shiny hideTab
# ===================================================================
# 4. DEFINE THE SERVER LOGIC
# ===================================================================
app_server <- function(input, output, session) {
  
  # --- Hide the pre-loader once the main UI is ready ---
  # Use debounce to add a small delay, ensuring the server is fully
  # ready before hiding the loader. This prevents it from getting stuck.
  hide_loader <- shiny::debounce(function() {
    waiter::waiter_hide()
  }, 1000) # 1000 milliseconds = 1 second delay
  
  shiny::observe({
    shiny::req(input$main_navbar) # Waits until the main navbar is rendered
    hide_loader()
  })
  
  # --- Call the Home Module Server ---
  home_inputs <- homeServer(id = "home")
  
  # --- Create Reactive Values for each downstream module ---
  eda_shared_data    <- reactiveVal()
  mating_shared_data <- reactiveValues(file_data = NULL, mating_design = NULL)
  multi_shared_data  <- reactiveValues(file_data = NULL, multi_subtype = NULL)
  
  # --- Dynamic Tab Navigation (Hide/Show Tabs) ---
  
  # Hide all analysis tabs/menus at startup
  observe({
    hideTab("main_navbar", "Experimental Design")
    hideTab("main_navbar", "Mating Design Analysis")
    hideTab("main_navbar", "Multivariate Analysis")
  })
  
  # Observer to Route Data and Switch Tabs ---
  observeEvent(home_inputs(), {
    req(home_inputs())
    mode <- home_inputs()$analysis_mode
    
    # Hide all analysis tabs first to ensure a clean state
    hideTab("main_navbar", "Experimental Design")
    hideTab("main_navbar", "Mating Design Analysis")
    hideTab("main_navbar", "Multivariate Analysis")
    
    # Show and select the right tab(s) based on user's choice
    if (mode == "eda") {
      eda_shared_data(home_inputs())
      showTab("main_navbar", "Experimental Design")
      updateNavbarPage(session, "main_navbar", selected = "Analysis 1")
    } else if (mode == "mating") {
      mating_shared_data$file_data <- home_inputs()$file_data
      mating_shared_data$mating_design <- home_inputs()$mating_design
      showTab("main_navbar", "Mating Design Analysis")
      updateNavbarPage(session, "main_navbar", selected = "Mating Design Analysis")
    } else if (mode == "multivariate") {
      multi_shared_data$file_data <- home_inputs()$file_data
      multi_shared_data$multi_subtype <- home_inputs()$multi_subtype
      showTab("main_navbar", "Multivariate Analysis")
      updateNavbarPage(session, "main_navbar", selected = "Multivariate Analysis")
    }
  })
  
  # --- Call the Server Logic for each Analysis Module ---
  analysisServer(id = "eda", home_inputs = eda_shared_data)
  mating_design_server(id = "mating", shared_data = mating_shared_data)
  multivariate_analysis_server(id = "multi", shared_data = multi_shared_data)
  
}
