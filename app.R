# ---- Load Required Libraries ----
# This section installs and loads all necessary packages for the Shiny app.
# It checks if each package is installed and installs it if not, then loads them.
required_packages <- c(
  "shiny", "dplyr", "readr", "DT", "shinythemes",
  "RColorBrewer", "writexl", "plotly", "shinyjs",
  "ggplot2", "viridis", "tibble", "tidyr"
)
for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
  }
}

# Load the libraries after ensuring they are installed.
lapply(required_packages, library, character.only = TRUE)
useShinyjs()

# ---- Docker Instructions ----
# Last updated: August 30, 2025
# Set Shiny app to listen on all interfaces and a specific port for Docker compatibility.
options(shiny.host = "0.0.0.0")
options(shiny.port = 3838) # Also 8180 is a valid option

# ---- Load the Data ----
# Update paths for Docker environment
if (file.exists("/srv/shiny-server/final_ispr_men_table.csv")) {
  # Docker environment
  setwd("/srv/shiny-server")
  data <- read.csv("final_ispr_men_table.csv", check.names = FALSE)
  abbreviations_file <- "variable_abbreviations.csv"
} else {
  # Local development environment
  #path_outputs <- "C:/Users/schia/Documents/GitHub/Consulting_Catholic_Church"
  path_outputs <- "C:/Users/soffi/Documents/Consulting_Catholic_Church"
  setwd(path_outputs)
  data <- read.csv("final_ispr_men_table.csv", check.names = FALSE)
  abbreviations_file <- file.path(path_outputs, "variable_abbreviations.csv")
}

# ---- Define Variable Abbreviations ----
# Read variable abbreviations from a CSV file and create a named vector.
# The CSV file should have two columns: 'variable_name' and 'abbreviation'.
if (!file.exists(abbreviations_file)) {
  stop("Variable abbreviations CSV file not found at: ", abbreviations_file)
}
abbreviations_df <- read.csv(abbreviations_file, stringsAsFactors = FALSE, check.names = FALSE)
variable_abbreviations <- setNames(abbreviations_df$abbreviation, abbreviations_df$variable_name)

# ---- Data Processing ----
# Identify numeric columns excluding 'Year' and 'Categories of Institutes'.
num_cols <- names(data)[sapply(data, is.numeric)]
num_cols <- setdiff(num_cols, "Year")

# Aggregate or clean data if needed, but since no countries, use as is.
# Ensure 'Year' is integer.
data <- data %>%
  mutate(Year = as.integer(Year))

# ---- Identify All Variables and Time Series Variables ----
all_vars <- num_cols
# Filter variables that have data for more than one year for time series use.
time_series_vars <- num_cols[
  sapply(num_cols, function(var) {
    years <- data %>%
      filter(!is.na(.data[[var]])) %>%
      pull(Year) %>%
      unique()
    length(years) > 1
  })
]

# Identify variables with non-NA data for congregations in 2022
congregations <- c(
  "Congr. for the Inst. of Cons. Life and Soc. of Apos. Life",
  "Congregation for the Eastern Churches",
  "Congregation for the Evangelization of Peoples",
  "Congregations (total)"
)
congregation_vars <- num_cols[
  sapply(num_cols, function(var) {
    any(!is.na(data %>%
                 filter(Year == 2022, `Categories of Institutes` %in% congregations) %>%
                 pull(.data[[var]])))
  })
]

# ---- UI Layout ----
# Prepare category choices for the selector.
all_categories <- sort(unique(data$`Categories of Institutes`))
category_choices_list <- as.list(all_categories)
names(category_choices_list) <- all_categories
final_category_dropdown_choices <- category_choices_list

# Helper function to create select inputs for variables, years, or categories.
create_select_input <- function(id, label, choices, selected = NULL, multiple = FALSE, placeholder = NULL) {
  if (!is.null(placeholder)) {
    selectizeInput(
      inputId = id,
      label = label,
      choices = choices,
      selected = selected,
      multiple = multiple,
      options = list(placeholder = placeholder)
    )
  } else {
    selectInput(
      inputId = id,
      label = label,
      choices = choices,
      selected = selected,
      multiple = multiple
    )
  }
}

# Helper function to create download buttons for CSV and Excel.
create_download_buttons <- function() {
  div(
    style = "margin-top: 10px;",
    downloadButton("download_csv", "CSV", class = "btn btn-sm btn-success"),
    downloadButton("download_excel", "Excel", class = "btn btn-sm btn-info")
  )
}

# Helper function to create download data for CSV/Excel handlers.
create_download_data <- function(data, year, variable, selected_category, view_by_congregation) {
  filtered <- data %>%
    filter(Year == year) %>%
    select(`Categories of Institutes`, Year, all_of(variable))
  if (view_by_congregation) {
    filtered <- filtered %>% filter(`Categories of Institutes` %in% congregations)
  } else {
    filtered <- filtered %>% filter(!(`Categories of Institutes` %in% congregations))
  }
  if (!is.null(selected_category) && selected_category %in% filtered$`Categories of Institutes`) {
    filtered <- filtered %>% filter(`Categories of Institutes` == selected_category)
  }
  filtered
}

# Define the Shiny UI with custom styles and layout.
ui <- tagList(
  tags$head(
    tags$style(HTML("
    html, body {
      height: 100%;
      margin: 0;
      padding: 0;
      overflow: hidden;
    }
    .navbar {
      z-index: 1001 !important;
    }
    body {
      font-family: 'Helvetica Neue', Helvetica, Arial, sans-serif;
    }
    .plotly, .js-plotly-plot, .plotly text {
      font-family: 'Helvetica Neue', Helvetica, Arial, sans-serif;
    }
    .shiny-plot-output {
      margin-top: 10px;
    }
    div.tab-pane[data-value='Time Series'] .ts-wrap {
      height: calc(100vh - 150px);
    }
    div.tab-pane[data-value='Yearly Snapshot'] .ts-wrap {
      height: calc(100vh - 150px);
    }
    /* Time Series sidebar - 40% of vertical space */
    div.tab-pane[data-value='Time Series'] .ts-sidebar {
      max-height: 30vh;
      height: 30vh;
      overflow-y: auto;
      background-color: #f8f9fa;
      border-radius: 8px;
      border: 1px solid #dee2e6;
      padding: 15px;
    }
    /* Yearly Snapshot sidebar - keep original full height */
    div.tab-pane[data-value='Yearly Snapshot'] .ts-sidebar {
      max-height: calc(100vh - 100px);
      overflow-y: auto;
      background-color: #f8f9fa;
      border-radius: 8px;
      border: 1px solid #dee2e6;
      padding: 15px;
    }
    /* General ts-sidebar fallback */
    .ts-sidebar {
      max-height: calc(100vh - 100px);
      overflow-y: auto;
      background-color: #f8f9fa;
      border-radius: 8px;
      border: 1px solid #dee2e6;
      padding: 15px;
    }
    div.tab-pane[data-value='Data Explorer'] .data-explorer-main {
      overflow-y: auto !important;
      max-height: 80vh !important;
      padding: 15px;
    }
    #ts_variable, #ys_variable {
      size: 15; /* Show more options in dropdown */
      max-height: 400px;
      overflow-y: auto;
    }
  ")), useShinyjs()
  ),
  
  navbarPage("ISPR Men Statistics", id = "navbar", theme = shinytheme("flatly"),
             
             tabPanel("Time Series",
                      fluidRow(
                        column(
                          width = 3,
                          class = "ts-sidebar",
                          style = "background-color: #f8f9fa; padding: 15px; border-radius: 8px; border: 1px solid #dee2e6;",
                          create_select_input("ts_variable", "Select variable:", time_series_vars, selected = time_series_vars[1]),
                          div(style = "margin-top: 10px;",
                              downloadButton("download_ts_plot", "Download Plot", class = "btn btn-sm btn-primary"),
                              actionButton("reset_ts", "Reset", icon = icon("undo"), class = "btn btn-sm btn-secondary")
                          )
                        ),
                        column(
                          width = 9,
                          div(class = "ts-wrap",
                              fluidRow(
                                column(
                                  8,
                                  htmlOutput("ts_breadcrumb")
                                ),
                                column(
                                  4,
                                  div(style="text-align:right;",
                                      actionButton("ts_back", "Back", icon = icon("arrow-left"), class = "btn btn-sm btn-secondary")
                                  )
                                )
                              ),
                              plotlyOutput("ts_plot", height = "100%")
                          )
                        )
                      )
             ),
             
             # Data Explorer Tab
             tabPanel("Data Explorer",
                      sidebarLayout(
                        sidebarPanel(
                          width = 3,
                          tags$div(
                            style = "background-color: #f8f9fa; border-radius: 8px; padding: 15px; border: 1px solid #dee2e6; font-size: 14px;",
                            create_select_input("explorer_variable", "Select variable:", c("Select a variable..." = "", all_vars)),
                            create_select_input("explorer_year", "Select year:", sort(unique(data$Year))),
                            create_select_input("explorer_category", "Search for a category:", c("Type to search..." = "", final_category_dropdown_choices), selected = "", multiple = FALSE, placeholder = "Type to search..."),
                            checkboxInput("explorer_view_congregation", "View by congregation", value = FALSE),
                            create_download_buttons(),
                            br(),
                            actionButton("reset_table", "Reset Filters", icon = icon("redo"), class = "btn btn-sm btn-secondary")
                          )
                        ),
                        mainPanel(
                          class = "data-explorer-main",
                          width = 9,
                          DTOutput("table"),
                          br()
                        )
                      )
             ),
             
             # Yearly Snapshot Tab
             tabPanel("Yearly Snapshot",
                      fluidRow(
                        column(
                          width = 3,
                          class = "ts-sidebar",
                          style = "background-color: #f8f9fa; padding: 15px; border-radius: 8px; border: 1px solid #dee2e6;",
                          create_select_input("ys_variable", "Select variable:", all_vars, selected = all_vars[1]),
                          create_select_input("ys_year", "Select year:", sort(unique(data$Year))),
                          checkboxInput("ys_view_congregation", "View by congregation", value = FALSE),
                          div(style = "margin-top: 10px;",
                              downloadButton("download_ys_plot", "Download Plot", class = "btn btn-sm btn-primary"),
                              actionButton("reset_ys", "Reset", icon = icon("undo"), class = "btn btn-sm btn-secondary")
                          )
                        ),
                        column(
                          width = 9,
                          div(class = "ts-wrap",
                              # Breadcrumb + Back
                              fluidRow(
                                column(
                                  8,
                                  htmlOutput("ys_breadcrumb")
                                ),
                                column(
                                  4,
                                  div(style="text-align:right;",
                                      actionButton("ys_back", "Back", icon = icon("arrow-left"), class = "btn btn-sm btn-secondary")
                                  )
                                )
                              ),
                              plotlyOutput("ys_plot", height = "100%")
                          )
                        )
                        
                      )
             ),
             
             tabPanel(
               "Credits",
               tags$div(
                 style = "padding: 20px; max-width: 800px; margin: 0 auto; font-size: 16px; line-height: 1.6;",
                 tags$h3("Credits"),
                 tags$p(
                   "The data presented in this web application was extracted using Optical Character Recognition (OCR) by the University Library at the University of Mannheim from the 2022 edition of the ",
                   tags$i("Annuarium Statisticum Ecclesiae."),
                   "The ",
                   tags$i("Annuarium Statisticum Ecclesiae"),
                   " is compiled annually by the Central Office of Church Statistics of the Holy See's Secretariat of State and published by the Vatican Publishing House."
                 ),
                 tags$p(
                   "The data was then transformed and edited by Felicitas Hörl, student assistant for Prof. Dr. Andreas Wollbold. Additional preprocessing steps and development of the web apps were carried out by Claudia Schiavetti and Manuel Soffici. Claudia Schiavetti and Manuel Soffici worked on the project as Master students in Statistics and Data Science at LMU Munich as part of the Consulting Project module."
                 ),
                 tags$p(
                   "The initial idea for the project was developed and supervised by Dr. Anna-Carolina Haensch (Institute of Statistics) and supported by Prof. Dr. Andreas Wollbold and Prof. Dr. Jean-Olivier Nke Ongono (Faculty of Catholic Theology)."
                 ),
                 tags$p(
                   "For further information or inquiries, please contact Dr. Haensch at C.Haensch[at]lmu.de."
                 ),
                 tags$p(
                   "This work is licensed under a ",
                   tags$a(
                     href = "https://creativecommons.org/licenses/by-nc/4.0/",
                     target = "_blank",
                     "Creative Commons Attribution-NonCommercial (CC BY-NC) License."
                   ), 
                 )
               )
             )
  )
)

# ---- Server Logic ----
# ---- DRILLDOWN HIERARCHY ----
# Leaves of the hierarchy (exact strings as they appear in `Categories of Institutes`)
C_RI_ORDERS <- c(
  "Orders - monastic",
  "Orders - canons regular",
  "Orders - mendicant",
  "Orders - clerics regular"
)
C_RI_CONG_CLERICS <- c(
  "Clerical religious congregations"
)
C_RI_CONG_LAY <- c(
  "Lay religious congregations"
)
C_SOCIETIES <- c(
  "Societies of apostolic life (total)"
)
C_RI_TOTAL_ROW <- c("Religious institutes (total)")
C_SOCIETIES_TOTAL_ROW <- c("Societies of apostolic life (total)")
C_ISPR_TOTAL_ROW <- c("ISPRs (total)")

# Helper: a named list for level labels (do NOT change the names L1/L2/L3)
YS_LEVELS <- list(
  L1 = c("Religious institutes", "Societies of apostolic life", "Total"),
  L2_RI = c("Orders", "Congregations of clerics", "Congregations of lay"),
  L3_ORDERS = c("Monastic orders", "Canons regular", "Mendicant orders", "Clerics regular")
)

# ---- STATE FOR DRILLDOWN ----
ys_state <- reactiveValues(
  level = "L1", # "L1" | "L2_RI" | "L3_ORDERS"
  parent = NULL, # used when going down (e.g., "Religious institutes" or "Orders")
  path = character() # breadcrumb path (character vector)
)

# ---- STATE FOR TIME SERIES DRILLDOWN ----
ts_state <- reactiveValues(
  level = "L1", # "L1" | "L2_RI" | "L3_ORDERS"
  parent = NULL, # used when going down (e.g., "Religious institutes" or "Orders")
  path = character() # breadcrumb path (character vector)
)

# Define the server function for the Shiny app.
server <- function(input, output, session) {
  
  # Helper function to wrap text for titles
  wrap_title <- function(text, width = 50, sep = "<br>") {
    paste(strwrap(text, width = width), collapse = sep)
  }
  
  # Sum helper for a set of leaf categories
  sum_for <- function(values, categories, leaves) {
    if (length(leaves) == 0) return(0)
    sum(values[categories %in% leaves], na.rm = TRUE)
  }
  
  # Compute data for current drilldown level (only used when NOT by congregation)
  ys_drill_data <- function(year, variable) {
    # Base frame for the selected year/variable
    base <- data %>%
      filter(Year == year) %>%
      select(`Categories of Institutes`, !!sym(variable)) %>%
      rename(category = `Categories of Institutes`, value = !!sym(variable)) %>%
      filter(!is.na(value))
    
    if (ys_state$level == "L1") {
      # L1: RI, Societies, Total
      # Use total if present; otherwise sum leaves
      ri_sum <- if (any(base$category %in% C_RI_TOTAL_ROW)) {
        sum_for(base$value, base$category, C_RI_TOTAL_ROW)
      } else {
        sum_for(base$value, base$category, c(C_RI_ORDERS, C_RI_CONG_CLERICS, C_RI_CONG_LAY))
      }
      
      so_sum <- if (any(base$category %in% C_SOCIETIES_TOTAL_ROW)) {
        sum_for(base$value, base$category, C_SOCIETIES_TOTAL_ROW)
      } else {
        sum_for(base$value, base$category, C_SOCIETIES)
      }
      
      tot_sum <- ri_sum + so_sum
      
      tibble::tibble(
        label = YS_LEVELS$L1,
        value = c(ri_sum, so_sum, tot_sum)
      )
    } else if (ys_state$level == "L2_RI") {
      tibble::tibble(
        label = YS_LEVELS$L2_RI,
        value = c(
          sum_for(base$value, base$category, C_RI_ORDERS),
          sum_for(base$value, base$category, C_RI_CONG_CLERICS),
          sum_for(base$value, base$category, C_RI_CONG_LAY)
        )
      )
    } else if (ys_state$level == "L3_ORDERS") {
      # Break down the Orders into 4 families
      tibble::tibble(
        label = YS_LEVELS$L3_ORDERS,
        value = c(
          sum_for(base$value, base$category, C_RI_ORDERS[grep("monastic", C_RI_ORDERS, ignore.case = TRUE)]),
          sum_for(base$value, base$category, C_RI_ORDERS[grep("Canons", C_RI_ORDERS, ignore.case = TRUE)]),
          sum_for(base$value, base$category, C_RI_ORDERS[grep("Mendicant", C_RI_ORDERS, ignore.case = TRUE)]),
          sum_for(base$value, base$category, C_RI_ORDERS[grep("Clerics regular", C_RI_ORDERS, ignore.case = TRUE)])
        )
      )
    } else {
      tibble::tibble(label = character(0), value = numeric(0))
    }
  }
  
  # Breadcrumb HTML
  ys_breadcrumb_html <- reactive({
    if (input$ys_view_congregation) return(HTML("&nbsp;"))
    if (length(ys_state$path) == 0) return(HTML("<b>Path:</b> Yearly Snapshot"))
    HTML(paste0("<b>Path:</b> Yearly Snapshot &rsaquo; ", paste(ys_state$path, collapse = " &rsaquo; ")))
  })
  output$ys_breadcrumb <- renderUI(ys_breadcrumb_html())
  
  # Time Series Breadcrumb HTML
  ts_breadcrumb_html <- reactive({
    if (length(ts_state$path) == 0) return(HTML("<b>Path:</b> Time Series"))
    HTML(paste0("<b>Path:</b> Time Series &rsaquo; ", paste(ts_state$path, collapse = " &rsaquo; ")))
  })
  output$ts_breadcrumb <- renderUI(ts_breadcrumb_html())
  
  # ---- Initialize Reactive Values ----
  # Reactive value for selected category and view type.
  selected_category <- reactiveVal(NULL)
  view_by_congregation <- reactiveVal(FALSE)
  
  # Reactive values for synchronizing selections across tabs.
  selections <- reactiveValues(
    variable = NULL,
    year = NULL,
    category = NULL,
    from_tab_var = NULL, # Track which tab triggered the change for variable
    from_tab_year = NULL, # Track which tab triggered the change for year
    from_tab_view = NULL # Track which tab triggered the change for view type
  )
  
  # ---- Time Series Drill-Down Data ----
  ts_drill_data <- reactive({
    req(input$ts_variable)
    base <- data %>%
      filter(!(`Categories of Institutes` %in% congregations)) %>%
      select(`Categories of Institutes`, Year, !!sym(input$ts_variable)) %>%
      rename(category = `Categories of Institutes`, value = !!sym(input$ts_variable)) %>%
      filter(!is.na(value)) %>%
      group_by(Year)
    
    if (ts_state$level == "L1") {
      base %>% summarise(
        `Religious institutes` = if (any(category %in% C_RI_TOTAL_ROW))
          sum_for(value, category, C_RI_TOTAL_ROW)
        else
          sum_for(value, category, c(C_RI_ORDERS, C_RI_CONG_CLERICS, C_RI_CONG_LAY)),
        `Societies of apostolic life` = if (any(category %in% C_SOCIETIES_TOTAL_ROW))
          sum_for(value, category, C_SOCIETIES_TOTAL_ROW)
        else
          sum_for(value, category, C_SOCIETIES),
        Total = `Religious institutes` + `Societies of apostolic life`
      ) %>% ungroup() %>% pivot_longer(-Year, names_to = "label", values_to = "value")
    } else if (ts_state$level == "L2_RI") {
      base %>% summarise(
        Orders = sum_for(value, category, C_RI_ORDERS),
        `Congregations of clerics` = sum_for(value, category, C_RI_CONG_CLERICS),
        `Congregations of lay` = sum_for(value, category, C_RI_CONG_LAY)
      ) %>% ungroup() %>%
        pivot_longer(-Year, names_to = "label", values_to = "value",
                     names_transform = list(label = ~factor(., levels = c("Orders", "Congregations of clerics", "Congregations of lay"))))
    } else if (ts_state$level == "L3_ORDERS") {
      base %>% summarise(
        `Monastic orders` = sum_for(value, category, C_RI_ORDERS[grep("monastic", C_RI_ORDERS, ignore.case = TRUE)]),
        `Canons regular` = sum_for(value, category, C_RI_ORDERS[grep("canons regular", C_RI_ORDERS, ignore.case = TRUE)]),
        `Mendicant orders` = sum_for(value, category, C_RI_ORDERS[grep("mendicant", C_RI_ORDERS, ignore.case = TRUE)]),
        `Clerics regular` = sum_for(value, category, C_RI_ORDERS[grep("clerics regular", C_RI_ORDERS, ignore.case = TRUE)])
      ) %>% ungroup() %>% pivot_longer(-Year, names_to = "label", values_to = "value")
    } else {
      tibble::tibble(Year = integer(0), label = character(0), value = numeric(0))
    }
  })
  
  # ---- Time Series Plot Data ----
  plot_data_reactive <- reactive({
    req(input$ts_variable)
    
    filtered_data <- ts_drill_data() %>%
      rename(category = label)
    
    filtered_data
  })
  
  # Go back one level
  observeEvent(input$ys_back, {
    if (input$ys_view_congregation) return(NULL)
    if (ys_state$level == "L3_ORDERS") {
      ys_state$level <- "L2_RI"
      ys_state$path <- ys_state$path[1:max(0, length(ys_state$path)-1)]
    } else if (ys_state$level == "L2_RI") {
      ys_state$level <- "L1"
      ys_state$path <- character()
    } # L1: no-op
  })
  
  # Time Series Drill-Down Click
  observeEvent(plotly::event_data("plotly_click", source = "ts_drill"), {
    ed <- plotly::event_data("plotly_click", source = "ts_drill")
    if (is.null(ed) || is.null(ed$curveNumber)) return(NULL)
    
    # Get unique labels in the order they appear in the plot
    labels <- unique(plot_data_reactive()$category)
    if (ed$curveNumber + 1 > length(labels)) return(NULL) # Prevent index out of bounds
    clicked <- labels[ed$curveNumber + 1] # curveNumber is 0-based
    
    if (ts_state$level == "L1" && clicked == "Religious institutes") {
      ts_state$level <- "L2_RI"
      ts_state$path <- c("Religious institutes")
    } else if (ts_state$level == "L2_RI" && clicked == "Orders") {
      ts_state$level <- "L3_ORDERS"
      ts_state$path <- c("Religious institutes", "Orders")
    }
  })
  
  # Click to drill down
  observeEvent(plotly::event_data("plotly_click", source = "ys_drill"), {
    if (input$ys_view_congregation) return(NULL)
    ed <- plotly::event_data("plotly_click", source = "ys_drill")
    if (is.null(ed) || is.null(ed$x)) return(NULL)
    clicked <- as.character(ed$x)
    
    if (ys_state$level == "L1") {
      if (clicked == "Religious institutes") {
        ys_state$level <- "L2_RI"
        ys_state$path <- c("Religious institutes")
      }
      # clicking "Societies of apostolic life" or "Total" does not drill further
    } else if (ys_state$level == "L2_RI") {
      if (clicked == "Orders") {
        ys_state$level <- "L3_ORDERS"
        ys_state$path <- c("Religious institutes", "Orders")
      }
      # other L2 items (Congregations…) stop here
    }
  })
  
  # Time Series Back Button
  observeEvent(input$ts_back, {
    if (ts_state$level == "L3_ORDERS") {
      ts_state$level <- "L2_RI"
      ts_state$path <- ts_state$path[1:max(0, length(ts_state$path)-1)]
    } else if (ts_state$level == "L2_RI") {
      ts_state$level <- "L1"
      ts_state$path <- character()
    }
  })
  
  observeEvent(list(input$ys_variable, input$ys_year), {
    if (!input$ys_view_congregation) {
      ys_state$level <- "L1"; ys_state$path <- character()
    }
  })
  
  observeEvent(input$ys_view_congregation, {
    ys_state$level <- "L1"; ys_state$path <- character()
  })
  
  # Force ys_plot_static to invalidate when drill-down state changes
  observeEvent(list(ys_state$level, ys_state$path), {
    # This will cause ys_plot_static() to re-evaluate
  }, ignoreInit = TRUE)
  
  # ---- Render Time Series Plot ----
  output$ts_plot <- renderPlotly({
    plot_data <- plot_data_reactive()
    
    if (nrow(plot_data) == 0) {
      return(
        plot_ly() %>%
          layout(title = "No data available for the selected variable") %>%
          config(displayModeBar = FALSE, responsive = TRUE)
      )
    }
    
    subtitle <- switch(
      ts_state$level,
      "L1" = "Click 'Religious institutes' to drill down",
      "L2_RI" = "Click 'Orders' to drill down",
      "L3_ORDERS" = "Deepest level"
    )
    
    main_title <- paste("Time Series of", input$ts_variable)
    wrapped_title <- wrap_title(main_title, width = 50, sep = "<br>")
    
    full_title_text <- paste0(wrapped_title, "<br><sup>", subtitle, "</sup>")
    num_br <- length(unlist(gregexpr("<br>", full_title_text)))
    num_lines <- num_br + 1
    t_margin <- 30 + 20 * num_lines
    
    plot_ly(
      data = plot_data,
      x = ~Year,
      y = ~value,
      color = ~category,
      colors = viridis::viridis(n = length(unique(plot_data$category))),
      type = "scatter",
      mode = "lines+markers",
      hoverinfo = "text",
      text = ~paste0(
        "<b>", category, "</b><br>",
        "Year: ", Year, "<br>",
        "Value: ", round(value, 2)
      ),
      line = list(width = 2),
      marker = list(size = 6, opacity = 0.8),
      source = "ts_drill"
    ) %>%
      layout(
        title = list(text = full_title_text),
        hovermode = "closest",
        xaxis = list(title = "Year"),
        yaxis = list(title = "Absolute Value"),
        legend = list(title = list(text = "Categories"), x = 1.02, y = 1, xanchor = "left", yanchor = "top"),
        margin = list(r = 150, t = t_margin)
      ) %>%
      event_register('plotly_click') %>%
      config(displayModeBar = FALSE, responsive = TRUE)
  })
  
  # ---- Time Series Download Button ----
  plot_ts_static <- reactive({
    plot_data <- plot_data_reactive()
    
    if (nrow(plot_data) == 0) return(NULL)
    
    main_title <- paste("Time Series of", input$ts_variable)
    wrapped_title <- wrap_title(main_title, width = 50, sep = "\n")
    
    ggplot(plot_data, aes(x = Year, y = value, color = category)) +
      geom_line(linewidth = 1) +
      geom_point(size = 2) +
      scale_color_viridis_d() +
      labs(
        title = wrapped_title,
        x = "Year", y = "Absolute Value", color = "Categories"
      ) +
      theme_minimal(base_size = 13) +
      theme(
        plot.background = element_rect(fill = "white", colour = "white"),
        panel.background = element_rect(fill = "white", colour = "white"),
        legend.position = "bottom"
      )
  })
  
  output$download_ts_plot <- downloadHandler(
    filename = function() {
      paste0("time_series_", input$ts_variable, "_", Sys.Date(), ".png")
    },
    content = function(file) {
      ggsave(file, plot = plot_ts_static(), width = 10, height = 6, dpi = 300)
    }
  )
  
  # ---- Yearly Snapshot Plot Data ----
  ys_plot_data_reactive <- reactive({
    req(input$ys_variable)
    req(input$ys_year)
    
    filtered_data <- data %>%
      filter(Year == input$ys_year) %>%
      select(`Categories of Institutes`, Year, !!sym(input$ys_variable)) %>%
      rename(category = `Categories of Institutes`, value = !!sym(input$ys_variable)) %>%
      filter(!is.na(value))
    
    if (input$ys_view_congregation) {
      filtered_data <- filtered_data %>% filter(category %in% congregations)
    } else {
      filtered_data <- filtered_data %>% filter(!(category %in% congregations))
    }
    
    filtered_data
  })
  
  # ---- Render Yearly Snapshot Histogram ----
  # Generate Plotly bar chart for selected year data.
  output$ys_plot <- renderPlotly({
    req(input$ys_variable, input$ys_year)
    
    # Case A: BY CONGREGATION (keep your original behavior)
    if (input$ys_view_congregation) {
      plot_data <- data %>%
        filter(Year == input$ys_year) %>%
        select(`Categories of Institutes`, Year, !!sym(input$ys_variable)) %>%
        rename(category = `Categories of Institutes`, value = !!sym(input$ys_variable)) %>%
        filter(!is.na(value), category %in% congregations)
      
      if (nrow(plot_data) == 0) {
        return(
          plot_ly() %>%
            layout(title = "No data available for the selected variable and year") %>%
            config(displayModeBar = FALSE, responsive = TRUE)
        )
      }
      
      main_title <- paste("Yearly Snapshot of", input$ys_variable, "in", input$ys_year)
      wrapped_title <- wrap_title(main_title, width = 50, sep = "<br>")
      
      num_br <- length(unlist(gregexpr("<br>", wrapped_title)))
      num_lines <- num_br + 1
      t_margin <- 30 + 20 * num_lines
      
      return(
        plot_ly(
          data = plot_data,
          x = ~category,
          y = ~value,
          type = "bar",
          color = ~category,
          colors = viridis::viridis(length(unique(plot_data$category))),
          text = NULL, # no labels on bars
          hovertemplate = "<b>%{x}</b><br>Value: %{y:,}<extra></extra>"
        ) %>%
          layout(
            title = wrapped_title,
            hovermode = "closest",
            showlegend = FALSE, # legend off (x-axis already tells categories)
            xaxis = list(title = "Categories of Institutes", tickangle = -15),
            yaxis = list(title = "Absolute Value"),
            margin = list(r = 20, t = t_margin, b = 60, l = 60)
          ) %>%
          config(displayModeBar = FALSE, responsive = TRUE)
      )
      
    }
    
    # Case B: NOT BY CONGREGATION ➜ Drilldown bars
    dd <- ys_drill_data(input$ys_year, input$ys_variable)
    if (nrow(dd) == 0) {
      return(plot_ly() %>%
               layout(title="No data available for the selected variable and year") %>%
               config(displayModeBar = FALSE, responsive = TRUE))
    }
    
    subtitle <- switch(
      ys_state$level,
      "L1" = "Click 'Religious institutes' to drill down",
      "L2_RI" = "Click 'Orders' to drill down",
      "L3_ORDERS" = "Deepest level"
    )
    
    main_title <- paste("Yearly Snapshot of", input$ys_variable, "in", input$ys_year)
    wrapped_title <- wrap_title(main_title, width = 50, sep = "<br>")
    
    full_title_text <- paste0(wrapped_title, "<br><sup>", subtitle, "</sup>")
    num_br <- length(unlist(gregexpr("<br>", full_title_text)))
    num_lines <- num_br + 1
    t_margin <- 30 + 20 * num_lines
    
    p <- plot_ly(
      data = dd,
      x = ~label,
      y = ~value,
      type = "bar",
      color = ~label,
      colors = viridis::viridis(length(unique(dd$label))),
      text = NULL,
      hovertemplate = "<b>%{x}</b><br>Value: %{y:,}<extra></extra>",
      source = "ys_drill" # this is the source you listen to
    ) %>%
      layout(
        title = list(text = full_title_text),
        hovermode = "closest",
        showlegend = FALSE,
        xaxis = list(title = "", tickangle = -15),
        yaxis = list(title = "Absolute Value"),
        margin = list(r = 20, t = t_margin, b = 60, l = 60)
      )
    
    p %>%
      config(displayModeBar = FALSE, responsive = TRUE) %>%
      event_register('plotly_click')
    
  })
  
  # ---- Yearly Snapshot Download Button ----
  # Create a static ggplot for downloading the yearly snapshot histogram.
  ys_plot_static <- reactive({
    req(input$ys_variable, input$ys_year)
    
    main_title <- paste("Yearly Snapshot of", input$ys_variable, "in", input$ys_year)
    wrapped_title <- wrap_title(main_title, width = 50, sep = "\n")
    
    if (input$ys_view_congregation) {
      # Mirror the BY CONGREGATION bars
      plot_data <- data %>%
        filter(Year == input$ys_year) %>%
        select(`Categories of Institutes`, Year, !!sym(input$ys_variable)) %>%
        rename(category = `Categories of Institutes`, value = !!sym(input$ys_variable)) %>%
        filter(!is.na(value), category %in% congregations)
      
      if (nrow(plot_data) == 0) return(NULL)
      
      plot_data$category <- factor(plot_data$category, levels = plot_data$category)
      
      return(
        ggplot(plot_data, aes(x = category, y = value, fill = category)) +
          geom_col() +
          scale_fill_viridis_d(guide = "none") +
          labs(
            title = wrapped_title,
            x = "Categories of Institutes", y = "Absolute Value"
          ) +
          theme_minimal(base_size = 13) +
          theme(
            plot.background = element_rect(fill = "white", colour = "white"),
            panel.background = element_rect(fill = "white", colour = "white"),
            axis.text.x = element_text(angle = 15, hjust = 1)
          )
      )
    }
    
    # Use the same drill-down data function that the interactive plot uses
    dd <- ys_drill_data(input$ys_year, input$ys_variable)
    if (nrow(dd) == 0) return(NULL)
    
    # Add drill-down level info to the title
    level_subtitle <- switch(
      ys_state$level,
      "L1" = "",
      "L2_RI" = " - Religious Institutes Breakdown",
      "L3_ORDERS" = " - Orders Breakdown"
    )
    
    full_title <- paste0(wrapped_title, level_subtitle)
    
    dd$label <- factor(dd$label, levels = dd$label)
    
    ggplot(dd, aes(x = label, y = value, fill = label)) +
      geom_col() +
      scale_fill_viridis_d(guide = "none") +
      labs(
        title = full_title,
        x = NULL, y = "Absolute Value"
      ) +
      theme_minimal(base_size = 13) +
      theme(
        plot.background = element_rect(fill = "white", colour = "white"),
        panel.background = element_rect(fill = "white", colour = "white"),
        axis.text.x = element_text(angle = 15, hjust = 1)
      )
  })
  
  output$download_ys_plot <- downloadHandler(
    filename = function() {
      paste0("yearly_snapshot_", input$ys_variable, "_", input$ys_year, "_", Sys.Date(), ".png")
    },
    content = function(file) {
      ggsave(file, plot = ys_plot_static(), width = 10, height = 6, dpi = 300)
    }
  )
  
  # ---- Render Data Table for Explorer Tab ----
  # Display data table for selected variable, year, and optional category.
  output$table <- renderDT({
    if (is.null(input$explorer_variable) || input$explorer_variable == "") {
      return(datatable(data.frame(Message = "Please select a variable to explore.")))
    }
    req(input$explorer_year)
    
    filtered <- data %>%
      filter(Year == input$explorer_year) %>%
      select(`Categories of Institutes`, Year, !!input$explorer_variable)
    
    if (input$explorer_view_congregation) {
      filtered <- filtered %>% filter(`Categories of Institutes` %in% congregations)
    } else {
      filtered <- filtered %>% filter(!(`Categories of Institutes` %in% congregations))
    }
    
    if (!is.null(input$explorer_category) && input$explorer_category != "" && input$explorer_category %in% filtered$`Categories of Institutes`) {
      filtered <- filtered %>% filter(`Categories of Institutes` == input$explorer_category)
    }
    
    datatable(filtered, options = list(pageLength = 20))
  })
  
  # ---- Compute Available Variables for Yearly Snapshot Tab ----
  # Dynamically compute variables that have data for the selected year and view type
  ys_available_vars <- reactive({
    req(input$ys_year)
    if (input$ys_view_congregation) {
      # Variables with non-NA data for congregations in the selected year
      num_cols[
        sapply(num_cols, function(var) {
          any(!is.na(data %>%
                       filter(Year == input$ys_year, `Categories of Institutes` %in% congregations) %>%
                       pull(.data[[var]])))
        })
      ]
    } else {
      # All variables with non-NA data for non-congregations in the selected year
      num_cols[
        sapply(num_cols, function(var) {
          any(!is.na(data %>%
                       filter(Year == input$ys_year, !(`Categories of Institutes` %in% congregations)) %>%
                       pull(.data[[var]])))
        })
      ]
    }
  })
  
  # ---- Update Available Variables for Yearly Snapshot Tab ----
  observeEvent(list(input$ys_view_congregation, input$ys_year), {
    available_vars <- ys_available_vars()
    selected_var <- if (!is.null(selections$variable) && selections$variable %in% available_vars) {
      selections$variable
    } else {
      available_vars[1]
    }
    updateSelectInput(session, "ys_variable", choices = available_vars, selected = selected_var)
  })
  
  # ---- Synchronize Variable Selections ----
  # Update from Time Series tab.
  observeEvent(input$ts_variable, {
    if (is.null(selections$from_tab_var) || selections$from_tab_var != "time_series") {
      selections$variable <- input$ts_variable
      selections$from_tab_var <- "time_series"
      if (input$explorer_view_congregation) {
        if (input$ts_variable %in% congregation_vars) {
          updateSelectInput(session, "explorer_variable", selected = input$ts_variable)
        }
      } else {
        updateSelectInput(session, "explorer_variable", selected = input$ts_variable)
      }
      if (input$ys_view_congregation) {
        if (input$ts_variable %in% ys_available_vars()) {
          updateSelectInput(session, "ys_variable", selected = input$ts_variable)
        }
      } else {
        if (input$ts_variable %in% ys_available_vars()) {
          updateSelectInput(session, "ys_variable", selected = input$ts_variable)
        }
      }
      ts_state$level <- "L1"
      ts_state$path <- character()
    }
  })
  
  # Update from Time Series tab to Data Explorer tab
  observeEvent(input$ts_variable, {
    if (is.null(selections$from_tab) || selections$from_tab != "map") {
      selections$variable <- input$ts_variable
      selections$from_tab <- "time_series"
      updateSelectInput(session, "explorer_variable", selected = input$ts_variable)
      if (input$ts_variable %in% time_series_vars) {
        updateSelectInput(session, "variable", selected = input$ts_variable)
      } else {
        updateSelectInput(session, "variable", selected = time_series_vars[1])
      }
    }
  })
  
  observeEvent(input$ts_variable, {
    ts_state$level <- "L1"
    ts_state$path <- character()
  })
  
  # Update from Data Explorer tab.
  observeEvent(input$explorer_variable, {
    req(input$explorer_variable)
    if (input$explorer_variable != "" && (is.null(selections$from_tab_var) || selections$from_tab_var != "explorer")) {
      selections$variable <- input$explorer_variable
      selections$from_tab_var <- "explorer"
      
      # Update ys_variable if valid
      available_vars <- if (input$ys_view_congregation) ys_available_vars() else all_vars
      if (input$explorer_variable %in% available_vars) {
        updateSelectInput(session, "ys_variable", selected = input$explorer_variable)
      } else {
        updateSelectInput(session, "ys_variable", selected = available_vars[1])
      }
      
      # Update ts_variable if valid
      if (input$explorer_variable %in% time_series_vars) {
        updateSelectInput(session, "ts_variable", selected = input$explorer_variable)
      }
    }
  })
  
  # Update from Yearly Snapshot tab.
  observeEvent(input$ys_variable, {
    if (is.null(selections$from_tab_var) || selections$from_tab_var != "yearly_snapshot") {
      selections$variable <- input$ys_variable
      selections$from_tab_var <- "yearly_snapshot"
      
      # Update explorer_variable if valid
      available_vars <- if (input$explorer_view_congregation) congregation_vars else all_vars
      if (input$ys_variable %in% available_vars) {
        updateSelectInput(session, "explorer_variable", selected = input$ys_variable)
      } else {
        updateSelectInput(session, "explorer_variable", selected = available_vars[1])
      }
      
      # Update ts_variable if valid
      if (input$ys_variable %in% time_series_vars) {
        updateSelectInput(session, "ts_variable", selected = input$ys_variable)
      }
    }
  })
  
  # ---- Synchronize Year Selections ----
  # Update from Data Explorer tab.
  observeEvent(input$explorer_year, {
    if (is.null(selections$from_tab_year) || selections$from_tab_year != "explorer") {
      selections$year <- input$explorer_year
      selections$from_tab_year <- "explorer"
      
      # Update ys_year if valid
      req(input$ys_variable)
      available_ys <- sort(unique(data %>%
                                    filter(!is.na(.data[[input$ys_variable]])) %>%
                                    pull(Year)))
      if (input$explorer_year %in% available_ys) {
        updateSelectInput(session, "ys_year", selected = input$explorer_year)
      } else {
        updateSelectInput(session, "ys_year", selected = max(available_ys))
      }
    }
  })
  
  # Update from Yearly Snapshot tab.
  observeEvent(input$ys_year, {
    if (is.null(selections$from_tab_year) || selections$from_tab_year != "yearly_snapshot") {
      selections$year <- input$ys_year
      selections$from_tab_year <- "yearly_snapshot"
      
      # Update explorer_year if valid
      req(input$explorer_variable)
      req(input$explorer_variable != "")
      available_explorer <- sort(unique(data %>%
                                          filter(!is.na(.data[[input$explorer_variable]])) %>%
                                          pull(Year)))
      if (input$ys_year %in% available_explorer) {
        updateSelectInput(session, "explorer_year", selected = input$ys_year)
      } else {
        updateSelectInput(session, "explorer_year", selected = max(available_explorer))
      }
    }
  })
  
  # ---- Synchronize View Type (Congregation/ISPR) ----
  # Update from Yearly Snapshot tab.
  observeEvent(input$ys_view_congregation, {
    if (is.null(selections$from_tab_view) || selections$from_tab_view != "yearly_snapshot") {
      view_by_congregation(input$ys_view_congregation)
      selections$from_tab_view <- "yearly_snapshot"
      updateCheckboxInput(session, "explorer_view_congregation", value = input$ys_view_congregation)
      
      # Update explorer_variable choices based on congregation view
      available_vars <- if (input$ys_view_congregation) congregation_vars else all_vars
      selected_var <- if (!is.null(selections$variable) && selections$variable %in% available_vars) {
        selections$variable
      } else {
        available_vars[1]
      }
      updateSelectInput(session, "explorer_variable",
                        choices = c("Select a variable..." = "", available_vars),
                        selected = selected_var)
      
      # Update ys_year and explorer_year to 2022 if switching to congregation view
      if (input$ys_view_congregation) {
        selections$year <- "2022"
        selections$from_tab_year <- "yearly_snapshot"
        updateSelectInput(session, "ys_year", selected = "2022")
        updateSelectInput(session, "explorer_year", selected = "2022")
      } else {
        # Restore year if available in explorer_variable
        req(input$explorer_variable)
        req(input$explorer_variable != "")
        available_years <- sort(unique(data %>%
                                         filter(!is.na(.data[[input$explorer_variable]])) %>%
                                         pull(Year)))
        selected_year <- if (!is.null(selections$year) && selections$year %in% available_years) {
          selections$year
        } else {
          max(available_years)
        }
        updateSelectInput(session, "explorer_year", selected = selected_year)
      }
    }
  })
  
  # Update from Data Explorer tab.
  observeEvent(input$explorer_view_congregation, {
    if (is.null(selections$from_tab_view) || selections$from_tab_view != "explorer") {
      view_by_congregation(input$explorer_view_congregation)
      selections$from_tab_view <- "explorer"
      updateCheckboxInput(session, "ys_view_congregation", value = input$explorer_view_congregation)
      
      # Update ys_variable choices based on congregation view
      available_vars <- if (input$explorer_view_congregation) {
        ys_available_vars()
      } else {
        all_vars
      }
      selected_var <- if (!is.null(selections$variable) && selections$variable %in% available_vars) {
        selections$variable
      } else {
        available_vars[1]
      }
      updateSelectInput(session, "ys_variable", choices = available_vars, selected = selected_var)
      
      # Update explorer_variable choices based on congregation view
      available_vars_explorer <- if (input$explorer_view_congregation) congregation_vars else all_vars
      selected_var_explorer <- if (!is.null(selections$variable) && selections$variable %in% available_vars_explorer) {
        selections$variable
      } else {
        available_vars_explorer[1]
      }
      updateSelectInput(session, "explorer_variable",
                        choices = c("Select a variable..." = "", available_vars_explorer),
                        selected = selected_var_explorer)
      
      # Update ys_year and explorer_year to 2022 if switching to congregation view
      if (input$explorer_view_congregation) {
        selections$year <- "2022"
        selections$from_tab_year <- "explorer"
        updateSelectInput(session, "ys_year", selected = "2022")
        updateSelectInput(session, "explorer_year", selected = "2022")
      } else {
        # Restore year if available in ys_variable
        req(input$ys_variable)
        available_years <- sort(unique(data %>%
                                         filter(!is.na(.data[[input$ys_variable]])) %>%
                                         pull(Year)))
        selected_year <- if (!is.null(selections$year) && selections$year %in% available_years) {
          selections$year
        } else {
          max(available_years)
        }
        updateSelectInput(session, "ys_year", selected = selected_year)
      }
    }
  })
  
  # ---- Update Available Variables for Data Explorer Tab ----
  observeEvent(input$explorer_view_congregation, {
    available_vars <- if (input$explorer_view_congregation) {
      congregation_vars
    } else {
      all_vars
    }
    selected_var <- if (!is.null(selections$variable) && selections$variable %in% available_vars) {
      selections$variable
    } else {
      available_vars[1]
    }
    updateSelectInput(session, "explorer_variable",
                      choices = c("Select a variable..." = "", available_vars),
                      selected = selected_var)
  })
  
  # ---- Update Available Years for Explorer Tab ----
  # Dynamically update years based on variable data availability.
  observeEvent(input$explorer_variable, {
    req(input$explorer_variable)
    available_years <- sort(unique(data %>%
                                     filter(!is.na(.data[[input$explorer_variable]])) %>%
                                     pull(Year)))
    selected_year <- if (!is.null(selections$year) && selections$year %in% available_years) {
      selections$year
    } else {
      max(available_years)
    }
    updateSelectInput(session, "explorer_year", choices = available_years, selected = selected_year)
  })
  
  # ---- Update Available Years for Yearly Snapshot Tab ----
  # Dynamically update years based on variable data availability.
  observeEvent(input$ys_variable, {
    req(input$ys_variable)
    available_years <- sort(unique(data %>%
                                     filter(!is.na(.data[[input$ys_variable]])) %>%
                                     pull(Year)))
    selected_year <- if (!is.null(selections$year) && selections$year %in% available_years) {
      selections$year
    } else {
      max(available_years)
    }
    updateSelectInput(session, "ys_year", choices = available_years, selected = selected_year)
  })
  
  # ---- Update Category Choices for Explorer Tab ----
  # Dynamically update categories based on view type.
  observeEvent(input$explorer_view_congregation, {
    available_categories <- if (input$explorer_view_congregation) {
      sort(intersect(congregations, unique(data$`Categories of Institutes`)))
    } else {
      sort(setdiff(unique(data$`Categories of Institutes`), congregations))
    }
    selected_category_val <- if (!is.null(selections$category) && selections$category %in% available_categories) {
      selections$category
    } else {
      ""
    }
    updateSelectizeInput(session, "explorer_category",
                         choices = c("Type to search..." = "", available_categories),
                         selected = selected_category_val)
    selected_category(selected_category_val)
  })
  
  # ---- Handle Category Search Selection ----
  observeEvent(input$explorer_category, {
    req(input$explorer_category)
    selected_category(input$explorer_category)
    selections$category <- input$explorer_category
  })
  
  # ---- Reset Table Filters ----
  observeEvent(input$reset_table, {
    available_vars <- if (view_by_congregation()) congregation_vars else all_vars
    updateSelectInput(session, "explorer_variable", selected = "", choices = c("Select a variable..." = "", available_vars))
    updateSelectInput(session, "explorer_year", selected = max(data$Year))
    updateSelectInput(session, "explorer_category", selected = "")
    updateCheckboxInput(session, "explorer_view_congregation", value = FALSE)
    selected_category(NULL)
    view_by_congregation(FALSE)
    selections$category <- NULL
  })
  
  # ---- Time Series Reset Button ----
  observeEvent(input$reset_ts, {
    updateSelectInput(session, "ts_variable", selected = time_series_vars[1])
    ts_state$level <- "L1"
    ts_state$path <- character()
    shiny::invalidateLater(100, session)
  })
  
  # ---- Yearly Snapshot Reset Button ----
  # Reset yearly snapshot selections to defaults.
  observeEvent(input$reset_ys, {
    available_vars <- if (view_by_congregation()) ys_available_vars() else all_vars
    updateSelectInput(session, "ys_variable", selected = available_vars[1])
    updateSelectInput(session, "ys_year", selected = max(data$Year))
    updateCheckboxInput(session, "ys_view_congregation", value = FALSE)
    view_by_congregation(FALSE)
    shiny::invalidateLater(100, session)
  })
  
  # ---- Download Data as CSV ----
  output$download_csv <- downloadHandler(
    filename = function() {
      paste0("data_explorer_", input$explorer_variable, "_", input$explorer_year, ".csv")
    },
    content = function(file) {
      req(input$explorer_variable, input$explorer_year)
      write.csv(create_download_data(data, input$explorer_year, input$explorer_variable, selected_category(), input$explorer_view_congregation), file, row.names = FALSE)
    }
  )
  
  # ---- Download Data as Excel ----
  output$download_excel <- downloadHandler(
    filename = function() {
      paste0("data_explorer_", input$explorer_variable, "_", input$explorer_year, ".xlsx")
    },
    content = function(file) {
      req(input$explorer_variable, input$explorer_year)
      writexl::write_xlsx(create_download_data(data, input$explorer_year, input$explorer_variable, selected_category(), input$explorer_view_congregation), path = file)
    }
  )
}

# ---- Launch the Shiny App ----
shinyApp(ui, server)
