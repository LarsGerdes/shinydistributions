#### UI ####
ui <- shiny::fluidPage(
  shiny::titlePanel(
    title = "Fit Distributions to Your Data",
    windowTitle = "Shiny.Distributions"
  ),
  shiny::tabsetPanel(
    #### Tab: Plot Distribution ####
    shiny::tabPanel(
      title = "Plot Distribution",
      # Sidebar layout with input and output definitions
      shiny::sidebarLayout(
        sidebarPanel = shiny::sidebarPanel(
          # Output: Select variable
          shiny::uiOutput(outputId = "variable"),
          # Input: Select plot type
          # selectInput(
          #   inputId = "plot_type",
          #   label = "Plot Type",
          #   choices = list("Histogram", "Empiric Density Curve")
          #),
          # Output: Select plot type
          shiny::uiOutput(outputId = "plot_type"),
          # Output: Bins for Histogram
          shiny::uiOutput(outputId = "num_bins"),
          # Function type
          shiny::radioButtons(
            inputId = "fun_type",
            label = "Function Type",
            choices = c("all", "continuous", "discrete", "mixed"),
            selected = "all",
            inline = TRUE
          ),
          # Number of parameters
          shiny::radioButtons(
            inputId = "parameters",
            label = "Number of Parameters",
            choices = c("all", 1, 2, 3, 4),
            selected = "all",
            inline = TRUE
          ),
          # Input: Select density function
          shiny::selectInput(
            inputId = "dist",
            label = "Density Function",
            choices = distributions$dist_density
          ),
          shiny::uiOutput(outputId = "num_location"),
          shiny::uiOutput(outputId = "num_scale"),
          shiny::uiOutput(outputId = "num_skewness"),
          shiny::uiOutput(outputId = "num_kurtosis"),
          shiny::uiOutput(outputId = "num_binomial_denominators"),
          # Horizontal line
          shiny::tags$hr(),
          shiny::uiOutput(outputId = "num_max_y"),
          shiny::uiOutput(outputId = "num_max_x"),
          shiny::uiOutput(outputId = "num_min_x")
        ),
        mainPanel = shiny::mainPanel(
          shiny::h3(shiny::textOutput(outputId = "caption")),
          shiny::uiOutput(outputId = "plot"),
          shiny::br(),
          shiny::br(),
          shiny::h4("Default Parameter(s) of Theoretic Density Function:"),
          shiny::tags$b(shiny::textOutput(outputId = "info_text"),
                        style = "font-size:120%"),
          shiny::br(),
          shiny::h4(shiny::textOutput(outputId = "head_ml_estimates")),
          shiny::tags$b(shiny::textOutput(outputId = "info_text2"),
                 style = "font-size:120%")
        )
      )
    ),
    #### Tab: Data upload ####
    shiny::tabPanel(
      title = "Data Upload",
      # Sidebar layout with input and output definitions
      shiny::sidebarLayout(
        sidebarPanel = shiny::sidebarPanel(
          width = 2,
          # Input: Select a file
          shiny::fileInput(
            inputId = "dataset",
            label = "Choose CSV-file",
            multiple = TRUE,
            accept = c("text/csv",
                       "text/comma-separated-values,text/plain", ".csv")
          ),
          shiny::actionButton('reset', 'Reset Input'),
          #uiOutput(outputId = "stop_mle"),
          # Horizontal line
          shiny::tags$hr(),
          # Input: Checkbox if file has header
          shiny::checkboxInput(
            inputId = "header",
            label = "Header",
            value = TRUE
          ),
          # Input: Select separator
          shiny::radioButtons(
            inputId = "sep",
            label = "Seperator",
            choices = c(Comma = ",", Semicolon = ";", Tab = "\t"),
            selected = ","
          ),
          # Input: Select quotes
          shiny::radioButtons(
            inputId = "quote",
            label = "Quote",
            choices = c(None = "", "Double Quote" = '"', "Single Quote" = "'"),
            selected = '"'
          ),
          # Horizontal line
          shiny::tags$hr(),
          # Input: Select number of rows to display
          #radioButtons(
          #  inputId = "display",
          #  label = "Display",
          #  choices = c(Head = "head", All = "all"),
          #  selected = "head"
          #),
          #Input: Select data table or summary of data set
          shiny::radioButtons(
            inputId = "display",
            label = "Display",
            choices = c("Data Frame", "Summary"),
            selected = "Data Frame"
          ),
          shiny::tags$hr(),
          # R-Datasets
          shiny::selectInput(
            inputId = "r_dataset",
            label = "Choose a R-dataset:",
            choices = c("None", "iris", "diamonds", "mtcars")
          )
        ),
        mainPanel = shiny::mainPanel(
          DT::dataTableOutput(outputId = "contents"),
          #tableOutput(outputId = "contents"),
          shiny::verbatimTextOutput(outputId = "summary")
        )
      )
    ),
    #### Tab: Properties of Distributions ####
    shiny::tabPanel(
      title = "Properties of Distributions",
      DT::dataTableOutput(outputId = "distributions_tab")
    )
  )
)
