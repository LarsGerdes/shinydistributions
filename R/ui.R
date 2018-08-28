#### UI ####
ui <- fluidPage(
  titlePanel(
    title = "Fit Distributions to Your Data",
    windowTitle = "Shiny.Distributions"
  ),
  tabsetPanel(
    #### Tab: Plot Distribution ####
    tabPanel(
      title = "Plot Distribution",
      # Sidebar layout with input and output definitions
      sidebarLayout(
        sidebarPanel = sidebarPanel(
          # Output: Select variable
          uiOutput(outputId = "variable"),
          # Input: Select plot type
          # selectInput(
          #   inputId = "plot_type",
          #   label = "Plot Type",
          #   choices = list("Histogram", "Empiric Density Curve")
          #),
          # Output: Select plot type
          uiOutput(outputId = "plot_type"),
          # Output: Bins for Histogram
          uiOutput(outputId = "num_bins"),
          # Function type
          radioButtons(
            inputId = "fun_type",
            label = "Function Type",
            choices = c("all", "continuous", "discrete", "mixed"),
            selected = "all",
            inline = TRUE
          ),
          # Number of parameters
          radioButtons(
            inputId = "parameters",
            label = "Number of Parameters",
            choices = c("all", 1, 2, 3, 4),
            selected = "all",
            inline = TRUE
          ),
          # Input: Select density function
          selectInput(
            inputId = "dist",
            label = "Density Function",
            choices = distributions$dist_density
          ),
          uiOutput(outputId = "num_location"),
          uiOutput(outputId = "num_scale"),
          uiOutput(outputId = "num_skewness"),
          uiOutput(outputId = "num_kurtosis"),
          uiOutput(outputId = "num_binomial_denominators"),
          # Horizontal line
          tags$hr(),
          uiOutput(outputId = "num_max_y"),
          uiOutput(outputId = "num_max_x"),
          uiOutput(outputId = "num_min_x")
        ),
        mainPanel = mainPanel(
          h3(textOutput(outputId = "caption")),
          uiOutput(outputId = "plot"),
          br(),
          br(),
          h4("Default Parameter(s) of Theoretic Density Function:"),
          tags$b(textOutput(outputId = "info_text"), style = "font-size:120%"),
          br(),
          h4(textOutput(outputId = "head_ml_estimates")),
          tags$b(textOutput(outputId = "info_text2"),
                 style = "font-size:120%")
        )
      )
    ),
    #### Tab: Data upload ####
    tabPanel(
      title = "Data Upload",
      # Sidebar layout with input and output definitions
      sidebarLayout(
        sidebarPanel = sidebarPanel(
          width = 2,
          # Input: Select a file
          fileInput(
            inputId = "dataset",
            label = "Choose CSV-file",
            multiple = TRUE,
            accept = c("text/csv",
                       "text/comma-separated-values,text/plain", ".csv")
          ),
          actionButton('reset', 'Reset Input'),
          #uiOutput(outputId = "stop_mle"),
          # Horizontal line
          tags$hr(),
          # Input: Checkbox if file has header
          checkboxInput(
            inputId = "header",
            label = "Header",
            value = TRUE
          ),
          # Input: Select separator
          radioButtons(
            inputId = "sep",
            label = "Seperator",
            choices = c(Comma = ",", Semicolon = ";", Tab = "\t"),
            selected = ","
          ),
          # Input: Select quotes
          radioButtons(
            inputId = "quote",
            label = "Quote",
            choices = c(None = "", "Double Quote" = '"', "Single Quote" = "'"),
            selected = '"'
          ),
          # Horizontal line
          tags$hr(),
          # Input: Select number of rows to display
          #radioButtons(
          #  inputId = "display",
          #  label = "Display",
          #  choices = c(Head = "head", All = "all"),
          #  selected = "head"
          #),
          #Input: Select data table or summary of data set
          radioButtons(
            inputId = "display",
            label = "Display",
            choices = c("Data Frame", "Summary"),
            selected = "Data Frame"
          ),
          tags$hr(),
          # R-Datasets
          selectInput(
            inputId = "r_dataset",
            label = "Choose a R-dataset:",
            choices = c("None", "iris", "diamonds", "mtcars")
          )
        ),
        mainPanel = mainPanel(
          DT::dataTableOutput(outputId = "contents"),
          #tableOutput(outputId = "contents"),
          verbatimTextOutput(outputId = "summary")
        )
      )
    ),
    #### Tab: Properties of Distributions ####
    tabPanel(
      title = "Properties of Distributions",
      DT::dataTableOutput(outputId = "distributions_tab")
    )
  ))
