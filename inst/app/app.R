#' UI object for the shiny app.
#'

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
            choices = shinydistributions:::distributions$dist_density
          ),
          uiOutput(outputId = "num_location"),
          uiOutput(outputId = "num_scale"),
          uiOutput(outputId = "num_skewness"),
          uiOutput(outputId = "num_kurtosis"),
          uiOutput(outputId = "num_binomial_denominator"),
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
          h4(textOutput(outputId = "head_default_parameters")),
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
          verbatimTextOutput(outputId = "summary")
        )
      )
    ),
    #### Tab: Properties of Distributions ####
    tabPanel(
      title = "Properties of Distributions",
      DT::dataTableOutput(outputId = "distributions_tab")
    )
  )
)

#' Server function for the shiny app.
#'
#' @param input from the ui object
#' @param output for the ui object
#' @param session for observe object
#'
#'

# server function for shiny app
server <- function(input, output, session) {

  #### Tab: Data upload ####
  # input$dataset will be NULL initially. After the user selects
  # and uploads a file, head of that data file by default,
  # or summary if selected, will be shown.

  # Increase maximum upload size from 5 MB to 30 MB
  #old <- options(shiny.maxRequestSize = 5*1024^2)
  options(shiny.maxRequestSize = 30*1024^2)

  # Upload data and reset-button to switch between upload and R data sets
  values <- reactiveValues(upload_state = NULL)

  observeEvent(input$dataset, {
    values$upload_state <- 'uploaded'
  })
  observeEvent(input$reset, {
    values$upload_state <- 'reset'
  })
  datasetInput <- reactive({
    if (is.null(values$upload_state)){
      switch (input$r_dataset,
              "iris" = iris,
              "diamonds" = ggplot2::diamonds,
              "mtcars" = mtcars,
              "None" = return()
      )
    } else if (values$upload_state == 'uploaded') {
      read.csv(file = input$dataset$datapath, header = input$header,
               sep = input$sep, quote = input$quote)
      # Set maximum upload size back to the initial value after upload
      #on.exit(expr = options(old))
    } else if (values$upload_state == 'reset') {
      switch (input$r_dataset,
              "iris" = iris,
              "diamonds" = ggplot2::diamonds,
              "mtcars" = mtcars,
              "None" = return()
      )
    }
  })
  # Display data frame or summary of data
  output$contents <- DT::renderDataTable({

    if (is.null(datasetInput())) {
      return()
    } else {

      if (input$display == "Data Frame") {
        # Selected number of records shown per page (range to chose from dependent
        # on data set size)
        page_length <-  if (nrow(datasetInput()) > 100){
          c(10, 15, 20, 50, 100, nrow(datasetInput()) )
          } else if (nrow(datasetInput()) <= 10) {
            nrow(datasetInput())
          } else if (nrow(datasetInput()) <= 15) {
            c(10, nrow(datasetInput()) )
          } else if (nrow(datasetInput()) <= 20) {
            c(10, 15, nrow(datasetInput()) )
          } else if (nrow(datasetInput()) <= 50) {
            c(10, 15, 20, nrow(datasetInput()) )
          } else if (nrow(datasetInput()) <= 100) {
            c(10, 15, 20, 50, nrow(datasetInput()) )
          }
        DT::datatable(datasetInput(), rownames = FALSE,
                      options = list(
                        lengthMenu = page_length,
                        pageLength = 15
                      )
        )
      }
    }
  })
  # Summary
  output$summary <- renderPrint({
    if (input$display == "Summary") {
      summary(object = datasetInput())
    }
  })
  #### Tab: Properties of Distributions ####
  output$distributions_tab <- DT::renderDataTable({
    DT::datatable(shinydistributions:::distributions_tab, rownames = FALSE,
                  options = list(lengthMenu = c(10, 15, 20, 50, 109),
                                 pageLength = 10
                                 )
    )
  })
  #### Tab: Plot Distribution ####
  # Update variable selection based on uploaded data set
  output$variable <- renderUI({
    if (is.null(datasetInput())) {
      return()
    } else {
      selectInput(
        inputId = "variable",
        label = "Variable",
        choices = colnames(datasetInput())
      )
    }
  })
  # Update no of parameters based on fun_type
  observe({
    if (input$fun_type != "mixed") {
      choices <- c("all", 1, 2, 3, 4)
      selected <- "all"
    } else {
      choices <- c("all", 3, 4)
      selected <- "all"
    }
    updateRadioButtons(
      session = session,
      inputId = "parameters",
      label = "No of Parameters",
      choices = choices,
      selected = selected,
      inline = TRUE
    )
  })
  # Update density function based on selected type and parameters
  observe({
    all <- shinydistributions:::distributions$dist_density
    continuous <- shinydistributions:::distributions[
      shinydistributions:::distributions$dist_type == "continuous",
      "dist_density"]
    discrete <- shinydistributions:::distributions[
      shinydistributions:::distributions$dist_type == "discrete",
      "dist_density"]
    mixed <- shinydistributions:::distributions[
      shinydistributions:::distributions$dist_type == "mixed", "dist_density"]
    one <- shinydistributions:::distributions[
      shinydistributions:::distributions$no_of_parameters == 1, "dist_density"]
    two <- shinydistributions:::distributions[
      shinydistributions:::distributions$no_of_parameters == 2, "dist_density"]
    three <- shinydistributions:::distributions[
      shinydistributions:::distributions$no_of_parameters == 3, "dist_density"]
    four <- shinydistributions:::distributions[
      shinydistributions:::distributions$no_of_parameters == 4, "dist_density"]
    choices <- shinydistributions:::distributions$dist_density
    if (input$fun_type == "mixed") {
      choices.type <- mixed
      if (input$parameters == 4) {
        choices.parameters <- four
        selected <- "Beta inflated - dBEINF"
      }
      if (input$parameters == 3) {
        choices.parameters <- three
        selected <- "Beta inflated - dBEINF0"
      }
      if (input$parameters == 2) {
        choices.parameters <- two
        selected <- "Zero adjusted IG - dZAIG"
      }
      if (input$parameters == 1) {
        choices.parameters <- one
        selected <- NULL
      }
      if (input$parameters == "all") {
        choices.parameters <- all
        selected <- "Beta inflated - dBEINF"
      }
    }
    if (input$fun_type == "discrete") {
      choices.type <- discrete
      if (input$parameters == 4) {
        choices.parameters <- four
        selected <- "Zero adjusted Sichel - dZASICHEL"
      }
      if (input$parameters == 3) {
        choices.parameters <- three
        selected <- "Negative Binomial family - dNBF"
      }
      if (input$parameters == 2) {
        choices.parameters <- two
        selected <- "Double Poisson - dDPO"
      }
      if (input$parameters == 1) {
        choices.parameters <- one
        selected <- "Poison - dPO"
      }
      if (input$parameters == "all") {
        choices.parameters <- all
        selected <- "Poison - dPO"
      }
    }
    if (input$fun_type == "continuous") {
      choices.type <- continuous
      if (input$parameters == 4) {
        choices.parameters <- four
        selected <- "Box-Cox Power Exponential - dBCPE"
      }
      if (input$parameters == 3) {
        choices.parameters <- three
        selected <- "t-distribution - dTF"
      }
      if (input$parameters == 2) {
        choices.parameters <- two
        selected <- "Normal (mean, var) - dNO2"

      }
      if (input$parameters == 1) {
        choices.parameters <- one
        selected <- "Exponential - dEXP"
      }
      if (input$parameters == "all") {
        choices.parameters <- all
        selected <- "Normal (mean, var) - dNO2"
      }
    }
    if (input$fun_type == "all") {
      choices.type <- all
      if (input$parameters == 4) {
        choices.parameters <- four
        selected <- "Box-Cox Power Exponential - dBCPE"
      }
      if (input$parameters == 3) {
        choices.parameters <- three
        selected <- "t-distribution - dTF"
      }
      if (input$parameters == 2) {
        choices.parameters <- two
        selected <- "Normal (mean, var) - dNO2"
      }
      if (input$parameters == 1) {
        choices.parameters <- one
        selected <- "Exponential - dEXP"
      }
      if (input$parameters == "all") {
        choices.parameters <- all
        selected <- "Normal (mean, var) - dNO2"
      }
    }
    choices <- choices.parameters[choices.parameters %in% choices.type]
    updateSelectInput(
      session = session,
      inputId = "dist",
      label = "Density Function",
      choices = choices,
      selected = selected
    )
  })

  # Density functions for dynamic density plot
  # To improve performance we created the default functions in reactive section
  # below beforehand in a separate script. Thereby the relatively large number
  # of necessary if-else statements doesn't need to run every single time a user
  # selects a different function from the drop down menu.The functions contain 1
  # to 4 paramters only (location / scale / skewness / kurtosis). Binomial
  # distributions include parameter "bd" (binomial denominator) additionally.
  d_funct <- reactive({
    dist <- switch(
      EXPR = input$dist,
      "Beta Binomial - dBB" = function(y) gamlss.dist::dBB(y, mu = input$location, sigma = input$scale, bd = input$binomial_denominator),
      "Box-Cox Cole and Green - dBCCG" = function(y) gamlss.dist::dBCCG(y, mu = input$location, sigma = input$scale, nu = input$skewness),
      "Box-Cox Cole and Green - dBCCGo" = function(y) gamlss.dist::dBCCGo(y, mu = input$location, sigma = input$scale, nu = input$skewness),
      "Box-Cox Power Exponential - dBCPE" = function(y) gamlss.dist::dBCPE(y, mu = input$location, sigma = input$scale, nu = input$skewness, tau = input$kurtosis),
      "Box-Cox Power Exponential - dBCPEo" = function(y) gamlss.dist::dBCPEo(y, mu = input$location, sigma = input$scale, nu = input$skewness, tau = input$kurtosis),
      "Box-Cox-t - dBCT" = function(y) gamlss.dist::dBCT(y, mu = input$location, sigma = input$scale, nu = input$skewness, tau = input$kurtosis),
      "Box-Cox-t - dBCTo" = function(y) gamlss.dist::dBCTo(y, mu = input$location, sigma = input$scale, nu = input$skewness, tau = input$kurtosis),
      "Beta - dBE" = function(y) gamlss.dist::dBE(y, mu = input$location, sigma = input$scale),
      "Beta inflated - dBEINF" = function(y) gamlss.dist::dBEINF(y, mu = input$location, sigma = input$scale, nu = input$skewness, tau = input$kurtosis),
      "Beta inflated - dBEINF0" = function(y) gamlss.dist::dBEINF0(y, mu = input$location, sigma = input$scale, nu = input$skewness),
      "Beta inflated - dBEINF1" = function(y) gamlss.dist::dBEINF1(y, mu = input$location, sigma = input$scale, nu = input$skewness),
      "Beta - dBEo" = function(y) gamlss.dist::dBEo(y, mu = input$location, sigma = input$scale),
      "Beta one inflated - dBEOI" = function(y) gamlss.dist::dBEOI(y, mu = input$location, sigma = input$scale, nu = input$skewness),
      "Beta zero inflated - dBEZI" = function(y) gamlss.dist::dBEZI(y, mu = input$location, sigma = input$scale, nu = input$skewness),
      "Binomial - dBI" = function(y) gamlss.dist::dBI(y, mu = input$location, bd = input$binomial_denominator),
      "Beta negative binomial - dBNB" = function(y) gamlss.dist::dBNB(y, mu = input$location, sigma = input$scale, nu = input$skewness),
      "Double binomial - dDBI" = function(y) gamlss.dist::dDBI(y, mu = input$location, sigma = input$scale, bd = input$binomial_denominator),
      "Delaport - dDEL" = function(y) gamlss.dist::dDEL(y, mu = input$location, sigma = input$scale, nu = input$skewness),
      "Double Poisson - dDPO" = function(y) gamlss.dist::dDPO(y, mu = input$location, sigma = input$scale),
      "Exponential generalized Beta type 2 - dEGB2" = function(y) gamlss.dist::dEGB2(y, mu = input$location, sigma = input$scale, nu = input$skewness, tau = input$kurtosis),
      "Exponential Gaussian - dexGAUS" = function(y) gamlss.dist::dexGAUS(y, mu = input$location, sigma = input$scale, nu = input$skewness),
      "Exponential - dEXP" = function(y) gamlss.dist::dEXP(y, mu = input$location),
      "Gamma - dGA" = function(y) gamlss.dist::dGA(y, mu = input$location, sigma = input$scale),
      "Generalized Beta type 1 - dGB1" = function(y) gamlss.dist::dGB1(y, mu = input$location, sigma = input$scale, nu = input$skewness, tau = input$kurtosis),
      "Generalized Beta type 2 - dGB2" = function(y) gamlss.dist::dGB2(y, mu = input$location, sigma = input$scale, nu = input$skewness, tau = input$kurtosis),
      "Geometric - dGEOM" = function(y) gamlss.dist::dGEOM(y, mu = input$location),
      "Geometric (original) - dGEOMo" = function(y) gamlss.dist::dGEOMo(y, mu = input$location),
      "Generalized Gamma - dGG" = function(y) gamlss.dist::dGG(y, mu = input$location, sigma = input$scale, nu = input$skewness),
      "Generalized Inverse Gaussian - dGIG" = function(y) gamlss.dist::dGIG(y, mu = input$location, sigma = input$scale, nu = input$skewness),
      "Generalised Pareto - dGP" = function(y) gamlss.dist::dGP(y, mu = input$location, sigma = input$scale),
      "Generalised Pareto - dGPO" = function(y) gamlss.dist::dGPO(y, mu = input$location, sigma = input$scale),
      "Generalized t - dGT" = function(y) gamlss.dist::dGT(y, mu = input$location, sigma = input$scale, nu = input$skewness, tau = input$kurtosis),
      "Gumbel - dGU" = function(y) gamlss.dist::dGU(y, mu = input$location, sigma = input$scale),
      "Inverse Gaussian - dIG" = function(y) gamlss.dist::dIG(y, mu = input$location, sigma = input$scale),
      "Inverse Gamma - dIGAMMA" = function(y) gamlss.dist::dIGAMMA(y, mu = input$location, sigma = input$scale),
      "Johnson's SU - dJSU" = function(y) gamlss.dist::dJSU(y, mu = input$location, sigma = input$scale, nu = input$skewness, tau = input$kurtosis),
      "Johnson's SU - dJSUo" = function(y) gamlss.dist::dJSUo(y, mu = input$location, sigma = input$scale, nu = input$skewness, tau = input$kurtosis),
      "Logarithmic - dLG" = function(y) gamlss.dist::dLG(y, mu = input$location),
      "log-Normal (Box-Cox) - dLNO" = function(y) gamlss.dist::dLNO(y, mu = input$location, sigma = input$scale, nu = input$skewness),
      "Logistic - dLO" = function(y) gamlss.dist::dLO(y, mu = input$location, sigma = input$scale),
      "Logit Normal - dLOGITNO" = function(y) gamlss.dist::dLOGITNO(y, mu = input$location, sigma = input$scale),
      "log-Normal - dLOGNO" = function(y) gamlss.dist::dLOGNO(y, mu = input$location, sigma = input$scale),
      "log-Normal - dLOGNO2" = function(y) gamlss.dist::dLOGNO2(y, mu = input$location, sigma = input$scale),
      "Normal Linear Quadratic - dLQNO" = function(y) gamlss.dist::dLQNO(y, mu = input$location, sigma = input$scale),
      "Multinomial - dMN3" = function(y) gamlss.dist::dMN3(y, mu = input$location, sigma = input$scale),
      "Multinomial - dMN4" = function(y) gamlss.dist::dMN4(y, mu = input$location, sigma = input$scale, nu = input$skewness),
      "Multinomial - dMN5" = function(y) gamlss.dist::dMN5(y, mu = input$location, sigma = input$scale, nu = input$skewness, tau = input$kurtosis),
      "Negative Binomial family - dNBF" = function(y) gamlss.dist::dNBF(y, mu = input$location, sigma = input$scale, nu = input$skewness),
      "Negative Binomial type I - dNBI" = function(y) gamlss.dist::dNBI(y, mu = input$location, sigma = input$scale),
      "Negative Binomial type II - dNBII" = function(y) gamlss.dist::dNBII(y, mu = input$location, sigma = input$scale),
      "Normal Exponential t - dNET" = function(y) gamlss.dist::dNET(y, mu = input$location, sigma = input$scale, nu = input$skewness, tau = input$kurtosis),
      "Normal (mean, sd) - dNO" = function(y) gamlss.dist::dNO(y, mu = input$location, sigma = input$scale),
      "Normal (mean, var) - dNO2" = function(y) gamlss.dist::dNO2(y, mu = input$location, sigma = input$scale),
      "Normal Family - dNOF" = function(y) gamlss.dist::dNOF(y, mu = input$location, sigma = input$scale, nu = input$skewness),
      "Pareto type 2 - dPARETO2" = function(y) gamlss.dist::dPARETO2(y, mu = input$location, sigma = input$scale),
      "Pareto type 2 original - dPARETO2o" = function(y) gamlss.dist::dPARETO2o(y, mu = input$location, sigma = input$scale),
      "Power Exponential - dPE" = function(y) gamlss.dist::dPE(y, mu = input$location, sigma = input$scale, nu = input$skewness),
      "Power Exponential type 2 - dPE2" = function(y) gamlss.dist::dPE2(y, mu = input$location, sigma = input$scale, nu = input$skewness),
      "Poisson inverse Gaussian - dPIG" = function(y) gamlss.dist::dPIG(y, mu = input$location, sigma = input$scale),
      "Poison - dPO" = function(y) gamlss.dist::dPO(y, mu = input$location),
      "Reverse Gumbel - dRG" = function(y) gamlss.dist::dRG(y, mu = input$location, sigma = input$scale),
      "Reverse generalized extreme - dRGE" = function(y) gamlss.dist::dRGE(y, mu = input$location, sigma = input$scale, nu = input$skewness),
      "Skew Power Exponential - dSEP" = function(y) gamlss.dist::dSEP(y, mu = input$location, sigma = input$scale, nu = input$skewness, tau = input$kurtosis),
      "Skew Power Exponential type 1 - dSEP1" = function(y) gamlss.dist::dSEP1(y, mu = input$location, sigma = input$scale, nu = input$skewness, tau = input$kurtosis),
      "Skew Power Exponential type 2 - dSEP2" = function(y) gamlss.dist::dSEP2(y, mu = input$location, sigma = input$scale, nu = input$skewness, tau = input$kurtosis),
      "Skew Power Exponential type 3 - dSEP3" = function(y) gamlss.dist::dSEP3(y, mu = input$location, sigma = input$scale, nu = input$skewness, tau = input$kurtosis),
      "Skew Power Exponential type 4 - dSEP4" = function(y) gamlss.dist::dSEP4(y, mu = input$location, sigma = input$scale, nu = input$skewness, tau = input$kurtosis),
      "Shash - dSHASH" = function(y) gamlss.dist::dSHASH(y, mu = input$location, sigma = input$scale, nu = input$skewness, tau = input$kurtosis),
      "Shash original - dSHASHo" = function(y) gamlss.dist::dSHASHo(y, mu = input$location, sigma = input$scale, nu = input$skewness, tau = input$kurtosis),
      "Shash original 2 - dSHASHo2" = function(y) gamlss.dist::dSHASHo2(y, mu = input$location, sigma = input$scale, nu = input$skewness, tau = input$kurtosis),
      "Sichel (original) - dSI" = function(y) gamlss.dist::dSI(y, mu = input$location, sigma = input$scale, nu = input$skewness),
      "Sichel (mu as the mean) - dSICHEL" = function(y) gamlss.dist::dSICHEL(y, mu = input$location, sigma = input$scale, nu = input$skewness),
      "Skew Normal Type 1 - dSN1" = function(y) gamlss.dist::dSN1(y, mu = input$location, sigma = input$scale, nu = input$skewness),
      "Skew Normal Type 2 - dSN2" = function(y) gamlss.dist::dSN2(y, mu = input$location, sigma = input$scale, nu = input$skewness),
      "Skew t - dSST" = function(y) gamlss.dist::dSST(y, mu = input$location, sigma = input$scale, nu = input$skewness, tau = input$kurtosis),
      "Skew t type 1 - dST1" = function(y) gamlss.dist::dST1(y, mu = input$location, sigma = input$scale, nu = input$skewness, tau = input$kurtosis),
      "Skew t type 2 - dST2" = function(y) gamlss.dist::dST2(y, mu = input$location, sigma = input$scale, nu = input$skewness, tau = input$kurtosis),
      "Skew t type 3 - dST3" = function(y) gamlss.dist::dST3(y, mu = input$location, sigma = input$scale, nu = input$skewness, tau = input$kurtosis),
      "Skew t - dST3C" = function(y) gamlss.dist::dST3C(y, mu = input$location, sigma = input$scale, nu = input$skewness, tau = input$kurtosis),
      "Skew t type 4 - dST4" = function(y) gamlss.dist::dST4(y, mu = input$location, sigma = input$scale, nu = input$skewness, tau = input$kurtosis),
      "Skew t type 5 - dST5" = function(y) gamlss.dist::dST5(y, mu = input$location, sigma = input$scale, nu = input$skewness, tau = input$kurtosis),
      "t-distribution - dTF" = function(y) gamlss.dist::dTF(y, mu = input$location, sigma = input$scale, nu = input$skewness),
      "t-distribution - dTF2" = function(y) gamlss.dist::dTF2(y, mu = input$location, sigma = input$scale, nu = input$skewness),
      "Waring - dWARING" = function(y) gamlss.dist::dWARING(y, mu = input$location, sigma = input$scale),
      "Weibull - dWEI" = function(y) gamlss.dist::dWEI(y, mu = input$location, sigma = input$scale),
      "Weibull(PH parameterization) - dWEI2" = function(y) gamlss.dist::dWEI2(y, mu = input$location, sigma = input$scale),
      "Weibull (mu as mean) - dWEI3" = function(y) gamlss.dist::dWEI3(y, mu = input$location, sigma = input$scale),
      "Yule - dYULE" = function(y) gamlss.dist::dYULE(y, mu = input$location),
      "Zero adjusted beta binomial - dZABB" = function(y) gamlss.dist::dZABB(y, mu = input$location, sigma = input$scale, nu = input$skewness, bd = input$binomial_denominator),
      "Zero adjusted binomial - dZABI" = function(y) gamlss.dist::dZABI(y, mu = input$location, sigma = input$scale, bd = input$binomial_denominator),
      "Zero adjusted beta neg. bin. - dZABNB" = function(y) gamlss.dist::dZABNB(y, mu = input$location, sigma = input$scale, nu = input$skewness, tau = input$kurtosis),
      "Zero adjusted Gamma - dZAGA" = function(y) gamlss.dist::dZAGA(y, mu = input$location, sigma = input$scale, nu = input$skewness),
      "Zero adjusted IG - dZAIG" = function(y) gamlss.dist::dZAIG(y, mu = input$location, sigma = input$scale, nu = input$skewness),
      "Zero adjusted logarithmic - dZALG" = function(y) gamlss.dist::dZALG(y, mu = input$location, sigma = input$scale),
      "Zero adjusted neg. bin. - dZANBI" = function(y) gamlss.dist::dZANBI(y, mu = input$location, sigma = input$scale, nu = input$skewness),
      "Zero adjusted poisson - dZAP" = function(y) gamlss.dist::dZAP(y, mu = input$location, sigma = input$scale),
      "Zero adjusted PIG - dZAPIG" = function(y) gamlss.dist::dZAPIG(y, mu = input$location, sigma = input$scale, nu = input$skewness),
      "Zero adjusted Sichel - dZASICHEL" = function(y) gamlss.dist::dZASICHEL(y, mu = input$location, sigma = input$scale, nu = input$skewness, tau = input$kurtosis),
      "Zero adjusted Zipf - dZAZIPF" = function(y) gamlss.dist::dZAZIPF(y, mu = input$location, sigma = input$scale),
      "Zero inflated beta binomial - dZIBB" = function(y) gamlss.dist::dZIBB(y, mu = input$location, sigma = input$scale, nu = input$skewness, bd = input$binomial_denominator),
      "Zero inflated binomial - dZIBI" = function(y) gamlss.dist::dZIBI(y, mu = input$location, sigma = input$scale, bd = input$binomial_denominator),
      "Zero inflated beta neg. bin. - dZIBNB" = function(y) gamlss.dist::dZIBNB(y, mu = input$location, sigma = input$scale, nu = input$skewness, tau = input$kurtosis),
      "Zero inflated neg. bin. family - dZINBF" = function(y) gamlss.dist::dZINBF(y, mu = input$location, sigma = input$scale, nu = input$skewness, tau = input$kurtosis),
      "Zero inflated neg. bin. - dZINBI" = function(y) gamlss.dist::dZINBI(y, mu = input$location, sigma = input$scale, nu = input$skewness),
      "Zero inflated poisson - dZIP" = function(y) gamlss.dist::dZIP(y, mu = input$location, sigma = input$scale),
      "Zero inf. poiss.(mu as mean) - dZIP2" = function(y) gamlss.dist::dZIP2(y, mu = input$location, sigma = input$scale),
      "Zipf - dZIPF" = function(y) gamlss.dist::dZIPF(y, mu = input$location),
      "Zero inflated PIG - dZIPIG" = function(y) gamlss.dist::dZIPIG(y, mu = input$location, sigma = input$scale, nu = input$skewness),
      "Zero inflated Sichel - dZISICHEL" = function(y) gamlss.dist::dZISICHEL(y, mu = input$location, sigma = input$scale, nu = input$skewness, tau = input$kurtosis)

      # default: "Normal (mean, var) - dNO2"
      #function(y) gamlss.dist::dNO2(y, mu = input$location, sigma = input$scale)
    )
  })
  # Quantile function to estimate lower and upper bounds (without data)
  q_funct <- reactive({
    dist <- switch(input$dist,
                   "Beta Binomial - dBB" = gamlss.dist::qBB,
                   "Box-Cox Cole and Green - dBCCG" = gamlss.dist::qBCCG,
                   "Box-Cox Cole and Green - dBCCGo" = gamlss.dist::qBCCGo,
                   "Box-Cox Power Exponential - dBCPE" = gamlss.dist::qBCPE,
                   "Box-Cox Power Exponential - dBCPEo" = gamlss.dist::qBCPEo,
                   "Box-Cox-t - dBCT" = gamlss.dist::qBCT,
                   "Box-Cox-t - dBCTo" = gamlss.dist::qBCTo,
                   "Beta - dBE" = gamlss.dist::qBE,
                   "Beta inflated - dBEINF" = gamlss.dist::qBEINF,
                   "Beta inflated - dBEINF0" = gamlss.dist::qBEINF0,
                   "Beta inflated - dBEINF1" = gamlss.dist::qBEINF1,
                   "Beta - dBEo" = gamlss.dist::qBEo,
                   "Beta one inflated - dBEOI" = gamlss.dist::qBEOI,
                   "Beta zero inflated - dBEZI" = gamlss.dist::qBEZI,
                   "Binomial - dBI" = gamlss.dist::qBI,
                   "Beta negative binomial - dBNB" = gamlss.dist::qBNB,
                   "Double binomial - dDBI" = gamlss.dist::qDBI,
                   "Delaport - dDEL" = gamlss.dist::qDEL,
                   "Double Poisson - dDPO" = gamlss.dist::qDPO,
                   "Exponential generalized Beta type 2 - dEGB2" = gamlss.dist::qEGB2,
                   "Exponential Gaussian - dexGAUS" = gamlss.dist::qexGAUS,
                   "Exponential - dEXP" = gamlss.dist::qEXP,
                   "Gamma - dGA" = gamlss.dist::qGA,
                   "Generalized Beta type 1 - dGB1" = gamlss.dist::qGB1,
                   "Generalized Beta type 2 - dGB2" = gamlss.dist::qGB2,
                   "Geometric - dGEOM" = gamlss.dist::qGEOM,
                   "Geometric (original) - dGEOMo" = gamlss.dist::qGEOMo,
                   "Generalized Gamma - dGG" = gamlss.dist::qGG,
                   "Generalized Inverse Gaussian - dGIG" = gamlss.dist::qGIG,
                   "Generalised Pareto - dGP" = gamlss.dist::qGP,
                   "Generalised Pareto - dGPO" = gamlss.dist::qGPO,
                   "Generalized t - dGT" = gamlss.dist::qGT,
                   "Gumbel - dGU" = gamlss.dist::qGU,
                   "Inverse Gaussian - dIG" = gamlss.dist::qIG,
                   "Inverse Gamma - dIGAMMA" = gamlss.dist::qIGAMMA,
                   "Johnson's SU - dJSU" = gamlss.dist::qJSU,
                   "Johnson's SU - dJSUo" = gamlss.dist::qJSUo,
                   "Logarithmic - dLG" = gamlss.dist::qLG,
                   "log-Normal (Box-Cox) - dLNO" = gamlss.dist::qLNO,
                   "Logistic - dLO" = gamlss.dist::qLO,
                   "Logit Normal - dLOGITNO" = gamlss.dist::qLOGITNO,
                   "log-Normal - dLOGNO" = gamlss.dist::qLOGNO,
                   "log-Normal - dLOGNO2" = gamlss.dist::qLOGNO2,
                   "Normal Linear Quadratic - dLQNO" = gamlss.dist::qLQNO,
                   "Multinomial - dMN3" = gamlss.dist::qMN3,
                   "Multinomial - dMN4" = gamlss.dist::qMN4,
                   "Multinomial - dMN5" = gamlss.dist::qMN5,
                   "Negative Binomial family - dNBF" = gamlss.dist::qNBF,
                   "Negative Binomial type I - dNBI" = gamlss.dist::qNBI,
                   "Negative Binomial type II - dNBII" = gamlss.dist::qNBII,
                   "Normal Exponential t - dNET" = gamlss.dist::qNET,
                   "Normal (mean, sd) - dNO" = gamlss.dist::qNO,
                   "Normal (mean, var) - dNO2" = gamlss.dist::qNO2,
                   "Normal Family - dNOF" = gamlss.dist::qNOF,
                   "Pareto type 2 - dPARETO2" = gamlss.dist::qPARETO2,
                   "Pareto type 2 original - dPARETO2o" = gamlss.dist::qPARETO2o,
                   "Power Exponential - dPE" = gamlss.dist::qPE,
                   "Power Exponential type 2 - dPE2" = gamlss.dist::qPE2,
                   "Poisson inverse Gaussian - dPIG" = gamlss.dist::qPIG,
                   "Poison - dPO" = gamlss.dist::qPO,
                   "Reverse Gumbel - dRG" = gamlss.dist::qRG,
                   "Reverse generalized extreme - dRGE" = gamlss.dist::qRGE,
                   "Skew Power Exponential - dSEP" = gamlss.dist::qSEP,
                   "Skew Power Exponential type 1 - dSEP1" = gamlss.dist::qSEP1,
                   "Skew Power Exponential type 2 - dSEP2" = gamlss.dist::qSEP2,
                   "Skew Power Exponential type 3 - dSEP3" = gamlss.dist::qSEP3,
                   "Skew Power Exponential type 4 - dSEP4" = gamlss.dist::qSEP4,
                   "Shash - dSHASH" = gamlss.dist::qSHASH,
                   "Shash original - dSHASHo" = gamlss.dist::qSHASHo,
                   "Shash original 2 - dSHASHo2" = gamlss.dist::qSHASHo2,
                   "Sichel (original) - dSI" = gamlss.dist::qSI,
                   "Sichel (mu as the maen) - dSICHEL" = gamlss.dist::qSICHEL,
                   "Skew Normal Type 1 - dSN1" = gamlss.dist::qSN1,
                   "Skew Normal Type 2 - dSN2" = gamlss.dist::qSN2,
                   "Skew t - dSST" = gamlss.dist::qSST,
                   "Skew t type 1 - dST1" = gamlss.dist::qST1,
                   "Skew t type 2 - dST2" = gamlss.dist::qST2,
                   "Skew t type 3 - dST3" = gamlss.dist::qST3,
                   "Skew t - dST3C" = gamlss.dist::qST3C,
                   "Skew t type 4 - dST4" = gamlss.dist::qST4,
                   "Skew t type 5 - dST5" = gamlss.dist::qST5,
                   "t-distribution - dTF" = gamlss.dist::qTF,
                   "t-distribution - dTF2" = gamlss.dist::qTF2,
                   "Waring - dWARING" = gamlss.dist::qWARING,
                   "Weibull - dWEI" = gamlss.dist::qWEI,
                   "Weibull(PH parameterization) - dWEI2" = gamlss.dist::qWEI2,
                   "Weibull (mu as mean) - dWEI3" = gamlss.dist::qWEI3,
                   "Yule - dYULE" = gamlss.dist::qYULE,
                   "Zero adjusted beta binomial - dZABB" = gamlss.dist::qZABB,
                   "Zero adjusted binomial - dZABI" = gamlss.dist::qZABI,
                   "Zero adjusted beta neg. bin. - dZABNB" = gamlss.dist::qZABNB,
                   "Zero adjusted Gamma - dZAGA" = gamlss.dist::qZAGA,
                   "Zero adjusted IG - dZAIG" = gamlss.dist::qZAIG,
                   "Zero adjusted logarithmic - dZALG" = gamlss.dist::qZALG,
                   "Zero adjusted neg. bin. - dZANBI" = gamlss.dist::qZANBI,
                   "Zero adjusted poisson - dZAP" = gamlss.dist::qZAP,
                   "Zero adjusted PIG - dZAPIG" = gamlss.dist::qZAPIG,
                   "Zero adjusted Sichel - dZASICHEL" = gamlss.dist::qZASICHEL,
                   "Zero adjusted Zipf - dZAZIPF" = gamlss.dist::qZAZIPF,
                   "Zero inflated beta binomial - dZIBB" = gamlss.dist::qZIBB,
                   "Zero inflated binomial - dZIBI" = gamlss.dist::qZIBI,
                   "Zero inflated beta neg. bin. - dZIBNB" = gamlss.dist::qZIBNB,
                   "Zero inflated neg. bin. family - dZINBF" = gamlss.dist::qZINBF,
                   "Zero inflated neg. bin. - dZINBI" = gamlss.dist::qZINBI,
                   "Zero inflated poisson - dZIP" = gamlss.dist::qZIP,
                   "Zero inf. poiss.(mu as mean) - dZIP2" = gamlss.dist::qZIP2,
                   "Zipf - dZIPF" = gamlss.dist::qZIPF,
                   "Zero inflated PIG - dZIPIG" = gamlss.dist::qZIPIG,
                   "Zero inflated Sichel - dZISICHEL" = gamlss.dist::qZISICHEL
    )
  })
  # Density functions only for Maximum Likelihood Estimation below
  d_funct2 <- reactive({
    dist <- switch(input$dist,
                   "Beta Binomial - dBB" = gamlss.dist::dBB,
                   "Box-Cox Cole and Green - dBCCG" = gamlss.dist::dBCCG,
                   "Box-Cox Cole and Green - dBCCGo" = gamlss.dist::dBCCGo,
                   "Box-Cox Power Exponential - dBCPE" = gamlss.dist::dBCPE,
                   "Box-Cox Power Exponential - dBCPEo" = gamlss.dist::dBCPEo,
                   "Box-Cox-t - dBCT" = gamlss.dist::dBCT,
                   "Box-Cox-t - dBCTo" = gamlss.dist::dBCTo,
                   "Beta - dBE" = gamlss.dist::dBE,
                   "Beta inflated - dBEINF" = gamlss.dist::dBEINF,
                   "Beta inflated - dBEINF0" = gamlss.dist::dBEINF0,
                   "Beta inflated - dBEINF1" = gamlss.dist::dBEINF1,
                   "Beta - dBEo" = gamlss.dist::dBEo,
                   "Beta one inflated - dBEOI" = gamlss.dist::dBEOI,
                   "Beta zero inflated - dBEZI" = gamlss.dist::dBEZI,
                   "Binomial - dBI" = gamlss.dist::dBI,
                   "Beta negative binomial - dBNB" = gamlss.dist::dBNB,
                   "Double binomial - dDBI" = gamlss.dist::dDBI,
                   "Delaport - dDEL" = gamlss.dist::dDEL,
                   "Double Poisson - dDPO" = gamlss.dist::dDPO,
                   "Exponential generalized Beta type 2 - dEGB2" = gamlss.dist::dEGB2,
                   "Exponential Gaussian - dexGAUS" = gamlss.dist::dexGAUS,
                   "Exponential - dEXP" = gamlss.dist::dEXP,
                   "Gamma - dGA" = gamlss.dist::dGA,
                   "Generalized Beta type 1 - dGB1" = gamlss.dist::dGB1,
                   "Generalized Beta type 2 - dGB2" = gamlss.dist::dGB2,
                   "Geometric - dGEOM" = gamlss.dist::dGEOM,
                   "Geometric (original) - dGEOMo" = gamlss.dist::dGEOMo,
                   "Generalized Gamma - dGG" = gamlss.dist::dGG,
                   "Generalized Inverse Gaussian - dGIG" = gamlss.dist::dGIG,
                   "Generalised Pareto - dGP" = gamlss.dist::dGP,
                   "Generalised Pareto - dGPO" = gamlss.dist::dGPO,
                   "Generalized t - dGT" = gamlss.dist::dGT,
                   "Gumbel - dGU" = gamlss.dist::dGU,
                   "Inverse Gaussian - dIG" = gamlss.dist::dIG,
                   "Inverse Gamma - dIGAMMA" = gamlss.dist::dIGAMMA,
                   "Johnson's SU - dJSU" = gamlss.dist::dJSU,
                   "Johnson's SU - dJSUo" = gamlss.dist::dJSUo,
                   "Logarithmic - dLG" = gamlss.dist::dLG,
                   "log-Normal (Box-Cox) - dLNO" = gamlss.dist::dLNO,
                   "Logistic - dLO" = gamlss.dist::dLO,
                   "Logit Normal - dLOGITNO" = gamlss.dist::dLOGITNO,
                   "log-Normal - dLOGNO" = gamlss.dist::dLOGNO,
                   "log-Normal - dLOGNO2" = gamlss.dist::dLOGNO2,
                   "Normal Linear Quadratic - dLQNO" = gamlss.dist::dLQNO,
                   "Multinomial - dMN3" = gamlss.dist::dMN3,
                   "Multinomial - dMN4" = gamlss.dist::dMN4,
                   "Multinomial - dMN5" = gamlss.dist::dMN5,
                   "Negative Binomial family - dNBF" = gamlss.dist::dNBF,
                   "Negative Binomial type I - dNBI" = gamlss.dist::dNBI,
                   "Negative Binomial type II - dNBII" = gamlss.dist::dNBII,
                   "Normal Exponential t - dNET" = gamlss.dist::dNET,
                   "Normal (mean, sd) - dNO" = gamlss.dist::dNO,
                   "Normal (mean, var) - dNO2" = gamlss.dist::dNO2,
                   "Normal Family - dNOF" = gamlss.dist::dNOF,
                   "Pareto type 2 - dPARETO2" = gamlss.dist::dPARETO2,
                   "Pareto type 2 original - dPARETO2o" = gamlss.dist::dPARETO2o,
                   "Power Exponential - dPE" = gamlss.dist::dPE,
                   "Power Exponential type 2 - dPE2" = gamlss.dist::dPE2,
                   "Poisson inverse Gaussian - dPIG" = gamlss.dist::dPIG,
                   "Poison - dPO" = gamlss.dist::dPO,
                   "Reverse Gumbel - dRG" = gamlss.dist::dRG,
                   "Reverse generalized extreme - dRGE" = gamlss.dist::dRGE,
                   "Skew Power Exponential - dSEP" = gamlss.dist::dSEP,
                   "Skew Power Exponential type 1 - dSEP1" = gamlss.dist::dSEP1,
                   "Skew Power Exponential type 2 - dSEP2" = gamlss.dist::dSEP2,
                   "Skew Power Exponential type 3 - dSEP3" = gamlss.dist::dSEP3,
                   "Skew Power Exponential type 4 - dSEP4" = gamlss.dist::dSEP4,
                   "Shash - dSHASH" = gamlss.dist::dSHASH,
                   "Shash original - dSHASHo" = gamlss.dist::dSHASHo,
                   "Shash original 2 - dSHASHo2" = gamlss.dist::dSHASHo2,
                   "Sichel (original) - dSI" = gamlss.dist::dSI,
                   "Sichel (mu as the maen) - dSICHEL" = gamlss.dist::dSICHEL,
                   "Skew Normal Type 1 - dSN1" = gamlss.dist::dSN1,
                   "Skew Normal Type 2 - dSN2" = gamlss.dist::dSN2,
                   "Skew t - dSST" = gamlss.dist::dSST,
                   "Skew t type 1 - dST1" = gamlss.dist::dST1,
                   "Skew t type 2 - dST2" = gamlss.dist::dST2,
                   "Skew t type 3 - dST3" = gamlss.dist::dST3,
                   "Skew t - dST3C" = gamlss.dist::dST3C,
                   "Skew t type 4 - dST4" = gamlss.dist::dST4,
                   "Skew t type 5 - dST5" = gamlss.dist::dST5,
                   "t-distribution - dTF" = gamlss.dist::dTF,
                   "t-distribution - dTF2" = gamlss.dist::dTF2,
                   "Waring - dWARING" = gamlss.dist::dWARING,
                   "Weibull - dWEI" = gamlss.dist::dWEI,
                   "Weibull(PH parameterization) - dWEI2" = gamlss.dist::dWEI2,
                   "Weibull (mu as mean) - dWEI3" = gamlss.dist::dWEI3,
                   "Yule - dYULE" = gamlss.dist::dYULE,
                   "Zero adjusted beta binomial - dZABB" = gamlss.dist::dZABB,
                   "Zero adjusted binomial - dZABI" = gamlss.dist::dZABI,
                   "Zero adjusted beta neg. bin. - dZABNB" = gamlss.dist::dZABNB,
                   "Zero adjusted Gamma - dZAGA" = gamlss.dist::dZAGA,
                   "Zero adjusted IG - dZAIG" = gamlss.dist::dZAIG,
                   "Zero adjusted logarithmic - dZALG" = gamlss.dist::dZALG,
                   "Zero adjusted neg. bin. - dZANBI" = gamlss.dist::dZANBI,
                   "Zero adjusted poisson - dZAP" = gamlss.dist::dZAP,
                   "Zero adjusted PIG - dZAPIG" = gamlss.dist::dZAPIG,
                   "Zero adjusted Sichel - dZASICHEL" = gamlss.dist::dZASICHEL,
                   "Zero adjusted Zipf - dZAZIPF" = gamlss.dist::dZAZIPF,
                   "Zero inflated beta binomial - dZIBB" = gamlss.dist::dZIBB,
                   "Zero inflated binomial - dZIBI" = gamlss.dist::dZIBI,
                   "Zero inflated beta neg. bin. - dZIBNB" = gamlss.dist::dZIBNB,
                   "Zero inflated neg. bin. family - dZINBF" = gamlss.dist::dZINBF,
                   "Zero inflated neg. bin. - dZINBI" = gamlss.dist::dZINBI,
                   "Zero inflated poisson - dZIP" = gamlss.dist::dZIP,
                   "Zero inf. poiss.(mu as mean) - dZIP2" = gamlss.dist::dZIP2,
                   "Zipf - dZIPF" = gamlss.dist::dZIPF,
                   "Zero inflated PIG - dZIPIG" = gamlss.dist::dZIPIG,
                   "Zero inflated Sichel - dZISICHEL" = gamlss.dist::dZISICHEL

                   #gamlss.dist::dNO2 # default: "Normal (mean, var) - dNO2"
    )
  })
  # maximum-likelihood-estimates for standard values
  # checks if data is discrete or continuous with checking if all modulus' are
  #   equal to zero or if one is unequal to zero (checks if it is a integer)
  mle <- reactive({
    # Access value of chosen variable from dataset
    plot.obj <<- list()
    plot.obj$data <<- datasetInput()
    plot.obj$variable <<- with(data = plot.obj$data,
                               expr = get(input$variable))
    # without data
    if (is.null(datasetInput())) {
      return()
      # with data
      # without numeric input
    } else if (!is.numeric(plot.obj$variable)) {
      return()
    } else {
      # with numeric input
      default.parameter <- as.numeric(shinydistributions:::distributions[
        shinydistributions:::distributions$dist_density == input$dist,
        c("default_location", "default_scale", "default_skewness",
          "default_kurtosis")])
      default.parameter[is.na(default.parameter)] <- 0
      binomial_denominator <- shinydistributions:::distributions[
        shinydistributions:::distributions$dist_density == input$dist,
        "default_binomial_denominator"]
      log.likelihood <- function(parameter, data) {
        -sum(R.utils::doCall(
          d_funct2(),
          x = data,
          log = TRUE,
          bd = ifelse(test = input$max_x <= binomial_denominator,
                      yes = binomial_denominator, no = input$max_x),
          mu = parameter[1],
          sigma = parameter[2],
          nu = parameter[3],
          tau = parameter[4])
        )
      }
      # function: continuous, data: discrete
      if ((shinydistributions:::distributions[
        shinydistributions:::distributions$dist_density == input$dist,
        "discrete_flag"] == 0) && all(plot.obj$variable %% 1 == 0)) {
      #  (length(unique(plot.obj$variable))/length(plot.obj$variable) <= 0.1)) {
        showNotification(
          ui = "Input variable is considered as discrete while using a
          continuous function.",
          type = "warning"
        )
        mle <- optim(
          par = default.parameter,
          fn = log.likelihood,
          data = plot.obj$variable,
          lower = c(
            shinydistributions:::distributions[
              shinydistributions:::distributions$dist_density == input$dist,
              "location_lower_bound"],
            shinydistributions:::distributions[
              shinydistributions:::distributions$dist_density == input$dist,
              "scale_lower_bound"],
            shinydistributions:::distributions[
              shinydistributions:::distributions$dist_density == input$dist,
              "skewness_lower_bound"],
            shinydistributions:::distributions[
              shinydistributions:::distributions$dist_density == input$dist,
              "kurtosis_lower_bound"]
          ),
          upper = c(
            shinydistributions:::distributions[
              shinydistributions:::distributions$dist_density == input$dist,
              "location_upper_bound"],
            shinydistributions:::distributions[
              shinydistributions:::distributions$dist_density == input$dist,
              "scale_upper_bound"],
            shinydistributions:::distributions[
              shinydistributions:::distributions$dist_density == input$dist,
              "skewness_upper_bound"],
            shinydistributions:::distributions[
              shinydistributions:::distributions$dist_density == input$dist,
              "kurtosis_upper_bound"]
          ),
          method = "L-BFGS-B"
        )
        mle <- mle$par
      # functions: discrete, data: continuous
      } else if ((shinydistributions:::distributions[
        shinydistributions:::distributions$dist_density == input$dist,
        "discrete_flag"] == 1) && any(plot.obj$variable %% 1 != 0)) {
      # (length(unique(plot.obj$variable)) / length(plot.obj$variable) > 0.1)) {
        showNotification(
          ui = "You are using continuous data with a discrete function.",
          type = "error"
        )
        mle <- return()
        #mle <- default.parameter
      # function type and data type are equal
      } else {
        mle <- optim(
          par = default.parameter,
          fn = log.likelihood,
          data = plot.obj$variable,
          lower = c(
            shinydistributions:::distributions[
              shinydistributions:::distributions$dist_density == input$dist,
              "location_lower_bound"],
            shinydistributions:::distributions[
              shinydistributions:::distributions$dist_density == input$dist,
              "scale_lower_bound"],
            shinydistributions:::distributions[
              shinydistributions:::distributions$dist_density == input$dist,
              "skewness_lower_bound"],
            shinydistributions:::distributions[
              shinydistributions:::distributions$dist_density == input$dist,
              "kurtosis_lower_bound"]
          ),
          upper = c(
            shinydistributions:::distributions[
              shinydistributions:::distributions$dist_density == input$dist,
              "location_upper_bound"],
            shinydistributions:::distributions[
              shinydistributions:::distributions$dist_density == input$dist,
              "scale_upper_bound"],
            shinydistributions:::distributions[
              shinydistributions:::distributions$dist_density == input$dist,
              "skewness_upper_bound"],
            shinydistributions:::distributions[
              shinydistributions:::distributions$dist_density == input$dist,
              "kurtosis_upper_bound"]
          ),
          method = "L-BFGS-B"
        )
        mle <- mle$par
      }
    }
  })
  # Update parameters (location, scale, skewness, kurtosis, binomial
  # denominator) by default values of particular density function

  # Location
  output$num_location <- renderUI({
    # without data
    if ( is.null(datasetInput()) ) {
      numericInput(
        inputId = "location",
        label = "Location",
        value = shinydistributions:::distributions[
          shinydistributions:::distributions$dist_density == input$dist,
          "default_location"]
      )
      # with data
    } else {
      plot.obj <<- list()
      plot.obj$data <<- datasetInput()
      plot.obj$variable <<- with(data = plot.obj$data,
                                 expr = get(input$variable))
      numericInput(
        inputId = "location",
        label = "Location",
        value = ifelse(
          test = is.null(mle()[1]),
          yes = shinydistributions:::distributions[
            shinydistributions:::distributions$dist_density == input$dist,
            "default_location"],
          no = round(mle()[1], digits = 2)
        )
      )
    }
  })
  # Scale
  output$num_scale <- renderUI({
    # Hide if scale is not a parameter of the density function
    if (is.na(shinydistributions:::distributions[
      shinydistributions:::distributions$dist_density == input$dist,
      "default_scale"])) {
      return()
    }
    # without data
    if ( is.null(datasetInput()) ) {
      numericInput(
        inputId = "scale",
        label = "Scale",
        value = shinydistributions:::distributions[
          shinydistributions:::distributions$dist_density == input$dist,
                                                   "default_scale"],
        step = 0.1
      )
      # with data
    } else {
      plot.obj <<- list()
      plot.obj$data <<- datasetInput()
      plot.obj$variable <<- with(data = plot.obj$data,
                                 expr = get(input$variable))
      numericInput(
        inputId = "scale",
        label = "Scale",
        value = ifelse(
          test = is.null(mle()[2]),
          yes = shinydistributions:::distributions[
            shinydistributions:::distributions$dist_density == input$dist,
            "default_scale"],
          no = round(mle()[2], digits = 2)
        ),
        step = 0.1
      )
    }
  })
  # Skewness
  output$num_skewness <- renderUI({
    # Hide if skewness is not a parameter of the density function
    if (is.na(shinydistributions:::distributions[
      shinydistributions:::distributions$dist_density == input$dist,
      "default_skewness"])) {
      return()
    }
    # without data
    if (is.null(datasetInput())) {
      numericInput(
        inputId = "skewness",
        label = "Skewness",
        value = shinydistributions:::distributions[
          shinydistributions:::distributions$dist_density == input$dist,
          "default_skewness"]
      )
      # with data
    } else {
      plot.obj <<- list()
      plot.obj$data <<- datasetInput()
      plot.obj$variable <<- with(data = plot.obj$data,
                                 expr = get(input$variable))

      numericInput(
        inputId = "skewness",
        label = "Skewness",
        value = ifelse(
          test = is.null(mle()[3]),
          yes = shinydistributions:::distributions[
            shinydistributions:::distributions$dist_density == input$dist,
            "default_skewness"],
          no = round(mle()[3], digits = 2)
        ),
        step = 0.1
      )
    }
  })
  # Kurtosis
  output$num_kurtosis <- renderUI({
    # Hide if kurtosis is not a parameter of the density function
    if (is.na(shinydistributions:::distributions[
      shinydistributions:::distributions$dist_density == input$dist,
      "default_kurtosis"])) {
      return()
    }
    # without data
    if (is.null(datasetInput())) {
      numericInput(
        inputId = "kurtosis",
        label = "Kurtosis",
        value = shinydistributions:::distributions[
          shinydistributions:::distributions$dist_density == input$dist,
          "default_kurtosis"]
      )
      # with data
    } else {
      plot.obj <<- list()
      plot.obj$data <<- datasetInput()
      plot.obj$variable <<- with(data = plot.obj$data,
                                 expr = get(input$variable))
      numericInput(
        inputId = "kurtosis",
        label = "Kurtosis",
        value = ifelse(
          test = is.null(mle()[4]),
          yes = shinydistributions:::distributions[
            shinydistributions:::distributions$dist_density == input$dist,
            "default_kurtosis"],
          no = round(mle()[4], digits = 2)
        ),
        step = 0.1
      )
    }
  })
  # Binomial denominator
  output$num_binomial_denominator <- renderUI({
    # Hide if Binomial denominator is not a parameter of the density function
    if (is.na(shinydistributions:::distributions[
      shinydistributions:::distributions$dist_density == input$dist,
      "default_binomial_denominator"])) {
      return()
    }
    binomial_denominator <- shinydistributions:::distributions[
      shinydistributions:::distributions$dist_density == input$dist,
      "default_binomial_denominator"]

    numericInput(
      inputId = "binomial_denominator",
      label = "Binomial denominator",
      value = if (is.na(binomial_denominator)) {
        binomial_denominator
      } else if (!is.na(binomial_denominator)) {
        ifelse(input$max_x <= binomial_denominator,
               yes = binomial_denominator, no = input$max_x)
      },
      min = input$max_x
    )
  })
  #### Plot ####

  # Plot Type
  #(Histogram or Kernel Density Estimator / Empirical Probability Function)
  output$plot_type <- renderUI({
    if (is.null(datasetInput())) {
      return()
    } else if (shinydistributions:::distributions[
      shinydistributions:::distributions$dist_density == input$dist,
      "discrete_flag"] != 1) {

      selectInput(
        inputId = "plot_type",
        label = "Plot Type",
        choices = list("Histogram",
                       "Kernel Density Estimator"
        )
      )
    } else if (shinydistributions:::distributions[
      shinydistributions:::distributions$dist_density == input$dist,
      "discrete_flag"] == 1) {

      selectInput(
        inputId = "plot_type",
        label = "Plot Type",
        choices = list("Empirical Probability Function")
      )
    }
  })
  # Number of Bins for Histogram
  output$num_bins <- renderUI({
    if (is.null(datasetInput()) || input$plot_type != "Histogram") {
      return()
    }
    plot.obj <<- list()
    plot.obj$data <<- datasetInput()
    plot.obj$variable <<- with(data = plot.obj$data, expr = get(input$variable))

    numericInput(
      inputId = "num_bins",
      label = "Histogram Bins", #"Number of Bins for Histogram",
      value = nclass.Sturges(plot.obj$variable),
      min = 0
    )
  })
  # Maximum y-axis
  output$num_max_y <- renderUI({
    # without data
    if (is.null(datasetInput())) {
      numericInput(
        inputId = "max_y",
        label = "Maximum of Y-axis",
        step = 0.1,
        value = 0.5
      )
    # with data
    } else {
      plot.obj <<- list()
      plot.obj$data <<- datasetInput()
      plot.obj$variable <<- with(data = plot.obj$data,
                                 expr = get(input$variable))
      validate(
        need(expr = is.numeric(plot.obj$variable), message = "")
      )
      density <- density(plot.obj$variable)
      max.density <- max(density$y)

      numericInput(
        inputId = "max_y",
        label = "Maximum of Y-axis",
        step = 0.1,
        value = if (max.density + max.density*0.3 >= 0.1) {
          round(x = max.density + max.density*0.3, digits = 2)
        } else {
          max.density + max.density*0.3
        }
      )
    }
  })
  # Maximum x-axis
  output$num_max_x <- renderUI({
    # without data
    if (is.null(datasetInput())) {
      numericInput(
        inputId = "max_x",
        label = "Maximum of X-axis",
        value = ifelse(
          test = is.na(shinydistributions:::distributions[
            shinydistributions:::distributions$dist_density == input$dist,
            "x_upper_bound"]),
          yes = 5,
          no = shinydistributions:::distributions[
            shinydistributions:::distributions$dist_density == input$dist,
            "x_upper_bound"]
        )
      )
    # with data
    } else {
      plot.obj <<- list()
      plot.obj$data <<- datasetInput()
      plot.obj$variable <<- with(data = plot.obj$data,
                                 expr = get(input$variable))
      validate(
        need(expr = is.numeric(plot.obj$variable), message = "")
      )
      numericInput(
        inputId = "max_x",
        label = "Maximum of X-axis",
        value = round(max(plot.obj$variable), digits = 2)
      )
    }
  })
  # Minimum x-axis
  output$num_min_x <- renderUI({
    # without data
    if ( is.null(datasetInput()) ) {
      numericInput(
        inputId = "min_x",
        label = "Minimum of X-axis",
        value = ifelse(
          test = is.na(shinydistributions:::distributions[
            shinydistributions:::distributions$dist_density == input$dist,
            "x_lower_bound"]),
          yes = -5,
          no = shinydistributions:::distributions[
            shinydistributions:::distributions$dist_density == input$dist,
            "x_lower_bound"]
        )
      )
    # with data
    } else {
      plot.obj <<- list()
      plot.obj$data <<- datasetInput()
      plot.obj$variable <<- with(data = plot.obj$data,
                                 expr = get(input$variable))
      validate(
        need(expr = is.numeric(plot.obj$variable), message = "")
      )
      numericInput(
        inputId = "min_x",
        label = "Minimum of X-axis",
        value = round(min(plot.obj$variable), digits = 2)
      )
    }
  })
  #### Generate plot of the data ####
  output$plot <- renderUI({
    plotOutput(outputId = "p")

  })
  output$p <- renderPlot({
    # Define selected distribution as "dist" for usage within renderPlot
    dist <- input$dist

    # Define limits of x-axis
    x.limits <- c(input$min_x, input$max_x)

    # Set limits of x and y-axis (continuous)
    axes.limits.con <- ggplot2::coord_cartesian(xlim = x.limits,
                                                ylim = c(0, input$max_y),
                                                expand = FALSE)
    # Set limits of x and y-axis (discrete)
    axes.limits.dis <- ggplot2::coord_cartesian(xlim = x.limits,
                                                ylim = c(0, input$max_y),
                                                expand = TRUE)
    # Label axes (continuous, with data)
    axes.label.con <- ggplot2::labs(
      x = input$variable,
      y = "Probability Density",
      title = shinydistributions:::distributions[
        shinydistributions:::distributions$dist_density == dist,
        "distribution"]
    )
    # Label axes (discrete, with data)
    axes.label.dis <- ggplot2::labs(
      x = input$variable,
      y = "Probability",
      title = shinydistributions:::distributions[
        shinydistributions:::distributions$dist_density == dist,
        "distribution"]
    )
    # ggplot theme
    .theme <- ggplot2::theme(
      axis.line = ggplot2::element_line(colour = "#000000"),
      panel.background = ggplot2::element_blank(),
      plot.background = ggplot2::element_blank(),

      # Adjust fonts of plot title, axes
      plot.title = ggplot2::element_text(size = 25, hjust = 0.5,
                                         face = "bold", colour = "maroon",
                                         vjust = 0.5),
      axis.text = ggplot2::element_text(colour = "black", size = 13),
      axis.title = ggplot2::element_text(size = 16, face = "bold"),
      axis.title.y = ggplot2::element_text(vjust = 4)
    )
    # Define theoretical density (continuous)
    theo.dens <- ggplot2::stat_function(ggplot2::aes(x = x.limits),
                                        fun = d_funct(), n = 151,
                                        size = 1.5, colour = "maroon"
    )
    # Define theoretical probability function (discrete)
    theo.pf <- ggplot2::stat_function(ggplot2::aes(x = x.limits),
                                      fun = d_funct(),
                                      n = input$max_x - input$min_x + 1,
                                      geom = "point",
                                      size = 3, color = "maroon")
    # Add line to theoretical probability function (discrete)
    theo.pf.lines <- ggplot2::geom_linerange(
      ggplot2::aes(x = seq(input$min_x, input$max_x, 1),
                   ymax = d_funct()(y = seq(input$min_x, input$max_x, 1)),
                   ymin = 0),
      size = 1, colour = "maroon")

    ################
    # without data #
    ################

    #### discrete_flag != 1: continuous or mixed (0, 2 respectively) ####
    if ( is.null(datasetInput()) &&
         shinydistributions:::distributions[
           shinydistributions:::distributions$dist_density == dist,
           "discrete_flag"] != 1) {

      # ggplot
      ggplot2::ggplot() +
        # Label axes and set limits
        axes.limits.con +
        ggplot2::labs(x = NULL, y = "Probability Density",
                      title = shinydistributions:::distributions[
                        shinydistributions:::distributions$dist_density == dist,
                        "distribution"]) +
        .theme +
        # Add theoretical density
        theo.dens

      #### discrete_flag == 1: discrete ####
    } else if ( is.null(datasetInput()) &&
                shinydistributions:::distributions[
                  shinydistributions:::distributions$dist_density == dist,
                  "discrete_flag"] == 1) {
      # ggplot
      ggplot2::ggplot() +
        # Label axes and set limits
        axes.limits.dis +
        ggplot2::labs(x = NULL, y = "Probability",
                      title = shinydistributions:::distributions[
                        shinydistributions:::distributions$dist_density == dist,
                        "distribution"]) +
        .theme +
        # Add theoretical probability function
        theo.pf +
        theo.pf.lines

      #############
      # with data #
      #############
    } else if (!is.null(datasetInput())) {

      # Access value of chosen variable from dataset
      plot.obj <<- list()
      plot.obj$data <<- datasetInput()
      plot.obj$variable <<- with(plot.obj$data, get(input$variable))
      validate(
        need(expr = is.numeric(plot.obj$variable),
             message = "Input variable has to be numeric.")
      )
      # Discrete variable and function
      # Create data frame containing relative frequencies of variable
      if ((shinydistributions:::distributions[
        shinydistributions:::distributions$dist_density == input$dist,
        "discrete_flag"] == 1) && any(plot.obj$variable %% 1 == 0)) {

        df <- data.frame(prop.table(table(plot.obj$variable)))
        # Convert input variable (Var1) from factor to integer (=Var2)
        Var2 <- as.integer(levels(df$Var1))[df$Var1]
      }
      # Use selected plot type (histogram or kernel density estimator/
      # empirical probability function)
      plot_type <- switch (
        EXPR = input$plot_type,
        #### Continuous/mixed distributions ####
        "Histogram" = ggplot2::ggplot() +
          ggplot2::geom_histogram(
            mapping = ggplot2::aes(x = plot.obj$variable, y = ..density..),
            data = plot.obj$data,
            alpha = 0.5,
            bins = input$num_bins,
            colour = "#000000",
            fill = "#56B4E9"
          ) +
          # Add theoretical density
          theo.dens +
          # Label axes and set limits
          axes.limits.con +
          axes.label.con,

        "Kernel Density Estimator" = ggplot2::ggplot() +
          ggplot2::geom_density(
            mapping = ggplot2::aes(x = plot.obj$variable),
            data = plot.obj$data,
            alpha = 0.5,
            colour = "#000000",
            fill = "#56B4E9"
          ) +
          # Add theoretical density
          theo.dens +
          # Label axes and set limits
          axes.limits.con +
          axes.label.con,

        #### Discrete distributions ####
        # Do not run plot if variable is continuous
        "Empirical Probability Function" = if(any(plot.obj$variable %% 1 != 0)){
          return()
        } else {
          ggplot2::ggplot() +
            ggplot2::geom_point(data = df, ggplot2::aes(x = Var2, y = Freq),
                                size = 3.5, colour = "#56B4E9") +
            ggplot2::geom_linerange(data = df,
                                    ggplot2::aes(x = Var2, ymax = Freq, ymin = 0),
                                    size = 1.5, colour = "#56B4E9") +
            # Add theoretical probability function
            theo.pf +
            theo.pf.lines +
            # Label axes and set limits
            axes.limits.dis +
            axes.label.dis
        }
      )
      # Plot Histogramm or Kernel Density Estimator and theoretical density or
      # Empirical and Theoretical Probability Functions
      plot_type +
        .theme
    }
  })
  # Headline above plot dependent on plot type and distribution type
  output$caption <- renderText({
    # without data
    if (is.null(datasetInput()) &&
        shinydistributions:::distributions[
          shinydistributions:::distributions$dist_density == input$dist,
          "discrete_flag"] != 1) {
      "Theoretical Density"
    } else if (is.null(datasetInput()) &&
               shinydistributions:::distributions[
                 shinydistributions:::distributions$dist_density == input$dist,
                 "discrete_flag"] == 1) {
      "Theoretical Probability Function"

      # with data
    } else if (shinydistributions:::distributions[
      shinydistributions:::distributions$dist_density == input$dist,
      "discrete_flag"] != 1) {
      switch(EXPR = input$plot_type,
             "Histogram" = "Histogram of Data and Theoretical Density",
             "Kernel Density Estimator" =
               "Kernel Density Estimator and Theoretical Density"
      )
    } else if (shinydistributions:::distributions[
      shinydistributions:::distributions$dist_density == input$dist,
      "discrete_flag"] == 1) {
      switch(EXPR = input$plot_type,
             "Empirical Probability Function" =
               "Empirical and Theoretical Probability Functions")
    }
  })
  #### Info text on main panel below plot ####

  # Headline Default Parameter(s) of Theoretical Density / Probability Function
  output$head_default_parameters <- renderText({
    if (shinydistributions:::distributions[
      shinydistributions:::distributions$dist_density == input$dist,
      "discrete_flag"] != 1 ) {

      "Default Parameter(s) of Theoretical Density Function:"
    } else {
      "Default Parameter(s) of Theoretical Probability Function:"
    }
  })
  # Values of Default Parameter(s) of Theoretical Density / Probability Function
  output$info_text <- renderText({
    dist <- input$dist

    paste(
      "Location = ", shinydistributions:::distributions[
        shinydistributions:::distributions$dist_density == dist,
        "default_location"],
      ifelse(is.na(shinydistributions:::distributions[
        shinydistributions:::distributions$dist_density == dist,
        "default_scale"]), '',
        paste("; Scale = ", shinydistributions:::distributions[
          shinydistributions:::distributions$dist_density == dist,
          "default_scale"], sep = '')),
      ifelse(is.na(shinydistributions:::distributions[
        shinydistributions:::distributions$dist_density == dist,
        "default_skewness"]), '',
        paste("; Skewness = ", shinydistributions:::distributions[
          shinydistributions:::distributions$dist_density == dist,
          "default_skewness"], sep = '')),
      ifelse(is.na(shinydistributions:::distributions[
        shinydistributions:::distributions$dist_density == dist,
        "default_kurtosis"]), '',
        paste("; Kurtosis = ", shinydistributions:::distributions[
          shinydistributions:::distributions$dist_density == dist,
          "default_kurtosis"], sep = '')),
      ifelse(is.na(shinydistributions:::distributions[
        shinydistributions:::distributions$dist_density == dist,
        "default_binomial_denominator"]), '',
        paste("; Binomial denominator = ", shinydistributions:::distributions[
          shinydistributions:::distributions$dist_density == dist,
          "default_binomial_denominator"], sep = '')),
      sep = '')
  })

  # Headline Maximum Likelihood Estimates
  output$head_ml_estimates <- renderText({
    if (is.null(datasetInput())) {
      return()
    } else if (is.null(mle())) {
      return()
    } else {
      "Maximum Likelihood Estimate(s) of Parameter(s):"
    }
  })
  # Maximum Likelihood Estimates of Parameters
  output$info_text2 <- renderText({

    if (is.null(datasetInput())) {
      return()
    } else if (is.null(mle())) {
      return()
    } else {
      dist <- input$dist

      paste(
        "Location = ", round(mle()[1], digits = 2),

        ifelse(is.na(shinydistributions:::distributions[
          shinydistributions:::distributions$dist_density == dist,
          "default_scale"]), '',
          paste("; Scale = ", round(mle()[2], digits = 2), sep = '')),

        ifelse(is.na(shinydistributions:::distributions[
          shinydistributions:::distributions$dist_density == dist,
          "default_skewness"]), '',
          paste("; Skewness = ", round(mle()[3], digits = 2), sep = '')),

        ifelse(is.na(shinydistributions:::distributions[
          shinydistributions:::distributions$dist_density == dist,
          "default_kurtosis"]), '',
          paste("; Kurtosis = ", round(mle()[4], digits = 2), sep = '')),
        sep = '')
    }
  })
}

shiny::shinyApp(ui = ui, server = server)
