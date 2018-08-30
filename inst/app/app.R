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
            choices = shinydistributions::distributions$dist_density
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
  # or all rows if selected, will be shown.

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
      # Increase maximum upload size from 5 MB to 30 MB
      old <- options(shiny.maxRequestSize = 5*1024^2)
      options(shiny.maxRequestSize = 30*1024^2)
      read.csv(file = input$dataset$datapath, header = input$header,
               sep = input$sep, quote = input$quote)
      # ...and set it back to the initial value
      on.exit(expr = options(old), add = TRUE)
    } else if (values$upload_state == 'reset') {
      switch (input$r_dataset,
              "iris" = iris,
              "diamonds" = ggplot2::diamonds,
              "mtcars" = mtcars,
              "None" = return()
      )
    }
  })
  #output$stop_mle <- renderUI({
  #  actionButton(
  #    inputId = "stop_mle",
  #    label = "Stop Maximum-Likelihood-Estimation"
  #  )
  #})
  # Display data frame or summary of data
  output$contents <- DT::renderDataTable({

    if ( is.null(datasetInput()) ) {
      return()
    } else {

      if (input$display == "Data Frame") {

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
  output$shinydistributions::distributions_tab <- DT::renderDataTable({
    DT::datatable(shinydistributions::distributions_tab, rownames = FALSE,
                  options = list(lengthMenu = c(10, 15, 20, 50, 109),
                                 pageLength = 10
                                 #columnDefs = list(
                                 # list(className = 'dt-left', targets= '_all')
                                 #)
                  )
    )
  })
  #### Tab: Plot Distribution ####
  # Update variable selection based on uploaded data set
  output$variable <- renderUI({
    if ( is.null(datasetInput()) ) {
      return()
    } else {
      selectInput(
        inputId = "variable",
        label = "Variable",
        choices = colnames(datasetInput())
      )
    }
  })
  # Update density function based on selected type and parameters
  observe({
    all <- shinydistributions::distributions$dist_density
    continuous <- shinydistributions::distributions[shinydistributions::distributions$dist_type == "continuous",
                                                     "dist_density"]
    discrete <- shinydistributions::distributions[shinydistributions::distributions$dist_type == "discrete",
                                                   "dist_density"]
    mixed <- shinydistributions::distributions[shinydistributions::distributions$dist_type == "mixed", "dist_density"]
    one <- shinydistributions::distributions[shinydistributions::distributions$no_of_parameters == 1, "dist_density"]
    two <- shinydistributions::distributions[shinydistributions::distributions$no_of_parameters == 2, "dist_density"]
    three <- shinydistributions::distributions[shinydistributions::distributions$no_of_parameters == 3, "dist_density"]
    four <- shinydistributions::distributions[shinydistributions::distributions$no_of_parameters == 4, "dist_density"]
    choices <- shinydistributions::distributions$dist_density
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
        selected <- "Normal (mu, var) - dNO2"
      }
      if (input$parameters == 1) {
        choices.parameters <- one
        selected <- "Exponential - dEXP"
      }
      if (input$parameters == "all") {
        choices.parameters <- all
        selected <- "Normal (mu, var) - dNO2"
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
        selected <- "Normal (mu, var) - dNO2"
      }
      if (input$parameters == 1) {
        choices.parameters <- one
        selected <- "Exponential - dEXP"
      }
      if (input$parameters == "all") {
        choices.parameters <- all
        selected <- "Normal (mu, var) - dNO2"
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
  # distributions include parameter "bd" (binomial denominators) additionally.
  d_funct <- reactive({
    dist <- switch(
      EXPR = input$dist,
      "Beta Binomial - dBB" = function(y) gamlss.dist::dBB(y, mu = input$location, sigma = input$scale, bd = input$binomial_denominators),
      "Box-Cox Cole and Green - dBCCG" = function(y) gamlss.dist::dBCCG(y, mu = input$location, sigma = input$scale, nu = input$skewness),
      "Box-Cox Cole and Green - dBCCGo" = function(y) gamlss.dist::dBCCGo(y, mu = input$location, sigma = input$scale, nu = input$skewness),
      "Box-Cox Power Exponential - dBCPE" = function(y) gamlss.dist::dBCPE(y, mu = input$location, sigma = input$scale, nu = input$skewness, tau = input$kurtosis),
      "Box-Cox Power Exponential - dBCPEo" = function(y) gamlss.dist::dBCPEo(y, mu = input$location, sigma = input$scale, nu = input$skewness),
      "Box-Cox-t - dBCT" = function(y) gamlss.dist::dBCT(y, mu = input$location, sigma = input$scale, nu = input$skewness, tau = input$kurtosis),
      "Box-Cox-t - dBCTo" = function(y) gamlss.dist::dBCTo(y, mu = input$location, sigma = input$scale, nu = input$skewness),
      "Beta - dBE" = function(y) gamlss.dist::dBE(y, mu = input$location, sigma = input$scale),
      "Beta inflated - dBEINF" = function(y) gamlss.dist::dBEINF(y, mu = input$location, sigma = input$scale, nu = input$skewness, tau = input$kurtosis),
      "Beta inflated - dBEINF0" = function(y) gamlss.dist::dBEINF0(y, mu = input$location, sigma = input$scale, nu = input$skewness),
      "Beta inflated - dBEINF1" = function(y) gamlss.dist::dBEINF1(y, mu = input$location, sigma = input$scale, nu = input$skewness),
      "Beta - dBEo" = function(y) gamlss.dist::dBEo(y, mu = input$location, sigma = input$scale),
      "Beta one inflated - dBEOI" = function(y) gamlss.dist::dBEOI(y, mu = input$location, sigma = input$scale, nu = input$skewness),
      "Beta zero inflated - dBEZI" = function(y) gamlss.dist::dBEZI(y, mu = input$location, sigma = input$scale, nu = input$skewness),
      "Binomial - dBI" = function(y) gamlss.dist::dBI(y, mu = input$location, bd = input$binomial_denominators),
      "Beta negative binomial - dBNB" = function(y) gamlss.dist::dBNB(y, mu = input$location, sigma = input$scale, nu = input$skewness),
      "Double binomial - dDBI" = function(y) gamlss.dist::dDBI(y, mu = input$location, sigma = input$scale, bd = input$binomial_denominators),
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
      "Johnson's SU - dJSUo" = function(y) gamlss.dist::dJSUo(y, mu = input$location),
      "Logarithmic - dLG" = function(y) gamlss.dist::dLG(y, mu = input$location),
      "log-Normal (Box-Cox) - dLNO" = function(y) gamlss.dist::dLNO(y, mu = input$location, sigma = input$scale, nu = input$skewness),
      "Logistic - dLO" = function(y) gamlss.dist::dLO(y, mu = input$location, sigma = input$scale),
      "Logit Normal - dLOGITNO" = function(y) gamlss.dist::dLOGITNO(y, mu = input$location, sigma = input$scale),
      "log-Normal - dLOGNO" = function(y) gamlss.dist::dLOGNO(y, mu = input$location, sigma = input$scale),
      "log-Normal - dLOGNO2" = function(y) gamlss.dist::dLOGNO2(y, mu = input$location, sigma = input$scale),
      "Normal Linear Quadratic - dLQNO" = function(y) gamlss.dist::dLQNO(y, mu = input$location, sigma = input$scale),
      "Multinomial - dMN3" = function(y) gamlss.dist::dMN3(y, mu = input$location, sigma = input$scale),
      "Multinomial - dMN4" = function(y) gamlss.dist::dMN4(y, mu = input$location, sigma = input$scale, nu = input$skewness),
      "Multinomial - dMN5" = function(y) gamlss.dist::dMN5(y, mu = input$location, sigma = input$scale, nu = input$skewness),
      "Negative Binomial family - dNBF" = function(y) gamlss.dist::dNBF(y, mu = input$location, sigma = input$scale, nu = input$skewness),
      "Negative Binomial type I - dNBI" = function(y) gamlss.dist::dNBI(y, mu = input$location, sigma = input$scale),
      "Negative Binomial type II - dNBII" = function(y) gamlss.dist::dNBII(y, mu = input$location, sigma = input$scale),
      "Normal Exponential t - dNET" = function(y) gamlss.dist::dNET(y, mu = input$location, sigma = input$scale, nu = input$skewness, tau = input$kurtosis),
      "Normal (mu, sd) - dNO" = function(y) gamlss.dist::dNO(y, mu = input$location, sigma = input$scale),
      "Normal (mu, var) - dNO2" = function(y) gamlss.dist::dNO2(y, mu = input$location, sigma = input$scale),
      "Normal Family - dNOF" = function(y) gamlss.dist::dNOF(y, mu = input$location, sigma = input$scale, nu = input$skewness),
      "Pareto type 2 - dPARETO2" = function(y) gamlss.dist::dPARETO2(y, mu = input$location, sigma = input$scale),
      "Pareto type 2 original - dPARETO2o" = function(y) gamlss.dist::dPARETO2o(y, mu = input$location, sigma = input$scale),
      "Power Exponential - dPE" = function(y) gamlss.dist::dPE(y, mu = input$location, sigma = input$scale, nu = input$skewness),
      "Power Exponential type 2 - dPE2" = function(y) gamlss.dist::dPE2(y, mu = input$location, sigma = input$scale, nu = input$skewness),
      "Poisson inverse Gaussian - dPIG" = function(y) gamlss.dist::dPIG(y, mu = input$location, sigma = input$scale),
      "Poison - dPO" = function(y) gamlss.dist::dPO(y, mu = input$location),
      "Reverse Gumbel - dRG" = function(y) gamlss.dist::dRG(y, mu = input$location, sigma = input$scale),
      "Reverse generalized extreme - dRGE" = function(y) gamlss.dist::dRGE(y, mu = input$location, sigma = input$scale, nu = input$skewness),
      "Skew Power Exponential - dSEP" = function(y) gamlss.dist::dSEP(y, mu = input$location),
      "Skew Power Exponential type 1 - dSEP1" = function(y) gamlss.dist::dSEP1(y, mu = input$location, sigma = input$scale, nu = input$skewness, tau = input$kurtosis),
      "Skew Power Exponential type 2 - dSEP2" = function(y) gamlss.dist::dSEP2(y, mu = input$location, sigma = input$scale, nu = input$skewness, tau = input$kurtosis),
      "Skew Power Exponential type 3 - dSEP3" = function(y) gamlss.dist::dSEP3(y, mu = input$location, sigma = input$scale, nu = input$skewness, tau = input$kurtosis),
      "Skew Power Exponential type 4 - dSEP4" = function(y) gamlss.dist::dSEP4(y, mu = input$location, sigma = input$scale, nu = input$skewness, tau = input$kurtosis),
      "Shash - dSHASH" = function(y) gamlss.dist::dSHASH(y, mu = input$location, sigma = input$scale, nu = input$skewness, tau = input$kurtosis),
      "Shash original - dSHASHo" = function(y) gamlss.dist::dSHASHo(y, mu = input$location, sigma = input$scale, nu = input$skewness, tau = input$kurtosis),
      "Shash original 2 - dSHASHo2" = function(y) gamlss.dist::dSHASHo2(y, mu = input$location, sigma = input$scale, nu = input$skewness, tau = input$kurtosis),
      "Sichel (original) - dSI" = function(y) gamlss.dist::dSI(y, mu = input$location, sigma = input$scale, nu = input$skewness),
      "Sichel (mu as the maen) - dSICHEL" = function(y) gamlss.dist::dSICHEL(y, mu = input$location, sigma = input$scale, nu = input$skewness),
      "Skew Normal Type 1 - dSN1" = function(y) gamlss.dist::dSN1(y, mu = input$location),
      "Skew Normal Type 2 - dSN2" = function(y) gamlss.dist::dSN2(y, mu = input$location, sigma = input$scale),
      "Skew t - dSST" = function(y) gamlss.dist::dSST(y, mu = input$location, sigma = input$scale),
      "Skew t type 1 - dST1" = function(y) gamlss.dist::dST1(y, mu = input$location, sigma = input$scale, nu = input$skewness),
      "Skew t type 2 - dST2" = function(y) gamlss.dist::dST2(y, mu = input$location, sigma = input$scale, nu = input$skewness),
      "Skew t type 3 - dST3" = function(y) gamlss.dist::dST3(y, mu = input$location, sigma = input$scale, nu = input$skewness),
      "Skew t - dST3C" = function(y) gamlss.dist::dST3C(y, mu = input$location, sigma = input$scale),
      "Skew t type 4 - dST4" = function(y) gamlss.dist::dST4(y, mu = input$location, sigma = input$scale, nu = input$skewness),
      "Skew t type 5 - dST5" = function(y) gamlss.dist::dST5(y, mu = input$location, sigma = input$scale, nu = input$skewness),
      "t-distribution - dTF" = function(y) gamlss.dist::dTF(y, mu = input$location, sigma = input$scale, nu = input$skewness),
      "t-distribution - dTF2" = function(y) gamlss.dist::dTF2(y, mu = input$location, sigma = input$scale),
      "Waring - dWARING" = function(y) gamlss.dist::dWARING(y, mu = input$location),
      "Weibull - dWEI" = function(y) gamlss.dist::dWEI(y, mu = input$location, sigma = input$scale),
      "Weibull(PH parameterization) - dWEI2" = function(y) gamlss.dist::dWEI2(y, mu = input$location, sigma = input$scale),
      "Weibull (mu as mean) - dWEI3" = function(y) gamlss.dist::dWEI3(y, mu = input$location, sigma = input$scale),
      "Yule - dYULE" = function(y) gamlss.dist::dYULE(y, mu = input$location),
      "Zero adjusted beta binomial - dZABB" = function(y) gamlss.dist::dZABB(y, mu = input$location, sigma = input$scale, nu = input$skewness, bd = input$binomial_denominators),
      "Zero adjusted binomial - dZABI" = function(y) gamlss.dist::dZABI(y, mu = input$location, sigma = input$scale, bd = input$binomial_denominators),
      "Zero adjusted beta neg. bin. - dZABNB" = function(y) gamlss.dist::dZABNB(y, mu = input$location, sigma = input$scale, nu = input$skewness, tau = input$kurtosis),
      "Zero adjusted Gamma - dZAGA" = function(y) gamlss.dist::dZAGA(y, mu = input$location, sigma = input$scale, nu = input$skewness),
      "Zero adjusted IG - dZAIG" = function(y) gamlss.dist::dZAIG(y, mu = input$location, sigma = input$scale),
      "Zero adjusted logarithmic - dZALG" = function(y) gamlss.dist::dZALG(y, mu = input$location, sigma = input$scale),
      "Zero adjusted neg. bin. - dZANBI" = function(y) gamlss.dist::dZANBI(y, mu = input$location, sigma = input$scale, nu = input$skewness),
      "Zero adjusted poisson - dZAP" = function(y) gamlss.dist::dZAP(y, mu = input$location, sigma = input$scale),
      "Zero adjusted PIG - dZAPIG" = function(y) gamlss.dist::dZAPIG(y, mu = input$location, sigma = input$scale, nu = input$skewness),
      "Zero adjusted Sichel - dZASICHEL" = function(y) gamlss.dist::dZASICHEL(y, mu = input$location, sigma = input$scale, nu = input$skewness, tau = input$kurtosis),
      "Zero adjusted Zipf - dZAZIPF" = function(y) gamlss.dist::dZAZIPF(y, mu = input$location, sigma = input$scale),
      "Zero inflated beta binomial - dZIBB" = function(y) gamlss.dist::dZIBB(y, mu = input$location, sigma = input$scale, nu = input$skewness, bd = input$binomial_denominators),
      "Zero inflated binomial - dZIBI" = function(y) gamlss.dist::dZIBI(y, mu = input$location, sigma = input$scale, bd = input$binomial_denominators),
      "Zero inflated beta neg. bin. - dZIBNB" = function(y) gamlss.dist::dZIBNB(y, mu = input$location, sigma = input$scale, nu = input$skewness, tau = input$kurtosis),
      "Zero inflated neg. bin. family - dZINBF" = function(y) gamlss.dist::dZINBF(y, mu = input$location, sigma = input$scale, nu = input$skewness),
      "Zero inflated neg. bin. - dZINBI" = function(y) gamlss.dist::dZINBI(y, mu = input$location, sigma = input$scale, nu = input$skewness),
      "Zero inflated poisson - dZIP" = function(y) gamlss.dist::dZIP(y, mu = input$location, sigma = input$scale),
      "Zero inf. poiss.(mu as mean) - dZIP2" = function(y) gamlss.dist::dZIP2(y, mu = input$location, sigma = input$scale),
      "Zipf - dZIPF" = function(y) gamlss.dist::dZIPF(y, mu = input$location),
      "Zero inflated PIG - dZIPIG" = function(y) gamlss.dist::dZIPIG(y, mu = input$location, sigma = input$scale, nu = input$skewness),
      "Zero inflated Sichel - dZISICHEL" = function(y) gamlss.dist::dZISICHEL(y, mu = input$location, sigma = input$scale, nu = input$skewness, tau = input$kurtosis),

      # default: "Normal (mu, var) - dNO2"
      function(y) gamlss.dist::dNO2(y, mu = input$location, sigma = input$scale)
    )
  })
  # gamlss-functions with access of variables
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
                   "Normal (mu, sd) - dNO" = gamlss.dist::dNO,
                   "Normal (mu, var) - dNO2" = gamlss.dist::dNO2,
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
                   "Zero inflated Sichel - dZISICHEL" = gamlss.dist::dZISICHEL,

                   gamlss.dist::dNO2 # default: "Normal (mu, var) - dNO2"
    )
  })
  # maximum-likelihood-estimates for standard values
  mle <- reactive({
    # without data
    if (is.null(datasetInput())) {
      return()
      # with data
    } else {
      # Access value of chosen variable from dataset
      plot.obj <<- list()
      plot.obj$data <<- datasetInput()
      plot.obj$variable <<- with(data = plot.obj$data,
                                 expr = get(input$variable))
      default.parameter <- c(
        shinydistributions::distributions[shinydistributions::distributions$dist_density == input$dist,
                      "default_location"],
        shinydistributions::distributions[shinydistributions::distributions$dist_density == input$dist,
                      "default_scale"],
        shinydistributions::distributions[shinydistributions::distributions$dist_density == input$dist,
                      "default_skewness"],
        shinydistributions::distributions[shinydistributions::distributions$dist_density == input$dist,
                      "default_kurtosis"]
      )
      default.parameter[is.na(default.parameter)] <- 0
      binomial_denominator <- shinydistributions::distributions[
        shinydistributions::distributions$dist_density == input$dist,
        "default_binomial_denominators"]
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
      if ((shinydistributions::distributions[shinydistributions::distributions$dist_density == input$dist,
                         "discrete_flag"] == 0) &&
      #  is.integer(plot.obj$variable)) {
         (length(unique(plot.obj$variable))/length(plot.obj$variable) <= 0.1)) {
        showNotification(
          ui = "Input variable is considered as discrete while using a
          continuous function.",
          type = "warning"
        )
        mle <- optim(par = default.parameter, fn = log.likelihood,
                     data = plot.obj$variable,
                     lower = c(0.000001, 0.000001, 0.000001, 0.000001),
                     method = "L-BFGS-B")
        mle <- mle$par
      # functions: discrete, data: continuous
      } else if ((shinydistributions::distributions[shinydistributions::distributions$dist_density == input$dist,
                                "discrete_flag"] == 1) &&
      #          !is.integer(plot.obj$variable)) {
        (length(unique(plot.obj$variable)) / length(plot.obj$variable) > 0.1)) {
        showNotification(
          ui = "You are using continuous data with a discrete function.",
          type = "error"
        )
        mle <- return()
        #mle <- default.parameter
      # function type and data type are equal
      } else {
        mle <- optim(par = default.parameter, fn = log.likelihood,
                     data = plot.obj$variable,
                     lower = c(0.000001, 0.000001, 0.000001, 0.000001),
                     method = "L-BFGS-B")
        mle <- mle$par
      }
    }
  })
  # Update parameters (location, scale, skewness, kurtosis, binomial
  # denominators) by default values of particular density function

  # Location
  output$num_location <- renderUI({
    validate(
      need(expr = input$dist != "",
           message = "There is no mixed density function with one parameter.")
    )
    # without data
    if ( is.null(datasetInput()) ) {
      numericInput(
        inputId = "location",
        label = "Location",
        value = shinydistributions::distributions[shinydistributions::distributions$dist_density == input$dist,
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
        value = if (is.null(mle()[1])) {
          shinydistributions::distributions[shinydistributions::distributions$dist_density == input$dist,
                        "default_location"]
        } else {
          round(mle()[1], digits = 2)
        }
      )
    }
  })
  # Scale
  output$num_scale <- renderUI({
    validate(
      need(expr = input$dist != "",
           message = "")
    )
    # Hide if scale is not a parameter of the density function
    if (is.na(shinydistributions::distributions[shinydistributions::distributions$dist_density == input$dist,
                                                 "default_scale"])) {
      return()
    }
    # without data
    if ( is.null(datasetInput()) ) {
      numericInput(
        inputId = "scale",
        label = "Scale",
        value = shinydistributions::distributions[shinydistributions::distributions$dist_density == input$dist,
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
        value = round(mle()[2], digits = 2),
        step = 0.1
      )
    }
  })
  # Skewness
  output$num_skewness <- renderUI({
    validate(
      need(expr = input$dist != "",
           message = "")
    )
    # Hide if skewness is not a parameter of the density function
    if (is.na(shinydistributions::distributions[shinydistributions::distributions$dist_density == input$dist,
                            "default_skewness"])) {
      return()
    }
    # without data
    if (is.null(datasetInput())) {
      numericInput(
        inputId = "skewness",
        label = "Skewness",
        value = shinydistributions::distributions[shinydistributions::distributions$dist_density == input$dist,
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
        value = round(mle()[3], digits = 2)
      )
    }
  })
  # Kurtosis
  output$num_kurtosis <- renderUI({
    validate(
      need(expr = input$dist != "",
           message = "")
    )
    # Hide if kurtosis is not a parameter of the density function
    if (is.na(shinydistributions::distributions[shinydistributions::distributions$dist_density == input$dist,
                            "default_kurtosis"])) {
      return()
    }
    # without data
    if (is.null(datasetInput())) {
      numericInput(
        inputId = "kurtosis",
        label = "Kurtosis",
        value = shinydistributions::distributions[shinydistributions::distributions$dist_density == input$dist,
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
        value = round(mle()[4], digits = 2)
      )
    }
  })
  # Binomial denominators
  output$num_binomial_denominators <- renderUI({
    validate(
      need(expr = input$dist != "",
           message = "")
    )
    # Hide if binomial denominators is not a parameter of the density function
    if (is.na(shinydistributions::distributions[shinydistributions::distributions$dist_density == input$dist,
                            "default_binomial_denominators"])) {
      return()
    }
    binomial_denominator <- shinydistributions::distributions[
      shinydistributions::distributions$dist_density == input$dist, "default_binomial_denominators"]
    numericInput(
      inputId = "binomial_denominators",
      label = "Binomial denominators",
      value = ifelse(test = input$max_x <= binomial_denominator,
                     yes = binomial_denominator, no = input$max_x),
      min = input$max_x
    )
  })

  # Plot

  # Plot Type (Histogram or Empiric Density Curve)
  output$plot_type <- renderUI({
    if ( is.null(datasetInput()) ) {
      return()
    } else {
      selectInput(
        inputId = "plot_type",
        label = "Plot Type",
        choices = list("Histogram", "Empiric Density Curve")
      )
    }
  })

  # Number of Bins
  output$num_bins <- renderUI({
    if (is.null(datasetInput()) || input$plot_type == "Empiric Density Curve") {
      return()
    }
    plot.obj <<- list()
    plot.obj$data <<- datasetInput()
    plot.obj$variable <<- with(data = plot.obj$data, expr = get(input$variable))

    numericInput(
      inputId = "num_bins",
      label = "Number of Bins for Histogram",
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
    if ( is.null(datasetInput()) ) {
      numericInput(
        inputId = "max_x",
        label = "Maximum of X-axis",
        value = 5
      )
      # with data
    } else {
      plot.obj <<- list()
      plot.obj$data <<- datasetInput()
      plot.obj$variable <<- with(data = plot.obj$data,
                                 expr = get(input$variable))
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
        value = -5
      )
      # with data
    } else {
      plot.obj <<- list()
      plot.obj$data <<- datasetInput()
      plot.obj$variable <<- with(data = plot.obj$data,
                                 expr = get(input$variable))
      numericInput(
        inputId = "min_x",
        label = "Minimum of X-axis",
        value = round(min(plot.obj$variable), digits = 2)
      )
    }
  })
  # Generate plot of the data
  output$plot <- renderUI({
    plotOutput(outputId = "p")
  })

  output$p <- renderPlot({
    validate(
      need(expr = input$dist != "",
           message = "")
    )
    # Define selected distribution as "dist" for usage within renderPlot
    dist <- input$dist

    # Define limits of x-axis
    x.limits <- c(input$min_x, input$max_x)

    # Set limits of x and y-axis
    axes.limits <- ggplot2::coord_cartesian(xlim = x.limits,
                                            ylim = c(0, input$max_y),
                                            expand = FALSE)
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
    # Add theoretical density curve for discrete /
    # continuous or mixed distributions

    # discrete_flag != 1:
    # continuous or mixed (0, 2 respectively)
    density.curve <- if (shinydistributions::distributions[shinydistributions::distributions$dist_density == dist,
                                       "discrete_flag"] != 1) {
      ggplot2::stat_function(
        mapping = ggplot2::aes(x = x.limits, size = "0.1"),
        fun = d_funct(), colour = "maroon",
        show.legend = FALSE)
      # discrete_flag == 1: discrete
    } else {
      ggplot2::stat_function(
        mapping = ggplot2::aes(x = x.limits, size = "0.1"),
        fun = d_funct(),
        n = (input$max_x - input$min_x + 1),
        colour = "maroon", show.legend = FALSE)
    }

    ################
    # without data #
    ################
    if (is.null(datasetInput())) {

      # ggplot
      ggplot2::ggplot() +

        # Label axes
        ggplot2::labs(x = NULL, y = "Probability Density",
                      title = shinydistributions::distributions[shinydistributions::distributions$dist_density == dist,
                                            "distribution"]) +
        axes.limits +
        .theme +
        # Add Theoretical Density Curve
        density.curve

      #############
      # with data #
      #############
    } else if (!is.null(datasetInput())) {

      # Access value of chosen variable from dataset
      plot.obj <<- list()
      plot.obj$data <<- datasetInput()
      plot.obj$variable <<- with(plot.obj$data, get(input$variable))

      # Use selected plot type (histogram or empiric density curve)
      # colourblindfriendly colours
      plot_type <- switch (
        EXPR = input$plot_type,
        "Histogram" = ggplot2::geom_histogram(
          mapping = ggplot2::aes(x = plot.obj$variable, y = ..density..),
          data = plot.obj$data,
          alpha = 0.5,
          bins = input$num_bins,
          colour = "#000000",
          fill = "#56B4E9"
        ),
        "Empiric Density Curve" = ggplot2::geom_density(
          mapping = ggplot2::aes(x = plot.obj$variable),
          data = plot.obj$data,
          alpha = 0.5,
          colour = "#000000",
          fill = "#56B4E9"
        )
      )
      # ggplot
      ggplot2::ggplot() +

        # Add Histogramm or Empiric Density Curve
        plot_type +

        # Label axes
        ggplot2::labs(x = input$variable, y = "Probability Density",
                      title = shinydistributions::distributions[shinydistributions::distributions$dist_density == dist,
                                            "distribution"]) +
        axes.limits +
        .theme +
        # Add Theoretical Density Curve
        density.curve

    }
  })
  # Headline above plot dependent on plot type (histogram or empiric density)
  output$caption <- renderText({
    # without data
    if (is.null(datasetInput())) {
      "Theoretic Density Curve"
      # with data
    } else {
      switch(EXPR = input$plot_type,
             "Histogram" = "Histogram of Data and Theoretic Density Curve",
             "Empiric Density Curve" = "Empiric and Theoretic Densities")
    }
  })
  # Info text on main panel below plot

  # Default Parameter(s) of Theoretic Density Function
  output$info_text <- renderText({
    validate(
      need(expr = input$dist != "",
           message = "")
    )
    dist <- input$dist

    paste(
      "Location = ", shinydistributions::distributions[shinydistributions::distributions$dist_density == dist,
                                                        "default_location"],
      ifelse(is.na(shinydistributions::distributions[
        shinydistributions::distributions$dist_density == dist, "default_scale"]), '',
        paste("; Scale = ",
              shinydistributions::distributions[shinydistributions::distributions$dist_density == dist,
                            "default_scale"], sep = '')),
      ifelse(is.na(shinydistributions::distributions[
        shinydistributions::distributions$dist_density == dist, "default_skewness"]), '',
        paste("; Skewness = ",
              shinydistributions::distributions[shinydistributions::distributions$dist_density == dist,
                            "default_skewness"], sep = '')),
      ifelse(is.na(shinydistributions::distributions[
        shinydistributions::distributions$dist_density == dist, "default_kurtosis"]), '',
        paste("; Kurtosis = ",
              shinydistributions::distributions[shinydistributions::distributions$dist_density == dist,
                            "default_kurtosis"], sep = '')),
      ifelse(is.na(shinydistributions::distributions[shinydistributions::distributions$dist_density == dist,
                                 "default_binomial_denominators"]), '',
             paste("; Binomial denominators = ",
                   shinydistributions::distributions[shinydistributions::distributions$dist_density == dist,
                                 "default_binomial_denominators"], sep = '')),
      sep = '')
  })

  # Headline Maximum Likelihood Estimates
  output$head_ml_estimates <- renderText({
    if (is.null(datasetInput())) {
      return()
    } else if ( is.null(mle()) ) {
      return()
    } else {
      "Maximum Likelihood Estimate(s) of Parameter(s):"
    }
  })
  # Maximum Likelihood Estimates of Parameters
  output$info_text2 <- renderText({
    if ( is.null(datasetInput()) ) {
      return()
    } else if ( is.null(mle()) ) {
      return()
    } else {
      dist <- input$dist

      paste(
        "Location = ", round(mle()[1], digits = 2),

        ifelse(is.na(shinydistributions::distributions[
          shinydistributions::distributions$dist_density == dist, "default_scale"]), '',
          paste("; Scale = ", round(mle()[2], digits = 2), sep = '')),

        ifelse(is.na(shinydistributions::distributions[
          shinydistributions::distributions$dist_density == dist, "default_skewness"]), '',
          paste("; Skewness = ", round(mle()[3], digits = 2), sep = '')),

        ifelse(is.na(shinydistributions::distributions[
          shinydistributions::distributions$dist_density == dist, "default_kurtosis"]), '',
          paste("; Kurtosis = ", round(mle()[4], digits = 2), sep = '')),
        sep = '')
    }
  })
}

shiny::shinyApp(ui = ui, server = server)
