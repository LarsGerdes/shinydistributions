# shinydistributions
A shiny app to show which distribution fits best.

## Getting Started
1. Installation: devtools::install_github("LarsGerdes/shinydistributions")
2. Start app: shinydistributions::launch()

## Web Application
(http://gerdes.shinyapps.io/server) (App is not optimized for server version)

## Usage
In the "Plot Distribution" window you can plot the density function of any distribution and adjust their parameters.  

The "Data Upload" window can be used to upload data. Alternatively to your own data you can select a R-Dataset for test purposes. In the "Plot Distribution" window will now be shown a histogram of the data, which can be compared to any distribution. The standard parameters are calculated with maximum likelihood estimation.  

The "Properties of Distribution" window shows properties of the available distributions. The source of those and of the used density functions is the gamlss.dist package. This includes the errors of the package.  

(https://github.com/LarsGerdes/shinydistributions/blob/master/screenshot.png)
