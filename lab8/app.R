setwd("~/Desktop/Fall_2016/Stat159/Stat159Labs/lab8/")
advertising = read.csv('Advertising.csv')
library(ggplot2)
library(stringr)
library(shiny)

ui = fluidPage(
  selectInput(inputId = 'channel', label = 'Choose a channel', choices = c('TV', 'Radio', 'Newspaper')),
  plotOutput('scatterplot')
  
)
server = function(input, output) {
  output$scatterplot = renderPlot({
    name = input$channel
    if (name == 'TV') {
      predictor = advertising$TV
    } else if (name == 'Radio') {
      predictor = advertising$Radio
    } else if (name == 'Newspaper') {
      predictor = advertising$Newspaper
    }
    result = advertising$Sales
    fit = lm(result~predictor)
    modelInfo = summary(fit)
    intercept = modelInfo$coefficients[1]
    slope = modelInfo$coefficients[2]
    plot(x = predictor, y = result, xlab = input$channel, ylab = 'Sales', main = 'Scattor plot for Simple Regression')
    abline(a = intercept, b = slope, col = 'red')
  })
}

shinyApp(ui = ui, server = server)