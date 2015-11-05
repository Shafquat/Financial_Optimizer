library(shiny)
library(quantmod)

# CSS for Stock Rows
textInputRow<-function (inputId, label, value = "") 
{
  div(style="display:inline-block",
      tags$label(label, 'for' = inputId), 
      tags$input(id = inputId, type = "text", value = value,class="input-small"))
}

# Define server logic for random distribution application
shinyServer(function(input, output) {
  
  # Output of a plot
  #output$plot1 <- renderPlot({
  #  plot(mtcars$wt, mtcars$mpg)
  #})
  
  # Add one new text input THIS NEEDS TO BE RECURRING
  observeEvent(input$addmore,{
    output$newrow <- renderUI({
      textInputRow("symb10", "", "")})
  })
  
  #Stock Input Procedure NOT SURE IF THIS WORKS... Hardcoded
  #ticker = c(input$symb1, input$symb2, input$symb3)
  
  # when button click, retrieves stock information
  dataInput <- reactive({
    if (input$get == 0)
      return(NULL)

    return(isolate({
    getSymbols(input$symb1, src = 'yahoo', 
                from = input$dates[1], 
                to = input$dates[2], 
                auto.assign = FALSE)
    }))
  })
  
  
  # outputs a chart
  output$plot1 <- renderPlot({    
    chartSeries(dataInput(), theme = chartTheme("white"), 
                type = "line", TA = NULL)
  })
  
  # gives coordinates on chart
  output$info <- renderText({
    paste0("Expected Return: ", input$plot_click$x, "\nVariance: ", input$plot_click$y)})
})