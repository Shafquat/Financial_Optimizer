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
  
  # Add one new text input THIS NEEDS TO BE RECURRING
  observeEvent(input$addmore,{
    output$newrow <- renderUI({
      textInputRow("symb5", "", "")})
  })
  
  # when button click, retrieves stock information
  dataInput <- reactive({
    # wait until optimize button is pressed
    if (input$get == 0)
      return(NULL)
    
    data1 <- getSymbols(input$symb1, src = "yahoo",      #Seperated the data into two seperate data sets and set auto.assign=FALSE
                        from = input$dates[1],
                        to = input$dates[2],
                        auto.assign = FALSE)
    data2 <- getSymbols(input$symb2, src = "yahoo",     #Seperated the data into two seperate data sets and set auto.assign=FALSE
                        from = input$dates[1],
                        to = input$dates[2],
                        auto.assign = FALSE)
    data3 <- getSymbols(input$symb3, src = "yahoo",      #Seperated the data into two seperate data sets and set auto.assign=FALSE
                        from = input$dates[1],
                        to = input$dates[2],
                        auto.assign = FALSE)
    data4 <- getSymbols(input$symb4, src = "yahoo",     #Seperated the data into two seperate data sets and set auto.assign=FALSE
                        from = input$dates[1],
                        to = input$dates[2],
                        auto.assign = FALSE)
    data5 <- getSymbols(input$symb5, src = "yahoo",     #Seperated the data into two seperate data sets and set auto.assign=FALSE
                        from = input$dates[1],
                        to = input$dates[2],
                        auto.assign = FALSE)
    return (list(data1,data2,data3,data4,data5)) 
      
  })
  
  output$table <- renderDataTable(dataInput())
  
  # outputs a chart
  output$plot1 <- renderPlot({    
    chartSeries(dataInput()[[1]], name = input$symb1, theme = chartTheme("white"), 
                type = "line", TA = NULL)
  })
  
  # gives coordinates on chart
  output$info <- renderText({
    paste0("Expected Return: ", input$plot_click$x, "\nVariance: ", input$plot_click$y)})
})