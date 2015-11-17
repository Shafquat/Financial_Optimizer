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
shinyServer(function(input, output, session) {
  
  # Change default value of minimum portfolio to -1 if shorting is selected
  observe({
    c_box <- input$short
    updateNumericInput(session,"min_portfolio",value = (c_box %% 2)*-1)
  })
  
  # Change default value of minimum portfolio to -1 if shorting is selected
  observe({
    num_stocks <- input$morestocks
    output$newrow <- renderUI({
    lapply(5:(num_stocks+4), function(i){
      textInputRow(inputId = paste0("symb", i),"","")
    }
           )
    })
  })
  
  
  
  # when button click, retrieves stock information
  list_of_stocks <- reactive({
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
  observeEvent(input$get,{
    # outputs a chart
    output$plot1 <- renderPlot({
      # wait until optimize button is pressed
      
      chartSeries(list_of_stocks()[[1]], name = input$symb1, theme = chartTheme("white"), 
                  type = "line", TA = NULL)
    })
  })
  
  # gives coordinates on chart
  output$info <- renderText({
    paste0("Expected Return: ", input$plot_click$x, "\nVariance: ", input$plot_click$y)})
})