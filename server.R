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
  
  

  # when button click, collate a list of stocks
  list_of_stocks <- observeEvent(input$get, {
    observe({
      num_stocks <- input$morestocks
      # Enter atleast one stock into the list 
      stock_list2 = c(input[[paste0("symb", 1)]])
      # Iterate over the rest of the stocks and add them to the list if they are not empty
      lapply(2:(num_stocks+4), function(i){
        if (input[[paste0("symb", i)]] != ""){
          print(input[[paste0("symb", i)]])
          # ************this is not working...*************
          stock_list2 <- c(stock_list2,input[[paste0("symb", i)]])
        }
      })
      print(stock_list2)
      return(stock_list2)
    })
  })
  
  observeEvent(input$get,{
    # Call the respective model function ***ANDREW***
    
    # outputs a chart
    output$plot1 <- renderPlot({
      # wait until optimize button is pressed
      
      chartSeries(LISTOFSTOCKS, name = input$symb1, theme = chartTheme("white"), 
                  type = "line", TA = NULL)
    })
  })
  
  # gives coordinates on chart
  output$info <- renderText({
    paste0("Expected Return: ", input$plot_click$x, "\nVariance: ", input$plot_click$y)})
})