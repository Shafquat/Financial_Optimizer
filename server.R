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
      })
    })
  })

  # when button click, collate a list of stocks
  list_of_stocks <- observeEvent(input$get, {
    observe({
      num_stocks <- input$morestocks
      # Enter atleast one stock into the list or else ask user to input a stock
      stock_list2 <- c(input[[paste0("symb", 1)]])
      # Iterate over the rest of the stocks and add them to the list if they are not empty
      lapply(2:(num_stocks+4), function(i){
        if (input[[paste0("symb", i)]] != ""){
          # Store value in previously declared list outside the lappy func using "<<-"
          stock_list2[[length(stock_list2)+1]] <<- input[[paste0("symb", i)]]
        }
      })
      return(stock_list2)
    })
  })
  
  # wait until optimize button is pressed
  observeEvent(input$get,{
    
    #download data from yahoo finance
    mylist <- lapply(list_of_stocks, function(x){
      try(getSymbols(x, src = 'yahoo', from = input$start, to = input$end, auto.assign = FALSE))
    })
    
    # takes in the following parameters: list_of_stocks, input$riskfree_rate, input$short, input$min_portfolio, input$max_portfolio
    folio <- MVO(ticker = list_of_stocks, mylist = mylist, wmax = 1, nports = 20, shorts = TRUE, rf = input$riskfree_rate)
    
    # outputs a chart
    output$plot1 <- renderPlot({
      plot(folio$vol, folio$ret, main = "MVO", type = "l", xlab = "Variance", ylab = "Returns")
    })
  })
  
  # gives coordinates on chart
  output$info <- renderText({
    paste0("Expected Return: ", input$plot_click$x, "\nVariance: ", input$plot_click$y)})
})