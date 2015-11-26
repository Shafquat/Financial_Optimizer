library(shiny)
library(quantmod)
source("MVO.R")
require(tseries)
library(fPortfolio)

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

  # get number of new stocks when button is pressed
  num_stocks_caller <- eventReactive(input$get, {
    input$morestocks
  })

  # when button click, collate a list of stocks
  observeEvent(input$get, {
    observe({
      num_stocks <- num_stocks_caller()
      # Enter atleast one stock into the list or else ask user to input a stock
      stock_list2 <- c(input[[paste0("symb", 1)]])
      # Iterate over the rest of the stocks and add them to the list if they are not empty
      lapply(2:(num_stocks+4), function(i){
        if (input[[paste0("symb", i)]] != ""){
          # Store value in previously declared list outside the lappy func using "<<-"
          stock_list2[[length(stock_list2)+1]] <<- input[[paste0("symb", i)]]
        }
      })

      #download data from yahoo finance
      mylist <- lapply(stock_list2, function(x){
        try(getSymbols(x, src = 'yahoo', from = input$dates[1], to = input$dates[2], auto.assign = FALSE))
      })

      # takes in the following parameters: list_of_stocks, input$riskfree_rate, input$short, input$min_portfolio, input$max_portfolio
      folio <- MVO(ticker = stock_list2, mylist = mylist, wmax = 1, nports = 20, shorts = input$short, rf = input$riskfree_rate)

      # outputs a chart
      output$plot1 <- renderPlot({
        plot(folio$vol, folio$ret, main = "MVO", type = "l", xlab = "Variance", ylab = "Returns")
      })
      
      # Get Investment amount from weights
      folio$weights <- folio$weights * input$investment_amount
      # Add the Returns to the weights
      X <- (data.frame(folio$ret,folio$weights))
      # Rename the headers of the first two columns of the data frame
      colnames(X)[1] <- "Expected Return"
      # Round all the values in X to 4 digits beyond decimal
      is.num <- sapply(X, is.numeric)
      X[is.num] <- lapply(X[is.num], round, 4)
      # Create a table for weights
      output$mytable1 <- renderDataTable({
        X
      })
    })
  })
  
  # gives coordinates on chart
  output$info <- renderText({
    paste0("Variance:", input$plot_click$x, "\nExpected Return:", input$plot_click$y)})
  

})
