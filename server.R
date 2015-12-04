library(shiny)
library(quantmod)
source("MVO.R")
source("CVaR.R")
source("MAD.R")
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
  
  # Add Alpha Slider if CVAR is picked
    output$alpha_selection <- renderUI({
      if(input$model == "CVaR"){
      sliderInput(inputId = "alpha", label = "Select Alpha", min = 0.01, max = 0.1, value = 0.05, step = 0.01)
      } 
    })
  
  # Change the number of new rows based on user input
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
      if (input$model == "MAD"){
        # takes in the following parameters: list_of_stocks, input$riskfree_rate, input$short, input$min_portfolio, input$max_portfolio
        folio <- MAD(ticker = stock_list2, mylist = mylist, wmax = input$max_portfolio, nports = 20, shorts = input$short, rf = input$riskfree_rate)
      }else if(input$model == "CVaR"){
        folio <- CVaR(ticker = stock_list2, mylist = mylist, wmax = input$max_portfolio, nports = 20, shorts = input$short, rf = input$riskfree_rate, alpha = input$alpha)
      }else{
      # takes in the following parameters: list_of_stocks, input$riskfree_rate, input$short, input$min_portfolio, input$max_portfolio
      folio <- MVO(ticker = stock_list2, mylist = mylist, wmax = input$max_portfolio, nports = 20, shorts = input$short, rf = input$riskfree_rate)
      }
      # outputs a chart
      output$plot1 <- renderPlot({
        plot(folio$vol, folio$ret, main = input$model, type = "l", xlab = "Variance", ylab = "Returns")
      })
      
      # Get Investment amount from weights
      folio$weights <- folio$weights * input$investment_amount
      # Add the Returns to the weights
      X <- (data.frame(folio$ret,folio$weights))
      # Rename the headers of the first two columns of the data frame
      colnames(X)[1] <- "Expected Return"
      
      # gives coordinates on chart
      output$info <- renderText({
        if (length(input$plot_click$x) != 1 ){
          error_string <-"Please click on the Efficient Frontier"
          return(error_string)
        }
        
        min_distance <- (which(abs(X[1]-input$plot_click$y)==min(abs(X[1]-input$plot_click$y))))
        # Return the Variance on plot and the closest value to Expected Return Possible
        original_string <- paste0("Variance:", lapply(input$plot_click$x,round,4),
                                  "\nExpected Return:", lapply(X[[min_distance,1]],round,4),
                                  "\n" )
        for(i in 2:(ncol(X))){
          original_string <- paste0(original_string,colnames(X)[i], ": ", lapply(X[[min_distance, i]],round,2),"\n")
        }
        original_string
      })
    })
    
  })
  
  session$onSessionEnded(function() {
    stopApp()
  })
})