library(shiny)

textInputRow<-function (inputId, label, value = "") 
{
  div(style="display:inline-block",
      tags$label(label, 'for' = inputId), 
      tags$input(id = inputId, type = "text", value = value,class="input-small"))
}

# Define UI for randomdistribution application 
shinyUI(pageWithSidebar(
  
  headerPanel("Stock Optimization"),
  
  sidebarPanel(
    radioButtons(inputId = "model", label = "Choose an optimization model", choices = c("MVO" = "MVO","MAD" = "MAD","Black Litterman" = "BL","CVAR" = "CVaR"), inline = TRUE),

    helpText("Please select stocks for your portfolio"),

    textInputRow("symb1", "", "FB"),
    textInputRow("symb2", "", "F"),
    textInputRow("symb3", "", "TSLA"),
    textInputRow("symb4", ""),
    numericInput("morestocks","# of stocks to add","1",min = 1),
    uiOutput("newrow"),
    
    dateRangeInput("dates", 
                   "Please select the historic range of returns",
                   start = Sys.Date()-365, end = Sys.Date()),
    
    numericInput("riskfree_rate","Please enter the current risk free rate",0.05, step = 0.01),
    checkboxInput("short","Allow shorting",FALSE),
    numericInput("min_portfolio","Minimum weight amount per stock",0, step = 0.01, min = -1, max = 1),
    numericInput("max_portfolio","Maximum weight amount per stock",1, step = 0.01, min = -1, max = 1),
    
    actionButton("get", "Optimize!"),
    br(),
    br(),
    
    uiOutput("newBox")
    
    ),
  
  # Show a tabset that includes a plot, summary, and table view
  # of the generated distribution
  mainPanel(
    plotOutput("plot1", click = "plot_click"),
    verbatimTextOutput("info")
    )
  )
)