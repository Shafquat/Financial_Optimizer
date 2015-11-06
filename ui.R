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
    textInputRow("symb5", ""),
    textInputRow("symb6", ""),
    textInputRow("symb7", ""),
    textInputRow("symb8", ""),
    textInputRow("symb9", ""),
    uiOutput("newrow"),
    
    actionButton("addmore", "Add more stocks"),
    
    dateRangeInput("dates", 
                   "Please select the historic range of returns",
                   start = Sys.Date()-365, end = Sys.Date()),
    
    actionButton("get", "Optimize!"),
    br(),
    br(),
    
    uiOutput("newBox")
    
    ),
  
  # Show a tabset that includes a plot, summary, and table view
  # of the generated distribution
  mainPanel(
    plotOutput("plot1", click = "plot_click"),
    verbatimTextOutput("info"),
    column(11,
           dataTableOutput('table')
    )
  )
))