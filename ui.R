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
    radioButtons(inputId = "model", label = "Choose an optimization model", choices = c("MAD" = "MAD","MVO" = "MVO","Black Litterman" = "BL","CVAR" = "CVaR"), inline = TRUE),

    helpText("Please select stocks for your portfolio"),

    textInputRow("symb", "", "FB"),
    textInputRow("symb1", "", "F"),
    textInputRow("symb2", "", "TSLA"),
    
    actionButton("addmore", "Add more stocks"),
    
    dateRangeInput("dates", 
                   "Please select the historic range of returns",
                   start = "2013-01-01", end = "2014-09-05"),
    
    actionButton("get", "Optimize!"),
    br(),
    br(),
    
    uiOutput("newBox")
    
    ),
  
  # Show a tabset that includes a plot, summary, and table view
  # of the generated distribution
  mainPanel(
    tabsetPanel(
      tabPanel("Charts", plotOutput("chart")), 
      id = "tab"
    )
  )
))