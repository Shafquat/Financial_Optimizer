library(shiny)
library(quantmod)

textInputRow<-function (inputId, label, value = "") 
{
  div(style="display:inline-block",
      tags$label(label, 'for' = inputId), 
      tags$input(id = inputId, type = "text", value = value,class="input-small"))
}

# Define server logic for random distribution application
shinyServer(function(input, output) {

  # Add new text inputs
  observeEvent(input$addmore,{
    output$newrow <- renderUI({
      textInputRow("symb4", "", "")})
  })
})