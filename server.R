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
  
  # acquiring data
  dataInput <- reactive({
    if (input$get == 0)
      return(NULL)
    
    return(isolate({
      getSymbols(input$symb1,src="yahoo", auto.assign = FALSE)
    }))
  })
  
  datesInput <- reactive({
    if (input$get == 0)
      return(NULL)
    
    return(isolate({
      paste0(input$dates[1], "::",  input$dates[2])
    }))
  })
  
  returns <- reactive({ 
    if (input$get == 0)
      return(NULL)
    
    dailyReturn(dataInput())
  })
  
  xs <- reactive({ 
    if (input$get == 0)
      return(NULL)
    
    span <- range(returns())
    seq(span[1], span[2], by = diff(span) / 100)
  })
  
  # Add new text inputs
  observeEvent(input$addmore,{
    output$newrow <- renderUI({
      textInputRow("symb4", "", "")})
  })

  
  
  # tab based controls
  output$newBox <- renderUI({
    switch(input$tab,
           "Charts" = chartControls
    )
  })
  
  # Charts tab
  chartControls <- div(
    wellPanel(
      selectInput("chart_type",
                  label = "Chart type",
                  choices = c("Bar" = "bars",
                              "Line" = "line"),
                  selected = "Line"
      )
    ),
    
    wellPanel(
      p(strong("Technical Analysis")),
      
      br(),
      
      actionButton("chart_act", "Add Technical Analysis")
    )
  )
  
  TAInput <- reactive({
    if (input$chart_act == 0)
      return("NULL")
    
    tas <- isolate({c(input$ta_vol, input$ta_sma, input$ta_ema, 
                      input$ta_wma,input$ta_bb, input$ta_momentum)})
    
    if (any(tas)) funcs[tas]
    else "NULL"
  })
  
  output$chart <- renderPlot({
    chartSeries(dataInput(),
                name = input$symb1,
                type = input$chart_type,
                subset = datesInput(),
                theme = "white",
                TA = TAInput())
  })
})