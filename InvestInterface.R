#install shiny

#remove "#" to install shiny for first time

#options(repos = c(CRAN = "http://cran.rstudio.com"))
#install.packages('shiny')

#load shiny package
library(shiny)

#Define User Interface and create a responsive page
ui <- fluidPage(
	# *Input() functions
	sliderInput(inputId = "num", label = "Choose a number", value = 50, min = 1, max = 100),
	radioButtons(inputId = "model", label = "Choose an optimization model", choices = c("MAD" = "MAD","MVO" = "MVO")),
	
	# *output() functions
	plotOutput(outputId = "hist")	

)

#define server
server <- function(input, output) {
  #save frontier object to output
  output$hist <- renderPlot({
	#render a histogram of number of values in num
	hist(rnorm(input$num))
  })
}


#call shiny app
shinyApp(ui = ui, server = server)