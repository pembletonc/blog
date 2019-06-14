library(shiny)

ui <- fluidPage(sliderInput(inputId = "num",
                            label = "choose a number",
                            value = 25, min = 1, max = 100),
                plotOutput("hist"))

server <- function(input, output){
  
  output$hist <-  renderPlot({
    hist(rnorm(input$num))
  })
  
}


shinyApp(ui = ui, server = server)

#notes----
#inputs with Input_() functions, 

#over a dozen input functions (e.g. text input, date, date range)
#same basic layout for inputs: inputId (not ID), label


#outputs with Output_() functions

#many output functions e.g. tableOutout() or htmlOutput() or plotOutput()
#same layout for outputs: 



#three rules of server functions:

#1. must be stored in an output$x object
#2. what is saved as output should be with a render*({}) function e.g. renderPrint(), renderPlot(), renderTable()
#3. access input values with input$ to allow for reactivity


#notes - sharing apps
#need replace local hosting with web server. 
#can create multiple filed shiny apps with ui.R and server.R with NO app.R
#simple apps only need app.R, but need to use the correct names of app.R, server.R, or ui.R

