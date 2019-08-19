
library(shiny)
library(shinydashboard)
library(plotly)
#library(openxlsx)

ui <- shinyUI(dashboardPage(
  dashboardHeader(title = "Canadian 2019 Election MP Tweet Tracker", 
                  titleWidth = 450),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("chart-pie")),
      menuItem("Tweets by Party", icon = icon("twitter"),
               menuSubItem("Positive Tweets", tabName = "pTweets", icon = icon("smile")),
               menuSubItem("Neutral Tweets", tabName = "neuTweets", icon = icon("meh")),
               menuSubItem("Negative Tweets", tabName = "negTweets", icon = icon("frown"))
      ),
      menuItemOutput("out1") # added
    )
  ),
  ## Body content
  dashboardBody(
    
    tabItems(
      # First tab content
      tabItem(tabName = "dashboard",
              div(class = "my-class", h2("What is your MP Saying leading up to the election?")),
              fluidRow(
                #valueBox(count, "Total Number of Tweets Analyzed in the competition", icon = icon("twitter"), width = 6),
                valueBox(15, "Total Number of Tweets Collected to Date", icon = icon("twitter"), width = 6),
                #valueBox(countDays, "Number of Days ", icon = icon("calendar-check-o"), width = 6, color = "yellow")
                valueBox(10, "Number of Days Until the Election ", icon = icon("calendar-check-o"), width = 6, color = "yellow")
              ),
              fluidRow(
                #infoBox("Positive", paste(positivePercent, "%"), icon = icon("thumbs-up"), width = 4, fill = TRUE, color = "green"),
                infoBox("Positive", "80%", icon = icon("smile"), width = 4, fill = TRUE, color = "green"),
                infoBox("Neutral", "15%", icon = icon("meh"), width = 4, fill = TRUE, color = "light-blue"),
                infoBox("Negative", "5%", icon = icon("frown"), width = 4, fill = TRUE, color = "red")
              )
      ),
      
      # Positive Tweets tab content
      tabItem(tabName = "pTweets",
              h2("Positive Tweets"),
              fluidRow(
                box(title = "Summary of Tweet Sentiment, by Major Party",
                    status = "primary", solidHeader = TRUE,
                    background = "olive",
                    collapsible = TRUE,
                    plotlyOutput("plot1"), width  ="50%")
        )),
      # Neutral Tweets tab content
      tabItem(tabName = "neuTweets",
              h2("Neutral Tweets")
              #h4(neutralTweets)
      ),
      # Negative Tweets tab content
      tabItem(tabName = "negTweets",
              h2("Negative Tweets")
              #h4(negativeTweets)
      )
    )
  )
))

server <- function(input, output) {
  
    output$plot1 <- renderPlotly({
      ggplot(iris, aes(Sepal.Width, Petal.Width)) + 
        geom_line() + 
        geom_point()
    })    

  #my_files will be updated each time you run the app
  #my_files <- list.files() 
  
  # for testing purposes generate 5 tabs with names given by random letters
  my_files <- letters[sample(1:26, 5)] 
  
  # There could also be the case when there is no files in a folder
  # You can handle it with `req` or `validate(need(...))` functions
  #my_files <- ""
  
  output$out1 <- renderUI({
    # Just in case if you would put new files to the folder
    # while the app is working and wanted an update of tabs:
    #   - create eventReactive with an actionButton which will
    #     return list.files().
    #   - pass new names of files to this renderUi function.
    
    # be careful because "tabName" must not have a "." in it.
    
    req(my_files) # show tabs only if there are files in a directory
    
    # generate and save tabs in a list
    tabs <- lapply(seq_along(my_files), function(i) {
      menuSubItem(my_files[i], tabName = my_files[i], icon = icon("thumbs-up"))
    })
    
    menuItem("Files", tabName = "Files", icon = NULL, tabs)
  })
}
shinyApp(ui, server)

