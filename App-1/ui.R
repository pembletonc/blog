library(shiny)
library(shinydashboard)

dashboard_box_size <- if(is.null(TOPIC$full_community)) "3" else "4 col-lg-2"

dashboardPage(
  # Dashboard Page Setup-------------------------------------------------------
  title = META$name,
  skin = META$skin_color,
  #theme = c(META$theme_css, "custom.css"),
  #sidebar_mini = TRUE,
  dashboardHeader(
    title = HTML(glue::glue(
      '<span class="logo-mini">{META$logo_mini}</span>
      <span class="logo-lg">{META$logo_lg}</span>'
    ))
  ), 
  
  # Dashboard Sidebar-------------------------------------------------------
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "tab_dashboard", icon = icon("dashboard")),
      menuItem("About", tabName = "tab_about", icon = icon("info"))
    )
  ),
  
  #Dashboard Body--------------------------------------------------------------
  dashboardBody(
    tabItems(
      
      # Front Page - tab_dashboard----------------------------------------------
      tabItem(
        "tab_dashboard", 
        tags$head(
          #Metadata <head>------------------------------------------------------
          '<meta property="og:title" content="{META$name}">
          <meta property="og:description" content="{META$description}">
          <meta property="og:url" content="{META$app_url}">
          <meta property="og:image" content="{META$app_icon}">
          <meta name="twitter:card" content="summary">
          <meta name="twitter:creator" content="@coreypembleton">
          <meta name="twitter:site" content="https://data-break.netlify.com">
          '
        )),
      #insert GA if added later to meta
      #if (!is.null(GA_KEY)) HTML(
      #  glue::glue(
      #    '
      #        <!-- Global site tag (gtag.js) - Google Analytics -->
      #        <script async src="https://www.googletagmanager.com/gtag/js?id={GA_KEY}"></script>
      #        <script>
      #          window.dataLayer = window.dataLayer || [];
      #          function gtag(){{dataLayer.push(arguments);}}
      #            gtag(\'js\', new Date());
      #            gtag(\'config\', \'{GA_KEY}\');
      #         </script>
      #        ')
      #)
      #Metadata <head> end------------------------------------------------------
      
    ),
    fluidRow(
      #Frontpage - boxes start
      valueBox(
        inputID = "total_today",
        "—", "Tweets Today",
        color = "purple",
        icon = icon("comment-dots"),
        width = "4 col-lg-2")
      #Frontpage - boxes end
      
      ),
    fluidRow(
      #Frontpage - tweet volume plots start
      tabBox(
        width = 12, 
        tabPanel(
          status = "primary",
          title = "Tweet Volume",
          withSpinner(plotlyOutput("plotly_hourly_tweet_volume", height = "250px"))
        ),
        tabPanel(
          status = "success", 
          title = "Tweets by Hour of Day",
          withSpinner(plotlyOutput("plot_tweets_by_hour", height = "250px"))
        )
      )
      #Frontpage - tweet volume plots end
    ),
    #About- tab_about
    tabItem(
      fluidRow(
        box(
          title = "Credits",
          status = "danger",
          width = "6 col-lg-4",
          tags$p(
            class = "text-center",
            tags$img(class = "img-responsive img-rounded center-block", src = "profile.jpg", style = "max-width: 150px;")
          ),
          tags$p(
            class = "text-center",
            HTML(twemoji("1F44B")),
            tags$strong("Credits go to Garrick"),
            HTML(paste0("(", tags$a(href = "https://twitter.com/grrrck", "@grrrck"), ")"))
          )
        )
      )
    )
      
    )
  )
  