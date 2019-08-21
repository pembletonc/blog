META <- list(
  #name of app
  name = "Canada 2019 Election Candidate Tweets", 
  description = "A dashboard tracking all tweets of 2019 election candidates in Canada", 
  app_url = "https://data-break.shinyapps.io/Canada_2019_Election_Tweets", 
  app_icon = "https://www.yorkregion.com/static/images/symbols-vote-2019.png",
  logo_lg = "Canada Votes 2019",
  logo_mini = "CV2019",
  topic_icon = "comments",
  topic_icon_full = "r-project", #change
  skin_color = "blue-light", 
  theme_css   = c("ocean-next/AdminLTE.css", "ocean-next/_all-skins.css")
  )

TOPIC <- list(
  #terms related to the topic which can be included
  terms <- c("climate","housing","healthcare","immigration", "climatechange", "housingcrisis"),
  
  #hashtags to exclude (if broader analysis occurs)
  #hashtag_exclude = "",
  
  #wordlist exclude
  #wordlist_exclude = "",
  
)

TWEET_MOST <- list(
  hours   = 12,
  days    = 0,
  minutes = 0,
  text    = "12 hours"
)


TWEETS_START_DATE <- "2019-08-01"  # Don't show tweets before this date
TZ_GLOBAL <- "America/Chicago"     # Time zone where conference is taking place
Sys.setenv(TZ = TZ_GLOBAL)


# A helper to get today() in the app's timezone
# * Note that tz_global() returns the system timezone,
#   or can be overwritten with tz_global("other/timezone")
today_tz <- function() today(tz_global())

TWEET_WALL_DATE_INPUTS <- c(
  "Today"     = "today",
  "Yesterday" = "yesterday",
  "Past week" = "past_week",
  "In 2019"   = "in_2019"
)



TWEET_WALL_DATE_RANGE <- function(inputId) {
  switch(
    inputId,
    "today"          = c(start = today_tz(),        end = today_tz()),
    "yesterday"      = c(start = today_tz() - 1,    end = today_tz() - 1),
    "past_week"      = c(start = today_tz() - 7,    end = today_tz()),
    "in_2019"        = c(start = ymd("2019-01-01"), end = today_tz()),
    "since_workshop" = c(start = .workshop_start,   end = today_tz()),
    "conf_prop"      = c(start = .conference_start, end = today_tz()),
    "conf_and_after" = c(start = .workshop_start,   end = today_tz()),
    NA
  )
}


CP_KEY <- if (file.exists("google_analytics_key.txt")) readLines("google_analytics_key.txt")


TWEETS_FILE <- paste0("data/", c("tweets_simplified.rds", "tweets.rds")) %>%
  keep(file.exists) %>% .[1]
message("Using tweets from: ", TWEETS_FILE)


ADMINLTE_COLORS <- list(
  "light-blue" = "#6699CC",
  "green"      = "#99C794",
  "red"        = "#EC5f67",
  "purple"     = "#C594C5",
  "aqua"       = "#a3c1e0",
  "yellow"     = "#FAC863",
  "navy"       = "#343D46",
  "olive"      = "#588b8b",
  "blue"       = "#4080bf",
  "orange"     = "#F99157",
  "teal"       = "#5FB3B3",
  "fuchsia"    = "#aa62aa",
  "lime"       = "#b0d4b0",
  "maroon"     = "#AB7967",
  "black"      = "#1B2B34",
  "gray-lte"   = "#D8DEE9",
  "primary"    = "#6699CC",
  "success"    = "#99C794",
  "danger"     = "#EC5f67",
  "info"       = "#a3c1e0",
  "warning"    = "#FAC863"
)
options("spinner.color" = ADMINLTE_COLORS$`gray-lte`)
options("spinner.color.background" = "#F9FAFB")


# ---- Blocklist ----
# A list with named list elements `status_id` and `screen_name`. Block specific
# tweets by adding the status id of the tweet to the `status_id` list, or block
# specific people by adding their screen name to the `screen_name` list.
BLOCKLIST <- list(
  status_id = list(
    "1087748087454535680"
  ),
  screen_name = list(
    "paukniccadi"
  )
)