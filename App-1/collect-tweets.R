library(rtweet)
library(tidyverse)

handles_orig <- read_csv("./App-1/candidate_twitter_handles.csv", 
                         locale = locale(encoding = "latin1"),
                         na = "")

handles_only <- handles_orig %>% 
  filter(!is.na(twitter_handle)) %>% 
  mutate(twitter_handle = tolower(twitter_handle))

candidate_timelines <- 
  get_timelines(user = as.character(handles_only$twitter_handle),
                parse = FALSE,
                n = 100000,
                retryonratelimit = TRUE)

candidate_timelines %>% 
  select() %>% 
  mutate(screen_name = tolower(screen_name)) 

#join to get the political party
candidate_tweets <- 
  handles_only %>% 
  left_join(candidate_timelines, by = c("screen_name" = "twiter_handle"))



# to stream tweets until the end of the election:----
## set stream time
timeout <- as.numeric(
  difftime(as.POSIXct("2018-02-04 00:00:00"),
           Sys.time(), tz = "US/Pacific", "secs")
)

## search terms
rstudioconf <- c("rstudioconf", "rstudio::conf",
                 "rstudioconference", "rstudioconference18",
                 "rstudioconference2018", "rstudio18",
                 "rstudioconf18", "rstudioconf2018",
                 "rstudio::conf18", "rstudio::conf2018")

## name of file to save output
json_file <- file.path("data", "stream.json")

## stream the tweets and write to "data/stream.json"
stream_tweets(
  q = paste(rstudioconf, collapse = ","),
  timeout = timeout,
  file_name = json_file,
  parse = FALSE
)

## parse json data and convert to tibble
rt <- parse_stream(json_file)




