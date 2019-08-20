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



