library(tidyverse)
library(rvest)

url <- "https://www.goodreads.com/quotes/search?page=1&q=simone+de+beauvoir&utf8=%E2%9C%93"
url1 <- "https://www.goodreads.com/quotes/search?page=1&q=rainer+maria+rilke&utf8=%E2%9C%93"

last_page_count <- function(html){
  
  path <- read_html(html)
  
  pages_data <- path %>%
    html_nodes(xpath = "/html/body/div[2]/div[3]/div[1]/div[1]/div[2]/div[22]") %>% 
    html_nodes("a") %>% 
    html_text() %>% 
    str_extract("\\(?[0-9,.]+\\)?") %>%
    map_int(., parse_integer) %>% enframe(name = NULL) %>% 
    drop_na() %>% 
    max(.)
                                  
}

last_page <- last_page_count(url)

#collect all pages from author

pages <- str_replace_all(url, "page=1", paste0("page=", as.character(2:last_page)))

quote_text <- function(html){
  
  path <- read_html(html)
  
  path %>% 
    html_nodes(xpath=paste(selectr::css_to_xpath(".quoteText"), "/text()")) %>%
    html_text(trim = TRUE) %>% 
    str_trim(side = "both") %>% 
    unlist() %>% 
    enframe(name = NULL) %>% 
    naniar::replace_with_na(replace = list(value = c("", "―"))) %>% 
    janitor::remove_empty("rows")
}

t <- quote_text(url)

t

janitor::remove_empty(t, "rows")

?na_if

View(t)

#making it iterative

#need to add content to these -> need names, country, sex associated to urls

#note that can replace everything after 'q=' and before '&utf' to add new names -> next iteration

url <- "https://www.goodreads.com/quotes/search?page=1&q=simone+de+beauvoir&utf8=%E2%9C%93"
url1 <- "https://www.goodreads.com/quotes/search?page=1&q=rainer+maria+rilke&utf8=%E2%9C%93"

authors <- list(url, url1)

authors %>% map(., last_page_count)
