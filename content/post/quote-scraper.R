library(tidyverse)
library(rvest)
#https://www.datacamp.com/community/tutorials/r-web-scraping-rvest
url <- "https://www.goodreads.com/quotes/search?page=1&q=simone+de+beauvoir&utf8=%E2%9C%93"
url1 <- "https://www.goodreads.com/quotes/search?page=1&q=rainer+maria+rilke&utf8=%E2%9C%93"


#

# 

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
list_of_pages <- str_c(url, '?page=', 1:last_page)


#= div:nth-child(3)

quote_text <- function(html){
  
  path <- read_html(html)
  
  path %>% 
    html_nodes(xpath=paste(selectr::css_to_xpath(".quoteText"), "/text()")) %>%
    html_text(trim = TRUE) %>% 
    str_trim(side = "both") %>% 
    unlist()
}

t <- quote_text(url)

t
xml_remove(t)

#making it iterative

#need to add content to these -> need names, country, sex associated to urls

#note that can replace everything after 'q=' and before '&utf' to add new names -> next iteration

url <- "https://www.goodreads.com/quotes/search?page=1&q=simone+de+beauvoir&utf8=%E2%9C%93"
url1 <- "https://www.goodreads.com/quotes/search?page=1&q=rainer+maria+rilke&utf8=%E2%9C%93"

authors <- list(url, url1)

authors %>% map(., last_page_count)
