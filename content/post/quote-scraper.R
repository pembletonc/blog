library(tidyverse)

#https://www.datacamp.com/community/tutorials/r-web-scraping-rvest
url <- "https://www.goodreads.com/quotes/search?utf8=%E2%9C%93&q=simone+de+beauvoir"
url1 <- "https://www.goodreads.com/quotes/search?utf8=%E2%9C%93&q=rainer+maria+rilke&commit=Search"

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


(last_page <- last_page_count(url))

#collect all pages from author
list_of_pages <- str_c(url, '?page=', 1:last_page)

head(list_of_pages)
