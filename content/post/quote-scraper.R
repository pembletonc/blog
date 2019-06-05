library(rvest)
#https://www.datacamp.com/community/tutorials/r-web-scraping-rvest
url <- "https://www.goodreads.com/quotes/search?utf8=%E2%9C%93&q=simone+de+beauvoir"

get_last_page <- function(html){
  
  pages_data <- html %>% 
    # The '.' indicates the class
    html_node('.a') %>% 
    # Extract the raw text as a list
    html_text()                   
  
  # The second to last of the buttons is the one
  pages_data[(length(pages_data)-1)] %>%            
    # Take the raw string
    unname() %>%                                     
    # Convert to number
    as.numeric()                                     
}

first_page <- read_html(url)
(last_page <- get_last_page(first_page))

html_attr(name = "a")
