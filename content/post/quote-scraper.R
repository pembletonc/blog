library(tidyverse)
library(rvest)

url <- "https://www.goodreads.com/quotes/search?page=1&q=simone+de+beauvoir&utf8=%E2%9C%93"
url1 <- "https://www.goodreads.com/quotes/search?page=1&q=rainer+maria+rilke&utf8=%E2%9C%93"

last_page_count_function <- function(html){
  
  path <- read_html(html)
  
  pages_data <- path %>%
    html_nodes(xpath = "/html/body/div[2]/div[3]/div[1]/div[1]/div[2]/div[22]") %>% 
    html_nodes("a") %>% 
    html_text() %>% 
    str_extract("\\(?[0-9,.]+\\)?") %>%
    map_int(., parse_integer) %>% enframe(name = NULL) %>% 
    drop_na() %>% 
    max(.) %>% 
    rowid_to_column("ID")
}

last_page_count_function <- last_page_count(url)

#collect all pages from author

pages <- str_replace_all(url, "page=1", paste0("page=", as.character(2:last_page)))

quote_text_function <- function(html){
  
  path <- read_html(html)
  
  path %>% 
    html_nodes(xpath=paste(selectr::css_to_xpath(".quoteText"), "/text()")) %>%
    html_text(trim = TRUE) %>% 
    str_trim(side = "both") %>% 
    unlist() %>% 
    enframe(name = NULL) %>% 
    naniar::replace_with_na(replace = list(value = c("", "―"))) %>% 
    janitor::remove_empty("rows") %>% 
    rowid_to_column("ID")
}

t <- quote_text_function(url)


quote_rating_function <- function(html){
  
  path <- read_html(html) 
  
  path %>% 
    html_nodes("a.smallText") %>% 
    html_text(trim = TRUE) %>%
    enframe(name = NULL) %>% 
    mutate(value = str_remove_all(value, "likes")) %>% 
    mutate(value = as.numeric(value),
           Author_Rank = min_rank(-value)) %>% 
    arrange(desc(value)) %>% 
    rename(Rating = value) %>% 
    rowid_to_column("ID")
    
  }

(rating <- quote_rating_function(url))


#function to extract author name & ensure the quote is the authors (not a commentor)

Author_name_function <- function(html){
  
path <- read_html(html)
  
 titles <-  path %>% 
    html_nodes("a.authorOrTitle") %>%
    html_text() %>% 
    str_trim() %>% 
    enframe(name = NULL)

 path %>% 
   html_nodes(".authorOrTitle") %>%
   html_text() %>% 
   str_trim() %>% 
   enframe(name = NULL) %>% 
   anti_join(titles) %>% 
   rename(Author = value) %>% 
   mutate(Author = str_remove_all(Author, ",")) %>% 
   rowid_to_column("ID")
   
}

(author <- Author_name_function(url))

#combining all 

author %>% 
  left_join(rating, by = "ID")


#making it iterative----

#need to add content to these -> need names, country, sex associated to urls

#note that can replace everything after 'q=' and before '&utf' to add new names -> next iteration

url <- "https://www.goodreads.com/quotes/search?page=1&q=simone+de+beauvoir&utf8=%E2%9C%93"
url1 <- "https://www.goodreads.com/quotes/search?page=1&q=rainer+maria+rilke&utf8=%E2%9C%93"

authors <- list(url, url1)

authors %>% map(., last_page_count)
