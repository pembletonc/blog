library(tidyverse)
library(rvest)

url <- "https://www.goodreads.com/quotes/search?page=1&q=simone+de+beauvoir&utf8=%E2%9C%93"
url1 <- "https://www.goodreads.com/quotes/search?page=1&q=rainer+maria+rilke&utf8=%E2%9C%93"

#function to extract the quote text itself

quote_text_function <- function(html){
  
  path <- read_html(html)
  
  p <- path %>% 
    html_nodes(xpath=paste(selectr::css_to_xpath(".quoteText"), "/text()")) %>%
    html_text(trim = TRUE) %>% 
    enframe(name=NULL)
  
  p <- paste(unlist(p$value), collapse = " ") %>% enframe(name=NULL)
  
  p %>% 
    separate_rows(value, sep = "―", convert = TRUE) %>% 
    naniar::replace_with_na(replace = list(value = c(" "))) %>% 
    janitor::remove_empty("rows") %>% 
    rename(Quote = value) %>% 
    rowid_to_column("ID") %>% 
    mutate(Quote = str_trim(Quote, side = "both"))

}

#function to extract the ratings on goodreads
quote_rating_function <- function(html){
  
  path <- read_html(html) 
  
  path %>% 
    html_nodes("a.smallText") %>% 
    html_text(trim = TRUE) %>%
    enframe(name = NULL) %>% 
    mutate(value = str_remove_all(value, "likes"),
           Rating = as.numeric(value)) %>%
    rowid_to_column("ID") %>% 
    select(-value)
  }

#function to extract author name & ensure the quote is the authors (not a commentor)
author_name_function <- function(html){
  
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

#combining all 

get_data_table <- function(html){
  
  quote <-  quote_text_function(html)
  rating <-  quote_rating_function(html)
  author <-  author_name_function(html)
  
  author %>% 
    left_join(rating, by = "ID") %>% 
    left_join(quote, by = "ID")
  
}

last_page_count_function <- function(html){
  
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

page_collect_function <-  function(author){
  
  authors_formatted <- tolower(str_replace_all(author, pattern = " ", replacement = "+"))
  authors_url <- paste("https://www.goodreads.com/quotes/search?page=1&q=", authors_formatted, "&utf8=%E2%9C%93", sep="")
  last_page_number <- last_page_count_function(authors_url)
  list_of_pages <- str_replace_all(authors_url, "page=1", paste0("page=", as.character(1:last_page_number)))
  
}


scrape_multiple <- function(author) {

  list_of_pages <- page_collect_function(author)

  nested_df <- list_of_pages %>% 
    map(get_data_table) %>% 
    bind_rows() %>% 
    filter(Author %in% author) %>%
    select(-ID) %>% 
    group_by(Author) %>% 
    nest()
  
  nested_df %>% 
    mutate(data = map(data, ~mutate(.x, Author_Rank = 1:nrow(.x)))) %>% 
    unnest()
  }
authors <- list("Simone de Beauvoir")

authors <- list("Simone de Beauvoir", "Rainer Maria Rilke", "Socrates")

three_authors <- map_dfr(.f = scrape_multiple, .x = authors) %>% 
  rowid_to_column("ID") %>%
  arrange(desc(Rating)) %>% 
  mutate(All_Authors_Ranking = 1:nrow(.)) %>% 
  arrange(Author)

View(three_authors)

saveRDS(three_authors, file = "./content/post/three_authors.rds")

file <- read_rds("three_authors.rds")








