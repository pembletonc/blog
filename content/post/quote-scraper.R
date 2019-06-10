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
    mutate(value = str_remove_all(value, "likes")) %>% 
    rowid_to_column("ID")
    
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


scrape_multiple <- function(html) {
  
  last_page_number <- last_page_count_function(html)
  
  list_of_pages <-  str_replace_all(html, "page=1", paste0("page=", as.character(1:last_page_number)))
  
  
  
  list_of_pages %>% 
    map(get_data_table) %>% 
    bind_rows() %>% 
    filter(Author %in% c("Simone de Beauvoir")) %>%
    select(-ID) %>% 
    group_by(Author) %>% 
    nest()
    
  }

test <- scrape_multiple(url)

test %>% mutate(data = map(data, ~mutate(.x, Author_Rank = 1:nrow(.x)
                                              ))) %>% unnest() %>% rowid_to_column("ID") %>% View()

  
#note -> rating is inaccurate: doesn't account for added pages, and re-ranks for each page
#note -> doesn't appear to have any of the higher liked quotes


View(test)


combine(url1)

url_list <- list(url = url, url1 = url1)


#drop IDs to add a DF-wide ID
map_dfr(url_list, combine) %>% select(-ID) %>% rowid_to_column("ID") %>% View()

map_dfr(url_list, combine) %>% View()



#making it iterative----




last_page_simone <- last_page_count_function(url)

#collect all pages from author

pages_simone <- str_replace_all(url, "page=1", paste0("page=", as.character(2:last_page_simone)))


pages_simone






#need to add content to these -> need names, country, sex associated to urls

#note that can replace everything after 'q=' and before '&utf' to add new names -> next iteration

url <- "https://www.goodreads.com/quotes/search?page=1&q=simone+de+beauvoir&utf8=%E2%9C%93"
url1 <- "https://www.goodreads.com/quotes/search?page=1&q=rainer+maria+rilke&utf8=%E2%9C%93"

authors <- list(url, url1)

authors %>% map(., last_page_count)
