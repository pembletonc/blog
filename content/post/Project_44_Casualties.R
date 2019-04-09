library(tidyverse)

cas1 <- read_excel("./content/post/Project44_Casualty-List_v3.xlsx")

cas1 <- cas1 %>% mutate(date_of_death = dmy(date_of_death),
                        Division = as_factor(Division),
                        unitshipsquadron = as_factor(unitshipsquadron))

cas1 <- cas1 %>% select(age_text, date_of_death, rank, Division, regiment, cemeterymemorial)

#geom_tile

cas1 %>% group_by(age_text) %>% count 

cas1 %>% 
  filter(!is.na(age_text)) %>% 
  ggplot(aes(x = age_text)) + 
  geom_bar() +
  theme_minimal()

cas1 %>% 
  ggplot(aes(x))





