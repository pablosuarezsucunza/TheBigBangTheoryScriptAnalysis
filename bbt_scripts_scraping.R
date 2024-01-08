library(tidyverse)
library(rvest)
library(stringr)
library(tidytext)




# get links for episodes ------------------------------------------------------------------------------------------

main_link <- "https://bigbangtrans.wordpress.com/"
episodes_link <- data.frame(episode = read_html(main_link) %>% html_nodes("#pages-2 a") %>% html_text(),
                            link = read_html(main_link) %>% html_nodes("#pages-2 a") %>% html_attr("href")) %>% 
                      slice(2:nrow(.))



# scrape actual script --------------------------------------------------------------------------------------------

script <- data.frame()
for (i in episodes_links$link) {
  sc <- read_html(i) %>%  html_nodes("p") %>% html_text()
  
  script <- rbind(script, data.frame(link = rep(i, nrow(data.frame(sc))),
                                     script = sc))
  print(i)#see progress
}




# clean and set episode name --------------------------------------------------------------------------------------

  #changes link by episode number and title 
script <- script %>% 
  left_join(., episodes_links, by = "link") %>% 
  select(-link)
  
  #creat shortened variable for episode id
script <- script %>% 
  mutate(episode_id = gsub("[^[:digit:]., ]", "", episode),
         episode_id = str_trim(episode_id, "both"),
         episode_id = str_replace(episode_id, "[\\s]+", "_"),
         #in case there are more numbers in the title:
         episode_id = str_sub(episode_id, 1, 5)) 




# cleaning and preparing ------------------------------------------------------------------------------------------
 
script <- script %>% 
  #removes directions like "Leonard enters the room"
  filter(str_detect(script, ":")) %>% 
  #separates the character and the actual script
  mutate(character = str_split_fixed(script, ": ", n = 2)[,1],
         script = str_split_fixed(script, ": ", n = 2)[,2])%>% 
  filter(character != "Scene")


  #eliminate parenthesis that describe actions like "(screams across the room)" from character variable
script <- script %>% 
  mutate(character = str_remove(character, pattern = "\\s+\\([^()]*\\)"), #for " (.........)"
         character = str_trim(character, "both")) #for removing spaces at the beginning and at the end

  #clean script variable
script <- script %>% 
  mutate(script = tolower(script),
         #remove script comments inside parenthesis
         script = str_replace_all(script, pattern = "\\([^()]*\\)", " "),
         #remove punctuation
         script = str_replace_all(script, pattern = "[[:punct:]]", " "),
         #remove digits
         script = str_replace_all(script, pattern = "[[:digit:]]", " "),
         #remove multiple spaces
         script = str_replace_all(script,"[\\s]+", " "))


  #tokenize script (it is named "words")
script <- script %>% 
  mutate(#tokenize - a variable that is a list of all the words in that scriptline
         words = str_split(script, " "),
         #remove words that only have 1 character
         words = lapply(words, function(m) m[nchar(m) > 1])) 


  
# unnest to get all individual words in a classic df format -------------------------------------------------------

script_tidy <- script %>%
  select(-script) %>% 
  unnest(cols = c(words))


# words that are lexicons for later filtering -----------------------------------------------------------
lexicons <- c(tidytext::stop_words$word, "don", "ve", "ll", "didn", "uh", "yeah", "um", "gonna", 
              "isn", "doesn", "aa", "aaah", "aaaah", "aaaaah", "oontz")

script_tidy <- script_tidy %>%  
  filter(!(words %in% lexicons))






# output as csv ---------------------------------------------------------------------------------------------------
#write.csv(script, "script.csv", row.names = F)



# get cast pics ---------------------------------------------------------------------------------------------------

cast_pics = c("cast pics\Amy.png", "cast pics\Bernadette.png", "cast pics\Howard.png", "cast pics\Leonard.png",
              "cast pics\Penny.png", "cast pics\Raj.png", "cast pics\Sheldon.png")



















