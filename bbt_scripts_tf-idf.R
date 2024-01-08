library(tidyverse)


# Calculating tf_idf ----------------------------------------------------------------------------------------------

# with idf the theoretical proper way
tf_idf <- script_tidy %>% 
  group_by(character) %>% 
  mutate(character_no_words = n()) %>% 
  filter(character_no_words > 100) %>% 
  group_by(words, character) %>% 
  summarise(no_times_bych = n(),
            character_no_words = mean(character_no_words)) %>% 
  filter(no_times_bych > 5) %>% 
  mutate(tf = no_times_bych / character_no_words) %>% 
  #idf
  group_by(words) %>% 
    #number of characters that said each word
  mutate(no_times_total = n()) %>% 
  ungroup() %>% 
  mutate(idf = length({unique({.$character})}) / no_times_total) %>% 
  #tf_idf
  mutate(tf_idf = tf*idf, 2) %>% 
  filter(character %in% c("Amy", "Bernadette", "Penny", "Leonard", "Howard", "Raj", "Sheldon")) %>% 
  arrange(desc(tf_idf)) 




# Plots -----------------------------------------------------------------------------------------------------------

tf_idf %>% 
  mutate(character = factor(character, 
                            levels = c("Sheldon", "Leonard", "Penny", "Raj", "Howard", "Bernadette", "Amy"))) %>% 
  group_by(character) %>% 
  arrange(desc(tf_idf)) %>% 
  slice(1:10) %>% 
  ggplot(aes(x = reorder(words, tf_idf), y = tf_idf))+
  geom_bar(stat = "identity", width = 0.7)+
  labs(x = NULL, y = "tf-idf value",
       title = "Most common words for each character with respect to the others \nin TBBT (using tf-idf metric)",
       subtitle = "The higher the tf-idf value, the more common that word is to that \ncharacter and less to the other characters")+
  facet_wrap(~character, ncol = 3, scales = "free")+
  coord_flip()+
  theme_classic()+
  theme(axis.text.x = element_text(size = 7),
        axis.text.y = element_text(siz = 9),
        plot.title = element_text(size = 12, hjust = 0.5),
        plot.subtitle = element_text(size = 10.5, hjust = 0.5))
ggsave("tf_idf.png", dpi = 1000, units = "in", width = 6, height = 6)


#plot without names
tf_idf %>% 
  filter(!(words %in% c("amy", "bernadette", "penny", "leonard", "howard", "raj", "sheldon", "rajesh", "bernie", "howie"))) %>% 
  mutate(character = factor(character, 
                            levels = c("Sheldon", "Leonard", "Penny", "Raj", "Howard", "Bernadette", "Amy"))) %>% 
  group_by(character) %>% 
  arrange(desc(tf_idf)) %>% 
  slice(1:10) %>% 
  ggplot(aes(x = reorder(words, tf_idf), y = tf_idf))+
  geom_bar(stat = "identity", width = 0.7)+
  labs(x = NULL, y = "tf-idf value",
       title = "Most common words (excluding main character names) for each \ncharacter with respect to the others in TBBT (using tf-idf metric)",
       subtitle = "The higher the tf-idf value, the more common that word is to that \ncharacter and less to the other characters")+
  facet_wrap(~character, ncol = 3, scales = "free")+
  coord_flip()+
  theme_classic()+
  theme(axis.text.x = element_text(size = 7),
        axis.text.y = element_text(siz = 9),
        plot.title = element_text(size = 12, hjust = 0.5),
        plot.subtitle = element_text(size = 10.5, hjust = 0.5))
ggsave("tf_idf (no names).png", dpi = 1000, units = "in", width = 6, height = 6)





#plot with some specific words
tf_idf %>% 
  filter(!(words %in% c("amy", "bernadette", "penny", "leonard", "howard", "raj", "sheldon", "rajesh", "bernie", "howie"))) %>% 
  #filter the words we want
  filter(words %inz% c("seat", "spot", "physics"))












