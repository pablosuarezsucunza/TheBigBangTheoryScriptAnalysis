# Packages -------------------------------------------------------------------------------------------------------
library(tidyverse)
library(ggimage)
library(directlabels)
library(wordcloud)
library(tidytext)
  #for network graphs
library(tidygraph)
library(ggraph)
  #puts graphs together
library(ggpubr)


# who speaks most (by line spoken)? -----------------------------------------------------------------------------

script %>% 
  mutate(season = str_sub(episode_id, 1, 2)) %>% 
  group_by(character, season) %>% 
  summarise(n = n()) %>% 
  mutate(season = as.numeric(season)) %>% 
  filter(character %in% c("Amy", "Bernadette", "Penny", "Leonard", "Howard", "Raj", "Sheldon")) %>% 
  #add pic links
  mutate(pics = paste0("cast pics/", character, ".png")) %>% 
  #plot
  ggplot(aes(x = season, y = n))+
  geom_line(aes(color = character), size = 0.4)+
  geom_image(aes(image = ifelse(season == 1 | season == 3 & character %in% c("Amy", "Bernadette"), pics, NA)), 
             size = 0.05)+
  scale_x_continuous(limits = c(0.85,10), breaks = 1:10)+
  scale_y_continuous(breaks = seq(0, 1250, 250))+
  labs(x = "Season", y = "Lines per season",
       title = "Lines per character per season",
       subtitle = "Seasons 1 through 10")+
  theme_classic()+
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

ggsave("Lines by character by season.png", dpi = 1000, width = 6, height = 5, units = "in")


# Lines per episode vs. words per line ---------------------------------------------------------------------------------
  
script %>% 
  filter(character %in% c("Amy", "Bernadette", "Penny", "Leonard", "Howard", "Raj", "Sheldon", "Stuart")) %>% 
  #get number of lines per episode 
  group_by(character, episode_id) %>% 
  summarise(lines = n()) %>% 
  group_by(character) %>% 
  summarise(lines = mean(lines)) %>% 
  #add data of words per line
  left_join(., script %>% 
                  mutate(no_words = str_count(script, "\\w+")) %>%
                  filter(character %in% c("Amy", "Bernadette", "Penny", "Leonard", "Howard", "Raj", "Sheldon")) %>% 
                  group_by(character) %>% 
                  summarise(words_perline = mean(no_words))) %>% 
  #add pic links
  mutate(pics = paste0("cast pics/", character, ".png")) %>% 
  #plot
  ggplot(aes(x = lines, y = words_perline))+
  geom_image(aes(image = pics), size = 0.07)+
  ylim(9.5,16)+
  labs(x = "Lines per episode", y = "Words per line",
       title = "Lines per episode & words per line",
       subtitle = "Seasons 1 through 10")+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

ggsave("Lines per episode and words per line.png", dpi = 1000, width = 5, height = 5, units = "in")



# Other characters names -------------------------------------------------------------------------------------------------
  
  #filter the main 7 characters  
main_7_names <- script_tidy %>% 
  filter(character %in% c("Amy", "Bernadette", "Penny", "Leonard", "Howard", "Raj", "Sheldon"),
         words %in% c("amy", "bernadette", "penny", "leonard", "howard", "raj", "sheldon", "rajesh", "bernie", "howie")) %>%
  mutate(words = case_when(words == "rajesh" ~ "raj",
                           words == "bernie" ~ "bernadette",
                           words == "howie" ~ "howard",
                           TRUE ~ words),
         words = str_to_sentence(words)) 


  #heatmap of times
main_7_names %>% 
  group_by(character, words) %>% 
  summarise(n = n()) %>% 
  ggplot(aes(x = character, y = words, fill = n))+
  geom_tile()+
  scale_fill_gradient(low = "white", high = "red", space = "Lab", 
                      name="")+
  geom_text(aes(label = n), size = 3, color = "black")+
  labs(x = "This character says", y = "Name said",
       title = "Times each character said other character's name",
       subtitle = "Seasons 1 through 10")+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

ggsave("Characters said other characters.png", dpi = 1000, width = 6, height = 5, units = "in")


  #heatmap of percentage of total names said
main_7_names %>% 
  group_by(character, words) %>% 
  summarise(n = n()) %>% 
  with_groups(character, ~mutate(.x, total = sum(n))) %>% 
  mutate(pctg = (n/total)*100) %>% 
  ggplot(aes(x = character, y = words, fill = pctg))+
  geom_tile()+
  scale_fill_gradient(low = "white", high = "red", space = "Lab", 
                      name="")+
  geom_text(aes(label = paste0(round(pctg, 2), "%")), size = 3, color = "black")+
  labs(x = "This character says", y = "Name said",
       title = "% of total  character's names said by each character",
       subtitle = "Seasons 1 through 10")+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

ggsave("Characters said other characters (pctg).png", dpi = 1000, width = 6, height = 5, units = "in")


#heatmap of percentage of total names said AFTER SEASON 4
plot_cnames_4to10 <- main_7_names %>% 
  mutate(season = as.numeric(str_sub(episode_id, 1,2))) %>% 
  filter(season > 3) %>% 
  group_by(character, words) %>% 
  summarise(n = n()) %>% 
  with_groups(character, ~mutate(.x, total = sum(n))) %>% 
  mutate(pctg = (n/total)*100) %>% 
  ggplot(aes(x = character, y = words, fill = pctg))+
  geom_tile()+
  scale_fill_gradient(low = "white", high = "red", space = "Lab", 
                      name="")+
  geom_text(aes(label = paste0(round(pctg, 2), "%")), size = 3, color = "black")+
  labs(x = "This character says", y = "Name said",
       title = "% of total  character's names said by each character",
       subtitle = "Seasons 4 through 10 \n Amy and Bernadette enter the show on a regular basis")+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

ggsave("Characters said other characters (season4-10, pctg).png", dpi = 1000, width = 6, height = 5, units = "in")



  #network of times
nodes <- data.frame(label = unique(main_7_names$character),
                    id = unique(main_7_names$character)) %>% arrange(label) 

edges <- main_7_names %>% 
  group_by(character, words) %>% 
  summarise(n = n()) %>% 
  rename(from = character,
         to = words,
         weight = n)

net.tidy <- tbl_graph(nodes = nodes,
                      edges = edges,
                      directed = FALSE)

ggraph(net.tidy, layout = "linear", circular = TRUE) + 
  geom_edge_arc(aes(width = weight), alpha = 0.8) + 
  scale_edge_width(range = c(0.2, 2)) +
  geom_node_text(aes(label = label), repel = TRUE) +
  labs(title = "Times characters said each other's names",
       subtitle = "Seasons 1 through 10") +
  theme_graph()+
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

ggsave("Network of character names.png", dpi = 1000, width = 6, height = 5, units = "in")













# word clouds ------------------------------------------------------------------------------------------------------

wordcloud_custom <- function(character, df){
  print(character)
  wordcloud(words = df$words, freq = df$frequency,
            max.words = 400, random.order = FALSE, rot.per = 0.35,
            colors = brewer.pal(8, "Dark2"))
}

df_grouped <- script_tidy %>% 
  filter(!(words %in% lexicons))%>% 
  filter(character %in% c("Amy", "Bernadette", "Penny", "Leonard", "Howard", "Raj", "Sheldon")) %>% 
  group_by(character, words) %>% 
  count(words) %>%
  group_by(character) %>%
  mutate(frequency = n / n()) %>%
  arrange(character, desc(frequency)) %>% 
  nest() 

walk2(.x = df_grouped$character, .y = df_grouped$data, .f = wordcloud_custom)







# Putting some plots together -------------------------------------------------------------------------------------
ggarrange(plot_cnames_all, plot_cnames_4to10, labels = c("A", "B"))

ggsave("Conbined - pctg of times said a name.png", dpi = 1000, width = 12, height = 5, units = "in")




