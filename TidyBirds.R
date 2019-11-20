.libPaths("D:/R/Library") 
library(tidyverse)
library(wordcloud2)

birds <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-11-19/nz_bird.csv")

birds$vote_rank <- as.numeric(gsub("vote_","",birds$vote_rank))
weight <- birds %>% 
  mutate(weight=(6-vote_rank)) %>% 
  na.omit() %>% 
  group_by(bird_breed) %>% 
  summarise(total=n(), weight=sum(weight)) %>% 
  arrange(desc(weight)) %>% 
  select(1,3)
  
wordcloud2(weight, color="goldenrod")

#That's it. If it doesn't load some values, open it in your browser.

#Also, here's a bar chart I made which is quite nice.

first <- birds %>% 
  filter(vote_rank=="vote_1") %>% 
  group_by(bird_breed) %>% 
  summarise(number_of_first=n())

best <- left_join(weight,first) %>% 
  arrange(desc(weight)) %>% 
  head(30) %>% 
  gather(value="numbers", key="type", `number_of_first`,`weight`)

ggplot(best,aes(x=reorder(bird_breed,numbers), y=ifelse(type=="weight", yes = -numbers, no = numbers), fill = type))+
  geom_col() +
  coord_flip() +
  scale_y_continuous(labels = abs) +
  labs(y = "Weighted votes ---  First Places")
  