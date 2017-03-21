mapsplus %>% 
  summarize(count = n())

# Worse
wj %>% 
  left_join(overallplus[,c(1,5)], by = 'hero') %>% 
  mutate(Games.Played = as.numeric(as.character(Games.Played)),
         Win.Percent = as.numeric(as.character(gsub('%','',Win.Percent)))*.01,
         avg.winrate = as.numeric(as.character(gsub('%','',avg.winrate)))*.01) %>% 
  group_by(hero, Games.Played, avg.winrate, Win.Percent) %>%
  filter(Games.Played > 1000) %>% 
  summarize(difference = Win.Percent - avg.winrate) %>% 
  filter(difference < -0.03) %>% 
  arrange(difference)

# Better
wj %>% 
  left_join(overallplus[,c(1,5)], by = 'hero') %>% 
  mutate(Games.Played = as.numeric(as.character(Games.Played)),
         Win.Percent = as.numeric(as.character(gsub('%','',Win.Percent)))*.01,
         avg.winrate = as.numeric(as.character(gsub('%','',avg.winrate)))*.01) %>% 
  group_by(hero, Games.Played, avg.winrate, Win.Percent) %>%
  filter(Games.Played > 1000) %>% 
  summarize(difference = Win.Percent - avg.winrate) %>% 
  filter(difference > 0.03) %>% 
  arrange(difference)
