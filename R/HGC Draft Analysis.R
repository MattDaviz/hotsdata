# Load libraries------
library(ggplot2); library(ggthemes); library(tidyr); library(dplyr); library(readr)

# Load data -------
data <- read_csv('.\\data\\hgc drafts.csv')

# Clean Data ------
data.long <- gather(data[,c(1,9:22)], variable, value, -region)

na <- test %>% 
  filter(region == 'North America')
eu <- test %>% 
  filter(region == 'Europe')

text.labels <- data.long %>% 
  group_by(region, variable) %>% 
  mutate(games = n()) %>% 
  group_by(region, value) %>% 
  mutate(involvement = n() / games) %>% 
  filter(n() > 2)

ann_text <- text.labels %>% 
  distinct(region, value, involvement) 

ann_text <- ann_text[,c(1:3)]
ann_text$x = 12.5
#ann_text$x = 10.5
ann_text$y = .5
#ann_text$y = .65

data.long <- data.long %>% 
  group_by(region, value) %>% 
  filter(n() > 2)

# Pick rates by draft position -----
data.long %>% 
  group_by(value) %>% 
  summarize(count = n()) %>% 
  arrange(desc(count)) %>% 
  top_n(10)

# Most popluar
a <- test %>% 
  filter(value %in% c('Malfurion', 'Tassadar', 'Ragnaros', 'Tychus', 'Rehgar', 'E.T.C.', 'Falstad', 'Valla', 'Zarya',)) %>% 
  group_by(variable, value) %>%
  select(region, variable, value) %>% 
  arrange(value)

ann_text <- ann_text %>% 
  filter(value %in% c('Malfurion', 'Tassadar', 'Ragnaros', 'Tychus', 'Rehgar', 'E.T.C.', 'Falstad', 'Valla', 'Zarya', 'Zeratul'))

a <- as.data.frame(a)

# Supports
a <- test %>% 
  filter(value %in% c('Malfurion', 'Kharazim', 'Lt. Morales', 'Auriel', 'Brightwing', 'Tyrande', 'Rehgar', 'Uther', 'Tassadar', 
                      'Li Li')) %>% 
  group_by(variable, value) %>% 
  arrange(value)

ann_text <- ann_text %>% 
  filter(value %in% c('Malfurion', 'Kharazim', 'Lt. Morales', 'Auriel', 'Brightwing', 'Tyrande', 'Rehgar', 'Uther', 'Tassadar', 
                      'Li Li'))

a <- as.data.frame(a)

# Specialists
a <- test %>% 
  filter(value %in% c('Medivh', 'Sylvanas', 'Xul', 'Sgt. Hammer', 'Nazeebo', 'The Lost Vikings', 'Abathur')) %>% 
  group_by(variable, value) %>% 
  arrange(value)

ann_text <- ann_text %>% 
  filter(value %in% c('Medivh', 'Sylvanas', 'Xul', 'Sgt. Hammer', 'Nazeebo', 'The Lost Vikings', 'Abathur')) 

a <- as.data.frame(a)

# Asassins
a <- test %>% 
  filter(value %in% c('Ragnaros', 'Tychus', 'Falstad', 'Valla', 'Zeratul', 'Li-Ming', 'Jaina', 'Thrall', "Gul'dan", 'Greymane')) %>% 
  group_by(variable, value) %>% 
  arrange(value)

ann_text <- ann_text %>% 
  filter(value %in% c('Ragnaros', 'Tychus', 'Falstad', 'Valla', 'Zeratul', 'Li-Ming', 'Jaina', 'Thrall', "Gul'dan", 'Greymane'))

a <- as.data.frame(a)

# Warriors
a <- test %>% 
  filter(value %in% c('E.T.C.', 'Zarya', 'Dehaka', 'Varian', 'Diablo', 'Artanis', 'Tyrael', 'Muradin', 'Johanna', 'Leoric')) %>% 
  group_by(variable, value) %>% 
  arrange(value)

ann_text <- ann_text %>% 
  filter(value %in% c('E.T.C.', 'Zarya', 'Dehaka', 'Varian', 'Diablo', 'Artanis', 'Tyrael', 'Muradin', 'Johanna', 'Leoric'))

a <- as.data.frame(a)

# Back to normal ------

a$variable <- factor(a$variable, levels = c('ban.1', 'ban.2', 'pick.1', 'pick.2', 'pick.3', 'pick.4', 'pick.5', 'ban.3',
                                            'ban.4', 'pick.6', 'pick.7', 'pick.8', 'pick.9', 'pick.10'))

library(ggplot2)
library(ggthemes)

plot <- a %>% 
ggplot(aes(x = variable, fill = value, group = value)) +
  geom_rect(xmin = 0.5, xmax = 1.5, ymin = 0, ymax = 1, fill = 'grey99', alpha = .02, color = 'black') +
  geom_rect(xmin = 1.5, xmax = 2.5, ymin = 0, ymax = 1, fill = 'grey75', alpha = .02, color = 'black') +
  geom_rect(xmin = 2.5, xmax = 3.5, ymin = 0, ymax = 1, fill = 'grey99', alpha = .02, color = 'black') +
  geom_rect(xmin = 3.5, xmax = 5.5, ymin = 0, ymax = 1, fill = 'grey75', alpha = .02, color = 'black') +
  geom_rect(xmin = 5.5, xmax = 7.5, ymin = 0, ymax = 1, fill = 'grey99', alpha = .02, color = 'black') +
  geom_rect(xmin = 7.5, xmax = 8.5, ymin = 0, ymax = 1, fill = 'grey75', alpha = .02, color = 'black') +
  geom_rect(xmin = 8.5, xmax = 9.5, ymin = 0, ymax = 1, fill = 'grey99', alpha = .02, color = 'black') +
  geom_rect(xmin = 9.5, xmax = 11.5, ymin = 0, ymax = 1, fill = 'grey75', alpha = .02, color = 'black') +
  geom_rect(xmin = 11.5, xmax = 13.5, ymin = 0, ymax = 1, fill = 'grey99', alpha = .02, color = 'black') +
  geom_rect(xmin = 13.5, xmax = 14.5, ymin = 0, ymax = 1, fill = 'grey75', alpha = .02, color = 'black') +
  geom_label(data = ann_text, aes(x = x, y = y, group = value, label = paste0(scales::percent(round(involvement,2)), '\nPOPULARITY')), size = 2.3,  color = NA, fill = 'grey75', alpha = 0.02) +
  geom_text(data = ann_text, aes(x = x, y = y, group = value, label = paste0(scales::percent(round(involvement,2)), '\nPOPULARITY')), size = 2.3,  color = 'black', alpha = 0.5) +
  geom_density(alpha = .95) +
  scale_y_continuous(limits = c(0,1)) +
  scale_x_discrete(drop = FALSE,
                   labels = c('Ban\n1', 'Ban\n2', 'Pick\n1', 'Pick\n2', 'Pick\n3', 'Pick\n4', 'Pick\n5', 'Ban\n3', 'Ban\n4',
                              'Pick\n6', 'Pick\n7', 'Pick\n8', 'Pick\n9', 'Pick\n10')) +
  theme_fivethirtyeight() +
  theme(panel.grid.major.x = element_blank(),
        legend.position = 'none',
        strip.text.x = element_text(size = 12),
        strip.text.y = element_text(size = 8),
        axis.text.y = element_blank(),
        plot.caption = element_text(vjust = -2, color = 'grey62')) +
  labs(title = 'Heroes of the Storm HGC Phase 1 - Western Meta\nWeeks 1-5: Specialists',
       subtitle = 'Probability of specialist heroes being selected throughout the draft.',
       caption = '@MattDaviz, ggplots.com                                                                                                                        Source: /u/Dthehunter, HGC Weeks: 1-5') +
  facet_grid(value ~ region)

plot

setwd('C:\\Users\\mattd\\Dropbox\\HotS\\HGC Viz')
#setwd('C:\\Users\\Matthew\\Dropbox\\HotS\\HGC Viz')
#setwd('C:\\users\\Matt\\Dropbox\\HotS\\HGC Viz')

# Create unique output filename
output_filename <- 'HGC Western - Weeks 1-5 - Specialists.jpeg'

# Open the file for the plot to be written to
jpeg(output_filename, height = 2400, width = 3150, res = 300, quality = 400)

print(plot)
dev.off()

wtf <- a %>% 
  filter(region == 'Europe' & value == 'E.T.C.') %>% 
  group_by(variable)
  
  
na <- data %>% 
  filter(region == 'North America')

?geom_density()
wtf$variable <- factor(wtf$variable, levels = c('ban.1', 'ban.2', 'pick.1', 'pick.2', 'pick.3', 'pick.4', 'pick.5', 'ban.3',
                                              'ban.4', 'pick.6', 'pick.7', 'pick.8', 'pick.9', 'pick.10', 'not.picked'))

ggplot(wtf,aes(x = variable, fill = value, group = value)) +
  geom_density(position = 'stack', stat = 'count') +
  scale_x_discrete(drop = FALSE)
?geom_density()

ggplot(wtf,aes(variable,color = value, fill = value, group = value)) +
  geom_density() +
  scale_x_discrete(drop = FALSE)

# Game 1: Team Dignitas vs Team 8 -------

# Team 8 Cleanup
team8 <- data %>% 
  filter(team.a == 'Team 8' | team.b == 'Team 8')

# What maps do they choose to play on?
team8 %>% 
  filter(map.pick.team == 'Team 8') %>% 
  group_by(map) %>% 
  summarize(number = n()) %>% 
  arrange(desc(number))

# What maps do their opponents choose to play on?
team8 %>% 
  filter(first.pick.team == 'Team 8') %>% 
  group_by(map) %>% 
  summarize(number = n()) %>% 
  arrange(desc(number))

# Team 8 First Pick drafts
team8.fp <- team8 %>% 
  filter(first.pick.team == 'Team 8')
team8.fp <- gather(team8.fp[,c(7,9:22)], variable, value, -first.pick.team)
colnames(team8.fp) <- c('team', 'variable', 'value')

# Team 8 Map Pick Drafts
team8.mp <- team8 %>% 
  filter(map.pick.team == 'Team 8')
team8.mp <- gather(team8.mp[,c(8,9:22)], variable, value, -map.pick.team)
colnames(team8.mp) <- c('team', 'variable', 'value')

# Combine together
team8.drafts <- rbind(team8.fp, team8.mp)

# Team Dignitas Cleanup
dignitas <- data %>% 
  filter(team.a == 'Team Dignitas' | team.b == 'Team Dignitas')

# What maps do they choose to play on?
dignitas %>% 
  filter(map.pick.team == 'Team Dignitas') %>% 
  group_by(map) %>% 
  summarize(times.selected = n()) %>% 
  arrange(desc(times.selected))

# What maps do their opponents choose to play on?
dignitas %>% 
  filter(first.pick.team == 'Team Dignitas') %>% 
  group_by(map) %>% 
  summarize(number = n()) %>% 
  arrange(desc(number))

# Team Dignitas First Pick Drafts
dignitas.fp <- dignitas %>% 
  filter(first.pick.team == 'Team Dignitas')
dignitas.fp <- gather(dignitas.fp[,c(7,9:22)], variable, value, -first.pick.team)
colnames(dignitas.fp) <- c('team', 'variable', 'value')

# Team Dignitas Map Pick Drafts
dignitas.mp <- dignitas %>% 
  filter(map.pick.team == 'Team Dignitas')
dignitas.mp <- gather(dignitas.mp[,c(8,9:22)], variable, value, -map.pick.team)
colnames(dignitas.mp) <- c('team', 'variable', 'value')

# Combine together
dignitas.drafts <- rbind(dignitas.fp, dignitas.mp)

# Combine Team Dignitas and Team 8 drafts together
game1.drafts <- rbind(team8.drafts, dignitas.drafts)

# Find out the 20 most played heroes in all matches the two teams have played in
game1.drafts %>% 
  group_by(value) %>% 
  summarize(count = n()) %>% 
  #filter(count > 6) %>% 
  top_n(n = 20) %>% 
  arrange(desc(count))

# Filter all data to only include the 20 most popular heroes between the two teams
game1.drafts <- game1.drafts %>% 
  filter(value %in% c('Malfurion', 'Tassadar', 'Ragnaros', 'Rehgar', 'Tychus', 'Valla', 'E.T.C.', 'Varian', 'Falstad', 'Medivh', 'Diablo', 'Zarya', 'Zeratul', 'Artanis', 'Dehaka'))

# Reset factor of variable level to order the x-axis of the following graph
game1.drafts$variable <- factor(game1.drafts$variable, levels = c('ban.1', 'ban.2', 'pick.1', 'pick.2', 'pick.3', 'pick.4', 'pick.5', 'ban.3',
                                            'ban.4', 'pick.6', 'pick.7', 'pick.8', 'pick.9', 'pick.10'))

# Create data frames to grab annotation text (ann_text)
new <- rbind(dignitas, team8)
test <- melt(new[,c(1,9:22)], id = c('region'))

text.labels <- test %>% 
  group_by(region, variable) %>% 
  mutate(games = n())

text.labels <- text.labels %>% 
  group_by(region, value) %>% 
  mutate(involvement = n() /games) %>% 
  filter(n() > 2)

ann_text <- text.labels %>% 
  distinct(region, value, involvement) 
ann_text <- ann_text[,c(1:3)]
ann_text$x = 12.5
ann_text$y = .6

ann_text <- ann_text %>% 
  filter(value %in% c('Malfurion', 'Tassadar', 'Ragnaros', 'Rehgar', 'Tychus', 'Valla', 'E.T.C.', 'Varian', 'Falstad', 'Medivh', 'Diablo', 'Zarya', 'Zeratul', 'Artanis', 'Dehaka'))
ann_text$region <- gsub('Europe', 'Team Dignitas', ann_text$region)
ann_text$region <- gsub('North America', 'Team 8', ann_text$region)
colnames(ann_text) <- c('team', 'value', 'involvement', 'x', 'y')


library(ggplot2)
library(ggthemes)

#a <- a %>% 
#  filter(value != 'Chen' & value != 'Rexxar' & value != 'Sonya' & value != 'Stitches' & value != 'Cho' & value != 'Arthas' &
#           value != "Anub'arak")

plot <- game1.drafts %>% 
  ggplot(aes(x = variable, fill = value, group = value)) +
  geom_rect(xmin = 0.5, xmax = 1.5, ymin = 0, ymax = 1, fill = 'grey99', alpha = .02, color = 'black') +
  geom_rect(xmin = 1.5, xmax = 2.5, ymin = 0, ymax = 1, fill = 'grey75', alpha = .02, color = 'black') +
  geom_rect(xmin = 2.5, xmax = 3.5, ymin = 0, ymax = 1, fill = 'grey99', alpha = .02, color = 'black') +
  geom_rect(xmin = 3.5, xmax = 5.5, ymin = 0, ymax = 1, fill = 'grey75', alpha = .02, color = 'black') +
  geom_rect(xmin = 5.5, xmax = 7.5, ymin = 0, ymax = 1, fill = 'grey99', alpha = .02, color = 'black') +
  geom_rect(xmin = 7.5, xmax = 8.5, ymin = 0, ymax = 1, fill = 'grey75', alpha = .02, color = 'black') +
  geom_rect(xmin = 8.5, xmax = 9.5, ymin = 0, ymax = 1, fill = 'grey99', alpha = .02, color = 'black') +
  geom_rect(xmin = 9.5, xmax = 11.5, ymin = 0, ymax = 1, fill = 'grey75', alpha = .02, color = 'black') +
  geom_rect(xmin = 11.5, xmax = 13.5, ymin = 0, ymax = 1, fill = 'grey99', alpha = .02, color = 'black') +
  geom_rect(xmin = 13.5, xmax = 14.5, ymin = 0, ymax = 1, fill = 'grey75', alpha = .02, color = 'black') +
  geom_label(data = ann_text, aes(x = x, y = y, group = value, label = paste0(scales::percent(round(involvement,2)), '\nPOPULARITY')), size = 2.3,  color = NA, fill = 'grey75', alpha = 0.02) +
  geom_text(data = ann_text, aes(x = x, y = y, group = value, label = paste0(scales::percent(round(involvement,2)), '\nPOPULARITY')), size = 2.3,  color = 'black', alpha = 0.5) +
  geom_density(alpha = .95) +
  scale_y_continuous(limits = c(0,1)) +
  scale_x_discrete(drop = FALSE,
                   labels = c('Ban\n1', 'Ban\n2', 'Pick\n1', 'Pick\n2', 'Pick\n3', 'Pick\n4', 'Pick\n5', 'Ban\n3', 'Ban\n4',
                              'Pick\n6', 'Pick\n7', 'Pick\n8', 'Pick\n9', 'Pick\n10')) +
  theme_fivethirtyeight() +
  theme(panel.grid.major.x = element_blank(),
        legend.position = 'none',
        strip.text.x = element_text(size = 12),
        strip.text.y = element_text(size = 6),
        plot.caption = element_text(vjust = -2, color = 'grey62'),
        axis.text.y = element_blank()) +
  labs(title = 'Western Clash, Match 2: Team 8 vs. Team Dignitas',
       subtitle = 'Probability of the most popular heroes being selected throughout the draft.',
       caption = '@MattDaviz, ggplots.com                                                                                                                        Source: /u/Dthehunter, HGC Weeks: 1-5') +
  facet_grid(value ~ team)

plot

setwd('C:\\Users\\mattd\\Dropbox\\HotS\\HGC Viz')
#setwd('C:\\Users\\Matthew\\Dropbox\\HotS\\HGC Viz')
#setwd('C:\\users\\Matt\\Dropbox\\HotS\\HGC Viz')

# Create unique output filename
output_filename <- 'HGC Western Clash - Team 8 vs Team Dignitas.jpeg'

# Open the file for the plot to be written to
jpeg(output_filename, height = 2400, width = 3150, res = 300, quality = 400)

print(plot)
dev.off()

# Match 3: Gale Force eSports vs Fnatic ----------

gfe <- data %>% 
  filter(team.a == 'Gale Force eSports' | team.b == 'Gale Force eSports')

gfe.fp <- gfe %>% 
  filter(first.pick.team == 'Gale Force eSports')
gfe.fp <- gather(gfe.fp[,c(7,9:22)], variable, value, -first.pick.team)
colnames(gfe.fp) <- c('team', 'variable', 'value')

gfe.mp <- gfe %>% 
  filter(map.pick.team == 'Gale Force eSports')
gfe.mp <- gather(gfe.mp[,c(8,9:22)], variable, value, -map.pick.team)
colnames(gfe.mp) <- c('team', 'variable', 'value')

gfe.drafts <- rbind(gfe.fp, gfe.mp)

# What maps do they choose to play on?
gfe %>% 
  filter(map.pick.team == 'Gale Force eSports') %>% 
  group_by(map) %>% 
  summarize(number = n()) %>% 
  arrange(desc(number))

# What maps do their opponents choose to play on?
gfe %>% 
  filter(first.pick.team == 'Gale Force eSports') %>% 
  group_by(map) %>% 
  summarize(number = n()) %>% 
  arrange(desc(number))

fnatic <- data %>% 
  filter(team.a == 'Fnatic' | team.b == 'Fnatic')

fnatic.fp <- fnatic %>% 
  filter(first.pick.team == 'Fnatic')
fnatic.fp <- gather(fnatic.fp[,c(7,9:22)], variable, value, -first.pick.team)
colnames(fnatic.fp) <- c('team', 'variable', 'value')

fnatic.mp <- fnatic %>% 
  filter(map.pick.team == 'Fnatic')
fnatic.mp <- gather(fnatic.mp[,c(8,9:22)], variable, value, -map.pick.team)
colnames(fnatic.mp) <- c('team', 'variable', 'value')

fnatic.drafts <- rbind(fnatic.fp, fnatic.mp)

# What maps do they choose to play on?
fnatic %>% 
  filter(map.pick.team == 'Fnatic') %>% 
  group_by(map) %>% 
  summarize(number = n()) %>% 
  arrange(desc(number))

# What maps do their opponents choose to play on?
fnatic %>% 
  filter(first.pick.team == 'Fnatic') %>% 
  group_by(map) %>% 
  summarize(number = n()) %>% 
  arrange(desc(number))

game3.drafts <- rbind(gfe.drafts, fnatic.drafts)

game3.drafts %>% 
  group_by(value) %>% 
  summarize(count = n()) %>% 
  #filter(count > 6) %>% 
  top_n(n = 20) %>% 
  arrange(desc(count))

game3.drafts <- game3.drafts %>% 
  filter(value %in% c('Malfurion', 'Tassadar', 'Ragnaros', 'Rehgar', 'Tychus', 'E.T.C.', 'Dehaka', 'Falstad', 'Zeratul', 'Valla', 'Diablo', 'Zarya', 'Tyrael', 'Li-Ming', 'Varian'))


game3.drafts$variable <- factor(game1.drafts$variable, levels = c('ban.1', 'ban.2', 'pick.1', 'pick.2', 'pick.3', 'pick.4', 'pick.5', 'ban.3',
                                                                  'ban.4', 'pick.6', 'pick.7', 'pick.8', 'pick.9', 'pick.10'))

new <- rbind(gfe, fnatic)
test <- melt(new[,c(1,9:22)], id = c('region'))

text.labels <- test %>% 
  group_by(region, variable) %>% 
  mutate(games = n())

text.labels <- text.labels %>% 
  group_by(region, value) %>% 
  mutate(involvement = n() /games) %>% 
  filter(n() > 2)

ann_text <- text.labels %>% 
  distinct(region, value, involvement) 
ann_text <- ann_text[,c(1:3)]
ann_text$x = 12.5
ann_text$y = .6

ann_text <- ann_text %>% 
  filter(value %in% c('Malfurion', 'Tassadar', 'Ragnaros', 'Rehgar', 'Tychus', 'E.T.C.', 'Dehaka', 'Falstad', 'Zeratul', 'Valla', 'Diablo', 'Zarya', 'Tyrael', 'Li-Ming', 'Varian'))
ann_text$region <- gsub('Europe', 'Fnatic', ann_text$region)
ann_text$region <- gsub('North America', 'Gale Force eSports', ann_text$region)
colnames(ann_text) <- c('team', 'value', 'involvement', 'x', 'y')


library(ggplot2)
library(ggthemes)

plot <- game3.drafts %>% 
  ggplot(aes(x = variable, fill = value, group = value)) +
  geom_rect(xmin = 0.5, xmax = 1.5, ymin = 0, ymax = 1, fill = 'grey99', alpha = .02, color = 'black') +
  geom_rect(xmin = 1.5, xmax = 2.5, ymin = 0, ymax = 1, fill = 'grey75', alpha = .02, color = 'black') +
  geom_rect(xmin = 2.5, xmax = 3.5, ymin = 0, ymax = 1, fill = 'grey99', alpha = .02, color = 'black') +
  geom_rect(xmin = 3.5, xmax = 5.5, ymin = 0, ymax = 1, fill = 'grey75', alpha = .02, color = 'black') +
  geom_rect(xmin = 5.5, xmax = 7.5, ymin = 0, ymax = 1, fill = 'grey99', alpha = .02, color = 'black') +
  geom_rect(xmin = 7.5, xmax = 8.5, ymin = 0, ymax = 1, fill = 'grey75', alpha = .02, color = 'black') +
  geom_rect(xmin = 8.5, xmax = 9.5, ymin = 0, ymax = 1, fill = 'grey99', alpha = .02, color = 'black') +
  geom_rect(xmin = 9.5, xmax = 11.5, ymin = 0, ymax = 1, fill = 'grey75', alpha = .02, color = 'black') +
  geom_rect(xmin = 11.5, xmax = 13.5, ymin = 0, ymax = 1, fill = 'grey99', alpha = .02, color = 'black') +
  geom_rect(xmin = 13.5, xmax = 14.5, ymin = 0, ymax = 1, fill = 'grey75', alpha = .02, color = 'black') +
  geom_label(data = ann_text, aes(x = x, y = y, group = value, label = paste0(scales::percent(round(involvement,2)), '\nPOPULARITY')), size = 2.3,  color = NA, fill = 'grey75', alpha = 0.02) +
  geom_text(data = ann_text, aes(x = x, y = y, group = value, label = paste0(scales::percent(round(involvement,2)), '\nPOPULARITY')), size = 2.3,  color = 'black', alpha = 0.5) +
  geom_density(alpha = .95) +
  scale_y_continuous(limits = c(0,1)) +
  scale_x_discrete(drop = FALSE,
                   labels = c('Ban\n1', 'Ban\n2', 'Pick\n1', 'Pick\n2', 'Pick\n3', 'Pick\n4', 'Pick\n5', 'Ban\n3', 'Ban\n4',
                              'Pick\n6', 'Pick\n7', 'Pick\n8', 'Pick\n9', 'Pick\n10')) +
  theme_fivethirtyeight() +
  theme(panel.grid.major.x = element_blank(),
        legend.position = 'none',
        strip.text.x = element_text(size = 12),
        strip.text.y = element_text(size = 6),
        plot.caption = element_text(vjust = -2, color = 'grey62'),
        axis.text.y = element_blank()) +
  labs(title = 'Western Clash, Match 4: Gale Force eSports vs. Fnatic',
       subtitle = 'Probability of the most popular heroes being selected throughout the draft.',
       caption = '@MattDaviz, ggplots.com                                                                                                                        Source: /u/Dthehunter, HGC Weeks: 1-5') +
  facet_grid(value ~ team)

plot

setwd('C:\\Users\\mattd\\Dropbox\\HotS\\HGC Viz')
#setwd('C:\\Users\\Matthew\\Dropbox\\HotS\\HGC Viz')
#setwd('C:\\users\\Matt\\Dropbox\\HotS\\HGC Viz')

# Create unique output filename
output_filename <- 'HGC Western Clash - Gale Force eSports vs Fnatic.jpeg'

# Open the file for the plot to be written to
jpeg(output_filename, height = 2400, width = 3150, res = 300, quality = 400)

print(plot)
dev.off()


dev.off()


# Match 1: Misfits vs Nomia ----------

misfits <- data %>% 
  filter(team.a == 'Misfits' | team.b == 'Misfits')

misfits.fp <- misfits %>% 
  filter(first.pick.team == 'Misfits')
misfits.fp <- gather(misfits.fp[,c(7,9:22)], variable, value, -first.pick.team)
colnames(misfits.fp) <- c('team', 'variable', 'value')

misfits.mp <- misfits %>% 
  filter(map.pick.team == 'Misfits')
misfits.mp <- gather(misfits.mp[,c(8,9:22)], variable, value, -map.pick.team)
colnames(misfits.mp) <- c('team', 'variable', 'value')

misfits.drafts <- rbind(misfits.fp, misfits.mp)

# What maps do they choose to play on?
misfits %>% 
  filter(map.pick.team == 'Misfits') %>% 
  group_by(map) %>% 
  summarize(number = n()) %>% 
  arrange(desc(number))

# What maps do their opponents choose to play on?
misfits %>% 
  filter(first.pick.team == 'Misfits') %>% 
  group_by(map) %>% 
  summarize(number = n()) %>% 
  arrange(desc(number))

game3.drafts <- rbind(misfits.drafts)

game3.drafts %>% 
  group_by(value) %>% 
  summarize(count = n()) %>% 
  #filter(count > 6) %>% 
  top_n(n = 20) %>% 
  arrange(desc(count))

game3.drafts <- game3.drafts %>% 
  filter(value %in% c('Tassadar', 'Ragnaros', 'Malfurion', 'Tychus', 'Rehgar', 'Dehaka', 'Falstad', 'Zeratul', 'Muradin', 'E.T.C.', 'Jaina', 'Tyrael', 'Li-Ming', 'Varian', 'Valla'))


game3.drafts$variable <- factor(game3.drafts$variable, levels = c('ban.1', 'ban.2', 'pick.1', 'pick.2', 'pick.3', 'pick.4', 'pick.5', 'ban.3',
                                                                  'ban.4', 'pick.6', 'pick.7', 'pick.8', 'pick.9', 'pick.10'))

new <- rbind(misfits)
library(reshape2)
test <- melt(new[,c(1,9:22)], id = c('region'))

text.labels <- test %>% 
  group_by(region, variable) %>% 
  mutate(games = n())

text.labels <- text.labels %>% 
  group_by(region, value) %>% 
  mutate(involvement = n() /games) %>% 
  filter(n() > 2)

ann_text <- text.labels %>% 
  distinct(region, value, involvement) 
ann_text <- ann_text[,c(1:3)]
ann_text$x = 12.5
ann_text$y = .6

ann_text <- ann_text %>% 
  filter(value %in% c('Tassadar', 'Ragnaros', 'Malfurion', 'Tychus', 'Rehgar', 'Dehaka', 'Falstad', 'Zeratul', 'Muradin', 'E.T.C.', 'Jaina', 'Tyrael', 'Li-Ming', 'Varian', 'Valla'))
ann_text$region <- gsub('Europe', 'Misfits', ann_text$region)
colnames(ann_text) <- c('team', 'value', 'involvement', 'x', 'y')


library(ggplot2)
library(ggthemes)

plot <- game3.drafts %>% 
  ggplot(aes(x = variable, fill = value, group = value)) +
  geom_rect(xmin = 0.5, xmax = 1.5, ymin = 0, ymax = 1, fill = 'grey99', alpha = .02, color = 'black') +
  geom_rect(xmin = 1.5, xmax = 2.5, ymin = 0, ymax = 1, fill = 'grey75', alpha = .02, color = 'black') +
  geom_rect(xmin = 2.5, xmax = 3.5, ymin = 0, ymax = 1, fill = 'grey99', alpha = .02, color = 'black') +
  geom_rect(xmin = 3.5, xmax = 5.5, ymin = 0, ymax = 1, fill = 'grey75', alpha = .02, color = 'black') +
  geom_rect(xmin = 5.5, xmax = 7.5, ymin = 0, ymax = 1, fill = 'grey99', alpha = .02, color = 'black') +
  geom_rect(xmin = 7.5, xmax = 8.5, ymin = 0, ymax = 1, fill = 'grey75', alpha = .02, color = 'black') +
  geom_rect(xmin = 8.5, xmax = 9.5, ymin = 0, ymax = 1, fill = 'grey99', alpha = .02, color = 'black') +
  geom_rect(xmin = 9.5, xmax = 11.5, ymin = 0, ymax = 1, fill = 'grey75', alpha = .02, color = 'black') +
  geom_rect(xmin = 11.5, xmax = 13.5, ymin = 0, ymax = 1, fill = 'grey99', alpha = .02, color = 'black') +
  geom_rect(xmin = 13.5, xmax = 14.5, ymin = 0, ymax = 1, fill = 'grey75', alpha = .02, color = 'black') +
  geom_label(data = ann_text, aes(x = x, y = y, group = value, label = paste0(scales::percent(round(involvement,2)), '\nPOPULARITY')), size = 2.3,  color = NA, fill = 'grey75', alpha = 0.02) +
  geom_text(data = ann_text, aes(x = x, y = y, group = value, label = paste0(scales::percent(round(involvement,2)), '\nPOPULARITY')), size = 2.3,  color = 'black', alpha = 0.5) +
  geom_density(alpha = .95) +
  scale_y_continuous(limits = c(0,1)) +
  scale_x_discrete(drop = FALSE,
                   labels = c('Ban\n1', 'Ban\n2', 'Pick\n1', 'Pick\n2', 'Pick\n3', 'Pick\n4', 'Pick\n5', 'Ban\n3', 'Ban\n4',
                              'Pick\n6', 'Pick\n7', 'Pick\n8', 'Pick\n9', 'Pick\n10')) +
  theme_fivethirtyeight() +
  theme(panel.grid.major.x = element_blank(),
        legend.position = 'none',
        strip.text.x = element_text(size = 12),
        strip.text.y = element_text(size = 6),
        plot.caption = element_text(vjust = -2, color = 'grey62'),
        axis.text.y = element_blank()) +
  labs(title = 'Western Clash, Match 1: Misfits',
       subtitle = 'Probability of the most popular heroes being selected throughout the draft.',
       caption = '@MattDaviz, ggplots.com                                                                                                                        Source: /u/Dthehunter, HGC Weeks: 1-5') +
  facet_grid(value ~ team)

plot

setwd('C:\\Users\\mattd\\Dropbox\\HotS\\HGC Viz')
#setwd('C:\\Users\\Matthew\\Dropbox\\HotS\\HGC Viz')
#setwd('C:\\users\\Matt\\Dropbox\\HotS\\HGC Viz')

# Create unique output filename
output_filename <- 'HGC Western Clash - Misfits.jpeg'

# Open the file for the plot to be written to
jpeg(output_filename, height = 2400, width = 3150, res = 300, quality = 400)

print(plot)
dev.off()


dev.off()


# Match 3: tempo.storm vs infamous ----------

tempo.storm <- data %>% 
  filter(team.a == 'Tempo Storm' | team.b == 'Tempo Storm')

tempo.storm.fp <- tempo.storm %>% 
  filter(first.pick.team == 'Tempo Storm')
tempo.storm.fp <- gather(tempo.storm.fp[,c(7,9:22)], variable, value, -first.pick.team)
colnames(tempo.storm.fp) <- c('team', 'variable', 'value')

tempo.storm.mp <- tempo.storm %>% 
  filter(map.pick.team == 'Tempo Storm')
tempo.storm.mp <- gather(tempo.storm.mp[,c(8,9:22)], variable, value, -map.pick.team)
colnames(tempo.storm.mp) <- c('team', 'variable', 'value')

tempo.storm.drafts <- rbind(tempo.storm.fp, tempo.storm.mp)

# What maps do they choose to play on?
tempo.storm %>% 
  filter(map.pick.team == 'Tempo Storm') %>% 
  group_by(map) %>% 
  summarize(number = n()) %>% 
  arrange(desc(number))

# What maps do their opponents choose to play on?
tempo.storm %>% 
  filter(first.pick.team == 'Tempo Storm') %>% 
  group_by(map) %>% 
  summarize(number = n()) %>% 
  arrange(desc(number))

game3.drafts <- rbind(tempo.storm.drafts)

game3.drafts %>% 
  group_by(value) %>% 
  summarize(count = n()) %>% 
  #filter(count > 6) %>% 
  top_n(n = 20) %>% 
  arrange(desc(count))

game3.drafts <- game3.drafts %>% 
  filter(value %in% c('Malfurion', 'Tychus', 'Tassadar', 'Ragnaros', 'Diablo', 'Zarya', 'Zeratul', 'Li-Ming', 'Artanis', 'Falstad', 'Rehgar', 'E.T.C.', 'Kharazim', 'Varian', 'Valla'))


game3.drafts$variable <- factor(game3.drafts$variable, levels = c('ban.1', 'ban.2', 'pick.1', 'pick.2', 'pick.3', 'pick.4', 'pick.5', 'ban.3',
                                                                  'ban.4', 'pick.6', 'pick.7', 'pick.8', 'pick.9', 'pick.10'))

new <- rbind(tempo.storm)
library(reshape2)
test <- melt(new[,c(1,9:22)], id = c('region'))

text.labels <- test %>% 
  group_by(region, variable) %>% 
  mutate(games = n())

text.labels <- text.labels %>% 
  group_by(region, value) %>% 
  mutate(involvement = n() /games) %>% 
  filter(n() > 2)

ann_text <- text.labels %>% 
  distinct(region, value, involvement) 
ann_text <- ann_text[,c(1:3)]
ann_text$x = 12.5
ann_text$y = .6

ann_text <- ann_text %>% 
  filter(value %in% c('Malfurion', 'Tychus', 'Tassadar', 'Ragnaros', 'Diablo', 'Zarya', 'Zeratul', 'Li-Ming', 'Artanis', 'Falstad', 'Rehgar', 'E.T.C.', 'Kharazim', 'Varian', 'Valla'))
ann_text$region <- gsub('North America', 'Tempo Storm', ann_text$region)
colnames(ann_text) <- c('team', 'value', 'involvement', 'x', 'y')


library(ggplot2)
library(ggthemes)

plot <- game3.drafts %>% 
  ggplot(aes(x = variable, fill = value, group = value)) +
  geom_rect(xmin = 0.5, xmax = 1.5, ymin = 0, ymax = 1, fill = 'grey99', alpha = .02, color = 'black') +
  geom_rect(xmin = 1.5, xmax = 2.5, ymin = 0, ymax = 1, fill = 'grey75', alpha = .02, color = 'black') +
  geom_rect(xmin = 2.5, xmax = 3.5, ymin = 0, ymax = 1, fill = 'grey99', alpha = .02, color = 'black') +
  geom_rect(xmin = 3.5, xmax = 5.5, ymin = 0, ymax = 1, fill = 'grey75', alpha = .02, color = 'black') +
  geom_rect(xmin = 5.5, xmax = 7.5, ymin = 0, ymax = 1, fill = 'grey99', alpha = .02, color = 'black') +
  geom_rect(xmin = 7.5, xmax = 8.5, ymin = 0, ymax = 1, fill = 'grey75', alpha = .02, color = 'black') +
  geom_rect(xmin = 8.5, xmax = 9.5, ymin = 0, ymax = 1, fill = 'grey99', alpha = .02, color = 'black') +
  geom_rect(xmin = 9.5, xmax = 11.5, ymin = 0, ymax = 1, fill = 'grey75', alpha = .02, color = 'black') +
  geom_rect(xmin = 11.5, xmax = 13.5, ymin = 0, ymax = 1, fill = 'grey99', alpha = .02, color = 'black') +
  geom_rect(xmin = 13.5, xmax = 14.5, ymin = 0, ymax = 1, fill = 'grey75', alpha = .02, color = 'black') +
  geom_label(data = ann_text, aes(x = x, y = y, group = value, label = paste0(scales::percent(round(involvement,2)), '\nPOPULARITY')), size = 2.3,  color = NA, fill = 'grey75', alpha = 0.02) +
  geom_text(data = ann_text, aes(x = x, y = y, group = value, label = paste0(scales::percent(round(involvement,2)), '\nPOPULARITY')), size = 2.3,  color = 'black', alpha = 0.5) +
  geom_density(alpha = .95) +
  scale_y_continuous(limits = c(0,1)) +
  scale_x_discrete(drop = FALSE,
                   labels = c('Ban\n1', 'Ban\n2', 'Pick\n1', 'Pick\n2', 'Pick\n3', 'Pick\n4', 'Pick\n5', 'Ban\n3', 'Ban\n4',
                              'Pick\n6', 'Pick\n7', 'Pick\n8', 'Pick\n9', 'Pick\n10')) +
  theme_fivethirtyeight() +
  theme(panel.grid.major.x = element_blank(),
        legend.position = 'none',
        strip.text.x = element_text(size = 12),
        strip.text.y = element_text(size = 6),
        plot.caption = element_text(vjust = -2, color = 'grey62'),
        axis.text.y = element_blank()) +
  labs(title = 'Western Clash, Match 3: Tempo Storm',
       subtitle = 'Probability of the most popular heroes being selected throughout the draft.',
       caption = '@MattDaviz, ggplots.com                                                                                                                        Source: /u/Dthehunter, HGC Weeks: 1-5') +
  facet_grid(value ~ team)

plot

setwd('C:\\Users\\mattd\\Dropbox\\HotS\\HGC Viz')
#setwd('C:\\Users\\Matthew\\Dropbox\\HotS\\HGC Viz')
#setwd('C:\\users\\Matt\\Dropbox\\HotS\\HGC Viz')

# Create unique output filename
output_filename <- 'HGC Western Clash - Tempo Storm.jpeg'

# Open the file for the plot to be written to
jpeg(output_filename, height = 2400, width = 3150, res = 300, quality = 400)

print(plot)
dev.off()
# Western Clash Analysis --------

europe <- data %>% 
  filter(team.a %in% c('Misfits', 'Team Dignitas', 'Fnatic') |
           team.b %in% c('Misfits', 'Team Dignitas', 'Fnatic'))

northamerica <- data %>% 
  filter(team.a %in% c('Gale Force eSports', 'Tempo Storm', 'Team 8') |
           team.b %in% c('Gale Force eSports', 'Tempo Storm', 'Team 8'))

westernclash <- rbind(europe, northamerica)

# Other
results.win <- westernclash %>% 
  group_by(region, map, winner) %>% 
  summarize(wins = n())

results.loss <- westernclash %>% 
  group_by(region, map, loser) %>% 
  summarize(losses = n())

results <- full_join(results.win, results.loss, by = c('region' = 'region', 'map' = 'map', 'winner' = 'loser'))
results[is.na(results)] <- 0
results <- as.data.frame(results) %>% 
  select(map, winner, wins, losses)

results <- results %>% 
  gather(variable, value, -map, -winner)

results <- results %>% 
  filter(winner %in% c('Misfits', 'Team Dignitas', 'Fnatic', 'Gale Force eSports', 'Team 8', 'Tempo Storm'))

results$map <- str_wrap(results$map, 10)

plot <- ggplot(results, aes(x = map, y = value)) +
  geom_col(position = 'stack', aes(fill = variable)) +
  facet_wrap(~winner, ncol = 1) +
  theme_fivethirtyeight() +
  scale_y_continuous(limits = c(0,10),
                     breaks = c(seq(0,10,5))) +
  scale_fill_discrete('', labels = c('Losses', 'Wins')) +
  theme(strip.text = element_text(face = 'bold'),
        axis.title = element_text(face = 'bold')) +
  xlab('') +
  ylab('Number of Matches Played') +
  labs(title = 'HGC Western Clash: Team win rates on battlegrounds',
       subtitle = 'Wins and losses for HGC Western Clash teams on each battleground through HGC Phase 1',
       caption = '@MattDaviz, ggplots.com                                                                           Source: /u/Dthehunter, HGC Weeks: 1-5')
plot
setwd('C:\\Users\\mattd\\Dropbox\\HotS\\HGC Viz')

# Create unique output filename
output_filename <- 'HGC Western Clash - Phase 1 Win rates.jpeg'

# Open the file for the plot to be written to
jpeg(output_filename, height = 3150, width = 2400, res = 300, quality = 400)

print(plot)
dev.off()


western.results <- results %>% 
  filter(winner == 'Team 8' |
           winner == 'Gale Force eSports' |
           winner == 'Tempo Storm' |
           winner == 'Team Dignitas' |
           winner == 'Misfits' |
           winner == 'Fnatic')

fp.drafts <- data %>% 
  select(region, patch, date.played, team = first.pick.team, opponent = map.pick.team, ban.1, pick.1, pick.4, pick.5, ban.4, pick.8, pick.9, winner, loser)

mp.drafts <- data %>% 
  select(region, patch, date.played, team = map.pick.team, opponent = first.pick.team, ban.2, pick.2, pick.3, ban.3, pick.6, pick.7, pick.10, winner, loser)



na <- data %>% 
  filter(region == 'North America')

look <- data %>% 
  filter(map == 'Braxis Holdout')
