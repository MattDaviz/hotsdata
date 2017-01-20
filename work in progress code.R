plot <- ggplot(data = win.percent[win.percent$hero=='Artanis',], aes(x = subrole, y = value, fill = factor(subrole))) +
  geom_bar(width = 1, stat = 'identity', colour = 'black', show.legend = FALSE) +
  geom_hline(data = hline, aes(yintercept = y), color = 'black', lty = 2, size = 0.3, show.legend = TRUE) +
  geom_label(aes(x = subrole, y = max(value) + max(value/2.4), label = factor(subrole)),fill = '#F0F0F0', color = NA, show.legend = FALSE) +
  geom_text(aes(x = subrole, y = max(value) + max(value)/2.5, label = factor(subrole)), show.legend = FALSE) +
  scale_y_continuous(limits = c(0,.9), breaks = c(seq(0,9,.1))) +
  scale_fill_discrete('50% Win Rate', breaks = c('Tank'), labels = NULL) +
  facet_grid(. ~ variable, labeller = labeller(variable = labels)) +
  labs(title = ' win rate with and against different hero roles',
       subtitle = 'Hero league win rate across all leagues for the last 7 days. Last updated: ',
       caption = '@MattDaviz                                                                                                                                                                                 Source: HOTS LOGS') +
  theme(axis.ticks.y = element_blank())

plot + coord_polar() + theme_fivethirtyeight() + RadarTheme

boe <- left_join(boe, test[,c(1,5)], by = 'hero')
colnames(boe)[7] <- 'avg.winrate'

ann_text <- data.frame(Win.Percent.x = .65, hero = 'Li-Ming', Role = factor('Assassin', levels = c('Assassin', 'Specialist', 'Support', 'Warrior')), y = boe$avg.winrate[boe$hero == 'Li-Ming'], z = boe$Win.Percent.x[boe$hero == "Li-Ming"], games = boe$Games.Played[boe$hero == "Li-Ming"])

ann_line <- data.frame(hero = 'Li-Ming', x = 'Li-Ming', xend = 'Li-Ming', Role = factor('Assassin', levels = c('Assassin', 'Specialist', 'Support', 'Warrior')), y = boe$Win.Percent.x[boe$hero == 'Li-Ming'] * 1.025, yend = .625, Win.Percent.x = boe$Win.Percent.x[boe$hero=='Li-Ming'])

# Plot
plot <- ggplot(data = is, aes(x = hero, y = Win.Percent.x, fill = Role, group = Role)) +
  geom_bar(aes(alpha = Games.Played), stat = 'identity', width = 0.5, color = 'grey75') +
  geom_point(aes(x = hero, y = Win.Percent.y, shape = Map.Name), alpha = 0.25,  size = 1) +
  coord_flip() +
  theme(legend.position = 'none') +
  scale_y_continuous(limits = c(0,.7), 
                     breaks = c(seq(0,.6,.1)),
                     labels = scales::percent, 
                     sec.axis = dup_axis()) +
  geom_hline(yintercept = .5, alpha = .75, lty = 2) +
  facet_grid(Role ~ ., scales = "free", space = "free") +
  theme_fivethirtyeight() +
  scale_shape_discrete('Overall Hero Win Rate', labels = c('')) +
  scale_alpha_continuous('Total Games Played', labels = c('0', '1,000', '2,000', '3,000', '4,000'),
                         breaks = c(0,1000,2000,3000,4000), 
                         limits = c(0,4000)) +
  labs(title = paste0('Hero league win rate on '),
       subtitle = paste0('Hero league win rate across all leagues for the last 7 days.\nLast update: ', 
                         Sys.time(), 
                         ' CST.'),
       caption = '@MattDaviz                                                                                                                   Source: HOTS LOGS') +
  xlab('') +
  ylab('Win Rate') +
  geom_segment(data = ann_line, aes(x = ann_line$x, xend = ann_line$xend, y = ann_line$y, yend = ann_line$yend)) +
  geom_label(data = ann_text, label = paste0("Li-Ming's win rate\nin ", scales::comma(ann_text$games), " games on\nTomb of the Spider Queen\nis ", scales::percent(ann_text$z), ", which is\n", scales::percent(round(ann_text$z - ann_text$y,3)), 
                                             if(ann_text$z - ann_text$y > 0) {
                                               " better"
                                             } else {
                                               " worse"
                                             },
                                             " than his\noverall win rate"), size = 2.5, color = 'black', fill = "#F0F0F0") +
  guides(fill = FALSE) +
  guides(shape = guide_legend(override.aes= list(color = 'black'))) +
  guides(alpha = guide_legend(override.aes = list(fill = '#C77CFF'))) +
  theme(legend.position = 'bottom',
        legend.box = 'vertical',
        legend.key = element_rect(colour = 'grey75', size = .5, linetype = 'solid'),
        axis.title = element_text(face = 'bold'),
        legend.spacing = unit(.05, 'line'))

plot

#guides(alpha = guide_legend(nrow =1,title.position = 'left', override.aes = (list(fill = '#C77CFF')), shape = NA)) +
#guides(alpha = guide_legend(override.aes = list(fill = '#C77CFF', shape = NA))) +
#guides(shape = guide_legend(nrow = 1, title.position = 'left')) +
#guides(shape = guide_legend(override.aes = list(fill = NA))) +

mapdata <- left_join(mapdata, test[,c(1,5)], by = 'hero')

data <- mapdata %>%
  group_by(Map.Name, Role) %>%
  summarize(correlation = cor(Win.Percent.y, Win.Percent.x)) %>%
  filter(correlation < .57) %>% 
  arrange(desc(correlation))

other <- mapdata %>%
  filter(Map.Name == 'Battlefield of Eternity' & Role == 'Assassin' |
           Map.Name == 'Braxis Holdout' & Role == 'Specialist' |
           Map.Name == 'Tomb of the Spider Queen' & Role == 'Specialist' |
           Map.Name == 'Battlefield of Eternity' & Role == 'Specialist')

ggplot(mapdata, aes(x = Win.Percent.x, y = Win.Percent.y, label = hero)) +
  geom_point() +
  geom_text() +
  geom_smooth(method = 'lm', se = FALSE)

lapply(maps, function(x) ggplot(data = x, aes(x = Win.Percent.x, y = Win.Percent.y)) +
         geom_point() +
         geom_smooth(method = 'lm', se = FALSE) +
         labs(title = x$Map.Name))

# Order data
is <- is %>%
  arrange(Win.Percent.x)
is$hero <- factor(is$hero, levels = is$hero)
is$Role <- factor(is$Role, levels = c('Warrior', 'Support', 'Specialist', 'Assassin'))

other <- is

mapdata %>%
  group_by(hero) %>% 
  summarize(mean = mean(Win.Percent.x),
            sd = sd(Win.Percent.x)) %>% 
  arrange(sd)

mapdata %>%
  subset(hero == 'Varian')
library(ggrepel)
ggplot(other, aes(x = Win.Percent.x, y = Win.Percent.y, label = hero)) +
  geom_smooth(method = 'lm', se = FALSE) +
  geom_point() +
  geom_label_repel(size = 2) +
  facet_grid(Role ~ Map.Name) +
  theme_fivethirtyeight() +
  scale_y_continuous(labels = scales::percent, breaks = c(.3,.4,.5,.6), limits = c(.3,.6)) +
  scale_x_continuous(labels = scales::percent, breaks = c(.3,.4,.5,.6), limits = c(.3,.6)) +
  theme(axis.title = element_text(face = 'bold')) +
  xlab('Map Win Rate') +
  ylab('Overall Win Rate')

bhb <- as.data.frame(bhb)
str(bhb)
str(boe)

bhb %>%
  group_by(hero) %>%
  summarize(mean.wr = mean(Win.Percent),
            sd.wr = sd(Win.Percent))
