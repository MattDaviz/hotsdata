# Overall Win Rate Platinum+ -----------
overallplus$Win.Percent <- overallplus$avg.winrate
overallplus$Win.Percent <- gsub('%', '', overallplus$Win.Percent)
overallplus$Games.Played <- as.numeric(as.character(overallplus$Games.Played))
overallplus$Win.Percent <- as.numeric(as.character(overallplus$Win.Percent)) * .01
overallplus$hero <- as.factor(overallplus$hero)
overallplus$avg.winrate <- gsub('%', '', as.character(overallplus$avg.winrate))
overallplus$avg.winrate <- as.numeric(as.character(overallplus$avg.winrate)) * .01

overallplus <- overallplus %>% 
  rowwise() %>% 
  mutate(stderror = se(rnorm(n = as.numeric(as.character(Games.Played)), mean = as.numeric(as.character(gsub('%','',Win.Percent))))))

ann_text <- data.frame(Win.Percent = .85, hero = 'Varian', Role = factor('Assassin', levels = c('Assassin', 'Specialist', 'Support', 'Warrior')), y = overallplus$avg.winrate[overallplus$hero == 'Varian'], z = overallplus$Win.Percent[overallplus$hero == "Varian"], games = overallplus$Games.Played[overallplus$hero == "Varian"], stderror = overallplus$stderror[overallplus$hero == 'Varian'])

ann_line <- data.frame(hero = 'Varian', x = 'Varian', xend = 'Varian', Role = factor('Assassin', levels = c('Assassin', 'Specialist', 'Support', 'Warrior')), y = overallplus$Win.Percent[overallplus$hero == 'Varian'], yend = .85, Win.Percent = overallplus$Win.Percent[overallplus$hero=='Varian'], stderror = overallplus$stderror[overallplus$hero == 'Varian'])

# Order data
overallplus <- overallplus %>%
  arrange(Win.Percent)
overallplus$hero <- factor(overallplus$hero, levels = overallplus$hero)

# Plot
overallplusplot <- ggplot(data = overallplus, aes(x = hero, y = Win.Percent)) +
  #geom_bar(aes(alpha = Games.Played), stat = 'identity', width = 0.5, color = 'grey75') +
  geom_errorbar(aes(ymin = Win.Percent - (2*stderror), ymax = Win.Percent + (2*stderror), color = Role),size = 1, show.legend = FALSE) +
  geom_point(aes(x = hero, y = Win.Percent, shape = Map.Name, color = Role), size = 1, pch = 3, stroke = 1) +
  #geom_point(aes(x = hero, y = avg.winrate, shape = Map.Name), size = 1) +
  geom_hline(yintercept = .5, alpha = .75, lty = 2) +
  coord_flip(ylim = c(0,1)) +
  theme(legend.position = 'none') +
  scale_y_continuous(breaks = c(seq(0,1,.1)),
                     expand = c(0,0),
                     limits = c(-100,100),
                     labels = scales::percent, 
                     sec.axis = dup_axis()) +
  facet_grid(Role ~ ., scales = "free", space = "free") +
  theme_fivethirtyeight() +
  scale_color_discrete('Hero Win Rate on Map') +
  scale_shape_discrete('Hero Win Rate Overall', labels = c('')) +
  #scale_alpha_continuous('Total Games Played', 
  #                       labels = c('0  ', '500  ', '1,000  ', '1,500  ', '2,000  ','2,500  ', '3,000  '),
  #                       breaks = c(0,500,1000,1500,2000,2500,3000),
  #                       #breaks = c(0,500,1000),
  #                       limits = c(0,3000) ) +
  labs(title = paste0('Overall Hero league win rate'),
       subtitle = paste0('Hero league win rate across Platinum, Diamond, and Master leagues for the last 7 days.\nLast update: ', 
                         Sys.time(), 
                         ' CST.'),
       caption = '@MattDaviz, ggplots.com                                                                                             Source: HOTS LOGS') +
  theme(axis.title = element_text(face = 'bold')) +
  xlab('') +
  ylab('Win Rate') +
  geom_segment(data = ann_line, aes(x = ann_line$x, xend = ann_line$xend, y = ann_line$y + (2.5*ann_line$stderror), yend = ann_line$yend )) +
  geom_label(data = ann_text, label = paste0("There is a 95% certainty\n",
                                             "Varian's win rate on\n",
                                             overallplus$Map.Name[1],
                                             "\nis between\n",
                                             scales::percent(round(ann_text$z - (2*ann_text$stderror),3)),
                                             " and ",
                                             scales::percent(round(ann_text$z + (2*ann_text$stderror),3))), fill = '#F0F0F0', size = 2.5) +
  #guides(shape = guide_legend(override.aes= list(color = 'black'))) +
  #guides(alpha = guide_legend(byrow = TRUE, nrow = 1, override.aes = list(fill = '#C77CFF'))) +
  theme(legend.position = 'bottom',
        legend.box = 'vertical',
        legend.key = element_rect(colour = 'grey75', size = .5, linetype = 'solid'),
        axis.title = element_text(face = 'bold'),
        legend.spacing = unit(.05, 'line'))

overallplusplot

# Overall Win Rate - Bronze+ -----------
overallminus$Win.Percent <- overallminus$avg.winrate
overallminus$Win.Percent <- gsub('%', '', overallminus$Win.Percent)
overallminus$Games.Played <- as.numeric(as.character(overallminus$Games.Played))
overallminus$Win.Percent <- as.numeric(as.character(overallminus$Win.Percent)) * .01
overallminus$hero <- as.factor(overallminus$hero)
overallminus$avg.winrate <- gsub('%', '', as.character(overallminus$avg.winrate))
overallminus$avg.winrate <- as.numeric(as.character(overallminus$avg.winrate)) * .01

overallminus <- overallminus %>% 
  rowwise() %>% 
  mutate(stderror = se(rnorm(n = as.numeric(as.character(Games.Played)), mean = as.numeric(as.character(gsub('%','',Win.Percent))))))

ann_text <- data.frame(Win.Percent = .85, hero = 'Varian', Role = factor('Assassin', levels = c('Assassin', 'Specialist', 'Support', 'Warrior')), y = overallminus$avg.winrate[overallminus$hero == 'Varian'], z = overallminus$Win.Percent[overallminus$hero == "Varian"], games = overallminus$Games.Played[overallminus$hero == "Varian"], stderror = overallminus$stderror[overallminus$hero == 'Varian'])

ann_line <- data.frame(hero = 'Varian', x = 'Varian', xend = 'Varian', Role = factor('Assassin', levels = c('Assassin', 'Specialist', 'Support', 'Warrior')), y = overallminus$Win.Percent[overallminus$hero == 'Varian'], yend = .85, Win.Percent = overallminus$Win.Percent[overallminus$hero=='Varian'], stderror = overallminus$stderror[overallminus$hero == 'Varian'])

# Order data
overallminus <- overallminus %>%
  arrange(Win.Percent)
overallminus$hero <- factor(overallminus$hero, levels = overallminus$hero)

# Plot
overallminusplot <- ggplot(data = overallminus, aes(x = hero, y = Win.Percent)) +
  #geom_bar(aes(alpha = Games.Played), stat = 'identity', width = 0.5, color = 'grey75') +
  geom_errorbar(aes(ymin = Win.Percent - (2*stderror), ymax = Win.Percent + (2*stderror), color = Role),size = 1, show.legend = FALSE) +
  geom_point(aes(x = hero, y = Win.Percent, shape = Map.Name, color = Role), size = 1, pch = 3, stroke = 1) +
  #geom_point(aes(x = hero, y = avg.winrate, shape = Map.Name), size = 1) +
  geom_hline(yintercept = .5, alpha = .75, lty = 2) +
  coord_flip(ylim = c(0,1)) +
  theme(legend.position = 'none') +
  scale_y_continuous(breaks = c(seq(0,1,.1)),
                     expand = c(0,0),
                     limits = c(-100,100),
                     labels = scales::percent, 
                     sec.axis = dup_axis()) +
  facet_grid(Role ~ ., scales = "free", space = "free") +
  theme_fivethirtyeight() +
  scale_color_discrete('Hero Win Rate on Map') +
  scale_shape_discrete('Hero Win Rate Overall', labels = c('')) +
  #scale_alpha_continuous('Total Games Played', 
  #                       labels = c('0  ', '500  ', '1,000  ', '1,500  ', '2,000  ','2,500  ', '3,000  '),
  #                       breaks = c(0,500,1000,1500,2000,2500,3000),
  #                       #breaks = c(0,500,1000),
  #                       limits = c(0,3000) ) +
  labs(title = paste0('Overall Hero league win rate'),
       subtitle = paste0('Hero league win rate across Bronze, Silver, and Gold leagues for the last 7 days.\nLast update: ', 
                         Sys.time(), 
                         ' CST.'),
       caption = '@MattDaviz, ggplots.com                                                                                             Source: HOTS LOGS') +
  theme(axis.title = element_text(face = 'bold')) +
  xlab('') +
  ylab('Win Rate') +
  geom_segment(data = ann_line, aes(x = ann_line$x, xend = ann_line$xend, y = ann_line$y + (2.5*ann_line$stderror), yend = ann_line$yend )) +
  geom_label(data = ann_text, label = paste0("There is a 95% certainty\n",
                                             "Varian's win rate on\n",
                                             overallminus$Map.Name[1],
                                             "\nis between\n",
                                             scales::percent(round(ann_text$z - (2*ann_text$stderror),3)),
                                             " and ",
                                             scales::percent(round(ann_text$z + (2*ann_text$stderror),3))), fill = '#F0F0F0', size = 2.5) +
  #guides(shape = guide_legend(override.aes= list(color = 'black'))) +
  #guides(alpha = guide_legend(byrow = TRUE, nrow = 1, override.aes = list(fill = '#C77CFF'))) +
  theme(legend.position = 'bottom',
        legend.box = 'vertical',
        legend.key = element_rect(colour = 'grey75', size = .5, linetype = 'solid'),
        axis.title = element_text(face = 'bold'),
        legend.spacing = unit(.05, 'line'))

overallminusplot

