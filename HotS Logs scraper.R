# Load libraries
library(XML)
library(RCurl)
library(plyr)
library(stringr)
library(ggplot2)
library(ggthemes)
library(ggrepel)
library(dplyr)
library(reshape2)
library(grid)
library(scales)

# Screw around code
url <- getURL('https://www.hotslogs.com/sitewide/HeroDetails?Hero=Murky')
data <- readHTMLTable(url)
table1 <- as.data.frame(data[2])
table2 <- as.data.frame(data[3])
table3 <- as.data.frame(data[4])
table4 <- as.data.frame(data[5])
table5 <- as.data.frame(data[6])

# Get all hero roles and subroles
url <- getURL('https://www.hotslogs.com/Default')
test <- readHTMLTable(url)
test <- as.data.frame(test[2])
test <- test[,2:9]
colnames(test) <- c('hero', 'Games.Played', 'Games.Banned', 'Popularity', 'Win.Percent', 'delta.Win.Percent' ,'Role', 'Specialty')
# Clean up scraped data
test$hero <- gsub('%20', ' ', test$hero)
test$hero <- gsub('%27', "'", test$hero)
test$Games.Played <- gsub(',', '', test$Games.Played)
test$Win.Percent <- gsub('%', '', test$Win.Percent)
test$Games.Played <- as.numeric(test$Games.Played)
test$Win.Percent <- as.numeric(test$Win.Percent) * .01
test$hero <- as.factor(test$hero)

# Set up template for scraping
utmp <- 'https://www.hotslogs.com/sitewide/HeroDetails?Hero='

# List of names to loop over for scraping hero specific pages
names <- c("Artanis", "Samuro", "The%20Lost%20Vikings", "Sgt.%20Hammer", "Ragnaros", 
           "Nazeebo", "Anub%27arak", "Jaina", "Gazlowe", "Kael%27thas", "The%20Butcher", 
           "Diablo", "Malfurion", "Zarya", "Rehgar", "Kerrigan", "Dehaka", 
           "Thrall", "Tracer", "Azmodan", "Xul", "Kharazim", "Zul%27jin", 
           "Rexxar", "Brightwing", "Lt.%20Morales", "Raynor", "Tyrael", "Gul%27dan", 
           "Lunara", "Tyrande", "Nova", "Sylvanas", "Valla", "Varian", 
           "Johanna", "Arthas", "Chen", "E.T.C.", "Tychus", "Illidan", "Li%20Li", 
           "Chromie", "Tassadar", "Zeratul", "Uther", 
           "Leoric", "Auriel", "Li-Ming", "Muradin", "Falstad", "Alarak", 
            "Abathur", "Stitches", "Medivh", "Sonya", "Zagara", "Greymane", "Murky")

#m <- readHTMLTable(getURL('https://www.hotslogs.com/sitewide/HeroDetails?Hero=Murky'))
#m <- as.data.frame(m[5])
#m <- m[,c(2:4)]
#m$hero <- 'Murky'

loopnames <- gsub('%20', ' ', names)
loopnames <- gsub('%27', "'", loopnames)
#loopnames <- c(loopnames)

# Set up empty vectors to deposit scraped data frames into
tables <- vector('list', length(names))
tablesnew <- vector('list', length(names))

# Scraper for win rate by battlefield
for(i in 1:length(names)) {
  tables[[i]] <- readHTMLTable(getURL(paste0(utmp, names[i])), stringsAsFactors = FALSE)
  tablesnew[[i]] <- tables[[i]][[6]][,c(2:4)]
  tablesnew[[i]]$hero <- names[[i]]
  # Use a Sys.sleep() command to be nice to a host's domain
  #Sys.sleep(3)
}

# Convert nested data frames into single data frame
mapdata <- ldply(tablesnew, data.frame)
#mapdata <- rbind(mapdata, m)

# Clean up scraped mapdata
mapdata$hero <- gsub('%20', ' ', mapdata$hero)
mapdata$hero <- gsub('%27', "'", mapdata$hero)
mapdata$Games.Played <- gsub(',', '', mapdata$Games.Played)
mapdata$Win.Percent <- gsub('%', '', mapdata$Win.Percent)
mapdata$Map.Name <- as.factor(mapdata$Map.Name)
mapdata$Games.Played <- as.numeric(mapdata$Games.Played)
mapdata$Win.Percent <- as.numeric(mapdata$Win.Percent) * .01
mapdata$hero <- as.factor(mapdata$hero)

# Join scraped mapdata together with roles and subroles
mapdata <- left_join(mapdata, test[,c(1,7:8)], by = c('hero'))
mapdata <- mapdata %>%
  arrange(Win.Percent)

# Set up color palette for plotting based on roles
#hotscolors <- c('red2', 'darkorchid2', 'deepskyblue2', 'blue3')

# Subset mapdata based on map
boe <- mapdata[mapdata$Map.Name == 'Battlefield of Eternity',]
bhb <- mapdata[mapdata$Map.Name == "Blackheart's Bay",]
bh <- mapdata[mapdata$Map.Name == 'Braxis Holdout',]
ch <- mapdata[mapdata$Map.Name == 'Cursed Hollow',]
ds <- mapdata[mapdata$Map.Name == 'Dragon Shire',]
got <- mapdata[mapdata$Map.Name == 'Garden of Terror',]
hm <- mapdata[mapdata$Map.Name == 'Haunted Mines',]
is <- mapdata[mapdata$Map.Name == 'Infernal Shrines',]
st <- mapdata[mapdata$Map.Name == 'Sky Temple',]
tosq <- mapdata[mapdata$Map.Name == 'Tomb of the Spider Queen',]
tod <- mapdata[mapdata$Map.Name == 'Towers of Doom',]
whj <- mapdata[mapdata$Map.Name == 'Warhead Junction',]

# Set list of data frames
mapsplus <- list(bh, bhb, boe, ch, ds, got, hm, is, st, tod, tosq, wj)

# Set working directory to Dropbox folder
setwd('C://Users//mattd//Dropbox//HotS//Hero WR x Map.Name//geom_bar')

translate3_trans <- function() {
  trans <- function(x) x - .3
  inv   <- function(x) x + .3
  trans_new("translate3_trans", trans, inv)
}  

# Loop for alternative plot - Platinum, Diamond, Master
lapply(mapsplus, function(x) {
  
  # Create unique output filename
  output_filename <- paste0('Higher leagues WR on ', x$Map.Name,".jpeg")
  
  # Open the file for the plot to be written to
  jpeg(output_filename, height = 3150, width = 2400, res = 300, quality = 400)
  
  x <- left_join(x, overallplus[,c(1,5)], by = 'hero')
  
  x$Win.Percent <- gsub('%', '', x$Win.Percent)
  x$Games.Played <- as.numeric(as.character(x$Games.Played))
  x$Win.Percent <- as.numeric(as.character(x$Win.Percent)) * .01
  x$hero <- as.factor(x$hero)
  x$avg.winrate <- gsub('%', '', as.character(x$avg.winrate))
  x$avg.winrate <- as.numeric(as.character(x$avg.winrate)) * .01
  
  ann_text <- data.frame(Win.Percent = .65, hero = 'Thrall', Role = factor('Assassin', levels = c('Assassin', 'Specialist', 'Support', 'Warrior')), y = x$avg.winrate[x$hero == 'Thrall'], z = x$Win.Percent[x$hero == "Thrall"], games = x$Games.Played[x$hero == "Thrall"])
  
  ann_line <- data.frame(hero = 'Thrall', x = 'Thrall', xend = 'Thrall', Role = factor('Assassin', levels = c('Assassin', 'Specialist', 'Support', 'Warrior')), y = x$Win.Percent[x$hero == 'Thrall'] * 1.05, yend = .625, Win.Percent = x$Win.Percent[x$hero=='Thrall'])

  # Order data
  x <- x %>%
    arrange(Win.Percent)
  x$hero <- factor(x$hero, levels = x$hero)
  
  # Plot
  plot <- ggplot(data = x, aes(x = hero, y = Win.Percent, fill = Role, group = Role)) +
    geom_hline(yintercept = .5, alpha = .75, lty = 2) +
    geom_bar(aes(alpha = Games.Played), stat = 'identity', width = 0.5, color = 'grey75') +
    geom_point(aes(x = hero, y = avg.winrate, shape = Map.Name), alpha = 0.25, size = 1) +
    coord_flip() +
    theme(legend.position = 'none') +
    scale_y_continuous(breaks = c(seq(.3,.7,.1)),
                       expand = c(0,0),
                       limits = c(0.3,.7),
                       labels = scales::percent, 
                       sec.axis = dup_axis(),
                       trans="translate3") +
    facet_grid(Role ~ ., scales = "free", space = "free") +
    theme_fivethirtyeight() +
    scale_fill_discrete(guide = FALSE) +
    scale_shape_discrete('Overall Hero Win Rate', labels = c('')) +
    scale_alpha_continuous('Total Games Played', 
                           labels = c('0  ', '500  ', '1,000  ', '1,500  ', '2,000  ','2,500  ', '3,000  '),
                           breaks = c(0,500,1000,1500,2000,2500,3000),
                           #breaks = c(0,500,1000),
                           limits = c(0,3000) ) +
    labs(title = paste0('Hero league win rate on ', x$Map.Name),
         subtitle = paste0('Hero league win rate across Platinum, Diamond, and Master leagues for the last 7 days.\nLast update: ', 
                           Sys.time(), 
                           ' CST.'),
         caption = '@MattDaviz                                                                                                                   Source: HOTS LOGS') +
    theme(axis.title = element_text(face = 'bold')) +
    xlab('') +
    ylab('Win Rate') +
    geom_segment(data = ann_line, aes(x = ann_line$x, xend = ann_line$xend, y = ann_line$y, yend = ann_line$yend)) +
    geom_label(data = ann_text, label = paste0("Thrall's win rate\nin ", scales::comma(ann_text$games), 
                                               " games on\n", x$Map.Name[1], "\n is ",
                                               scales::percent(ann_text$z), ", which is\n", 
                                               scales::percent(round(ann_text$z - ann_text$y,3)), 
                                               if(ann_text$z - ann_text$y > 0) {
                                                 " better"
                                               } else {
                                                 " worse"
                                               },
                                               " than his\noverall win rate"), size = 2.5, 
               color = 'black', fill = "#F0F0F0") +
    guides(shape = guide_legend(override.aes= list(color = 'black'))) +
    guides(alpha = guide_legend(byrow = TRUE, nrow = 1, override.aes = list(fill = '#C77CFF'))) +
    theme(legend.position = 'bottom',
          legend.box = 'vertical',
          legend.key = element_rect(colour = 'grey75', size = .5, linetype = 'solid'),
          axis.title = element_text(face = 'bold'),
          legend.spacing = unit(.05, 'line'))
  
  print(plot)
  dev.off()
})

# Set up list of maps mapdata frames to loop over for plotting
mapsminus <- list(bhminus, bhbminus, boeminus, chminus, dsminus, gotminus, hmminus, isminus, stminus, todminus,
             tosqminus, wjminus)

# Set working directory to Dropbox folder
setwd('C://Users//mattd//Dropbox//HotS//Hero WR x Map.Name//geom_bar')

# Loop for alternative plot - Bronze, Silver, Gold
lapply(mapsminus, function(x) {
  
  # Create unique output filename
  output_filename <- paste0('Lower leagues WR on ', x$Map.Name,".jpeg")
  
  # Open the file for the plot to be written to
  jpeg(output_filename, height = 3150, width = 2400, res = 300, quality = 400)
  
  x <- left_join(x, overallminus[,c(1,5)], by = 'hero')
  
  x$Win.Percent <- gsub('%', '', x$Win.Percent)
  x$Games.Played <- as.numeric(as.character(x$Games.Played))
  x$Win.Percent <- as.numeric(as.character(x$Win.Percent)) * .01
  x$hero <- as.factor(x$hero)
  x$avg.winrate <- gsub('%', '', as.character(x$avg.winrate))
  x$avg.winrate <- as.numeric(as.character(x$avg.winrate)) * .01
  
  ann_text <- data.frame(Win.Percent = .65, hero = 'Varian', Role = factor('Assassin', levels = c('Assassin', 'Specialist', 'Support', 'Warrior')), y = x$avg.winrate[x$hero == 'Varian'], z = x$Win.Percent[x$hero == "Varian"], games = x$Games.Played[x$hero == "Varian"])
  
  ann_line <- data.frame(hero = 'Varian', x = 'Varian', xend = 'Varian', Role = factor('Assassin', levels = c('Assassin', 'Specialist', 'Support', 'Warrior')), y = x$Win.Percent[x$hero == 'Varian'] * 1.05, yend = .625, Win.Percent = x$Win.Percent[x$hero=='Varian'])
  
  # Order data
  x <- x %>%
    arrange(Win.Percent)
  x$hero <- factor(x$hero, levels = x$hero)
  
  # Plot
  plot <- ggplot(data = x, aes(x = hero, y = Win.Percent, fill = Role, group = Role)) +
    geom_hline(yintercept = .5, alpha = .75, lty = 2) +
    geom_bar(aes(alpha = Games.Played), stat = 'identity', width = 0.5, color = 'grey75') +
    geom_point(aes(x = hero, y = avg.winrate, shape = Map.Name), alpha = 0.25, size = 1) +
    coord_flip() +
    theme(legend.position = 'none') +
    scale_y_continuous(breaks = c(seq(.3,.7,.1)),
                       expand = c(0,0),
                       limits = c(0.3,.7),
                       labels = scales::percent, 
                       sec.axis = dup_axis(),
                       trans="translate3") +
    facet_grid(Role ~ ., scales = "free", space = "free") +
    theme_fivethirtyeight() +
    scale_fill_discrete(guide = FALSE) +
    scale_shape_discrete('Overall Hero Win Rate', labels = c('')) +
    scale_alpha_continuous('Total Games Played', 
                           labels = c('0  ', '500  ', '1,000  ', '1,500  ', '2,000  ','2,500  ', '3,000  '),
                           breaks = c(0,500,1000,1500,2000,2500,3000),
                           #breaks = c(0,500,1000),
                           limits = c(0,3000) ) +
    labs(title = paste0('Hero league win rate on ', x$Map.Name),
         subtitle = paste0('Hero league win rate across Bronze, Silver, and Gold leagues for the last 7 days.\nLast update: ', 
                           Sys.time(), 
                           ' CST.'),
         caption = '@MattDaviz                                                                                                                   Source: HOTS LOGS') +
    theme(axis.title = element_text(face = 'bold')) +
    xlab('') +
    ylab('Win Rate') +
    geom_segment(data = ann_line, aes(x = ann_line$x, xend = ann_line$xend, y = ann_line$y, yend = ann_line$yend)) +
    geom_label(data = ann_text, label = paste0("Varian's win rate\nin ", scales::comma(ann_text$games), 
                                               " games on\n", x$Map.Name[1], "\n is ",
                                               scales::percent(ann_text$z), ", which is\n", 
                                               scales::percent(round(ann_text$z - ann_text$y,3)), 
                                               if(ann_text$z - ann_text$y > 0) {
                                                 " better"
                                               } else {
                                                 " worse"
                                               },
                                               " than his\noverall win rate"), size = 2.5, 
               color = 'black', fill = "#F0F0F0") +
    guides(shape = guide_legend(override.aes= list(color = 'black'))) +
    guides(alpha = guide_legend(byrow = TRUE, nrow = 1, override.aes = list(fill = '#C77CFF'))) +
    theme(legend.position = 'bottom',
          legend.box = 'vertical',
          legend.key = element_rect(colour = 'grey75', size = .5, linetype = 'solid'),
          axis.title = element_text(face = 'bold'),
          legend.spacing = unit(.05, 'line'))
  
  print(plot)
  dev.off()
})

# Scraper for win rate by opposing hero
for(i in 1:length(names)) {
  tables[[i]] <- readHTMLTable(getURL(paste0(utmp, names[i])), stringsAsFactors = FALSE)
  tablesnew[[i]] <- tables[[i]][[4]][,c(2:6)]
  tablesnew[[i]]$hero <- names[[i]]
  # Use a Sys.sleep() command to be nice to a host's domain
  #Sys.sleep(3)
}

m <- readHTMLTable(getURL('https://www.hotslogs.com/sitewide/HeroDetails?Hero=Murky'))
m <- as.data.frame(m[3])
m <- m[,c(2:6)]
m$hero <- 'Murky'

#other <- rbind(m,g,z,s)

# Convert nested data frames into single data frame
wpa <- ldply(tablesnew, data.frame)
#wpa <- wpa[,c(1:6)]
#wpa <- wpa[wpa$hero != 'Sonya',]

# Change column names
colnames(wpa) <- c('opposing.hero', 'games.played.against', 'win.percent.against', 'role', 'subrole', 'hero')
colnames(m) <- c('opposing.hero', 'games.played.against', 'win.percent.against', 'role', 'subrole', 'hero')

wpa <- rbind(wpa, m)

# Clean up scraped data
wpa$hero <- gsub('%20', ' ', wpa$hero)
wpa$hero <- gsub('%27', "'", wpa$hero)
wpa$games.played.against <- gsub(',', '', wpa$games.played.against)
wpa$win.percent.against <- gsub('%', '', wpa$win.percent.against)
wpa$opposing.hero <- as.factor(wpa$opposing.hero)
wpa$games.played.against <- as.numeric(wpa$games.played.against)
wpa$win.percent.against <- as.numeric(wpa$win.percent.against) * .01
wpa$hero <- as.factor(wpa$hero)

wpa.subset <- wpa %>%
  group_by(hero, subrole) %>%
  summarize(sum(games.played.against * win.percent.against)/sum(games.played.against))
wpa.subset <- as.data.frame(wpa.subset)

colnames(wpa.subset) <- c('hero', 'subrole', 'win.percent.against')

wpa.subset$subrole <- factor(wpa.subset$subrole, levels = c('Tank', 'Bruiser', 'Healer', 'Support', 'Ambusher', 'Burst Damage', 'Sustained Damage', 'Siege', 'Utility'))

# Scraper for win rate by teammate hero
for(i in 1:length(names)) {
  tables[[i]] <- readHTMLTable(getURL(paste0(utmp, names[i])), stringsAsFactors = FALSE)
  tablesnew[[i]] <- tables[[i]][[5]][,c(2:6)]
  tablesnew[[i]]$hero <- names[[i]]
  # Use a Sys.sleep() command to be nice to a host's domain
  #Sys.sleep(3)
}

m <- readHTMLTable(getURL('https://www.hotslogs.com/sitewide/HeroDetails?Hero=Murky'))
m <- as.data.frame(m[4])
m <- m[,c(2:6)]
m$hero <- 'Murky'

# Convert nested data frames into single data frame
wpw <- ldply(tablesnew, data.frame)
#wpw <- wpw[,c(1:6)]
#wpw <- wpw[wpw$hero != 'Sonya',]

# Change column names
colnames(wpw) <- c('opposing.hero', 'games.played.against', 'win.percent.against', 'role', 'subrole', 'hero')
colnames(m) <- c('opposing.hero', 'games.played.against', 'win.percent.against', 'role', 'subrole', 'hero')

wpw <- rbind(wpw, m)

# Change column names
colnames(wpw) <- c('opposing.hero', 'games.played.with', 'win.percent.with', 'role', 'subrole', 'hero')

# Clean up scraped data
wpw$hero <- gsub('%20', ' ', wpw$hero)
wpw$hero <- gsub('%27', "'", wpw$hero)
wpw$games.played.with <- gsub(',', '', wpw$games.played.with)
wpw$win.percent.with <- gsub('%', '', wpw$win.percent.with)
wpw$opposing.hero <- as.factor(wpw$opposing.hero)
wpw$games.played.with <- as.numeric(wpw$games.played.with)
wpw$win.percent.with <- as.numeric(wpw$win.percent.with) * .01
wpw$hero <- as.factor(wpw$hero)

wpw.subset <- wpw %>%
  group_by(hero, subrole) %>%
  summarize(sum(games.played.with * win.percent.with)/sum(games.played.with))
wpw.subset <- as.data.frame(wpw.subset)

colnames(wpw.subset) <- c('hero', 'subrole', 'win.percent.with')

# Combine both datasets together and re-order
win.percent <- left_join(wpw.subset, wpa.subset, by = c('hero', 'subrole'))
win.percent <- melt(win.percent, by = c('hero','subrole'))
win.percent$subrole <- gsub('Sustained Damage', 'Sustained\nDamage', win.percent$subrole)
win.percent$subrole <- gsub('Burst Damage', 'Burst\nDamage', win.percent$subrole)
win.percent$subrole <- factor(win.percent$subrole, levels = c('Tank', 'Bruiser', 'Healer', 'Support', 'Ambusher', 'Burst\nDamage', 'Sustained\nDamage', 'Siege', 'Utility'))

RadarTheme<-theme(panel.background=element_blank(),
                  axis.text.x = element_blank(),
                  panel.spacing = unit(2, 'lines'),
                  axis.ticks.y = element_blank(),
                  axis.text.y = element_blank(),
                  panel.grid.major=element_line(size=0.3,linetype = 2,colour="grey65"),
                  strip.text = element_text(size = 12),
                  legend.text = element_blank(),
                  legend.key = element_rect(color = 'black', linetype = 'solid'))

labels <- c(win.percent.with = 'Win Rate WITH Hero Role', 
            win.percent.against = 'Win Rate AGAINST Hero Role')

hline <- data.frame(y = 0.5)

# Set working directory to Dropbox folder
setwd('C://Users//mattd//Dropbox//HotS//Hero WR x Hero Type')

# Loop for plotting graphs
lapply(loopnames, function(x) {
  
  # Create unique output filename
  output_filename <- paste0(x, ' WR by hero type.jpeg')
  
  # Open the file for the plot to be written to
  jpeg(output_filename, height = 2025, width = 3075, res = 300, quality = 400)
  
  # Plot
  plotdata <- filter(win.percent, hero == x)
  
  plot <- ggplot(data = plotdata, aes(x = subrole, y = value, fill = factor(subrole))) +
    geom_bar(width = 1, stat = 'identity', colour = 'black', show.legend = FALSE) +
    geom_hline(data = hline, aes(yintercept = y), color = 'black', lty = 2, size = 0.5, show.legend = TRUE) +
    geom_label(aes(x = subrole, y = max(value) + max(value/2.8), label = factor(subrole)),fill = '#F0F0F0', 
               color = NA, show.legend = FALSE) +
    geom_text(aes(x = subrole, y = max(value) + max(value)/2.8, label = factor(subrole)),  show.legend = FALSE) +
    scale_y_continuous(limits = c(0,.9), breaks = c(seq(0,.9,.1))) +
    scale_fill_discrete('50% Win Rate', breaks = c('Tank'), labels = NULL) +
    facet_grid(. ~ variable, labeller = labeller(variable = labels)) +
    labs(title = paste0(x, ' win rate with and against different hero roles'),
         subtitle = paste0('Hero league win rate across all leagues for the last 7 days. Last updated: ', Sys.time(), ' CST.'),
         caption = '@MattDaviz                                                                                                                                                                                 Source: HOTS LOGS') +
    theme(axis.ticks.y = element_blank())
  
  plot <- plot + coord_polar() + theme_fivethirtyeight() + RadarTheme 
  
  print(plot)
  dev.off()
})


# Clean up and plot selenium data
# Set up list of maps mapdata frames to loop over for plotting
maps2 <- list(bh, bhb, boe, ch, ds, got, hm, is, st, tod, tosq, wj)
maps <- list(boe, got, bhb, bh, whj, tod, hm, ds, st, is, ch, tosq)

new_obj <- lapply(maps, function(x) {
  
  colnames(x) <- c('hero', 'Games.Played', 'Games.Banned', 'Popularity', 'Win.Percent', 'Delta', 'Role', 'Specialty',
                   'Map.Name')
  
  # Clean up scraped mapdata
  #x$Games.Played <- gsub(',', '', as.character(x$Games.Played))
  x$Win.Percent <- gsub('%', '', as.character(x$Win.Percent))
  x$Map.Name <- as.factor(x$Map.Name)
  #x$Games.Played <- as.numeric(as.character(x$Games.Played))
  x$Win.Percent <- as.numeric(as.character(x$Win.Percent)) * .01
  x$hero <- as.factor(x$hero)
  
  return(x)
  
})
str(new_obj)
str(bhb)
new_obj <-  lapply(maps, function(x) {
  colnames(x) <- c('hero', 'Games.Played', 'Games.Banned', 'Popularity', 'Win.Percent', 'Delta', 'Role', 'Specialty',
                   'Map.Name')
  x <- return(x)
})

# Set working directory to Dropbox folder
setwd('C://Users//mattd//Dropbox//HotS//Hero WR x Map.Name//geom_bar')

lapply(maps, function(x) {
  
  x <- left_join(x, overallplus[,c(1,5)], by = 'hero')

  # Clean up scraped mapdata
  x$Games.Played <- gsub(',', '', as.character(x$Games.Played))
  x$Map.Name <- as.factor(x$Map.Name)
  x$Win.Percent <- gsub('%', '', x$Win.Percent)
  x$Games.Played <- as.numeric(as.character(x$Games.Played))
  x$Win.Percent <- as.numeric(as.character(x$Win.Percent)) * .01
  x$hero <- as.factor(x$hero)
  x$avg.winrate <- gsub('%', '', as.character(x$avg.winrate))
  x$avg.winrate <- as.numeric(as.character(x$avg.winrate)) * .01
  


  ann_text <- data.frame(Win.Percent = .65, hero = 'Raynor', Role = factor('Assassin', levels = c('Assassin', 'Specialist', 'Support', 'Warrior')), y = x$avg.winrate[x$hero == 'Raynor'], z = x$Win.Percent[x$hero == "Raynor"], games = x$Games.Played[x$hero == "Raynor"])
  
  ann_line <- data.frame(hero = 'Raynor', x = 'Raynor', xend = 'Raynor', Role = factor('Assassin', levels = c('Assassin', 'Specialist', 'Support', 'Warrior')), y = x$Win.Percent[x$hero == 'Raynor'] * 1.08, yend = .625, Win.Percent = x$Win.Percent[x$hero=='Raynor'])
  
  # Order data
  x <- x %>%
    arrange(Win.Percent)
  x$hero <- factor(x$hero, levels = x$hero)

  # Plot
  plot <- ggplot(data = x, aes(x = hero, y = Win.Percent, fill = Role, group = Role)) +
    geom_bar(aes(alpha = Games.Played), stat = 'identity', width = 0.5, color = 'grey75') +
    geom_point(aes(x = hero, y = avg.winrate, shape = Map.Name), alpha = 0.25, size = 1) +
    coord_flip() +
    theme(legend.position = 'none') +
    scale_y_continuous(limits = c(0,.7), 
                       breaks = c(seq(0,.6,.1)),
                       labels = scales::percent, 
                       sec.axis = dup_axis()) +
    geom_hline(yintercept = .5, alpha = .75, lty = 2) +
    facet_grid(Role ~ ., scales = "free", space = "free") +
    theme_fivethirtyeight() +
    scale_fill_discrete(guide = FALSE) +
    scale_shape_discrete('Overall Hero Win Rate', labels = c('')) +
    scale_alpha_continuous('Total Games Played', labels = c('0', '1,000', '2,000', '3,000', '4,000'),
                           breaks = c(0,1000,2000,3000,4000), 
                           limits = c(0,4000)) +
    labs(title = paste0('Hero league win rate on ', x$Map.Name),
         subtitle = paste0('Hero league win rate across all leagues for the last 7 days.\nLast update: ', 
                           Sys.time(), 
                           ' CST.'),
         caption = '@MattDaviz                                                                                                                   Source: HOTS LOGS') +
    theme(axis.title = element_text(face = 'bold')) +
    xlab('') +
    ylab('Win Rate') +
    geom_segment(data = ann_line, aes(x = ann_line$x, xend = ann_line$xend, y = ann_line$y, yend = ann_line$yend)) +
    geom_label(data = ann_text, label = paste0("Varian's win rate\nin ", scales::comma(ann_text$games), 
                                               " games on\n", x$Map.Name[1], "\n is ",
                                               scales::percent(ann_text$z), ", which is\n", 
                                               scales::percent(round(ann_text$z - ann_text$y,3)), 
                                               if(ann_text$z - ann_text$y > 0) {
                                                 " better"
                                               } else {
                                                 " worse"
                                               },
                                               " than his\noverall win rate"), size = 2.5, 
               color = 'black', fill = "#F0F0F0") +
    guides(shape = guide_legend(override.aes= list(color = 'black'))) +
    guides(alpha = guide_legend(override.aes = list(fill = '#C77CFF'))) +
    theme(legend.position = 'bottom',
          legend.box = 'vertical',
          legend.key = element_rect(colour = 'grey75', size = .5, linetype = 'solid'),
          axis.title = element_text(face = 'bold'),
          legend.spacing = unit(.05, 'line'))
  
  print(plot)
  dev.off()
})

# Plot for difference between avg.winrate and Win.Percent sorted
# Set list of data frames
mapsplus <- list(bh, bhb, boe, ch, ds, got, hm, is, st, tod, tosq, wj)

# Set working directory to Dropbox folder
setwd('C://Users//mattd//Dropbox//HotS//Hero WR x Map.Name//requested plots')

# Loop for alternative plot - Platinum, Diamond, Master
lapply(mapsplus, function(x) {
  
  # Create unique output filename
  output_filename <- paste0('Higher leagues WR on ', x$Map.Name,".jpeg")
  
  # Open the file for the plot to be written to
  jpeg(output_filename, height = 3150, width = 2400, res = 300, quality = 400)
  
  x <- left_join(x, overallplus[,c(1,5)], by = 'hero')
  
  x$Win.Percent <- gsub('%', '', x$Win.Percent)
  x$Games.Played <- as.numeric(as.character(x$Games.Played))
  x$Win.Percent <- as.numeric(as.character(x$Win.Percent)) * .01
  x$hero <- as.factor(x$hero)
  x$avg.winrate <- gsub('%', '', as.character(x$avg.winrate))
  x$avg.winrate <- as.numeric(as.character(x$avg.winrate)) * .01
  x$color <- ifelse((x$Win.Percent - x$avg.winrate > 0), 'darkred', 'darkgreen')
  
  ann_text <- data.frame(Win.Percent = .65, hero = 'Thrall', Role = factor('Assassin', levels = c('Assassin', 'Specialist', 'Support', 'Warrior')), y = x$avg.winrate[x$hero == 'Thrall'], z = x$Win.Percent[x$hero == "Thrall"], games = x$Games.Played[x$hero == "Thrall"])
  
  ann_line <- data.frame(hero = 'Thrall', x = 'Thrall', xend = 'Thrall', Role = factor('Assassin', levels = c('Assassin', 'Specialist', 'Support', 'Warrior')), y = x$Win.Percent[x$hero == 'Thrall'] * 1.05, yend = .625, Win.Percent = x$Win.Percent[x$hero=='Thrall'])
  
  # Order data
  x <- x %>%
    arrange(Win.Percent)
  x$hero <- factor(x$hero, levels = x$hero)
  
  # Plot
  plot <- ggplot(data = x, aes(x = hero, y = Win.Percent, group = Role)) +
    #geom_bar(aes(alpha = Games.Played), stat = 'identity', width = 0.5, color = 'grey75') +
    geom_point(aes(alpha = Games.Played)) +
    geom_segment(aes(x = hero, xend = hero, y = Win.Percent, yend = avg.winrate, color = color))  +
    coord_flip() +
    theme(legend.position = 'none') +
    scale_y_continuous(limits = c(0.25,.75), 
                       breaks = c(seq(0.3,.7,.1)),
                       labels = scales::percent, 
                       sec.axis = dup_axis()) +
    geom_hline(yintercept = .5, alpha = .75, lty = 2) +
    facet_grid(Role ~ ., scales = "free", space = "free") +
    theme_fivethirtyeight() +
    scale_fill_discrete(guide = FALSE) +
    #scale_shape_discrete('Overall Hero Win Rate', labels = c('')) +
    scale_alpha_continuous('Total Games Played', labels = c('0  ', '500  ', '1,000  ', '1,500  ', '2,000  ',
                                                            '2,500  ', '3,000  '),
                           breaks = c(0,500,1000,1500,2000,2500,3000), 
                           limits = c(0,3000)) +
    labs(title = paste0('Hero league win rate on ', x$Map.Name),
         subtitle = paste0('Hero league win rate across Platinum, Diamond, and Master leagues for the last 7 days.\nLast update: ', 
                           Sys.time(), 
                           ' CST.'),
         caption = '@MattDaviz                                                                                                                   Source: HOTS LOGS') +
    theme(axis.title = element_text(face = 'bold')) +
    xlab('') +
    ylab('Win Rate') +
    geom_segment(data = ann_line, aes(x = ann_line$x, xend = ann_line$xend, y = ann_line$y, yend = ann_line$yend)) +
    geom_label(data = ann_text, label = paste0("Thrall's win rate\nin ", scales::comma(ann_text$games), 
                                               " games on\n", x$Map.Name[1], "\n is ",
                                               scales::percent(ann_text$z), ", which is\n", 
                                               scales::percent(round(ann_text$z - ann_text$y,3)), 
                                               if(ann_text$z - ann_text$y > 0) {
                                                 " better"
                                               } else {
                                                 " worse"
                                               },
                                               " than his\noverall win rate"), size = 2.5, 
               color = 'black', fill = "#F0F0F0") +
    guides(point = guide_legend(override.aes= list(color = 'black'))) +
    guides(alpha = guide_legend(byrow = TRUE, nrow = 1, override.aes = list(color = 'black'))) +
    scale_color_discrete('Map-Specific Win Rate', labels = c('Worse than overall win rate', 'Better than overall win rate')) +
    theme(legend.position = 'bottom',
          legend.box = 'vertical',
          legend.key = element_rect(colour = 'grey75', size = .5, linetype = 'solid'),
          axis.title = element_text(face = 'bold'),
          legend.spacing = unit(.05, 'line'))
  
  print(plot)
  dev.off()
})