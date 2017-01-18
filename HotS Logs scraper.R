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
           "Sonya", "Abathur", "Stitches", "Medivh", "Zagara", "Greymane")

m <- readHTMLTable(getURL('https://www.hotslogs.com/sitewide/HeroDetails?Hero=Murky'))
m <- as.data.frame(m[5])
m <- m[,c(2:4)]
m$hero <- 'Murky'

loopnames <- gsub('%20', ' ', names)
loopnames <- gsub('%27', "'", loopnames)
loopnames <- c(loopnames,)

# Set up empty vectors to deposit scraped data frames into
tables <- vector('list', length(names))
tablesnew <- vector('list', length(names))

# Scraper for win rate by battlefield
for(i in 1:length(names)) {
  tables[[i]] <- readHTMLTable(getURL(paste0(utmp, names[i])), stringsAsFactors = FALSE)
  tablesnew[[i]] <- tables[[i]][[6]][,c(2:4)]
  tablesnew[[i]]$hero <- names[[i]]
  # Use a Sys.sleep() command to be nice to a host's domain
  Sys.sleep(3)
}

# Convert nested data frames into single data frame
mapdata <- ldply(tablesnew, data.frame)
mapdata <- rbind(mapdata, m)

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
hotscolors <- c('red2', 'darkorchid2', 'deepskyblue2', 'blue3')

# Subset mapdata based on map
boe <- mapdata[mapdata$Map.Name == 'Battlefield of Eternity',]
got <- mapdata[mapdata$Map.Name == 'Garden of Terror',]
bhb <- mapdata[mapdata$Map.Name == "Blackheart's Bay",]
bh <- mapdata[mapdata$Map.Name == 'Braxis Holdout',]
whj <- mapdata[mapdata$Map.Name == 'Warhead Junction',]
tod <- mapdata[mapdata$Map.Name == 'Towers of Doom',]
hm <- mapdata[mapdata$Map.Name == 'Haunted Mines',]
ds <- mapdata[mapdata$Map.Name == 'Dragon Shire',]
st <- mapdata[mapdata$Map.Name == 'Sky Temple',]
is <- mapdata[mapdata$Map.Name == 'Infernal Shrines',]
ch <- mapdata[mapdata$Map.Name == 'Cursed Hollow',]
tosq <- mapdata[mapdata$Map.Name == 'Tomb of the Spider Queen',]

# Set up list of maps mapdata frames to loop over for plotting
maps <- list(boe, got, bhb, bh, whj, tod, hm, ds, st, is, ch, tosq)

# Set working directory to Dropbox folder
setwd('C://Users//mattd//Dropbox//HotS//Hero WR x Map.Name')

# Loop for plotting graphs
lapply(maps, function(x) {
  # Create unique output filename
  output_filename <- paste0('WR on ', x$Map.Name,".jpeg")
  
  # Open the file for the plot to be written to
  jpeg(output_filename, height = 2400, width = 3150, res = 300, quality = 400)
  
  # Plot
  plot <- ggplot(x, aes(x = Games.Played, y = Win.Percent, label = hero, color = Role)) +
    geom_hline(yintercept = .5, alpha = 0.75, lty = 2, size = 1) +
    geom_text_repel() +
    theme_fivethirtyeight() +
    labs(title = paste0('Hero league win rate on ', x$Map.Name),
         subtitle = paste0('Hero league win rate across all leagues for the last 7 days. Last updated: ', Sys.time(), ' CST.'),
         caption = '@MattDaviz                                                                                                                                                                                    Source: HOTS LOGS') +
    scale_y_continuous(limits = c(.25,.65),labels = scales::percent) +
    scale_x_continuous(labels = scales::comma) +
    #scale_color_manual(values = hotscolors) +
    theme(axis.title = element_text(face = 'bold')) +
    ylab('Win Rate') +
    xlab('Number of Games Played')
  
  print(plot)
  dev.off()
})

# Set working directory to Dropbox folder
setwd('C://Users//mattd//Dropbox//HotS//Hero WR x Map.Name//geom_bar')

# Loop for alternative plot
lapply(maps, function(x) {
  # Create unique output filename
  output_filename <- paste0('WR on ', x$Map.Name,".jpeg")
  
  # Open the file for the plot to be written to
  jpeg(output_filename, height = 3150, width = 2400, res = 300, quality = 400)
  
  # Order data
  x <- x %>%
    arrange(Win.Percent)
  x$hero <- factor(x$hero, levels = x$hero)
  
  # Plot
  plot <- ggplot(data = x, aes(x = hero, y = Win.Percent, fill = Role, group = Role)) +
    geom_bar(aes(alpha = Games.Played), stat = 'identity', width = 0.5, color = 'grey75') +
    coord_flip() +
    theme(legend.position = 'none') +
    scale_y_continuous(limits = c(0,.65), 
                       breaks = c(seq(0,.6,.1)),
                       labels = scales::percent, 
                       sec.axis = dup_axis()) +
    geom_hline(yintercept = .5, alpha = .75, lty = 2) +
    facet_grid(Role ~ ., scales = "free_y", space = "free_y") +
    theme_fivethirtyeight() +
    scale_fill_discrete(guide = FALSE) +
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
    ylab('Win Rate')
  
  print(plot)
  dev.off()
})

# Scraper for win rate by opposing hero
for(i in 1:length(names)) {
  tables[[i]] <- readHTMLTable(getURL(paste0(utmp, names[i])), stringsAsFactors = FALSE)
  tablesnew[[i]] <- tables[[i]][[4]][,c(2:6)]
  tablesnew[[i]]$hero <- names[[i]]
  # Use a Sys.sleep() command to be nice to a host's domain
  Sys.sleep(3)
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
  Sys.sleep(3)
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
