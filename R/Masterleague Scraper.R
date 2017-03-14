# Load Libraries
library(plyr)
library(dplyr)
library(httr)
library(RCurl)
library(tidyr)
library(ggplot2)
library(ggthemes)

# Scrape Heroes Data
heroes <- 'https://api.masterleague.net/heroes?page_size=100'
heroes.data <- GET(heroes)
heroes.data <- content(heroes.data)
heroes.df <- data.frame(t(sapply(heroes.data[[4]],c)))
heroes.df <- heroes.df %>% 
  select(id, name, role)
rm(heroes.df)

# Scrape Maps Data
maps <- 'https://api.masterleague.net/maps.json'
maps.data <- GET(maps)
maps.data <- content(maps.data)
maps.df <- cbind(maps.data, do.call(rbind, lapply(seq(nrow(maps.data)), function(i) unlist(maps.data[i]))))
maps.df <- cbind(maps.df, do.call(rbind, lapply(seq(nrow(maps.df)), function(i) unlist(maps.df[i]))))
maps.df <- as.data.frame(maps.df)
maps.df <- maps.df %>% 
  select(id, name)
rm(maps.df)

# Scrape Regions Data
regions <- 'https://api.masterleague.net/regions.json'
regions.data <- GET(regions)
regions.data <- content(regions.data)
regions.df <- cbind(regions.data, do.call(rbind, lapply(seq(nrow(regions.data)), function(i) unlist(regions.data[i]))))
regions.df <- cbind(regions.df, do.call(rbind, lapply(seq(nrow(regions.df)), function(i) unlist(regions.df[i]))))
regions.df <- as.data.frame(regions.df)
regions.df <- regions.df %>% 
  select(id, name)

# Scrape Patches Data
patches <- 'https://api.masterleague.net/patches.json'
patches.data <- GET(patches)
patches.data <- content(patches.data)
patches.df <- cbind(patches.data, do.call(rbind, lapply(seq(nrow(patches.data)), function(i) unlist(patches.data[i]))))
patches.df <- cbind(patches.df, do.call(rbind, lapply(seq(nrow(patches.df)), function(i) unlist(patches.df[i]))))
patches.df <- as.data.frame(patches.df)
patches.df <- patches.df %>% 
  select(id, name)

# Scrape Teams Data
url <- 'https://api.masterleague.net/teams/?page='

    # create the list to hold the tables
tables <- vector("list", 8)
    # get the tables
for(i in 1:8) {
    # create the url for the day and if it exists, read it - if not, NULL
  tables[[i]] <- data.frame(t(sapply(content(GET(paste0(url,i))),c)))
}

    # unlist all data into one data frame
teams.df <- do.call(rbind, tables)
teams.df <- cbind(tables, do.call(rbind, lapply(seq(nrow(test)), function(i) unlist(teams.df$results[i]))))
teams.df <- as.data.frame(teams.df)
teams.df <- teams.df[,2:141]

    # grab first column of data to reorganize
teams1 <- matrix(unlist(teams.df[1,]), nrow=7, ncol = 20)
rownames(teams1) <- c('id', 'name', 'region', 'url', 'logo.small', 'logo.big', 'logo.medium')
teams1 <- t(teams1)
teams1 <- as.data.frame(teams1[,1:3])

    # grab second column of data to reorganize
teams2 <- matrix(unlist(teams.df[2,]), nrow=7, ncol = 20)
rownames(teams2) <- c('id', 'name', 'region', 'url', 'logo.small', 'logo.big', 'logo.medium')
teams2 <- t(teams2)
teams2 <- as.data.frame(teams2[,1:3])

    # grab third column of data to reorganize
teams3 <- matrix(unlist(teams.df[3,]), nrow=7, ncol = 20)
rownames(teams3) <- c('id', 'name', 'region', 'url', 'logo.small', 'logo.big', 'logo.medium')
teams3 <- t(teams3)
teams3 <- as.data.frame(teams3[,1:3])

    # grab fourth column of data to reorganize
teams4 <- matrix(unlist(teams.df[4,]), nrow=7, ncol = 20)
rownames(teams4) <- c('id', 'name', 'region', 'url', 'logo.small', 'logo.big', 'logo.medium')
teams4 <- t(teams4)
teams4 <- as.data.frame(teams4[,1:3])

    # grab fifth column of data to reorganize
teams5 <- matrix(unlist(teams.df[5,]), nrow=7, ncol = 20)
rownames(teams5) <- c('id', 'name', 'region', 'url', 'logo.small', 'logo.big', 'logo.medium')
teams5 <- t(teams5)
teams5 <- as.data.frame(teams5[,1:3])

    # grab sixth column of data to reorganize
teams6 <- matrix(unlist(teams.df[6,]), nrow=7, ncol = 20)
rownames(teams6) <- c('id', 'name', 'region', 'url', 'logo.small', 'logo.big', 'logo.medium')
teams6 <- t(teams6)
teams6 <- as.data.frame(teams6[,1:3])

    # grab seventh column of data to reorganize
teams7 <- matrix(unlist(teams.df[7,]), nrow=7, ncol = 20)
rownames(teams7) <- c('id', 'name', 'region', 'url', 'logo.small', 'logo.big', 'logo.medium')
teams7 <- t(teams7)
teams7 <- as.data.frame(teams7[,1:3])

    # grab eighth column of data to reorganize
teams8 <- matrix(unlist(teams.df[8,]), nrow=7, ncol = 20)
rownames(teams8) <- c('id', 'name', 'region', 'url', 'logo.small', 'logo.big', 'logo.medium')
teams8 <- t(teams8)
teams8 <- as.data.frame(teams8[,1:3])

    # put all reorganized data frames together into one list of teams
teams.df <- rbind(teams1, teams2, teams3, teams4, teams5, teams6, teams7, teams8)

    # remove data frames that are no longer necessary
rm(teams1)
rm(teams2)
rm(teams3)
rm(teams4)
rm(teams5)
rm(teams6)
rm(teams7)
rm(teams8)

# Scrape Players Data
url <- 'https://api.masterleague.net/players/?page='

    # create the list to hold the tables
tables <- vector("list", 26)
    # get the tables
for(i in 1:26) {
    # create the url for the day and if it exists, read it - if not, NULL
  tables[[i]] <- data.frame(t(sapply(content(GET(paste0(url,i)))[[4]],c)))
}

    # unlist all data into one data frame
players <- do.call(rbind, tables)
players.df <- players %>% 
  select(id, team, region, nickname, realname, country, role)
rm(players)

# Scrape Tournaments Data
    # scrape the first page
url <- 'https://api.masterleague.net/tournaments/?page=1'
tournament.data <- GET(url)
tournament.data <- content(tournament.data)
tournament.df.one <- data.frame(t(sapply(content(GET(url))[[4]],c)))

    # scrape the second page
url <- 'https://api.masterleague.net/tournaments/?page=2&format=json'
tournament.data <- GET(url)
tournament.data <- content(tournament.data)
tournament.df.two <- data.frame(t(sapply(content(GET(url))[[4]],c)))

    # put the two data frames together
tournament.df <- rbind(tournament.df.one, tournament.df.two)
tournament.df <- tournament.df %>% 
  filter(name != 'Eastern Clash')

    # Needs to be cleaned
test.tournament.df <- cbind(tournament.df, do.call(rbind, lapply(seq(nrow(tournament.df)), function(i) unlist(tournament.df$stages[i]))))
test.tournament.gather <- bind_rows(test.tournament.df[,c(1:5,8:9)],
          test.tournament.df[,c(1:5,10:11)],
          test.tournament.df[,c(1:5,12:13)],
          test.tournament.df[,c(1:5,14:15)],
          test.tournament.df[,c(1:5,16:17)],
          test.tournament.df[,c(1:5,18:19)],
          test.tournament.df[,c(1:5,20:21)],
          test.tournament.df[,c(1:5,22:23)],
          test.tournament.df[,c(1:5,24:25)]
          )

# Scrape Matches Data
url <- 'https://api.masterleague.net/matches/?page='

    # create the list to hold the tables
tables <- vector("list", 227) #227 total number
    # get the tables
for(i in 1:227) {
    # create the url for the day and if it exists, read it - if not, NULL
  tables[[i]] <- data.frame(t(sapply(content(GET(paste0(url,i)))[[4]],c)))
  Sys.sleep(2)
}

    # Convert tables into Data Frame
matches.df <- do.call(rbind, tables)
matches.df <- cbind(matches.df, do.call(rbind, lapply(seq(nrow(matches.df)), function(i) unlist(matches.df$drafts[i]))))
matches.df <- matches.df[,c(1:9,12:39)]

    # Separate First Pick Drafts
matches.df.first.pick <- matches.df[,c(1:23)]
matches.df.first.pick$is_first_pick <- 1
matches.df.first.pick$is_map_pick <- 0
matches.df.first.pick <- matches.df.first.pick[,c(1:11,24:25, 12,14:19,13,20:23)]
colnames(matches.df.first.pick)[14:25] <- c('ban.1', 'pick.1', 'player.1', 'pick.4', 'player.4', 'pick.5', 'player.5', 'ban.4', 'pick.8', 'player.8', 'pick.9', 'player.9')

    # match all data with the text values for masterleague api numbers
matches.df.first.pick$map <- maps.df$name[match(matches.df.first.pick$map, maps.df$id)]
matches.df.first.pick$tournament <- tournament.df$name[match(matches.df.first.pick$tournament, tournament.df$id)]
matches.df.first.pick$patch <- patches.df$name[match(matches.df.first.pick$patch, patches.df$id)]
matches.df.first.pick$team <- teams.df$name[match(matches.df.first.pick$team, teams.df$id)]
matches.df.first.pick$ban.1 <- heroes.df$name[match(matches.df.first.pick$ban.1, heroes.df$id)]
matches.df.first.pick$pick.1 <- heroes.df$name[match(matches.df.first.pick$pick.1, heroes.df$id)]
matches.df.first.pick$pick.4 <- heroes.df$name[match(matches.df.first.pick$pick.4, heroes.df$id)]
matches.df.first.pick$pick.5 <- heroes.df$name[match(matches.df.first.pick$pick.5, heroes.df$id)]
matches.df.first.pick$ban.4 <- heroes.df$name[match(matches.df.first.pick$ban.4, heroes.df$id)]
matches.df.first.pick$pick.8 <- heroes.df$name[match(matches.df.first.pick$pick.8, heroes.df$id)]
matches.df.first.pick$pick.9 <- heroes.df$name[match(matches.df.first.pick$pick.9, heroes.df$id)]
matches.df.first.pick$player.1 <- players.df$nickname[match(matches.df.first.pick$player.1, players.df$id)]
matches.df.first.pick$player.4 <- players.df$nickname[match(matches.df.first.pick$player.4, players.df$id)]
matches.df.first.pick$player.5 <- players.df$nickname[match(matches.df.first.pick$player.5, players.df$id)]
matches.df.first.pick$player.8 <- players.df$nickname[match(matches.df.first.pick$player.8, players.df$id)]
matches.df.first.pick$player.9 <- players.df$nickname[match(matches.df.first.pick$player.9, players.df$id)]

    # unlist all data frame variables that are being stored as lists
matches.df.first.pick.test <- cbind(matches.df.first.pick, do.call(rbind, lapply(seq(nrow(matches.df.first.pick)), function(i) unlist(matches.df.first.pick$id[i]))))
matches.df.first.pick.test <- cbind(matches.df.first.pick.test, do.call(rbind, lapply(seq(nrow(matches.df.first.pick.test)), function(i) unlist(matches.df.first.pick.test$date[i]))))
matches.df.first.pick.test <- cbind(matches.df.first.pick.test, do.call(rbind, lapply(seq(nrow(matches.df.first.pick.test)), function(i) unlist(matches.df.first.pick.test$patch[i]))))
matches.df.first.pick.test <- cbind(matches.df.first.pick.test, do.call(rbind, lapply(seq(nrow(matches.df.first.pick.test)), function(i) unlist(matches.df.first.pick.test$tournament[i]))))
matches.df.first.pick.test <- cbind(matches.df.first.pick.test, do.call(rbind, lapply(seq(nrow(matches.df.first.pick.test)), function(i) unlist(matches.df.first.pick.test$stage[i]))))
matches.df.first.pick.test <- cbind(matches.df.first.pick.test, do.call(rbind, lapply(seq(nrow(matches.df.first.pick.test)), function(i) unlist(matches.df.first.pick.test$round[i]))))
matches.df.first.pick.test <- cbind(matches.df.first.pick.test, do.call(rbind, lapply(seq(nrow(matches.df.first.pick.test)), function(i) unlist(matches.df.first.pick.test$series[i]))))
matches.df.first.pick.test <- cbind(matches.df.first.pick.test, do.call(rbind, lapply(seq(nrow(matches.df.first.pick.test)), function(i) unlist(matches.df.first.pick.test$game[i]))))
matches.df.first.pick.test <- cbind(matches.df.first.pick.test, do.call(rbind, lapply(seq(nrow(matches.df.first.pick.test)), function(i) unlist(matches.df.first.pick.test$map[i]))))
matches.df.first.pick.test <- cbind(matches.df.first.pick.test, do.call(rbind, lapply(seq(nrow(matches.df.first.pick.test)), function(i) unlist(matches.df.first.pick.test$ban.1[i]))))
matches.df.first.pick.test <- cbind(matches.df.first.pick.test, do.call(rbind, lapply(seq(nrow(matches.df.first.pick.test)), function(i) unlist(matches.df.first.pick.test$pick.1[i]))))
matches.df.first.pick.test <- cbind(matches.df.first.pick.test, do.call(rbind, lapply(seq(nrow(matches.df.first.pick.test)), function(i) unlist(matches.df.first.pick.test$player.1[i]))))
matches.df.first.pick.test <- cbind(matches.df.first.pick.test, do.call(rbind, lapply(seq(nrow(matches.df.first.pick.test)), function(i) unlist(matches.df.first.pick.test$pick.4[i]))))
matches.df.first.pick.test <- cbind(matches.df.first.pick.test, do.call(rbind, lapply(seq(nrow(matches.df.first.pick.test)), function(i) unlist(matches.df.first.pick.test$player.4[i]))))
matches.df.first.pick.test <- cbind(matches.df.first.pick.test, do.call(rbind, lapply(seq(nrow(matches.df.first.pick.test)), function(i) unlist(matches.df.first.pick.test$pick.5[i]))))
matches.df.first.pick.test <- cbind(matches.df.first.pick.test, do.call(rbind, lapply(seq(nrow(matches.df.first.pick.test)), function(i) unlist(matches.df.first.pick.test$player.5[i]))))
matches.df.first.pick.test <- cbind(matches.df.first.pick.test, do.call(rbind, lapply(seq(nrow(matches.df.first.pick.test)), function(i) unlist(matches.df.first.pick.test$ban.4[i]))))
matches.df.first.pick.test <- cbind(matches.df.first.pick.test, do.call(rbind, lapply(seq(nrow(matches.df.first.pick.test)), function(i) unlist(matches.df.first.pick.test$pick.8[i]))))
matches.df.first.pick.test <- cbind(matches.df.first.pick.test, do.call(rbind, lapply(seq(nrow(matches.df.first.pick.test)), function(i) unlist(matches.df.first.pick.test$player.8[i]))))
matches.df.first.pick.test <- cbind(matches.df.first.pick.test, do.call(rbind, lapply(seq(nrow(matches.df.first.pick.test)), function(i) unlist(matches.df.first.pick.test$pick.9[i]))))
matches.df.first.pick.test <- cbind(matches.df.first.pick.test, do.call(rbind, lapply(seq(nrow(matches.df.first.pick.test)), function(i) unlist(matches.df.first.pick.test$player.9[i]))))

    # relabel data frame column names and remove unnecessary data frames
colnames(matches.df.first.pick.test)[26:46] <- c('id', 'date', 'patch', 'tournament', 'stage', 'round', 'series', 'game', 'map', 'ban.1', 'pick.1', 'player.1', 'pick.4', 'player.4', 'pick.5', 'player.5', 'ban.4', 'pick.8', 'player.8', 'pick.9', 'player.9')
matches.df.first.pick <- matches.df.first.pick.test[,c(26:34, 10:13, 35:46)]
rm(matches.df.first.pick.test)

    # Separate Map Pick Drafts
matches.df.map.pick <- matches.df[,c(1:9,24:37)]
matches.df.map.pick$is_first_pick <- 0
matches.df.map.pick$is_map_pick <- 1
matches.df.map.pick <- matches.df.map.pick[,c(1:11,24:25,12,14:17,13,18:23)]
colnames(matches.df.map.pick)[14:25] <- c('ban.2', 'pick.2', 'player.2', 'pick.3', 'player.3', 'ban.3', 'pick.6', 'player.6', 'pick.7', 'player.7', 'pick.10', 'player.10')
colnames(matches.df.map.pick)[10:11] <- c('team', 'is_winner')

    # convert all number values from masterleague into text
matches.df.map.pick$map <- maps.df$name[match(matches.df.map.pick$map, maps.df$id)]
matches.df.map.pick$tournament <- tournament.df$name[match(matches.df.map.pick$tournament, tournament.df$id)]
matches.df.map.pick$patch <- patches.df$name[match(matches.df.map.pick$patch, patches.df$id)]
matches.df.map.pick$team <- teams.df$name[match(matches.df.map.pick$team, teams.df$id)]
matches.df.map.pick$ban.2 <- heroes.df$name[match(matches.df.map.pick$ban.2, heroes.df$id)]
matches.df.map.pick$pick.2 <- heroes.df$name[match(matches.df.map.pick$pick.2, heroes.df$id)]
matches.df.map.pick$pick.3 <- heroes.df$name[match(matches.df.map.pick$pick.3, heroes.df$id)]
matches.df.map.pick$ban.3 <- heroes.df$name[match(matches.df.map.pick$ban.3, heroes.df$id)]
matches.df.map.pick$pick.6 <- heroes.df$name[match(matches.df.map.pick$pick.6, heroes.df$id)]
matches.df.map.pick$pick.7 <- heroes.df$name[match(matches.df.map.pick$pick.7, heroes.df$id)]
matches.df.map.pick$pick.10 <- heroes.df$name[match(matches.df.map.pick$pick.10, heroes.df$id)]
matches.df.map.pick$player.2 <- players.df$nickname[match(matches.df.map.pick$player.2, players.df$id)]
matches.df.map.pick$player.3 <- players.df$nickname[match(matches.df.map.pick$player.3, players.df$id)]
matches.df.map.pick$player.6 <- players.df$nickname[match(matches.df.map.pick$player.6, players.df$id)]
matches.df.map.pick$player.7 <- players.df$nickname[match(matches.df.map.pick$player.7, players.df$id)]
matches.df.map.pick$player.10 <- players.df$nickname[match(matches.df.map.pick$player.10, players.df$id)]

    # unlist all variables that are being stored as lists
matches.df.map.pick.test <- cbind(matches.df.map.pick, do.call(rbind, lapply(seq(nrow(matches.df.map.pick)), function(i) unlist(matches.df.map.pick$id[i]))))
matches.df.map.pick.test <- cbind(matches.df.map.pick.test, do.call(rbind, lapply(seq(nrow(matches.df.map.pick.test)), function(i) unlist(matches.df.map.pick.test$date[i]))))
matches.df.map.pick.test <- cbind(matches.df.map.pick.test, do.call(rbind, lapply(seq(nrow(matches.df.map.pick.test)), function(i) unlist(matches.df.map.pick.test$patch[i]))))
matches.df.map.pick.test <- cbind(matches.df.map.pick.test, do.call(rbind, lapply(seq(nrow(matches.df.map.pick.test)), function(i) unlist(matches.df.map.pick.test$tournament[i]))))
matches.df.map.pick.test <- cbind(matches.df.map.pick.test, do.call(rbind, lapply(seq(nrow(matches.df.map.pick.test)), function(i) unlist(matches.df.map.pick.test$stage[i]))))
matches.df.map.pick.test <- cbind(matches.df.map.pick.test, do.call(rbind, lapply(seq(nrow(matches.df.map.pick.test)), function(i) unlist(matches.df.map.pick.test$round[i]))))
matches.df.map.pick.test <- cbind(matches.df.map.pick.test, do.call(rbind, lapply(seq(nrow(matches.df.map.pick.test)), function(i) unlist(matches.df.map.pick.test$series[i]))))
matches.df.map.pick.test <- cbind(matches.df.map.pick.test, do.call(rbind, lapply(seq(nrow(matches.df.map.pick.test)), function(i) unlist(matches.df.map.pick.test$game[i]))))
matches.df.map.pick.test <- cbind(matches.df.map.pick.test, do.call(rbind, lapply(seq(nrow(matches.df.map.pick.test)), function(i) unlist(matches.df.map.pick.test$map[i]))))
matches.df.map.pick.test <- cbind(matches.df.map.pick.test, do.call(rbind, lapply(seq(nrow(matches.df.map.pick.test)), function(i) unlist(matches.df.map.pick.test$ban.2[i]))))
matches.df.map.pick.test <- cbind(matches.df.map.pick.test, do.call(rbind, lapply(seq(nrow(matches.df.map.pick.test)), function(i) unlist(matches.df.map.pick.test$pick.2[i]))))
matches.df.map.pick.test <- cbind(matches.df.map.pick.test, do.call(rbind, lapply(seq(nrow(matches.df.map.pick.test)), function(i) unlist(matches.df.map.pick.test$player.2[i]))))
matches.df.map.pick.test <- cbind(matches.df.map.pick.test, do.call(rbind, lapply(seq(nrow(matches.df.map.pick.test)), function(i) unlist(matches.df.map.pick.test$pick.3[i]))))
matches.df.map.pick.test <- cbind(matches.df.map.pick.test, do.call(rbind, lapply(seq(nrow(matches.df.map.pick.test)), function(i) unlist(matches.df.map.pick.test$player.3[i]))))
matches.df.map.pick.test <- cbind(matches.df.map.pick.test, do.call(rbind, lapply(seq(nrow(matches.df.map.pick.test)), function(i) unlist(matches.df.map.pick.test$ban.3[i]))))
matches.df.map.pick.test <- cbind(matches.df.map.pick.test, do.call(rbind, lapply(seq(nrow(matches.df.map.pick.test)), function(i) unlist(matches.df.map.pick.test$pick.6[i]))))
matches.df.map.pick.test <- cbind(matches.df.map.pick.test, do.call(rbind, lapply(seq(nrow(matches.df.map.pick.test)), function(i) unlist(matches.df.map.pick.test$player.6[i]))))
matches.df.map.pick.test <- cbind(matches.df.map.pick.test, do.call(rbind, lapply(seq(nrow(matches.df.map.pick.test)), function(i) unlist(matches.df.map.pick.test$pick.7[i]))))
matches.df.map.pick.test <- cbind(matches.df.map.pick.test, do.call(rbind, lapply(seq(nrow(matches.df.map.pick.test)), function(i) unlist(matches.df.map.pick.test$player.7[i]))))
matches.df.map.pick.test <- cbind(matches.df.map.pick.test, do.call(rbind, lapply(seq(nrow(matches.df.map.pick.test)), function(i) unlist(matches.df.map.pick.test$pick.10[i]))))
matches.df.map.pick.test <- cbind(matches.df.map.pick.test, do.call(rbind, lapply(seq(nrow(matches.df.map.pick.test)), function(i) unlist(matches.df.map.pick.test$player.10[i]))))

    # relabel column variables and remove unncessary data frame
colnames(matches.df.map.pick.test)[26:46] <- c('id', 'date', 'patch', 'tournament', 'stage', 'round', 'series', 'game', 'map', 'ban.2', 'pick.2', 'player.2', 'pick.3', 'player.3', 'ban.3', 'pick.6', 'player.6', 'pick.7', 'player.7', 'pick.10', 'player.10')
matches.df.map.pick <- matches.df.map.pick.test[,c(26:34, 10:13, 35:46)]
rm(matches.df.map.pick.test)

# History of All Player Matches
    # create groups for rbind from first pick drafts
group1 <- matches.df.first.pick[,c(1:13, 15:16)]
group2 <- matches.df.first.pick[,c(1:13, 17:18)]
group3 <- matches.df.first.pick[,c(1:13, 19:20)]
group4 <- matches.df.first.pick[,c(1:13, 22:23)]
group5 <- matches.df.first.pick[,c(1:13, 24:25)]

    # relabel column names for first pick drafts
colnames(group1)[14:15] <- c('hero', 'player')
colnames(group2)[14:15] <- c('hero', 'player')
colnames(group3)[14:15] <- c('hero', 'player')
colnames(group4)[14:15] <- c('hero', 'player')
colnames(group5)[14:15] <- c('hero', 'player')

    # create groups for rbind from map pick drafts
group6 <- matches.df.map.pick[,c(1:13, 15:16)]
group7 <- matches.df.map.pick[,c(1:13, 17:18)]
group8 <- matches.df.map.pick[,c(1:13, 20:21)]
group9 <- matches.df.map.pick[,c(1:13, 22:23)]
group10 <- matches.df.map.pick[,c(1:13, 24:25)]

    # relabel column names for map pick drafts
colnames(group6)[14:15] <- c('hero', 'player')
colnames(group7)[14:15] <- c('hero', 'player')
colnames(group8)[14:15] <- c('hero', 'player')
colnames(group9)[14:15] <- c('hero', 'player')
colnames(group10)[14:15] <- c('hero', 'player')

    # combine all data frames together into one large data frame and remove unncessary data frames
player.hero.history.df <- rbind(group1, group2, group3, group4, group5, group6, group7, group8, group9, group10)
rm(group1)
rm(group2)
rm(group3)
rm(group4)
rm(group5)
rm(group6)
rm(group7)
rm(group8)
rm(group9)
rm(group10)

    # change variables to match real class
player.hero.history.df$date <- as.Date(player.hero.history.df$date)

# Analysis 
player.hero.history.df %>% 
  filter(tournament == 'HGC North America') %>% 
  group_by(player, hero) %>% 
  summarize(played = n(),
            wins = sum(is_winner),
            losses = played - wins,
            win.percent = scales::percent(round(wins/played,2)))

player.hero.history.df %>% 
  filter(date > '2016-03-01',
         team == 'Gale Force eSports') %>% 
  group_by(map, hero, player) %>% 
  summarize(played = n(),
            wins = sum(is_winner),
            losses = played - wins,
            win.percent = scales::percent(round(wins/played,2))) %>% 
  arrange(desc(played))

player.hero.history.df %>% 
  filter(player == 'Splendour',
         map == 'Infernal Shrines') %>% 
  group_by(hero) %>% 
  select(hero, is_winner) %>% 
  summarize(played = n(),
            wins = sum(is_winner),
            losses = played - wins,
            win.percent = scales::percent(round(wins/played,2))) %>% 
  arrange(desc(played))

matches.df.first.pick %>% 
  filter(team == 'Misfits' | team == 'mYinsanity') %>% 
  select(map, date, team, is_winner, pick.1, pick.4, pick.5, pick.8, pick.9) %>% 
  filter(pick.1 == 'Diablo' | pick.4 == 'Diablo' | pick.5 == 'Diablo' | pick.8 == 'Diablo' | pick.9 == 'Diablo')

matches.df.map.pick %>% 
  filter(team == 'Misfits' | team == 'mYinsanity') %>% 
  select(map, date, team, is_winner, pick.2, pick.3, pick.6, pick.7, pick.10) %>% 
  filter(pick.2 == 'Diablo' | pick.3 == 'Diablo' | pick.6 == 'Diablo' | pick.7 == 'Diablo' | pick.10 == 'Diablo')

player.hero.history.df %>% 
  filter(player == 'Blumbi',
         hero == 'Muradin') %>% 
  group_by(hero, date, is_winner) %>% 
  arrange(desc(date))

player.hero.history.df %>% 
  filter(team == 'Team Dignitas',
         date > '2016-06-01',
         !hero %in% c('Malfurion', 'Rehgar')) %>% 
  group_by(map, hero) %>% 
  summarize(played = n(),
            wins = sum(is_winner),
            losses = played - wins,
            win.percent = scales::percent(round(wins/played,2))) %>% 
  arrange(desc(played))

player.hero.history.df %>% 
  filter(team == 'Team Dignitas',
         hero == 'Arthas') %>% 
  group_by(hero, map, date, is_winner) %>% 
  arrange(desc(date))
