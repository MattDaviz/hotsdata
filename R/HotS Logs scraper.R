# Load libraries ----
library(XML); library(RCurl); library(plyr); library(stringr); library(ggplot2); library(ggthemes); library(dplyr); library(tidyr); library(ggrepel)
library(reshape2); library(grid); library(scales); library(jpeg); library(png); library(grid); library(RSelenium)

# Overall Hero Win Rates - Regardless of HL Rank ----
    # Get all hero roles and subroles
url <- getURL('https://www.hotslogs.com/Default')
overall.win.rate <- readHTMLTable(url)
overall.win.rate <- as.data.frame(overall.win.rate[2])
overall.win.rate <- overall.win.rate[,2:9]
colnames(overall.win.rate) <- c('hero', 'Games.Played', 'Games.Banned', 'Popularity', 'Win.Percent', 'delta.Win.Percent' ,'Role', 'Specialty')

    # Clean up scraped data
overall.win.rate <- overall.win.rate %>% 
  mutate(hero = gsub('%20', ' ', hero),
         hero = as.factor(gsub('%27', ' ', hero)),
         Games.Played = as.numeric(gsub(',', '', Games.Played)),
         Win.Percent = as.numeric(gsub('%', '', Win.Percent)),
         Popularity = as.numeric(gsub('%', '', Win.Percent)),
         Games.Banned = as.numeric(gsub(',', '', Games.Banned)),
         delta.Win.Percent = as.numeric(gsub('%', '', delta.Win.Percent)))

# Selenium Scraper for HL data by division ----
    # Higher level data (Platinum, Diamond, Master) ----

    # Set up and open phantomjs browser (NOTE: YOU MUST INSTALL PHANTOMJS AND PUT THE PATH TO THE EXECUTABLE FILE IN YOUR SYTEM PATH       AND THEN RESTART RSTUDIO)
pJS <- phantom()
remDr <- remoteDriver(browserName = 'phantomjs')
remDr$open(silent = T)

    # Set url to navigate to
url <- 'https://www.hotslogs.com/Sitewide/HeroAndMapStatistics'
remDr$navigate(url)

    # Check Platinum, Diamond, and Master
dropdown <- remDr$findElement(using = 'css selector', value = "#ComboBoxLeague .rcbActionButton")
Sys.sleep(1)
dropdown$clickElement()
platinum <- remDr$findElement(using = 'xpath', value = "//*[(((count(preceding-sibling::*) + 1) = 3) and parent::*)]//label | //*[contains(concat( ' ', @class, ' ' ), concat( ' ', 'rcbHovered', ' ' ))]//label//*[contains(concat( ' ', @class, ' ' ), concat( ' ', 'rcbItem', ' ' )) and (((count(preceding-sibling::*) + 1) = 1) and parent::*)]//label")
Sys.sleep(1)
platinum$clickElement()
masters <- remDr$findElement(using = 'xpath', value = "//*[(((count(preceding-sibling::*) + 1) = 2) and parent::*)]//label | //*[contains(concat( ' ', @class, ' ' ), concat( ' ', 'rcbHovered', ' ' ))]//label//*[contains(concat( ' ', @class, ' ' ), concat( ' ', 'rcbItem', ' ' )) and (((count(preceding-sibling::*) + 1) = 1) and parent::*)]//label")
Sys.sleep(1)
masters$clickElement()
diamond <- remDr$findElement(using = 'xpath', value = "//*[contains(concat( ' ', @class, ' ' ), concat( ' ', 'rcbItem', ' ' ))]//label")
Sys.sleep(1)
diamond$clickElement()
dropdown$clickElement()
Sys.sleep(10)

doc <- htmlParse(remDr$getPageSource()[[1]])
test <- readHTMLTable(doc)
overallplus <- as.data.frame(test[3])
overallplus <- overallplus[,2:9]
overallplus$Map.Name <- 'Overall'
colnames(overallplus) <- c('hero', 'Games.Played', 'Games.Banned', 'Popularity', 'avg.winrate', 'Delta', 'Role', 'Specialty', 'Map.Name')
Sys.sleep(1)

    # Battlefield of Eternity - Plat+
maphl <- remDr$findElement(using = 'css selector', value = '#ctl00_MainContent_ComboBoxMapName .rcbActionButton')
Sys.sleep(1)
maphl$clickElement()
boehl <- remDr$findElement(using = 'xpath', value = "//*[contains(concat( ' ', @class, ' ' ), concat( ' ', 'rcbItem', ' ' )) and (((count(preceding-sibling::*) + 1) = 1) and parent::*)]//label")
Sys.sleep(1)
boehl$clickElement()
maphl <- remDr$findElement(using = 'css selector', value = '#ctl00_MainContent_ComboBoxMapName .rcbActionButton')
Sys.sleep(1)
maphl$clickElement()
Sys.sleep(20)

doc <- htmlParse(remDr$getPageSource()[[1]])
test <- readHTMLTable(doc)
boe <- as.data.frame(test[3])
boe$Map.Name <- 'Battlefield of Eternity'
boe <- boe[,c(2:9,11)]
colnames(boe) <- c('hero', 'Games.Played', 'Games.Banned', 'Popularity', 'Win.Percent', 'Delta', 'Role', 'Specialty',
                   'Map.Name')
Sys.sleep(1)

    # Blackheart's Bay - Plat+
maphl <- remDr$findElement(using = 'css selector', value = '#ctl00_MainContent_ComboBoxMapName .rcbActionButton')
Sys.sleep(1)
maphl$clickElement()
bhbhl <- remDr$findElement(using = 'xpath', value = "//*[contains(concat( ' ', @class, ' ' ), concat( ' ', 'rcbItem', ' ' )) and (((count(preceding-sibling::*) + 1) = 2) and parent::*)]//label")
Sys.sleep(1)
bhbhl$clickElement()
boehl <- remDr$findElement(using = 'xpath', value = "//*[contains(concat( ' ', @class, ' ' ), concat( ' ', 'rcbItem', ' ' )) and (((count(preceding-sibling::*) + 1) = 1) and parent::*)]//label")
Sys.sleep(1)
boehl$clickElement()
maphl <- remDr$findElement(using = 'css selector', value = '#ctl00_MainContent_ComboBoxMapName .rcbActionButton')
maphl$clickElement()
Sys.sleep(20)

doc <- htmlParse(remDr$getPageSource()[[1]])
Sys.sleep(1)
test <- readHTMLTable(doc)
Sys.sleep(1)
bhb <- as.data.frame(test[3])
bhb$Map.Name <- "Blackheart's Bay"
bhb <- bhb[,c(2:9,11)]
colnames(bhb) <- c('hero', 'Games.Played', 'Games.Banned', 'Popularity', 'Win.Percent', 'Delta', 'Role', 'Specialty',
                   'Map.Name')
Sys.sleep(1)

    # Braxis Holdout - Plat+
maphl <- remDr$findElement(using = 'css selector', value = '#ctl00_MainContent_ComboBoxMapName .rcbActionButton')
Sys.sleep(1)
maphl$clickElement()
bhhl <- remDr$findElement(using = 'xpath', value = "//*[contains(concat( ' ', @class, ' ' ), concat( ' ', 'rcbItem', ' ' )) and (((count(preceding-sibling::*) + 1) = 3) and parent::*)]//label")
Sys.sleep(1)
bhhl$clickElement()
bhbhl <- remDr$findElement(using = 'xpath', value = "//*[contains(concat( ' ', @class, ' ' ), concat( ' ', 'rcbItem', ' ' )) and (((count(preceding-sibling::*) + 1) = 2) and parent::*)]//label")
Sys.sleep(1)
bhbhl$clickElement()
maphl$clickElement()
Sys.sleep(20)

doc <- htmlParse(remDr$getPageSource()[[1]])
Sys.sleep(1)
test <- readHTMLTable(doc)
Sys.sleep(1)
bh <- as.data.frame(test[3])
bh$Map.Name <- 'Braxis Holdout'
bh <- bh[,c(2:9,11)]
colnames(bh) <- c('hero', 'Games.Played', 'Games.Banned', 'Popularity', 'Win.Percent', 'Delta', 'Role', 'Specialty',
                  'Map.Name')
Sys.sleep(1)

    # Cursed Hollow - Plat+
maphl <- remDr$findElement(using = 'css selector', value = '#ctl00_MainContent_ComboBoxMapName .rcbActionButton')
Sys.sleep(1)
maphl$clickElement()
chhl <- remDr$findElement(using = 'xpath', value = "//*[contains(concat( ' ', @class, ' ' ), concat( ' ', 'rcbItem', ' ' )) and (((count(preceding-sibling::*) + 1) = 5) and parent::*)]//label")
Sys.sleep(1)
chhl$clickElement()
bhhl <- remDr$findElement(using = 'xpath', value = "//*[contains(concat( ' ', @class, ' ' ), concat( ' ', 'rcbItem', ' ' )) and (((count(preceding-sibling::*) + 1) = 3) and parent::*)]//label")
Sys.sleep(1)
bhhl$clickElement()
maphl$clickElement()
Sys.sleep(30)

doc <- htmlParse(remDr$getPageSource()[[1]])
test <- readHTMLTable(doc)
ch <- as.data.frame(test[3])
ch$Map.Name <- 'Cursed Hollow'
ch <- ch[,c(2:9,11)]
colnames(ch) <- c('hero', 'Games.Played', 'Games.Banned', 'Popularity', 'Win.Percent', 'Delta', 'Role', 'Specialty',
                  'Map.Name')
Sys.sleep(1)

    # Dragon Shire - Plat+
maphl <- remDr$findElement(using = 'css selector', value = '#ctl00_MainContent_ComboBoxMapName .rcbActionButton')
Sys.sleep(1)
maphl$clickElement()
dshl <- remDr$findElement(using = 'xpath', value = "//*[contains(concat( ' ', @class, ' ' ), concat( ' ', 'rcbItem', ' ' )) and (((count(preceding-sibling::*) + 1) = 6) and parent::*)]//label")
Sys.sleep(1)
dshl$clickElement()
chhl <- remDr$findElement(using = 'xpath', value = "//*[contains(concat( ' ', @class, ' ' ), concat( ' ', 'rcbItem', ' ' )) and (((count(preceding-sibling::*) + 1) = 5) and parent::*)]//label")
Sys.sleep(1)
chhl$clickElement()
maphl$clickElement()
Sys.sleep(20)

doc <- htmlParse(remDr$getPageSource()[[1]])
test <- readHTMLTable(doc)
ds <- as.data.frame(test[3])
ds$Map.Name <- 'Dragon Shire'
ds <- ds[,c(2:9,11)]
colnames(ds) <- c('hero', 'Games.Played', 'Games.Banned', 'Popularity', 'Win.Percent', 'Delta', 'Role', 'Specialty',
                  'Map.Name')
Sys.sleep(1)

    # Garden of Terror - Plat+
maphl <- remDr$findElement(using = 'css selector', value = '#ctl00_MainContent_ComboBoxMapName .rcbActionButton')
Sys.sleep(1)
maphl$clickElement()
gothl <- remDr$findElement(using = 'xpath', value = "//*[contains(concat( ' ', @class, ' ' ), concat( ' ', 'rcbItem', ' ' )) and (((count(preceding-sibling::*) + 1) = 7) and parent::*)]//label")
Sys.sleep(1)
gothl$clickElement()
dshl <- remDr$findElement(using = 'xpath', value = "//*[contains(concat( ' ', @class, ' ' ), concat( ' ', 'rcbItem', ' ' )) and (((count(preceding-sibling::*) + 1) = 6) and parent::*)]//label")
Sys.sleep(1)
dshl$clickElement()
maphl$clickElement()
offclick <- remDr$findElement(using = 'css selector', value = '.section-title')
offclick$clickElement()
Sys.sleep(20)

doc <- htmlParse(remDr$getPageSource()[[1]])
test <- readHTMLTable(doc)
got <- as.data.frame(test[3])
got$Map.Name <- 'Garden of Terror'
got <- got[,c(2:9,11)]
colnames(got) <- c('hero', 'Games.Played', 'Games.Banned', 'Popularity', 'Win.Percent', 'Delta', 'Role', 'Specialty',
                   'Map.Name')
Sys.sleep(1)

    # Haunted Mines - Plat+
maphl <- remDr$findElement(using = 'css selector', value = '#ctl00_MainContent_ComboBoxMapName .rcbActionButton')
Sys.sleep(1)
maphl$clickElement()
hmhl <- remDr$findElement(using = 'xpath', value = "//*[contains(concat( ' ', @class, ' ' ), concat( ' ', 'rcbItem', ' ' )) and (((count(preceding-sibling::*) + 1) = 8) and parent::*)]//label")
Sys.sleep(1)
hmhl$clickElement()
gothl <- remDr$findElement(using = 'xpath', value = "//*[contains(concat( ' ', @class, ' ' ), concat( ' ', 'rcbItem', ' ' )) and (((count(preceding-sibling::*) + 1) = 7) and parent::*)]//label")
Sys.sleep(1)
gothl$clickElement()
maphl$clickElement()
Sys.sleep(20)

doc <- htmlParse(remDr$getPageSource()[[1]])
test <- readHTMLTable(doc)
hm <- as.data.frame(test[3])
hm$Map.Name <- 'Haunted Mines'
hm <- hm[,c(2:9,11)]
colnames(hm) <- c('hero', 'Games.Played', 'Games.Banned', 'Popularity', 'Win.Percent', 'Delta', 'Role', 'Specialty',
                  'Map.Name')
Sys.sleep(1)

    # Infernal Shrines - Plat+
maphl <- remDr$findElement(using = 'css selector', value = '#ctl00_MainContent_ComboBoxMapName .rcbActionButton')
Sys.sleep(1)
maphl$clickElement()
ishl <- remDr$findElement(using = 'xpath', value = "//*[contains(concat( ' ', @class, ' ' ), concat( ' ', 'rcbItem', ' ' )) and (((count(preceding-sibling::*) + 1) = 9) and parent::*)]//label")
Sys.sleep(1)
ishl$clickElement()
hmhl <- remDr$findElement(using = 'xpath', value = "//*[contains(concat( ' ', @class, ' ' ), concat( ' ', 'rcbItem', ' ' )) and (((count(preceding-sibling::*) + 1) = 8) and parent::*)]//label")
Sys.sleep(1)
hmhl$clickElement()
maphl$clickElement()
Sys.sleep(20)

doc <- htmlParse(remDr$getPageSource()[[1]])
test <- readHTMLTable(doc)
is <- as.data.frame(test[3])
is$Map.Name <- 'Infernal Shrines'
is <- is[,c(2:9,11)]
colnames(is) <- c('hero', 'Games.Played', 'Games.Banned', 'Popularity', 'Win.Percent', 'Delta', 'Role', 'Specialty',
                  'Map.Name')
Sys.sleep(1)

    # Sky Temple - Plat+
maphl <- remDr$findElement(using = 'css selector', value = '#ctl00_MainContent_ComboBoxMapName .rcbActionButton')
Sys.sleep(1)
maphl$clickElement()
sthl <- remDr$findElement(using = 'xpath', value = "//*[contains(concat( ' ', @class, ' ' ), concat( ' ', 'rcbItem', ' ' )) and (((count(preceding-sibling::*) + 1) = 12) and parent::*)]//label")
Sys.sleep(1)
sthl$clickElement()
ishl <- remDr$findElement(using = 'xpath', value = "//*[contains(concat( ' ', @class, ' ' ), concat( ' ', 'rcbItem', ' ' )) and (((count(preceding-sibling::*) + 1) = 9) and parent::*)]//label")
Sys.sleep(1)
ishl$clickElement()
maphl$clickElement()
Sys.sleep(30)

doc <- htmlParse(remDr$getPageSource()[[1]])
test <- readHTMLTable(doc)
st <- as.data.frame(test[3])
st$Map.Name <- 'Sky Temple'
st <- st[,c(2:9,11)]
colnames(st) <- c('hero', 'Games.Played', 'Games.Banned', 'Popularity', 'Win.Percent', 'Delta', 'Role', 'Specialty',
                  'Map.Name')
Sys.sleep(1)

    # Tomb of the Spider Queen - Plat+
maphl <- remDr$findElement(using = 'css selector', value = '#ctl00_MainContent_ComboBoxMapName .rcbActionButton')
Sys.sleep(1)
maphl$clickElement()
tosqhl <- remDr$findElement(using = 'xpath', value = "//*[contains(concat( ' ', @class, ' ' ), concat( ' ', 'rcbItem', ' ' )) and (((count(preceding-sibling::*) + 1) = 13) and parent::*)]//label")
Sys.sleep(1)
tosqhl$clickElement()
sthl <- remDr$findElement(using = 'xpath', value = "//*[contains(concat( ' ', @class, ' ' ), concat( ' ', 'rcbItem', ' ' )) and (((count(preceding-sibling::*) + 1) = 12) and parent::*)]//label")
Sys.sleep(1)
sthl$clickElement()
maphl$clickElement()
Sys.sleep(20)

doc <- htmlParse(remDr$getPageSource()[[1]])
test <- readHTMLTable(doc)
tosq <- as.data.frame(test[3])
tosq$Map.Name <- 'Tomb of the Spider Queen'
tosq <- tosq[,c(2:9,11)]
colnames(tosq) <- c('hero', 'Games.Played', 'Games.Banned', 'Popularity', 'Win.Percent', 'Delta', 'Role', 'Specialty',
                    'Map.Name')
Sys.sleep(1)

    # Towers of Doom - Plat+
maphl <- remDr$findElement(using = 'css selector', value = '#ctl00_MainContent_ComboBoxMapName .rcbActionButton')
Sys.sleep(1)
maphl$clickElement()
todhl <- remDr$findElement(using = 'xpath', value = "//*[contains(concat( ' ', @class, ' ' ), concat( ' ', 'rcbItem', ' ' )) and (((count(preceding-sibling::*) + 1) = 14) and parent::*)]//label")
Sys.sleep(1)
todhl$clickElement()
tosqhl <- remDr$findElement(using = 'xpath', value = "//*[contains(concat( ' ', @class, ' ' ), concat( ' ', 'rcbItem', ' ' )) and (((count(preceding-sibling::*) + 1) = 13) and parent::*)]//label")
Sys.sleep(1)
tosqhl$clickElement()
maphl$clickElement()
Sys.sleep(20)

doc <- htmlParse(remDr$getPageSource()[[1]])
test <- readHTMLTable(doc)
tod <- as.data.frame(test[3])
tod$Map.Name <- 'Towers of Doom'
tod <- tod[,c(2:9,11)]
colnames(tod) <- c('hero', 'Games.Played', 'Games.Banned', 'Popularity', 'Win.Percent', 'Delta', 'Role', 'Specialty',
                   'Map.Name')
Sys.sleep(1)

    # Warhead Junction - Plat+
maphl <- remDr$findElement(using = 'css selector', value = '#ctl00_MainContent_ComboBoxMapName .rcbActionButton')
Sys.sleep(1)
maphl$clickElement()
wjhl <- remDr$findElement(using = 'xpath', value = "//*[contains(concat( ' ', @class, ' ' ), concat( ' ', 'rcbItem', ' ' )) and (((count(preceding-sibling::*) + 1) = 15) and parent::*)]//label")
Sys.sleep(1)
wjhl$clickElement()
todhl <- remDr$findElement(using = 'xpath', value = "//*[contains(concat( ' ', @class, ' ' ), concat( ' ', 'rcbItem', ' ' )) and (((count(preceding-sibling::*) + 1) = 14) and parent::*)]//label")
Sys.sleep(1)
todhl$clickElement()
maphl$clickElement()
Sys.sleep(20)

doc <- htmlParse(remDr$getPageSource()[[1]])
test <- readHTMLTable(doc)
wj <- as.data.frame(test[3])
wj$Map.Name <- 'Warhead Junction'
wj <- wj[,c(2:9,11)]
colnames(wj) <- c('hero', 'Games.Played', 'Games.Banned', 'Popularity', 'Win.Percent', 'Delta', 'Role', 'Specialty',
                  'Map.Name')
Sys.sleep(1)

    # Close browser window
remDr$close()

    # Screen shot
#remDr$screenshot(display = TRUE)

    # Lower level data (Bronze, Silver, Gold) ----

    # Set up and open phantomjs browser
pJS <- phantom()
remDr <- remoteDriver(browserName = 'phantomjs')
remDr$open(silent = T)

    # Set url to navigate to
url <- 'https://www.hotslogs.com/Sitewide/HeroAndMapStatistics'
remDr$navigate(url)

    # Check Bronze, Silver, and Gold
dropdown <- remDr$findElement(using = 'css selector', value = "#ComboBoxLeague .rcbActionButton")
Sys.sleep(1)
dropdown$clickElement()
gold <- remDr$findElement(using = 'xpath', value = "//*[contains(concat( ' ', @class, ' ' ), concat( ' ', 'rcbItem', ' ' )) and (((count(preceding-sibling::*) + 1) = 4) and parent::*)]//label")
Sys.sleep(1)
gold$clickElement()
silver <- remDr$findElement(using = 'xpath', value = "//*[contains(concat( ' ', @class, ' ' ), concat( ' ', 'rcbItem', ' ' )) and (((count(preceding-sibling::*) + 1) = 5) and parent::*)]//label")
Sys.sleep(1)
silver$clickElement()
bronze <- remDr$findElement(using = 'xpath', value = "//*[contains(concat( ' ', @class, ' ' ), concat( ' ', 'rcbItem', ' ' )) and (((count(preceding-sibling::*) + 1) = 6) and parent::*)]//label")
Sys.sleep(1)
bronze$clickElement()
dropdown$clickElement()
Sys.sleep(10)

doc <- htmlParse(remDr$getPageSource()[[1]])
test <- readHTMLTable(doc)
overallminus <- as.data.frame(test[3])
overallminus <- overallminus[,2:9]
overallminus$Map.Name <- 'Overall'
colnames(overallminus) <- c('hero', 'Games.Played', 'Games.Banned', 'Popularity', 'avg.winrate', 'Delta', 'Role', 'Specialty', 'Map.Name')
Sys.sleep(1)

    # Battlefield of Eternity - Lower leagues
maphl <- remDr$findElement(using = 'css selector', value = '#ctl00_MainContent_ComboBoxMapName .rcbActionButton')
Sys.sleep(1)
maphl$clickElement()
boehl <- remDr$findElement(using = 'xpath', value = "//*[contains(concat( ' ', @class, ' ' ), concat( ' ', 'rcbItem', ' ' )) and (((count(preceding-sibling::*) + 1) = 1) and parent::*)]//label")
Sys.sleep(1)
boehl$clickElement()
maphl <- remDr$findElement(using = 'css selector', value = '#ctl00_MainContent_ComboBoxMapName .rcbActionButton')
Sys.sleep(1)
maphl$clickElement()
Sys.sleep(20)

doc <- htmlParse(remDr$getPageSource()[[1]])
test <- readHTMLTable(doc)
boeminus <- as.data.frame(test[3])
boeminus$Map.Name <- 'Battlefield of Eternity'
boeminus <- boeminus[,c(2:9,11)]
colnames(boeminus) <- c('hero', 'Games.Played', 'Games.Banned', 'Popularity', 'Win.Percent', 'Delta', 'Role', 'Specialty',
                        'Map.Name')
Sys.sleep(1)

    # Blackheart's Bay - Bronze+
maphl <- remDr$findElement(using = 'css selector', value = '#ctl00_MainContent_ComboBoxMapName .rcbActionButton')
Sys.sleep(1)
maphl$clickElement()
bhbminushl <- remDr$findElement(using = 'xpath', value = "//*[contains(concat( ' ', @class, ' ' ), concat( ' ', 'rcbItem', ' ' )) and (((count(preceding-sibling::*) + 1) = 2) and parent::*)]//label")
Sys.sleep(1)
bhbminushl$clickElement()
boehl <- remDr$findElement(using = 'xpath', value = "//*[contains(concat( ' ', @class, ' ' ), concat( ' ', 'rcbItem', ' ' )) and (((count(preceding-sibling::*) + 1) = 1) and parent::*)]//label")
Sys.sleep(1)
boehl$clickElement()
maphl <- remDr$findElement(using = 'css selector', value = '#ctl00_MainContent_ComboBoxMapName .rcbActionButton')
maphl$clickElement()
Sys.sleep(20)

doc <- htmlParse(remDr$getPageSource()[[1]])
Sys.sleep(1)
test <- readHTMLTable(doc)
Sys.sleep(1)
bhbminus <- as.data.frame(test[3])
bhbminus$Map.Name <- "Blackheart's Bay"
bhbminus <- bhbminus[,c(2:9,11)]
colnames(bhbminus) <- c('hero', 'Games.Played', 'Games.Banned', 'Popularity', 'Win.Percent', 'Delta', 'Role', 'Specialty',
                        'Map.Name')
Sys.sleep(1)

    # Braxis Holdout - Bronze+
maphl <- remDr$findElement(using = 'css selector', value = '#ctl00_MainContent_ComboBoxMapName .rcbActionButton')
Sys.sleep(1)
maphl$clickElement()
bhhl <- remDr$findElement(using = 'xpath', value = "//*[contains(concat( ' ', @class, ' ' ), concat( ' ', 'rcbItem', ' ' )) and (((count(preceding-sibling::*) + 1) = 3) and parent::*)]//label")
Sys.sleep(1)
bhhl$clickElement()
bhbhl <- remDr$findElement(using = 'xpath', value = "//*[contains(concat( ' ', @class, ' ' ), concat( ' ', 'rcbItem', ' ' )) and (((count(preceding-sibling::*) + 1) = 2) and parent::*)]//label")
Sys.sleep(1)
bhbhl$clickElement()
maphl$clickElement()
Sys.sleep(20)

doc <- htmlParse(remDr$getPageSource()[[1]])
Sys.sleep(1)
test <- readHTMLTable(doc)
Sys.sleep(1)
bhminus <- as.data.frame(test[3])
bhminus$Map.Name <- 'Braxis Holdout'
bhminus <- bhminus[,c(2:9,11)]
colnames(bhminus) <- c('hero', 'Games.Played', 'Games.Banned', 'Popularity', 'Win.Percent', 'Delta', 'Role', 'Specialty',
                       'Map.Name')
Sys.sleep(1)

    # Cursed Hollow - Bronze+
maphl <- remDr$findElement(using = 'css selector', value = '#ctl00_MainContent_ComboBoxMapName .rcbActionButton')
Sys.sleep(1)
maphl$clickElement()
chhl <- remDr$findElement(using = 'xpath', value = "//*[contains(concat( ' ', @class, ' ' ), concat( ' ', 'rcbItem', ' ' )) and (((count(preceding-sibling::*) + 1) = 5) and parent::*)]//label")
Sys.sleep(1)
chhl$clickElement()
bhhl <- remDr$findElement(using = 'xpath', value = "//*[contains(concat( ' ', @class, ' ' ), concat( ' ', 'rcbItem', ' ' )) and (((count(preceding-sibling::*) + 1) = 3) and parent::*)]//label")
Sys.sleep(1)
bhhl$clickElement()
maphl$clickElement()
Sys.sleep(20)

doc <- htmlParse(remDr$getPageSource()[[1]])
test <- readHTMLTable(doc)
chminus <- as.data.frame(test[3])
chminus$Map.Name <- 'Cursed Hollow'
chminus <- chminus[,c(2:9,11)]
colnames(chminus) <- c('hero', 'Games.Played', 'Games.Banned', 'Popularity', 'Win.Percent', 'Delta', 'Role', 'Specialty',
                       'Map.Name')
Sys.sleep(1)

    # Dragon Shire - Bronze+
maphl <- remDr$findElement(using = 'css selector', value = '#ctl00_MainContent_ComboBoxMapName .rcbActionButton')
Sys.sleep(1)
maphl$clickElement()
dshl <- remDr$findElement(using = 'xpath', value = "//*[contains(concat( ' ', @class, ' ' ), concat( ' ', 'rcbItem', ' ' )) and (((count(preceding-sibling::*) + 1) = 6) and parent::*)]//label")
Sys.sleep(1)
dshl$clickElement()
chhl <- remDr$findElement(using = 'xpath', value = "//*[contains(concat( ' ', @class, ' ' ), concat( ' ', 'rcbItem', ' ' )) and (((count(preceding-sibling::*) + 1) = 5) and parent::*)]//label")
Sys.sleep(1)
chhl$clickElement()
maphl$clickElement()
Sys.sleep(20)

doc <- htmlParse(remDr$getPageSource()[[1]])
test <- readHTMLTable(doc)
dsminus <- as.data.frame(test[3])
dsminus$Map.Name <- 'Dragon Shire'
dsminus <- dsminus[,c(2:9,11)]
colnames(dsminus) <- c('hero', 'Games.Played', 'Games.Banned', 'Popularity', 'Win.Percent', 'Delta', 'Role', 'Specialty',
                       'Map.Name')
Sys.sleep(1)

    # Garden of Terror - Bronze+
maphl <- remDr$findElement(using = 'css selector', value = '#ctl00_MainContent_ComboBoxMapName .rcbActionButton')
Sys.sleep(1)
maphl$clickElement()
gothl <- remDr$findElement(using = 'xpath', value = "//*[contains(concat( ' ', @class, ' ' ), concat( ' ', 'rcbItem', ' ' )) and (((count(preceding-sibling::*) + 1) = 7) and parent::*)]//label")
Sys.sleep(1)
gothl$clickElement()
dshl <- remDr$findElement(using = 'xpath', value = "//*[contains(concat( ' ', @class, ' ' ), concat( ' ', 'rcbItem', ' ' )) and (((count(preceding-sibling::*) + 1) = 6) and parent::*)]//label")
Sys.sleep(1)
dshl$clickElement()
maphl$clickElement()
offclick <- remDr$findElement(using = 'css selector', value = '.section-title')
offclick$clickElement()
Sys.sleep(20)

doc <- htmlParse(remDr$getPageSource()[[1]])
test <- readHTMLTable(doc)
gotminus <- as.data.frame(test[3])
gotminus$Map.Name <- 'Garden of Terror'
gotminus <- gotminus[,c(2:9,11)]
colnames(gotminus) <- c('hero', 'Games.Played', 'Games.Banned', 'Popularity', 'Win.Percent', 'Delta', 'Role', 'Specialty',
                        'Map.Name')
Sys.sleep(1)

    # Haunted Mines - Bronze+
maphl <- remDr$findElement(using = 'css selector', value = '#ctl00_MainContent_ComboBoxMapName .rcbActionButton')
Sys.sleep(1)
maphl$clickElement()
hmhl <- remDr$findElement(using = 'xpath', value = "//*[contains(concat( ' ', @class, ' ' ), concat( ' ', 'rcbItem', ' ' )) and (((count(preceding-sibling::*) + 1) = 8) and parent::*)]//label")
Sys.sleep(1)
hmhl$clickElement()
gothl <- remDr$findElement(using = 'xpath', value = "//*[contains(concat( ' ', @class, ' ' ), concat( ' ', 'rcbItem', ' ' )) and (((count(preceding-sibling::*) + 1) = 7) and parent::*)]//label")
Sys.sleep(1)
gothl$clickElement()
maphl$clickElement()
Sys.sleep(20)

doc <- htmlParse(remDr$getPageSource()[[1]])
test <- readHTMLTable(doc)
hmminus <- as.data.frame(test[3])
hmminus$Map.Name <- 'Haunted Mines'
hmminus <- hmminus[,c(2:9,11)]
colnames(hmminus) <- c('hero', 'Games.Played', 'Games.Banned', 'Popularity', 'Win.Percent', 'Delta', 'Role', 'Specialty',
                       'Map.Name')
Sys.sleep(1)

    # Infernal Shrines - Bronze+
maphl <- remDr$findElement(using = 'css selector', value = '#ctl00_MainContent_ComboBoxMapName .rcbActionButton')
Sys.sleep(1)
maphl$clickElement()
ishl <- remDr$findElement(using = 'xpath', value = "//*[contains(concat( ' ', @class, ' ' ), concat( ' ', 'rcbItem', ' ' )) and (((count(preceding-sibling::*) + 1) = 9) and parent::*)]//label")
Sys.sleep(1)
ishl$clickElement()
hmhl <- remDr$findElement(using = 'xpath', value = "//*[contains(concat( ' ', @class, ' ' ), concat( ' ', 'rcbItem', ' ' )) and (((count(preceding-sibling::*) + 1) = 8) and parent::*)]//label")
Sys.sleep(1)
hmhl$clickElement()
maphl$clickElement()
Sys.sleep(20)

doc <- htmlParse(remDr$getPageSource()[[1]])
test <- readHTMLTable(doc)
isminus <- as.data.frame(test[3])
isminus$Map.Name <- 'Infernal Shrines'
isminus <- isminus[,c(2:9,11)]
colnames(isminus) <- c('hero', 'Games.Played', 'Games.Banned', 'Popularity', 'Win.Percent', 'Delta', 'Role', 'Specialty',
                       'Map.Name')
Sys.sleep(1)

    # Sky Temple - Bronze+
maphl <- remDr$findElement(using = 'css selector', value = '#ctl00_MainContent_ComboBoxMapName .rcbActionButton')
Sys.sleep(1)
maphl$clickElement()
sthl <- remDr$findElement(using = 'xpath', value = "//*[contains(concat( ' ', @class, ' ' ), concat( ' ', 'rcbItem', ' ' )) and (((count(preceding-sibling::*) + 1) = 12) and parent::*)]//label")
Sys.sleep(1)
sthl$clickElement()
ishl <- remDr$findElement(using = 'xpath', value = "//*[contains(concat( ' ', @class, ' ' ), concat( ' ', 'rcbItem', ' ' )) and (((count(preceding-sibling::*) + 1) = 9) and parent::*)]//label")
Sys.sleep(1)
ishl$clickElement()
maphl$clickElement()
Sys.sleep(20)

doc <- htmlParse(remDr$getPageSource()[[1]])
test <- readHTMLTable(doc)
stminus <- as.data.frame(test[3])
stminus$Map.Name <- 'Sky Temple'
stminus <- stminus[,c(2:9,11)]
colnames(stminus) <- c('hero', 'Games.Played', 'Games.Banned', 'Popularity', 'Win.Percent', 'Delta', 'Role', 'Specialty',
                       'Map.Name')
Sys.sleep(1)

    # Tomb of the Spider Queen - Bronze+
maphl <- remDr$findElement(using = 'css selector', value = '#ctl00_MainContent_ComboBoxMapName .rcbActionButton')
Sys.sleep(1)
maphl$clickElement()
tosqhl <- remDr$findElement(using = 'xpath', value = "//*[contains(concat( ' ', @class, ' ' ), concat( ' ', 'rcbItem', ' ' )) and (((count(preceding-sibling::*) + 1) = 13) and parent::*)]//label")
Sys.sleep(1)
tosqhl$clickElement()
sthl <- remDr$findElement(using = 'xpath', value = "//*[contains(concat( ' ', @class, ' ' ), concat( ' ', 'rcbItem', ' ' )) and (((count(preceding-sibling::*) + 1) = 12) and parent::*)]//label")
Sys.sleep(1)
sthl$clickElement()
maphl$clickElement()
Sys.sleep(20)

doc <- htmlParse(remDr$getPageSource()[[1]])
test <- readHTMLTable(doc)
tosqminus <- as.data.frame(test[3])
tosqminus$Map.Name <- 'Tomb of the Spider Queen'
tosqminus <- tosqminus[,c(2:9,11)]
colnames(tosqminus) <- c('hero', 'Games.Played', 'Games.Banned', 'Popularity', 'Win.Percent', 'Delta', 'Role', 'Specialty',
                         'Map.Name')
Sys.sleep(1)

    # Towers of Doom - Bronze+
maphl <- remDr$findElement(using = 'css selector', value = '#ctl00_MainContent_ComboBoxMapName .rcbActionButton')
Sys.sleep(1)
maphl$clickElement()
todhl <- remDr$findElement(using = 'xpath', value = "//*[contains(concat( ' ', @class, ' ' ), concat( ' ', 'rcbItem', ' ' )) and (((count(preceding-sibling::*) + 1) = 14) and parent::*)]//label")
Sys.sleep(1)
todhl$clickElement()
tosqhl <- remDr$findElement(using = 'xpath', value = "//*[contains(concat( ' ', @class, ' ' ), concat( ' ', 'rcbItem', ' ' )) and (((count(preceding-sibling::*) + 1) = 13) and parent::*)]//label")
Sys.sleep(1)
tosqhl$clickElement()
maphl$clickElement()
Sys.sleep(20)

doc <- htmlParse(remDr$getPageSource()[[1]])
test <- readHTMLTable(doc)
todminus <- as.data.frame(test[3])
todminus$Map.Name <- 'Towers of Doom'
todminus <- todminus[,c(2:9,11)]
colnames(todminus) <- c('hero', 'Games.Played', 'Games.Banned', 'Popularity', 'Win.Percent', 'Delta', 'Role', 'Specialty',
                        'Map.Name')
Sys.sleep(1)

    # Warhead Junction - Bronze+
maphl <- remDr$findElement(using = 'css selector', value = '#ctl00_MainContent_ComboBoxMapName .rcbActionButton')
Sys.sleep(1)
maphl$clickElement()
wjhl <- remDr$findElement(using = 'xpath', value = "//*[contains(concat( ' ', @class, ' ' ), concat( ' ', 'rcbItem', ' ' )) and (((count(preceding-sibling::*) + 1) = 15) and parent::*)]//label")
Sys.sleep(1)
wjhl$clickElement()
todhl <- remDr$findElement(using = 'xpath', value = "//*[contains(concat( ' ', @class, ' ' ), concat( ' ', 'rcbItem', ' ' )) and (((count(preceding-sibling::*) + 1) = 14) and parent::*)]//label")
Sys.sleep(1)
todhl$clickElement()
maphl$clickElement()
Sys.sleep(20)

doc <- htmlParse(remDr$getPageSource()[[1]])
test <- readHTMLTable(doc)
wjminus <- as.data.frame(test[3])
wjminus$Map.Name <- 'Warhead Junction'
wjminus <- wjminus[,c(2:9,11)]
colnames(wjminus) <- c('hero', 'Games.Played', 'Games.Banned', 'Popularity', 'Win.Percent', 'Delta', 'Role', 'Specialty',
                       'Map.Name')
Sys.sleep(1)

    # Close browser window
remDr$close()


# Standard Error function ----
se <- function(x) sqrt(var(x)/length(x))

# Plotting -----
# Higher level graphs (Platinum, Diamond, Master)
    # Set list of data frames
mapsplus <- list(bh, bhb, boe, ch, ds, got, hm, is, st, tod, tosq, wj)

    # Set working directory to Dropbox folder
setwd('C://Users//mattd//Dropbox//HotS//Hero WR x Map.Name//geom_bar')

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
  
  x <- x %>% 
    rowwise() %>% 
    mutate(stderror = se(rnorm(n = as.numeric(as.character(Games.Played)), mean = as.numeric(as.character(gsub('%','',Win.Percent))))))
  
  ann_text <- data.frame(Win.Percent = .85, hero = 'Varian', Role = factor('Assassin', levels = c('Assassin', 'Specialist', 'Support', 'Warrior')), y = x$avg.winrate[x$hero == 'Varian'], z = x$Win.Percent[x$hero == "Varian"], games = x$Games.Played[x$hero == "Varian"], stderror = x$stderror[x$hero == 'Varian'])
  
  ann_line <- data.frame(hero = 'Varian', x = 'Varian', xend = 'Varian', Role = factor('Assassin', levels = c('Assassin', 'Specialist', 'Support', 'Warrior')), y = x$Win.Percent[x$hero == 'Varian'], yend = .85, Win.Percent = x$Win.Percent[x$hero=='Varian'], stderror = x$stderror[x$hero == 'Varian'])

    # Order data
  x <- x %>%
    arrange(Role, Win.Percent)
  x$hero <- factor(x$hero, levels = x$hero)
  x$group <- rep(1:2, length = length(x$hero), times = length(x$hero)/2)
  
    # Plot
  plot <- ggplot(data = x, aes(x = hero, y = Win.Percent)) +
    #geom_bar(aes(alpha = Games.Played), stat = 'identity', width = 0.5, color = 'grey75') +
    geom_errorbar(aes(ymin = Win.Percent - (2*stderror), ymax = Win.Percent + (2*stderror), color = Role, alpha = group),size = 1, show.legend = FALSE) +
    geom_point(aes(x = hero, y = Win.Percent, shape = Map.Name, color = Role, alpha = group), size = 1, pch = 3, stroke = 1) +
    geom_point(aes(x = hero, y = avg.winrate, shape = Map.Name), size = 1) +
    geom_hline(yintercept = .5, alpha = .75, lty = 2) +
    coord_flip(ylim = c(0,1)) +
    theme(legend.position = 'none') +
    scale_y_continuous(breaks = c(seq(0,1,.1)),
                       expand = c(0,0),
                       limits = c(-100,100),
                       labels = scales::percent, 
                       sec.axis = dup_axis()) +
    facet_grid(Role ~ ., scales = "free", space = "free") +
    scale_alpha_continuous(range = c(0.45,1), guide = FALSE) +
    theme_fivethirtyeight() +
    #scale_fill_discrete('Map Hero Win Rate') +
    scale_color_discrete('Hero Win Rate on Map') +
    scale_shape_discrete('Hero Win Rate Overall', labels = c('')) +
    labs(title = paste0('Hero league win rate on ', x$Map.Name),
         subtitle = paste0('Hero league win rate across Platinum, Diamond, and Master leagues for the last 7 days.\nLast update: ', 
                           Sys.time(), 
                           ' CST.'),
         caption = '@ggplots                                                                                                                   Source: HOTS LOGS') +
    theme(axis.title = element_text(face = 'bold')) +
    xlab('') +
    ylab('Win Rate') +
    geom_segment(data = ann_line, aes(x = ann_line$x, xend = ann_line$xend, y = ann_line$y + (2.5*ann_line$stderror), yend = ann_line$yend )) +
    geom_label(data = ann_text, label = paste0("There is a 95% certainty\n",
                                               "Varian's win rate on\n",
                                               x$Map.Name[1],
                                               "\nis between\n",
                                               scales::percent(round(ann_text$z - (2*ann_text$stderror),3)),
                                               " and ",
                                               scales::percent(round(ann_text$z + (2*ann_text$stderror),3))), fill = '#F0F0F0', size = 2.5) +
    theme(legend.position = 'bottom',
          legend.box = 'vertical',
          legend.key = element_rect(colour = 'grey75', size = .5, linetype = 'solid'),
          axis.title = element_text(face = 'bold'),
          legend.spacing = unit(.05, 'line'))
  
  print(plot)
  dev.off()
})

# Lower level graphs (Bronze, Silver, Gold)
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
  
  x <- x %>% 
    rowwise() %>% 
    mutate(stderror = se(rnorm(n = as.numeric(as.character(Games.Played)), mean = as.numeric(as.character(gsub('%','',Win.Percent))))))
  
  ann_text <- data.frame(Win.Percent = .85, hero = "Gul'dan", Role = factor('Assassin', levels = c('Assassin', 'Specialist', 'Support', 'Warrior')), y = x$avg.winrate[x$hero == "Gul'dan"], z = x$Win.Percent[x$hero == "Gul'dan"], games = x$Games.Played[x$hero == "Gul'dan"], stderror = x$stderror[x$hero == "Gul'dan"])
  
  ann_line <- data.frame(hero = "Gul'dan", x = "Gul'dan", xend = "Gul'dan", Role = factor('Assassin', levels = c('Assassin', 'Specialist', 'Support', 'Warrior')), y = x$Win.Percent[x$hero == "Gul'dan"], yend = .85, Win.Percent = x$Win.Percent[x$hero=="Gul'dan"], stderror = x$stderror[x$hero == "Gul'dan"])
  
    # Order data
  x <- x %>%
    arrange(Role, Win.Percent)
  x$hero <- factor(x$hero, levels = x$hero)
  x$group <- rep(1:2, length = length(x$hero), times = length(x$hero)/2)
  
    # Plot
  plot <- ggplot(data = x, aes(x = hero, y = Win.Percent)) +
    #geom_bar(aes(alpha = Games.Played), stat = 'identity', width = 0.5, color = 'grey75') +
    geom_errorbar(aes(ymin = Win.Percent - (2*stderror), ymax = Win.Percent + (2*stderror), color = Role, alpha = group),size = 1, show.legend = FALSE) +
    geom_point(aes(x = hero, y = Win.Percent, shape = Map.Name, color = Role, alpha = group), size = 1, pch = 3, stroke = 1) +
    geom_point(aes(x = hero, y = avg.winrate, shape = Map.Name), size = 1) +
    geom_hline(yintercept = .5, alpha = .75, lty = 2) +
    coord_flip(ylim = c(0,1)) +
    theme(legend.position = 'none') +
    scale_y_continuous(breaks = c(seq(0,1,.1)),
                       expand = c(0,0),
                       limits = c(-100,100),
                       labels = scales::percent, 
                       sec.axis = dup_axis()) +
    facet_grid(Role ~ ., scales = "free", space = "free") +
    scale_alpha_continuous(range = c(0.45,1), guide = FALSE) +
    theme_fivethirtyeight() +
    #scale_fill_discrete('Map Hero Win Rate') +
    scale_color_discrete('Hero Win Rate on Map') +
    scale_shape_discrete('Hero Win Rate Overall', labels = c('')) +
    labs(title = paste0('Hero league win rate on ', x$Map.Name),
         subtitle = paste0('Hero league win rate across Bronze, Silver, and Gold leagues for the last 7 days.\nLast update: ', 
                           Sys.time(), 
                           ' CST.'),
         caption = '@ggplots                                                                                                                   Source: HOTS LOGS') +
    theme(axis.title = element_text(face = 'bold')) +
    xlab('') +
    ylab('Win Rate') +
    geom_segment(data = ann_line, aes(x = ann_line$x, xend = ann_line$xend, y = ann_line$y + (2.5*ann_line$stderror), yend = ann_line$yend )) +
    geom_label(data = ann_text, label = paste0("There is a 95% certainty\n",
                                               "Gul'dan's win rate on\n",
                                               x$Map.Name[1],
                                               "\nis between\n",
                                               scales::percent(round(ann_text$z - (2*ann_text$stderror),3)),
                                               " and ",
                                               scales::percent(round(ann_text$z + (2*ann_text$stderror),3))), fill = '#F0F0F0', size = 2.5) +
    theme(legend.position = 'bottom',
          legend.box = 'vertical',
          legend.key = element_rect(colour = 'grey75', size = .5, linetype = 'solid'),
          axis.title = element_text(face = 'bold'),
          legend.spacing = unit(.05, 'line'))
  
  print(plot)
  dev.off()
})
