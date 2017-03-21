library(RSelenium)
library(XML)
library(httr)

# Higher level data (Platinum, Diamond, Master)---------

# Set up and open phantomjs browser
psPath <- 'C:/Users/mattd/Desktop/phantomjs-2.1.1-windows'
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

#Screen shot
remDr$screenshot(display = TRUE)

# Repeat process above for lower leagues -----------

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

# Blackheart's Bay - Plat+
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
bhminus <- as.data.frame(test[3])
bhminus$Map.Name <- 'Braxis Holdout'
bhminus <- bhminus[,c(2:9,11)]
colnames(bhminus) <- c('hero', 'Games.Played', 'Games.Banned', 'Popularity', 'Win.Percent', 'Delta', 'Role', 'Specialty',
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
Sys.sleep(20)

doc <- htmlParse(remDr$getPageSource()[[1]])
test <- readHTMLTable(doc)
chminus <- as.data.frame(test[3])
chminus$Map.Name <- 'Cursed Hollow'
chminus <- chminus[,c(2:9,11)]
colnames(chminus) <- c('hero', 'Games.Played', 'Games.Banned', 'Popularity', 'Win.Percent', 'Delta', 'Role', 'Specialty',
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
dsminus <- as.data.frame(test[3])
dsminus$Map.Name <- 'Dragon Shire'
dsminus <- dsminus[,c(2:9,11)]
colnames(dsminus) <- c('hero', 'Games.Played', 'Games.Banned', 'Popularity', 'Win.Percent', 'Delta', 'Role', 'Specialty',
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
gotminus <- as.data.frame(test[3])
gotminus$Map.Name <- 'Garden of Terror'
gotminus <- gotminus[,c(2:9,11)]
colnames(gotminus) <- c('hero', 'Games.Played', 'Games.Banned', 'Popularity', 'Win.Percent', 'Delta', 'Role', 'Specialty',
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
hmminus <- as.data.frame(test[3])
hmminus$Map.Name <- 'Haunted Mines'
hmminus <- hmminus[,c(2:9,11)]
colnames(hmminus) <- c('hero', 'Games.Played', 'Games.Banned', 'Popularity', 'Win.Percent', 'Delta', 'Role', 'Specialty',
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
isminus <- as.data.frame(test[3])
isminus$Map.Name <- 'Infernal Shrines'
isminus <- isminus[,c(2:9,11)]
colnames(isminus) <- c('hero', 'Games.Played', 'Games.Banned', 'Popularity', 'Win.Percent', 'Delta', 'Role', 'Specialty',
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
Sys.sleep(20)

doc <- htmlParse(remDr$getPageSource()[[1]])
test <- readHTMLTable(doc)
stminus <- as.data.frame(test[3])
stminus$Map.Name <- 'Sky Temple'
stminus <- stminus[,c(2:9,11)]
colnames(stminus) <- c('hero', 'Games.Played', 'Games.Banned', 'Popularity', 'Win.Percent', 'Delta', 'Role', 'Specialty',
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
tosqminus <- as.data.frame(test[3])
tosqminus$Map.Name <- 'Tomb of the Spider Queen'
tosqminus <- tosqminus[,c(2:9,11)]
colnames(tosqminus) <- c('hero', 'Games.Played', 'Games.Banned', 'Popularity', 'Win.Percent', 'Delta', 'Role', 'Specialty',
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
todminus <- as.data.frame(test[3])
todminus$Map.Name <- 'Towers of Doom'
todminus <- todminus[,c(2:9,11)]
colnames(todminus) <- c('hero', 'Games.Played', 'Games.Banned', 'Popularity', 'Win.Percent', 'Delta', 'Role', 'Specialty',
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
wjminus <- as.data.frame(test[3])
wjminus$Map.Name <- 'Warhead Junction'
wjminus <- wjminus[,c(2:9,11)]
colnames(wjminus) <- c('hero', 'Games.Played', 'Games.Banned', 'Popularity', 'Win.Percent', 'Delta', 'Role', 'Specialty',
                  'Map.Name')
Sys.sleep(1)

# Close browser window
remDr$close()
