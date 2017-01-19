library(RSelenium)
library(XML)
library(httr)

# Set up and open phantomjs browser
pJS <- phantom()
remDr <- remoteDriver(browserName = 'phantomjs')
remDr$open(silent = T)

# Set url to navigate to
url <- 'https://www.hotslogs.com/Sitewide/HeroAndMapStatistics'
remDr$navigate(url)
Sys.sleep(1)

# Check Platinum, Diamond, and Masters
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

#doc <- htmlParse(remDr$getPageSource()[[1]])
#test <- readHTMLTable(doc)
#overallplus <- as.data.frame(test[3])

# Battlefield of Eternity - Plat+
maphl <- remDr$findElement(using = 'css selector', value = '#ctl00_MainContent_ComboBoxMapName .rcbActionButton')
Sys.sleep(1)
maphl$clickElement()
boehl <- remDr$findElement(using = 'xpath', value = "//*[contains(concat( ' ', @class, ' ' ), concat( ' ', 'rcbItem', ' ' ))]//label")
Sys.sleep(1)
boehl$clickElement()
maphl <- remDr$findElement(using = 'css selector', value = '#ctl00_MainContent_ComboBoxMapName .rcbActionButton')
Sys.sleep(1)
maphl$clickElement()

#doc <- htmlParse(remDr$getPageSource()[[1]])
#test <- readHTMLTable(doc)
#boe <- as.data.frame(test[3])

# Blackheart's Bay - Plat+
maphl <- remDr$findElement(using = 'css selector', value = '#ctl00_MainContent_ComboBoxMapName .rcbActionButton')
Sys.sleep(1)
maphl$clickElement()
bhbhl <- remDr$findElement(using = 'xpath', value = "//*[contains(concat( ' ', @class, ' ' ), concat( ' ', 'rcbItem', ' ' ))]//label")
Sys.sleep(1)
bhbhl$clickElement()
boehl <- remDr$findElement(using = 'xpath', value = "//*[contains(concat( ' ', @class, ' ' ), concat( ' ', 'rcbItem', ' ' )) and (((count(preceding-sibling::*) + 1) = 1) and parent::*)]//label")
Sys.sleep(1)
boehl$clickElement()
maphl <- remDr$findElement(using = 'css selector', value = '#ctl00_MainContent_ComboBoxMapName .rcbActionButton')
maphl$clickElement()

#doc <- htmlParse(remDr$getPageSource()[[1]])
#test <- readHTMLTable(doc)
#bhb <- as.data.frame(test[3])

# Braxis Holdout - Plat+
maphl <- remDr$findElement(using = 'css selector', value = '#ctl00_MainContent_ComboBoxMapName .rcbActionButton')
maphl$clickElement()
bhhl <- remDr$findElement(using = 'xpath', value = "//*[contains(concat( ' ', @class, ' ' ), concat( ' ', 'rcbItem', ' ' )) and (((count(preceding-sibling::*) + 1) = 3) and parent::*)]//label")
bhhl$clickElement()
bhbhl <- remDr$findElement(using = 'xpath', value = "//*[contains(concat( ' ', @class, ' ' ), concat( ' ', 'rcbItem', ' ' )) and (((count(preceding-sibling::*) + 1) = 2) and parent::*)]//label")
bhbhl$clickElement()
maphl$clickElement()

#doc <- htmlParse(remDr$getPageSource()[[1]])
#test <- readHTMLTable(doc)
#bh <- as.data.frame(test[3])

# Cursed Hollow - Plat+
maphl <- remDr$findElement(using = 'css selector', value = '#ctl00_MainContent_ComboBoxMapName .rcbActionButton')
maphl$clickElement()
chhl <- remDr$findElement(using = 'xpath', value = "//*[contains(concat( ' ', @class, ' ' ), concat( ' ', 'rcbItem', ' ' )) and (((count(preceding-sibling::*) + 1) = 4) and parent::*)]//label")
chhl$clickElement()
bhhl <- remDr$findElement(using = 'xpath', value = "//*[contains(concat( ' ', @class, ' ' ), concat( ' ', 'rcbItem', ' ' )) and (((count(preceding-sibling::*) + 1) = 3) and parent::*)]//label")
bhhl$clickElement()
maphl$clickElement()

#doc <- htmlParse(remDr$getPageSource()[[1]])
#test <- readHTMLTable(doc)
#ch <- as.data.frame(test[3])

# Dragon Shire - Plat+
maphl <- remDr$findElement(using = 'css selector', value = '#ctl00_MainContent_ComboBoxMapName .rcbActionButton')
maphl$clickElement()
dshl <- remDr$findElement(using = 'xpath', value = "//*[contains(concat( ' ', @class, ' ' ), concat( ' ', 'rcbItem', ' ' )) and (((count(preceding-sibling::*) + 1) = 5) and parent::*)]//label")
dshl$clickElement()
chhl <- remDr$findElement(using = 'xpath', value = "//*[contains(concat( ' ', @class, ' ' ), concat( ' ', 'rcbItem', ' ' )) and (((count(preceding-sibling::*) + 1) = 4) and parent::*)]//label")
chhl$clickElement()
maphl$clickElement()

#doc <- htmlParse(remDr$getPageSource()[[1]])
#test <- readHTMLTable(doc)
#ds <- as.data.frame(test[3])

# Garden of Terror - Plat+
maphl <- remDr$findElement(using = 'css selector', value = '#ctl00_MainContent_ComboBoxMapName .rcbActionButton')
maphl$clickElement()
gothl <- remDr$findElement(using = 'xpath', value = "//*[contains(concat( ' ', @class, ' ' ), concat( ' ', 'rcbItem', ' ' )) and (((count(preceding-sibling::*) + 1) = 6) and parent::*)]//label")
gothl$clickElement()
dshl <- remDr$findElement(using = 'xpath', value = "//*[contains(concat( ' ', @class, ' ' ), concat( ' ', 'rcbItem', ' ' )) and (((count(preceding-sibling::*) + 1) = 5) and parent::*)]//label")
dshl$clickElement()
maphl$clickElement()

#doc <- htmlParse(remDr$getPageSource()[[1]])
#test <- readHTMLTable(doc)
#got <- as.data.frame(test[3])

# Haunted Mines - Plat+
maphl <- remDr$findElement(using = 'css selector', value = '#ctl00_MainContent_ComboBoxMapName .rcbActionButton')
maphl$clickElement()
hmhl <- remDr$findElement(using = 'xpath', value = "//*[contains(concat( ' ', @class, ' ' ), concat( ' ', 'rcbItem', ' ' )) and (((count(preceding-sibling::*) + 1) = 7) and parent::*)]//label")
hmhl$clickElement()
gothl <- remDr$findElement(using = 'xpath', value = "//*[contains(concat( ' ', @class, ' ' ), concat( ' ', 'rcbItem', ' ' )) and (((count(preceding-sibling::*) + 1) = 6) and parent::*)]//label")
gothl$clickElement()
maphl$clickElement()

#doc <- htmlParse(remDr$getPageSource()[[1]])
#test <- readHTMLTable(doc)
#hm <- as.data.frame(test[3])

# Infernal Shrines - Plat+
maphl <- remDr$findElement(using = 'css selector', value = '#ctl00_MainContent_ComboBoxMapName .rcbActionButton')
maphl$clickElement()
ishl <- remDr$findElement(using = 'xpath', value = "//*[contains(concat( ' ', @class, ' ' ), concat( ' ', 'rcbItem', ' ' )) and (((count(preceding-sibling::*) + 1) = 8) and parent::*)]//label")
ishl$clickElement()
hmhl <- remDr$findElement(using = 'xpath', value = "//*[contains(concat( ' ', @class, ' ' ), concat( ' ', 'rcbItem', ' ' )) and (((count(preceding-sibling::*) + 1) = 7) and parent::*)]//label")
hmhl$clickElement()
maphl$clickElement()

#doc <- htmlParse(remDr$getPageSource()[[1]])
#test <- readHTMLTable(doc)
#is <- as.data.frame(test[3])

# Sky Temple - Plat+
maphl <- remDr$findElement(using = 'css selector', value = '#ctl00_MainContent_ComboBoxMapName .rcbActionButton')
maphl$clickElement()
sthl <- remDr$findElement(using = 'xpath', value = "//*[contains(concat( ' ', @class, ' ' ), concat( ' ', 'rcbItem', ' ' )) and (((count(preceding-sibling::*) + 1) = 11) and parent::*)]//label")
sthl$clickElement()
ishl <- remDr$findElement(using = 'xpath', value = "//*[contains(concat( ' ', @class, ' ' ), concat( ' ', 'rcbItem', ' ' )) and (((count(preceding-sibling::*) + 1) = 8) and parent::*)]//label")
ishl$clickElement()
maphl$clickElement()

#doc <- htmlParse(remDr$getPageSource()[[1]])
#test <- readHTMLTable(doc)
#st <- as.data.frame(test[3])

# Tomb of the Spider Queen - Plat+
maphl <- remDr$findElement(using = 'css selector', value = '#ctl00_MainContent_ComboBoxMapName .rcbActionButton')
maphl$clickElement()
tosqhl <- remDr$findElement(using = 'xpath', value = "//*[contains(concat( ' ', @class, ' ' ), concat( ' ', 'rcbItem', ' ' )) and (((count(preceding-sibling::*) + 1) = 12) and parent::*)]//label")
tosqhl$clickElement()
sthl <- remDr$findElement(using = 'xpath', value = "//*[contains(concat( ' ', @class, ' ' ), concat( ' ', 'rcbItem', ' ' )) and (((count(preceding-sibling::*) + 1) = 11) and parent::*)]//label")
sthl$clickElement()
maphl$clickElement()

#doc <- htmlParse(remDr$getPageSource()[[1]])
#test <- readHTMLTable(doc)
#tosq <- as.data.frame(test[3])

# Towers of Doom - Plat+
maphl <- remDr$findElement(using = 'css selector', value = '#ctl00_MainContent_ComboBoxMapName .rcbActionButton')
maphl$clickElement()
todhl <- remDr$findElement(using = 'xpath', value = "//*[contains(concat( ' ', @class, ' ' ), concat( ' ', 'rcbItem', ' ' )) and (((count(preceding-sibling::*) + 1) = 13) and parent::*)]//label")
todhl$clickElement()
tosqhl <- remDr$findElement(using = 'xpath', value = "//*[contains(concat( ' ', @class, ' ' ), concat( ' ', 'rcbItem', ' ' )) and (((count(preceding-sibling::*) + 1) = 12) and parent::*)]//label")
tosqhl$clickElement()
maphl$clickElement()

#doc <- htmlParse(remDr$getPageSource()[[1]])
#test <- readHTMLTable(doc)
#tod <- as.data.frame(test[3])

# Warhead Junction - Plat+
maphl <- remDr$findElement(using = 'css selector', value = '#ctl00_MainContent_ComboBoxMapName .rcbActionButton')
maphl$clickElement()
wjhl <- remDr$findElement(using = 'xpath', value = "//*[contains(concat( ' ', @class, ' ' ), concat( ' ', 'rcbItem', ' ' )) and (((count(preceding-sibling::*) + 1) = 14) and parent::*)]//label")
wjhl$clickElement()
todhl <- remDr$findElement(using = 'xpath', value = "//*[contains(concat( ' ', @class, ' ' ), concat( ' ', 'rcbItem', ' ' )) and (((count(preceding-sibling::*) + 1) = 13) and parent::*)]//label")
todhl$clickElement()
maphl$clickElement()

doc <- htmlParse(remDr$getPageSource()[[1]])
test <- readHTMLTable(doc)
wj <- as.data.frame(test[3])

#Screen shot
remDr$screenshot(display = TRUE)
#########