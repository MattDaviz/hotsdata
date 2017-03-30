# Load Libraries ----
library(jpeg); library(png); library(ggplot2); library(tidyr); library(dplyr); library(grid); library(ggrepel); library(ggthemes)
library(extrafont)

# Extra data tidying ----
plat.plus <- rbind(bh, bhb, boe, ch, ds, got, hm, is, st, tod, tosq, wj)
plat.plus$division <- 'Plat+'
bronze.plus <- rbind(bhminus, bhbminus, boeminus, chminus, dsminus, gotminus, hmminus, isminus, stminus, todminus, tosqminus, wjminus)
bronze.plus$division <- 'Bronze+'
all.division.win.rates <- rbind(plat.plus, bronze.plus)

setwd('C:\\Users\\mattd\\Dropbox\\HotS\\hotsdata')

# Infographic Loop ----
    # Set list of data heroes to include
heroes.small <- list('Alarak', "Anub'arak", 'Artanis', 'Auriel', 'Brightwing', 'Chromie', 'Cho', 'Diablo', 'E.T.C.', 'Falstad', 'Greymane',
                     "Gul'dan", 'Illidan', 'Jaina','Johanna', 'Kharazim', 'Leoric', 'Li Li', 'Li-Ming', 'Lt. Morales', 'Lúcio', 
                     'Lunara', 'Malfurion',  'Medivh', 'Murky', 'Nazeebo','Nova', 'Probius', 'Ragnaros', 'Raynor', 'Rehgar', 'Rexxar',
                     'Samuro', 'Sylvanas', 'Tassadar', 'Thrall', 'Tracer', 'Tychus', 'Uther', 'Valeera', 'Valla', 'Varian',
                     'Xul', 'Zagara', 'Zarya', "Zul'jin")

    # Load background images
bg.img <- readJPEG('.\\hero images\\_Base.jpg')
bg.raster <- rasterGrob(bg.img, interpolate = TRUE)

icon.img <- readPNG('.\\hero images\\_Icon.png')
icon.raster <- rasterGrob(icon.img, interpolate = TRUE)

    # Loop for battleground win rate infographic per hero
lapply(heroes.small, function(x) {
  
        # Create image files to use in plot
  hero.img <- readPNG(paste0('.\\hero images\\', x, '.png'))
  hero.raster <- rasterGrob(hero.img, interpolate = TRUE)
  
        # Create unique output filename
  output_filename <- paste0(x, ' Battleground Win Rates.jpeg')
  
        # Open the file for the plot to be written to
  jpeg(output_filename, height = 2400, width = 3000, res = 300, quality = 400)

        # Plot
  y <- plat.plus %>% 
    left_join(overallplus[,c('hero', 'avg.winrate')], by = 'hero') %>% #overallplus needs to be scraped from 'HotS Logs scraper.R'
    filter(hero == x) %>% 
    mutate(Win.Percent = as.numeric(as.character(gsub('%', '', Win.Percent))) * .01,
           Games.Played = as.numeric(as.character(Games.Played)),
           Games.Banned = as.numeric(as.character(Games.Banned)),
           Popularity = as.numeric(as.character(gsub('%', '', Popularity))) * .01,
           Delta = as.numeric(as.character(gsub('%', '', Delta))) * .01,
           avg.winrate = as.numeric(as.character(gsub('%', '', avg.winrate))) * .01,
           hero = as.factor(hero),
           difference = Win.Percent - avg.winrate,
           Map.Name = as.factor(Map.Name)) %>% 
    select(Map.Name, Win.Percent, avg.winrate, Games.Played, Games.Banned, Popularity, Delta, difference, hero) %>% 
    arrange(difference) %>% 
    mutate(Map.Name = factor(Map.Name, levels = Map.Name))
  
  plot <- ggplot() +
    annotation_custom(bg.raster, xmin = -2.3, xmax = 2, ymin = -2, ymax = Inf) +
    annotation_custom(hero.raster, xmin = -1.32, xmax = Inf, ymin = 1, ymax = 14) + 
    annotation_custom(icon.raster, xmin = -1.71, xmax = Inf, ymin = 16, ymax = 19) +
    annotate('text', label = paste0(toupper(x), ' BATTLEGROUND WIN RATES'), x = -.65, y = 17.5, family = 'Exo', size = 10, color = 'white', fontface = 'bold', hjust = 0) +
    geom_segment(aes(x = 0, xend = 0, y = 1.5, yend = 13.75), color = 'darkgoldenrod2', size = 1) +
    geom_segment(data = y, aes(x = difference, xend = 0, y = Map.Name, yend = Map.Name), color = ifelse(y$difference > 0, 
                                                                                                        'green4', 'red4'),
                 size = 1.25) +
    geom_text(data = y[which(y$difference <= 0),], aes(x = .01, y = Map.Name, label = Map.Name), family = 'Exo', fontface = 'bold',
              hjust = 0, color = 'grey75', size = 4) +
    geom_text(data = y[which(y$difference > 0),], aes(x = -.01, y = Map.Name, label = Map.Name), fontface = 'bold', color = 'grey75',
              family = 'Exo', hjust = 1, size = 4) +
    geom_text(data = y, aes(x = ifelse(difference > 0, difference + .04, difference - .04), y = Map.Name, 
                            label = scales::percent(round(difference,3))), family = 'Exo', fontface = 'bold', color = 'white', size = 4) +
    #geom_point(data = y, aes(x = difference, y = Map.Name), color = ifelse(y$difference > 0, 'green4', 'red4'), size = 3) +
    geom_point(data = y[which(y$difference > 0),], aes(x = difference, y = Map.Name, size = Games.Played), color = 'green4', 
               show.legend = FALSE) +
    geom_point(data = y[which(y$difference <= 0),], aes(x = difference, y = Map.Name, size = Games.Played), color = 'red4', 
               show.legend = FALSE) +
    annotate('point', x = 0, y = 15, size = 30, pch = 21, color = 'darkgoldenrod2', stroke = 4) +
    annotate('text', label = scales::percent(unique(y$avg.winrate)), x = 0, y = 15, size = 6, color = 'white', family = 'Exo', fontface = 'bold') +
    theme_fivethirtyeight() +
    scale_x_continuous(limits = c(-.8, .2)) +
    scale_y_discrete(limits = c('', levels(y$Map.Name), '', '', '', '', '', '')) +
    annotate('text', label = paste0('@ggplots                                                                                                                                        Source: HOTS LOGS | Date: ', Sys.Date()), x = -.775, y = 1, color = 'grey75', hjust = 0, alpha = 0.5) + 
    theme(axis.text = element_blank(),
          panel.grid = element_blank(),
          plot.background = element_blank(),
          panel.background = element_blank(),
          plot.margin=unit(c(0,0,-1,-10), "mm"))
    
  
        # Print plot and output to working directory
  print(plot)
  dev.off()
})
  
# Function to plot heroes with images that are too large
heroes.large <- list('Abathur', 'Arthas', 'Azmodan', 'Chen', 'Dehaka',  'Gazlowe', "Kael'thas",'Kerrigan', 'Muradin', 'Sgt. Hammer', 'Sonya',
                     'Stitches', 'The Lost Vikings', 'The Butcher', 'Tyrael', 'Tyrande', 'Zeratul')

# Load background images
bg.img <- readJPEG('.\\hero images\\_Base.jpg')
bg.raster <- rasterGrob(bg.img, interpolate = TRUE)

icon.img <- readPNG('.\\hero images\\_Icon.png')
icon.raster <- rasterGrob(icon.img, interpolate = TRUE)

# Loop for battleground win rate infographic per hero
lapply(heroes.large, function(x) {
  
  # Create image files to use in plot
  hero.img <- readPNG(paste0('.\\hero images\\', x, '.png'))
  hero.raster <- rasterGrob(hero.img, interpolate = TRUE)
  
  # Create unique output filename
  output_filename <- paste0(x, ' Battleground Win Rates.jpeg')
  
  # Open the file for the plot to be written to
  jpeg(output_filename, height = 2400, width = 3000, res = 300, quality = 400)
  
  # Plot
  y <- plat.plus %>% 
    left_join(overallplus[,c('hero', 'avg.winrate')], by = 'hero') %>% # overallplus needs to be scraped from 'HotS Logs scraper.R'
    filter(hero == x) %>% 
    mutate(Win.Percent = as.numeric(as.character(gsub('%', '', Win.Percent))) * .01,
           Games.Played = as.numeric(as.character(Games.Played)),
           Games.Banned = as.numeric(as.character(Games.Banned)),
           Popularity = as.numeric(as.character(gsub('%', '', Popularity))) * .01,
           Delta = as.numeric(as.character(gsub('%', '', Delta))) * .01,
           avg.winrate = as.numeric(as.character(gsub('%', '', avg.winrate))) * .01,
           hero = as.factor(hero),
           difference = Win.Percent - avg.winrate,
           Map.Name = as.factor(Map.Name)) %>% 
    select(Map.Name, Win.Percent, avg.winrate, Games.Played, Games.Banned, Popularity, Delta, difference, hero) %>% 
    arrange(difference) %>% 
    mutate(Map.Name = factor(Map.Name, levels = Map.Name))
  
  plot <- ggplot() +
    annotation_custom(bg.raster, xmin = -2.3, xmax = 2, ymin = -2, ymax = Inf) +
    annotation_custom(hero.raster, xmin = -1.2, xmax = Inf, ymin = 3, ymax = 13) +
    annotation_custom(icon.raster, xmin = -1.71, xmax = Inf, ymin = 16, ymax = 19) +
    annotate('text', label = paste0(toupper(x), ' BATTLEGROUND WIN RATES'), x = -.65, y = 17.5, family = 'Exo', size = 10, color = 'white', fontface = 'bold', hjust = 0) +
    geom_segment(aes(x = 0, xend = 0, y = 1.5, yend = 13.75), color = 'darkgoldenrod2', size = 1) +
    geom_segment(data = y, aes(x = difference, xend = 0, y = Map.Name, yend = Map.Name), color = ifelse(y$difference > 0, 
                                                                                                        'green4', 'red4'),
                 size = 1.25) +
    geom_text(data = y[which(y$difference <= 0),], aes(x = .01, y = Map.Name, label = Map.Name), family = 'Exo', fontface = 'bold',
              hjust = 0, color = 'grey75', size = 4) +
    geom_text(data = y[which(y$difference > 0),], aes(x = -.01, y = Map.Name, label = Map.Name), fontface = 'bold', color = 'grey75',
              family = 'Exo', hjust = 1, size = 4) +
    geom_text(data = y, aes(x = ifelse(difference > 0, difference + .04, difference - .04), y = Map.Name, 
                            label = scales::percent(round(difference,3))), family = 'Exo', fontface = 'bold', color = 'white', size = 4) +
    #geom_point(data = y, aes(x = difference, y = Map.Name), color = ifelse(y$difference > 0, 'green4', 'red4'), size = 3) +
    geom_point(data = y[which(y$difference > 0),], aes(x = difference, y = Map.Name, size = Games.Played), color = 'green4',
               show.legend = FALSE) +
    geom_point(data = y[which(y$difference <= 0),], aes(x = difference, y = Map.Name, size = Games.Played), color = 'red4', 
               show.legend = FALSE) +
    annotate('point', x = 0, y = 15, size = 30, pch = 21, color = 'darkgoldenrod2', stroke = 4) +
    annotate('text', label = scales::percent(unique(y$avg.winrate)), x = 0, y = 15, size = 6, color = 'white', family = 'Exo', fontface = 'bold') +
    theme_fivethirtyeight() +
    scale_x_continuous(limits = c(-.8, .2)) +
    scale_y_discrete(limits = c('', levels(y$Map.Name), '', '', '', '', '', '')) +
    annotate('text', label = paste0('@ggplots                                                                                                                                        Source: HOTS LOGS | Date: ', Sys.Date()), x = -.775, y = 1, color = 'grey75', hjust = 0, alpha = 0.5) + 
    theme(axis.text = element_blank(),
          panel.grid = element_blank(),
          plot.background = element_blank(),
          panel.background = element_blank(),
          plot.margin=unit(c(0,0,-1,-10), "mm"))
  
  
  # Print plot and output to working directory
  print(plot)
  dev.off()
})
