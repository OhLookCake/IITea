library(ggplot2)
library(jpeg)
library(grid)
library(RColorBrewer)
library(stringr)

setwd("C:/Projects/Data/Insight Datagiri/IITea")
img <- readJPEG("images/Map_Google_Grayscale.jpg")
bgimg <- rasterGrob(img, interpolate=TRUE)

df <- read.csv("data/placelist.csv", header=TRUE, colClasses=c("character", "character",rep("numeric", 29)))

# Exploratory Stuff
baseplot <- ggplot(data = df, aes(x = X+OffsetX, y = Y+OffsetY)) +
  annotation_custom(bgimg, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf) +
  xlim(0,2000) +
  ylim(0,2000) +
  theme(axis.text.y = element_blank(), axis.text.x = element_blank(),
        axis.title = element_blank(), axis.title.x = element_blank())
 
baseplot + geom_point(color="#EF8000", size=8, shape=20, alpha=0.7)
ggsave("AllStalls.jpg", path = "output",
       scale = 1, width = 25.3, height = 30, units ="cm", dpi = 150) 

# baseplot +
#   geom_point(aes(color="#4466DD"), size=48, shape=20, alpha=0.3, show_guide=FALSE) +
#   geom_point(aes(color="#e22233"), size=24, shape=20, alpha=0.4, show_guide=TRUE) +
#   scale_color_identity(name = 'Distance Walked in', guide = 'legend',labels = c('2 minutes', '1 minute')) +
#   theme(legend.justification=c(0,0), legend.position=c(0,0)) +
#   guides(col = guide_legend(reverse = TRUE))
# 



# According to opening time
startHour <- 8
ctr <- 1
for(hour in (startHour:(startHour+23)%%24)){
  currentlyOpen <- df[ ,paste0("X",hour)] > 0
  clockHour <- ((hour-1)%%12) + 1
    
  clockTime <- paste0(ifelse(clockHour < 10, "0", ""),
                      clockHour,
                      " ",
                      ifelse(hour<12, "am", "pm"))
  
  
  if(sum(currentlyOpen) > 0){
    dfHour <- df[currentlyOpen, ]
  } else {
    dfHour <- df[nrow(df), ]  #dummy row, with off-world coordinates
  }
    
  baseplotHour <- ggplot(data = dfHour, aes(x = X, y = Y)) +
    annotation_custom(bgimg, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf) +
    xlim(0,2000) +
    ylim(0,2000) +
    theme(axis.text.y = element_blank(), axis.text.x = element_blank(),
          axis.title = element_blank(), axis.title.x = element_blank())
  
  positionsplot <- baseplotHour +
    geom_point(aes(color="#4466DD"), size=70, shape=20, alpha=0.3, show_guide=FALSE) +
    geom_point(aes(color="#e22233"), size=35, shape=20, alpha=0.4, show_guide=TRUE) +
    scale_color_identity(name = 'Distance Walked in', guide = 'legend',labels = c('2 minutes', '1 minute')) +
    theme(legend.justification=c(0,0), legend.position=c(0,0)) +
    theme(legend.text = element_text(size = 18), legend.title = element_text(size = 18)) +
    guides(col = guide_legend(reverse = TRUE)) +
    annotate("text", x = 120, y = 640, label = clockTime, colour = "#5555aa", size=14)
  
  
  if(sum(currentlyOpen) > 0){
    positionsplot +
      geom_text(aes(label=Text,  x=X + OffsetX, y=Y + OffsetY), size=5, hjust=0.5, vjust=0.35)
      # This will not plot on the screen, because it's not in the global scope. If needed, use print() to draw
  } else {
    positionsplot
  }
  
  #ggsave(paste0("map", ctr, "_", hour*100, ".jpg"), path = "output", 
  #       scale = 1, width = 25.3, height = 30, units ="cm", dpi = 300) 
  
  ggsave(paste0("map", str_pad(ctr, 2, "left", "0"), ".bmp"), path = "output",
         scale = 1, width = 25.3, height = 30, units ="cm", dpi = 150) 

  ggsave(paste0("map", str_pad(ctr, 2, "left", "0"), ".png"), path = "output",
         scale = 1, width = 25.3, height = 30, units ="cm", dpi = 150) 
  
    
  ctr <- ctr + 1
}




