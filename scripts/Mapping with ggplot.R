library(ggplot2)
library(jpeg)
library(grid)
library(RColorBrewer)
library(stringr)

setwd("C:/Projects/Data/Insight Datagiri/IITea")
img <- readJPEG("images/Map_Google_Grayscale.jpg")
bgimg <- rasterGrob(img, interpolate=TRUE)

#df <- read.csv("data/placelist.csv", header=TRUE, colClasses=c("character", "character", "numeric", "numeric"))
df <- read.csv("data/megalist.csv", header=TRUE, colClasses=c("character", "character",rep("numeric", 29)))


# 
# baseplot <- ggplot(data = df, aes(x = X+OffsetX, y = Y+OffsetY)) +
#   annotation_custom(bgimg, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf) +
#   xlim(0,2000) +
#   ylim(0,2000) +
#   theme(axis.text.y = element_blank(), axis.text.x = element_blank(),
#         axis.title = element_blank(), axis.title.x = element_blank())
# 
# baseplot + geom_point() +
#   geom_text(aes(label = Text), size=3, hjust=-0.2, vjust=0.35)
# 
# 
# baseplot + geom_point(aes(color = Text)) +
#   theme(legend.position=c(.5, .5))
# 
# 
# 
# baseplot +
#   geom_point(aes(color="#4466DD"), size=48, shape=20, alpha=0.3, show_guide=FALSE) +
#   geom_point(aes(color="#e22233"), size=24, shape=20, alpha=0.4, show_guide=TRUE) +
#   scale_color_identity(name = 'Distance Walked in', guide = 'legend',labels = c('2 minutes', '1 minute')) +
#   theme(legend.justification=c(0,0), legend.position=c(0,0)) +
#   guides(col = guide_legend(reverse = TRUE))
# 



# According to opening time
startHour <- 6 
ctr <- 1
for(hour in (startHour:(startHour+23)%%24)){
  currentlyOpen <- df[ ,paste0("X",hour)] > 0
  clockHour <- ((hour-1)%%12) + 1
  clockTime <- paste0(ifelse(clockHour < 10, "0", ""),
                      clockHour,
                      " ",
                      ifelse(hour==0, "midnight",
                             ifelse(hour==12, "noon",
                                    ifelse(hour<12, "am", "pm"))))
  
  if(sum(currentlyOpen) > 0){
    dfHour <- df[currentlyOpen, ]
  } else {
    dfHour <- df[43, ]  #dummy row, with off-world coordinates
  }
    
  baseplotHour <- ggplot(data = dfHour, aes(x = X, y = Y)) +
    annotation_custom(bgimg, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf) +
    xlim(0,2000) +
    ylim(0,2000) +
    theme(axis.text.y = element_blank(), axis.text.x = element_blank(),
          axis.title = element_blank(), axis.title.x = element_blank())
  
  positionsplot <- baseplotHour +
    geom_point(aes(color="#4466DD"), size=48, shape=20, alpha=0.3, show_guide=FALSE) +
    geom_point(aes(color="#e22233"), size=24, shape=20, alpha=0.4, show_guide=TRUE) +
    scale_color_identity(name = 'Distance Walked in', guide = 'legend',labels = c('2 minutes', '1 minute')) +
    theme(legend.justification=c(0,0), legend.position=c(0,0)) +
    guides(col = guide_legend(reverse = TRUE)) +
    annotate("text", x = 120, y = 450, label = clockTime, colour = "#5555aa", size=12)
  
  
  if(sum(currentlyOpen) > 0){
    positionsplot +
      geom_text(aes(label = Text), size=3, hjust=0.5, vjust=0.35)
  } else {
    positionsplot
  }
  
  #ggsave(paste0("map", ctr, "_", hour*100, ".jpg"), path = "output", 
  #       scale = 1, width = 25.3, height = 30, units ="cm", dpi = 300) 
  
  ggsave(paste0("map", str_pad(ctr, 2, side ="left", pad="0"), ".png"), path = "output",
         scale = 1, width = 25.3, height = 30, units ="cm", dpi = 300) 
  
    
  ctr <- ctr + 1
}











# 
# 
# # Voronoi | Where's the Nearest Stall
# 
# #Quickcheck:
# #dfpoints <- expand.grid(seq(1,2000,100),seq(1,2000,100))
# #Actual
# dfpoints <- expand.grid(seq(1,2000,1),seq(1,2000,1))
# 
# nearestshop <- function(point){
#   distances <- apply(df[,c(3,4)], 1, function(r) sqrt(sum((point - r)^2)))
#   nearest <- which(distances == min(distances))
#   nearest <- nearest[1]
#   nearest
# }
# 
# dfpoints$NearestShop <- apply(dfpoints, 1, nearestshop)
# colnames(dfpoints) <- c("X", "Y", "NearestShop")
# save("dfpoints", file="points.RData")
# load("points.RData") #dfpoints
# 
# set.seed(5)
# n <- nrow(df)
# cols  <- colorRampPalette(brewer.pal(9,"Blues"))(n)
# set.seed(5)
# cols2 <- sample(topo.colors(n))
# set.seed(5)
# cols3 <- sample(rep(c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7"), 4))[1:n]
# 
# ggplot(data=dfpoints, aes(x = X, y = Y)) +
#   annotation_custom(bgimg, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf) +
#   geom_point(data=dfpoints, aes(color = factor(NearestShop), x = X, y = Y), shape=19, size=1, alpha=0.06) +
#   scale_colour_manual(values=cols3) +
#   
#   geom_point(data=df, aes(x = X, y = Y)) +
#   
#   xlim(0,2000) +
#   ylim(0,2000) +
#   theme(axis.text.y = element_blank(), axis.text.x = element_blank(),
#         axis.title = element_blank(), axis.title.x = element_blank())

