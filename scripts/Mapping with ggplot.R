library(ggplot2)
library(jpeg)

setwd("C:/Projects/Data/Insight Data Blog/IITea")
img <- readJPEG("images/Map_Google_Grayscale.jpg") 
bgimg <- rasterGrob(img, interpolate=TRUE) 


df <- read.csv("data//placelist.csv")
baseplot <- ggplot(data = df, aes(x = X, y = Y)) +
              annotation_custom(bgimg, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf) +
              xlim(0,2000) +
              ylim(0,2000) +
              theme(axis.text.y = element_blank(), axis.text.x = element_blank(),
                    axis.title = element_blank(), axis.title.x = element_blank())

baseplot + geom_point()

baseplot +
  geom_point(color="#997766", fill="#997766", size=36, shape=21, alpha=0.3, show_guide=FALSE) +
  geom_point(color="#4466DD", fill="#4466DD", size=18, shape=21, alpha=0.6, show_guide=FALSE)
