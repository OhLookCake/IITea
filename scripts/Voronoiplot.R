# Voronoi | Where's the Nearest Stall

#Quickcheck:
dfpoints <- expand.grid(seq(1,2000,100),seq(1,2000,100))
#Actual
dfpoints <- expand.grid(seq(1,2000,1),seq(1,2000,1))
 
nearestshop <- function(point){
   distances <- apply(df[,c(3,4)], 1, function(r) sqrt(sum((point - r)^2)))
   nearest <- which(distances == min(distances))
   nearest <- nearest[1]
   nearest
}
 
dfpoints$NearestShop <- apply(dfpoints, 1, nearestshop)
colnames(dfpoints) <- c("X", "Y", "NearestShop")
save("dfpoints", file="points.RData")
load("points.RData") #dfpoints

set.seed(5)
n <- nrow(df)
cols  <- colorRampPalette(brewer.pal(9,"Blues"))(n)
set.seed(5)
cols2 <- sample(topo.colors(n))
set.seed(5)
cols3 <- sample(rep(c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7"), 4))[1:n]

ggplot(data=dfpoints, aes(x = X, y = Y)) +
  annotation_custom(bgimg, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf) +
  geom_point(data=dfpoints, aes(color = factor(NearestShop), x = X, y = Y), shape=19, size=1, alpha=0.06) +
  scale_colour_manual(values=cols3) +
  
  geom_point(data=df, aes(x = X, y = Y)) +
  
  xlim(0,2000) +
  ylim(0,2000) +
  theme(axis.text.y = element_blank(), axis.text.x = element_blank(),
        axis.title = element_blank(), axis.title.x = element_blank())
