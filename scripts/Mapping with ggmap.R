library(ggmap)
library(jpeg)

loc2 <- c(72.9,19.12,72.92,19.143)
myMap1 <- get_map(location = loc2, zoom = 15,
                  source="google",
                  maptype="roadmap",
                  color="bw")

myMap2 <- get_map(location = loc2, zoom = 15,
                  source="osm",
                  color="bw")

myMap3 <- get_stmap(location = loc2, zoom = 15,
                    source="stamen",
                    maptype="watercolor")

ggmap(myMap2)
