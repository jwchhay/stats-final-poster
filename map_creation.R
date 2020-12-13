library(tidyverse)
library(readr)
library(maps)
library(maptools)
library(tm)
library(wordcloud)
library(RColorBrewer)
library(ggmap)
library(maps)
listings <- read_csv("listings.csv")


### looking at Price variable
# price variable is character and has dollar signs
listings$priceNew <- gsub("[\\$,]", "", listings$price)
listings$priceNew <- as.numeric(listings$priceNew)
listings$color <- ifelse(listings$priceNew > 139, "blue", "red")



# Map
counties <- map_data("county")
ca_county <- subset(counties, region == "california")
LA <- map_data("state", region = "california")

LA_base <- ggplot(data = LA, mapping = aes(x = long, y = lat, group = group)) + coord_fixed(1.3) +
  geom_polygon(color = "black", fill = "gray")

LA_base + geom_polygon(data = ca_county, fill = NA, color = "white") + geom_polygon(color = "black", fill = NA) + 
  coord_fixed(xlim=c(-119, -117.6), ylim=c(33.3, 34.9), ratio = 1.3) + 
  geom_point(data = listings, aes(x = longitude, y = latitude, color = color), shape = 1, inherit.aes = FALSE) + 
  labs(color = "Prices", title = "Map of Prices of LA Airbnb Locations") + 
  scale_color_manual(labels = c("$139 or above", "$138 or below"), values = c("blue", "red")) + theme(legend.position = c(0.8, 0.1))