# Session 4: another reason for Dimension Reduction
setwd("C:/Users/Rob/Box Sync/My R Work/BUS212")
library(tidyverse)
library(ggmap)
library(maps)
library(mapdata)

airbnb <- read_csv("Data/Boston Listings 2018 Case 1.csv")
glimpse(airbnb)
View(airbnb)

# let's map the properties in the data frame - crude map
mass <- subset(map_data("state"), region == "massachusetts")
counties <- map_data("county")
ma_county <- subset(counties, region == "massachusetts")
suffolk <- subset(ma_county, subregion == "suffolk")


suff <- ggplot() + geom_polygon(data = suffolk, aes(x=long, y = lat, group = group)) + 
     coord_fixed(1.3) + ggtitle("Suffolk county")
# add points
suff + geom_point(data=airbnb, mapping=aes(x= longitude, y= latitude, color = Commercial))

# inspect the location variables
table(airbnb$neighbourhood)
table(airbnb$neighbourhood_cleansed)
table(airbnb$smart_location)








