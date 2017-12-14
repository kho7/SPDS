library(ggplot2)
library(ggmap)
us=map_data("state")
# Create US map data frame
USmapDF <- data.frame(state.name, stringsAsFactors=F)
USmapDF$state=tolower(USmapDF$state.name)
# Create Black and White US map
map.us=ggplot(USmapDF,aes(map_id=state))
map.us=map.us+geom_map(map=us,fill="light yellow", color="black")
map.us=map.us+expand_limits(x=us$long,y=us$lat)
map.us=map.us+coord_map()+ggtitle("Basic Map of Continental USA")
map.us
map.taiwan <- get_map(location = 'Taiwan', zoom = 8, maptype="roadmap")
ggmap(map.taiwan)
