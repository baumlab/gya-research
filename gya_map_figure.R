

## Creating map of responses
library(rworldmap)
library(ggplot2)
library(RColorBrewer)
library(maptools)



#map.world <- map_data(map="world")

setwd("/Users/kristinatietjen/Documents/git_hub/gya-research")
#setwd("/Users/IMAC3/Documents/git-jpwrobinson/gya-research")

survey<-read.csv(file="data/gya-without-incomplete.csv")
nations<-aggregate(gender ~ nation, survey, length)



data(wrld_simpl)
# this lets us use the contry name vs 3-letter ISO
wrld_simpl@data$id <- wrld_simpl@data$NAME

wrld <- fortify(wrld_simpl, region="id")
wrld <- subset(wrld_simpl, id != "Antarctica") # we don't need Antarctica

gg <- ggplot()

# setup base map
gg <- gg + geom_map(data=wrld, map=wrld, aes(map_id=id, x=long, y=lat), fill="white", color="#7f7f7f", size=0.25)

# add our colored regions
gg <- gg + geom_map(data=nations, map=wrld, aes(map_id=nations, fill=gender),  color="white", size=0.25)
gg




# this gives us proper coords. mercator proj is default
gg <- gg + coord_map()
gg <- gg + labs(x="", y="")
gg <- gg + theme(plot.background = element_rect(fill = "transparent", colour = NA),
                 panel.border = element_blank(),
                 panel.background = element_rect(fill = "transparent", colour = NA),
                 panel.grid = element_blank(),
                 axis.text = element_blank(),
                 axis.ticks = element_blank(),
                 legend.position = "right")
gg
