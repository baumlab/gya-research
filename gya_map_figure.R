

## Creating map of responses
library(rworldmap)
library(ggplot2)
library(RColorBrewer)
library(maptools)
library(mapproj)



#map.world <- map_data(map="world")

setwd("/Users/kristinatietjen/Documents/git_hub/gya-research")
#setwd("/Users/IMAC3/Documents/git-jpwrobinson/gya-research")

survey<-read.csv(file="data/gya-without-incomplete.csv")
nations<-aggregate(gender ~ nation, survey, length)



data(wrld_simpl)
# this lets us use the country name vs 3-letter ISO
wrld_simpl@data$id <- wrld_simpl@data$NAME

wrld <- fortify(wrld_simpl, region="id")
wrld <- subset(wrld, id != "Antarctica") # we don't need Antarctica
#wrld <- wrld_simpl[wrld_simpl$id!="Antarctica",]
#wrld <- fortify(wrld_simpl, region="id")


gg <- ggplot()

# setup base map
#gg <- gg + geom_map(data=wrld, map=wrld, aes(map_id=id), fill="white", color="#7f7f7f", size=0.25)

gg <- gg + geom_map(data=wrld, map=wrld, aes(map_id=id, x=long, y=lat), fill="white", color="#7f7f7f", size=0.25)

# add our colored regions
gg <- gg + geom_map(data=nations, map=wrld, aes(map_id=nation, fill=gender),  color="white", size=0.25)

#gg<- gg +scale_fill_continuous(expand=c(0,0), limits=c(0,1350),breaks=c(0,50,100,150, 200,250, 500, 750, 1000, 1250))

pdf(file="figures/map_1.pdf", width = 11, height= 7)

gg

dev.off()


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

pdf(file="figures/map_proper.coords_1.pdf", width=11, height= 7)
gg

dev.off()

 ###### ##### ##### ##### ##### ##### #####
###### now do it all with out Canada ########
 ###### ##### ##### ##### ##### ##### #####

#load data
survey<-read.csv(file="data/gya-without-incomplete.csv")

#remove Canada
n.gbl.wCA <- survey[survey$nation!="Canada",]
nations.wCA<-aggregate(gender ~ nation, n.gbl.wCA, length)

#now seperate out Canada so it can be a different color
canada <- survey[survey$nation=="Canada",]
ca<-aggregate(gender ~ nation, canada, length)


data(wrld_simpl)
# this lets us use the country name vs 3-letter ISO
wrld_simpl@data$id <- wrld_simpl@data$NAME

wrld <- fortify(wrld_simpl, region="id")
wrld <- subset(wrld, id != "Antarctica") # we don't need Antarctica
#wrld <- wrld_simpl[wrld_simpl$id!="Antarctica",]
#wrld <- fortify(wrld_simpl, region="id")


gg <- ggplot()

# setup base map
#gg <- gg + geom_map(data=wrld, map=wrld, aes(map_id=id), fill="white", color="#7f7f7f", size=0.25)

gg <- gg + geom_map(data=wrld, map=wrld, aes(map_id=id, x=long, y=lat), fill="white", color="#7f7f7f", size=0.25)

# add our colored regions (not canada)
gg <- gg + geom_map(data=nations.wCA, map=wrld, aes(map_id=nation, fill=gender),  color="white", size=0.25)
# add our colored regions (canada)
#gg <- gg + geom_map(data=ca, map=wrld, aes(map_id=nation, fill=gender), fill="tomato2",  color="white", size=0.25)+geom_text(aes(label=ca$gender, x=-110, y=58), size=5)     # saved as red
#gg <- gg + geom_map(data=ca, map=wrld, aes(map_id=nation, fill=gender), fill="mediumseagreen",  color="white", size=0.25)+geom_text(aes(label=ca$gender, x=-110, y=58), size=5)  # saved as green2
#gg <- gg + geom_map(data=ca, map=wrld, aes(map_id=nation, fill=gender), fill="springgreen3",  color="white", size=0.25)+geom_text(aes(label=ca$gender, x=-110, y=58), size=5)    # saved as green
#gg <- gg + geom_map(data=ca, map=wrld, aes(map_id=nation, fill=gender), fill="slategray2",  color="white", size=0.25)+geom_text(aes(label=ca$gender, x=-110, y=58), size=5)     #saved as lightblue
#gg <- gg + geom_map(data=ca, map=wrld, aes(map_id=nation, fill=gender), fill="grey",  color="white", size=0.25)+geom_text(aes(label=ca$gender, x=-110, y=58), size=5)      # saved as grey


#pdf(file="figures/map_w.out_CA_1.pdf", width = 11, height= 7)
gg
#dev.off()


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

pdf(file="figures/map_w.out_CA_proper.coords_1_grey.pdf", width=11, height= 7)
gg

dev.off()
