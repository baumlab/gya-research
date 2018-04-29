rm(list=ls())

dev.off()

## Creating map of responses
library(rworldmap)
library(ggplot2)
library(RColorBrewer)
library(maptools)
library(mapproj)
library(rgdal)
library(rgeos)


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
gg <- gg + geom_map(data=nations, map=wrld, aes(map_id=nation, fill=gender),  color="white", size=0.25)+ scale_fill_continuous(name="Number of\nresponses")

#gg<- gg +scale_fill_continuous(expand=c(0,0), limits=c(0,1350),breaks=c(0,50,100,150, 200,250, 500, 750, 1000, 1250))

#pdf(file="figures/map_1.pdf", width = 11, height= 7)

#gg

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

#pdf(file="figures/map_proper.coords_1_16March18.pdf", width=11, height= 7)
gg

#dev.off()


# now do it with the color scheme switched - darker = more responses
gg <- ggplot()
gg <- gg + geom_map(data=wrld, map=wrld, aes(map_id=id, x=long, y=lat), fill="white", color="#7f7f7f", size=0.25)
gg <- gg + geom_map(data=nations, map=wrld, aes(map_id=nation, fill=gender),  color="white", size=0.25)
gg <- gg + scale_fill_continuous(high = "#132B43", low = "#56B1F7", name="Number of\nresponses")
gg <- gg + coord_map()
gg <- gg + labs(x="", y="")
gg <- gg + theme(plot.background = element_rect(fill = "transparent", colour = NA),
                 panel.border = element_blank(),
                 panel.background = element_rect(fill = "transparent", colour = NA),
                 panel.grid = element_blank(),
                 axis.text = element_blank(),
                  axis.ticks = element_blank(),
                 legend.position = "right")

#pdf(file="figures/map_proper.coords_revcolor.pdf", width=11, height= 7)
gg

#dev.off()




 ###### ##### ##### ##### ##### ##### #####
###### now do it all with out Canada ########
 ###### ##### ##### ##### ##### ##### #####

#load data
survey<-read.csv(file="data/gya-without-incomplete.csv")

#remove Canada
n.gbl.wCA <- survey[survey$nation!="Canada",]
n.gbl.wCA <- droplevels(n.gbl.wCA)
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
gg <- gg + geom_map(data=nations.wCA, map=wrld, aes(map_id=nation, fill=gender),  color="white", size=0.25)+ scale_fill_continuous(name="Number of\nresponses")
# add our colored regions (canada)
gg <- gg + geom_map(data=ca, map=wrld, aes(map_id=nation, fill=gender), fill="tomato2",  color="white", size=0.25)+geom_text(aes(label=ca$gender, x=-110, y=58), size=5)     # saved as red
#gg <- gg + geom_map(data=ca, map=wrld, aes(map_id=nation, fill=gender), fill="mediumseagreen",  color="white", size=0.25)+geom_text(aes(label=ca$gender, x=-110, y=58), size=5)  # saved as green2
#gg <- gg + geom_map(data=ca, map=wrld, aes(map_id=nation, fill=gender), fill="springgreen3",  color="white", size=0.25)+geom_text(aes(label=ca$gender, x=-110, y=58), size=5)    # saved as green
#gg <- gg + geom_map(data=ca, map=wrld, aes(map_id=nation, fill=gender), fill="slategray2",  color="white", size=0.25)+geom_text(aes(label=ca$gender, x=-110, y=58), size=5)     #saved as lightblue
#gg <- gg + geom_map(data=ca, map=wrld, aes(map_id=nation, fill=gender), fill="grey",  color="white", size=0.25)+geom_text(aes(label=ca$gender, x=-110, y=58), size=5)      # saved as grey


#pdf(file="figures/map_w.out_CA_1.pdf", width = 11, height= 7)
#gg
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

#pdf(file="figures/map_w.out_CA_proper.coords_1_red_16March18.pdf", width=11, height= 7)
gg

#dev.off()





#############
# now do it with the color scheme switched - darker = more responses
#############

gg <- ggplot()
gg <- gg + geom_map(data=wrld, map=wrld, aes(map_id=id, x=long, y=lat), fill="white", color="#7f7f7f", size=0.25)
gg <- gg + geom_map(data=nations.wCA, map=wrld, aes(map_id=nation, fill=gender),  color="white", size=0.25)
gg <- gg + scale_fill_continuous(high = "#132B43", low = "#56B1F7", name="Number of\nresponses")
gg <- gg + geom_map(data=ca, map=wrld, aes(map_id=nation, fill=gender), fill="grey",  color="white", size=0.25)+geom_text(aes(label=ca$gender, x=-110, y=58), size=5)      # saved as grey
gg <- gg + coord_map()
gg <- gg + labs(x="", y="")
gg <- gg + theme(plot.background = element_rect(fill = "transparent", colour = NA),
                 panel.border = element_blank(),
                 panel.background = element_rect(fill = "transparent", colour = NA),
                 panel.grid = element_blank(),
                 axis.text = element_blank(),
                 axis.ticks = element_blank(),
                 legend.position = "right")

#pdf(file="figures/map_proper.coords_revcolor_CA_diff_16March18.pdf", width=11, height= 7)
gg

#dev.off()


########################################################################################
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#<---------------Now going to make the map with two different scales ------------------>
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
########################################################################################
#one scale with developing and one with developed

#re read in full data
survey<-read.csv(file="data/gya-without-incomplete.csv")
# select for just developed
devlp <- survey[survey$class=="developed",]
#now aggregate
nations.devlp<-aggregate(gender ~ nation, devlp, length)


#select for just developing
dev <- survey[survey$class=="developing",]
#now aggregate
nations.dev<-aggregate(gender ~ nation, dev, length)

##idfferent approach
head(nations)

#add in a column for developed/developing 
developed<-c("Canada","Australia", "Israel", "Barbados","Russia","Germany", "United Kingdom", "Netherlands", "Japan","United States", "Taiwan","New Zealand","France", "Switzerland", "Poland",              
             "Portugal", "Italy",  "Belgium",  "Norway", "Finland", "Greece","Cyprus",  "Hungary", "Spain","Singapore", "Korea, South","Romania", "Denmark","Austria", "Sweden",                
             "Estonia", "Malta", "Iceland" )

developing<-c("Brazil","South Africa", "Mauritius", "Uruguay","Turkey","Indonesia", "Morocco", "India", "Bangladesh","Ghana", "Malaysia","Vietnam","Nigeria", "Egypt", "Montenegro",              
              "China", "Serbia",  "Kenya",  "Central African Republic", "Chad", "Chile","Argentina",  "Mexico", "Nepal","Benin", "Ethiopia","Lesotho", "Nicaragua","Philippines", "Colombia",                
              "Mozambique", "Dominican Republic", "Lebanon","Gabon", "Cameroon", "Uganda", "Iran", "Sudan", "Thailand", "Marshall Islands" ) 
nations$class <- ifelse(nations$nation%in%developed, "developed", "developing")
head(nations)


gg <- ggplot()
gg <- gg + geom_map(data=wrld, map=wrld, aes(map_id=id, x=long, y=lat), fill="white", color="#7f7f7f", size=0.25)
gg <- gg + geom_map(data=nations, map=wrld, aes(map_id=nation, fill=class, alpha = gender),  color="white", size=0.25) 
gg <- gg + scale_fill_manual(values = c("#132B43", "#67000d"), name = "") 
gg <- gg + scale_alpha_continuous(range = c(0.4, 1), name = "Responses")
gg <- gg + coord_map()
gg <- gg + labs(x="", y="")
gg <- gg + theme(plot.background = element_rect(fill = "transparent", colour = NA),
                 panel.border = element_blank(),
                 panel.background = element_rect(fill = "transparent", colour = NA),
                 panel.grid = element_blank(),
                 axis.text = element_blank(),
                 axis.ticks = element_blank(),
                 legend.position = "right")

#pdf(file="figures/map_proper.coords_revcolor_both_alpha.pdf", width=11, height= 7)
gg

#dev.off()



# ggplot() +
#   geom_map(data=wrld, map=wrld, aes(map_id=id, x=long, y=lat),      fill="white", color="#7f7f7f", size=0.25) +
#   geom_map(data=df, map=wrld, aes(map_id=country, fill=area, alpha = responses),  color="white", size=0.25) +
#   scale_fill_manual(values = c("#132B43", "#67000d")) + 
#   scale_alpha_continuous(range = c(0.3, 1)) + 
#   coord_map() +
#   labs(x="", y="") +
#   theme(plot.background = element_rect(fill = "transparent", colour = NA),
#         panel.border = element_blank(),
#         panel.background = element_rect(fill = "transparent", colour = NA),
#         panel.grid = element_blank(),
#         axis.text = element_blank(),
#         axis.ticks = element_blank(),
#         legend.position = "right"


# gg <- gg + geom_map(data=nations.devlp, map=wrld, aes(map_id=nation, fill=gender),  color="white", size=0.25, fill = "blue")
# gg <- gg + scale_fill_continuous(high = "#132B43", low = "#56B1F7", name="Number of\nresponses\ndeveloped nations")
# gg <- gg + geom_map(data=nations.dev, map=wrld, aes(map_id=nation, fill=gender),  color="white", size=0.25, fill = "red")
# gg <- gg + scale_color_identity(high = "#67000d", low = "#fcbba1", name="Number of\nresponses\ndeveloping nations")
# 
# gg <- gg + geom_map(data=nations.devlp, map=wrld, aes(map_id=nation, fill=gender),  color="white", size=0.25)
# gg <- gg + scale_fill_continuous(aes = "developed", high = "#132B43", low = "#56B1F7", name="Number of\nresponses\ndeveloped nations")
# gg <- gg + geom_map(data=nations.dev, map=wrld, aes(map_id=nation, fill=gender),  color="white", size=0.25)
# gg <- gg + scale_fill_continuous(aes = "developing", high = "#67000d", low = "#fb6a4a", name="Number of\nresponses\ndeveloping nations")


#   gg <- gg + geom_map(data=nations, map=wrld, aes(map_id=nation, fill=gender), fill = class , color="white", size=0.25)
#  # gg <- gg + scale_color_manual(values = c())
# gg <- gg + scale_fill_continuous(values= c(blue, red), name="Number of\nresponses")


#gg <- gg + geom_map(data=nations, map=wrld, aes(map_id=nation, fill=gender),  color="white", size=0.25)
#gg <- gg + scale_fill_continuous(high = "#132B43", low = "#56B1F7", name="Number of\nresponses")
#gg <- gg + scale_fill_continuous(values = , name="Number of\nresponses")

#<------------------------------------------>
# ok for now I am going to just make two maps and overlay them in photoshop
gg <- ggplot()
gg <- gg + geom_map(data=wrld, map=wrld, aes(map_id=id, x=long, y=lat), fill="white", color="#7f7f7f", size=0.25)
#gg <- gg + geom_map(data=nations.devlp, map=wrld, aes(map_id=nation, fill=gender),  color="#7f7f7f", size=0.25)
#gg <- gg + scale_fill_continuous(high = "#132B43", low = "#56B1F7", name="Responses from\ndeveloped nations")
gg <- gg + geom_map(data=nations.dev, map=wrld, aes(map_id=nation, fill=gender),  color="#7f7f7f", size=0.25)
gg <- gg + scale_fill_continuous(high = "#67000d", low = "#fcbba1", name="Responses from\ndeveloping nations")
gg <- gg + coord_map()
gg <- gg + labs(x="", y="")
gg <- gg + theme(plot.background = element_rect(fill = "transparent", colour = NA),
                 panel.border = element_blank(),
                 panel.background = element_rect(fill = "transparent", colour = NA),
                 panel.grid = element_blank(),
                 axis.text = element_blank(),
                 axis.ticks = element_blank(),
                 legend.position = "bottom",
                 legend.text = element_text(angle = 90))

#pdf(file="figures/map_proper.coords_revcolor_developed.pdf", width=11, height= 7)
pdf(file="figures/map_proper.coords_revcolor_developing.pdf", width=11, height= 7)
#pdf(file="figures/map_proper.coords_revcolor.pdf", width=11, height= 7)
gg

dev.off()




##asking question



df<-read.csv(file="data/questiondata.csv")

df

na <- df[df$area=="northamerica",]
euro <- df[df$area=="europe",]

library(maptools)
#library(rworldmap)
#library(mapproj)
library(ggplot2)
data(wrld_simpl)
wrld_simpl@data$id <- wrld_simpl@data$NAME
wrld <- fortify(wrld_simpl, region="id")
wrld <- subset(wrld, id != "Antarctica")

# 
# gg <- ggplot()
# gg <- gg + geom_map(data=wrld, map=wrld, aes(map_id=id, x=long, y=lat), fill="white", color="#7f7f7f", size=0.25)
# gg <- gg + geom_map(data=df, map=wrld, aes(map_id=country, fill=responses),  color="white", size=0.25)
# gg <- gg + geom_map(aes(alpha = ..responses..), fill = area) 
# #gg <- gg + geom_map(data=na, map=wrld, aes(map_id=country, fill=responses),  color="white", size=0.25, fill = "blue")
# #gg <- gg + scale_fill_continuous(high = "#132B43", low = "#56B1F7", name="Number of\nresponses\nNorth America")
# #gg <- gg + geom_map(data=euro, map=wrld, aes(map_id=country, fill=responses),  color="white", size=0.25, fill = "red")
# #gg <- gg + scale_fill_continuous(high = "#67000d", low = "#fcbba1", name="Number of\nresponses\nEurope")
# gg <- gg + coord_map()
# gg <- gg + labs(x="", y="")
# gg <- gg + theme(plot.background = element_rect(fill = "transparent", colour = NA),
#                  panel.border = element_blank(),
#                  panel.background = element_rect(fill = "transparent", colour = NA),
#                  panel.grid = element_blank(),
#                  axis.text = element_blank(),
#                  axis.ticks = element_blank(),
#                  legend.position = "right")
# gg


gg <- ggplot()
gg <- gg + geom_map(data=wrld, map=wrld, aes(map_id=id, x=long, y=lat), fill="white", color="#7f7f7f", size=0.25)
gg <- gg + geom_map(data=na, map=wrld, aes(map_id=country, fill=responses),  color="white", size=0.25)
gg <- gg + scale_fill_continuous(high = "#132B43", low = "#56B1F7", name="Number of\nresponses\nNorth America")
#gg <- gg + geom_map(data=euro, map=wrld, aes(map_id=country, fill=responses),  color="white", size=0.25, fill = "red")
#gg <- gg + scale_fill_continuous(high = "#67000d", low = "#fcbba1", name="Number of\nresponses\nEurope")
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

gg+geom_map(data = euro, map = wrld, stat = "identity", aes(map_id=country, fill = responses))+scale_fill_gradient2(high = "#67000d", low = "#fcbba1", space = "Lab", guide = "colourbar")
gg
