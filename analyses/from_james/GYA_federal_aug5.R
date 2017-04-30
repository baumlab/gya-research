
### creating plots for Megan Dodd + Julia Baum for global young academy document


setwd("/Users/jpwrobinson/Google_Drive/R_PROJECTS_DATA/VISUALISATIONS/global-young-academy-gdp")
setwd("/Users/IMAC3/Google_Drive/R_PROJECTS_DATA/VISUALISATIONS/global-young-academy-GDP")
setwd("/Users/kristinatietjen/Documents/git_hub/gya-research")
theme_set(theme_minimal(base_size=14))
require(tidyr); require(scales); require(grid); require(RColorBrewer)

palette(brewer.pal(n=8, name="Dark2"))

## read in data
dat1<-read.csv("data/from_James/part5-international-july/aug5/federal/graph1.csv", check.names=FALSE)
dat2<-read.csv("data/from_James/part5-international-july/aug5/federal/graph2.csv", check.names=FALSE)
dat3<-read.csv("data/from_James/part5-international-july/aug5/federal/graph3.csv", check.names=FALSE)
dat4<-read.csv("data/from_James/part5-international-july/aug5/federal/graph4.csv", check.names=FALSE)
dat5<-read.csv("data/from_James/part5-international-july/aug5/federal/graph5.csv", check.names=FALSE)
dat6<-read.csv("data/from_James/part5-international-july/aug5/federal/graph6.csv", check.names=FALSE)
dat7<-read.csv("data/from_James/part5-international-july/aug5/federal/graph7.csv", check.names=FALSE)


#pdf(file="figures/GYA_federal_aug5_2016.pdf", height=7, width=11)
# png(file="figures/GYA_federal_aug5_2016.png", pointsize=20,height=900, width=1500)
#-----------------------#-----------------------#-----------------------
	#----------------------- GRAPH 1 #-----------------------
#-----------------------#-----------------------#-----------------------

### A 
# NOTES:
# Also create graphs that include identical data but only from 2006-2015
# Keep data points for first year in every graph starting around the same visual point
# label each axis to indicate which dataset they represent?
head(dat1)
dat1<-gather(dat1,  Year, value, -panel, -Type)



ylabel <- "Expenditure (Millions of 2015 Constant Dollars)"

p<-ggplot(dat1, aes(Year, value, col=Type, group=1)) + 
  geom_line(size=1, col="#1F78B4") + #scale_x_continuous(breaks=NULL) + 
  labs(x="", y=ylabel) + 
  theme(legend.title=element_blank(),
  	plot.margin=unit(c(0,0,-0.2,0.8), "cm"),
  	 axis.title.y=element_text(hjust=0.5, vjust=-1),
  	 axis.text.x=element_text(angle=0),
  	 strip.background = element_blank(),
       strip.text.x = element_blank())  +
  scale_y_continuous(labels=comma)+ 	
    # annotate("text", 2005, 34000, label="(a) All Sectors", hjust=0) +
  facet_wrap(~Type, scales="free")+
  guides(col=FALSE) + scale_colour_manual(values=c(brewer.pal(2, name="Paired")))

  ann_text <- data.frame(Year = c('2005','2005'), value = c(34000,3225),lab = c("Text",'Text'),
                       Type = factor(c("Intramural R&D performed by all sectors (GERD)",
                       		"Intramural R&D performed by the federal government"),levels = c("Intramural R&D performed by all sectors (GERD)",
                       		"Intramural R&D performed by the federal government")))

p + geom_text(data = ann_text[1,],label = "(a) GERD", col="black", size=4, hjust=-0.001) +
	geom_text(data = ann_text[2,],label = "(b) Federal", col="black", size=4, hjust=-0.001)




### B

#-----------------------#-----------------------#-----------------------
	#----------------------- GRAPH 2 #-----------------------
#-----------------------#-----------------------#-----------------------
head(dat2)
dat2<-gather(dat2,  Year, value, -panel, -type)
dat2$sector<-ifelse(grepl("Social", dat2$type), "Social sciences and Humanities", "Natural sciences and Engineering")
dat2$type<-revalue(dat2$type,c("Intramural R&D performed by all sectors in the Natural Sciences and Engineering"="All sectors",
				"Intramural R&D performed by the federal government in the Natural Sciences and Engineering"="Federal government",
				"Intramural R&D performed by all sectors in the Social Sciences and Humanities"=" ",
				"Intramural R&D performed by the federal government in the Social Sciences and Humanities"="  "))

dat2$type<-factor(dat2$type, levels(dat2$type)[c(1,3,2,4)])

ylabel <- "Expenditure (Millions of 2015 Constant Dollars)"

g1<-ggplot(dat2, aes(Year, value, col=sector, group=1)) + 
  geom_line(size=1) + #scale_x_continuous(breaks=NULL) + 
  labs(x="", y="") + 
  theme(legend.title=element_blank(),
  	legend.position=c(0.8,0.1),
  	plot.margin=unit(c(0,0,-0.2,0.8), "cm"),
  	 axis.title.y=element_text(hjust=0.5, vjust=-1),
  	 axis.text.x=element_text(angle=0,size=10))+
  scale_y_continuous(labels=comma)+ 	
   # annotate("text", 1998, 34000, label="(a) All Sectors", hjust=0) +
  facet_wrap(~type, scales="free")+
  scale_colour_manual(values=c(brewer.pal(2, name="Paired")))
  # guides(col=FALSE)
grid.arrange(g1, left = textGrob(ylabel, rot = 90, vjust = 3))



# NOTES		
# please also create an identical graph with data from 2006-2015 only		
# could you pleas
#-----------------------#-----------------------#-----------------------
	#----------------------- GRAPH 3 #-----------------------
#-----------------------#-----------------------#-----------------------


dat3<-gather(dat3, Year, value, -panel, -Type)

ylabel='Expenditure\n (Millions of 2015 Constant Dollars)'

g1<-ggplot(dat3[!dat3$Type=="Percent of Total R&D in the Natural Sciences and Engineering funded by the Federal Government",],
			aes(Year, value, fill=Type)) + geom_bar(stat="identity") +  scale_y_continuous(labels=comma) +
			labs(x="", y=ylabel) +
			guides(fill=guide_legend(nrow=7,byrow=TRUE)) + 
			theme(legend.position="right", legend.title=element_blank(),
				plot.margin=unit(c(0,0,0,0), "cm"),
				axis.title.y=element_text(hjust=0.5, size=10),
				axis.text.x=element_text()) +
			scale_fill_manual(values=c(brewer.pal(7, name="Paired"))) 
			# scale_x_continuous(breaks=seq(2005, 2015, 1))


ylabel="Percent funded by the Federal Government"
g2<-ggplot(dat3[dat3$Type=="Percent of Total R&D in the Natural Sciences and Engineering funded by the Federal Government",],
			aes(Year, value, group=1)) + geom_line(size=1, col="#1F78B4") +  scale_y_continuous(labels=comma) +
			labs(x="", y=ylabel) +
			guides(fill=FALSE) + 
			theme(legend.position="right", legend.title=element_blank(),
				plot.margin=unit(c(0,0,0,0), "cm"),
				axis.title.y=element_text(hjust=0.5, size=10),
				axis.text.x=element_text()) +
			# scale_colour_manual(values=c(brewer.pal(2, name="Paired"))) +
			guides(col=FALSE)

g2
dat3.sub<-dat3[!(dat3$Type=="Provincial and municipal governments"| 
		dat3$Type=="Other Canadian performers" | 
			dat3$Type=="Percent of Total R&D in the Natural Sciences and Engineering funded by the Federal Government"),]
## plot with % GERD
grid.arrange(g1, g2, layout_matrix = rbind(1,1,1,2,2))
grid.arrange(g1%+% dat3.sub, g2, layout_matrix = rbind(1,1,1,2,2))


#-----------------------#-----------------------#-----------------------
	#----------------------- GRAPH 4 #-----------------------
# #-----------------------#-----------------------#-----------------------


dat4<-gather(dat4, Year, value, -panel, -Type)

ylabel='Expenditure\n (Millions of 2015 Constant Dollars)'

g1<-ggplot(dat4[!dat4$Type=="Percent of Total R&D in the Social Sciences and Humanities funded by the Federal Government",],
			aes(Year, value, fill=Type)) + geom_bar(stat="identity") +  scale_y_continuous(labels=comma) +
			labs(x="", y=ylabel) +
			guides(fill=guide_legend(nrow=7,byrow=TRUE)) + 
			theme(legend.position="right", legend.title=element_blank(),
				plot.margin=unit(c(0,0,0,0), "cm"),
				axis.title.y=element_text(hjust=0.5, size=10),
				axis.text.x=element_text()) +
	scale_fill_manual(values=c(brewer.pal(8, name="Paired")))
			# scale_x_continuous(breaks=seq(2005, 2015, 1))

ylabel="Percent funded by the Federal Government"
g2<-ggplot(dat4[dat4$Type=="Percent of Total R&D in the Social Sciences and Humanities funded by the Federal Government",],
			aes(Year, value,  group=1)) + geom_line(size=1col="#1F78B4") +  scale_y_continuous(labels=comma) +
			labs(x="", y=ylabel) +
			guides(fill=FALSE) + 
			theme(legend.position="right", legend.title=element_blank(),
				plot.margin=unit(c(0,0,0,0), "cm"),
				axis.title.y=element_text(hjust=0.5, size=10),
				axis.text.x=element_text()) +
			scale_colour_manual(values=c(brewer.pal(2, name="Paired")))


dat4.sub<-dat4[!(dat4$Type=="Provincial and municipal governments"| 
		dat4$Type=="Other Canadian performers" | 
			dat4$Type=="Percent of Total R&D in the Social Sciences and Humanities funded by the Federal Government"),]
## plot with % GERD
grid.arrange(g1, g2, layout_matrix = rbind(1,1,1,2,2))
grid.arrange(g1%+% dat4.sub, g2, layout_matrix = rbind(1,1,1,2,2))



#-----------------------#-----------------------#-----------------------
	#----------------------- GRAPH 5 #-----------------------
#-----------------------#-----------------------#-----------------------


dat5<-gather(dat5,  year, value, -Type)

ylabel='Expenditure (Millions of 2015 Constant Dollars)'

ggplot(dat5, aes(as.numeric(year), value, col=Type)) + 
  geom_line(size=1) + scale_x_continuous(breaks=seq(2005, 2015,1)) + 
  labs(x="", y=ylabel) + 
  theme(legend.title=element_blank(),
  	legend.position="bottom",
  	plot.margin=unit(c(0,0,-0.2,0.8), "cm"),
  	 axis.title.y=element_text(hjust=0.5, vjust=-1),
  	 axis.text.x=element_text(angle=0),
  	 strip.background = element_blank(),
       strip.text.x = element_blank())  +
  scale_y_continuous(labels=comma)+
   # annotate("text", 1998, 34000, label="(a) All Sectors", hjust=0) +
  guides(col=guide_legend(nrow=2)) +
  scale_colour_manual(values=c(brewer.pal(4, name="Paired")))



#-----------------------#-----------------------#-----------------------
	#----------------------- GRAPH 6 #-----------------------
#-----------------------#-----------------------#-----------------------
dat6<-read.csv("data/from_James/part5-international-july/aug5/federal/graph6.csv", check.names=FALSE)
dat6<-gather(dat6, Year, value, -panel, -type, -sector)
ylabel="Number of Researchers Employed per Sector"
dat6$sector<-revalue(dat6$sector, c("Natural Sciences and Engineering"="(a) Natural Sciences and Engineering"))
dat6$sector<-revalue(dat6$sector, c("Social Sciences and Humanities"="(b) Social Sciences and Humanities"))

dat6<-dat6[!dat6$type=='Business enterprise',]

pdf(file="figures/from_James/GYA_figure3.4_29April17_3.pdf", height=8, width =11)

ggplot(dat6, aes(as.numeric(Year), value, col=type)) + 
  geom_line(size=1) + scale_x_continuous(breaks=seq(2005, 2015,1)) + 
  labs(x="", y=ylabel) + 
  facet_wrap(~ sector)+
  theme(aspect.ratio=3/4, legend.title=element_blank(),
  	legend.position=c(0.85, 0.4),
  	# plot.margin=unit(c(0,0,-0.2,0.8), "cm"),
  	 axis.title.y=element_text(hjust=0.5, vjust=-1),
  	 axis.text.x=element_text(angle=0),
  	 # strip.background = element_blank(),
       strip.text.x = element_text(hjust=-0.001, face='bold') ) +
  scale_y_continuous(labels=comma)+
   # annotate("text", 1998, 34000, label="(a) All Sectors", hjust=0) +
  guides(col=guide_legend(nrow=3)) +
  scale_colour_manual(values=c(brewer.pal(4, name="Paired")))


dev.off()

#-----------------------#-----------------------#-----------------------
	#----------------------- GRAPH 7 #-----------------------
#-----------------------#-----------------------#-----------------------

dat7<-gather(dat7, Year, value, -panel, -type)

ylabel='Expenditure\n (Millions of 2015 Constant Dollars)'

g1<-ggplot(dat7[!dat7$type=="Personnel",],
			aes(Year, value, fill=type)) + geom_bar(stat="identity") +  scale_y_continuous(labels=comma) +
			labs(x="", y=ylabel) +
			guides(fill=guide_legend(nrow=7,byrow=TRUE)) + 
			theme(legend.position=c(0.1, 0.2), 
				legend.title=element_blank(),
				legend.background=element_rect(fill="white"),
				legend.key.height=unit(0, 'cm'),
				legend.margin = unit(0, "cm"),
				plot.margin=unit(c(0,0,0,0), "cm"),
				axis.title.y=element_text(hjust=0.5, size=12),
				axis.text.x=element_text()) +
scale_fill_manual(values=c(brewer.pal(2, name="Paired")))
			# scale_x_continuous(breaks=seq(2005, 2015, 1))

ylabel="Number of personnel engaged in R&D"

g2<-ggplot(dat7[dat7$type=="Personnel",],
			aes(Year, value, fill=type, group=1)) + geom_line(col="#1F78B4", size=1) +  scale_y_continuous(labels=comma) +
			labs(x="", y=ylabel) +
			guides(fill=FALSE) + 
			theme(legend.position="none", legend.title=element_blank(),
				plot.margin=unit(c(0,0,0,0), "cm"),
				axis.title.y=element_text(hjust=0.5, size=12),
				axis.text.x=element_text()) +
			scale_colour_manual(values=c(brewer.pal(2, name="Paired")))


grid.arrange(g1, g2)


dev.off()