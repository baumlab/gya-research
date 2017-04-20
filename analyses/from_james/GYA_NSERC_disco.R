
### creating plots for Megan Dodd + Julia Baum for global young academy document


## Aim: make 5 plots
require(tidyr)
require(scales)
require(grid)

### return to default ggplot colors


# cols <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
cols <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
cols2 <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#000000")


setwd("/Users/jpwrobinson/Google_Drive/R_PROJECTS_DATA/VISUALISATIONS/global-young-academy-gdp")
theme_set(theme_minimal(base_size=14))
# panel.grid.minor =   element_line(colour = "grey",size=0.25),
        # panel.grid.major =   element_line(colour = "grey",size=0.25),



pdf(file="GYA_NSERC_disco.pdf", height=7, width=12)

disco<-read.csv("Data/part3_may/GYA_NSERC_discovery.csv", header=TRUE, stringsAsFactors=TRUE, check.names=FALSE)
colnames(disco)[1]<-'Type'

df<-gather(disco, "Type")
colnames(df)<-c('Grant', "Year", "value")

df$Year<-(str_split_fixed(df$Year, "-", 2))[,1]
# df$Year<-as.numeric(df$Year)
### Figure 1
### Bar plot of average award value and grants awarded
## Problem - different scales.....

## 1a = 2005-06 : 2014-15
## 1b = 1991-92 : 2014-15
	
cols<-c("#1b9e77","#d95f02")

## grants awarded	
g1<-ggplot(df[df$Grant=="Grants Awarded (n)",], aes(Year, value, group=1)) +
labs (y= "Grants Awarded (n)\n ", col="", x="", title="Number and Value of NSERC Discovery Grants Awarded \n(1991-2014)")  +
scale_x_discrete(labels=unique(df$Year)) +
geom_point(stat='summary', fun.y=sum, size=0) +
stat_summary(fun.y=sum, geom="line",size=2, col=cols[1]) + 
scale_y_continuous(labels=comma) +
theme(axis.title.y=element_text(vjust=0.9),
	# panel.grid.minor =   element_line(colour = "grey",size=0.25),
        # panel.grid.major =   element_line(colour = "grey",size=0.25),
		legend.position="none",
		 axis.text.y= element_text(size=12),
		 axis.text.x= element_text(size=0, angle=0),
		 axis.ticks.x=element_blank(),
		 legend.position="left",
		 strip.background=element_rect(fill = "white",
		 colour = "white"), 
 plot.margin=unit(c(1,1,-0.2,1), "cm"))

# average award value
g2<- ggplot(df[df$Grant=="Average Award Value (Constant 2015 $)",], aes(Year, value, group=1)) +
labs (y= "Average Award Value \n(Constant 2015 $)", col="", x="")+#, title="Number and Value of NSERC Discovery Grants Awarded \n(2005-2014)")  +
# scale_x_discrete(labels=unique(df$Year)) +
geom_point(stat='summary', fun.y=sum, size=0) +
stat_summary(fun.y=sum, geom="line",size=2, col=cols[2]) +
scale_y_continuous(labels=comma, breaks=seq(32000, 42000, 2000), limits=c(32000, 42000))+
theme(axis.title.y=element_text(vjust=0.9),
	# panel.grid.minor =   element_line(colour = "grey",size=0.25),
        # panel.grid.major =   element_line(colour = "grey",size=0.25),
		legend.position="none",
		 axis.text.y= element_text(size=12),
		 axis.text.x= element_text(size=12, angle=0),
		 axis.ticks.x=element_line(),
		 legend.position="left",
		 strip.background=element_rect(fill = "white",
		 colour = "white"),
		  plot.margin=unit(c(-0.2,1,1,1), "cm")) 

grid.arrange(g1, g2)

ggplot(df[df$Grant=="Average Award Value (Constant 2015 $)",], aes(Year, value,group=1)) +
labs (y= "Average Award Value \n(Constant 2015 $)", col="", x="", title="Number and Value of NSERC Discovery Grants Awarded \n(2005-2014)")  +
scale_x_discrete(labels=unique(df$Year[df$Grant=="Average Award Value (Constant 2015 $)"])) +
geom_line(col=cols[2]) +
# geom_point(stat='summary', fun.y=sum, size=0) +
# stat_summary(fun.y=sum, geom="line",size=2, col=cols[2]) +
scale_y_continuous(labels=comma, breaks=seq(32000, 42000, 2000), limits=c(32000, 42000))+
theme(axis.title.y=element_text(vjust=0.9),
	# panel.grid.minor =   element_line(colour = "grey",size=0.25),
        # panel.grid.major =   element_line(colour = "grey",size=0.25),
		legend.position="none",
		 axis.text.y= element_text(size=12),
		 axis.text.x= element_text(size=12, angle=0),
		 axis.ticks.x=element_line(),
		 legend.position="left",
		 strip.background=element_rect(fill = "white",
		 colour = "white"))
		 


######## FOR 2005 ONWARDS ############
## grants awarded	
# years<-c("2005-2006","2006-2007","2007-2008","2008-2009","2009-2010","2010-2011","2011-2012","2012-2013","2013-2014","2014-2015")
df$Year.num<-as.numeric(df$Year)
df2005<-df[df$Year.num>2004,]


g1<-ggplot(df2005[df2005$Grant=="Grants Awarded (n)",], aes(Year, value, group=1)) +
labs (y= "Grants Awarded (n)\n ", col="", x="", title="Number and Value of NSERC Discovery Grants Awarded \n(2005-2014)")  +
scale_x_discrete(labels=unique(df2005$Year[df2005$Grant=="Grants Awarded (n)"])) +
geom_point(stat='summary', fun.y=sum, size=0) +
stat_summary(fun.y=sum, geom="line",size=2, col=cols[1]) + 
scale_y_continuous(labels=comma) +
theme(axis.title.y=element_text(vjust=0.9),
	# panel.grid.minor =   element_line(colour = "grey",size=0.25),
        # panel.grid.major =   element_line(colour = "grey",size=0.25),
		legend.position="none",
		 axis.text.y= element_text(size=12),
		 axis.text.x= element_text(size=0, angle=0),
		 axis.ticks.x=element_blank(),
		 legend.position="left",
		 strip.background=element_rect(fill = "white",
		 colour = "white"), 
 plot.margin=unit(c(1,1,-0.2,1), "cm"))

# average award value
g2<- ggplot(df2005[df2005$Grant=="Average Award Value (Constant 2015 $)",], aes(Year, value, group=1)) +
labs (y= "Average Award Value \n(Constant 2015 $)", col="", x="")+#, title="Number and Value of NSERC Discovery Grants Awarded \n(2005-2014)")  +
scale_x_discrete(labels=unique(df2005$Year[df2005$Grant=="Grants Awarded (n)"])) +
geom_point(stat='summary', fun.y=sum, size=0) +
stat_summary(fun.y=sum, geom="line",size=2, col=cols[2]) +
scale_y_continuous(labels=comma, breaks=seq(32000, 38000, 1000), limits=c(32000, 38000))+
theme(axis.title.y=element_text(vjust=0.9),
	# panel.grid.minor =   element_line(colour = "grey",size=0.25),
        # panel.grid.major =   element_line(colour = "grey",size=0.25),
		legend.position="none",
		 axis.text.y= element_text(size=12),
		 axis.text.x= element_text(size=12, angle=0),
		 axis.ticks.x=element_line(size=0),
		 legend.position="left",
		 strip.background=element_rect(fill = "white",
		 colour = "white"),
		  plot.margin=unit(c(-0.2,1,1,1), "cm")) 

grid.arrange(g1, g2)


ggplot(df2005[df2005$Grant=="Average Award Value (Constant 2015 $)",], aes(Year, value, group=1)) +
labs (y= "Average Award Value \n(Constant 2015 $)", col="", x="", title="Number and Value of NSERC Discovery Grants Awarded \n(2005-2014)")  +
scale_x_discrete(labels=unique(df2005$Year)) +
geom_point(stat='summary', fun.y=sum, size=0) +
stat_summary(fun.y=sum, geom="line",size=2, col=cols[2]) +
scale_y_continuous(labels=comma, breaks=seq(32000, 38000, 1000), limits=c(32000, 38000))+
theme(axis.title.y=element_text(vjust=0.9),
	# panel.grid.minor =   element_line(colour = "grey",size=0.25),
        # panel.grid.major =   element_line(colour = "grey",size=0.25),
		legend.position="none",
		 axis.text.y= element_text(size=12),
		 axis.text.x= element_text(size=12, angle=0),
		 axis.ticks.x=element_line(size=0),
		 legend.position="left",
		 strip.background=element_rect(fill = "white",
		 colour = "white"))
		 # plot.margin=unit(c(-0.2,1,1,1), "cm")) 

###### average award value
df.award<-df[df$Grant=="Average Award Value (Constant 2015 $)" | df$Grant =="Average Award Value (Current $)",]

ggplot(df.award, aes(Year, value, group=Grant, col=Grant)) +
labs (y= "Average Award Value", col="", x="", title="Average NSERC Discovery Award Value \n(1991-2014)")  +
scale_x_discrete(labels=unique(df.award$Year)) +
geom_point(stat='summary', fun.y=sum, size=0) +
stat_summary(fun.y=sum, geom="line",size=2) +
scale_y_continuous(labels=comma, breaks=seq(25000, 45000, 5000), limits=c(25000, 45000))+
theme(axis.title.y=element_text(vjust=0.9),
	# panel.grid.minor =   element_line(colour = "grey",size=0.25),
        # panel.grid.major =   element_line(colour = "grey",size=0.25),
		legend.position=c(0.9,0.9),
		legend.text=element_text(size=15),
		 axis.text.y= element_text(size=12),
		 axis.text.x= element_text(size=12, angle=0),
		 axis.ticks.x=element_line(size=0),
		 strip.background=element_rect(fill = "white",
		 colour = "white"),
		  plot.margin=unit(c(1,1,1,1), "cm")) +
scale_colour_manual( values=cols, labels=c("Constant 2015 $", "Current $"))

ggplot(df.award[df.award$Grant=="Average Award Value (Constant 2015 $)",], aes(Year, value, group=Grant, col=Grant)) +
labs (y= "Average Award Value (Constant 2015 $)", col="", x="", title="Average NSERC Discovery Award Value \n(1991-2014)")  +
scale_x_discrete(labels=unique(df.award$Year)) +
geom_point(stat='summary', fun.y=sum, size=0) +
stat_summary(fun.y=sum, geom="line",size=2) +
scale_y_continuous(labels=comma, breaks=seq(25000, 45000, 5000), limits=c(25000, 45000))+
theme(axis.title.y=element_text(vjust=0.9),
	# panel.grid.minor =   element_line(colour = "grey",size=0.25),
        # panel.grid.major =   element_line(colour = "grey",size=0.25),
		legend.position=c(0.9,0.9),
		legend.text=element_text(size=15),
		 axis.text.y= element_text(size=12),
		 axis.text.x= element_text(size=12, angle=0),
		 axis.ticks.x=element_line(size=0),
		 strip.background=element_rect(fill = "white",
		 colour = "white"),
		  plot.margin=unit(c(1,1,1,1), "cm")) +
scale_colour_manual( values=cols, labels=c("Constant 2015 $", "Current $"))




########################################################################
######## grant expediture ############
################################################################################


df.grant<-df[df$Grant=="Total Discovery Grant Award Expenditure (Constant 2015 $)" | df$Grant =="Total Discovery Grant Award Expenditure (Current $)",]
## for full time series
ggplot(df.grant, aes(Year, value, group=Grant, col=Grant)) +
labs (y= "Total NSERC Discovery Grant Expenditure", col="", x="", title="Total NSERC Discovery Grant Expenditure \n(1991-2014)")  +
scale_x_discrete(labels=unique(df.grant$Year)) +
geom_point(stat='summary', fun.y=sum, size=0) +
stat_summary(fun.y=sum, geom="line",size=2) +
scale_y_continuous(labels=comma, breaks=seq(150000000, 360000000, 50000000), limits=c(150000000, 360000000))+
theme(axis.title.y=element_text(vjust=0.9),
	# panel.grid.minor =   element_line(colour = "grey",size=0.25),
        # panel.grid.major =   element_line(colour = "grey",size=0.25),
		legend.position=c(0.9,0.4),
		legend.text=element_text(size=15),
		 axis.text.y= element_text(size=12),
		 axis.text.x= element_text(size=12, angle=0),
		 axis.ticks.x=element_line(size=0),
		 strip.background=element_rect(fill = "white",
		 colour = "white"),
		  plot.margin=unit(c(1,1,1,1), "cm")) +
scale_colour_manual( values=cols, labels=c("Constant 2015 $", "Current $"))

ggplot(df.grant[df.grant$Grant=="Total Discovery Grant Award Expenditure (Constant 2015 $)",], aes(Year, value, group=Grant, col=Grant)) +
labs (y= "Constant 2015 $", col="", x="", title="Total NSERC Discovery Grant Expenditure \n(1991-2014)")  +
scale_x_discrete(labels=unique(df.grant$Year)) +
geom_point(stat='summary', fun.y=sum, size=0) +
stat_summary(fun.y=sum, geom="line",size=2) +
scale_y_continuous(labels=comma, breaks=seq(150000000, 360000000, 50000000), limits=c(150000000, 360000000))+
theme(axis.title.y=element_text(vjust=0.9),
	# panel.grid.minor =   element_line(colour = "grey",size=0.25),
        # panel.grid.major =   element_line(colour = "grey",size=0.25),
		legend.position=c(0.9,0.4),
		legend.text=element_text(size=15),
		 axis.text.y= element_text(size=12),
		 axis.text.x= element_text(size=12, angle=0),
		 axis.ticks.x=element_line(size=0),
		 strip.background=element_rect(fill = "white",
		 colour = "white"),
		  plot.margin=unit(c(1,1,1,1), "cm")) +
scale_colour_manual( values=cols, labels=c("Constant 2015 $", "Current $"))


## for 2005 onwards
df.grant2005<-df.grant[df.grant$Year.num>2004,]

ggplot(df.grant2005, aes(Year, value, group=Grant, col=Grant)) +
labs (y= "Total NSERC Discovery Grant Expenditure", col="", x="", title="Total NSERC Discovery Grant Expenditure \n(2005-2014)")  +
scale_x_discrete(labels=unique(df.grant2005$Year)) +
geom_point(stat='summary', fun.y=sum, size=0) +
stat_summary(fun.y=sum, geom="line",size=2) +
scale_y_continuous(labels=comma, breaks=seq(300000000, 360000000, 10000000), limits=c(300000000, 360000000))+
theme(axis.title.y=element_text(vjust=0.9),
	# panel.grid.minor =   element_line(colour = "grey",size=0.25),
        # panel.grid.major =   element_line(colour = "grey",size=0.25),
		legend.position=c(0.9,0.4),
		legend.text=element_text(size=15),
		 axis.text.y= element_text(size=12),
		 axis.text.x= element_text(size=12, angle=0),
		 axis.ticks.x=element_line(size=0),
		 strip.background=element_rect(fill = "white",
		 colour = "white"),
		  plot.margin=unit(c(1,1,1,1), "cm")) +
scale_colour_manual( values=cols, labels=c("Constant 2015 $", "Current $"))

ggplot(df.grant2005[df.grant2005$Grant=="Total Discovery Grant Award Expenditure (Constant 2015 $)",], aes(Year, value, group=Grant, col=Grant)) +
labs (y= "Constant 2015 $", col="", x="", title="Total NSERC Discovery Grant Expenditure \n(1991-2014)")  +
scale_x_discrete(labels=unique(df.grant2005$Year)) +
geom_point(stat='summary', fun.y=sum, size=0) +
stat_summary(fun.y=sum, geom="line",size=2) +
scale_y_continuous(labels=comma, breaks=seq(340000000, 360000000, 5000000), limits=c(340000000, 360000000))+
theme(axis.title.y=element_text(vjust=0.9),
	# panel.grid.minor =   element_line(colour = "grey",size=0.25),
        # panel.grid.major =   element_line(colour = "grey",size=0.25),
		legend.position=c(0.9,0.4),
		legend.text=element_text(size=15),
		 axis.text.y= element_text(size=12),
		 axis.text.x= element_text(size=12, angle=0),
		 axis.ticks.x=element_line(size=0),
		 strip.background=element_rect(fill = "white",
		 colour = "white"),
		  plot.margin=unit(c(1,1,1,1), "cm")) +
scale_colour_manual( values=cols, labels=c("Constant 2015 $", "Current $"))




dev.off()