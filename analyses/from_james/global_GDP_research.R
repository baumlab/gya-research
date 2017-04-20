
### creating plots for Megan Dodd + Julia Baum for global young academy document


## Aim: plot changes to  1) Basic research (%GDP) and 
       #   2) GDE on R&D (%GDP) over time for
       # Canada, Australia, Netherlands, Israel, Poland, Spain, US.



 setwd("/Users/jpwrobinson/Dropbox/R_PROJECTS_DATA/VISUALISATIONS/global-young-academy-gdp")

 gdp<-read.csv("research_GDP_MDodd.csv")
gdp$GDP<-NULL

gdp$comb<-gdp$RE/gdp$GDRE


theme_set(theme_minimal())
ggplot(gdp, aes(Year, GDRE, col=Country)) + geom_point()
ggplot(gdp, aes(Year, GDRE, col=Country)) + geom_point() + facet_grid(Country~.)




## rearrange data frame 
require(tidyr)

gdp1<-gather(gdp, "Year", "Country")

ggplot(gdp1, aes(Year,value, col=variable)) + geom_point() + facet_grid(Country~., scales="free")
ggplot(gdp1, aes(Year,value, col=variable)) + geom_line() + facet_grid(Country~., scales="free")


### placing on different panels either 1) hides trend by setting to same scale on y-axis;
  ### or 2) having different scales is misleading.


## so need all data on 1 panel.
g1<-ggplot(gdp, aes(Year, GDRE, col=Country)) + geom_point() +theme(legend.position = "none") #+ facet_grid(Country~.)
g2<-ggplot(gdp, aes(Year, RE, col=Country)) + geom_point() + theme(legend.position = "none") #+ facet_grid(Country~.)
grid.arrange(g1, g2, nrow=1)


# pdf(file="research_GDP_GYA.pdf", height=7, width=7)
theme_set(theme_bw())
ggplot(gdp1, aes(Year,value, fill=variable)) + geom_bar(stat="identity") + 
facet_grid(Country~.) + theme(axis.title.y=element_text(vjust=0.9), axis.text.y= element_text(size=8), legend.position="left",strip.background=element_rect(fill = "white", colour = "white")) +
labs(x="Year", y="% GDP", fill="") + scale_fill_discrete(labels=c("GERD", "Basic research"))

ggplot(gdp1, aes(Year,value, fill=variable)) + geom_bar(stat="identity") + 
facet_grid(Country~., scales="free") + theme(axis.title.y=element_text(vjust=0.9), axis.text.y= element_text(size=8), legend.position="left",strip.background=element_rect(fill = "white", colour = "white")) +
labs(x="Year", y="% GDP", fill="") + scale_fill_discrete(labels=c("GERD", "Basic research"))

ggplot(gdp1, aes(Year,value, col=variable)) + geom_point() + 
facet_grid(Country~.) + theme(axis.title.y=element_text(vjust=0.9), axis.text.y= element_text(size=8), legend.position="left",strip.background=element_rect(fill = "white", colour = "white")) +
labs(x="Year", y="% GDP", colour="") + scale_colour_discrete(labels=c("GERD", "Basic research"))


ggplot(gdp1, aes(Year,value, col=variable)) + geom_point() + 
facet_grid(Country~., scales="free") + theme(axis.title.y=element_text(vjust=0.9), axis.text.y= element_text(size=8), legend.position="left",strip.background=element_rect(fill = "white", colour = "white")) +
labs(x="Year", y="% GDP", colour="") + scale_colour_discrete(labels=c("GERD", "Basic research"))
# dev.off()


## plot for RE/GDRE (email from megan 18th Aug)


pdf(file="research_RE_GDRE_prop.pdf", height=7, width=7)
theme_set(theme_bw())
ggplot(gdp1[gdp1$variable=="comb",], aes(Year,value, fill=variable)) + geom_bar(stat="identity") + 
facet_grid(Country~.) + theme(axis.title.y=element_text(vjust=0.9),legend.position="none", axis.text.y= element_text(size=8), legend.position="left",strip.background=element_rect(fill = "white", colour = "white")) +
labs(x="Year", y="RE as proportion of GDRE", fill="") + scale_fill_discrete(labels=c("GERD", "Basic research"))
dev.off()










vars <- data.frame(expand.grid(levels(gdp1$Country)))
colnames(vars) <- c("Country")
dat <- data.frame(x = rep(2002, 7), y = rep(0.5, 7), vars, labs=levels(gdp1$Country))

## change NAs to zeroes


## try area plot
ggplot(gdp1, aes(Year,value, fill=variable)) + geom_area(alpha=0.9,stat="identity") + scale_x_continuous(breaks=c(seq(1990, 2012, 2)),labels=c(seq(1990, 2012, 2)), minor_breaks=waiver(),limits=c(1990, 2013), expand = c(0, 0)) +
facet_grid(Country~., scales="free")  + labs(x="", y="% GDP")  + geom_text(aes(x, y, label=labs, group=NULL, fill=NULL),data=dat, col="white", fontface=2) +
theme(legend.position = "none",axis.line=element_line(colour="black", size=0.4, linetype="solid"), strip.text.y = element_text(size = 0, angle = 0))
