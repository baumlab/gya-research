

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

## colours for line plots

### default ggplot colors
gg_color_hue <- function(n) {
  hues = seq(15, 375, length=n+1)
  hcl(h=hues, l=65, c=100)[1:n]
}

cols<-gg_color_hue(9)




## load in data
gerd<-read.csv("Data/part5-international-july/GERD_gross.csv", header=TRUE, stringsAsFactors=TRUE, check.names=FALSE)
gerd.fed<-read.csv("Data/part5-international-july/GERD_federal.csv", header=TRUE, stringsAsFactors=TRUE, check.names=FALSE)
gerd.bus<-read.csv("Data/part5-international-july/GERD_business.csv", header=TRUE, stringsAsFactors=TRUE, check.names=FALSE)
basic<-read.csv("Data/part5-international-july/basic_research.csv", header=TRUE, stringsAsFactors=TRUE, check.names=FALSE)
res.tot<-read.csv("Data/part5-international-july/number_researchers_total.csv", header=TRUE, stringsAsFactors=TRUE, check.names=FALSE)
res.govt<-read.csv("Data/part5-international-july/number_researchers_govt.csv", header=TRUE, stringsAsFactors=TRUE, check.names=FALSE)

pdf(file="GERD_july16.pdf", height=7, width=11)
## arrange data

## 1. For GERD plots
gerd<-gather(gerd, YEAR, value, -Country)
gerd$Country <- factor(gerd$Country, levels = c("Canada", "G7 Average", "OECD Total", "Australia", "Israel", "Netherlands", "Poland", "UK", "US"))
gerd$YEAR<-as.numeric(gerd$YEAR)
gerd.na<-gerd[!is.na(gerd$value),]

gerd.fed<-gather(gerd.fed, YEAR, value, -Country)
gerd.fed$Country <- factor(gerd.fed$Country, levels = c("Canada", "G7 Average", "OECD Total", "Australia", "Israel", "Netherlands", "Poland", "UK", "US"))
gerd.fed$source <- "Federal"
gerd.fed$YEAR<-as.numeric(gerd.fed$YEAR)
gerd.fed.na<-gerd.fed[!is.na(gerd.fed$value),]


gerd.bus<-gather(gerd.bus, YEAR, value, -Country)
gerd.bus$Country <- factor(gerd.bus$Country, levels = c("Canada", "G7 Average", "OECD Total", "Australia", "Israel", "Netherlands", "Poland", "UK", "US"))
gerd.bus$source <- "Industry"
gerd.bus$YEAR<-as.numeric(gerd.bus$YEAR)
gerd.bus.na<-gerd.bus[!is.na(gerd.bus$value),]

# gerd.com<-rbind(gerd.fed, gerd.bus)
# gerd.com$group<-paste(gerd.com$source, gerd.com$Country)


## 2. For basic research plot
basic<-gather(basic, YEAR, value, -Country)
basic$value<-as.numeric(basic$value)
basic$Country <- factor(basic$Country, levels = c("Canada", "G7 Average", "OECD Total", "Australia", "Israel", "Netherlands", "Poland", "UK", "US"))
basic$YEAR<-as.numeric(basic$YEAR)
basic.na<-basic[!is.na(basic$value),]

## 3. For number of researchers
res.tot<-gather(res.tot, YEAR, value, -Country)
res.tot$Country <- factor(res.tot$Country, levels = c("Canada", "G7 Average", "OECD Average", "Australia", "Israel", "Netherlands", "Poland", "UK","US", "Spain"))
res.tot$YEAR<-as.numeric(res.tot$YEAR)
res.tot.na<-res.tot[!is.na(res.tot$value),]

res.govt<-gather(res.govt, YEAR, value, -Country)
res.govt$Country <- factor(res.govt$Country, levels = c("Canada", "G7 Average", "OECD Average", "Australia", "Israel", "Netherlands", "Poland", "UK","US", "Spain"))
res.govt$YEAR<-as.numeric(res.govt$YEAR)
res.govt.na<-res.govt[!is.na(res.govt$value),]


## now plot


## 1a. GERD full
ggplot(gerd[gerd$Country=="Canada" | gerd$Country=="OECD Total" | gerd$Country=="G7 Average",], aes(YEAR, value, col=Country)) + geom_line(size=1) + 
  labs(y="Expenditure (% of GDP)", x="", col="") +
geom_line(data=gerd[!(gerd$Country=="Canada" | gerd$Country=="OECD Total" | gerd$Country=="G7 Average"),],aes(group=Country), size=0.25, linetype="dashed")  +
geom_line(data=gerd.na[!(gerd.na$Country=="Canada" | gerd.na$Country=="OECD Total" | gerd.na$Country=="G7 Average"),],aes(group=Country), size=0.75, linetype="dashed")  +
  scale_x_continuous(breaks=seq(1981, 2013, 1)) + ylim(c(0, 5)) + ggtitle("Gross Domestic Expenditure on R&D 
(% of GDP)") + scale_colour_manual(limits=levels(gerd$variable), values=cols) + theme(axis.text.x=element_text(angle=45))

gerd04<-gerd[gerd$YEAR>2003,]
gerd04.na<-gerd.na[gerd.na$YEAR>2003,]

ggplot(gerd04[gerd04$Country=="Canada" | gerd04$Country=="OECD Total" | gerd04$Country=="G7 Average",], aes(YEAR, value, col=Country)) + geom_line(size=1) + 
  labs(y="Expenditure (% of GDP)", x="", col="") +
geom_line(data=gerd04[!(gerd04$Country=="Canada" | gerd04$Country=="OECD Total" | gerd04$Country=="G7 Average"),],aes(group=Country), size=0.25, linetype="dashed")  +
geom_line(data=gerd04.na[!(gerd04.na$Country=="Canada" | gerd04.na$Country=="OECD Total" | gerd04.na$Country=="G7 Average"),],aes(group=Country), size=0.75, linetype="dashed")  +
  scale_x_continuous(breaks=seq(2004, 2013, 1)) + ylim(c(0, 5)) + ggtitle("Gross Domestic Expenditure on R&D 
(% of GDP)") + scale_colour_manual(limits=levels(gerd04$variable), values=cols)



## 1b. GERD fed + business
# ggplot(gerd.com[gerd.com$Country=="Canada" | gerd.com$Country=="OECD Total" | gerd.com$Country=="G7 Average" & gerd.com$source=="Federal",]
# 	, aes(YEAR, value, col=Country, group=group)) + 
# 	geom_line(size=1) + 
#   	geom_line(data=gerd.com[!(gerd.com$Country=="Canada" | gerd.com$Country=="OECD Total" | gerd.com$Country=="G7 Average") & gerd.com$source=="Federal",],aes(group=group), size=0.5)  +
#   	geom_line(data=gerd.com[!(gerd.com$Country=="Canada" | gerd.com$Country=="OECD Total" | gerd.com$Country=="G7 Average" & gerd.com$source=="Federal"),],aes(group=group), size=0.5, linetype="dashed")  +
#   	geom_line(data=gerd.com[gerd.com$Country=="Canada" | gerd.com$Country=="OECD Total" | gerd.com$Country=="G7 Average" & !(gerd.com$source=="Federal"),],aes(group=group), size=1, linetype="dashed")  +
#   scale_x_continuous(breaks=seq(1981, 2013, 1)) + ylim(c(0, 70)) + ggtitle("Gross Domestic Expenditure on R&D 
# (% of GDP)") + scale_colour_manual(limits=levels(gerd.com$variable), values=cols) + labs(y="Expenditure (% of GDP)", x="", col="") 

g1<-ggplot(gerd.fed[gerd.fed$Country=="Canada" | gerd.fed$Country=="OECD Total" | gerd.fed$Country=="G7 Average",], aes(YEAR, value, col=Country)) + geom_line(size=1) + 
  labs(y="Proportion of total GERD (% of GDP)", x="", col="") +
geom_line(data=gerd.fed[!(gerd.fed$Country=="Canada" | gerd.fed$Country=="OECD Total" | gerd.fed$Country=="G7 Average"),],aes(group=Country), size=0.25, linetype="dashed")  +
geom_line(data=gerd.fed.na[!(gerd.fed.na$Country=="Canada" | gerd.fed.na$Country=="OECD Total" | gerd.fed.na$Country=="G7 Average"),],aes(group=Country), size=0.75, linetype="dashed")  +
  scale_x_continuous(breaks=seq(1981, 2013, 1)) + ylim(c(10, 80)) +  scale_colour_manual(limits=levels(gerd.fed$variable), values=cols)+ 
  theme(plot.margin=unit(c(0,1,0,0), 'cm'), axis.text.x=element_blank()) + guides(col=FALSE) + annotate("text", 2000, 80, hjust=0, label="(a) Funded by Federal Government")

g2<-ggplot(gerd.bus[gerd.bus$Country=="Canada" | gerd.bus$Country=="OECD Total" | gerd.bus$Country=="G7 Average",], aes(YEAR, value, col=Country)) + geom_line(size=1) + 
  labs(y="Proportion of total GERD (% of GDP)", x="", col="") +
geom_line(data=gerd.bus[!(gerd.bus$Country=="Canada" | gerd.bus$Country=="OECD Total" | gerd.bus$Country=="G7 Average"),],aes(group=Country), size=0.25, linetype="dashed")  +
geom_line(data=gerd.bus.na[!(gerd.bus.na$Country=="Canada" | gerd.bus.na$Country=="OECD Total" | gerd.bus.na$Country=="G7 Average"),],aes(group=Country), size=0.75, linetype="dashed")  +
  scale_x_continuous(breaks=seq(1981, 2013, 1)) + ylim(c(10, 80)) + scale_colour_manual(limits=levels(gerd.bus$variable), values=cols) +
  theme(plot.margin=unit(c(-0.2,1,0,0), "cm"), legend.position="bottom") + guides(col=guide_legend(nrow=1,byrow=TRUE)) + annotate("text", 2000, 80, hjust=0, label="(b) Funded by Business Enterprises")

gerd.fed04<-gerd.fed[gerd.fed$YEAR>2003,]
gerd.fed04.na<-gerd.fed.na[gerd.fed.na$YEAR>2003,]

gerd.bus04<-gerd.bus[gerd.bus$YEAR>2003,]
gerd.bus04.na<-gerd.bus.na[gerd.bus.na$YEAR>2003,]

g3<-ggplot(gerd.fed04[gerd.fed04$Country=="Canada" | gerd.fed04$Country=="OECD Total" | gerd.fed04$Country=="G7 Average",], aes(YEAR, value, col=Country)) + geom_line(size=1) + 
  labs(y="Proportion of total GERD (% of GDP)", x="", col="") +
geom_line(data=gerd.fed04[!(gerd.fed04$Country=="Canada" | gerd.fed04$Country=="OECD Total" | gerd.fed04$Country=="G7 Average"),],aes(group=Country), size=0.25, linetype="dashed")  +
geom_line(data=gerd.fed04.na[!(gerd.fed04.na$Country=="Canada" | gerd.fed04.na$Country=="OECD Total" | gerd.fed04.na$Country=="G7 Average"),],aes(group=Country), size=0.75, linetype="dashed")  +
  scale_x_continuous(breaks=seq(2004, 2013, 1)) + ylim(c(10, 80)) +  scale_colour_manual(limits=levels(gerd.fed04$variable), values=cols)+ 
  theme(plot.margin=unit(c(0,1,0,0), 'cm'), axis.text.x=element_blank()) + guides(col=FALSE) + annotate("text", 2004, 80, hjust=0, label="(a) Funded by Federal Government")

g4<-ggplot(gerd.bus04[gerd.bus04$Country=="Canada" | gerd.bus04$Country=="OECD Total" | gerd.bus04$Country=="G7 Average",], aes(YEAR, value, col=Country)) + geom_line(size=1) + 
  labs(y="Proportion of total GERD (% of GDP)", x="", col="") +
geom_line(data=gerd.bus04[!(gerd.bus04$Country=="Canada" | gerd.bus04$Country=="OECD Total" | gerd.bus04$Country=="G7 Average"),],aes(group=Country), size=0.25, linetype="dashed")  +
geom_line(data=gerd.bus04.na[!(gerd.bus04.na$Country=="Canada" | gerd.bus04.na$Country=="OECD Total" | gerd.bus04.na$Country=="G7 Average"),],aes(group=Country), size=0.75, linetype="dashed")  +
  scale_x_continuous(breaks=seq(2004, 2013, 1)) + ylim(c(10, 80)) + scale_colour_manual(limits=levels(gerd.bus04$variable), values=cols) +
  theme(plot.margin=unit(c(-0.2,1,0,0), "cm"), legend.position="bottom") + guides(col=guide_legend(nrow=1,byrow=TRUE)) + annotate("text", 2004, 80, hjust=0, label="(b) Funded by Business Enterprises")


grid.arrange(g1, g2, layout_matrix = rbind(1,2), top=textGrob("Gross Expenditure on R&D", gp=gpar(fontsize=15,font=8)))
grid.arrange(g3, g4, layout_matrix = rbind(1,2), top=textGrob("Gross Expenditure on R&D", gp=gpar(fontsize=15,font=8)))



### Figure 2 - basic research
cols<-gg_color_hue(10)
cols<-cols[-2]
ggplot(basic.na[basic.na$Country=="Canada" | basic.na$Country=="OECD Total" | basic.na$Country=="G7 Average",], aes(YEAR, value, col=Country)) + geom_line(size=1) + 
  labs(y="Expenditure (% of GDP)", x="", col="") +
geom_line(data=basic.na[!(basic.na$Country=="Canada" | basic.na$Country=="OECD Total" | basic.na$Country=="G7 Average"),],aes(group=Country), size=0.25, linetype="dashed")  +
geom_line(data=basic[!(basic$Country=="Canada" | basic$Country=="OECD Total" | basic$Country=="G7 Average"),],aes(group=Country), size=0.75, linetype="dashed")  +
  scale_x_continuous(breaks=seq(1981, 2013, 1)) + ylim(c(0.1, 0.7)) + ggtitle("Basic Research Expenditure 
(% of GDP)") + scale_colour_manual(limits=levels(basic.na$variable), values=cols) + theme(axis.text.x=element_text(angle=45))

basic04<-basic[basic$YEAR>2003,]
basic.na04<-basic.na[basic.na$YEAR>2003,]

ggplot(basic.na04[basic.na04$Country=="Canada" | basic.na04$Country=="OECD Total" | basic.na04$Country=="G7 Average",], aes(YEAR, value, col=Country)) + geom_line(size=1) + 
  labs(y="Expenditure (% of GDP)", x="", col="") +
geom_line(data=basic.na04[!(basic.na04$Country=="Canada" | basic.na04$Country=="OECD Total" | basic.na04$Country=="G7 Average"),],aes(group=Country), size=0.25, linetype="dashed")  +
geom_line(data=basic04[!(basic04$Country=="Canada" | basic04$Country=="OECD Total" | basic04$Country=="G7 Average"),],aes(group=Country), size=0.75, linetype="dashed")  +
  scale_x_continuous(breaks=seq(2004, 2013, 1)) + ylim(c(0.1, 0.7)) + ggtitle("Basic Research Expenditure 
(% of GDP)") + scale_colour_manual(limits=levels(basic.na04$variable), values=cols)

## Figure 3 - number of researchers

  ## total
  cols<-gg_color_hue(10)

ggplot(res.tot.na[res.tot.na$Country=="Canada" | res.tot.na$Country=="OECD Average" | res.tot.na$Country=="G7 Average",], aes(YEAR, value, col=Country)) + geom_line(size=1) + 
  labs(y="Researchers employed  (per 1000 employed)", x="", col="") +
geom_line(data=res.tot.na[!(res.tot.na$Country=="Canada" | res.tot.na$Country=="OECD Average" | res.tot.na$Country=="G7 Average"),],aes(group=Country), size=0.25, linetype="dashed")  +
geom_line(data=res.tot[!(res.tot$Country=="Canada" | res.tot$Country=="OECD Average" | res.tot$Country=="G7 Average"),],aes(group=Country), size=0.75, linetype="dashed")  +
  scale_x_continuous(breaks=seq(1981, 2013, 1)) + ylim(c(0, 20)) + ggtitle("Researchers Employed") + scale_colour_manual(limits=levels(res.tot.na$variable), values=cols)


res.tot04<-res.tot[res.tot$YEAR>2003,]
res.tot04.na<-res.tot.na[res.tot.na$YEAR>2003,]

ggplot(res.tot04.na[res.tot04.na$Country=="Canada" | res.tot04.na$Country=="OECD Average" | res.tot04.na$Country=="G7 Average",], aes(YEAR, value, col=Country)) + geom_line(size=1) + 
  labs(y="Researchers employed  (per 1000 employed)", x="", col="") +
geom_line(data=res.tot04.na[!(res.tot04.na$Country=="Canada" | res.tot04.na$Country=="OECD Average" | res.tot04.na$Country=="G7 Average"),],aes(group=Country), size=0.25, linetype="dashed")  +
geom_line(data=res.tot04[!(res.tot04$Country=="Canada" | res.tot04$Country=="OECD Average" | res.tot04$Country=="G7 Average"),],aes(group=Country), size=0.75, linetype="dashed")  +
  scale_x_continuous(breaks=seq(2004, 2013, 1)) + ylim(c(0, 20)) + ggtitle("Researchers Employed") + scale_colour_manual(limits=levels(res.tot04.na$variable), values=cols)


## government
ggplot(res.govt.na[res.govt.na$Country=="Canada" | res.govt.na$Country=="OECD Average" | res.govt.na$Country=="G7 Average",], aes(YEAR, value, col=Country)) + geom_line(size=1) + 
  labs(y="Proportion of Total Researchers (%)", x="", col="") +
geom_line(data=res.govt.na[!(res.govt.na$Country=="Canada" | res.govt.na$Country=="OECD Average" | res.govt.na$Country=="G7 Average"),],aes(group=Country), size=0.25, linetype="dashed")  +
geom_line(data=res.govt[!(res.govt$Country=="Canada" | res.govt$Country=="OECD Average" | res.govt$Country=="G7 Average"),],aes(group=Country), size=0.75, linetype="dashed")  +
  scale_x_continuous(breaks=seq(1981, 2013, 1)) + ylim(c(0, 30)) + ggtitle("Government Researchers") + scale_colour_manual(limits=levels(res.govt.na$variable), values=cols)


res04.govt<-res.govt[res.govt$YEAR>2003,]
res04.govt.na<-res.govt.na[res.govt.na$YEAR>2003,]

ggplot(res04.govt.na[res04.govt.na$Country=="Canada" | res04.govt.na$Country=="OECD Average" | res04.govt.na$Country=="G7 Average",], aes(YEAR, value, col=Country)) + geom_line(size=1) + 
  labs(y="Proportion of Total Researchers (%)", x="", col="") +
geom_line(data=res04.govt.na[!(res04.govt.na$Country=="Canada" | res04.govt.na$Country=="OECD Average" | res04.govt.na$Country=="G7 Average"),],aes(group=Country), size=0.25, linetype="dashed")  +
geom_line(data=res04.govt[!(res04.govt$Country=="Canada" | res04.govt$Country=="OECD Average" | res04.govt$Country=="G7 Average"),],aes(group=Country), size=0.75, linetype="dashed")  +
  scale_x_continuous(breaks=seq(2004, 2013, 1)) + ylim(c(0, 25)) + ggtitle("Government Researchers") + scale_colour_manual(limits=levels(res04.govt.na$variable), values=cols)





dev.off()








