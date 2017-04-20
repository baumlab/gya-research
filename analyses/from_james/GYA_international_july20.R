

## Aim: make 5 plots
require(tidyr)
require(scales)
require(grid); require(gridExtra)
require(ggplot2)

### return to default ggplot colors


# cols <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
cols <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
cols2 <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#000000")


setwd("/Users/jpwrobinson/Google_Drive/R_PROJECTS_DATA/VISUALISATIONS/global-young-academy-gdp")
setwd("/Users/IMAC3/Google\ Drive/R_PROJECTS_DATA/VISUALISATIONS/global-young-academy-gdp")
theme_set(theme_minimal(base_size=14))

## colours for line plots

### default ggplot colors
gg_color_hue <- function(n) {
  hues = seq(15, 375, length=n+1)
  hcl(h=hues, l=65, c=100)[1:n]
}

cols<-gg_color_hue(16)

g8<-c("Canada", "France", "Germany", "Italy", "Japan", "Russia", "UK", "US")
topten<-c( "Israel", "Korea", "Japan", "Finland", "Sweden", "Taiwan", "Denmark", "Switzerland", "Germany", "Austria")
countries<-c(g8, topten)
countries<-countries[!duplicated(countries)]



## load in data
gerd<-read.csv("Data/part5-international-july/july20/GERD_gross.csv", header=TRUE, stringsAsFactors=TRUE, check.names=FALSE)
# gerd.fed<-read.csv("Data/part5-international-july/july20/GERD_federal.csv", header=TRUE, stringsAsFactors=TRUE, check.names=FALSE)
# gerd.bus<-read.csv("Data/part5-international-july/july20/GERD_business.csv", header=TRUE, stringsAsFactors=TRUE, check.names=FALSE)
basic<-read.csv("Data/part5-international-july/july20/basic_research.csv", header=TRUE, stringsAsFactors=TRUE, check.names=FALSE)
# res.tot<-read.csv("Data/part5-international-july/july20/number_researchers_total.csv", header=TRUE, stringsAsFactors=TRUE, check.names=FALSE)
# res.govt<-read.csv("Data/part5-international-july/july20/number_researchers_govt.csv", header=TRUE, stringsAsFactors=TRUE, check.names=FALSE)

pdf(file="GERD_july20.pdf", height=7, width=11)
## arrange data

## 1. For GERD plots
gerd<-gather(gerd, YEAR, value, -Country)
gerd<-gerd[!gerd$Country=="G8 Average",]
gerd$Country <- factor(gerd$Country, levels = countries)
gerd$YEAR<-as.numeric(gerd$YEAR)
gerd$type<-ifelse(gerd$Country%in%g8, "G8", "Top 10 spender")
gerd$value<-as.numeric(gerd$value)
gerd$data<-'Basic Research'
gerd.na<-gerd[!is.na(gerd$value),]

# gerd.fed<-gather(gerd.fed, YEAR, value, -Country)
# gerd.fed$Country <- factor(gerd.fed$Country, levels = c("Canada", "G7 Average", "OECD Total", "Australia", "Israel", "Netherlands", "Poland", "UK", "US"))
# gerd.fed$source <- "Federal"
# gerd.fed$YEAR<-as.numeric(gerd.fed$YEAR)
# gerd.fed.na<-gerd.fed[!is.na(gerd.fed$value),]


# gerd.bus<-gather(gerd.bus, YEAR, value, -Country)
# gerd.bus$Country <- factor(gerd.bus$Country, levels = c("Canada", "G7 Average", "OECD Total", "Australia", "Israel", "Netherlands", "Poland", "UK", "US"))
# gerd.bus$source <- "Industry"
# gerd.bus$YEAR<-as.numeric(gerd.bus$YEAR)
# gerd.bus.na<-gerd.bus[!is.na(gerd.bus$value),]

# gerd.com<-rbind(gerd.fed, gerd.bus)
# gerd.com$group<-paste(gerd.com$source, gerd.com$Country)


## 2. For basic research plot
basic<-gather(basic, YEAR, value, -Country)
basic$value<-as.numeric(basic$value)
basic<-basic[!basic$Country=="G8 Average",]
basic$Country <- factor(basic$Country, levels = countries)
basic$type<-ifelse(basic$Country%in%g8, "G8", "Top 10 spender")
basic$YEAR<-as.numeric(basic$YEAR)
basic$data<-"Gross Domestic (GERD)"
basic.na<-basic[!is.na(basic$value),]

# ## 3. For number of researchers
# res.tot<-gather(res.tot, YEAR, value, -Country)
# res.tot$Country <- factor(res.tot$Country, levels = c("Canada", "G7 Average", "OECD Average", "Australia", "Israel", "Netherlands", "Poland", "UK","US", "Spain"))
# res.tot$YEAR<-as.numeric(res.tot$YEAR)
# res.tot.na<-res.tot[!is.na(res.tot$value),]

# res.govt<-gather(res.govt, YEAR, value, -Country)
# res.govt$Country <- factor(res.govt$Country, levels = c("Canada", "G7 Average", "OECD Average", "Australia", "Israel", "Netherlands", "Poland", "UK","US", "Spain"))
# res.govt$YEAR<-as.numeric(res.govt$YEAR)
# res.govt.na<-res.govt[!is.na(res.govt$value),]


## now plot
gdp<-rbind(gerd, basic)


## 1a. GERD full
ggplot(gdp, aes(YEAR, value, col=Country)) + geom_line(size=1, aes(linetype=type)) + facet_grid(data~type, scale="free_y") +
  labs(y="Expenditure (% of GDP)", x="") +
# geom_line(data=gerd[gerd$type=="Top 10 spender",],aes(group=Country), size=1, linetype="dashed")  +
# geom_line(data=gerd.na[!(gerd.na$Country=="Canada" | gerd.na$Country=="OECD Total" | gerd.na$Country=="G7 Average"),],aes(group=Country), size=0.75, linetype="dashed")  +
  scale_x_continuous(breaks=seq(2003, 2014, 1)) + #ylim(c(0, 5)) + 
  ggtitle("Expenditure on R&D (% of GDP)") + scale_colour_manual(limits=levels(gerd$variable), values=cols) + 
  theme(
    axis.text.x=element_text(angle=45), 
    legend.title=element_blank(), 
    legend.position="right",
    panel.background = element_rect(fill=NA))




# ### Figure 2 - basic research

# g2<-ggplot(basic, aes(YEAR, value, col=Country)) + geom_line(size=1, aes(linetype=type)) + 
#   labs(y="Expenditure (% of GDP)", x="") +
# # geom_line(data=basic.na[!(basic.na$Country=="Canada" | basic.na$Country=="OECD Total" | basic.na$Country=="G7 Average"),],aes(group=Country), size=0.25, linetype="dashed")  +
# # geom_line(data=basic[!(basic$Country=="Canada" | basic$Country=="OECD Total" | basic$Country=="G7 Average"),],aes(group=Country), size=0.75, linetype="dashed")  +
#   scale_x_continuous(breaks=seq(2003, 2015, 1)) + ylim(c(0, 5)) + ggtitle("Basic Research Expenditure 
# (% of GDP)") + scale_colour_manual(limits=levels(basic$variable), values=cols) + theme(axis.text.x=element_text(angle=45), legend.title=element_blank(), legend.position="right")



# grid.arrange(g1, g2)






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








