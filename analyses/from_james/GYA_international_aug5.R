

## Aim: make 5 plots
require(tidyr)
require(scales)
require(grid); require(gridExtra)
require(ggplot2)
require(RColorBrewer)
### return to default ggplot colors


# cols <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
cols <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
cols2 <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#000000")


setwd("/Users/jpwrobinson/Google_Drive/R_PROJECTS_DATA/VISUALISATIONS/global-young-academy-gdp")
setwd("/Users/IMAC3/Google\ Drive/R_PROJECTS_DATA/VISUALISATIONS/global-young-academy-gdp")
theme_set(theme_minimal(base_size=14))

## colours for line plots

# ## default ggplot colors
# gg_color_hue <- function(n) {
#   hues = seq(15, 375, length=n+1)
#   hcl(h=hues, l=65, c=100)[1:n]
# }

# cols<-gg_color_hue(17)

### colour palette

myColors<-c((brewer.pal(12, "Paired")), "")
myColors<-myColors[-6]
myColors<-c( myColors[1], '#E31A1C', myColors[2:10])
# myColors<-(myColors[c(9, 7, 6, 4, 2)])
colScale <- scale_color_manual(name = "country.col",values = myColors)


g8<-c("Canada", "France", "Germany", "Italy", "Japan", "Russia", "UK", "US", "G8 Average")
topten<-c( "Israel", "Korea", "Japan", "Finland", "Sweden", "Taiwan", "Denmark", "Germany", "Austria", 'OECD Average')
countries<-c(g8, topten)
countries<-countries[!duplicated(countries)]



## load in data
gerd<-read.csv("Data/part5-international-july/aug5/GERD_gross.csv", header=TRUE, stringsAsFactors=TRUE, check.names=FALSE)
gerd.source<-read.csv("Data/part5-international-july/aug5/GERD_source.csv", header=TRUE, stringsAsFactors=TRUE, check.names=FALSE)
basic<-read.csv("Data/part5-international-july/aug5/basic_research.csv", header=TRUE, stringsAsFactors=TRUE, check.names=FALSE)
res.tot<-read.csv("Data/part5-international-july/aug5/number_researchers_total.csv", header=TRUE, stringsAsFactors=TRUE, check.names=FALSE)


pdf(file="figures/GERD_aug5.pdf", height=7, width=11)
## arrange data

## 1a. For GERD plots - total
gerd<-gather(gerd, YEAR, value, -Country, -panel)
gerd$Country<-revalue(gerd$Country, c("Taiwan (Chinese Taipei)"="Taiwan"))
gerd$Country <- factor(gerd$Country, levels = countries)
gerd$YEAR<-as.numeric(gerd$YEAR)
gerd$type<-ifelse(gerd$Country%in%g8, "G8", "Top 10 spender")
gerd$value<-as.numeric(gerd$value)
gerd$data<-'Basic Research'
gerd$bold<-ifelse((grepl( "Average",gerd$Country)| grepl("Canada", gerd$Country)), "Average", "Country")
gerd$panel<-revalue(gerd$panel, c("left"="GERD Top Ten", 'right'='G8'))



## 1b. For GERD plots - fed and gov.
gerd.source<-gather(gerd.source, YEAR, value, -Country, -panel)
gerd.source$Country<-revalue(gerd.source$Country, c("Taiwan (Chinese Taipei)"="Taiwan"))
gerd.source$Country <- factor(gerd.source$Country, levels = countries)
gerd.source$YEAR<-as.numeric(gerd.source$YEAR)
gerd.source$type<-ifelse(gerd.source$Country%in%g8, "G8", "Top 10 spender")
gerd.source$value<-as.numeric(gerd.source$value)
gerd.source.na<-gerd.source[!is.na(gerd.source$value),]
gerd.source$bold<-ifelse((grepl( "Average",gerd.source$Country)| grepl("Canada", gerd.source$Country)), "Average", "Country")
gerd.source$panel<-revalue(gerd.source$panel, c("lu"="Industry - GERD Top Ten", 'll'='Government - GERD Top Ten', 'ru'='Industry - G8', 'rl'='Government - G8'))
gerd.source$panel<-factor(gerd.source$panel, levels(gerd.source$panel)[c(1,3,2,4)])
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
basic<-gather(basic, YEAR, value, -Country, -panel)
basic$value<-as.numeric(basic$value)
basic$Country<-revalue(basic$Country, c("Taiwan (Chinese Taipei)"="Taiwan"))
basic$Country <- factor(basic$Country, levels = countries)
basic$type<-ifelse(basic$Country%in%g8, "G8", "Top 10 spender")
basic$YEAR<-as.numeric(basic$YEAR)
basic.na<-basic[!is.na(basic$value),]
basic$bold<-ifelse(grepl( "Average",basic$Country), "Average", "Country")
basic$bold<-ifelse((grepl( "Average",basic$Country)| grepl("Canada", basic$Country)), "Average", "Country")
basic$panel<-revalue(basic$panel, c("left"="GERD Top Ten", 'right'='G8'))


# ## 3. For number of researchers

res.tot<-gather(res.tot, YEAR, value, -Country, -panel)
res.tot$value<-as.numeric(res.tot$value)
res.tot$Country<-revalue(res.tot$Country, c("Taiwan (Chinese Taipei)"="Taiwan"))
res.tot$Country <- factor(res.tot$Country, levels = countries)
res.tot$type<-ifelse(res.tot$panel %in% c("lu", "ur"), "employed", "govt")
res.tot$YEAR<-as.numeric(res.tot$YEAR)
res.tot$panel<-revalue(res.tot$panel, c("lu"="GERD Top Ten", 'lb'='GERD Top Ten', 'ur'='G8', 'br'='G8'))
res.tot$bold<-ifelse((grepl( "Average",res.tot$Country)| grepl("Canada", res.tot$Country)), "Average", "Country")
res.tot$panel<-factor(res.tot$panel, levels(res.tot$panel)[c(2,1)])
res.govt<-res.tot[res.tot$type=="govt",]
res.res<-res.tot[res.tot$type=="employed",]

## now plots



## 1a. GERD full
ggplot(gerd[gerd$bold=="Country" & !(gerd$Country=="Canada"),], aes(YEAR, value, col=Country, linetype=Country), size=1) + 
      geom_line(data=gerd[gerd$bold=="Average",], col='black',aes(group=Country,size="Average"), linetype="dashed") + 
      geom_line(data=gerd[gerd$Country=="Canada",], aes(group=Country, size="Average"))+#, col="#E31A1C", size='1.5')+
      geom_line() +
  facet_grid(~panel, scale="free_y") +
  labs(y="Expenditure (% of GDP)", x="") +
# geom_line(data=gerd[gerd$type=="Top 10 spender",],aes(group=Country), size=1, linetype="dashed")  +
# geom_line(data=gerd.na[!(gerd.na$Country=="Canada" | gerd.na$Country=="OECD Total" | gerd.na$Country=="G7 Average"),],aes(group=Country), size=0.75, linetype="dashed")  +
  scale_x_continuous(breaks=seq(2003, 2014, 1)) + #ylim(c(0, 5)) + 
  scale_y_continuous(breaks=seq(1, 4.5, 0.5)) +
  scale_linetype_manual(values = c(rep("solid", 9), rep("dashed", 6))) +
  scale_color_manual(values = c(myColors[1:9], myColors[1:6])) +
  scale_size_manual(values=c(1.25, 1.5))+
   #scale_colour_manual(limits=levels(gerd$variable), values=cols) + 
  theme(
    axis.text.x=element_text(angle=0), 
    legend.title=element_blank(), 
    legend.position="right",
    panel.background = element_rect(fill=NA)) 






# ### Figure 1b - GERD source

ggplot(gerd.source[gerd.source$bold=="Country" & !(gerd.source$Country=="Canada"),], aes(YEAR, value, col=Country, linetype=Country), size=1) + 
      geom_line(data=gerd.source[gerd.source$bold=="Average",], col='black',aes(group=Country,size="Average"), linetype="dashed") + 
      geom_line(data=gerd.source[gerd.source$Country=="Canada",], aes(group=Country, size="Average"))+#, col="#E31A1C", size='1.5')+
      geom_line() +
  facet_wrap(~panel) +
  labs(y="Contribution (% of GDP)", x="") +
# geom_line(data=gerd.source[gerd.source$type=="Top 10 spender",],aes(group=Country), size=1, linetype="dashed")  +
# geom_line(data=gerd.source.na[!(gerd.source.na$Country=="Canada" | gerd.source.na$Country=="OECD Total" | gerd.source.na$Country=="G7 Average"),],aes(group=Country), size=0.75, linetype="dashed")  +
  scale_x_continuous(breaks=seq(2003, 2014, 1)) + #ylim(c(0, 5)) + 
  scale_y_continuous(breaks=seq(10, 80, 5)) +
  scale_linetype_manual(values = c(rep("solid", 9), rep("dashed", 6))) +
scale_color_manual(values = c(myColors[1:9], myColors[1:6])) +
  scale_size_manual(values=1.25)+
   #scale_colour_manual(limits=levels(gerd.source$variable), values=cols) + 
  theme(
    axis.text.x=element_text(angle=0), 
    legend.title=element_blank(), 
    legend.position="right",
    panel.background = element_rect(fill=NA)) 







## Figure 2 - basic researhc

ggplot(basic[basic$bold=="Country" & !(basic$Country=="Canada"),], aes(YEAR, value, col=Country, linetype=Country), size=1) + 
      geom_line(data=basic[basic$bold=="Average",], col='black',aes(group=Country,size="Average"), linetype="dashed") + 
      geom_line(data=basic[basic$Country=="Canada",], aes(group=Country, size="Average"))+#, col="#E31A1C", size='1.5')+
      geom_line() +
  facet_grid(~panel, scale="free_y") +
  labs(y="Expenditure (% of GDP)", x="") +
# geom_line(data=basic[basic$type=="Top 10 spender",],aes(group=Country), size=1, linetype="dashed")  +
# geom_line(data=basic.na[!(basic.na$Country=="Canada" | basic.na$Country=="OECD Total" | basic.na$Country=="G7 Average"),],aes(group=Country), size=0.75, linetype="dashed")  +
  scale_x_continuous(breaks=seq(2003, 2014, 1)) + #ylim(c(0, 5)) + 
  scale_y_continuous(breaks=seq(0, 0.7, 0.1)) +
  scale_linetype_manual(values = c(rep("solid", 9), rep("dashed", 6))) +
scale_color_manual(values = c(myColors[1:9], myColors[1:6])) +
  scale_size_manual(values=1.25)+
   #scale_colour_manual(limits=levels(basic$variable), values=cols) + 
  theme(
    axis.text.x=element_text(angle=0), 
    legend.title=element_blank(), 
    legend.position="right",
    panel.background = element_rect(fill=NA)) 





## Figure 3 - number of researchers
g1<-ggplot(res.res[res.res$bold=="Country" & !(res.res$Country=="Canada"),], aes(YEAR, value, col=Country, linetype=Country), size=1) + 
      geom_line(data=res.res[res.res$bold=="Average",], col='black',aes(group=Country,size="Average"), linetype="dashed") + 
      geom_line(data=res.res[res.res$Country=="Canada",], aes(group=Country, size="Average"))+#, col="#E31A1C", size='1.5')+
      geom_line() +
  facet_grid(~panel, scale="free_y") +
  labs(y="Researchers Employed\n (per 1000 employed)", x="") +
# geom_line(data=res.res[res.res$type=="Top 10 spender",],aes(group=Country), size=1, linetype="dashed")  +
# geom_line(data=res.res.na[!(res.res.na$Country=="Canada" | res.res.na$Country=="OECD Total" | res.res.na$Country=="G7 Average"),],aes(group=Country), size=0.75, linetype="dashed")  +
  scale_x_continuous(breaks=seq(2003, 2014, 1)) + #ylim(c(0, 5)) + 
  scale_y_continuous(breaks=seq(0, 20, 5)) +
  scale_linetype_manual(values = c(rep("solid", 9), rep("dashed", 6))) +
scale_color_manual(values = c(myColors[1:9], myColors[1:6])) +
  scale_size_manual(values=1.25)+
   #scale_colour_manual(limits=levels(res.res$variable), values=cols) + 
  theme(
    axis.text.x=element_blank(), 
    legend.title=element_blank(), 
    legend.position="right",
    panel.background = element_rect(fill=NA),
      plot.margin=unit(c(0,0.5,-0.25,0), "cm"))




g2<-ggplot(res.govt[res.govt$bold=="Country" & !(res.govt$Country=="Canada") ,], aes(YEAR, value, col=Country, linetype=Country), size=1) + 
      geom_line(data=res.govt[res.govt$bold=="Average",], col='black',aes(group=Country,size="Average"), linetype="dashed") + 
      geom_line(data=res.govt[res.govt$Country=="Canada",], aes(group=Country, size="Average"))+#, col="#E31A1C", size='1.5')+
      geom_line() +
  facet_grid(~panel, scale="free_y") +
  labs(y="Government researchers\n (% of researchers)", x="") +
# geom_line(data=res.govt[res.govt$type=="Top 10 spender",],aes(group=Country), size=1, linetype="dashed")  +
# geom_line(data=res.govt.na[!(res.govt.na$Country=="Canada" | res.govt.na$Country=="OECD Total" | res.govt.na$Country=="G7 Average"),],aes(group=Country), size=0.75, linetype="dashed")  +
  scale_x_continuous(breaks=seq(2003, 2014, 1)) + #ylim(c(0, 5)) + 
  scale_y_continuous(breaks=seq(0, 35, 5)) +
   scale_linetype_manual(values = c(rep("solid", 9), rep("dashed", 6))) +
scale_color_manual(values = c(myColors[1:9], myColors[1:6])) +
  scale_size_manual(values=1.25)+
  guides(color=guide_legend(nrow=1))+
   #scale_colour_manual(limits=levels(res.govt$variable), values=cols) + 
  theme(
    axis.text.x=element_text(angle=0), 
    legend.title=element_blank(), 
    legend.position="none",
    panel.background = element_rect(fill=NA), 
    plot.margin=unit(c(-0.25,0.5,0,0), "cm")) 


#extract legend
#https://github.com/hadley/ggplot2/wiki/Share-a-legend-between-two-ggplot2-graphs
g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

mylegend<-g_legend(g1)

p3 <- grid.arrange(arrangeGrob(g1 + theme(legend.position="none"),
                         g2 + theme(legend.position="none"),
                         nrow=2),
             mylegend, nrow=1, widths=c(10,1))









dev.off()








