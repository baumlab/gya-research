

## Aim: make 5 plots
require(tidyr)
require(scales)
require(grid); require(gridExtra)
require(ggplot2)
require(RColorBrewer)
### return to default ggplot colors


#extract legend
#https://github.com/hadley/ggplot2/wiki/Share-a-legend-between-two-ggplot2-graphs
g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}


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
myColors<-c( myColors[1], myColors[2:11])
# change orange for japan 
myColors[6]<-"#6ffdbf"

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


pdf(file="figures/GERD_aug9.pdf", height=7, width=11)
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
gerd.10<-gerd[gerd$panel=="GERD Top Ten",]
gerd.G8<-gerd[gerd$panel=="G8",]

  country10<-unique(gerd.10$Country)
  country10<-country10[!(country10=='Canada' | country10=="OECD Average")]
  my10Colors<-myColors[1:length(country10)]

  countryG8<-unique(gerd.G8$Country)
  countryG8<-countryG8[!(countryG8=='Canada' | countryG8=="G8 Average")]
  myG8colors<-myColors[c(1,4,3,6,5,7,10)]



layout(matrix(c(1,1,1,2,2,2,3), nrow=1))

### TOP TEN COUNTRIES
par(mar=c(5,6,4,2))
with(gerd.10, plot(value ~ YEAR, col="transparent", axes=FALSE, xlab="",cex.axis=1.5, ylab="",ylim=c(1, 4.5)))
for(i in 1:length(country10)){
  with(gerd.10[gerd.10$Country==country10[i],], lines(value ~ YEAR, col=my10Colors[i], lty=2, lwd=2))
}
  with(gerd.10[gerd.10$Country=='Canada',], lines(value ~ YEAR, col='red', lty=1, lwd=4))
  with(gerd.10[gerd.10$Country=='OECD Average',], lines(value ~ YEAR, col='black', lty=2, lwd=4))
axis(1, at=seq(2005, 2015,1),labels=seq(2005, 2015,1))
axis(2,  at=seq(1, 4.5, 0.5))
text(2006.2, 4.5, '(a) GERD Top Ten', cex=1.2)
mtext("Expenditure (% of GDP)", 2, cex=1, line=2)

### G8 COUNTRIES
with(gerd.G8, plot(value ~ YEAR, col="transparent", axes=FALSE, xlab="",cex.axis=1.5, ylab="", ylim=c(1, 4.5)))
for(i in 1:length(countryG8)){
  with(gerd.G8[gerd.G8$Country==countryG8[i],], lines(value ~ YEAR, col=myG8colors[i], lty=1, lwd=2))
}
  with(gerd.G8[gerd.G8$Country=='Canada',], lines(value ~ YEAR, col='red', lty=1, lwd=4))
  with(gerd.G8[gerd.G8$Country=='G8 Average',], lines(value ~ YEAR, col='black', lty=1, lwd=4))
axis(1, at=seq(2005, 2015,1),labels=seq(2005, 2015,1))
axis(2,  at=seq(1, 4.5, 0.5))
text(2005.2, 4.5, '(b) G8', cex=1.2)

## legend
par(mar=c(0,0,0,0))
plot(1:10, 1:10, xlab="", ylab="",col="transparent", axes=FALSE )

legend(0.5,9.5, legend=c('Canada', "",  "OECD Average", "G8 Average", "",  "GERD Top Ten", paste(country10),"", "G8", paste(countryG8)), 
                      lty=c(1, 1, 2, 1, 2,2,rep(2, 10), 1, rep(1,8)), 
                      lwd=c(4, 2, 4, 4, 4,4,rep(2, 20), 2, rep(2,7)), 
                      col=c("red", "transparent","black","black", 'transparent','transparent', 
                        myColors[1:length(country10)],'transparent','transparent', myG8colors[1:length(countryG8)]),
                      bty='n', cex=1.3)




# ### Figure 1b - GERD source


gerd.10.govt<-gerd.source[gerd.source$panel=="Government - GERD Top Ten",]
gerd.10.industry<-gerd.source[gerd.source$panel=="Industry - GERD Top Ten",]
gerd.8.govt<-gerd.source[gerd.source$panel=="Government - G8",]
gerd.8.industry<-gerd.source[gerd.source$panel=="Industry - G8",]


  country10<-unique(gerd.10$Country)
  country10<-country10[!(country10=='Canada' | country10=="OECD Average")]
  my10Colors<-myColors[1:length(country10)]

  countryG8<-unique(gerd.G8$Country)
  countryG8<-countryG8[!(countryG8=='Canada' | countryG8=="G8 Average")]
  myG8colors<-myColors[c(1,4,3,6,5,7,10)]



layout(matrix(c(1,2,1,2,3,4,3,4,5,5), nrow=2, byrow=FALSE))

### TOP TEN COUNTRIES
par(mar=c(3,6,4,2))
with(gerd.10.govt, plot(value ~ YEAR, col="transparent", axes=FALSE, xlab="",cex.axis=1.5, ylab="",ylim=c(10, 80)))
for(i in 1:length(country10)){
  with(gerd.10.govt[gerd.10.govt$Country==country10[i],], lines(value ~ YEAR, col=my10Colors[i], lty=2, lwd=2))
}
  with(gerd.10.govt[gerd.10.govt$Country=='Canada',], lines(value ~ YEAR, col='red', lty=1, lwd=4))
  with(gerd.10.govt[gerd.10.govt$Country=='OECD Average',], lines(value ~ YEAR, col='black', lty=2, lwd=4))
axis(1, at=seq(2005, 2015,1),labels=seq(2005, 2015,1))
axis(2,  at=seq(10,  80, 5))
mtext('GERD Top Ten',3,cex=1.2)
mtext('Contribution (% of GDP)',2, cex=1, line=2)
# text(2005.8, 80, '(a) Government - GERD Top Ten', cex=1.2)

# INDUSTRY
par(mar=c(5,6,2,2))
with(gerd.10.industry, plot(value ~ YEAR, col="transparent", axes=FALSE, xlab="",cex.axis=1.5, ylab="",ylim=c(10, 80)))
for(i in 1:length(country10)){
  with(gerd.10.industry[gerd.10.industry$Country==country10[i],], lines(value ~ YEAR, col=my10Colors[i], lty=2, lwd=2))
}
  with(gerd.10.industry[gerd.10.industry$Country=='Canada',], lines(value ~ YEAR, col='red', lty=1, lwd=4))
  with(gerd.10.industry[gerd.10.industry$Country=='OECD Average',], lines(value ~ YEAR, col='black', lty=2, lwd=4))
axis(1, at=seq(2005, 2015,1),labels=seq(2005, 2015,1))
axis(2,  at=seq(10,  80, 5))
mtext('Contribution (% of GDP)',2, cex=1, line=2)

# text(2005.8, 80, '(b) Industry - GERD Top Ten', cex=1.2)


### G8 COUNTRIES
par(mar=c(3,4,4,2))
with(gerd.8.govt, plot(value ~ YEAR, col="transparent", axes=FALSE, xlab="",cex.axis=1.5, ylab="", ylim=c(10, 80)))
for(i in 1:length(countryG8)){
  with(gerd.8.govt[gerd.8.govt$Country==countryG8[i],], lines(value ~ YEAR, col=myG8colors[i], lty=1, lwd=2))
}
  with(gerd.8.govt[gerd.8.govt$Country=='Canada',], lines(value ~ YEAR, col='red', lty=1, lwd=4))
  with(gerd.8.govt[gerd.8.govt$Country=='G8 Average',], lines(value ~ YEAR, col='black', lty=1, lwd=4))
axis(1, at=seq(2005, 2015,1),labels=seq(2005, 2015,1))
axis(2,  at=seq(10,  80, 5))
mtext('G8',3,cex=1.2)
text(2014.5, 45, srt=270, label="Government", cex=2, xpd=TRUE)
# text(2005.2, 80, '(c) Government - G8', cex=1.2)

## industry
par(mar=c(5,4,2,2))
with(gerd.8.industry, plot(value ~ YEAR, col="transparent", axes=FALSE, xlab="",cex.axis=1.5, ylab="", ylim=c(10, 80)))
for(i in 1:length(countryG8)){
  with(gerd.8.industry[gerd.8.industry$Country==countryG8[i],], lines(value ~ YEAR, col=myG8colors[i], lty=1, lwd=2))
}
  with(gerd.8.industry[gerd.8.industry$Country=='Canada',], lines(value ~ YEAR, col='red', lty=1, lwd=4))
  with(gerd.8.industry[gerd.8.industry$Country=='G8 Average',], lines(value ~ YEAR, col='black', lty=1, lwd=4))
axis(1, at=seq(2005, 2015,1),labels=seq(2005, 2015,1))
axis(2,  at=seq(10,  80, 5))
text(2014.5, 45, srt=270, label="Industry", cex=2, xpd=TRUE)
# mtext("Industry", 4,las=3,cex=1.2)
# text(2005.2, 80, '(d) Industry - G8', cex=1.2)

## legend
par(mar=c(0,0,0,0))
plot(1:10, 1:10, xlab="", ylab="",col="transparent", axes=FALSE )

legend(1,9.5, legend=c('Canada', "",  "OECD Average", "G8 Average", "",  "GERD Top Ten", paste(country10),"", "G8", paste(countryG8)), 
                      lty=c(1, 1, 2, 1, 2,2,rep(2, 10), 1, rep(1,8)), 
                      lwd=c(4, 2, 4, 4, 4,4,rep(2, 20), 2, rep(2,7)), 
                      col=c("red", "transparent","black","black", 'transparent','transparent', 
                        myColors[1:length(country10)],'transparent','transparent', myG8colors[1:length(countryG8)]),
                      bty='n', cex=1.3)






## Figure 2 - basic researhc

basic.10<-basic[basic$panel=="GERD Top Ten",]
basic.G8<-basic[basic$panel=="G8",]

layout(matrix(c(1,1,1,2,2,2,3), nrow=1))

### TOP TEN COUNTRIES
par(mar=c(5,6,4,2))
with(basic.10, plot(value ~ YEAR, col="transparent", axes=FALSE, xlab="",cex.axis=1.5, ylab="",ylim=c(0, 0.8)))
for(i in 1:length(country10)){
  with(basic.10[basic.10$Country==country10[i],], lines(value ~ YEAR, col=my10Colors[i], lty=2, lwd=2))
}
  with(basic.10[basic.10$Country=='Canada',], lines(value ~ YEAR, col='red', lty=1, lwd=4))
  with(basic.10[basic.10$Country=='OECD Average',], lines(value ~ YEAR, col='black', lty=2, lwd=4))
axis(1, at=seq(2005, 2015,1),labels=seq(2005, 2015,1))
axis(2,  at=seq(0, 1, 0.1))
text(2005.8, 4.5, '(a) GERD Top Ten', cex=1.2)
mtext('Expenditure (% of GDP)', 2,cex=1, line=2)

### G8 COUNTRIES
with(basic.G8, plot(value ~ YEAR, col="transparent", axes=FALSE, xlab="",cex.axis=1.5, ylab="", ylim=c(0, 0.8)))
for(i in 1:length(countryG8)){
  with(basic.G8[basic.G8$Country==countryG8[i],], lines(value ~ YEAR, col=myG8colors[i], lty=1, lwd=2))
}
  with(basic.G8[basic.G8$Country=='Canada',], lines(value ~ YEAR, col='red', lty=1, lwd=4))
  with(basic.G8[basic.G8$Country=='G8 Average',], lines(value ~ YEAR, col='black', lty=1, lwd=4))
axis(1, at=seq(2005, 2015,1),labels=seq(2005, 2015,1))
axis(2,  at=seq(0,1 ,0.1))
text(2005.2, 4.5, '(b) G8', cex=1.2)

## legend
par(mar=c(0,0,0,0))
plot(1:10, 1:10, xlab="", ylab="",col="transparent", axes=FALSE )

legend(0.5,9.5, legend=c("", "",  "", "G8 Average", "",  "GERD Top Ten", paste(country10[1:2]),"","", paste(country10[5:7]),"", paste(country10[9:10]), "",
               "G8", paste(countryG8[1]), "", paste(countryG8[3:6])), 
                      lty=c(1, 1, 2, 1, 2,2,rep(2, 10), 1, rep(1,8)), 
                      lwd=c(4, 2, 4, 4, 4,4,rep(2, 20), 2, rep(2,7)), 
                      col=c("transparent", "transparent","transparent","black", 'transparent','transparent', 
                        myColors[1:2],'transparent','transparent',myColors[5:7], 'transparent',myColors[9:10],
                        'transparent','transparent', 
                        myG8colors[1], 'transparent',myG8colors[3:6]),
                      bty='n', cex=1.3)







## Figure 3 - number of researchers

gerd.10.res<-res.res[res.res$panel=="GERD Top Ten",]
gerd.10.govt<-res.govt[res.govt$panel=="GERD Top Ten",]
gerd.8.res<-res.res[res.res$panel=="G8",]
gerd.8.govt<-res.govt[res.govt$panel=="G8",]


  # country10<-unique(gerd.10$Country)
  # country10<-country10[!(country10=='Canada' | country10=="OECD Average")]
  # my10Colors<-myColors[1:length(country10)]

  # countryG8<-unique(gerd.G8$Country)
  # countryG8<-countryG8[!(countryG8=='Canada' | countryG8=="G8 Average")]
  # myG8colors<-myColors[c(1,4,3,6,5,7,10)]



layout(matrix(c(1,2,1,2,3,4,3,4,5,5), nrow=2, byrow=FALSE))

### TOP TEN COUNTRIES
par(mar=c(3,6,4,2))
with(gerd.10.res, plot(value ~ YEAR, col="transparent", axes=FALSE, xlab="",cex.axis=1.5, ylab="",ylim=c(0, 20)))
for(i in 1:length(country10)){
  with(gerd.10.res[gerd.10.res$Country==country10[i],], lines(value ~ YEAR, col=my10Colors[i], lty=2, lwd=2))
}
  with(gerd.10.res[gerd.10.res$Country=='Canada',], lines(value ~ YEAR, col='red', lty=1, lwd=4))
  with(gerd.10.res[gerd.10.res$Country=='OECD Average',], lines(value ~ YEAR, col='black', lty=2, lwd=4))
axis(1, at=seq(2005, 2015,1),labels=seq(2005, 2015,1))
axis(2,  at=seq(0,  20, 5))
mtext('GERD Top Ten',3,cex=1.2)
mtext("Researchers Employed\n (per 1000 employed)", 2, cex=1, line=2)
# text(2005.8, 80, '(a) Government - GERD Top Ten', cex=1.2)

# INDUSTRY
par(mar=c(5,6,2,2))
with(gerd.10.govt, plot(value ~ YEAR, col="transparent", axes=FALSE, xlab="",cex.axis=1.5, ylab="",ylim=c(0, 35)))
for(i in 1:length(country10)){
  with(gerd.10.govt[gerd.10.govt$Country==country10[i],], lines(value ~ YEAR, col=my10Colors[i], lty=2, lwd=2))
}
  with(gerd.10.govt[gerd.10.govt$Country=='Canada',], lines(value ~ YEAR, col='red', lty=1, lwd=4))
  with(gerd.10.govt[gerd.10.govt$Country=='OECD Average',], lines(value ~ YEAR, col='black', lty=2, lwd=4))
axis(1, at=seq(2005, 2015,1),labels=seq(2005, 2015,1))
axis(2,  at=seq(0, 35, 5))
mtext('Government researchers\n (% of researchers)', 2, cex=1, line=2)
# text(2005.8, 80, '(b) Industry - GERD Top Ten', cex=1.2)


### G8 COUNTRIES
par(mar=c(3,4,4,2))
with(gerd.8.res, plot(value ~ YEAR, col="transparent", axes=FALSE, xlab="",cex.axis=1.5, ylab="", ylim=c(0,20)))
for(i in 1:length(countryG8)){
  with(gerd.8.res[gerd.8.res$Country==countryG8[i],], lines(value ~ YEAR, col=myG8colors[i], lty=1, lwd=2))
}
  with(gerd.8.res[gerd.8.res$Country=='Canada',], lines(value ~ YEAR, col='red', lty=1, lwd=4))
  with(gerd.8.res[gerd.8.res$Country=='G8 Average',], lines(value ~ YEAR, col='black', lty=1, lwd=4))
axis(1, at=seq(2005, 2015,1),labels=seq(2005, 2015,1))
axis(2,  at=seq(0,  20, 5))
mtext('G8',3,cex=1.2)
text(2014.5, 10, srt=270, label="Government", cex=2, xpd=TRUE)
# text(2005.2, 80, '(c) Government - G8', cex=1.2)

## industry
par(mar=c(5,4,2,2))
with(gerd.8.govt, plot(value ~ YEAR, col="transparent", axes=FALSE, xlab="",cex.axis=1.5, ylab="", ylim=c(0, 35)))
for(i in 1:length(countryG8)){
  with(gerd.8.govt[gerd.8.govt$Country==countryG8[i],], lines(value ~ YEAR, col=myG8colors[i], lty=1, lwd=2))
}
  with(gerd.8.govt[gerd.8.govt$Country=='Canada',], lines(value ~ YEAR, col='red', lty=1, lwd=4))
  with(gerd.8.govt[gerd.8.govt$Country=='G8 Average',], lines(value ~ YEAR, col='black', lty=1, lwd=4))
axis(1, at=seq(2005, 2015,1),labels=seq(2005, 2015,1))
axis(2,  at=seq(0, 35, 5))
text(2014.5, 20, srt=270, label="Industry", cex=2, xpd=TRUE)
# mtext("Industry", 4,las=3,cex=1.2)
# text(2005.2, 80, '(d) Industry - G8', cex=1.2)

## legend
par(mar=c(0,0,0,0))
plot(1:10, 1:10, xlab="", ylab="",col="transparent", axes=FALSE )

legend(1,9.5, legend=c('Canada', "",  "OECD Average", "G8 Average", "",  "GERD Top Ten", paste(country10),"", "G8", paste(countryG8)), 
                      lty=c(1, 1, 2, 1, 2,2,rep(2, 10), 1, rep(1,8)), 
                      lwd=c(4, 2, 4, 4, 4,4,rep(2, 20), 2, rep(2,7)), 
                      col=c("red", "transparent","black","black", 'transparent','transparent', 
                        myColors[1:length(country10)],'transparent','transparent', myG8colors[1:length(countryG8)]),
                      bty='n', cex=1.3)









dev.off()