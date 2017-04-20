
### creating plots for Megan Dodd + Julia Baum for global young academy document


## Aim: make 10 plots.
require(tidyr)


### default ggplot colors
gg_color_hue <- function(n) {
  hues = seq(15, 375, length=n+1)
  hcl(h=hues, l=65, c=100)[1:n]
}


setwd("/Users/jpwrobinson/Google_Drive/R_PROJECTS_DATA/VISUALISATIONS/global-young-academy-gdp")
theme_set(theme_minimal(base_size=16))

pdf(file="research_canada_G7_OECD_V2.pdf", height=7, width=11)
#I.1 GERD data
#A) GERD as a % of GDP - Canada, OECD and G7 averages highlighted wrt other countries (you decide graph type)
gerd<-read.csv("Data/GERD_GDP.csv")
gerd$OECD.Total<-as.numeric(as.character(gerd$OECD.Total))



gerd<-gather(gerd,YEAR)
gerd$variable<-as.character(gerd$variable)
gerd$variable[gerd$variable=="OECD.Total"]<- "OECD average"
gerd$variable[gerd$variable=="G7.Average"]<- "G7 average"
gerd$variable<-as.factor(gerd$variable)

## drop australia
# gerd<-gerd[!gerd$variable=="Australia",]



### data from 2003
gerd<-gerd[gerd$YEAR>=2003,]

gerd<-gerd[ !is.na(gerd$value),]
#ggplot(gerd, aes(YEAR, value, col=variable)) + geom_point() + facet_grid(variable~.) + scale_y_continuous()

## change levels for plotting
gerd$variable <- factor(gerd$variable, levels = c("Canada", "G7 average", "OECD average", "Australia", "Israel", "Netherlands", "Poland", "Spain", "UK", "US"))
gerd
## colours for line plots
cols<-gg_color_hue(10)

### fix to get new dataset

ggplot(gerd[gerd$variable=="Canada" | gerd$variable=="OECD average" | gerd$variable=="G7 average",], aes(YEAR, value, col=variable)) + geom_line(size=1) + 
  labs(y=expression(atop("Gross domestic expenditure on research", paste("and development (GERD) as a % of GDP"))), x="", col="") +
geom_line(data=gerd[!(gerd$variable=="Canada" | gerd$variable=="OECD average" | gerd$variable=="G7 average"),],aes(group=variable), size=0.5, linetype="dashed")  +
  scale_x_continuous(breaks=seq(2003, 2013, 2)) + ylim(c(0, 5)) + ggtitle("Gross domestic expenditure on research & development (GERD)") + scale_colour_manual(limits=levels(gerd$variable), values=cols)








#B) Percent of GERD financed by government and industry.
#I think these could each be expressed as their own graph with data from all countries and G7 and OECD average,
# then a graph that combines both industry and gov data for Canada alone, 
#  or Canada and the two averages if possible.

gerd_gov<-read.csv("Data/GERD_govt.csv")
gerd_gov<-gather(gerd_gov, YEAR)

gerd_gov$variable<-as.character(gerd_gov$variable)
gerd_gov$variable[gerd_gov$variable=="OECD.Total"]<- "OECD average"
gerd_gov$variable[gerd_gov$variable=="G7.Average"]<- "G7 average"
gerd_gov$variable<-as.factor(gerd_gov$variable)

# gerd_gov<-gerd_gov[!gerd_gov$variable=="Australia",]

gerd_gov<-gerd_gov[gerd_gov$YEAR>=2003,]
gerd_gov<-gerd_gov[ !is.na(gerd_gov$value),]

gerd_gov$variable <- factor(gerd_gov$variable, levels = c("Canada", "G7 average", "OECD average", "Australia", "Israel", "Netherlands", "Poland", "Spain", "UK", "US"))

ggplot(gerd_gov[gerd_gov$variable=="Canada" | gerd_gov$variable=="OECD average" | gerd_gov$variable=="G7 average",], aes(YEAR, value, col=variable)) + geom_line(size=1) + labs(y="Proportion of total GERD contributed from federal government (%)", x="", col="") +
  geom_line(data=gerd_gov[!(gerd_gov$variable=="Canada" | gerd_gov$variable=="OECD average" | gerd_gov$variable=="G7 average"),],aes(col=variable), size=0.5, linetype="dashed") +
  scale_x_continuous(breaks=seq(2003, 2013, 2)) + scale_y_continuous(breaks=seq(10, 70, 10), limits=c(10, 70)) +
  ggtitle("GERD financed by government") + scale_colour_manual(limits=levels(gerd_gov$variable), values=cols)




gerd_ind<-read.csv("Data/GERD_industry.csv")
gerd_ind<-gather(gerd_ind, YEAR)


gerd_ind$variable<-as.character(gerd_ind$variable)
gerd_ind$variable[gerd_ind$variable=="OECD.Total"]<- "OECD average"
gerd_ind$variable[gerd_ind$variable=="G7.Average"]<- "G7 average"
gerd_ind$variable<-as.factor(gerd_ind$variable)

# gerd_ind<-gerd_ind[!gerd_ind$variable=="Australia",]
gerd_ind<-gerd_ind[gerd_ind$YEAR>=2003,]
gerd_ind<-gerd_ind[ !is.na(gerd_ind$value),]


gerd_ind$variable <- factor(gerd_ind$variable, levels = c("Canada", "G7 average", "OECD average", "Australia", "Israel", "Netherlands", "Poland", "Spain", "UK", "US"))


ggplot(gerd_ind[gerd_ind$variable=="Canada" | gerd_ind$variable=="OECD average" | gerd_ind$variable=="G7 average",], aes(YEAR, value, col=variable)) + geom_line(size=1) + labs(y="Proportion  of total GERD contributed from business enterprises (%)", x="", col="") +
  geom_line(data=gerd_ind[!(gerd_ind$variable=="Canada" | gerd_ind$variable=="OECD average" | gerd_ind$variable=="G7 average"),],aes(group=variable), size=0.5, linetype="dashed") +
  scale_x_continuous(breaks=seq(2003, 2013, 2))  + ylim(c(20, 70)) + scale_colour_manual(limits=levels(gerd_ind$variable), values=cols) +
  ggtitle("GERD financed by industry")




#I.2 Basic research expenditure as a percent of GDP
#For this graph there is no CDN data, but I think having a graph that highlights OECD and G7 data might still be worthwhile. (and eventually we will make the same graphs featuring each of the other countries)


res_exp<-read.csv("Data/research_exp_GDP.csv")
res_exp<-gather(res_exp, YEAR)
res_exp$YEAR<-as.numeric(res_exp$YEAR)

res_exp$variable<-as.character(res_exp$variable)
res_exp$variable[res_exp$variable=="OECD.Total"]<- "OECD average"
res_exp$variable[res_exp$variable=="G7.Average"]<- "G7 average"
res_exp$variable<-as.factor(res_exp$variable)

# res_exp<-res_exp[!res_exp$variable=="Australia",]
# res_exp<-res_exp[!res_exp$variable=="Canada",]
# res_exp<-res_exp[!res_exp$variable=="Netherlands",]
res_exp<-res_exp[ !is.na(res_exp$value),]

res_exp<-res_exp[res_exp$YEAR>=2003,]
res_exp$variable <- factor(res_exp$variable, levels = c("G7 average", "OECD average", "Australia", "Israel", "Netherlands", "Poland", "Spain", "UK", "US"))


ggplot(res_exp[ res_exp$variable=="OECD average" | res_exp$variable=="G7 average",], aes(YEAR, value, col=variable)) + geom_line(size=1) + labs(y="Basic research expenditure as % of GDP", x="", col="") +
  geom_line(data=res_exp[!( res_exp$variable=="OECD average" | res_exp$variable=="G7 average"),],aes(group=variable), size=0.5, linetype="dashed") +
  scale_x_continuous(breaks=seq(2003, 2013, 2)) + scale_y_continuous(breaks=seq(0, 0.7, 0.1))+ ggtitle("Basic research expenditure") + scale_colour_manual(limits=levels(res_exp$variable), values=cols[-1]) 


#I.3 BERD and HERD data
#I think that each of these could be expressed as individual graphs with all other countries and OECD and G7 average, and also we could have a graph that combines BERD and HERD expenditure for Canada only. 
BERD<-read.csv("Data/BERD.csv")
BERD<-gather(BERD, YEAR)
BERD$YEAR<-as.numeric(BERD$YEAR)

BERD$variable<-as.character(BERD$variable)
BERD$variable[BERD$variable=="OECD.Average"]<- "OECD average"
BERD$variable[BERD$variable=="G7.Average"]<- "G7 average"
BERD$variable<-as.factor(BERD$variable)
BERD<-BERD[ !is.na(BERD$value),]
# BERD<-BERD[!BERD$variable=="Australia",]

BERD$variable <- factor(BERD$variable, levels = c("Canada", "G7 average", "OECD average", "Australia", "Israel", "Netherlands", "Poland", "Spain", "UK", "US"))


ggplot(BERD[BERD$variable=="Canada" | BERD$variable=="OECD average" | BERD$variable=="G7 average",], aes(YEAR, value, col=variable)) + geom_line(size=1) + labs(y="R&D performed by business enterprises as % of GDP", x="", col="") +
  geom_line(data=BERD[!(BERD$variable=="Canada" | BERD$variable=="OECD average" | BERD$variable=="G7 average"),],aes(group=variable), size=0.5, linetype="dashed") +
  scale_x_continuous(breaks=c(2003, 2005, 2007, 2009, 2011)) + scale_y_continuous(breaks=seq(0, 3.5, 0.5)) + ggtitle("BERD")+ scale_colour_manual(limits=levels(BERD$variable), values=cols) 




HERD<-read.csv("Data/HERD.csv")
HERD<-gather(HERD, YEAR)

HERD$variable<-as.character(HERD$variable)
HERD$variable[HERD$variable=="G7.Average"]<- "G7 average"
HERD$variable[HERD$variable=="United.States"]<- "US"
HERD$variable[HERD$variable=="OECD"]<- "OECD average"
HERD$variable<-as.factor(HERD$variable)

# HERD<-HERD[!HERD$variable=="Australia",]
HERD$YEAR<-as.numeric(HERD$YEAR)

HERD<-HERD[HERD$YEAR>=2003,]
HERD<-HERD[ !is.na(HERD$value),]
HERD$variable <- factor(HERD$variable, levels = c("Canada", "G7 average", "OECD average", "Australia", "Israel", "Netherlands", "Poland", "Spain", "UK", "US"))


ggplot(HERD[HERD$variable=="Canada" | HERD$variable=="OECD average" | HERD$variable=="G7 average",], aes(YEAR, value, col=variable)) + geom_line(size=1) + labs(y="R&D performed by higher education as % of GDP", x="", col="") +
  geom_line(data=HERD[!(HERD$variable=="Canada" | HERD$variable=="OECD average" | HERD$variable=="G7 average"),],aes(group=variable), size=0.5, linetype="dashed") +
  scale_x_continuous(breaks=seq(2003, 2013, 2))+ ggtitle("HERD")+ scale_colour_manual(limits=levels(HERD$variable), values=cols) 


#1.4 Employed persons
#I think we just plot this as is - the same countries I mentioned for analysis plus the G7 and OECD averages, Canada and the averages highlighted somehow. 

employed<-read.csv("Data/researchers-employed.csv")
employed<-gather(employed, YEAR)

employed$variable<-as.character(employed$variable)
employed$variable[employed$variable=="G7.Average"]<- "G7 average"
employed$variable[employed$variable=="OECD.Average"]<- "OECD average"
employed$variable[employed$variable=="United.Kingdom"]<- "UK"
employed$variable[employed$variable=="United.States"]<- "US"
employed$variable<-as.factor(employed$variable)

employed<-employed[!employed$variable=="Israel",]
employed$YEAR<-as.numeric(employed$YEAR)

employed<-employed[employed$YEAR>=2003,]
employed<-employed[ !is.na(employed$value),]

## remove countries that aren't shown in other graphs
employed$variable <- factor(employed$variable, levels = c("Canada", "G7 average", "OECD average", "Australia","France", "Germany", "Italy", "Japan", "Netherlands", "Poland", "Spain", "UK", "US"))

cols<-gg_color_hue(13)

ggplot(employed[(employed$variable=="Canada" | employed$variable=="OECD average" | employed$variable=="G7 average") & !(is.na(employed$value)),], aes(YEAR, value, col=variable)) + geom_line(size=1) + labs(y="Researchers employed per 1000 total employed", x="", col="") +
  geom_line(data=employed[!(employed$variable=="Canada" | employed$variable=="OECD average" | employed$variable=="G7 average"),],aes(group=variable), size=0.5, linetype="dashed") +
  scale_x_continuous(breaks=seq(2003, 2013, 2))+ ggtitle("Researchers employed")+ scale_colour_manual(limits=levels(employed$variable), values=cols) 



#1.4 Employed persons - government
#I think we just plot this as is - the same countries I mentioned for analysis plus the G7 and OECD averages, Canada and the averages highlighted somehow. 

govt<-read.csv("Data/govt_researchers.csv")
govt<-gather(govt, YEAR)

govt$variable<-as.character(govt$variable)
govt$variable[govt$variable=="G7.Average"]<- "G7 average"
govt$variable[govt$variable=="OECD.Total"]<- "OECD average"
govt$variable<-as.factor(govt$variable)

# govt<-govt[!govt$variable=="Israel",]
govt$YEAR<-as.numeric(govt$YEAR)

govt<-govt[govt$YEAR>=2003,]
govt<-govt[ !is.na(govt$value),]

## remove countries that aren't shown in other graphs
govt$variable <- factor(govt$variable, levels = c("Canada", "G7 average", "OECD average", "Australia", "Netherlands", "Poland", "Spain", "UK", "US"))

cols<-gg_color_hue(9)

ggplot(govt[(govt$variable=="Canada" | govt$variable=="OECD average" | govt$variable=="G7 average") & !(is.na(govt$value)),], aes(YEAR, value, col=variable)) + geom_line(size=1) + labs(y="Government researchers employed per 1000 total employed", x="", col="") +
  geom_line(data=govt[!(govt$variable=="Canada" | govt$variable=="OECD average" | govt$variable=="G7 average"),],aes(group=variable), size=0.5, linetype="dashed") +
  scale_x_continuous(breaks=seq(2003, 2013, 2))+ ggtitle("Government reseachers")+ scale_colour_manual(limits=levels(govt$variable), values=cols) 




#II.1 Federal Expenditures
#I think for this one stay simple - just the percent of total expenditure spent on S&T by year? Or anything else you can think of. 

feds<-read.csv("Data/federal-expenditures.csv")
colnames(feds)<-c("YEAR", "ST_expenditure", "budget_expenditure", "percent_federal_expenditure_spent", "ST_expenditure_adjusted")
feds<-feds[feds$YEAR>=2003,]

ggplot(feds, aes(YEAR, percent_federal_expenditure_spent)) + geom_line() + labs(y="% federal expenditure spent on S&T", x="Year") +
  scale_x_continuous(breaks=seq(2000, 2013, 1)) +   scale_y_continuous(breaks=seq(3.6, 5.2, 0.1)) + ggtitle("Federal expenditures")


#II.2 Personnel by Sector
#I think this one could be expressed as a percent of total researchers - so stacked bar graph or something similar to show how the researchers are distributed amongst the sectors and how that has changed over time. 
persons<-read.csv("Data/personnel-by-sector.csv")
persons<-gather(persons, YEAR)

head(persons)

persons$Research<-persons$variable
persons<-persons[persons$YEAR>=2003,]

library(scales)
ggplot(persons[!persons$Research=="TOTAL.RESEARCHERS",], aes(YEAR, value, fill=Research)) + geom_bar(stat="identity") + labs(x="", y="Number of people") + scale_y_continuous(labels=comma) +
scale_fill_discrete(labels=c("Business enterprise", "Government", "Higher education", "Private non-profit")) + ggtitle("Personnel by sector")+
  scale_x_continuous(breaks=seq(1999.75, 2011.75, 1), labels=seq(2000, 2012, 1)) + theme(axis.text.x=element_text(angle=90), axis.ticks.x=element_blank())

#II.3 R&D expenditure on NSERC, SSHRC

#Not sure what to do with this data... either as a percent or total dollars is fine, I guess we could show it all on one graph and also have separate graphs?
rd<-read.csv("Data/RD-expenditures.csv", stringsAsFactors=FALSE)
str(rd)

str(rd)
rd<-gather(rd, YEAR)


require(stringr)
rd$type<- str_split_fixed(rd$variable, "_", n=2)[,2]
rd$source<- str_split_fixed(rd$variable, "_", n=2)[,1]
head(rd)

rd<-rd[rd$YEAR>=2003,]


ggplot(rd, aes(YEAR, value, col=type)) + geom_line() + facet_grid(source~., scales="free") + ggtitle("R&D expenditure on NSERC and SSHRC")+ scale_y_continuous(labels=comma) +
  labs(x='', y="R&D expenditure in $ (inflation corrected for 2015") + scale_colour_discrete(labels=c("All", "Business", "Federal", "Higher education", "Non-profit", "Provincial")) +
  scale_x_continuous(breaks=seq(2003, 2015, 2)) + theme(strip.text=element_text(size=15, angle=90), legend.title=element_blank())



#III.1 NSERC Discovery and Innovation Expenditure
#Something that plots the changes in both programs side by side for comparison would be great.  

disco<-read.csv("Data/discovery-expenditures.csv")


disco<-subset(disco, select=c("Year", "Discovery.Expenditured..Inflation.Corrected.2015.Million.of.Dollars.","Innovation.Expenditured..Inflation.Corrected.2015.Million.of.Dollars."))


colnames(disco)<-c("Year", "Discovery", "Innovation")
disco<-gather(disco, Year)
disco<-disco[disco$Year>=2003,]

ggplot(disco, aes(Year, value)) + geom_line() + facet_wrap(~variable) + 
  labs(x='', y="Expenditure in millions of dollars (inflation corrected for 2015)") +
  scale_x_continuous(breaks=seq(2003, 2013, 2)) + theme(strip.text=element_text(size=15)) + ggtitle("Expenditure on Discovery and Innovation grants")

ggplot(disco, aes(Year, value)) + geom_line() + facet_grid(variable~., scale="fixed") + 
  labs(x='', y="Expenditure in millions of dollars (inflation corrected for 2015)") +
  scale_x_continuous(breaks=seq(2003, 2014, 1)) + theme(strip.text=element_text(size=15))+ ggtitle("Expenditure on Discovery and Innovation grants")


#III.2 NSERC Discovery Grant Success
#might as well plot this data (starting 2000 or 2003ish) and I will continue to look for the data on innovation grant success if it exists. 
disco_success<-read.csv("Data/discovery-success.csv")
head(disco_success)
disco_success<-gather(disco_success, Year)
disco_success<-disco_success[disco_success$Year>=2003,]

g1<-ggplot(disco_success[!disco_success$variable=="Percent.of.Successful.Applications",], aes(Year, value, col=variable)) + geom_line() + 
  labs(x="", y="Number of applications") + scale_colour_discrete(labels=c("Applied", "Successful")) +   scale_x_continuous(breaks=seq(2003, 2015, 2)) +
  theme(legend.title=element_blank()) 


g2<-ggplot(disco_success[disco_success$variable=="Percent.of.Successful.Applications",], aes(Year, value)) + geom_line() + 
  labs(x="", y="% successful")  +   scale_x_continuous(breaks=seq(2003, 2015, 2))

require(gridExtra)
grid.arrange(g1, g2, top="Discovery grant applications")



#### SSHRC graphs

sshrc<-read.csv("Data/SSHRC_success.csv")
sshrc<-gather(sshrc, Year)


g1<-ggplot(sshrc[sshrc$variable=="success_rate" | sshrc$variable=="funding_rate",], aes(Year, value, col=variable)) + 
  geom_line() + scale_x_continuous(breaks=seq(2003, 2014, 1)) + labs(y = "%") + theme(legend.title=element_blank()) +
   scale_colour_discrete(labels=c("Successful application", "Funding awarded"))

g2<-ggplot(sshrc[!(sshrc$variable=="success_rate" | sshrc$variable=="funding_rate"),], aes(Year, value, col=variable)) + 
  geom_line() + scale_x_continuous(breaks=seq(2003, 2014, 1)) + labs(y = "Funding award in millions $") + theme(legend.title=element_blank()) +
  scale_colour_discrete(labels=c("Funding applied", "Funding awarded"))



grid.arrange(g1, g2, top="SSHRC funding applications")


dev.off()
