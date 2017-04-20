
### creating plots for Megan Dodd + Julia Baum for global young academy document


setwd("/Users/jpwrobinson/Google_Drive/R_PROJECTS_DATA/VISUALISATIONS/global-young-academy-gdp")
theme_set(theme_minimal(base_size=14))
require(tidyr); require(scales); require(grid); require(RColorBrewer)

palette(brewer.pal(n=8, name="Dark2"))

## read in data
dat1A<-read.csv("data/part4-june/graph1A.csv")
dat1B<-read.csv("data/part4-june/graph1B.csv")
dat1C<-read.csv("data/part4-june/graph1C.csv")
dat2<-read.csv("data/part4-june/graph2.csv")
dat3<-read.csv("data/part4-june/graph3.csv")
dat4<-read.csv("data/part4-june/graph4.csv", check.names=FALSE)
dat5A<-read.csv("data/part4-june/graph5A.csv", check.names=FALSE)
dat5B<-read.csv("data/part4-june/graph5B.csv", check.names=FALSE)
dat6<-read.csv("data/part4-june/graph6.csv", check.names=FALSE)


pdf(file="GYA_figures_june2016.pdf", height=7, width=11)
#-----------------------#-----------------------#-----------------------
	#----------------------- GRAPH 1 #-----------------------
#-----------------------#-----------------------#-----------------------

### A 
# NOTES:
# Also create graphs that include identical data but only from 2006-2015
# Keep data points for first year in every graph starting around the same visual point
# label each axis to indicate which dataset they represent?
head(dat1A)
dat1A<-gather(dat1A,  Year, value)
colnames(dat1A)[2]<-"Sector"
dat1A_sub<-dat1A[dat1A$Year>2005,]

ylabel <- "Millions of Dollars (Inflation-adjusted, 2015)"

g1<-ggplot(dat1A[dat1A$Sector=="All.sectors",], aes(Year, value, col=Sector)) + 
  geom_line(size=1,col=1) + scale_x_continuous(breaks=NULL) + labs(x="", y="Millions of Dollars (Inflation-Adjusted, 2015)") + theme(legend.title=element_blank())  +
  scale_y_continuous(labels=comma)+ 	annotate("text", 1998, 34000, label="(a) All Sectors", hjust=0) +
  theme(plot.margin=unit(c(0,0,-0.2,0.8), "cm"), axis.title.y=element_text(hjust=2, vjust=1)) +
  guides(col=FALSE)

g2<-ggplot(dat1A[dat1A$Sector=="Federal.government",], aes(Year, value, col=Sector)) + 
  geom_line(size=1,col=2) + scale_x_continuous(breaks=seq(1998, 2015, 1)) + labs(y=" \n \n ", x="") + theme(legend.title=element_blank()) +
  	 scale_y_continuous(labels=comma)+ 	annotate("text", x=1998, y=3400, label="(b) Federal Government", hjust=0) +
	theme(plot.margin=unit(c(-0.2,0,0,0), "cm"), legend.position="bottom", legend.title=element_blank()) +
	guides(col=FALSE)


g3<-ggplot(dat1A_sub[dat1A_sub$Sector=="All.sectors",], aes(Year, value, col=Sector)) + 
  geom_line(size=1,col=1) + scale_x_continuous(breaks=NULL) + labs(x="", y="Millions of Dollars (Inflation-Adjusted, 2015)") + theme(legend.title=element_blank())  +
  scale_y_continuous(labels=comma)+ 	annotate("text", 2006, 34500, label="(a) All Sectors", hjust=0) +
  theme(plot.margin=unit(c(0,0,-0.2,0.8), "cm"), axis.title.y=element_text(hjust=2, vjust=1)) +
  guides(col=FALSE)

g4<-ggplot(dat1A_sub[dat1A_sub$Sector=="Federal.government",], aes(Year, value, col=Sector)) + 
  geom_line(size=1,col=2) + scale_x_continuous(breaks=seq(2006, 2015, 1)) + labs(y=" \n \n ", x="") + theme(legend.title=element_blank()) +
  	 scale_y_continuous(labels=comma)+ 	annotate("text", x=2006, y=3400, label="(b) Federal Government", hjust=0) +
	theme(plot.margin=unit(c(-0.2,0,0,0), "cm"), legend.position="bottom", legend.title=element_blank()) +
	guides(col=FALSE)


## for full dataset 
grid.arrange(g1, g2, layout_matrix = rbind(1,2), top="1A. Intramural R&D Expenditure: Natural Sciences and Engineering, Social Sciences and Humanities")
## for 2006 - 2015
grid.arrange(g3, g4, layout_matrix = rbind(1,2), top="1A. Intramural R&D Expenditure: Natural Sciences and Engineering, Social Sciences and Humanities")


### B

head(dat1B)
dat1B<-gather(dat1B,  Year, value)
colnames(dat1B)[2]<-"Sector"
dat1B_sub<-dat1B[dat1B$Year>2005,]

ylabel <- "Millions of Dollars (Inflation-adjusted, 2015)"

g1<-ggplot(dat1B[dat1B$Sector=="All.sectors",], aes(Year, value, col=Sector)) + 
  geom_line(size=1,col=1) + scale_x_continuous(breaks=NULL) + labs(x="", y="Millions of Dollars (Inflation-Adjusted, 2015)") + theme(legend.title=element_blank())  +
  scale_y_continuous(labels=comma, breaks=seq(22000, 32000, 2000)) +	annotate("text", 1998, 32000, label="(a) All Sectors", hjust=0) +
  theme(plot.margin=unit(c(0,0,-0.2,1), "cm"), axis.title.y=element_text(hjust=3, vjust=1)) +
  guides(col=FALSE)


g2<-ggplot(dat1B[dat1B$Sector=="Federal.government",], aes(Year, value, col=Sector)) + 
  geom_line(size=1,col=2) + scale_x_continuous(breaks=seq(1998, 2015, 1)) + labs(y=" \n \n ", x="") + theme(legend.title=element_blank()) +
  	 scale_y_continuous(labels=comma)+ 	annotate("text", x=1998, y=3400, label="(b) Federal Government", hjust=0) +
	theme(plot.margin=unit(c(-0.2,0,0,0), "cm"), legend.position="bottom", legend.title=element_blank()) +
	guides(col=FALSE)


g3<-ggplot(dat1B_sub[dat1B_sub$Sector=="All.sectors",], aes(Year, value, col=Sector)) + 
  geom_line(size=1,col=1) + scale_x_continuous(breaks=NULL) + labs(x="", y="Millions of Dollars (Inflation-Adjusted, 2015)") + theme(legend.title=element_blank())  +
  scale_y_continuous(labels=comma) +	annotate("text", 2006, 34500, label="(a) All Sectors", hjust=0) +
  theme(plot.margin=unit(c(0,0,-0.2,0.8), "cm"), axis.title.y=element_text(hjust=3, vjust=1)) +
  guides(col=FALSE)

g4<-ggplot(dat1B_sub[dat1B_sub$Sector=="Federal.government",], aes(Year, value, col=Sector)) + 
  geom_line(size=1,col=2) + scale_x_continuous(breaks=seq(2006, 2015, 1)) + labs(y=" \n \n ", x="") + theme(legend.title=element_blank()) +
  	 scale_y_continuous(labels=comma)+ 	annotate("text", x=2006, y=3400, label="(b) Federal Government", hjust=0) +
	theme(plot.margin=unit(c(-0.2,0,0,0), "cm"), legend.position="bottom", legend.title=element_blank()) +
	guides(col=FALSE)


## for full dataset 
grid.arrange(g1, g2, layout_matrix = rbind(1,2), top="1B. Intramural R&D Expenditure: Natural Sciences and Engineering")
## for 2006 - 2015
grid.arrange(g3, g4, layout_matrix = rbind(1,2), top="1B. Intramural R&D Expenditure: Natural Sciences and Engineering")


### C
head(dat1C)
dat1C<-gather(dat1C,  Year, value)
colnames(dat1C)[2]<-"Sector"
dat1C_sub<-dat1C[dat1C$Year>2005,]


g1<-ggplot(dat1C[dat1C$Sector=="All.sectors",], aes(Year, value, col=Sector)) + 
  geom_line(size=1,col=1) + scale_x_continuous(breaks=NULL) + labs(x="", y="Millions of Dollars (Inflation-Adjusted, 2015)") + theme(legend.title=element_blank())  +
  scale_y_continuous(labels=comma)+ 	annotate("text", 1998, 3500, label="(a) All Sectors", hjust=0) +
  theme(plot.margin=unit(c(0,0,-0.2,0.8), "cm"), axis.title.y=element_text(hjust=3, vjust=1)) +
  guides(col=FALSE)

g2<-ggplot(dat1C[dat1C$Sector=="Federal.government",], aes(Year, value, col=Sector)) + 
  geom_line(size=1,col=2) + scale_x_continuous(breaks=seq(1998, 2015, 1)) + labs(y=" \n \n ", x="") + theme(legend.title=element_blank()) +
  	 scale_y_continuous(labels=comma)+ 	annotate("text", x=1998, y=310, label="(b) Federal Government", hjust=0) +
	theme(plot.margin=unit(c(-0.2,0,0,0), "cm"), legend.position="bottom", legend.title=element_blank()) +
	guides(col=FALSE)


g3<-ggplot(dat1C_sub[dat1C_sub$Sector=="All.sectors",], aes(Year, value, col=Sector)) + 
  geom_line(size=1,col=1) + scale_x_continuous(breaks=NULL) + labs(x="", y="Millions of Dollars (Inflation-Adjusted, 2015)") + theme(legend.title=element_blank())  +
  scale_y_continuous(labels=comma)+ 	annotate("text", 2006, 3500, label="(a) All Sectors", hjust=0) +
  theme(plot.margin=unit(c(0,0,-0.2,0.8), "cm"), axis.title.y=element_text(hjust=3, vjust=1)) +
  guides(col=FALSE)

g4<-ggplot(dat1C_sub[dat1C_sub$Sector=="Federal.government",], aes(Year, value, col=Sector)) + 
  geom_line(size=1,col=2) + scale_x_continuous(breaks=seq(2006, 2015, 1)) + labs(y=" \n \n ", x="") + theme(legend.title=element_blank()) +
  	 scale_y_continuous(labels=comma)+ 	annotate("text", x=2006, y=310, label="(b) Federal Government", hjust=0) +
	theme(plot.margin=unit(c(-0.2,0,0,0), "cm"), legend.position="bottom", legend.title=element_blank()) +
	guides(col=FALSE)


## for full dataset 
grid.arrange(g1, g2, layout_matrix = rbind(1,2), top="1C. Intramural R&D Expenditure: Social Sciences and Humanities")
## for 2006 - 2015
grid.arrange(g3, g4, layout_matrix = rbind(1,2), top="1C. Intramural R&D Expenditure: Social Sciences and Humanities")

#-----------------------#-----------------------#-----------------------
	#----------------------- GRAPH 2 #-----------------------
#-----------------------#-----------------------#-----------------------
# NOTES		
# please also create an identical graph with data from 2006-2015 only		
# could you please create versions with and without the percentage value		

dat2<-gather(dat2, "Year")
colnames(dat2)[2]<-'Sector'
unique(dat2$Sector)

## renaming for labels
dat2$Sector[dat2$Sector=="Federal.government..intramural."]<- "Federal government (intramural)"
dat2$Sector[dat2$Sector=="Higher.education"]<- "Higher education"
dat2$Sector[dat2$Sector=="Business.enterprise"]<- "Business enterprise"
dat2$Sector[dat2$Sector=="Canadian.non.profit.institutions"]<- "Canadian non-profit institutions"
dat2$Sector[dat2$Sector=="Foreign.performers"]<- "Foreign performers"
dat2$Sector[dat2$Sector=="Provincial.and.municipal.governments"]<- "Provincial and municipal governments"
dat2$Sector[dat2$Sector=="Other.Canadian.performers"]<- "Other Canadian performers"
dat2$Sector[dat2$Sector=="Federal.Expenditures.as.a.Percentage.of.GERD"]<- "Federal Expenditures as a % of GERD"


g1<-ggplot(dat2[!dat2$Sector=="Federal Expenditures as a % of GERD",],
			aes(Year, value, fill=Sector)) + scale_y_continuous(labels=comma,breaks=seq(0, 2500, 500)) +geom_bar(position="dodge",stat="identity") +
			labs(x="", y="Millions of Dollars \n(Inflation-Adjusted, 2015)") +
			guides(fill=guide_legend(nrow=4,byrow=TRUE)) + theme(legend.position="bottom", legend.title=element_blank(),
				plot.margin=unit(c(0,0,0,0), "cm")) +
			scale_x_continuous(breaks=seq(1998, 2015, 1))


 ### with percentage of GERD 
g2<-ggplot(dat2[dat2$Sector=="Federal Expenditures as a % of GERD",],
			aes(Year, value)) + labs(x="", y="\n % of GERD\n ")+
		geom_line() + theme(plot.margin=unit(c(0,0,-1,0), "cm"))+
		scale_x_continuous(breaks=NULL)


## for 2006 - 2015
dat2_sub<-dat2[dat2$Year>2005,]
g1.sub<-ggplot(dat2_sub[!dat2_sub$Sector=="Federal Expenditures as a % of GERD",],
			aes(Year, value, fill=Sector)) + scale_y_continuous(labels=comma,breaks=seq(0, 2500, 500)) +geom_bar(position="dodge",stat="identity") +
			labs(x="", y="Millions of Dollars \n(Inflation-Adjusted, 2015)") +
			guides(fill=guide_legend(nrow=4,byrow=TRUE)) + theme(legend.position="bottom", legend.title=element_blank(),
				plot.margin=unit(c(0,0,0,0), "cm")) +
			scale_x_continuous(breaks=seq(1998, 2015, 1))


 ### with percentage of GERD 
g2.sub<-ggplot(dat2_sub[dat2_sub$Sector=="Federal Expenditures as a % of GERD",],
			aes(Year, value)) + labs(x="", y="\n % of GERD\n ")+
		geom_line() + theme(plot.margin=unit(c(0,0,-1,0), "cm"))+
		scale_x_continuous(breaks=NULL)

## plot without %GERD
print(g1+ ggtitle("2. Intramural and Extramural R&D Funded by the Federal Government in the Natural Sciences and Engineering ") +
	 theme(plot.title = element_text(size = 12,)))
print(g1.sub+ ggtitle("2. Intramural and Extramural R&D Funded by the Federal Government in the Natural Sciences and Engineering ") +
	 theme(plot.title = element_text(size = 12,)))

## plot with % GERD
grid.arrange(g2, g1, layout_matrix = rbind(1,2,2,2), top="2. Intramural and Extramural R&D Funded by the Federal Government in the Natural Sciences and Engineering ")
grid.arrange(g2.sub, g1.sub, layout_matrix = rbind(1,2,2,2), top="2. Intramural and Extramural R&D Funded by the Federal Government in the Natural Sciences and Engineering ")

#-----------------------#-----------------------#-----------------------
	#----------------------- GRAPH 3 #-----------------------
#-----------------------#-----------------------#-----------------------


dat3<-gather(dat3, "Year")
colnames(dat3)[2]<-'Sector'
unique(dat3$Sector)

## renaming for labels
dat3$Sector[dat3$Sector=="Federal.government..intramural."]<- "Federal government (intramural)"
dat3$Sector[dat3$Sector=="Higher.education"]<- "Higher education"
dat3$Sector[dat3$Sector=="Business.enterprise"]<- "Business enterprise"
dat3$Sector[dat3$Sector=="Canadian.non.profit.institutions"]<- "Canadian non-profit institutions"
dat3$Sector[dat3$Sector=="Foreign.performers"]<- "Foreign performers"
dat3$Sector[dat3$Sector=="Provincial.and.municipal.governments"]<- "Provincial and municipal governments"
dat3$Sector[dat3$Sector=="Other.Canadian.performers"]<- "Other Canadian performers"
dat3$Sector[dat3$Sector=="Federal.Expenditures.as.a.Percentage.of.GERD"]<- "Federal Expenditures as a % of GERD"


g1<-ggplot(dat3[!dat3$Sector=="Federal Expenditures as a % of GERD",],
			aes(Year, value, fill=Sector)) + geom_bar(position="dodge",stat="identity") +  scale_y_continuous(labels=comma) +
			labs(x="", y="Millions of Dollars \n(Inflation-Adjusted, 2015)") +
			guides(fill=guide_legend(nrow=4,byrow=TRUE)) + theme(legend.position="bottom", legend.title=element_blank(),
				plot.margin=unit(c(0,0,0,0), "cm")) +
			scale_x_continuous(breaks=seq(1998, 2015, 1))


 ### with percentage of GERD 
g2<-ggplot(dat3[dat3$Sector=="Federal Expenditures as a % of GERD",],
			aes(Year, value)) + labs(x="", y="\n % of GERD\n ")+
		geom_line() + theme(plot.margin=unit(c(0,0,-1,0), "cm"))+
		scale_x_continuous(breaks=NULL)


## for 2006 - 2015
dat3_sub<-dat3[dat3$Year>2005,]
g1.sub<-ggplot(dat3_sub[!dat3_sub$Sector=="Federal Expenditures as a % of GERD",],
			aes(Year, value, fill=Sector)) + geom_bar(position="dodge",stat="identity") +  scale_y_continuous(labels=comma) +
			labs(x="", y="Millions of Dollars \n(Inflation-Adjusted, 2015)") +
			guides(fill=guide_legend(nrow=4,byrow=TRUE)) + theme(legend.position="bottom", legend.title=element_blank(),
				plot.margin=unit(c(0,0,0,0), "cm")) +
			scale_x_continuous(breaks=seq(1998, 2015, 1))


 ### with percentage of GERD 
g2.sub<-ggplot(dat3_sub[dat3_sub$Sector=="Federal Expenditures as a % of GERD",],
			aes(Year, value)) + labs(x="", y="\n % of GERD\n ")+
		geom_line() + theme(plot.margin=unit(c(0,0,-1,0), "cm"))+
		scale_x_continuous(breaks=NULL)

## plot without %GERD
print(g1+ ggtitle("3. Intramural and Extramural R&D Funded by the Federal Government in the Social Sciences and Humanities") +
	 theme(plot.title = element_text(size = 12,)))
print(g1.sub+ ggtitle("3. Intramural and Extramural R&D Funded by the Federal Government in the Social Sciences and Humanities") +
	 theme(plot.title = element_text(size = 12,)))

## plot with % GERD
grid.arrange(g2, g1, layout_matrix = rbind(1,2,2,2), top="3. Intramural and Extramural R&D Funded by the Federal Government in the Social Sciences and Humanities")
grid.arrange(g2.sub, g1.sub, layout_matrix = rbind(1,2,2,2), top="3. Intramural and Extramural R&D Funded by the Federal Government in the Social Sciences and Humanities")


#-----------------------#-----------------------#-----------------------
	#----------------------- GRAPH 4 #-----------------------
# #-----------------------#-----------------------#-----------------------
# please include identical graphs that include data from only 2006-2015							
# how to make scales more comparable for intra and extramural expend - ie on intr NRC is in different range							
dat4<-gather(dat4, year, value, `2000`:`2016`)
dat4$year<-as.numeric(dat4$year)
colnames(dat4)<-c("Expenditures", "Sector", "Department", "year", "value")

### A  - stacked bar, total fed expenditures
dat4A<-dat4[dat4$Expenditures=="Total expenditures",]
dat4A_sub<-dat4A[dat4A$year>2005,]
head(dat4A)
g1<-ggplot(dat4A[!dat4A$Department=="Total Departments and Agencies",], aes(year, value, col=Department)) + 
	geom_line() +  scale_y_continuous(breaks=seq(100, 1100, 100),labels=comma) +
			labs(x="", y="Millions of Dollars \n(Inflation-Adjusted, 2015)", title="") +
			guides(col=guide_legend(nrow=3,byrow=TRUE)) + theme(legend.position="bottom", legend.title=element_blank()) +
					#axis.title.y = element_text( vjust = 0, hjust=0)) +
			scale_x_continuous(breaks=seq(2000, 2016, 1)) #+ 	annotate("text", 2000, 1100, label="(b)")



g2<-ggplot(dat4A[dat4A$Department=="Total Departments and Agencies",], aes(year, value)) + 
	geom_line() +  scale_y_continuous(breaks=seq(5500, 8500, 1000),labels=comma) +
			labs(x="", y="Millions of Dollars \n(Inflation-Adjusted, 2015)", title="Total federal expenditures on R&D") +
			guides(fill=NULL) + theme(axis.title.y = element_text( vjust = 10, hjust=0),
				plot.margin=unit(c(0,0,-1.75,0.25), "cm")) +
			scale_x_continuous(breaks=NULL)#+ 	annotate("text", 2000, 9000, label="(a)")


g3<-ggplot(dat4A_sub[!dat4A_sub$Department=="Total Departments and Agencies",], aes(year, value, col=Department)) + 
	geom_line() +  scale_y_continuous(breaks=seq(100, 1100, 100),labels=comma) +
			labs(x="", y="Millions of Dollars \n(Inflation-Adjusted, 2015)", title="") +
			guides(col=guide_legend(nrow=3,byrow=TRUE)) + theme(legend.position="bottom", legend.title=element_blank()) +
					#axis.title.y = element_text( vjust = 0, hjust=0)) +
			scale_x_continuous(breaks=seq(2006, 2016, 1)) #+ 	annotate("text", 2000, 1100, label="(b)")



g4<-ggplot(dat4A_sub[dat4A_sub$Department=="Total Departments and Agencies",], aes(year, value)) + 
	geom_line() +  scale_y_continuous(breaks=seq(5500, 8500, 1000),labels=comma) +
			labs(x="", y="Millions of Dollars \n(Inflation-Adjusted, 2015)", title="Total federal expenditures on R&D") +
			guides(fill=NULL) + theme(axis.title.y = element_text( vjust = 10, hjust=0),
				plot.margin=unit(c(0,0,-1.75,0.25), "cm")) +
			scale_x_continuous(breaks=NULL)#+ 	annotate("text", 2000, 9000, label="(a)")


grid.arrange(g2, g1,  layout_matrix = rbind(1,2,2,2,2))
grid.arrange(g4, g3,  layout_matrix = rbind(1,2,2,2,2))

### B - stacked bar, extramural fed expenditures	
dat4B<-dat4[dat4$Expenditures=="Extramural",]
dat4B_sub<-dat4B[dat4B$year>2005,]

g1<-ggplot(dat4B[!dat4B$Department=="Extramural as % of Total",], aes(year, value, col=Department)) + 
	geom_line() +  scale_y_continuous(breaks=seq(0, 1200, 100),labels=comma) +
			labs(x="", y="Millions of Dollars \n(Inflation-Adjusted, 2015)", title="") +
			guides(col=guide_legend(nrow=3,byrow=TRUE)) + theme(legend.position="bottom", legend.title=element_blank()) +
					#axis.title.y = element_text( vjust = 0, hjust=0)) +
			scale_x_continuous(breaks=seq(2000, 2016, 1)) #+ 	annotate("text", 2000, 1200, label="(b)")



g2<-ggplot(dat4B[dat4B$Department=="Extramural as % of Total",], aes(year, value)) + 
	geom_line() +  scale_y_continuous(breaks=seq(0, 70, 10),labels=comma) +
			labs(x="", y="% Total Federal \nR&D Expenditures\n ", title="Federal Extramural Expenditures on R&D") +
			guides(fill=NULL) + theme(#axis.title.y = element_text( vjust = 0, hjust=0),
				plot.margin=unit(c(0,0,-1.75,0.25), "cm")) +
			scale_x_continuous(breaks=NULL)#+ 	annotate("text", 2000, 70, label="(a)")

g3<-ggplot(dat4B_sub[!dat4B_sub$Department=="Extramural as % of Total",], aes(year, value, col=Department)) + 
	geom_line() +  scale_y_continuous(breaks=seq(0, 1200, 100),labels=comma) +
			labs(x="", y="Millions of Dollars \n(Inflation-Adjusted, 2015)", title="") +
			guides(col=guide_legend(nrow=3,byrow=TRUE)) + theme(legend.position="bottom", legend.title=element_blank()) +
					#axis.title.y = element_text( vjust = 0, hjust=0)) +
			scale_x_continuous(breaks=seq(2006, 2016, 1)) #+ 	annotate("text", 2000, 1200, label="(b)")



g4<-ggplot(dat4B_sub[dat4B_sub$Department=="Extramural as % of Total",], aes(year, value)) + 
	geom_line() +  scale_y_continuous(breaks=seq(0, 70, 10),labels=comma) +
			labs(x="", y="% Total Federal \nR&D Expenditures\n ", title="Federal Extramural Expenditures on R&D") +
			guides(fill=NULL) + theme(#axis.title.y = element_text( vjust = 0, hjust=0),
				plot.margin=unit(c(0,0,-1.75,0.25), "cm")) +
			scale_x_continuous(breaks=NULL)#+ 	annotate("text", 2000, 70, label="(a)")


grid.arrange(g2, g1,  layout_matrix = rbind(1,2,2,2,2))
grid.arrange(g4, g3,  layout_matrix = rbind(1,2,2,2,2))


### C - stacked bar, intramural fed expenditures	
dat4C<-dat4[dat4$Expenditures=="Intramural",]
dat4C<-dat4C[!dat4C$Department=="National Research Council Canada",]
dat4C_sub<-dat4C[dat4C$year>2005,]

g1<-ggplot(dat4C[!dat4C$Department=="Intramural as % of total ",], aes(year, value, col=Department)) + 
	geom_line() +  scale_y_continuous(breaks=seq(0, 80, 10),labels=comma) +
			labs(x="", y="Millions of Dollars \n(Inflation-Adjusted, 2015)", title="") +
			guides(col=guide_legend(nrow=3,byrow=TRUE)) + theme(legend.position="bottom", legend.title=element_blank()) +
					#axis.title.y = element_text( vjust = 0, hjust=0)) +
			scale_x_continuous(breaks=seq(2000, 2016, 1)) #+ 	annotate("text", 2000, 80, label="(b)")



g2<-ggplot(dat4C[dat4C$Department=="Intramural as % of total ",], aes(year, value)) + 
	geom_line() +  scale_y_continuous(breaks=seq(0, 60, 10),labels=comma) +
			labs(x="", y="% Total Federal \nR&D Expenditures", title="Federal Intramural Expenditures on R&D* \n(does not include NRC)") +
			guides(fill=NULL) + theme(#axis.title.y = element_text( vjust = 0, hjust=0),
				plot.margin=unit(c(0,0,-1.75,0.25), "cm")) +
			scale_x_continuous(breaks=NULL)#+ 	annotate("text", 2000, 60, label="(a)")

g3<-ggplot(dat4C_sub[!dat4C_sub$Department=="Intramural as % of total ",], aes(year, value, col=Department)) + 
	geom_line() +  scale_y_continuous(breaks=seq(0, 80, 10),labels=comma) +
			labs(x="", y="Millions of Dollars \n(Inflation-Adjusted, 2015)", title="") +
			guides(col=guide_legend(nrow=3,byrow=TRUE)) + theme(legend.position="bottom", legend.title=element_blank()) +
					#axis.title.y = element_text( vjust = 0, hjust=0)) +
			scale_x_continuous(breaks=seq(2006, 2016, 1)) #+ 	annotate("text", 2000, 80, label="(b)")



g4<-ggplot(dat4C_sub[dat4C_sub$Department=="Intramural as % of total ",], aes(year, value)) + 
	geom_line() +  scale_y_continuous(breaks=seq(0, 60, 10),labels=comma) +
			labs(x="", y="% Total Federal \nR&D Expenditures", title="Federal Intramural Expenditures on R&D* \n(does not include NRC)") +
			guides(fill=NULL) + theme(#axis.title.y = element_text( vjust = 0, hjust=0),
				plot.margin=unit(c(0,0,-1.75,0.25), "cm")) +
			scale_x_continuous(breaks=NULL)#+ 	annotate("text", 2000, 60, label="(a)")


grid.arrange(g2, g1,  layout_matrix = rbind(1,2,2,2,2))
grid.arrange(g4, g3,  layout_matrix = rbind(1,2,2,2,2))



### D - line graph, Fed exp. intra vs extra
dat4D<-rbind(dat4B, dat4C)
dat4D<-dat4[dat4$'Department'=="National Research Council Canada",]
dat4D<-dat4D[!dat4D$'Expenditures'=="Total expenditures",]

ggplot(dat4D, aes(year, value, col=Expenditures)) + geom_line() + labs(x="", y="Millions of Dollars \n(Inflation Adjusted, 2015)",
	title="Federal Expenditure on the National Research Council Canada") +
guides(fill=guide_legend(nrow=2,byrow=TRUE)) + theme(legend.position="bottom", legend.title=element_blank())  +
scale_y_continuous(breaks=seq(100, 800, 100))+scale_x_continuous(breaks=seq(2000, 2016, 1))


ggplot(dat4D[dat4D$year>2005,], aes(year, value, col=Expenditures)) + geom_line() + labs(x="", y="Millions of Dollars \n(Inflation Adjusted, 2015)",
	title="Federal Expenditure on the National Research Council Canada") +
guides(fill=guide_legend(nrow=2,byrow=TRUE)) + theme(legend.position="bottom", legend.title=element_blank())  +
scale_y_continuous(breaks=seq(100, 800, 100))+scale_x_continuous(breaks=seq(2006, 2016, 1))







#-----------------------#-----------------------#-----------------------
	#----------------------- GRAPH 5 #-----------------------
#-----------------------#-----------------------#-----------------------
dat5A<-gather(dat5A, year, value, `2000`:`2016`)
colnames(dat5A)[1]<-"Sector"
dat5B<-gather(dat5B, year, value, `2000`:`2016`)
colnames(dat5B)[1]<-"Sector"
dat5B$year<-as.numeric(dat5B$year)

### A 
g1<-ggplot(dat5A, aes(year, value, fill=Sector)) + geom_bar(stat="identity", position="dodge") + 
	guides(fill=guide_legend(nrow=3,byrow=TRUE)) + theme(legend.position="bottom", legend.title=element_blank())  +
	scale_y_continuous(breaks=seq(0, 4500, 500), labels=comma) +
	labs(x="", y="Number of Personnel", title="5A. Federal Personnel Engaged in R&D")

### B
g2<-ggplot(dat5B, aes(year, value, col=Sector)) + geom_line()+
	guides(col=guide_legend(nrow=3,byrow=TRUE)) + theme(legend.position="bottom", legend.title=element_blank())  +
	scale_y_continuous(breaks=seq(0, 25, 2)) +
	labs(x="", y="%", title="5B.Percentage of Federal Personnel Engaged in R&D by Department and Agency")

## print graphs to device
g1
g2
g1 %+% dat5A[dat5A$year>2005,]
g2 %+% dat5B[dat5B$year>2005,]

#-----------------------#-----------------------#-----------------------
	#----------------------- GRAPH 6 #-----------------------
#-----------------------#-----------------------#-----------------------
dat6<-gather(dat6, year, value, `2000`:`2013`)
colnames(dat6)[1]<-"Sector"
colnames(dat6)[2]<-"Occupation"
colnames(dat6)[3]<-"Type"
dat6$year<-as.numeric(dat6$year)

dat6A<-dat6[dat6$Type=="Natural sciences and engineering",]
dat6A<-dat6[!dat6$Sector=="Total performing sector",]

dat6B<-dat6[which(dat6$Sector=="Higher education" & dat6$Occupation=="Researchers"),]
dat6B<-dat6B[!(dat6B$Type=="Total sciences"),]

dat6C<-dat6[dat6$Sector=="Higher education" ,]
dat6C<-dat6C[!(dat6C$Occupation=="Researchers"| dat6C$Occupation=="Total personnel"),]


### A - Bar: Personnel Engaged in R&D in the Natural Sciences and Engineering, by Sector
g1<-ggplot(dat6A,
			aes(year, value, fill=Sector)) + geom_bar(position="dodge",stat="identity") +  scale_y_continuous(labels=comma) +
			labs(x="", y="Number of personnel", title="Personnel Engaged in R&D in the Natural Sciences and Engineering, by Sector") +
			guides(col=guide_legend(nrow=4,byrow=TRUE)) + theme(legend.position="bottom", legend.title=element_blank(),
				plot.margin=unit(c(0,0,0,0), "cm")) + guides(fill=guide_legend(nrow=1)) +
			scale_x_continuous(breaks=seq(2000, 2013, 1))
g1


### B - Line: Personnel Engaged in R&D in Higher Education 
g2<-ggplot(dat6B,
			aes(year, value, col=Type)) + geom_line() +  scale_y_continuous(breaks=seq(15000, 33000, 2000),labels=comma) +
			labs(x="", y="Number of personnel", title="Personnel Engaged in R&D in Higher Education") +
			guides(fill=guide_legend(nrow=4,byrow=TRUE)) + theme(legend.position="bottom", legend.title=element_blank(),
				plot.margin=unit(c(0,0,0,0), "cm")) + guides(col=guide_legend(nrow=1)) +
			scale_x_continuous(breaks=seq(2000, 2013, 1))
g2



### C - Line: Percent of Canadian Researchers in Higher Education

g3<-ggplot(dat6C[dat6C$Type=="Natural sciences and engineering",],
			aes(year, value)) + geom_line(col=1) +  scale_y_continuous(breaks=seq(10,17, 1),labels=comma) +
			labs(x="", y="% researchers in Higher Education") +
			guides(col=FALSE) + theme(legend.position="bottom", legend.title=element_blank(),axis.title.y=element_text(hjust=-30),
				plot.margin=unit(c(0,0,0,0), "cm")) + guides(col=guide_legend(nrow=1)) +
			scale_x_continuous(breaks=NULL) + annotate("text", 2000, 18, hjust=0, label="(a) Natural sciences and engineering")

g4<-ggplot(dat6C[dat6C$Type=="Social sciences and humanities",],
			aes(year, value)) + geom_line(col=1) +  scale_y_continuous(breaks=seq(76,82, 1),labels=comma) +
			labs(x="", y="") +
			guides(col=FALSE) + theme(legend.position="bottom", legend.title=element_blank(),
				plot.margin=unit(c(0,0,0,0), "cm")) + guides(col=guide_legend(nrow=1)) +
			scale_x_continuous(breaks=seq(2000, 2013, 1)) + annotate("text", 2000, 82, hjust=0, label="(b) Social sciences and humanities")

grid.arrange(g3, g4, top="Percent of Canadian Researchers in Higher Education")

dev.off()