
### creating plots for Megan Dodd + Julia Baum for global young academy document
require(tidyr); require(scales); require(gridExtra); require(RColorBrewer); require(ggplot2)


setwd("/Users/jpwrobinson/Google_Drive/R_PROJECTS_DATA/VISUALISATIONS/global-young-academy-gdp")
setwd("/Users/IMAC3/Google\ Drive/R_PROJECTS_DATA/VISUALISATIONS/global-young-academy-gdp")
setwd("/Users/kristinatietjen/Google_Drive/R_Projects/GYA")
theme_set(theme_minimal(base_size=14))

palette(brewer.pal(n=8, name="Dark2"))

## read in data
dat1<-read.csv("data/part5-international-july/aug11/NSERC_grants_10Apr.csv", check.names=FALSE)
dat2<-read.csv("data/part5-international-july/aug11/SSHRC_grants_apr10.csv", check.names=FALSE)
dat3<-read.csv("data/part5-international-july/aug11/NSERC_success.csv", check.names=FALSE)
dat4<-read.csv("data/part5-international-july/aug11/SSHRC_success.csv", check.names=FALSE)
dat5<-read.csv("data/part5-international-july/aug11/CIHR_grants_Apr17.csv", check.names=FALSE)
dat6<-read.csv("data/part5-international-july/aug11/CIHR_success.csv", check.names=FALSE)
dat7<-read.csv("data/part5-international-july/aug11/highered_researchers.csv", check.names=FALSE)

#pdf(file="figures/GYA_agencydata_apr10_2017.pdf", height=7, width=11)
# png(file="figures/GYA_agencydata_aug11_2016.png", pointsize=20,height=900, width=1500)
#-----------------------#-----------------------#-----------------------
	#----------------------- GRAPH 1 #-----------------------
#-----------------------#-----------------------#-----------------------

# NSERC

jpeg(file="figures/jpeg/apr2017/GYA_agencydata_3.1_apr10_2017.jpeg")

dat1<-gather(dat1,  Year, value, -panel, -Type)
dat1$value<-as.numeric(dat1$value)


ylabel <- "Expenditure\n (Millions of \n2016 Constant Dollars)"
g1<-ggplot(dat1[dat1$panel=='top',], aes(Year, value, col=Type, group=Type)) + 
  geom_line(size=1) + #scale_x_continuous(breaks=NULL) + 
  labs(x="", y=ylabel) + 
  theme(legend.title=element_blank(),
  	plot.margin=unit(c(0.2,0,-0.2,0.8), "cm"),
  	 axis.title.y=element_text(hjust=0.5, vjust=-1,size=12),
  	 axis.text.x=element_blank(),
  	 strip.background = element_blank(),
  	 legend.position=c(0.8,0.2),
       strip.text.x = element_blank())  +
  scale_y_continuous(labels=comma) +
  scale_colour_manual(values=c(brewer.pal(2, name="Paired")), labels=c("Discovery", "Innovation"))+
    annotate("text", "2005", 500, label="(a)", hjust=2) 
  # facet_wrap(~Type, scales="free")+
  # guides(col=FALSE)

ylabel <- "Average Award Value\n (2016 Constant Dollars)"
g2<-ggplot(dat1[dat1$panel=='mid',], aes(Year, value, col=Type, group=Type)) + 
  geom_line(size=1) + #scale_x_continuous(breaks=NULL) + 
  labs(x="", y=ylabel) + 
  theme(legend.title=element_blank(),
  	plot.margin=unit(c(0,0,-0.2,0.45), "cm"),
  	 axis.title.y=element_text(hjust=0.5, vjust=-1,size=12),
  	 axis.text.x=element_blank(),
  	 strip.background = element_blank(),
  	 legend.position=c(0.8,0.8),
       strip.text.x = element_blank())  +
  scale_y_continuous(labels=comma)+
  scale_colour_manual(values=c(brewer.pal(2, name="Paired")))+
    annotate("text", "2005", 100000, label="(b)", hjust=2) +
  # facet_wrap(~Type, scales="free")+
  guides(col=FALSE)

ylabel <- "Number of Grants\n Awarded (Fiscal Year)\n "
g3<-ggplot(dat1[dat1$panel=='bot',], aes(Year, value, col=Type, group=Type)) + 
  geom_line(size=1) + #scale_x_continuous(breaks=NULL) + 
  labs(x="", y=ylabel) + 
  theme(legend.title=element_blank(),
  	plot.margin=unit(c(0,0,-0.2,0.2), "cm"),
  	 axis.title.y=element_text(hjust=0.5, vjust=-1,size=12),
  	 axis.text.x=element_text(angle=0),
  	 strip.background = element_blank(),
  	 legend.position=c(0.8,0.1),
       strip.text.x = element_blank())  +
  scale_y_continuous(labels=comma)+
  scale_colour_manual(values=c(brewer.pal(2, name="Paired")))+
    annotate("text", "2005", 10000, label="(c)", hjust=2) +
  # facet_wrap(~Type, scales="free")+
  guides(col=FALSE)


print(grid.arrange(g1, g2, g3, nrow=3))

dev.off()

#-----------------------#-----------------------#-----------------------
	#----------------------- GRAPH 2 #-----------------------
#-----------------------#-----------------------#-----------------------

# SSHRC
pdf(file="figures/GYA_agencydata_3.2_apr10_2017.pdf", height=7, width=11)

dat2<-gather(dat2,  Year, value, -panel, -Type)
dat2$value<-as.numeric(dat2$value)


ylabel <- "Expenditure\n (Millions of \n2016 Constant Dollars)"
g1<-ggplot(dat2[dat2$panel=='top',], aes(Year, value, col=Type, group=Type)) + 
  geom_line(size=1) + #scale_x_continuous(breaks=NULL) + 
  labs(x="", y=ylabel) + 
  theme(legend.title=element_blank(),
  	plot.margin=unit(c(0.2,0,-0.2,0.8), "cm"),
  	 axis.title.y=element_text(hjust=0.5, vjust=-1,size=12),
  	 axis.text.x=element_blank(),
  	 strip.background = element_blank(),
  	 legend.position=c(0.9,0.4),
       strip.text.x = element_blank())  +
  scale_y_continuous(labels=comma) +
  scale_colour_manual(values=c(brewer.pal(2, name="Paired")), labels=c("Connection", "Insight"))+
    annotate("text", "2005", 160, label="(a)", hjust=2) 
  # facet_wrap(~Type, scales="free")+
  # guides(col=FALSE)

ylabel <- "Average Award Value\n (2016 Constant Dollars)"
g2<-ggplot(dat2[dat2$panel=='mid',], aes(Year, value, col=Type, group=Type)) + 
  geom_line(size=1) + #scale_x_continuous(breaks=NULL) + 
  labs(x="", y=ylabel) + 
  theme(legend.title=element_blank(),
  	plot.margin=unit(c(0,0,-0.2,0.45), "cm"),
  	 axis.title.y=element_text(hjust=0.5, vjust=-1,size=12),
  	 axis.text.x=element_blank(),
  	 strip.background = element_blank(),
  	 legend.position=c(0.8,0.8),
       strip.text.x = element_blank())  +
  scale_y_continuous(labels=comma)+
  scale_colour_manual(values=c(brewer.pal(2, name="Paired")))+
    annotate("text", "2005", 125000, label="(b)", hjust=2) +
  # facet_wrap(~Type, scales="free")+
  guides(col=FALSE)

ylabel <- "Number of Grants\n Awarded (Fiscal Year)\n "
g3<-ggplot(dat2[dat2$panel=='bot',], aes(Year, value, col=Type, group=Type)) + 
  geom_line(size=1) + #scale_x_continuous(breaks=NULL) + 
  labs(x="", y=ylabel) + 
  theme(legend.title=element_blank(),
  	plot.margin=unit(c(0,0,-0.2,0.4), "cm"),
  	 axis.title.y=element_text(hjust=0.5, vjust=-1,size=12),
  	 axis.text.x=element_text(angle=0),
  	 strip.background = element_blank(),
  	 legend.position=c(0.8,0.1),
       strip.text.x = element_blank())  +
  scale_y_continuous(labels=comma)+
  scale_colour_manual(values=c(brewer.pal(2, name="Paired")))+
    annotate("text", "2005", 3800, label="(c)", hjust=2) +
  # facet_wrap(~Type, scales="free")+
  guides(col=FALSE)


print(grid.arrange(g1, g2, g3, nrow=3))

dev.off()

#-----------------------#-----------------------#-----------------------
	#----------------------- GRAPH 3 #-----------------------
#-----------------------#-----------------------#-----------------------

# NSERC grants


dat3<-gather(dat3,  Year, value, -panel, -Type)
dat3$value<-as.numeric(dat3$value)
dat3$star<-"nostar"
dat3$star[dat3$Type=="Discovery Grants Success Rate" & dat3$Year%in%c('2012', '2013', '2014', '2015')]<-'star'
dat3$star[dat3$Type=="Discovery Grants Number of Awards" & dat3$value%in%c(2135, 2026, 2005, 2059)]<-'star'



ylabel <- "Percent of Projects\n Funded\n "
g1<-ggplot(dat3[dat3$panel=='top',], aes(Year, value, col=Type, group=Type)) + 
  geom_line(size=1) + #scale_x_continuous(breaks=NULL) + 
  geom_point(data=dat3[dat3$panel=='top' & dat3$star=='star',], aes(Year, value), col='red', shape=8)+
  labs(x="", y=ylabel) + 
  theme(legend.title=element_blank(),
  	plot.margin=unit(c(0.2,0,-0.2,0.4), "cm"),
  	 axis.title.y=element_text(hjust=0.5, vjust=-1,size=12),
  	 axis.text.x=element_blank(),
  	 strip.background = element_blank(),
  	 legend.position=c(0.9,0.4),
       strip.text.x = element_blank())  +
  scale_y_continuous(labels=comma) +
  scale_colour_manual(values=c(brewer.pal(2, name="Paired")), labels=c("Discovery", "Innovation"))+
    annotate("text", "2005", 80, label="(a)", hjust=2) 
  # facet_wrap(~Type, scales="free")+
  # guides(col=FALSE)

ylabel <- "Number of Projects\n Awarded"
g2<-ggplot(dat3[dat3$panel=='mid',], aes(Year, value, col=Type, group=Type)) + 
  geom_line(size=1) + #scale_x_continuous(breaks=NULL) + 
  geom_point(data=dat3[dat3$panel=='mid' & dat3$star=='star',], aes(Year, value), col='red', shape=8)+
  labs(x="", y=ylabel) + 
  theme(legend.title=element_blank(),
  	plot.margin=unit(c(0,0,-0.2,0.3), "cm"),
  	 axis.title.y=element_text(hjust=0.5, vjust=-1,size=12),
  	 axis.text.x=element_blank(),
  	 strip.background = element_blank(),
  	 legend.position=c(0.8,0.8),
       strip.text.x = element_blank())  +
  scale_y_continuous(labels=comma)+
  scale_colour_manual(values=c(brewer.pal(2, name="Paired")))+
    annotate("text", "2005", 2200, label="(b)", hjust=2.5) +
  # facet_wrap(~Type, scales="free")+
  guides(col=FALSE)

ylabel <- "Average Percent of Budget\n Awarded to Successful\n Applications"
g3<-ggplot(dat3[dat3$panel=='bot',], aes(Year, value, col=Type, group=Type)) + 
  geom_line(size=1) + #scale_x_continuous(breaks=NULL) + 
  labs(x="", y=ylabel) + 
  theme(legend.title=element_blank(),
  	plot.margin=unit(c(0,0,-0.2,0.4), "cm"),
  	 axis.title.y=element_text(hjust=0.5, vjust=-1,size=12),
  	 axis.text.x=element_text(angle=0),
  	 strip.background = element_blank(),
  	 legend.position=c(0.8,0.1),
       strip.text.x = element_blank())  +
  scale_y_continuous(labels=comma)+
  scale_colour_manual(values=c(brewer.pal(2, name="Paired")))+
    annotate("text", "2005", 45, label="(c)", hjust=2) +
  # facet_wrap(~Type, scales="free")+
  guides(col=FALSE)


print(grid.arrange(g1, g2, g3, nrow=3))




#-----------------------#-----------------------#-----------------------
	#----------------------- GRAPH 4 #-----------------------
#-----------------------#-----------------------#-----------------------

# SSHRC grant success


dat4<-gather(dat4,  Year, value, -panel, -Type)
dat4$value<-as.numeric(dat4$value)


ylabel <- "Percent of Projects\n Funded\n "
g1<-ggplot(dat4[dat4$panel=='top',], aes(Year, value, col=Type, group=Type)) + 
  geom_line(size=1) + #scale_x_continuous(breaks=NULL) + 
  labs(x="", y=ylabel) + 
  theme(legend.title=element_blank(),
  	plot.margin=unit(c(0.2,0,-0.2,0.8), "cm"),
  	 axis.title.y=element_text(hjust=0.5, vjust=-1,size=12),
  	 axis.text.x=element_blank(),
  	 strip.background = element_blank(),
  	 legend.position=c(0.4,0.8),
       strip.text.x = element_blank())  +
  scale_y_continuous(labels=comma) +
  scale_colour_manual(values=c(brewer.pal(2, name="Paired")), labels=c("Connection Grant", "Insight Grant", "Standard Research Grant"))+
    annotate("text", "2005", 80, label="(a)", hjust=2) 
  # facet_wrap(~Type, scales="free")+
  # guides(col=FALSE)



ylabel <- "Average Percent of Budget\n Awarded to Successful\n Applications"
g3<-ggplot(dat4[dat4$panel=='bot',], aes(Year, value, col=Type, group=Type)) + 
  geom_line(size=1) + #scale_x_continuous(breaks=NULL) + 
  labs(x="", y=ylabel) + 
  theme(legend.title=element_blank(),
  	plot.margin=unit(c(0,0,-0.2,0.8), "cm"),
  	 axis.title.y=element_text(hjust=0.5, vjust=-1,size=12),
  	 axis.text.x=element_text(angle=0),
  	 strip.background = element_blank(),
  	 legend.position=c(0.8,0.1),
       strip.text.x = element_blank())  +
  scale_y_continuous(labels=comma)+
  scale_colour_manual(values=c(brewer.pal(2, name="Paired")))+
    annotate("text", "2005", 75, label="(b)", hjust=2) +
  # facet_wrap(~Type, scales="free")+
  guides(col=FALSE)


print(grid.arrange(g1, g3, nrow=2))


#-----------------------#-----------------------#-----------------------
	#----------------------- GRAPH 5 #-----------------------
#-----------------------#-----------------------#-----------------------

# CIHR grants


pdf(file="figures/GYA_agencydata_3.3_apr12_2017.pdf", height=7, width=11)

dat5<-gather(dat5,  Year, value, -panel, -Type)
dat5$value<-as.numeric(dat5$value)


ylabel <- "Expenditure\n (Millions of \n2016 Constant Dollars)\n "
g1<-ggplot(dat5[dat5$panel=='top',], aes(Year, value, col=Type, group=Type)) + 
  geom_line(size=1) + #scale_x_continuous(breaks=NULL) + 
  labs(x="", y=ylabel) + 
  theme(legend.title=element_blank(),
  	plot.margin=unit(c(0.2,0,-0.2,0.5), "cm"),
  	 axis.title.y=element_text(hjust=0.5, vjust=-1,size=10),
  	 axis.text.x=element_blank(),
  	 strip.background = element_blank(),
  	 legend.position=c(0.9,0.4),
       strip.text.x = element_blank())  +
  scale_y_continuous(labels=comma) +
    annotate("text", "2005", 500, label="(a)", hjust=2) +
    scale_colour_manual(values=c(brewer.pal(2, name="Paired")))+
  # facet_wrap(~Type, scales="free")+
  guides(col=FALSE)

ylabel <- "Average Award Value\n (2016 Constant Dollars)"
g2<-ggplot(dat5[dat5$panel=='mid',], aes(Year, value, col=Type, group=Type)) + 
  geom_line(size=1) + #scale_x_continuous(breaks=NULL) + 
  labs(x="", y=ylabel) + 
  theme(legend.title=element_blank(),
  	plot.margin=unit(c(0,0,-0.2,0.15), "cm"),
  	 axis.title.y=element_text(hjust=0.5, vjust=-1,size=10),
  	 axis.text.x=element_blank(),
  	 strip.background = element_blank(),
  	 legend.position=c(0.8,0.8),
       strip.text.x = element_blank())  +
  scale_y_continuous(labels=comma)+
  scale_colour_manual(values=c(brewer.pal(2, name="Paired")))+
    annotate("text", "2005", 1000000, label="(b)", hjust=2) +
  # facet_wrap(~Type, scales="free")+
  guides(col=FALSE)

ylabel <- "Number of Grants\n Awarded\n "
g3<-ggplot(dat5[dat5$panel=='bot',], aes(Year, value, col=Type, group=Type)) + 
  geom_line(size=1) + #scale_x_continuous(breaks=NULL) + 
  labs(x="", y=ylabel) + 
  theme(legend.title=element_blank(),
  	plot.margin=unit(c(0,0,-0.2,0.45), "cm"),
  	 axis.title.y=element_text(hjust=0.5, vjust=-1,size=10),
  	 axis.text.x=element_blank(),
  	 strip.background = element_blank(),
  	 legend.position=c(0.9,0.9),
       strip.text.x = element_blank())  +
  scale_y_continuous(labels=comma)+
  scale_colour_manual(values=c(brewer.pal(2, name="Paired")))+
    annotate("text", "2005", 1000, label="(c)", hjust=2) +
  # facet_wrap(~Type, scales="free")+
scale_colour_manual(values=c(brewer.pal(2, name="Paired")), labels=c("Open", "Fettered"))




#-----------------------#-----------------------#-----------------------
	#----------------------- GRAPH 6 #-----------------------
#-----------------------#-----------------------#-----------------------

# CIHR grant success


dat6<-gather(dat6,  Year, value, -Type)
dat6$value<-as.numeric(dat6$value)


ylabel <- "Percent of\n Projects Funded\n "
g4<-ggplot(dat6, aes(Year, value, col=Type, group=Type)) + 
  geom_line(size=1) + #scale_x_continuous(breaks=NULL) + 
  labs(x="", y=ylabel) + 
  theme(legend.title=element_blank(),
  	plot.margin=unit(c(0,0,-0.2,0.95), "cm"),
  	 axis.title.y=element_text(hjust=0.5, vjust=-1,size=10),
  	 axis.text.x=element_text(angle=0),
  	 strip.background = element_blank(),
  	 legend.position=c(0.8,0.8),
       strip.text.x = element_blank())  +
  scale_y_continuous(labels=comma) +
  scale_colour_manual(values=c(brewer.pal(2, name="Paired")), labels=c("Core Open Operating Grant Approval Rate (%)", "Core Open Operating Grant Success Rate (%)"))+
    annotate("text", "2005", 55, label="(d)", hjust=2) 
  # facet_wrap(~Type, scales="free")+
  # guides(col=FALSE)


print(grid.arrange(g1, g2, g3,g4, nrow=4))

dev.off()
#-----------------------#-----------------------#-----------------------
  #----------------------- GRAPH 7 #-----------------------
#-----------------------#-----------------------#-----------------------


dat7<-gather(dat7,  Year, value, -type)
dat7$value<-as.numeric(dat7$value)


ylabel <- "Thousands of 2015 Constant Dollars"
g<-ggplot(dat7, aes(Year, value, col=type, group=type)) + 
  geom_line(size=1) + #scale_x_continuous(breaks=NULL) + 
  labs(x="", y=ylabel) + 
  theme(legend.title=element_blank(),
    plot.margin=unit(c(0.2,0,-0.2,0.8), "cm"),
     axis.title.y=element_text(hjust=0.5, vjust=-1,size=12),
     # axis.text.x=element_blank(),
     strip.background = element_blank(),
     legend.position=c(0.8,0.5),
       strip.text.x = element_blank())  +
  scale_y_continuous(labels=comma) +
  scale_colour_manual(values=c(brewer.pal(2, name="Paired")), labels=c("NSE", "SSH"))
    # annotate("text", "2005", 55, label="(a)", hjust=2) 
  # facet_wrap(~type, scales="free")+
  # guides(col=FALSE)
print(g)



dev.off()


