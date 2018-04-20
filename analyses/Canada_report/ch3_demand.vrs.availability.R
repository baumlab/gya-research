## Read in and explore the GYA data

setwd("/Users/kristinatietjen/Documents/git_hub/gya-research")

theme_set(theme_minimal(base_size=14))
## read data

d.v.a<-read.csv("data/gya_demand.vrs.availibility.csv", header=TRUE)

require(ggplot2);require(RColorBrewer); require(tidyr);library(gtable); library(grid)
require(stringr); require(ggplot2)
library('plyr')

palette(brewer.pal(n=8, name="Dark2"))

## graph of difference between availability and demand

pdf(file="figures/funding_gap.pdf", height=7, width =11)   ### excel sheet has been updated since this code was last run so it may be broken now

gap_long<-gather(d.v.a, grant, amount, -year, -no.researchers.nse, -nse.discovery, -nse.innovation, -no.researchers.ssh, -ssh.discovery, -ssh.innovation)

ggplot(gap_long, aes(year, amount, col=grant, group=grant))+
  geom_line(size=1) + scale_x_continuous(breaks=2005:2015) + 
  labs(x="", y="Total Expenditure Per Canadian Researcher \n(2016 constant dollars)") + 
  theme(legend.title=element_blank(),
        plot.margin=unit(c(0.2,0,-0.2,0.8), "cm"),
        axis.title.y=element_text(hjust=0.5, vjust=-1,size=12),
        strip.background = element_blank(),
        axis.text.x=element_text(angle=0),
        legend.position=c(0.9,0.90),
        strip.text.x = element_blank())  +
  scale_colour_manual(values=c(brewer.pal(2, name="Paired")), labels=c("NSE", "SSH"))


dev.off()


#### figure for nse per researcher and nserc fundamental per reseracher and also for ssh
trend<-read.csv("data/gya_funding.trend.csv", header=TRUE, check.names=FALSE)
differ.long<-gather(trend, year, value, -type)

#add column so that the data may be facet wrapped for granting agency
differ.long$grant<-ifelse(grepl( "nserc", differ.long$type),"(a) NSE", "(b) SSH")

#change levels so fundamental is on top in legend
differ.long$type<-factor(differ.long$type, levels(differ.long$type)[c(2,1,4,3)])

pdf(file="figures/funding_trend_3.5_1Jun.pdf", height=8, width =11)

ggplot(differ.long, aes(as.numeric(year), value, col=type)) + 
  geom_line(size=1) + scale_x_continuous(breaks=seq(2005, 2015,1)) + 
  labs(x="", y="Expenditure per Canadian researcher\n(2016 constant dollars)") + 
  facet_wrap(~ grant)+
  theme(aspect.ratio=3/4, legend.title=element_blank(),
        legend.position=c(0.6, 0.85),
        #plot.margin=unit(c(0,0,-0.2,0.8), "cm"),
        axis.title.y=element_text(hjust=0.5, vjust=-1),
        axis.text.x=element_text(angle=0),
        strip.text.x = element_text(hjust=-0.001, face='bold'))+
  # strip.background = element_blank(),
  #scale_y_continuous(labels=comma)+
  # annotate("text", 1998, 34000, label="(a) All Sectors", hjust=0) +
  guides(col=guide_legend(nrow=4))+
  scale_colour_manual(values=c(brewer.pal(c(4), name="Paired")), labels=c("Discovery","Innovation", "Insight", "Connection"))

dev.off()
