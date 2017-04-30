## Read in and explore the GYA data

setwd("/Users/kristinatietjen/Documents/git_hub/gya-research")

theme_set(theme_minimal(base_size=14))
## read data

d.v.a<-read.csv("data/gya_demand.vrs.availibility.csv", header=TRUE)

require(ggplot2);require(RColorBrewer); require(tidyr);library(gtable); library(grid)

palette(brewer.pal(n=8, name="Dark2"))

## graph of difference between availability and demand

pdf(file="figures/funding_gap.pdf", height=7, width =11)

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
d.v.a<-read.csv("data/gya_demand.vrs.availibility.csv", header=TRUE)
fund<-subset(d.v.a, select = c("year", "nserc.per.researcher", "nserc.per.researcher.fun", "ssh.per.researcher", "ssh.per.reseracher.fun"))
grant<-ldply(strsplit(fund$type, "."))

fund_long<-gather(fund, type, amount, -year)



ggplot(fund_long, aes(year, c(amount, fun.amount), col=grant, group=grant))+ facet_wrap(~type)
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
