## Read in and explore the GYA data

setwd("/Users/kristinatietjen/Documents/git_hub/gya-research")

## read data

d.v.a<-read.csv("data/gya_demand.vrs.availibility.csv", header=TRUE)

require(ggplot2);require(RColorBrewer); require(tidyr);library(gtable); library(grid)

palette(brewer.pal(n=8, name="Dark2"))

##graph of demand and availability

pdf(file = "figures/demand_availability.pdf")

colnames(d.v.a)
d.v.a$total.nse<-rowSums(d.v.a[,3:4])
d.v.a$total.ssh<-rowSums(d.v.a[,7:8])

dva_long<-gather(d.v.a, type, no.res, -year, -nserc.per.researcher, -ssh.per.researcher, -nse.discovery, -nse.innovation, 
                 -ssh.discovery, -ssh.innovation, -total.nse, -total.ssh)

dva_long<-gather(dva_long, source, money, -year, -nserc.per.researcher, -ssh.per.researcher, -nse.discovery, -nse.innovation, 
                 -ssh.discovery, -ssh.innovation, -type, -no.res)

p1<-ggplot(dva_long, aes(year, no.res, col=type, group=type))+
  geom_line(size=1) + #scale_x_continuous(breaks=NULL) + 
  labs(x="", y="") + 
  theme(legend.title=element_blank(),
        axis.title.y=element_text(hjust=0.5, vjust=-1,size=12),
        strip.background = element_blank(),
        legend.position=c(0.8,0.2),
        strip.text.x = element_blank())  +
  scale_colour_manual(values=c(brewer.pal(2, name="Paired")), labels=c("NSE", "SSH"))
print(p1)

p2<-ggplot(dva_long, aes(year, money, col=source, group=source))+
  geom_line(size=1) + #scale_x_continuous(breaks=NULL) + 
  labs(x="", y="") + 
  theme(legend.title=element_blank(),
        axis.title.y=element_text(hjust=0.5, vjust=-1,size=12),
        strip.background = element_blank(),
        legend.position=c(0.8,0.2),
        strip.text.x = element_blank())  +
  scale_colour_manual(values=c(brewer.pal(4, name="Paired")), labels=c("NSE", "SSH"))
print(p2)

# extract gtable
g1 <- ggplot_gtable(ggplot_build(p1))
g2 <- ggplot_gtable(ggplot_build(p2))

# overlap the panel of 2nd plot on that of 1st plot
pp <- c(subset(g1$layout, name == "panel", se = t:r))
g <- gtable_add_grob(g1, g2$grobs[[which(g2$layout$name == "panel")]], pp$t, 
                     pp$l, pp$b, pp$l)

# axis tweaks
ia <- which(g2$layout$name == "axis-l")
ga <- g2$grobs[[ia]]
ax <- ga$children[[2]]
ax$widths <- rev(ax$widths)
ax$grobs <- rev(ax$grobs)
ax$grobs[[1]]$x <- ax$grobs[[1]]$x - unit(1, "npc") + unit(0.15, "cm")
g <- gtable_add_cols(g, g2$widths[g2$layout[ia, ]$l], length(g$widths) - 1)
g <- gtable_add_grob(g, ax, pp$t, length(g$widths) - 1, pp$b)

# draw it
grid.draw(g)



## graph of difference between availability and demand

pdf(file="figures/funding_gap.pdf")

gap_long<-gather(d.v.a, grant, amount, -year, -no.researchers.nse, -nse.discovery, -nse.innovation, -no.researchers.ssh, -ssh.discovery, -ssh.innovation)

ggplot(gap_long, aes(year, amount, col=grant, group=grant))+
  geom_line(size=1) + #scale_x_continuous(breaks=NULL) + 
  labs(x="", y="") + 
  theme(legend.title=element_blank(),
        axis.title.y=element_text(hjust=0.5, vjust=-1,size=12),
        strip.background = element_blank(),
        legend.position=c(0.8,0.2),
        strip.text.x = element_blank())  +
  scale_colour_manual(values=c(brewer.pal(2, name="Paired")), labels=c("NSE", "SSH"))


dev.off()
