
### creating plots for Megan Dodd + Julia Baum for global young academy document


## Aim: make 19 plots.
require(tidyr)
require(scales)

### return to default ggplot colors


# cols <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
cols <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
cols2 <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#000000")


setwd("/Users/jpwrobinson/Google_Drive/R_PROJECTS_DATA/VISUALISATIONS/global-young-academy-gdp")
theme_set(theme_minimal(base_size=14))



pdf(file="GYA_part2.pdf", height=7, width=12)
#I.1 S&T expenditures
# GRAPH 1: "Expenditure on Science and Technology (by sector)" - stacked bar?
# GRAPH 2 "Expenditure on S&T and R&D" - line?
# GRAPH 3 "Total Federal (intramural) Expenditure on Science & Technology, as a Percent of Total Federal Expenditure" - line?


st1<-read.csv("Data/part2/S&T_graph1.csv",header=TRUE, stringsAsFactors=TRUE, check.names=FALSE)
st1<-gather(st1, "Dollars ($ millions)")
colnames(st1)[1]<-"Sector"
st2<-read.csv("Data/part2/S&T_graph2.csv",header=TRUE, stringsAsFactors=TRUE, check.names=FALSE)
st2<-gather(st2, "Expenditure")
st2$variable.num<-rep(1:12, each=3)
st3<-read.csv("Data/part2/S&T_graph3.csv",header=TRUE, stringsAsFactors=TRUE, check.names=FALSE)
st3<-gather(st3, "Expenditure")
st3$variable.num<-rep(1:12)



ggplot(st1, aes(variable, value, fill=Sector)) + geom_bar(stat="identity") +
labs (y= "Current dollars ($ Millions)", fill="", x="", title="Expenditure on Science & Technology (by sector)") + guides(fill=guide_legend(nrow=3,byrow=TRUE)) + theme(legend.position="bottom") +
scale_fill_manual( values=cols) + scale_y_continuous(labels=comma)

ggplot(st2, aes(variable.num, value, col=Expenditure)) + geom_line(size=1) +labs (y= "2007 constant dollars ($ Millions)", col="", x="", title="Expenditure on S&T and R&D") + guides(fill=guide_legend(nrow=1,byrow=TRUE)) + theme(legend.position="bottom") +
scale_fill_manual( values=cols) + scale_x_discrete(labels=unique(st2$variable)) +
scale_y_continuous(breaks=c(5000, 6000, 7000, 8000, 9000, 10000, 11000, 12000), labels=comma)

ggplot(st3, aes(variable.num, value)) + geom_line() +labs (y= "Percent (%) of Total Federal Expenditure", col="", x="", title="Total Federal (intramural) Expenditure on Science & Technology (% of Total Federal Expenditure)") + guides(fill=guide_legend(nrow=1,byrow=TRUE)) + theme(legend.position="bottom") +
 scale_x_discrete(labels=unique(st3$variable))  + scale_y_continuous(lim=c(2.2, 2.7))



### I.2 Intramural R&D expenditures

# GRAPH 1: "Intramural R&D Expenditures in Natural Sciences and Engineering" (stacked bar graph)          
# GRAPH 2: "Intramural R&D Expenditures in Social Sciences and Humanities" (stacked bar graph)          
# y axis: 'Current Dollars ($ Millions)'          


intra1<-read.csv("Data/part2/intra_RD_graph1.csv",header=TRUE, stringsAsFactors=TRUE, check.names=FALSE)
intra1<-gather(intra1, "Performing sector")
colnames(intra1)[1]<-"Sector"
intra2<-read.csv("Data/part2/intra_RD_graph2.csv",header=TRUE, stringsAsFactors=TRUE, check.names=FALSE)
intra2<-gather(intra2, "Performing sector")
colnames(intra2)[1]<-"Sector"


ggplot(intra1, aes(variable, value, fill=Sector)) + geom_bar(stat="identity") +
labs (y= "Current dollars ($ Millions)", fill="", x="", title="Intramural R&D Expenditures in Natural Sciences and Engineering") + guides(fill=guide_legend(nrow=3,byrow=TRUE)) + theme(legend.position="bottom") +
scale_fill_manual( values=cols) + scale_y_continuous(labels=comma)


ggplot(intra2, aes(variable, value, fill=Sector)) + geom_bar(stat="identity") +
labs (y= "Current dollars ($ Millions)", fill="", x="", title="Intramural R&D Expenditures in Social Sciences and Humanities") + guides(fill=guide_legend(nrow=3,byrow=TRUE)) + theme(legend.position="bottom") +
scale_fill_manual( values=cols)+ scale_y_continuous(labels=comma)


## I.3 Extramural R&D expenditures

# GRAPH 1: 'Extramural R&D Expenditures in Natural Sciences and Engineering' (stacked bar graph?)   
# GRAPH 2: 'Extramural R&D Expenditures in Social Sciences and Humanities' (stacked bar graph?'   
# y axis: 'Current Dollars ($ Millions)'    


extra1<-read.csv("Data/part2/extra_RD_graph1.csv",header=TRUE, stringsAsFactors=TRUE, check.names=FALSE)
extra1[,2]<-NULL
colnames(extra1)[1]<-"Sector"
extra1<-gather(extra1, "Sector")

extra2<-read.csv("Data/part2/extra_RD_graph2.csv",header=TRUE, stringsAsFactors=TRUE, check.names=FALSE)
extra2[,2]<-NULL
colnames(extra2)[1]<-"Sector"
extra2<-gather(extra2, "Sector")


ggplot(extra1, aes(variable, value, fill=Sector)) + geom_bar(stat="identity") +
labs (y= "Current dollars ($ Millions)", fill="", x="", title="Extramural R&D Expenditures in Natural Sciences and Engineering") + guides(fill=guide_legend(nrow=1,byrow=TRUE)) + theme(legend.position="bottom") +
scale_fill_manual( values=cols[-c(1:3)]) + scale_y_continuous(labels=comma)


ggplot(extra2, aes(variable, value, fill=Sector)) + geom_bar(stat="identity") +
labs (y= "Current dollars ($ Millions)", fill="", x="", title="Extramural R&D Expenditures in Social Sciences and Humanities") + guides(fill=guide_legend(nrow=1,byrow=TRUE)) + theme(legend.position="bottom") +
scale_fill_manual( values=cols[-c(1:3)]) + ylim(c(0, 800))

### I.4 Federal R&D by department

# GRAPH 1: "Federal R&D Expenditure in Research Institutes  (Primarily Fundamental)" (line graph?)          
# GRAPH 2: "Federal R&D Expenditure in Research Institutes  (Primarily Applied)" (line graph?)          
# GRAPH 3: "Federal R&D Expenditure in Research Institutes  (Fundamental-Applied)" (line graph?)          
# y axis: 'Current Dollars ($ Millions)'          
# GRAPH 4: "Total Intramural and Extramural Expenditure in R&D" (line graph?)         
# y axis: 'Current Dollars ($ Millions)'          


fed1<-read.csv("Data/part2/fed_RD_graph1.csv",header=TRUE, stringsAsFactors=TRUE, check.names=FALSE)
colnames(fed1)[1]<-"Departments"
fed1<-gather(fed1, "Departments")
fed1$variable.num<-rep(1:13, each =4)

fed2<-read.csv("Data/part2/fed_RD_graph2.csv",header=TRUE, stringsAsFactors=TRUE, check.names=FALSE)
colnames(fed2)[1]<-"Departments"
fed2<-gather(fed2, "Departments")
fed2$variable.num<-rep(1:13, each =6)

fed3<-read.csv("Data/part2/fed_RD_graph3.csv",header=TRUE, stringsAsFactors=TRUE, check.names=FALSE)
colnames(fed3)[1]<-"Departments"
fed3<-gather(fed3, "Departments")
fed3$variable.num<-rep(1:13, each =9)

fed4<-read.csv("Data/part2/fed_RD_graph4.csv",header=TRUE, stringsAsFactors=TRUE, check.names=FALSE)
colnames(fed4)[1]<-"Departments"
fed4<-gather(fed4, "Departments")
fed4$variable.num<-rep(1:13, each =2)



ggplot(fed1, aes(variable.num, value, col=Departments)) + geom_line(size=1) +
labs (y= "Current dollars ($ Millions)", col="", x="", title="Federal R&D Expenditure in Research Institutes  (Primarily Fundamental)") + guides(col=guide_legend(nrow=2,byrow=TRUE)) + theme(legend.position="bottom") +
scale_color_manual( values=cols) + scale_x_discrete(labels=unique(fed1$variable))

ggplot(fed2, aes(variable.num, value, col=Departments)) + geom_line(size=1) +
labs (y= "Current dollars ($ Millions)", col="", x="", title="Federal R&D Expenditure in Research Institutes  (Primarily Applied)") + guides(col=guide_legend(nrow=2,byrow=TRUE)) + theme(legend.position="bottom") +
scale_color_manual( values=cols) + scale_x_discrete(labels=unique(fed2$variable)) + ylim(c(100, 800))

ggplot(fed3, aes(variable.num, value, col=Departments)) + geom_line(size=1) +
labs (y= "Current dollars ($ Millions)", col="", x="", title="Federal R&D Expenditure in Research Institutes  (Fundamental-Applied)") + guides(col=guide_legend(nrow=3,byrow=TRUE)) + theme(legend.position="bottom") +
scale_color_manual( values=cols2) + scale_x_discrete(labels=unique(fed3$variable))

ggplot(fed4, aes(variable.num, value, col=Departments)) + geom_line(size=1) +
labs (y= "Current dollars ($ Millions)", col="", x="", title="Total Intramural and Extramural Expenditure in R&D") + guides(col=guide_legend(nrow=1,byrow=TRUE)) + theme(legend.position="bottom") +
scale_color_manual( values=cols[-c(1:3)]) + scale_x_discrete(labels=unique(fed3$variable)) + scale_y_continuous(labels=comma)


### I.5 GERD by sector
# GRAPH 1: "R&D Performance in the Natural Sciences and Engineering by Sector" (stacked bar graph)  
# GRAPH 2: "R&D Performance in Social Sciences and Humanities by Sector" (stacked bar graph)  
# GRAPH 3: "R&D Funding by Sector in the Natural Sciences and Engineering" (stacked bar graph)  
# GRAPH 4: "R&D Funding by Sector in the Social Sciences and Humanities" (stacked bar graph)  
# y axis: 2007 Constant Dollars ($ Millions)  



gerd1<-read.csv("Data/part2/GERD_sector_graph1.csv",header=TRUE, stringsAsFactors=TRUE, check.names=FALSE)
gerd1<-gather(gerd1, "Funder")
gerd2<-read.csv("Data/part2/GERD_sector_graph2.csv",header=TRUE, stringsAsFactors=TRUE, check.names=FALSE)
gerd2<-gather(gerd2, "Funder")
gerd3<-read.csv("Data/part2/GERD_sector_graph3.csv",header=TRUE, stringsAsFactors=TRUE, check.names=FALSE)
gerd3<-gather(gerd3, "Funder")
gerd4<-read.csv("Data/part2/GERD_sector_graph4.csv",header=TRUE, stringsAsFactors=TRUE, check.names=FALSE)
gerd4<-gather(gerd4, "Funder")


ggplot(gerd1, aes(variable, value, fill=Funder)) + geom_bar(stat="identity") +
labs (y= "2007 Constant dollars ($ Millions)", fill="", x="", title="R&D Performance in the Natural Sciences and Engineering by Sector") + guides(fill=guide_legend(nrow=2,byrow=TRUE)) + theme(legend.position="bottom") +
scale_fill_manual( values=cols, drop=FALSE) + scale_y_continuous(labels=comma)

ggplot(gerd2, aes(variable, value, fill=Funder)) + geom_bar(stat="identity") +
labs (y= "2007 Constant dollars ($ Millions)", fill="", x="", title="R&D Performance in Social Sciences and Humanities by Sector") + guides(fill=guide_legend(nrow=2,byrow=TRUE)) + theme(legend.position="bottom") +
scale_fill_manual( values=cols, drop=FALSE) + scale_y_continuous(labels=comma)

ggplot(gerd3, aes(variable, value, fill=Funder)) + geom_bar(stat="identity") +
labs (y= "2007 Constant dollars ($ Millions)", fill="", x="", title="R&D Funding by Sector in the Natural Sciences and Engineering") + guides(fill=guide_legend(nrow=2,byrow=TRUE)) + theme(legend.position="bottom") +
scale_fill_manual( values=cols, drop=FALSE) + scale_y_continuous(labels=comma)

ggplot(gerd4, aes(variable, value, fill=Funder)) + geom_bar(stat="identity") +
labs (y= "2007 Constant dollars ($ Millions)", fill="", x="", title="R&D Funding by Sector in the Social Sciences and Humanities") + guides(fill=guide_legend(nrow=2,byrow=TRUE)) + theme(legend.position="bottom") +
scale_fill_manual( values=cols, drop=FALSE) + scale_y_continuous(labels=comma)


## I.6 Fed R&D personnel

# GRAPH 1: "Federal Scientific and Professional Personnel in R&D" (stacked bar graph) 
# GRAPH 2: "Federal Scientific and Professional Personnel in S&T" (stacked bar graph) 
# y axis: number of personnel 


fed_pers1<-read.csv("Data/part2/RD_personnel_graph1.csv",header=TRUE, stringsAsFactors=TRUE, check.names=FALSE)
fed_pers1[,"Personnel category"]<-NULL
colnames(fed_pers1)[1]<-"Type"
fed_pers1<-gather(fed_pers1, "Type")

fed_pers2<-read.csv("Data/part2/RD_personnel_graph2.csv",header=TRUE, stringsAsFactors=TRUE, check.names=FALSE)
fed_pers2[,"Personnel category"]<-NULL
colnames(fed_pers2)[1]<-"Type"
fed_pers2<-gather(fed_pers2, "Type")

ggplot(fed_pers1, aes(variable, value, fill=Type)) + geom_bar(stat="identity") +
labs (y= "Number of Personnel", fill="", x="", title="Federal Scientific and Professional Personnel in R&D") + guides(fill=guide_legend(nrow=1,byrow=TRUE)) + theme(legend.position="bottom") +
scale_fill_manual( values=cols[-c(1:3)]) + scale_y_continuous(labels=comma)


ggplot(fed_pers2, aes(variable, value, fill=Type)) + geom_bar(stat="identity") +
labs (y= "Number of Personnel", fill="", x="", title="Federal Scientific and Professional Personnel in S&T") + guides(fill=guide_legend(nrow=1,byrow=TRUE)) + theme(legend.position="bottom") +
scale_fill_manual( values=cols[-c(1:3)]) + scale_y_continuous(labels=comma)


## I.7 Personnel by sector

# GRAPH 1: "Researchers in Natural Sciences and Engineering (by Sector)" (stacked bar graph)        
# GRAPH 2: "Researchers in Social Sciences and Humanities (by Sector)" (Stacked bar graph)        
# Y-axis: Number of Personnel       

pers1<-read.csv("Data/part2/personnel_graph1.csv",header=TRUE, stringsAsFactors=TRUE, check.names=FALSE)
pers1[,"Occupational category"]<-NULL
pers1[,"Type of science"]<-NULL
colnames(pers1)[1]<-"Sector"
pers1<-gather(pers1, "Sector")


pers2<-read.csv("Data/part2/personnel_graph2.csv",header=TRUE, stringsAsFactors=TRUE, check.names=FALSE)
pers2[,"Occupational category"]<-NULL
pers2[,"Type of science"]<-NULL
colnames(pers2)[1]<-"Sector"
pers2<-gather(pers2, "Sector")

ggplot(pers1, aes(variable, value, fill=Sector)) + geom_bar(stat="identity") +
labs (y= "Number of Personnel", fill="", x="", title="Researchers in Natural Sciences and Engineering (by Sector)") + guides(fill=guide_legend(nrow=1,byrow=TRUE)) + theme(legend.position="bottom") +
scale_fill_manual( values=cols[-c(1:2)]) + scale_y_continuous(labels=comma)


ggplot(pers2, aes(variable, value, fill=Sector)) + geom_bar(stat="identity") +
labs (y= "Number of Personnel", fill="", x="", title="Researchers in Social Sciences and Humanities (by Sector)") + guides(fill=guide_legend(nrow=1,byrow=TRUE)) + theme(legend.position="bottom") +
scale_fill_manual( values=cols[-c(1:2)], drop=FALSE)+ scale_y_continuous(labels=comma)


## I.8 Expenditure on science

# GRAPH 1: "Expenditure on Science and Technology (by sector)" - stacked bar?
# y axis: 2007 Constant Dollars ($ Millions)
sci<-read.csv("Data/part2/exp_science_graph1.csv", header=TRUE, stringsAsFactors=TRUE, check.names=FALSE)
sci<-gather(sci, "Sector")
tail(sci)

ggplot(sci, aes(variable, value, fill=Sector)) + geom_bar(stat="identity") + 
labs (y= "2007 Constant Dollars ($ Millions)", fill="", x="", title="Expenditure on Science and Technology (by sector)") + guides(fill=guide_legend(nrow=3,byrow=TRUE)) + theme(legend.position="bottom") +
scale_fill_manual( values=cols)+ scale_y_continuous(labels=comma)

dev.off()