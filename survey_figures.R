
### Script for making first figures for Canada GYA surveys
## Read in and explore the GYA data

setwd("/Users/jpwrobinson/Documents/git_repos/gya-research")

setwd("/Users/kristinatietjen/Documents/git_hub/gya-research")



survey<-read.csv(file="data/gya-country-responses.csv")
research<-read.csv(file="data/gya-surveys-cleaned-research.csv")
research.past<-read.csv(file="data/gya-surveys-cleaned-research-past.csv")
research.change<-read.csv(file="data/gya-change-reason.csv")


################################
#### Summary statistics ########
################################
survey.table<-table(survey$Country,survey$gender)



gender<-aggregate(Location ~ gender, survey, length)
head(gender)

country_work<-aggregate(gender ~ Country_work, survey, length)





require(ggplot2)
## Now plotting results
qplot(x=Country, y=gender, data=locations, geom="point", col=Country )

qplot(x=Country, y=gender, data=locations, geom="boxplot" )


qplot(x=field_research, y=Location, data=survey, geom="point", col=field_research )


ggplot(data=field, aes(x=field_research, y=Location, fill=field_research)) + geom_bar(stat='identity')

## change default background
theme_set(theme_bw())

## plot countries with number of responses
ggplot(country_work, aes(x = reorder(Country_work, -gender), gender)) + geom_bar(stat='identity',position = position_dodge(width=0.5)) + 
  theme(axis.text.x=element_blank()) + labs(y="Number of responses",x="")  + 
  geom_text(aes(label=gender), vjust=-0.25) +
  geom_text(aes(label=Country_work), angle=90, hjust=-0.5) + lims(y=c(0, 1300)) 








## add a line that saves the plots in a pdf
pdf(file="figures/first_survey_responses.pdf", height=7, width=11)

###############
####Part5#####
###############


##1 # of responses by country
locations<-aggregate(gender ~ Country, survey, length)

ggplot(country_work, aes(x = reorder(Country_work, -gender), gender)) + geom_bar(stat='identity',position = position_dodge(width=0.5)) + 
  theme(axis.text.x=element_blank()) + labs(y="Number of responses", x="")  + 
  geom_text(aes(label=gender), vjust=-0.25, hjust=0.25) +
  geom_text(aes(label=Country_work), angle=90, hjust=-0.5) + lims(y=c(0, 1300)) + theme(legend.title=element_text(size=12), 
                                                                                        legend.text=element_text(size=10), axis.text=element_text(size=14), axis.title=element_text(size=16)) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())



##2a # of responses by field research 
field<-aggregate(Location ~ field_research, survey, length)

ggplot(field, aes(x = reorder(field_research, -Location), Location, fill=field_research)) + geom_bar(stat='identity',position = position_dodge(width=0.5)) + 
  theme(axis.text.x=element_blank()) + labs(y="Number of responses",x="")  + scale_fill_discrete(name="Field of Research") + 
  geom_text(aes(label=Location), vjust=-0.25) + theme(legend.position=c(0.78, 0.8)) + theme(legend.title=element_text(size=12), legend.text=element_text(size=10), axis.text=element_text(size=14), 
                                                                                            axis.title=element_text(size=14)) + guides(fill=guide_legend(reverse=TRUE)) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())



##2b # of responses by field research for canada
canada<-subset(survey, Country=="Canada")

field<-aggregate(Location ~ field_research, canada, length)


ggplot(field, aes(x = reorder(field_research, -Location), Location, fill=field_research)) + geom_bar(stat='identity',position = position_dodge(width=0.5)) + 
  theme(axis.text.x=element_blank()) + labs(y="Number of responses",x="")  + scale_fill_discrete(name="Field of Research") +
  geom_text(aes(label=Location), vjust=-0.25) + theme(legend.position=c(0.78, 0.8)) + theme(legend.title=element_text(size=12), 
                                                                                            legend.text=element_text(size=10), axis.text=element_text(size=14), axis.title=element_text(size=16))+ 
  guides(fill=guide_legend(reverse=TRUE)) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())


##############
####Part1#####
##############

##3a # of responses by participant group 
experience<-aggregate(Location ~ what_participant_group, survey, length)

ggplot(experience, aes(x = reorder(what_participant_group, -Location), Location, fill=what_participant_group)) + geom_bar(stat='identity',position = position_dodge(width=0.5)) +
  labs(y="Number of responses", x="") +  theme(legend.position="bottom") + theme(axis.text.x=element_blank()) + 
  geom_text(aes(label=Location),size=4, vjust=-0.25) + guides(fill=guide_legend(title=NULL, reverse=TRUE, nrow=6)) + scale_fill_discrete(name="Participant Group") + 
  theme(legend.title=element_text(size=12), legend.text=element_text(size=10), axis.text=element_text(size=14), 
        axis.title=element_text(size=14))+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) 


##3b # of responses by participant group for canada
canada<-subset(survey, Country=="Canada")

experience<-aggregate(Location ~ what_participant_group, canada, length)

ggplot(experience, aes(x = reorder(what_participant_group, -Location), Location, fill=what_participant_group)) + geom_bar(stat='identity',position = position_dodge(width=0.5)) +
  labs(y="Number of responses", x="") +  theme(legend.position="bottom") + theme(axis.text.x=element_blank()) + 
  geom_text(aes(label=Location),size=4, vjust=-0.25)+ guides(fill=guide_legend(title=NULL, reverse=TRUE, nrow=6)) + scale_fill_discrete(name="Participant Group") + 
  theme(legend.title=element_text(size=12), legend.text=element_text(size=10), axis.text=element_text(size=14), axis.title=element_text(size=16))+ 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())


research<-read.csv(file="data/gya-surveys-cleaned-research.csv")

research.wo.total<-subset(research, type%in%c("percent_fundemental_research_current",  "percent_Applied_Research_current" , "percent_Use_inspired_Research_current"))

##4a box plot of percent of each type of research by field

ggplot(research.wo.total, aes(type, percent, fill=type)) + geom_boxplot() + theme(axis.text.x = element_blank()) +
  theme(axis.title.x = element_blank())


#4b   average bar plots
type.avg<-aggregate(percent~ type + Country, research.wo.total, mean)


ggplot(type.avg, aes(Country, percent)) + geom_bar(stat="identity", aes(fill=factor(type))) + theme(axis.text.x = element_text(angle=90, vjust=0.5))

##4c canada
canada<-subset(research.wo.total, Country=="Canada")
summary(canada)

ggplot(research.wo.total, aes(type, percent, fill=type)) + geom_boxplot()

##### now for past#####


## 5 Change in type of research in the last 10yrs
research.change<-read.csv(file="data/gya-change-reason.csv")
yes.no<-subset(research.change, select=c("Location","Country",  "gender", "changed_10yrs"))

## using table to count cases of each category
sum.yesno<-data.frame(table(yes.no$changed_10yrs))
# remove non-response
sum.yesno<-sum.yesno[!sum.yesno$Var1=="",]

# for countries
data.frame(table(yes.no$changed_10yrs, yes.no$Country))





ggplot(data=sum.yesno, aes(x=Var1, y=Freq, fill=Var1)) + geom_bar(stat='identity') + guides(fill=FALSE)

#####

research.past<-read.csv(file="data/gya-surveys-cleaned-research-past.csv")
research.wo.past.total<-subset(research.past, type_past%in%c("percent_Applied_Research_past",  "percent_Fundamental_Research_past" , "percent_Use_inspired_Research_past"))

## 6a type of research in the past  box

ggplot(research.wo.past.total, aes(type_past, percent_past, fill=type_past)) + geom_boxplot() + theme(axis.text.x = element_blank()) +
  theme(axis.title.x = element_blank())


## 6b Type of research in the past  bar

type.avg.past<-aggregate(percent_past~ type_past + Country, research.wo.past.total, mean)


ggplot(type.avg.past, aes(Country, percent_past)) + geom_bar(stat="identity", aes(fill=factor(type_past))) + theme(axis.text.x = element_text(angle=90, vjust=0.5))


## 6c  Canada
canada<-subset(research.wo.past.total, Country=="Canada")
summary(canada)

ggplot(research.wo.past.total, aes(type_past, percent_past, fill=type_past)) + geom_boxplot()



## 7a  Reason for change
research.change<-read.csv(file="data/gya-change-reason.csv")

reason<-subset(research.change, select=c("Location","Country",  "gender", "Main_reason_change_interest_related", 
                                       "Main_reason_change_Career_related", "Main_reason_change_Funding_related","Main_reason_change_Socially_related",
                                       "Main_reason_change_Other"))


## switch to long format
require(tidyr)
reason.long<-gather(reason, reason.change, yes, -Location, -gender, -Country)

sum.reason<-data.frame(table(reason.long$reason.change, reason.long$yes))
head(sum.reason)

ggplot(data=sum.reason, aes(x=Var1, y=Freq, fill=Var1)) + geom_bar(stat='identity')+ 
  theme(axis.text.x = element_text(angle=90, vjust=0.5))

## 7b canada
canada<-subset(research.wo.past.total, Country=="Canada")

ggplot(data=reason.long, aes(x=reason.change, y=yes, fill=reason.change)) + geom_bar(stat='identity')+ 
  theme(axis.text.x = element_text(angle=90, vjust=0.5))


###############
####Part4######
###############

## 8  fundamental research important to your gov in your country


## 9  What type of research is higher priority for your gov

###############
####Part2######
###############


###############
####Part3######
###############






dev.off()


