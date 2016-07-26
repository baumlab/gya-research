
### Script for making first figures for Canada GYA surveys
## Read in and explore the GYA data

setwd("/Users/jpwrobinson/Documents/git_repos/gya-research")

setwd("/Users/kristinatietjen/Documents/git_hub/gya-research")


survey<-read.csv(file="data/gya-without-incomplete.csv")
dim(survey)
survey.what<-read.csv(file="data/gya-country-responses.csv")
research<-read.csv(file="data/gya-surveys-cleaned-research.csv")
research.past<-read.csv(file="data/gya-surveys-cleaned-research-past.csv")
research.change<-read.csv(file="data/gya-change-reason.csv")
part4<-read.csv(file="data/gya-survey-part4.csv")
part2.b.a<-read.csv(file="data/gya-part2.before.after.csv")
part2.change<-read.csv(file="data/gya-part2.change.csv")
part2.reason<-read.csv(file="data/gya-part2.reason.csv")
part2.view<-read.csv(file="data/gya-part2.view.csv")
part1.view<-read.csv(file="data/gya-part1.view.csv")
part3.grants.long<-read.csv(file="data/gya-part3.grants.long.csv")
part3.change<-read.csv(file="data/gya-part3.change.csv")

################################
#### Summary statistics ########
################################






require(ggplot2)

## change default background
theme_set(theme_bw())

## aggregate data for plotting
survey.table<-table(survey.what$Country,survey.what$gender)
gender<-aggregate(Location ~ gender, survey.what, length)
country_work<-aggregate(gender ~ Country_work, survey.what, length)
country_work
## plot countries with number of responses
ggplot(country_work, aes(x = reorder(Country_work, -gender), gender)) + geom_bar(stat='identity',position = position_dodge(width=0.5)) + 
  theme(axis.text.x=element_blank()) + labs(y="Number of responses",x="")  + 
  geom_text(aes(label=gender), hjust=-0.25, angle=90,size=3) +
  geom_text(aes(label=Country_work), angle=90, hjust=-1,size=3) + lims(y=c(0, 1450)) + theme(axis.ticks.x=element_blank())




## add a line that saves the plots in a pdf
pdf(file="figures/first_survey_responses.pdf", height=7, width=11)

###############
####Part5#####
###############


##1 # of responses by country
locations<-aggregate(gender ~ Country, survey.what, length)

ggplot(country_work, aes(x = reorder(Country_work, -gender), gender)) + geom_bar(stat='identity',position = position_dodge(width=0.5)) + theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5)) + labs(y="Number of responses", x="")  + 
  geom_text(aes(label=gender), angle=90, hjust=-1, size=3) +
  #geom_text(aes(label=Country_work), angle=90, hjust=-0.5) 
  lims(y=c(0, 1600)) + theme(legend.title=element_text(size=12), 
                             legend.text=element_text(size=10), axis.text=element_text(size=8), axis.title=element_text(size=12)) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

##1b  top 20+
require(plotrix)
country.list<-c("Canada" , "France",
                    "Germany","United Kingdom","Italy","Japan", "Russia","Korea, South",
                      "United States","Australia","Austria","Belgium",
                      "Ireland","Brazil", "Poland","Finland",
                       "Netherlands","New Zealand","Norway",
                      "Spain","Sweden","Switzerland","Turkey","Israel",
                      "India","Bangladesh","Uruguay","Malaysia","New Zealand","Taiwan","Denmark")

country.top<-survey[survey$Country_work %in% country.list,]
head(country.top)
locations.top<-aggregate(gender ~ Country_work, country.top, length)
gender.top<-aggregate(Country_work ~ gender, country.top, length)
locations.top[order(match(locations.top,gender.top))]
locations.top<-droplevels(locations.top)

require(colorRamps)
barlabs<-locations.top$Country_work[order(locations.top$gender, decreasing=TRUE)]

par(mar=c(7,4,4,2))
gap.barplot(sort(locations.top$gender,decreasing=TRUE), gap=c(201,1200),horiz=F, xaxlab=barlabs, xlab="", 
            ylab="Number of responses", ytics=c(0,50,100,150,200, 1242,1300),las=2, col=c(blue2red(29)))


#ggplot(locations.top, aes(x = reorder(Country_work, -gender), gender)) + geom_bar(stat='identity',position = position_dodge(width=0.5)) +
 # theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5)) + labs(y="Number of responses", x="")  + 
  #geom_text(aes(label=gender), angle=90, hjust=-1, size=3) +
  #geom_text(aes(label=Country), angle=90, hjust=-0.5) 
  #lims(y=c(0, 1600)) + theme(legend.title=element_text(size=12), 
   #                          legend.text=element_text(size=10), axis.text=element_text(size=8), axis.title=element_text(size=12)) + 
  #theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

##2a # of responses by field research 
field<-aggregate(Location ~ field_research, survey.what, length)

ggplot(field, aes(x = reorder(field_research, -Location), Location, fill=field_research)) + geom_bar(stat='identity',position = position_dodge(width=0.5)) + 
  theme(axis.text.x=element_blank()) + labs(y="Number of responses",x="")  + scale_fill_discrete(name="Field of Research") + 
  geom_text(aes(label=Location), vjust=-0.25) + theme(legend.position=c(0.78, 0.7)) + theme(legend.title=element_text(size=12), legend.text=element_text(size=10), axis.text=element_text(size=14), 
                                                                                            axis.title=element_text(size=14)) + guides(fill=guide_legend(reverse=TRUE)) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())



##2b # of responses by field research for canada
canada<-subset(survey.what, Country=="Canada")
field<-aggregate(Location ~ field_research, canada, length)


ggplot(field, aes(x = reorder(field_research, -Location), Location, fill=field_research)) + geom_bar(stat='identity',position = position_dodge(width=0.5)) + 
  theme(axis.text.x=element_blank()) + labs(y="Number of responses",x="")  + scale_fill_discrete(name="Field of Research") +
  geom_text(aes(label=Location), vjust=-0.25) + theme(legend.position=c(0.85, 0.75)) + theme(legend.title=element_text(size=12), 
                                                                                            legend.text=element_text(size=10), axis.text=element_text(size=14), axis.title=element_text(size=16))+ 
  guides(fill=guide_legend(reverse=TRUE)) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())


##############
####Part1#####
##############

##3a # of responses by participant group 
experience<-aggregate(Location ~ what_participant_group, survey.what, length)

ggplot(experience, aes(x = reorder(what_participant_group, -Location), Location, fill=what_participant_group)) + geom_bar(stat='identity',position = position_dodge(width=0.5)) +
  labs(y="Number of responses", x="") +  theme(legend.position="bottom") + theme(axis.text.x=element_blank()) + 
  geom_text(aes(label=Location),size=4, vjust=-0.25) + guides(fill=guide_legend(title=NULL, reverse=TRUE, nrow=6)) + scale_fill_discrete(name="Participant Group") + 
  theme(legend.title=element_text(size=12), legend.text=element_text(size=10), axis.text=element_text(size=14), 
        axis.title=element_text(size=14))+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) 


##3b # of responses by participant group for canada
canada<-subset(survey.what, Country=="Canada")

experience<-aggregate(Location ~ what_participant_group, canada, length)

ggplot(experience, aes(x = reorder(what_participant_group, -Location), Location, fill=what_participant_group)) + geom_bar(stat='identity',position = position_dodge(width=0.5)) +
  labs(y="Number of responses", x="") +  theme(legend.position="bottom") + theme(axis.text.x=element_blank()) + 
  geom_text(aes(label=Location),size=4, vjust=-0.25)+ guides(fill=guide_legend(title=NULL, reverse=TRUE, nrow=6)) + scale_fill_discrete(name="Participant Group") + 
  theme(legend.title=element_text(size=12), legend.text=element_text(size=10), axis.text=element_text(size=14), axis.title=element_text(size=16))+ 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())


#################################




research<-read.csv(file="data/gya-surveys-cleaned-research.csv")

research.wo.total<-subset(research, type%in%c("percent_fundemental_research_current",  "percent_Applied_Research_current" , "percent_Use_inspired_Research_current"))

##4a box plot of percent of each type of research by field

ggplot(research.wo.total, aes(type, percent, fill=type)) + geom_boxplot() + theme(axis.text.x = element_blank()) +
  theme(axis.title.x = element_blank())


#4b   average bar plots
type.avg<-aggregate(percent~ type + Country, research.wo.total, mean)


ggplot(type.avg, aes(Country, percent)) + geom_bar(stat="identity", aes(fill=factor(type))) + theme(axis.text.x = element_text(angle=90, vjust=0.5))

##4c canada
canada<-subset(research, Country=="Canada")
summary(canada)

ggplot(research.wo.total, aes(type, percent, fill=type)) + geom_boxplot()

##### now for past#####


## 5a Change in type of research in the last 10yrs
research.change<-read.csv(file="data/gya-change-reason.csv")
yes.no<-subset(research.change, select=c("Location","Country",  "gender", "changed_10yrs"))

## using table to count cases of each category
sum.yesno<-data.frame(table(yes.no$changed_10yrs))
# remove non-response
sum.yesno<-sum.yesno[!sum.yesno$Var1=="",]


ggplot(data=sum.yesno, aes(x=Var1, y=Freq, fill=Var1)) + geom_bar(stat='identity') + guides(fill=FALSE)

# 5b for Canada
canada<-subset(yes.no, Country=="Canada")
canada.yesno<-aggregate(Country~ changed_10yrs, canada, length)
canada.yesno<-canada.yesno[!canada.yesno$changed_10yrs=="",]

ggplot(data=canada.yesno, aes(x=changed_10yrs, y=Country, fill=Country))+ geom_bar(stat='identity') + guides(fill=FALSE)

# 5c  for countries
country.sum.yesno<-data.frame(table(yes.no$changed_10yrs, yes.no$Country))
# remove non-response
country.sum.yesno<-country.sum.yesno[!country.sum.yesno$Var1=="",]

head(country.sum.yesno)
ggplot(data=country.sum.yesno, aes(x=Var1, y=Freq, fill=Var2)) + geom_bar(stat='identity') +  theme(axis.text.x = element_text(angle=90, vjust=0.5))

ggplot(data=country.sum.yesno, aes(x=Var2, y=Freq, fill=Var1)) + geom_bar(stat='identity') +  theme(axis.text.x = element_text(angle=90, vjust=0.5))




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
canada<-subset(research.change, Country=="Canada")
summary(canada)

ggplot(research.wo.past.total, aes(type_past, percent_past, fill=type_past)) + geom_boxplot()



## 7a  Reason for change
research.change<-read.csv(file="data/gya-change-reason.csv")

reason<-subset(research.change, select=c("Location","Country",  "gender", "Main_reason_change_interest_related", 
                                       "Main_reason_change_Career_related", "Main_reason_change_Funding_related","Main_reason_change_Socially_related",
                                       "Main_reason_change_Other"))
head(reason)
## switch to long format
require(tidyr)
reason.long<-gather(reason, reason.change, yes, -Location, -gender, -Country)
head(reason.long)
sum.reason<-data.frame(table(reason.long$reason.change, reason.long$yes))
head(sum.reason)

ggplot(data=sum.reason, aes(x=Var1, y=Freq, fill=Var1)) + geom_bar(stat='identity')+ 
  theme(axis.text.x = element_text(angle=90, vjust=0.5)) +guides(fill=FALSE)

## 7b canada
research.change<-read.csv(file="data/gya-change-reason.csv")

reason<-subset(research.change, select=c("Location","Country",  "gender", "Main_reason_change_interest_related", 
                                         "Main_reason_change_Career_related", "Main_reason_change_Funding_related","Main_reason_change_Socially_related",
                                         "Main_reason_change_Other"))

canada<-subset(reason, Country=="Canada")

## switch to long format
require(tidyr)
canada.long<-gather(canada, reason.change.ca, yes.ca, -Location, -gender, -Country)

## using table to count cases of each category
sum.canada<-data.frame(table(canada.long$reason.change.ca, canada.long$yes.ca))

# remove non-response
sum.canada<-sum.canada[!sum.canada$Var1=="",]

ggplot(sum.canada, aes(x=Var1, y=Freq, fill=Var1)) + geom_bar(stat='identity')+ 
  theme(axis.text.x = element_text(angle=90, vjust=0.5)) +guides(fill=FALSE)


## 16a  View change
# remomve non-response
part1.view<-part1.view[!part1.view$view_change_of_type=="",]
part1.view<-droplevels(part1.view)

change.view<-aggregate(gender~ view_change_of_type, part1.view, length)

# change order of the levels
change.view$view_change_of_type<-factor(change.view$view_change_of_type, 
                                        levels(change.view$view_change_of_type)[c(5,3,1,2,4)])

ggplot(change.view, aes(x=view_change_of_type, y=gender, fill=view_change_of_type)) + geom_bar(stat='identity')+
  theme(axis.text.x = element_text(angle=90, vjust=0.5)) +guides(fill=FALSE)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+ labs(x="", y="Number of Responses")

# 16b Canada
canada<-subset(part1.view, Country=="Canada")
canada<-canada[!canada$view_change_of_type=="",]

change.view.ca<-aggregate(gender~ view_change_of_type, canada, length)

# change order of the levels
change.view.ca$view_change_of_type<-factor(change.view.ca$view_change_of_type, 
                                        levels(change.view.ca$view_change_of_type)[c(5,3,1,2,4)])

ggplot(change.view.ca, aes(x=view_change_of_type, y=gender, fill=view_change_of_type)) + geom_bar(stat='identity')+
  theme(axis.text.x = element_text(angle=90, vjust=0.5)) +guides(fill=FALSE)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+ labs(x="", y="Number of Responses")



###############
####Part4######
###############

part4<-read.csv(file="data/gya-survey-part4")
colnames(part4)

## 8a  fundamental research important to your gov in your country
important<-subset(part4, select=c("Location","Country", "gender","opinion_fundamental_important"))
## using table to count cases of each category
sum.important<-data.frame(table(important$opinion_fundamental_important))
# remove non-response
sum.important<-sum.important[!sum.important$Var1=="",]

ggplot(data=sum.important, aes(x=Var1, y=Freq, fill=Var1)) + geom_bar(stat='identity')+ 
  theme(axis.text.x = element_text(angle=90, vjust=0.5))



## 8b  Canada
important<-subset(part4, select=c("Location","Country", "gender","opinion_fundamental_important"))

canada<-subset(important, Country=="Canada")
## using table to count cases of each category
sum.important<-data.frame(table(canada$opinion_fundamental_important))
# remove non-response
sum.important<-sum.important[!sum.important$Var1=="",]

ggplot(data=sum.important, aes(x=Var1, y=Freq, fill=Var1)) + geom_bar(stat='identity')+ 
  theme(axis.text.x = element_text(angle=90, vjust=0.5))

##8c  countries
country.sum.important<-data.frame(table(important$opinion_fundamental_important, important$Country))
# remove non-response
country.sum.important<-country.sum.important[!country.sum.important$Var1=="",]

head(country.sum.important)
ggplot(data=country.sum.important, aes(x=Var1, y=Freq, fill=Var2)) + geom_bar(stat='identity') +  theme(axis.text.x = element_text(angle=90, vjust=0.5))

ggplot(data=country.sum.important, aes(x=Var2, y=Freq, fill=Var1)) + geom_bar(stat='identity') +  theme(axis.text.x = element_text(angle=90, vjust=0.5))



## 9a  What type of research is higher priority for your gov
priority<-subset(part4, select=c("Location","Country", "gender", "high_priority_fundamental", "high_priority_use_inspired", "high_priority_applied", 
                                                                 "high_priority_no_change"))

## switch to long format
require(tidyr)
priority.long<-gather(priority, what.type, higher.priority, -Location, -gender, -Country)
head(priority.long)

#high.priority<-aggregate(higher.priority~ what.type, priority.long, length)
high.priority<-aggregate(higher.priority~ what.type, priority.long, sum)

ggplot(data=high.priority, aes(x=what.type, higher.priority, fill=what.type)) + geom_bar(stat='identity') +  
  theme(axis.text.x = element_text(angle=90, vjust=0.5))

## 9c  countries
country.high.priority<-aggregate(higher.priority~ what.type + Country, priority.long, sum)


ggplot(data=country.high.priority, aes(x=Country, y=higher.priority, fill=what.type)) + geom_bar(stat='identity') +  
  theme(axis.text.x = element_text(angle=90, vjust=0.5))


## 9b Canada
priority<-subset(part4, select=c("Location","Country", "gender", "high_priority_fundamental", "high_priority_use_inspired", "high_priority_applied", 
                                 "high_priority_no_change"))
canada<-subset(priority, Country=="Canada")
## switch to long format
require(tidyr)
priority.long<-gather(canada, what.type, higher.priority, -Location, -gender, -Country)
head(priority.long)

high.priority<-aggregate(higher.priority~ what.type, priority.long, sum)

ggplot(data=high.priority, aes(x=what.type, higher.priority, fill=what.type)) + geom_bar(stat='identity') +  
  theme(axis.text.x = element_text(angle=90, vjust=0.5))



######10a  Availiability of research funding will change in your country of work in the next 5 yrs
availiability.change<-subset(part4, select=c("Location", "Country", "gender", "available_funding_fundamental",  
                                      "available_funding_use_inspired", "available_funding_applied"))

## switch to long format
require(tidyr)
availiability.change.long<-gather(availiability.change, what.type, level, -Location, -gender, -Country)
head(availiability.change.long)
availiability<-aggregate(gender~what.type+level, 
                         availiability.change.long,length)

# remove non-response
availiability<-availiability[!availiability$level=="",]

ggplot(data=availiability, aes(x=what.type, gender, fill=level))+ geom_bar(stat='identity') +  
  theme(axis.text.x = element_text(angle=90, vjust=0.5))

##10b countries 
availiability.cont<-aggregate(gender~what.type+level+Country, 
                         availiability.change.long,length)

# remove non-response
availiability.cont<-availiability.cont[!availiability.cont$level=="",]

ggplot(data=availiability.cont, aes(x=what.type, gender, fill=level))+ geom_bar(stat='identity') +  
  theme(axis.text.x = element_text(angle=90, vjust=0.5)) + facet_wrap(~ Country)

###subset out the countries with more than ~5 responses
########################not done###################
##################################################
##################################################
availiability.change<-subset(part4, select=c("Location", "Country", "gender", "available_funding_fundamental",  
                                             "available_funding_use_inspired", "available_funding_applied"))
head(availiability.change)
#five.countries<-subset(availiability.change, Country=="USA","United Kingdom","Canada","Israel","Italy")








###10c-Canada

availiability.change<-subset(part4, select=c("Location", "Country", "gender", "available_funding_fundamental",  
                                             "available_funding_use_inspired", "available_funding_applied"))
canada<-subset(availiability.change, Country=="Canada")

## switch to long format
require(tidyr)
availiability.change.long.ca<-gather(canada, what.type, level, -Location, -gender, -Country)
availiability.ca<-aggregate(gender~what.type+level, 
                         availiability.change.long.ca,length)
head(availiability.ca)
# remove non-response
availiability.ca<-availiability.ca[!availiability.ca$level=="",]

ggplot(data=availiability.ca, aes(x=what.type, gender, fill=level))+ geom_bar(stat='identity') +  
  theme(axis.text.x = element_text(angle=90, vjust=0.5))


##11a changes in funding influence the next generation

next.generation<-subset(part4, select=c("Location", "Country", "gender","next_generation"))
# remove non-response
next.generation<-next.generation[!next.generation$next_generation=="",]

impact<-aggregate(gender~ next_generation, next.generation, length)
head(impact)
ggplot(data=impact, aes(x=next_generation, gender, fill=next_generation)) + geom_bar(stat='identity') +  
  theme(axis.text.x = element_text(angle=90, vjust=0.5))

##11b countries
country.impact<-aggregate(gender~ next_generation + Country, next.generation, length)
# remove non-response
country.impact<-country.impact[!country.impact$next_generation=="",]

ggplot(data=country.impact, aes(x=Country, y=gender, fill=next_generation)) + geom_bar(stat='identity') +  
  theme(axis.text.x = element_text(angle=90, vjust=0.5))

##11c Canada
next.generation<-subset(part4, select=c("Location", "Country", "gender","next_generation"))
# remove non-response
next.generation<-next.generation[!next.generation$next_generation=="",]
canada<-subset(next.generation, Country=="Canada")
head(canada)
impact<-aggregate(gender~ next_generation, canada, length)
head(impact)
ggplot(data=impact, aes(x=next_generation, gender, fill=next_generation)) + geom_bar(stat='identity') +  
  theme(axis.text.x = element_text(angle=90, vjust=0.5))


###############
####Part2######
###############

###12a current partnership

b.part<-aggregate(gender~  partnership_outside_before, part2.b.a, length)
colnames(b.part)[1]<-"partnership_outside"
b.part$time<-"Past"
a.part<-aggregate(gender~ partnership_outside, part2.b.a, length)
a.part$time<-"Current"

part<-rbind(a.part, b.part)

# change order of the levels
part$partnership_outside<-as.factor(part$partnership_outside)
part$partnership_outside<-factor(part$partnership_outside, levels(part$partnership_outside)[c(1,4,3,2)])

ggplot() + 
  geom_bar(data=part, aes(x=partnership_outside, y=gender, fill = time), stat='identity', position="dodge") +
  theme(axis.text.x = element_text(angle=90, vjust=0.5))+labs(x="", y="Number of Responses", fill="")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
  

###12b canada
canada<-subset(part2.current, Country=="Canada")
current.part.ca<-aggregate(gender~ partnership_outside, canada, length)

# change order of the levels
current.part.ca$partnership_outside<-as.factor(current.part.ca$partnership_outside)
current.part.ca$partnership_outside<-factor(current.part.ca$partnership_outside, levels(current.part.ca$partnership_outside)[c(1,4,3,2)])

ggplot(data=current.part.ca, aes(x=partnership_outside, gender, fill=partnership_outside))+geom_bar(stat='identity')+
  theme(axis.text.x = element_text(angle=90, vjust=0.5))+labs(x="", y="Number of Responses", fill="")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+ guides(fill=FALSE)

####13a  change in partnership
head(part2.change)
change.part<-aggregate(gender~ partnership_change_10yrs, part2.change, length)

ggplot(data=change.part, aes(x=partnership_change_10yrs, gender, fill=partnership_change_10yrs))+geom_bar(stat='identity')+
  theme(axis.text.x = element_text(angle=0, vjust=0.5))+labs(x="", y="Number of Responses", fill="")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+ guides(fill=FALSE)

###13b Canada
canada<-subset(part2.change, Country=="Canada")
change.part.ca<-aggregate(gender~ partnership_change_10yrs, canada, length)

ggplot(data=change.part.ca, aes(x=partnership_change_10yrs, gender, fill=partnership_change_10yrs))+geom_bar(stat='identity')+
  theme(axis.text.x = element_text(angle=0, vjust=0.5))+labs(title="Change in past 10yrs", x="", y="Number of Responses", fill="")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+ guides(fill=FALSE)

###14a Reason for change
head(part2.reason)


require(tidyr)
reason.pt.long<-gather(part2.reason, change.reason, yes, -Country, -gender, -Location)
unique(reason.pt.long$change.reason)
sum.reason<-data.frame(table(reason.pt.long$change.reason, reason.pt.long$yes))

ggplot(data=sum.reason, aes(x=reorder(Var1, -Freq), y=Freq, fill=Var1)) + geom_bar(stat='identity')+ 
  theme(axis.text.x = element_text(angle=0, vjust=0.5)) + guides(fill=FALSE)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+labs(title= "Reason for Change", x="", y="Number of Responses")+
  scale_x_discrete(labels=c("Funding Related", "Interest Related", "Career Related", "Socially Related", "Other"))

### 14b Canada
canada<-subset(part2.reason, Country=="Canada")

require(tidyr)
reason.pt.long<-gather(canada, change.reason, yes, -Country, -gender, -Location)
unique(reason.pt.long$change.reason)
sum.reason<-data.frame(table(reason.pt.long$change.reason, reason.pt.long$yes))

ggplot(data=sum.reason, aes(x=reorder(Var1, -Freq), y=Freq, fill=Var1)) + geom_bar(stat='identity')+ 
  theme(axis.text.x = element_text(angle=0, vjust=0.5)) + guides(fill=FALSE)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+labs(title= "Reason for Change", x="", y="Number of Responses")+
  scale_x_discrete(labels=c("Funding Related", "Interest Related", "Career Related", "Socially Related", "Other"))

####15a View of Change
view<-aggregate(gender~ view_change_partnership, part2.view, length)
levels(view$view_change_partnership)
# change order of the levels
view$view_change_partnership<-factor(view$view_change_partnership, levels(view$view_change_partnership)[c(6,4,2,3,5)])

ggplot(data=view, aes(x=view_change_partnership, y=gender, fill=view_change_partnership)) + geom_bar(stat='identity')+ 
  theme(axis.text.x = element_text(angle=0, vjust=0.5)) + guides(fill=FALSE)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+labs(title= "View of Change", x="", y="Number of Responses")
  

###15b Canada
canada<-subset(part2.view, Country=="Canada")
view<-aggregate(gender~ view_change_partnership, canada, length)
levels(view$view_change_partnership)
# change order of the levels
view$view_change_partnership<-factor(view$view_change_partnership, levels(view$view_change_partnership)[c(6,4,2,3,5)])

ggplot(data=view, aes(x=view_change_partnership, y=gender, fill=view_change_partnership)) + geom_bar(stat='identity')+ 
  theme(axis.text.x = element_text(angle=0, vjust=0.5)) + guides(fill=FALSE)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+labs(x="", y="Number of Responses")


###############
####Part3######
###############

## 17a  grant applications  all countries

head(part3.grants.long)





## 17b  canada




## percent of applications successful






## grant success rates change in last 10 yrs
head(part3.change)
require(tidyr)
##switch to long format
change.long<-gather(part3.change, type, level, -Location, -gender, -Country)
head(change.long)

change<-aggregate(gender~type+level, change.long, length)

# remove non-response
change<-change[!change$level=="",]

ggplot(data=change, aes(x=type, gender, fill=level))+ geom_bar(stat='identity') +  
  theme(axis.text.x = element_text(angle=90, vjust=0.5))

###################not done#####################################
####################################
####################################
############################



dev.off()


