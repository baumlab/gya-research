
### Script for making first figures for Canada GYA surveys
## Read in and explore the GYA data

setwd("/Users/jpwrobinson/Documents/git_repos/gya-research")

setwd("/Users/kristinatietjen/Documents/git_hub/gya-research")


survey<-read.csv(file="data/gya-without-incomplete.csv")
survey.what<-read.csv(file="data/gya-country-responses.csv")
research<-read.csv(file="data/gya-surveys-cleaned-research.csv")
#research.past<-read.csv(file="data/gya-surveys-cleaned-research-past.csv")
research.change<-read.csv(file="data/gya-change-reason.csv")
part4<-read.csv(file="data/gya-survey-part4.csv")
colnames(p3_master)
part2.b.a<-read.csv(file="data/gya-part2.before.after.csv")
part2.change<-read.csv(file="data/gya-part2.change.csv")
part2.reason<-read.csv(file="data/gya-part2.reason.csv")
part2.view<-read.csv(file="data/gya-part2.view.csv")
part1.view<-read.csv(file="data/gya-part1.view.csv")
part3.grants.long<-read.csv(file="data/gya-part3.grants.long.csv")
part3.change<-read.csv(file="data/gya-part3.change.csv")
part3.success.long<-read.csv(file="data/gya-part3.success.long.csv")
part3.prac.long<-read.csv(file="data/gya-part3.prac.long.csv")
part3.part.long<-read.csv(file="data/gya-part3.part.long.csv")
p3_master.long<-read.csv(file="data/gya-p3_master.long.csv")
p3_master<-read.csv(file="data/gya-p3_master.csv")

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
gap.barplot(sort(locations.top$gender,decreasing=TRUE), gap=c(266,1200),horiz=F, xaxlab=barlabs, xlab="", 
            ylab="Number of responses", ytics=c(0,50,100,150,200,250, 265, 1265,1300),las=2, col=c(blue2red(29)))


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
canada<-survey.what[survey.what$Country_work=="Canada" | (!(survey.what$Country_work=="Canada") & 
                                                            survey.what$Country_work=="" & survey.what$Country=="Canada"),]

field<-aggregate(Location ~ field_research, canada, length)


ggplot(field, aes(x = reorder(field_research, -Location), Location, fill=field_research)) + geom_bar(stat='identity',position = position_dodge(width=0.5)) + 
  theme(axis.text.x=element_blank()) + labs(y="Number of responses",x="")  + scale_fill_discrete(name="Field of Research") +
  geom_text(aes(label=Location), vjust=-0.25) + theme(legend.position=c(0.85, 0.75)) + theme(legend.title=element_text(size=12), 
                                                                                            legend.text=element_text(size=10), axis.text=element_text(size=14), 
                                                                                            axis.title=element_text(size=16))+ guides(fill=guide_legend(reverse=TRUE)) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())


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
canada<-survey.what[survey.what$Country_work=="Canada" | (!(survey.what$Country_work=="Canada") & 
                                                            survey.what$Country_work=="" & survey.what$Country=="Canada"),]


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
canada<-research[research$Country_work=="Canada" | (!(research$Country_work=="Canada") & 
                                                      research$Country_work=="" & research$Country=="Canada"),]
summary(canada)

ggplot(canada, aes(type, percent, fill=type)) + geom_boxplot()

##### now for past#####


## 5a Change in type of research in the last 10yrs
research.change<-read.csv(file="data/gya-change-reason.csv")
yes.no<-subset(research.change, select=c("Location","Country", "Country_work",  "gender", "changed_10yrs"))

## using table to count cases of each category
sum.yesno<-data.frame(table(yes.no$changed_10yrs))
# remove non-response
sum.yesno<-sum.yesno[!sum.yesno$Var1=="",]


ggplot(data=sum.yesno, aes(x=Var1, y=Freq, fill=Var1)) + geom_bar(stat='identity') + guides(fill=FALSE)

# 5b for Canada
canada<-yes.no[yes.no$Country_work=="Canada" | (!(yes.no$Country_work=="Canada") & 
                                                  yes.no$Country_work=="" & yes.no$Country=="Canada"),]
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

research.past<-read.csv(file="data/gya-surveys-cleaned-research.csv")
research.wo.past.total<-subset(research.past, type%in%c("percent_Applied_Research_past",  "percent_Fundamental_Research_past" , "percent_Use_inspired_Research_past"))

## 6a type of research in the past  box

ggplot(research.wo.past.total, aes(type, percent, fill=type)) + geom_boxplot() + theme(axis.text.x = element_blank()) +
  theme(axis.title.x = element_blank())


## 6b Type of research in the past  bar

type.avg.past<-aggregate(percent~ type + Country, research.wo.past.total, mean)


ggplot(type.avg.past, aes(Country, percent)) + geom_bar(stat="identity", aes(fill=factor(type))) + theme(axis.text.x = element_text(angle=90, vjust=0.5))


## 6c  Canada
canada<-research.past[research.past$Country_work=="Canada" | (!(research.past$Country_work=="Canada") & 
                                                                research.past$Country_work=="" & research.past$Country=="Canada"),]

research.wo.past.total.ca<-subset(research.past, type%in%c("percent_Applied_Research_past",  "percent_Fundamental_Research_past" , "percent_Use_inspired_Research_past"))


ggplot(research.wo.past.total.ca, aes(type, percent, fill=type)) + geom_boxplot() + theme(axis.text.x = element_blank()) +
  theme(axis.title.x = element_blank())




## 7a  Reason for change
research.change<-read.csv(file="data/gya-change-reason.csv")

reason<-subset(research.change, select=c("Location","Country","Country_work",  "gender", "Main_reason_change_interest_related", 
                                       "Main_reason_change_Career_related", "Main_reason_change_Funding_related","Main_reason_change_Socially_related",
                                       "Main_reason_change_Other"))
head(reason)
## switch to long format
require(tidyr)
reason.long<-gather(reason, reason.change, yes, -Location, -gender, -Country_work, -Country)
head(reason.long)
sum.reason<-data.frame(table(reason.long$reason.change, reason.long$yes))
head(sum.reason)

ggplot(data=sum.reason, aes(x=Var1, y=Freq, fill=Var1)) + geom_bar(stat='identity')+ 
  theme(axis.text.x = element_text(angle=90, vjust=0.5)) +guides(fill=FALSE)

## 7b canada
research.change<-read.csv(file="data/gya-change-reason.csv")

reason<-subset(research.change, select=c("Location","Country","Country_work",  "gender", "Main_reason_change_interest_related", 
                                         "Main_reason_change_Career_related", "Main_reason_change_Funding_related","Main_reason_change_Socially_related",
                                         "Main_reason_change_Other"))

canada<-reason[reason$Country_work=="Canada" | (!(reason$Country_work=="Canada") & 
                                                  reason$Country_work=="" & reason$Country=="Canada"),]

head(canada)
## switch to long format
require(tidyr)
canada.long<-gather(canada, reason.change.ca, yes.ca, -Location, -gender, -Country_work, -Country)

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
canada<-part1.view[part1.view$Country_work=="Canada" | (!(part1.view$Country_work=="Canada") & 
                                                          part1.view$Country_work=="" & part1.view$Country=="Canada"),]

canada<-canada[!canada$view_change_of_type=="",]
canada<-droplevels(canada)
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


colnames(part4)

## 8a  fundamental research important to your gov in your country
important<-subset(part4, select=c("Location","Country","Country_work", "gender","opinion_fundamental_important"))
## using table to count cases of each category
sum.important<-data.frame(table(important$opinion_fundamental_important))
# remove non-response
sum.important<-sum.important[!sum.important$Var1=="",]

ggplot(data=sum.important, aes(x=Var1, y=Freq, fill=Var1)) + geom_bar(stat='identity')+ 
  theme(axis.text.x = element_text(angle=90, vjust=0.5))



## 8b  Canada
important<-subset(part4, select=c("Location","Country","Country_work", "gender","opinion_fundamental_important"))

canada<-important[important$Country_work=="Canada" | (!(important$Country_work=="Canada") & 
                                                        important$Country_work=="" & important$Country=="Canada"),]
## using table to count cases of each category
sum.important<-data.frame(table(canada$opinion_fundamental_important))
# remove non-response
sum.important<-sum.important[!sum.important$Var1=="",]
sum.important<-sum.important[!sum.important$Var2=="",]

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
priority.ca<-subset(part4, select=c("Location","Country", "Country_work", "gender", "high_priority_fundamental", "high_priority_use_inspired", "high_priority_applied", 
                                 "high_priority_no_change"))

canada<-priority.ca[priority.ca$Country_work=="Canada" | (!(priority.ca$Country_work=="Canada") & 
                                                            priority.ca$Country_work=="" & priority.ca$Country=="Canada"),]

## switch to long format
require(tidyr)
priority.long<-gather(canada, what.type, higher.priority, -Location, -gender, -Country, -Country_work)
tail(priority.long)

high.priority<-aggregate(higher.priority~ what.type, priority.long, sum)
high.priority
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

availiability.change<-subset(part4, select=c("Location", "Country", "Country_work", "gender", "available_funding_fundamental",  
                                             "available_funding_use_inspired", "available_funding_applied"))
canada<-availiability.change[availiability.change$Country_work=="Canada" | (!(availiability.change$Country_work=="Canada") & 
                                                                              availiability.change$Country_work=="" & availiability.change$Country=="Canada"),]


## switch to long format
require(tidyr)
availiability.change.long.ca<-gather(canada, what.type, level, -Location, -gender, -Country_work, -Country)
availiability.ca<-aggregate(gender~what.type+level, 
                         availiability.change.long.ca,length)

# remove non-response
availiability.ca<-availiability.ca[!availiability.ca$level=="",]

availiability.ca

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
next.generation<-subset(part4, select=c("Location", "Country", "Country_work", "gender","next_generation"))
# remove non-response
next.generation<-next.generation[!next.generation$next_generation=="",]
canada<-next.generation[next.generation$Country_work=="Canada" | (!(next.generation$Country_work=="Canada") & 
                                                                    next.generation$Country_work=="" & next.generation$Country=="Canada"),]

head(canada)
impact<-aggregate(gender~ next_generation, canada, length)
impact
head(impact)
ggplot(data=impact, aes(x=next_generation, gender, fill=next_generation)) + geom_bar(stat='identity') +  
  theme(axis.text.x = element_text(angle=90, vjust=0.5))


###############
####Part2######
###############

###12a current partnership
head(a.part)
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
canada<-part2.b.a[part2.b.a$Country_work=="Canada" | (!(part2.b.a$Country_work=="Canada") & 
                                                        part2.b.a$Country_work=="" & part2.b.a$Country=="Canada"),]

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
canada<-part2.change[part2.change$Country_work=="Canada" | (!(part2.change$Country_work=="Canada") & 
                                                              part2.change$Country_work=="" & part2.change$Country=="Canada"),]

change.part.ca<-aggregate(gender~ partnership_change_10yrs, canada, length)

ggplot(data=change.part.ca, aes(x=partnership_change_10yrs, gender, fill=partnership_change_10yrs))+geom_bar(stat='identity')+
  theme(axis.text.x = element_text(angle=0, vjust=0.5))+labs(title="Change in past 10yrs", x="", y="Number of Responses", fill="")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+ guides(fill=FALSE)

###14a Reason for change
head(part2.reason)


require(tidyr)
reason.pt.long<-gather(part2.reason, change.reason, yes, -Country, -gender, -Location, -what_participant_group, -field_research, -Country_work)
unique(reason.pt.long$change.reason)
sum.reason<-data.frame(table(reason.pt.long$change.reason, reason.pt.long$yes))

ggplot(data=sum.reason, aes(x=reorder(Var1, -Freq), y=Freq, fill=Var1)) + geom_bar(stat='identity')+ 
  theme(axis.text.x = element_text(angle=0, vjust=0.5)) + guides(fill=FALSE)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+labs(title= "Reason for Change", x="", y="Number of Responses")+
  scale_x_discrete(labels=c("Funding Related", "Interest Related", "Career Related", "Socially Related", "Other"))

### 14b Canada
canada<-part2.reason[part2.reason$Country_work=="Canada" | (!(part2.reason$Country_work=="Canada") & 
                                                              part2.reason$Country_work=="" & part2.reason$Country=="Canada"),]


require(tidyr)
reason.pt.long<-gather(canada, change.reason, yes, -Country, -gender, -Location,-what_participant_group, -field_research, -Country_work)
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
canada<-part2.view[part2.view$Country_work=="Canada" | (!(part2.view$Country_work=="Canada") & 
                                                          part2.view$Country_work=="" & part2.view$Country=="Canada"),]

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
str(part3.grants.long)
part3.grants.long$type.grant<-as.character(part3.grants.long$type.grant)

#add time variable
part3.grants.long$time<-ifelse(grepl("11_15", part3.grants.long$type.grant), "2011-2015", "2006-2010")
part3.grants.long$type.grant[part3.grants.long$type.grant=="external_pi_grant_11_15_fundamental"]<-"Fundamental"
part3.grants.long$type.grant[part3.grants.long$type.grant=="external_pi_grant_6_10_fundamental"]<-"Fundamental"
part3.grants.long$type.grant[part3.grants.long$type.grant=="external_pi_grant_11_15_use"]<-"Use-Inspired"
part3.grants.long$type.grant[part3.grants.long$type.grant=="external_pi_grant_6_10_use"]<-"Use-Inspired"
part3.grants.long$type.grant[part3.grants.long$type.grant=="external_pi_grant_11_15_applied"]<-"Applied"
part3.grants.long$type.grant[part3.grants.long$type.grant=="external_pi_grant_6_10_applied"]<-"Applied"


grants<-aggregate(gender~ type.grant+time+number, part3.grants.long, length)

grants$type.grant<-as.factor(grants$type.grant)

#change order of levels
grants$number<-factor(grants$number, levels(grants$number)[c(1,2,6,7,3,4,5)])
grants$type.grant<-factor(grants$type.grant, levels(grants$type.grant)[c(2,3,1)])



ggplot(grants, aes(type.grant, gender, fill=number)) + geom_bar(stat="identity", position = "dodge")+facet_wrap(~time) +guides(fill=guide_legend(title=NULL, reverse=FALSE)) +
  labs(x="Number of grant applications", y="Number of responses")+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())


## 17b  canada
canada<-part3.grants.long[part3.grants.long$Country_work=="Canada" | (!(part3.grants.long$Country_work=="Canada") & 
                                                                        part3.grants.long$Country_work=="" & part3.grants.long$Country=="Canada"),]

canada$type.grant<-as.character(canada$type.grant)


#add time variable
canada$time<-ifelse(grepl("11_15", canada$type.grant), "2011-2015", "2006-2010")
canada$type.grant[canada$type.grant=="external_pi_grant_11_15_fundamental"]<-"Fundamental"
canada$type.grant[canada$type.grant=="external_pi_grant_6_10_fundamental"]<-"Fundamental"
canada$type.grant[canada$type.grant=="external_pi_grant_11_15_use"]<-"Use-Inspired"
canada$type.grant[canada$type.grant=="external_pi_grant_6_10_use"]<-"Use-Inspired"
canada$type.grant[canada$type.grant=="external_pi_grant_11_15_applied"]<-"Applied"
canada$type.grant[canada$type.grant=="external_pi_grant_6_10_applied"]<-"Applied"

grants.ca<-aggregate(gender~ type.grant+time+number, canada, length)
grants.ca$type.grant<-as.factor(grants.ca$type.grant)
grants.ca$number
str(grants.ca)
#change order of levels
grants.ca$number<-factor(grants.ca$number, levels(grants.ca$number)[c(1,2,6,7,3,4,5)])
grants.ca
grants.ca$type.grant<-factor(grants.ca$type.grant, levels(grants.ca$type.grant)[c(2,3,1)])
grants.ca
ggplot(grants.ca, aes(type.grant, gender, fill=number)) + geom_bar(stat="identity", position = "dodge")+facet_wrap(~time) +guides(fill=guide_legend(title=NULL, reverse=FALSE)) +
  labs(x="Number of grant applications", y="Number of responses")+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())




#### 18a percent of applications successful
part3.success.long$type<-as.character(part3.success.long$type)

#add time variable
part3.success.long$time<-ifelse(grepl("11_15", part3.success.long$type), "2011-2015", "2006-2010")
part3.success.long$type[part3.success.long$type=="successful_grants_11_15_fundamental"]<-"Fundamental"
part3.success.long$type[part3.success.long$type=="successful_grants_6_10_fundamental"]<-"Fundamental"
part3.success.long$type[part3.success.long$type=="successful_grants_11_15_use"]<-"Use-Inspired"
part3.success.long$type[part3.success.long$type=="successful_grants_6_10_use"]<-"Use-Inspired"
part3.success.long$type[part3.success.long$type=="successful_grants_11_15_applied"]<-"Applied"
part3.success.long$type[part3.success.long$type=="successful_grants_6_10_applied"]<-"Applied"

success<-aggregate(gender~type+percent+time, part3.success.long, length)

#change order of types
success$type<-as.factor(success$type)
success$type<-factor(success$type, levels(success$type)[c(2,3,1)])


ggplot(success, aes(percent, gender, fill=time))+geom_bar(stat="identity", position = "dodge")+facet_wrap(~type)+
  guides(fill=guide_legend(title=NULL, reverse=FALSE)) +
  labs(x="Percent of success", y="Number of responses")+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

## 18b canada
canada<-part3.success.long[part3.success.long$Country_work=="Canada" | (!(part3.success.long$Country_work=="Canada") & 
                                                                          part3.success.long$Country_work=="" & part3.success.long$Country=="Canada"),]

canada$type<-as.character(canada$type)

#add time variable
canada$time<-ifelse(grepl("11_15", canada$type), "2011-2015", "2006-2010")
canada$type[canada$type=="successful_grants_11_15_fundamental"]<-"Fundamental"
canada$type[canada$type=="successful_grants_6_10_fundamental"]<-"Fundamental"
canada$type[canada$type=="successful_grants_11_15_use"]<-"Use-Inspired"
canada$type[canada$type=="successful_grants_6_10_use"]<-"Use-Inspired"
canada$type[canada$type=="successful_grants_11_15_applied"]<-"Applied"
canada$type[canada$type=="successful_grants_6_10_applied"]<-"Applied"

success.ca<-aggregate(gender~type+percent+time, canada, length)

#change order of types
success.ca$type<-as.factor(success.ca$type)
success.ca$type<-factor(success.ca$type, levels(success.ca$type)[c(2,3,1)])

ggplot(success.ca, aes(percent, gender, fill=time))+geom_bar(stat="identity", position = "dodge")+facet_wrap(~type)+
  guides(fill=guide_legend(title=NULL, reverse=FALSE)) +
  labs(x="Percent of success", y="Number of responses")+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())


#### 19a suggesting practical applications
part3.prac.long$year<-as.character(part3.prac.long$year)

part3.prac.long$year[part3.prac.long$year=="practical_applications_important_11_15"]<-"2011-2015"
part3.prac.long$year[part3.prac.long$year=="practical_applications_important_6_10"]<-"2006-2010"

prac<-aggregate(gender~year+level, part3.prac.long, length)

#change order of levels
prac$level<-factor(prac$level, levels(prac$level)[c(2,6,4,5,3,1)])


ggplot(prac, aes(level, gender, fill=year))+ geom_bar(stat="identity", position = "dodge")+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  guides(fill=guide_legend(title=NULL, reverse=FALSE)) + labs(x="", y="Number of responses")+theme(axis.text.x = element_text(angle=90, vjust=0.5)) 


## 19b Canada
canada<-part3.prac.long[part3.prac.long$Country_work=="Canada" | (!(part3.prac.long$Country_work=="Canada") & 
                                                                    part3.prac.long$Country_work=="" & part3.prac.long$Country=="Canada"),]


canada$year<-as.character(canada$year)

canada$year[canada$year=="practical_applications_important_11_15"]<-"2011-2015"
canada$year[canada$year=="practical_applications_important_6_10"]<-"2006-2010"

prac.ca<-aggregate(gender~year+level, canada, length)

#change order of levels
prac.ca$level<-factor(prac.ca$level, levels(prac.ca$level)[c(2,6,4,5,3,1)])
prac.ca

ggplot(prac.ca, aes(level, gender, fill=year))+ geom_bar(stat="identity", position = "dodge")+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  guides(fill=guide_legend(title=NULL, reverse=FALSE)) + labs(x="", y="Number of responses")+theme(axis.text.x = element_text(angle=90, vjust=0.5)) 


#####20a include partners from for profit or non gov sectors
part3.part.long$year<-as.character(part3.part.long$year)

part3.part.long$year[part3.part.long$year=="include_nonacademia_partners_success_11_15"]<-"2011-2015"
part3.part.long$year[part3.part.long$year=="include_nonacademia_partners_success_6_10"]<-"2006-2010"
head(part3.part.long)
partner<-aggregate(gender~year+level, part3.part.long, length)

#change order of levels
partner$level<-factor(partner$level, levels(partner$level)[c(2,5,6,4,3,1)])


ggplot(partner, aes(level, gender, fill=year))+ geom_bar(stat="identity", position = "dodge")+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  guides(fill=guide_legend(title=NULL, reverse=FALSE)) + labs(x="", y="Number of responses")+theme(axis.text.x = element_text(angle=90, vjust=0.5)) 

## 20b Canada
canada<-part3.part.long[part3.part.long$Country_work=="Canada" | (!(part3.part.long$Country_work=="Canada") & 
                                                                    part3.part.long$Country_work=="" & part3.part.long$Country=="Canada"),]


canada$year<-as.character(canada$year)

canada$year[canada$year=="include_nonacademia_partners_success_11_15"]<-"2011-2015"
canada$year[canada$year=="include_nonacademia_partners_success_6_10"]<-"2006-2010"

part.ca<-aggregate(gender~year+level, canada, length)

#change order of levels
part.ca$level<-factor(part.ca$level, levels(part.ca$level)[c(2,5,6,4,3,1)])

part.ca
ggplot(part.ca, aes(level, gender, fill=year))+ geom_bar(stat="identity", position = "dodge")+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  guides(fill=guide_legend(title=NULL, reverse=FALSE)) + labs(x="", y="Number of responses")+theme(axis.text.x = element_text(angle=90, vjust=0.5)) 


#### 21a distribution of funding

head(p3_master.long)

ggplot(p3_master.long, aes(percent, fill=year))+ geom_histogram(position='dodge', binwidth=25)  +  facet_wrap(~type)



## 21b

canada<-p3_master.long[p3_master.long$Country_work=="Canada" | (!(p3_master.long$Country_work=="Canada") & 
                                                                  p3_master.long$Country_work=="" & p3_master.long$Country=="Canada"),]

dim(canada)

canada[354,]
ggplot(canada, aes(percent, fill=year))+ geom_histogram(position='dodge', binwidth=25)  +  facet_wrap(~type)

funding.dist.ca.table<-data.frame(table(canada$percent, canada$type, canada$year))
funding.dist.ca.table


## grant success rates change in last 10 yrs
head(part3.change)
require(tidyr)
##switch to long format
change.long<-gather(part3.change, type, level, -Location, -gender, -Country, -what_participant_group, -field_research, -Country_work)
head(change.long)

change<-aggregate(gender~type+level, change.long, length)

# remove non-response
change<-change[!change$level=="",]

ggplot(data=change, aes(x=level, gender, fill=type))+ geom_bar(stat='identity', position = "dodge") +  
  theme(axis.text.x = element_text(angle=90, vjust=0.5))


## Canada  by gender
canada<-part3.change[part3.change$Country_work=="Canada" | (!(part3.change$Country_work=="Canada") & 
                                                              part3.change$Country_work=="" & part3.change$Country=="Canada"),]

require(tidyr)
##switch to long format
change.long.ca<-gather(canada, type, level, -Location, -gender, -Country, -what_participant_group, -field_research, -Country_work)
head(change.long.ca)

gender.change<-aggregate(Location~type+gender+level, change.long.ca, length)
head(gender.change)
#remove no response
gender.change<-gender.change[!gender.change$gender=="",]
gender.change<-gender.change[!gender.change$level=="",]

# turn to percents
male<-subset(gender.change, gender=="Male")
female<-subset(gender.change, gender=="Female")
other<-subset(gender.change, gender=="Other")
gender.change$Location<-ifelse(gender.change$gender=="Male", (gender.change$Location/sum(male$Location))*100, gender.change$Location)
gender.change$Location<-ifelse(gender.change$gender=="Female", (gender.change$Location/sum(female$Location))*100, gender.change$Location)
gender.change$Location<-ifelse(gender.change$gender=="Other", (gender.change$Location/sum(other$Location))*100, gender.change$Location)

str(gender.change)

# change type to factor with levels
require(stringr)
gender.change$type<-as.factor(gender.change$type)
gender.change$level<-str_replace_all(gender.change$level, "Will", "")
gender.change$level<-as.factor(gender.change$level)


#change order of levels
gender.change$level<-factor(gender.change$level, levels(gender.change$level)[c(1,3,2,6,4,5)])
gender.change$type<-factor(gender.change$type, levels(gender.change$type)[c(2,3,1)])

ggplot(data=gender.change, aes(x=level, Location, fill=type))+geom_bar(stat="identity", position="dodge")+ scale_x_discrete(limits = rev(levels(gender.change$level)))+
  facet_wrap(~gender)+theme(axis.text.x = element_text(angle=90, vjust=0.5))+labs(x="", y="Percentage of responses", fill="")+
  scale_fill_discrete(labels=c("Fundamental", "Use-inspired", "Applied")) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

## grant success rates change in last 10 yrs canada - creating a table 
head(part3.change)
require(tidyr)
##switch to long format
change.long<-gather(part3.change, type, level, -Location, -gender, -Country, -what_participant_group, -field_research, -Country_work)
head(change.long)
canada<-change.long[change.long$Country_work=="Canada" | (!(change.long$Country_work=="Canada") & 
                                                            change.long$Country_work=="" & change.long$Country=="Canada"),]
head(canada)

# remove non-response
canada<-canada[!canada$level=="",]
success.rate.ca.table<-data.frame(table(canada$type, canada$level))
success.rate.ca.table





dev.off()


