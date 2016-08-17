
## Script to examine gender differences in survey questions


## CHANGE WORKING DIRECTORY FOR YOUR LOCAL MACHINE BEFORE KNITTING ##
setwd("/Users/kristinatietjen/Documents/git_hub/gya-research")
#setwd("/Users/IMAC3/Documents/git-jpwrobinson/gya-research")
#setwd("/Users/jpwrobinson/Documents/git_repos/gya-research")
#setwd("/Users/Julia_2013MacBookAir/Desktop/GitRepos/gya-research")

## load in survey data
survey<-read.csv(file="data/gya-without-incomplete.csv")
survey.what<-read.csv(file="data/gya-country-responses.csv")
research<-read.csv(file="data/gya-surveys-cleaned-research.csv")
research.change<-read.csv(file="data/gya-change-reason.csv")
part4<-read.csv(file="data/gya-survey-part4.csv")
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

## load required packages
require(gridExtra); require(tidyr); require(ggplot2); require(stringr);require(RColorBrewer); require(colorRamps); require(plotrix); require(plyr); require(visreg)

theme_set(theme_bw())

#--------------------#--------------------#--------------------
#### Part1. Question 2. Proportions of type of research change
#--------------------#--------------------#--------------------

change<-subset(research.change, select=c("Location","Country",'Country_work', "gender", "changed_10yrs"))
canada<-change[change$Country_work=="Canada" | (!(change$Country_work=="Canada") & 
                                                  change$Country_work=="" & change$Country=="Canada"),]

canada<-canada[!canada$gender=="Other",]
canada<-canada[!canada$gender=="",]
canada<-canada[!canada$changed_10yrs=="",]
canada<-droplevels(canada)

## using table to count cases of each category
sum.change<-data.frame(table(canada$changed_10yrs, canada$gender))
sum.change


change.mod1<-(glm(Freq ~ Var1*Var2, sum.change, family="poisson"))
change.mod2<-(glm(Freq ~ Var1 +Var2, sum.change, family="poisson"))
visreg(change.mod1, "Var2",by="Var1", scale="response", ylab="Number of responses", xlab="Gender")
summary(change.mod1)
head(change.mod2)
anova(change.mod1, change.mod2, test="Chi")

#--------------------#--------------------#--------------------
#### Part1. Question 1 & 3. Proportions of type of research current and past
#--------------------#--------------------#--------------------



#--------------------#--------------------#--------------------
#### Part1. Question 4. Reason for change
#--------------------#--------------------#--------------------



#--------------------#--------------------#--------------------
#### Part1. Question 5. View of change
#--------------------#--------------------#--------------------



#--------------------#--------------------#--------------------
#### Part2. Question 1 & 3. Level of partnership outside academia -current and before
#--------------------#--------------------#--------------------



#--------------------#--------------------#--------------------
#### Part2. Question 2. Level of partnership outside academia -change
#--------------------#--------------------#--------------------


#--------------------#--------------------#--------------------
#### Part2. Question 4. Reason for change
#--------------------#--------------------#--------------------



#--------------------#--------------------#--------------------
#### Part2. Question 5. View of change
#--------------------#--------------------#--------------------




#--------------------#--------------------#--------------------
#### Part3. Question 1. Grant apps types 2006-2010, 2011-2015
#--------------------#--------------------#--------------------



#--------------------#--------------------#--------------------
#### Part3. Question 2. Successful grant apps types 2006-2010, 2011-2015
#--------------------#--------------------#--------------------



#--------------------#--------------------#--------------------
#### Part3. Question 3. Importance of suggesting practical applications 2006-2010, 2011-2015
#--------------------#--------------------#--------------------






#--------------------#--------------------#--------------------
#### Part3. Question 4. Importance of having partners 2006-2010, 2011-2015
#--------------------#--------------------#--------------------





#--------------------#--------------------#--------------------
#### Part3. Question 5. Distribution of funding 2006-2010, 2011-2015
#--------------------#--------------------#--------------------





#--------------------#--------------------#--------------------
#### Part3. Question 6. Success rate change for types 2006-2010, 2011-2015
#--------------------#--------------------#--------------------





#--------------------#--------------------#--------------------
#### Part4. Question 1. Research priority - fundamental
#--------------------#--------------------#--------------------

## order bars from very important (left) to can't comment (right)
important<-subset(part4, select=c("Location","Country",'Country_work', "gender","opinion_fundamental_important"))

canada<-important[important$Country_work=="Canada" | (!(important$Country_work=="Canada") & important$Country_work=="" & important$Country=="Canada"),]
canada<-canada[!canada$opinion_fundamental_important=="",]
canada<-canada[!canada$gender=="Other",]
canada$opinion_fundamental_important<-factor(canada$opinion_fundamental_important, levels(canada$opinion_fundamental_important)[c(6,5,4,3,2,1)])

canada<-droplevels(canada)
head(canada)
## using table to count cases of each category
sum.important<-data.frame(table(canada$opinion_fundamental_important, canada$gender))
# remove non-responses
#sum.important<-sum.important[!sum.important$Var2=="",]
#sum.important<-sum.important[!sum.important$Var1=="",]
sum.important<-sum.important[!sum.important$Var2=="Other",]
sum.important<-sum.important[!sum.important$Var2=="",]
head(sum.important)

#perceive<-aggregate(Freq~Var1, sum.important, sum)

temp<-with(canada, table(opinion_fundamental_important, gender))
chisq.test(temp)

priority.mod1<-(glm(Freq ~ Var1*Var2, sum.important, family="poisson"))
priority.mod2<-(glm(Freq ~ Var1 +Var2, sum.important, family="poisson"))
visreg(priority.mod1, "Var2",by="Var1", scale="response", ylab="Number of responses", xlab="Gender")
summary(priority.mod1)
head(priority.mod2)
anova(priority.mod1, priority.mod2, test="Chi")
#--------------------#--------------------#--------------------
#### Part4. Question 2. Change in research priority 
#--------------------#--------------------#--------------------

## order bars into applied > use > fundamental > no change
priority<-subset(part4, select=c("Location","Country",'Country_work', "gender", "high_priority_fundamental", "high_priority_use_inspired", "high_priority_applied", 
                                 "high_priority_no_change"))

canada<-priority[priority$Country_work=="Canada" | (!(priority$Country_work=="Canada") & priority$Country_work=="" & priority$Country=="Canada"),]


## switch to long format

priority.long<-gather(canada, what.type, higher.priority, -Location, -gender, -Country, -Country_work)
# remove non-responses (n = 1066)
#dim(priority.long[!priority.long$higher.priority=="",])/4

priority.long<-priority.long[!is.na(priority.long$higher.priority),]

#per.change<-aggregate(higher.priority~what.type +gender, priority.long, sum)

gender.perceive<-data.frame(table( priority.long$gender , priority.long$what.type))

# turn to percents
male<-subset(gender.perceive, Var1=="Male")
female<-subset(gender.perceive, Var1=="Female")
other<-subset(gender.perceive, Var1=="Other")
gender.perceive$Freq<-ifelse(gender.perceive$Var1=='Male',(gender.perceive$Freq/sum(male$Freq))*100, gender.perceive$Freq)
gender.perceive$Freq<-ifelse(gender.perceive$Var1=='Female',(gender.perceive$Freq/sum(female$Freq))*100, gender.perceive$Freq)
gender.perceive$Freq<-ifelse(gender.perceive$Var1=='Other',(gender.perceive$Freq/sum(other$Freq))*100, gender.perceive$Freq)

# change order of the levels
gender.perceive<-gender.perceive[!(gender.perceive$Var1=="Other" | gender.perceive$Var1==""),]
#gender.perceive$what.type<-factor(gender.perceive$what.type, levels(gender.perceive$what.type)[c(2,4,1,3)])

gender.perceive$Var2<-revalue(gender.perceive$Var2, c("high_priority_fundamental"="Fundamental",
                                                                'high_priority_use_inspired'='Use-inspired', 'high_priority_applied' = 'Applied',
                                                          'high_priority_no_change'="No change"))

priority.mod<-(glm(Freq ~ Var1*Var2, gender.perceive, family="poisson"))
visreg(priority.mod, "Var2",by="Var1", scale="response", ylab="No. of responses (scaled by gender)", xlab="Gender")

priority.mod1<-(glm(Freq ~ Var1*Var2, gender.perceive, family="poisson"))
priority.mod2<-(glm(Freq ~ Var1 +Var2, gender.perceive, family="poisson"))
visreg(priority.mod1, "Var2",by="Var1", scale="response", ylab="Number of responses", xlab="Gender")
summary(priority.mod1)

anova(priority.mod1, priority.mod2, test="Chi")



#--------------------#--------------------#--------------------
#### Part4. Question 3. Change in research funding 
#--------------------#--------------------#--------------------

availability.change<-subset(part4, select=c("Location", "Country",'Country_work', "gender", "available_funding_fundamental",  
                                            "available_funding_use_inspired", "available_funding_applied"))

canada<-availability.change[availability.change$Country_work=="Canada" | (!(availability.change$Country_work=="Canada") & availability.change$Country_work=="" & availability.change$Country=="Canada"),]

## switch to long format

availability.change.long.ca<-gather(canada, what.type, level, -gender, -Location, -Country, -Country_work)
availability.ca<-with(availability.change.long.ca, data.frame(table(what.type, level)))

#remove non response
availability.ca<-availability.ca[!availability.ca$level=="",]

#gender.perceive<-aggregate(Location ~ what.type + gender+level, availability.change.long.ca, length)

#availability.ca<-aggregate(gender~what.type+level, availability.change.long.ca,length)
# remove non-response
#gender.perceive<-gender.perceive[!gender.perceive$level=="",]
#gender.perceive<-gender.perceive[!gender.perceive$gender=="",]

# turn to percents
#male<-subset(gender.perceive, gender=="Male")
#female<-subset(gender.perceive, gender=="Female")
#other<-subset(gender.perceive, gender=="Other")
#gender.perceive$Location<-ifelse(gender.perceive$gender=='Male',(gender.perceive$Location/sum(male$Location))*100, gender.perceive$Location)
#gender.perceive$Location<-ifelse(gender.perceive$gender=='Female',(gender.perceive$Location/sum(female$Location))*100, gender.perceive$Location)
#gender.perceive$Location<-ifelse(gender.perceive$gender=='Other',(gender.perceive$Location/sum(other$Location))*100, gender.perceive$Location)

## change type to factor with levels
availability.ca$level<-str_replace_all(availability.ca$level, "Will ", "")
availability.ca$what.type<-as.factor(availability.ca$what.type)
availability.ca$level<-as.factor(availability.ca$level)

## change order of levels
availability.ca$level<-factor(availability.ca$level, levels(availability.ca$level)[c(4,5,6,3,2,1 )])
availability.ca$what.type<-factor(availability.ca$what.type, levels(availability.ca$what.type)[c(2,3,1)])

availability.ca$what.type<-revalue(availability.ca$what.type, c("available_funding_fundamental"="Fundamental",
                                                                'available_funding_use_inspired'='Use-inspired', 'available_funding_applied' = 'Applied'))

#--------------------#--------------------#--------------------
#### Part4. Question 4. Research - next generation
#--------------------#--------------------#--------------------

## order bars into increase considerably (left) to decrease considerably (right), then no comment at the far right
next.generation<-subset(part4, select=c("Location", "Country","Country_work", "gender","next_generation"))
# remove non-response
next.generation<-next.generation[!next.generation$next_generation=="",]
canada<-next.generation[next.generation$Country_work=="Canada" | (!(next.generation$Country_work=="Canada") & next.generation$Country_work=="" & next.generation$Country=="Canada"),]


impact<-data.frame(table(canada$next_generation))

## change type to factor with levels
impact$Var1<-str_replace_all(impact$Var1, "Will ", "")
impact$Var1<-as.factor(impact$Var1)
impact<-impact[!impact$Var1=="",]
## change order of Var1s
impact$Var1<-factor(impact$Var1, levels(impact$Var1)[c(4,5,6,3,2,1)])
impact<-droplevels(impact)
