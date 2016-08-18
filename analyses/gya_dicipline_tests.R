
## Script to examine gender differences in survey questions

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
head(research.change)
change<-subset(research.change, select=c("Location","Country",'Country_work', "field_research", "changed_10yrs"))
canada<-change[change$Country_work=="Canada" | (!(change$Country_work=="Canada") & 
                                                  change$Country_work=="" & change$Country=="Canada"),]
canada$what_participant_group<-revalue(canada$what_participant_group, c("Senior academic researcher with >10 years of experience applying for research grants"="Senior academic >10 yrs",
                                                                        'Non-academic researcher conducting or managing research in industry or government with >10 years of experience'='Non-academic >10yrs', 
                                                                        'Early career academic researcher with <10 years experience applying for research grants since completion of PhD' = 'Early academic <10yrs',
                                                                        'Postdoctoral fellow or research assistant with experience applying for research grants, or anticipating the need to apply for grants in the near future'="Post doc",
                                                                        'Non-academic researcher conducting or managing research in industry or government with <10 years of experience'='Non-academic <10yrs'))

canada<-canada[!canada$field_research=="",]
canada<-canada[!canada$changed_10yrs=="",]
canada<-droplevels(canada)
head(canada)
## using table to count cases of each category
sum.change<-data.frame(table(canada$changed_10yrs, canada$field_research))
sum.change

change.mod1<-(glm(Freq ~ Var1*Var2, sum.change, family="poisson"))
change.mod2<-(glm(Freq ~ Var1 +Var2, sum.change, family="poisson"))
visreg(change.mod1, "Var2",by="Var1", scale="response", ylab="Number of responses", xlab="Field of research")
summary(change.mod1)
head(change.mod2)
anova(change.mod1, change.mod2, test="Chi")

#*******************************************************************
#*******************Significant P = 0.005031************************
#*******************************************************************

#--------------------#--------------------#--------------------
#### Part1. Question 1 & 3. Proportions of type of research current and past
#--------------------#--------------------#--------------------


#@@@@@@@@@@@@@@@@@@@@@@@@@@Not Done @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

head(canada)
research<-subset(research, select=c("Location","Country",'Country_work', "field_research", "type", "percent"))
canada<-research[research$Country_work=="Canada" | (!(research$Country_work=="Canada") & 
                                                      research$Country_work=="" & research$Country=="Canada"),]

canada<-canada[!canada$field_research=="",]
canada<-droplevels(canada)

#canada<-spread(canada, type, percent)

## using table to count cases of each category
sum.change<-data.frame(table(canada$percent, canada$type))
sum.change


#--------------------#--------------------#--------------------
#### Part1. Question 4. Reason for change
#--------------------#--------------------#--------------------
head(canada)
reason<-subset(research.change, select=c("Location","Country",'Country_work', "field_research", "Main_reason_change_interest_related", "Main_reason_change_Career_related", 
                                         "Main_reason_change_Funding_related", "Main_reason_change_Socially_related", "Main_reason_change_Other"))
canada<-reason[reason$Country_work=="Canada" | (!(reason$Country_work=="Canada") & 
                                                  reason$Country_work=="" & reason$Country=="Canada"),]

canada<-canada[!canada$field_research=="",]
canada<-canada[!(is.na(canada$Main_reason_change_interest_related)& is.na(canada$Main_reason_change_Career_related) & is.na(canada$Main_reason_change_Funding_related) &
                   is.na(canada$Main_reason_change_Socially_related) & is.na(canada$Main_reason_change_Other)),]
canada<-droplevels(canada)

require(tidyr)
canada.long<-gather(canada, reason, value, -Location, -field_research, -Country_work, -Country)
canada.long
canada.long<-canada.long[!(is.na(canada.long$value)),]

## using table to count cases of each category
sum.reason<-data.frame(table(canada.long$reason, canada.long$value, canada.long$field_research))
sum.reason

reason.mod1<-(glm(Freq ~ Var1*Var3, sum.reason, family="poisson"))
reason.mod2<-(glm(Freq ~ Var1 +Var3, sum.reason, family="poisson"))
visreg(reason.mod1, "Var3",by="Var1", scale="response", ylab="Number of responses", xlab="field_research")
summary(reason.mod1)
head(reason.mod1)
anova(reason.mod1, reason.mod2, test="Chi")

#@@@@@@@@@@@@@@@@@@@@@@@@Need to check @@@@@@@@@@@@@@@@@@@@@@@@@@@@@

#*******************************************************************
#*******************Not Significant P = 0.**********************
#*******************************************************************

#--------------------#--------------------#--------------------
#### Part1. Question 5. View of change
#--------------------#--------------------#--------------------

head(canada)
view<-subset(part1.view, select=c("Location","Country",'Country_work', "field_research", "view_change_of_type"))

canada<-view[view$Country_work=="Canada" | (!(view$Country_work=="Canada") & 
                                              view$Country_work=="" & view$Country=="Canada"),]

canada<-canada[!canada$field_research=="",]
canada<-canada[!canada$view_change_of_type=="",]
canada<-droplevels(canada)

## using table to count cases of each category
sum.view<-data.frame(table(canada$view_change_of_type, canada$field_research))
sum.view

view.mod1<-(glm(Freq ~ Var1*Var2, sum.view, family="poisson"))
view.mod2<-(glm(Freq ~ Var1 +Var2, sum.view, family="poisson"))
visreg(view.mod1, "Var2",by="Var1", scale="response", ylab="Number of responses", xlab="field_research")
anova(view.mod1, view.mod2, test="Chi")

#*******************************************************************
#*******************Not Significant P = 0.**********************
#*******************************************************************

#--------------------#--------------------#--------------------
#### Part2. Question 1 & 3. Level of partnership outside academia -current and before
#--------------------#--------------------#--------------------
head(part2.b.a)


#--------------------#--------------------#--------------------
#### Part2. Question 2. Level of partnership outside academia -change
#--------------------#--------------------#--------------------
head(canada)
p.change<-subset(part2.change, select=c("Location","Country",'Country_work', "field_research", "partnership_change_10yrs"))

canada<-p.change[p.change$Country_work=="Canada" | (!(p.change$Country_work=="Canada") & 
                                                      p.change$Country_work=="" & p.change$Country=="Canada"),]

canada<-canada[!canada$field_research=="",]
canada<-canada[!canada$partnership_change_10yrs=="",]
canada<-droplevels(canada)

## using table to count cases of each category
sum.p.change<-data.frame(table(canada$partnership_change_10yrs, canada$field_research))
sum.p.change

p.change.mod1<-(glm(Freq ~ Var1*Var2, sum.p.change, family="poisson"))
p.change.mod2<-(glm(Freq ~ Var1 +Var2, sum.p.change, family="poisson"))
visreg(p.change.mod1, "Var2",by="Var1", scale="response", ylab="Number of responses", xlab="field_research")
anova(p.change.mod1, p.change.mod2, test="Chi")

#*******************************************************************
#*******************Significant P = 0.***********************
#*******************************************************************

#--------------------#--------------------#--------------------
#### Part2. Question 4. Reason for change
#--------------------#--------------------#--------------------
head(canada)
p.reason<-subset(part2.reason, select=c("Location","Country",'Country_work', "field_research", "reason_partnership_change_interest", "reason_partnership_change_career",
                                        "reason_partnership_change_socially", "reason_partnership_change_funding", "reason_partnership_change_other"))

canada<-p.reason[p.reason$Country_work=="Canada" | (!(p.reason$Country_work=="Canada") & 
                                                      p.reason$Country_work=="" & p.reason$Country=="Canada"),]

canada<-canada[!canada$field_research=="",]
canada<-canada[!(is.na(canada$reason_partnership_change_interest)& is.na(canada$reason_partnership_change_career) & is.na(canada$reason_partnership_change_socially) &
                   is.na(canada$reason_partnership_change_funding) & is.na(canada$reason_partnership_change_other)),]
canada<-droplevels(canada)

require(tidyr)
canada.long<-gather(canada, reason, value, -Location, -field_research, -Country_work, -Country)
canada.long
canada.long<-canada.long[!(is.na(canada.long$value)),]

## using table to count cases of each category
sum.p.reason<-data.frame(table(canada.long$reason, canada.long$value, canada.long$field_research))
sum.p.reason

p.reason.mod1<-(glm(Freq ~ Var1*Var3, sum.p.reason, family="poisson"))
p.reason.mod2<-(glm(Freq ~ Var1 +Var3, sum.p.reason, family="poisson"))
visreg(p.reason.mod1, "Var3",by="Var1", scale="response", ylab="Number of responses", xlab="field_research")
anova(p.reason.mod1, p.reason.mod2, test="Chi")

#@@@@@@@@@@@@@@@@@@@@@@@@Need to check @@@@@@@@@@@@@@@@@@@@@@@@@@@@@


#*******************************************************************
#*******************Not Significant P = 0.**********************
#*******************************************************************

#--------------------#--------------------#--------------------
#### Part2. Question 5. View of change
#--------------------#--------------------#--------------------
head(canada)
p.view<-subset(part2.view, select=c("Location","Country",'Country_work', "field_research", "view_change_partnership"))

canada<-p.view[p.view$Country_work=="Canada" | (!(p.view$Country_work=="Canada") & 
                                                  p.view$Country_work=="" & p.view$Country=="Canada"),]
canada<-canada[!canada$field_research=="",]
canada<-canada[!canada$view_change_partnership=="",]
canada<-droplevels(canada)

## using table to count cases of each category
sum.p.view<-data.frame(table(canada$view_change_partnership, canada$field_research))
sum.p.view

p.view.mod1<-(glm(Freq ~ Var1*Var2, sum.p.view, family="poisson"))
p.view.mod2<-(glm(Freq ~ Var1 +Var2, sum.p.view, family="poisson"))
visreg(p.view.mod1, "Var2",by="Var1", scale="response", ylab="Number of responses", xlab="field_research")
anova(p.view.mod1, p.view.mod2, test="Chi")

#*******************************************************************
#*******************Not Significant P = 0.**********************
#*******************************************************************


#--------------------#--------------------#--------------------
#### Part3. Question 1. Grant apps types 2006-2010, 2011-2015
#--------------------#--------------------#--------------------
head(part3.grants.long)


#--------------------#--------------------#--------------------
#### Part3. Question 2. Successful grant apps types 2006-2010, 2011-2015
#--------------------#--------------------#--------------------
head(part3.success.long)


#--------------------#--------------------#--------------------
#### Part3. Question 3. Importance of suggesting practical applications 2006-2010, 2011-2015
#--------------------#--------------------#--------------------
head(part3.prac.long)





#--------------------#--------------------#--------------------
#### Part3. Question 4. Importance of having partners 2006-2010, 2011-2015
#--------------------#--------------------#--------------------
head(part3.part.long)




#--------------------#--------------------#--------------------
#### Part3. Question 5. Distribution of funding 2006-2010, 2011-2015
#--------------------#--------------------#--------------------
head(p3_master)
head(p3_master.long)




#--------------------#--------------------#--------------------
#### Part3. Question 6. Success rate change for types 2006-2010, 2011-2015
#--------------------#--------------------#--------------------
head(part3.change)




#--------------------#--------------------#--------------------
#### Part4. Question 1. Research priority - fundamental
#--------------------#--------------------#--------------------

important<-subset(part4, select=c("Location","Country",'Country_work', "field_research","opinion_fundamental_important"))

canada<-important[important$Country_work=="Canada" | (!(important$Country_work=="Canada") & important$Country_work=="" & important$Country=="Canada"),]
canada<-canada[!canada$opinion_fundamental_important=="",]
canada<-canada[!canada$field_research=="",]
canada$opinion_fundamental_important<-factor(canada$opinion_fundamental_important, levels(canada$opinion_fundamental_important)[c(6,5,4,3,2,1)])

canada<-droplevels(canada)
head(canada)
## using table to count cases of each category
sum.important<-data.frame(table(canada$opinion_fundamental_important, canada$field_research))
# remove non-responses
#sum.important<-sum.important[!sum.important$Var2=="",]
#sum.important<-sum.important[!sum.important$Var1=="",]
sum.important<-sum.important[!sum.important$Var2=="",]
head(sum.important)

#temp<-with(canada, table(opinion_fundamental_important, gender))
#chisq.test(temp)

priority.mod1<-(glm(Freq ~ Var1*Var2, sum.important, family="poisson"))
priority.mod2<-(glm(Freq ~ Var1 +Var2, sum.important, family="poisson"))
visreg(priority.mod1, "Var2",by="Var1", scale="response", ylab="Number of responses", xlab="field_research")
summary(priority.mod1)
head(priority.mod2)
anova(priority.mod1, priority.mod2, test="Chi")

#*******************************************************************
#*******************Not Significant P = 0.**********************
#*******************************************************************

#--------------------#--------------------#--------------------
#### Part4. Question 2. Change in research priority 
#--------------------#--------------------#--------------------

priority<-subset(part4, select=c("Location","Country",'Country_work', "field_research", "high_priority_fundamental", "high_priority_use_inspired", "high_priority_applied", 
                                 "high_priority_no_change"))

canada<-priority[priority$Country_work=="Canada" | (!(priority$Country_work=="Canada") & priority$Country_work=="" & priority$Country=="Canada"),]

head(gender.perceive)
## switch to long format

priority.long<-gather(canada, what.type, higher.priority, -Location, -field_research, -Country, -Country_work)
# remove non-responses (n = 1066)

priority.long<-priority.long[!is.na(priority.long$higher.priority),]

field.perceive<-data.frame(table( priority.long$field_research , priority.long$what.type))

field.perceive$Var2<-revalue(field.perceive$Var2, c("high_priority_fundamental"="Fundamental",
                                                      'high_priority_use_inspired'='Use-inspired', 'high_priority_applied' = 'Applied',
                                                      'high_priority_no_change'="No change"))

#priority.mod<-(glm(Freq ~ Var1*Var2, gender.perceive, family="poisson"))
#visreg(priority.mod, "Var2",by="Var1", scale="response", ylab="No. of responses (scaled by gender)", xlab="Gender")

priority.mod1<-(glm(Freq ~ Var1*Var2, field.perceive, family="poisson"))
priority.mod2<-(glm(Freq ~ Var1 +Var2, field.perceive, family="poisson"))
visreg(priority.mod1, "Var2",by="Var1", scale="response", ylab="Number of responses", xlab="field_research")
summary(priority.mod1)

anova(priority.mod1, priority.mod2, test="Chi")

#*******************************************************************
#*******************Not Significant P = 0.**********************
#*******************************************************************

#--------------------#--------------------#--------------------
#### Part4. Question 3. Change in research funding 
#--------------------#--------------------#--------------------

availability.change<-subset(part4, select=c("Location", "Country",'Country_work', "field_research", "available_funding_fundamental",  
                                            "available_funding_use_inspired", "available_funding_applied"))

canada<-availability.change[availability.change$Country_work=="Canada" | (!(availability.change$Country_work=="Canada") & availability.change$Country_work=="" & availability.change$Country=="Canada"),]
head(availability.ca)
## switch to long format

availability.change.long.ca<-gather(canada, what.type, level, -field_research, -Location, -Country, -Country_work)
availability.ca<-with(availability.change.long.ca, data.frame(table(what.type, field_research, level)))
?with
#remove non response
availability.ca<-availability.ca[!availability.ca$level=="",]

availability.ca$what.type<-revalue(availability.ca$what.type, c("available_funding_fundamental"="Fundamental",
                                                                'available_funding_use_inspired'='Use-inspired', 'available_funding_applied' = 'Applied'))

f.change.mod1<-(glm(Freq ~ what.type*level*gender, availability.ca, family="poisson"))
f.change.mod2<-(glm(Freq ~ what.type +level+gender, availability.ca, family="poisson"))
visreg(f.change.mod1, "field_research",by="what.type", scale="response", ylab="Number of responses", xlab="field_research")


anova(f.change.mod1, f.change.mod2, test="Chi")

#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#@@@@@@@@@@@@@@@@@@@@@@@@@@Not Done - dont know if it is right@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

#--------------------#--------------------#--------------------
#### Part4. Question 4. Research - next generation
#--------------------#--------------------#--------------------

next.generation<-subset(part4, select=c("Location", "Country","Country_work", "field_research","next_generation"))
# remove non-response
next.generation<-next.generation[!next.generation$next_generation=="",]
canada<-next.generation[next.generation$Country_work=="Canada" | (!(next.generation$Country_work=="Canada") & next.generation$Country_work=="" & next.generation$Country=="Canada"),]

canada<-canada[!canada$field_research=="",]
canada<-droplevels(canada)
head(canada)
## using table to count cases of each category
impact<-data.frame(table(canada$next_generation, canada$field_research))
impact

impact.mod1<-(glm(Freq ~ Var1*Var2, impact, family="poisson"))
impact.mod2<-(glm(Freq ~ Var1 +Var2, impact, family="poisson"))
visreg(impact.mod1, "Var2",by="Var1", scale="response", ylab="Number of responses", xlab="field_research")
anova(impact.mod1, impact.mod2, test="Chi")

#*******************************************************************
#*******************Not Significant P = 0.**********************
#*******************************************************************

