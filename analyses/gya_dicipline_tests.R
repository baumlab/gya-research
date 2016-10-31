
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
research.type<-read.csv(file="data/gya-research-cleaned.csv")

## load required packages
require(gridExtra); require(tidyr); require(ggplot2); require(stringr);require(RColorBrewer); require(colorRamps); require(plotrix); require(plyr); require(visreg); require(betareg)

theme_set(theme_bw())

#--------------------#--------------------#--------------------
#### Part1. Question 2. Proportions of type of research change
#--------------------#--------------------#--------------------
head(research.change)
change<-subset(research.change, select=c("Location","Country",'Country_work', "field_research", "changed_10yrs"))
canada<-change[change$Country_work=="Canada" | (!(change$Country_work=="Canada") & 
                                                  change$Country_work=="" & change$Country=="Canada"),]

canada<-canada[!canada$field_research=="",]
canada<-canada[!canada$changed_10yrs=="",]
canada<-droplevels(canada)
head(canada)

canada$field_research<-as.character(canada$field_research)
canada$field_research<-ifelse(canada$field_research%in%c("Other","Interdisciplinary Science","Social Science / Humanities"), 'NonSci', canada$field_research)
canada$field_research<-ifelse(canada$field_research=="Physical Science (eg. math, physics, chemistry, computer science)", "PhysSci", canada$field_research)
canada$field_research<-ifelse(canada$field_research=="Medicine and Life Science", "MedSci", canada$field_research)
canada$field_research<-as.factor(canada$field_research)

canada$changed_10yrs<-factor(canada$changed_10yrs, levels(canada$changed_10yrs)[c(2,3,1)])


canada<-canada[!canada$field_research=="NonSci",]
canada<-droplevels(canada)
## using table to count cases of each category
sum.change<-data.frame(table(canada$changed_10yrs, canada$field_research))
sum.change

change.mod1<-(glm(Freq ~ Var1*Var2, sum.change, family="poisson"))
change.mod2<-(glm(Freq ~ Var1 +Var2, sum.change, family="poisson"))
visreg(change.mod1, "Var2",by="Var1", scale="response", ylab="Number of responses", xlab="Field of research")
summary(change.mod1)
anova(change.mod1, change.mod2, test="Chi")

#*******************************************************************
#*******************Significant P = 0.006599************************
#*******************************************************************
#combine interdisciplinary, other, and social sig p = 0.005773
# phys sci answered no more than expected on a model based off of can't comment responses
#med sci answered no and yes more than expected on a model based off of can't comment responses

#remove non sci sig p=0.002306
#med sci and phys sci did not answer can't comment as much as they should have
#so the other disciplines answered can't comment more often than the others


#--------------------#--------------------#--------------------
#### Part1. Question 1 & 3. Proportions of type of research current and past
#--------------------#--------------------#--------------------

#current
head(research.type)
research<-subset(research.type, select=c("Location","Country",'Country_work', "field_research", "percent_fundemental_research_current", "percent_Use_inspired_Research_current", "percent_Applied_Research_current"))
canada<-research[research$Country_work=="Canada" | (!(research$Country_work=="Canada") & 
                                                      research$Country_work=="" & research$Country=="Canada"),]

canada<-canada[!canada$field_research=="",]
canada<-droplevels(canada)
head(canada)

type.r.long<-gather(canada, type, percent, -Location, -Country, -Country_work, -field_research)
head(type.r.long)

#make decimal 
type.r.long$percent<-type.r.long$percent/100
head(type.r.long, 20)
#transform data
n.percent<-length(type.r.long$percent)
type.r.long$percent_trans<-(type.r.long$percent*(n.percent-1)+0.5)/n.percent

mod1<-betareg(percent_trans ~ field_research * type, type.r.long)
mod2<-betareg(percent_trans ~ field_research + type, type.r.long)
AIC(mod1, mod2)

#*******************************************************************
#************************* Significant  ****************************
#*******************************************************************
# mod1 fits best


#now past
head(research.type)
p.research<-subset(research.type, select=c("Location","Country",'Country_work', "field_research", "percent_Fundamental_Research_past", "percent_Use_inspired_Research_past", 
                                           "percent_Applied_Research_past"))
canada<-p.research[p.research$Country_work=="Canada" | (!(p.research$Country_work=="Canada") & 
                                                          p.research$Country_work=="" & p.research$Country=="Canada"),]
canada<-canada[!canada$field_research=="",]
canada<-droplevels(canada)
head(canada)

#turn into long form
type.rp.long<-gather(canada, type.p, percent, -Location, -Country, -Country_work, -field_research)
head(type.rp.long)

#make decimal 
type.rp.long$percent<-type.rp.long$percent/100
head(type.rp.long, 20)
unique(type.rp.long$percent)
#tranform data
n.percent<-length(type.rp.long$percent)
type.rp.long$percent_trans<-(type.rp.long$percent*(n.percent-1)+0.5)/n.percent


mod1<-betareg(percent_trans ~ field_research * type.p, type.rp.long)
mod2<-betareg(percent_trans ~ field_research + type.p, type.rp.long)
AIC(mod1, mod2)

#*******************************************************************
#******************* Significant *******************************
#*******************************************************************
#Mod1 fits better


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

canada$field_research<-as.character(canada$field_research)
canada$field_research<-ifelse(canada$field_research%in%c("Other","Interdisciplinary Science","Social Science / Humanities"), 'NonSci', canada$field_research)
canada$field_research<-ifelse(canada$field_research=="Physical Science (eg. math, physics, chemistry, computer science)", "PhysSci", canada$field_research)
canada$field_research<-ifelse(canada$field_research=="Medicine and Life Science", "MedSci", canada$field_research)
canada$field_research<-as.factor(canada$field_research)

canada<-canada[!canada$field_research=="NonSci",]
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
anova(reason.mod1, reason.mod2, test="Chi")
#Geoff thinks this is ok even though you are making more indepent answers than original

#*******************************************************************
#*******************Not Significant P = 0.7322**********************
#*******************************************************************
#combine non sci  no sig p =0.598
#remove non sci not sig p =0.7221


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

canada$field_research<-as.character(canada$field_research)
canada$field_research<-ifelse(canada$field_research%in%c("Other","Interdisciplinary Science","Social Science / Humanities"), 'NonSci', canada$field_research)
canada$field_research<-ifelse(canada$field_research=="Physical Science (eg. math, physics, chemistry, computer science)", "PhysSci", canada$field_research)
canada$field_research<-ifelse(canada$field_research=="Medicine and Life Science", "MedSci", canada$field_research)
canada$field_research<-as.factor(canada$field_research)

canada<-canada[!canada$field_research=="NonSci",]
canada<-droplevels(canada)

## using table to count cases of each category
sum.view<-data.frame(table(canada$view_change_of_type, canada$field_research))
sum.view

view.mod1<-(glm(Freq ~ Var1*Var2, sum.view, family="poisson"))
view.mod2<-(glm(Freq ~ Var1 +Var2, sum.view, family="poisson"))
visreg(view.mod1, "Var2",by="Var1", scale="response", ylab="Number of responses", xlab="field_research")
summary(view.mod1)
anova(view.mod1, view.mod2, test="Chi")

#*******************************************************************
#*******************Significant P = 0.0448 ************************
#*******************************************************************
#nat sci and phys sci answered very negative not as expected
#combine non sci   not sig 0.5469
#remove non sci  not sig 0.4401



#--------------------#--------------------#--------------------
#### Part2. Question 1 & 3. Level of partnership outside academia -current and before
#--------------------#--------------------#--------------------
head(part2.b.a)

#before

b4<-subset(part2.b.a, select=c("Location","Country",'Country_work', "field_research","partnership_outside_before"))
canada<-b4[b4$Country_work=="Canada" | (!(b4$Country_work=="Canada") & 
                                          b4$Country_work=="" & b4$Country=="Canada"),]
canada<-canada[!canada$field_research=="",]
canada<-canada[!canada$partnership_outside_before=="",]
canada<-droplevels(canada)
head(canada)

canada$field_research<-as.character(canada$field_research)
canada$field_research<-ifelse(canada$field_research%in%c("Other","Interdisciplinary Science","Social Science / Humanities"), 'NonSci', canada$field_research)
canada$field_research<-ifelse(canada$field_research=="Physical Science (eg. math, physics, chemistry, computer science)", "PhysSci", canada$field_research)
canada$field_research<-ifelse(canada$field_research=="Medicine and Life Science", "MedSci", canada$field_research)
canada$field_research<-as.factor(canada$field_research)

canada<-canada[!canada$field_research=="NonSci",]
canada<-droplevels(canada)

## using table to count cases of each category
sum.b4<-data.frame(table(canada$partnership_outside_before, canada$field_research))
sum.b4

b4.mod1<-(glm(Freq ~ Var1*Var2, sum.b4, family="poisson"))
b4.mod2<-(glm(Freq ~ Var1 +Var2, sum.b4, family="poisson"))
visreg(b4.mod1, "Var2",by="Var1", scale="response", ylab="Number of responses", xlab="field_research")
summary(b4.mod1)
anova(b4.mod1, b4.mod2, test="Chi")

#*******************************************************************
#******************* Not Significant P = 0.1768  *******************
#*******************************************************************
#combine non sci not sig p = 0.08652
#remove non sci  sig 0.04589
#looks like no partnership and some did not follow a pattern


#current

cur<-subset(part2.b.a, select=c("Location","Country",'Country_work', "field_research","partnership_outside"))
canada<-cur[cur$Country_work=="Canada" | (!(cur$Country_work=="Canada") & 
                                            cur$Country_work=="" & cur$Country=="Canada"),]
canada<-canada[!canada$field_research=="",]
canada<-canada[!canada$partnership_outside=="",]
canada<-droplevels(canada)
head(canada)

canada$field_research<-as.character(canada$field_research)
canada$field_research<-ifelse(canada$field_research%in%c("Other","Interdisciplinary Science","Social Science / Humanities"), 'NonSci', canada$field_research)
canada$field_research<-ifelse(canada$field_research=="Physical Science (eg. math, physics, chemistry, computer science)", "PhysSci", canada$field_research)
canada$field_research<-ifelse(canada$field_research=="Medicine and Life Science", "MedSci", canada$field_research)
canada$field_research<-as.factor(canada$field_research)

canada<-canada[!canada$field_research=="NonSci",]
canada<-droplevels(canada)

## using table to count cases of each category
sum.cur<-data.frame(table(canada$partnership_outside, canada$field_research))
sum.cur

cur.mod1<-(glm(Freq ~ Var1*Var2, sum.cur, family="poisson"))
cur.mod2<-(glm(Freq ~ Var1 +Var2, sum.cur, family="poisson"))
visreg(cur.mod1, "Var2",by="Var1", scale="response", ylab="Number of responses", xlab="field_research")
summary(cur.mod1)
anova(cur.mod1, cur.mod2, test="Chi")

#*******************************************************************
#******************* Significant P = 0.0001905 *********************
#*******************************************************************
#combine non sci  significant p = 2.812e-05
#remove non sci sig p = 2.818e-05


#is it ok that I spit up the time periods? - geoff thinks so


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

canada$field_research<-as.character(canada$field_research)
canada$field_research<-ifelse(canada$field_research%in%c("Other","Interdisciplinary Science","Social Science / Humanities"), 'NonSci', canada$field_research)
canada$field_research<-ifelse(canada$field_research=="Physical Science (eg. math, physics, chemistry, computer science)", "PhysSci", canada$field_research)
canada$field_research<-ifelse(canada$field_research=="Medicine and Life Science", "MedSci", canada$field_research)
canada$field_research<-as.factor(canada$field_research)

canada$partnership_change_10yrs<-factor(canada$partnership_change_10yrs, levels(canada$partnership_change_10yrs)[c(2,3,1)])


#canada<-canada[!canada$field_research=="NonSci",]
#canada<-droplevels(canada)

## using table to count cases of each category
sum.p.change<-data.frame(table(canada$partnership_change_10yrs, canada$field_research))
sum.p.change

p.change.mod1<-(glm(Freq ~ Var1*Var2, sum.p.change, family="poisson"))
p.change.mod2<-(glm(Freq ~ Var1 +Var2, sum.p.change, family="poisson"))
visreg(p.change.mod1, "Var2",by="Var1", scale="response", ylab="Number of responses", xlab="field_research")
summary(p.change.mod1)
anova(p.change.mod1, p.change.mod2, test="Chi")

#*******************************************************************
#*******************Significant P = 5.067e-05 **********************
#*******************************************************************
#combine non sci  sig p=0.0004483




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
#Geoff thinks this is ok even though you are making more indepent answers than original


#*******************************************************************
#*******************Not Significant P = 0.3012**********************
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
#*******************Not Significant P = 0.09768*********************
#*******************************************************************


#--------------------#--------------------#--------------------
#### Part3. Question 1. Grant apps types 2006-2010, 2011-2015
#--------------------#--------------------#--------------------
head(part3.grants.long)


#--------------------#--------------------#--------------------
#### Part3. Question 2. Successful grant apps types 2006-2010, 2011-2015            -percents
#--------------------#--------------------#--------------------

#2006-2010
head(part3.success.long)

g.success<-subset(part3.success.long, select=c("Location","Country",'Country_work', "field_research", "type", "percent"))

canada<-g.success[g.success$Country_work=="Canada" | (!(g.success$Country_work=="Canada") & 
                                                        g.success$Country_work=="" & g.success$Country=="Canada"),]
#clean up names
canada$type<-revalue(canada$type, c("successful_grants_11_15_applied"="Applied 2011-2015",
                                    'successful_grants_11_15_fundamental'='Fundamental 2011-2015',
                                    'successful_grants_11_15_use' = 'Use-Inspired 2011-2015',
                                    "successful_grants_6_10_applied"="Applied 2006-2010",
                                    "successful_grants_6_10_fundamental"="Fundamental 2006-2010",
                                    "successful_grants_6_10_use"="Use-Inspired 2006-2010"))

canada<-canada[!canada$field_research=="",]
unique(canada$type)
canada$type<-as.character(canada$type)
canada$year<-  str_split_fixed(canada$type, ' ', 2)[,2]
canada$type.g<-  str_split_fixed(canada$type, ' ', 2)[,1]
canada<-canada[!canada$year=="2011-2015",]
canada<-droplevels(canada)
tail(canada)

#make decimal 
canada$percent<-canada$percent/100
head(canada)
#unique(type.r.long$percent)
n.percent<-length(canada$percent)
canada$percent_trans<-(canada$percent*(n.percent-1)+0.5)/n.percent

mod1<-betareg(percent_trans ~ field_research * type.g, canada)
summary(mod1)
plot(mod1)
mod2<-betareg(percent_trans ~ field_research + type.g, canada)
AIC(mod1, mod2)

#*******************************************************************
#******************* Significant - warning errors  ******************************
#*******************************************************************
# mod1 fits better

#2011-2015
head(part3.success.long)

g.success<-subset(part3.success.long, select=c("Location","Country",'Country_work', "field_research", "type", "percent"))

canada<-g.success[g.success$Country_work=="Canada" | (!(g.success$Country_work=="Canada") & 
                                                        g.success$Country_work=="" & g.success$Country=="Canada"),]
#clean up names
canada$type<-revalue(canada$type, c("successful_grants_11_15_applied"="Applied 2011-2015",
                                    'successful_grants_11_15_fundamental'='Fundamental 2011-2015',
                                    'successful_grants_11_15_use' = 'Use-Inspired 2011-2015',
                                    "successful_grants_6_10_applied"="Applied 2006-2010",
                                    "successful_grants_6_10_fundamental"="Fundamental 2006-2010",
                                    "successful_grants_6_10_use"="Use-Inspired 2006-2010"))

canada<-canada[!canada$field_research=="",]
str(canada)
canada$type<-as.character(canada$type)
canada$year<-  str_split_fixed(canada$type, ' ', 2)[,2]
canada$type.g<-  str_split_fixed(canada$type, ' ', 2)[,1]
canada<-canada[!canada$year=="2006-2010",]
canada<-droplevels(canada)
tail(canada)
#canada$type.g<-as.factor(canada$type.g)
str(canada)


#make decimal 
canada$percent<-canada$percent/100
head(canada)
#unique(type.r.long$percent)
n.percent<-length(canada$percent)
canada$percent_trans<-(canada$percent*(n.percent-1)+0.5)/n.percent

mod1<-betareg(percent_trans ~ field_research * type.g, canada)
summary(mod1)
plot(mod1)
mod2<-betareg(percent_trans ~ field_research + type.g, canada)
AIC(mod1, mod2)

#*******************************************************************
#******************* Not Significant - warning error  ******************************
#*******************************************************************
# mod2 fits better but bad residuals plots



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

#2011-2015
head(p3_master)
funding.c<-subset(p3_master, select=c("Location","Country",'Country_work', "field_research", "survey", "year", "For.Profit", "Government" , "Internal", "Non.governmental",
                                      "Other"))
funding.c<-subset(funding.c, select = c("Location","Country",'Country_work', "field_research","year","For.Profit", "Government" , "Internal", "Non.governmental",
                                        "Other"))
canada<-funding.c[funding.c$Country_work=="Canada" | (!(funding.c$Country_work=="Canada") & 
                                                        funding.c$Country_work=="" & funding.c$Country=="Canada"),]
canada<-canada[!canada$field_research=="",]
canada<-canada[!canada$year=="6_10",]
canada<-droplevels(canada)
head(canada,20)

#put into long form
funding.c.long<-gather(canada, type, percent, -Location, -Country, -Country_work, -field_research, -year)
head(funding.c.long)
hist(funding.c.long$percent_trans)
#make decimal 
funding.c.long$percent<-funding.c.long$percent/100
tail(funding.c.long,20)
#unique(type.r.long$percent)
n.percent<-length(funding.c.long$percent)
funding.c.long$percent_trans<-(funding.c.long$percent*(n.percent-1)+0.5)/n.percent

mod1<-betareg(percent_trans ~ field_research * type, funding.c.long)
summary(mod1)
plot(mod2)
mod2<-betareg(percent_trans ~ field_research + type, funding.c.long)
AIC(mod1, mod2)
ggplot(funding.c.long, aes(field_research, percent)) + geom_point() +theme(axis.text.x = element_text(angle=90, vjust=0.5))
ggplot(funding.c.long, aes(field_research, percent, col=type)) + geom_point()+ theme(axis.text.x = element_text(angle=90, vjust=0.5))

#*******************************************************************
#***********************  There are some significances  *******************************
#*******************************************************************
#mod1 fits better

#2006-2010
head(p3_master)
funding.c<-subset(p3_master, select=c("Location","Country",'Country_work', "field_research", "survey", "year", "For.Profit", "Government" , "Internal", "Non.governmental",
                                      "Other"))
funding.c<-subset(funding.c, select = c("Location","Country",'Country_work', "field_research","year","For.Profit", "Government" , "Internal", "Non.governmental",
                                        "Other"))
canada<-funding.c[funding.c$Country_work=="Canada" | (!(funding.c$Country_work=="Canada") & 
                                                        funding.c$Country_work=="" & funding.c$Country=="Canada"),]
canada<-canada[!canada$field_research=="",]
canada<-canada[!canada$year=="11_15",]
canada<-droplevels(canada)
head(canada,20)

#put into long form
funding.p.long<-gather(canada, type, percent, -Location, -Country, -Country_work, -field_research, -year)
head(funding.p.long)
hist(funding.p.long$percent_trans)
#make decimal 
funding.p.long$percent<-funding.p.long$percent/100
tail(funding.p.long,20)
#unique(type.r.long$percent)
n.percent<-length(funding.p.long$percent)
funding.p.long$percent_trans<-(funding.p.long$percent*(n.percent-1)+0.5)/n.percent

mod1<-betareg(percent_trans ~ field_research * type, funding.p.long)
summary(mod1)
plot(mod2)
mod2<-betareg(percent_trans ~ field_research + type, funding.p.long)
AIC(mod1, mod2)

#*******************************************************************
#*********************** There are some significant differences  *******************************
#*******************************************************************
# mod1 fits better


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
#******************* Significant P = 0.003363 **********************
#*******************************************************************

#--------------------#--------------------#--------------------
#### Part4. Question 2. Change in research priority 
#--------------------#--------------------#--------------------

priority<-subset(part4, select=c("Location","Country",'Country_work', "field_research", "high_priority_fundamental", "high_priority_use_inspired", "high_priority_applied", 
                                 "high_priority_no_change"))

canada<-priority[priority$Country_work=="Canada" | (!(priority$Country_work=="Canada") & priority$Country_work=="" & priority$Country=="Canada"),]

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
#*******************Not Significant P = 0.1379**********************
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

f.change.mod1<-(glm(Freq ~ what.type*level*field_research, availability.ca, family="poisson"))
f.change.mod2<-(glm(Freq ~ what.type +level+field_research, availability.ca, family="poisson"))
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
#*******************Significant P = 2.874e-12 **********************
#*******************************************************************

