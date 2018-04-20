
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
change<-subset(research.change, select=c("Location","Country",'Country_work', "what_participant_group", "changed_10yrs"))
canada<-change[change$Country_work=="Canada" | (!(change$Country_work=="Canada") & 
                                                  change$Country_work=="" & change$Country=="Canada"),]

canada<-canada[!canada$what_participant_group=="",]
canada<-canada[!canada$changed_10yrs=="",]
canada<-droplevels(canada)
canada$what_participant_group<-revalue(canada$what_participant_group, c("Senior academic researcher with >10 years of experience applying for research grants"="Senior academic >10 yrs",
                                                    'Non-academic researcher conducting or managing research in industry or government with >10 years of experience'='Non-academic >10yrs', 
                                                    'Early career academic researcher with <10 years experience applying for research grants since completion of PhD' = 'Early academic <10yrs',
                                                    'Postdoctoral fellow or research assistant with experience applying for research grants, or anticipating the need to apply for grants in the near future'="Post doc",
                                                    'Non-academic researcher conducting or managing research in industry or government with <10 years of experience'='Non-academic <10yrs'))
head(canada,20)
## using table to count cases of each category
sum.change<-data.frame(table(canada$changed_10yrs, canada$what_participant_group))
sum.change$Var1<-factor(sum.change$Var1, levels=c("No","Yes","Can't comment (new researcher)"))
canada.stats<-sum.change[!sum.change$Var1=="Can't comment (new researcher)",]

levels(sum.change$Var1)
canada.stats

change.mod1<-(glm(Freq ~ Var1*Var2, canada.stats, family="poisson"))
change.mod2<-(glm(Freq ~ Var1 +Var2, canada.stats, family="poisson"))
visreg(change.mod1, "Var2",by="Var1", scale="response", ylab="Number of responses", xlab="Gender")
summary(change.mod1)
anova(change.mod1, change.mod2, test="Chi")

#*******************************************************************
#*******************Not Sig P =0.4666***************************
#*******************************************************************
# If you dont take out Cant comment then the model is significant but only because senior researchers did not answer cant comment as expected - there was zero in that column instead of course

#--------------------#--------------------#--------------------
#### Part1. Question 1 & 3. Proportions of type of research current and past
#--------------------#--------------------#--------------------

#current
head(research.type)
research<-subset(research.type, select=c("Location","Country",'Country_work', "what_participant_group", "percent_fundemental_research_current", "percent_Use_inspired_Research_current", "percent_Applied_Research_current"))
canada<-research[research$Country_work=="Canada" | (!(research$Country_work=="Canada") & 
                                                      research$Country_work=="" & research$Country=="Canada"),]

canada<-canada[!canada$what_participant_group=="",]
canada$what_participant_group<-revalue(canada$what_participant_group, c("Senior academic researcher with >10 years of experience applying for research grants"="Senior academic >10 yrs",
                                                                        'Non-academic researcher conducting or managing research in industry or government with >10 years of experience'='Non-academic >10yrs', 
                                                                        'Early career academic researcher with <10 years experience applying for research grants since completion of PhD' = 'Early academic <10yrs',
                                                                        'Postdoctoral fellow or research assistant with experience applying for research grants, or anticipating the need to apply for grants in the near future'="Post doc",
                                                                        'Non-academic researcher conducting or managing research in industry or government with <10 years of experience'='Non-academic <10yrs'))

canada<-droplevels(canada)
head(canada)

#turn into long form
type.r.long<-gather(canada, type, percent, -Location, -Country, -Country_work, -what_participant_group)
head(type.r.long)

#make decimal 
type.r.long$percent<-type.r.long$percent/100
head(type.r.long, 20)
#transform data
n.percent<-length(type.r.long$percent)
type.r.long$percent_trans<-(type.r.long$percent*(n.percent-1)+0.5)/n.percent

mod1<-betareg(percent_trans ~ what_participant_group * type, type.r.long)
mod2<-betareg(percent_trans ~ what_participant_group + type, type.r.long)
AIC(mod1, mod2)

#*******************************************************************
#************************* Significant  ****************************
#*******************************************************************
#mod1 fits better


#now past
head(research.type)
p.research<-subset(research.type, select=c("Location","Country",'Country_work', "what_participant_group", "percent_Fundamental_Research_past", "percent_Use_inspired_Research_past", 
                                           "percent_Applied_Research_past"))
canada<-p.research[p.research$Country_work=="Canada" | (!(p.research$Country_work=="Canada") & 
                                                          p.research$Country_work=="" & p.research$Country=="Canada"),]
canada$what_participant_group<-revalue(canada$what_participant_group, c("Senior academic researcher with >10 years of experience applying for research grants"="Senior academic >10 yrs",
                                                                        'Non-academic researcher conducting or managing research in industry or government with >10 years of experience'='Non-academic >10yrs', 
                                                                        'Early career academic researcher with <10 years experience applying for research grants since completion of PhD' = 'Early academic <10yrs',
                                                                        'Postdoctoral fellow or research assistant with experience applying for research grants, or anticipating the need to apply for grants in the near future'="Post doc",
                                                                        'Non-academic researcher conducting or managing research in industry or government with <10 years of experience'='Non-academic <10yrs'))

canada<-canada[!canada$what_participant_group=="",]
canada<-droplevels(canada)
head(canada)

type.rp.long<-gather(canada, type.p, percent, -Location, -Country, -Country_work, -what_participant_group)
head(type.rp.long)

#make decimal 
type.rp.long$percent<-type.rp.long$percent/100
head(type.rp.long, 20)
unique(type.rp.long$percent)
#tranform data
n.percent<-length(type.rp.long$percent)
type.rp.long$percent_trans<-(type.rp.long$percent*(n.percent-1)+0.5)/n.percent


mod1<-betareg(percent_trans ~ what_participant_group * type.p, type.rp.long)
summary(mod1)
mod2<-betareg(percent_trans ~ what_participant_group + type.p, type.rp.long)
AIC(mod1, mod2)

#*******************************************************************
#******************* Significant *******************************
#*******************************************************************
# mod1 fits better

#--------------------#--------------------#--------------------
#### Part1. Question 4. Reason for change
#--------------------#--------------------#--------------------
head(canada)
reason<-subset(research.change, select=c("Location","Country",'Country_work', "what_participant_group", "Main_reason_change_interest_related", "Main_reason_change_Career_related", 
                                         "Main_reason_change_Funding_related", "Main_reason_change_Socially_related", "Main_reason_change_Other"))
canada<-reason[reason$Country_work=="Canada" | (!(reason$Country_work=="Canada") & 
                                                  reason$Country_work=="" & reason$Country=="Canada"),]
canada<-canada[!canada$what_participant_group=="",]
canada<-canada[!(is.na(canada$Main_reason_change_interest_related)& is.na(canada$Main_reason_change_Career_related) & is.na(canada$Main_reason_change_Funding_related) &
                   is.na(canada$Main_reason_change_Socially_related) & is.na(canada$Main_reason_change_Other)),]
canada$what_participant_group<-revalue(canada$what_participant_group, c("Senior academic researcher with >10 years of experience applying for research grants"="Senior academic >10 yrs",
                                                                        'Non-academic researcher conducting or managing research in industry or government with >10 years of experience'='Non-academic >10yrs', 
                                                                        'Early career academic researcher with <10 years experience applying for research grants since completion of PhD' = 'Early academic <10yrs',
                                                                        'Postdoctoral fellow or research assistant with experience applying for research grants, or anticipating the need to apply for grants in the near future'="Post doc",
                                                                        'Non-academic researcher conducting or managing research in industry or government with <10 years of experience'='Non-academic <10yrs'))

                                                                        
canada<-canada[!canada$what_participant_group=="Non-academic <10yrs",]
canada<-canada[!canada$what_participant_group=="Non-academic >10yrs",]
canada<-canada[!canada$what_participant_group=="Post doc",]
canada<-droplevels(canada)

require(tidyr)
canada.long<-gather(canada, reason, value, -Location, -what_participant_group, -Country_work, -Country)
head(canada.long)
canada.long<-canada.long[!(is.na(canada.long$value)),]

## using table to count cases of each category
sum.reason<-data.frame(table(canada.long$reason, canada.long$value, canada.long$what_participant_group))
sum.reason

reason.mod1<-(glm(Freq ~ Var1*Var3, sum.reason, family="poisson"))
reason.mod2<-(glm(Freq ~ Var1 +Var3, sum.reason, family="poisson"))
visreg(reason.mod1, "Var3",by="Var1", scale="response", ylab="Number of responses", xlab="Gender")
summary(reason.mod1)
anova(reason.mod1, reason.mod2, test="Chi")

#Geoff thinks this is ok even though you are making more indepent answers than original
#need to take out the low responses so non academics and post docs to make the model work better
#when the above is done then it is not significant p=0.09855

#*******************************************************************
#*******************Significant P = 0.01042 ************************
#*******************************************************************

#--------------------#--------------------#--------------------
#### Part1. Question 5. View of change
#--------------------#--------------------#--------------------

head(canada)
view<-subset(part1.view, select=c("Location","Country",'Country_work', "what_participant_group", "view_change_of_type"))

canada<-view[view$Country_work=="Canada" | (!(view$Country_work=="Canada") & 
                                              view$Country_work=="" & view$Country=="Canada"),]

canada<-canada[!canada$what_participant_group=="",]
canada<-canada[!canada$view_change_of_type=="",]
canada$what_participant_group<-revalue(canada$what_participant_group, c("Senior academic researcher with >10 years of experience applying for research grants"="Senior academic >10 yrs",
                                                                        'Non-academic researcher conducting or managing research in industry or government with >10 years of experience'='Non-academic >10yrs', 
                                                                        'Early career academic researcher with <10 years experience applying for research grants since completion of PhD' = 'Early academic <10yrs',
                                                                        'Postdoctoral fellow or research assistant with experience applying for research grants, or anticipating the need to apply for grants in the near future'="Post doc",
                                                                        'Non-academic researcher conducting or managing research in industry or government with <10 years of experience'='Non-academic <10yrs'))

canada<-droplevels(canada)

## using table to count cases of each category
sum.view<-data.frame(table(canada$view_change_of_type, canada$what_participant_group))
sum.view

view.mod1<-(glm(Freq ~ Var1*Var2, sum.view, family="poisson"))
view.mod2<-(glm(Freq ~ Var1 +Var2, sum.view, family="poisson"))
visreg(view.mod1, "Var2",by="Var1", scale="response", ylab="Number of responses", xlab="Gender")
summary(view.mod1)
anova(view.mod1, view.mod2, test="Chi")

#*******************************************************************
#*******************Not Significant P =0.8814 **********************
#*******************************************************************

#--------------------#--------------------#--------------------
#### Part2. Question 1 & 3. Level of partnership outside academia -current and before
#--------------------#--------------------#--------------------
head(part2.b.a)

#before

b4<-subset(part2.b.a, select=c("Location","Country",'Country_work', "what_participant_group","partnership_outside_before"))
canada<-b4[b4$Country_work=="Canada" | (!(b4$Country_work=="Canada") & 
                                          b4$Country_work=="" & b4$Country=="Canada"),]
canada$what_participant_group<-revalue(canada$what_participant_group, c("Senior academic researcher with >10 years of experience applying for research grants"="Senior academic >10 yrs",
                                                                        'Non-academic researcher conducting or managing research in industry or government with >10 years of experience'='Non-academic >10yrs', 
                                                                        'Early career academic researcher with <10 years experience applying for research grants since completion of PhD' = 'Early academic <10yrs',
                                                                        'Postdoctoral fellow or research assistant with experience applying for research grants, or anticipating the need to apply for grants in the near future'="Post doc",
                                                                        'Non-academic researcher conducting or managing research in industry or government with <10 years of experience'='Non-academic <10yrs'))
canada<-canada[!canada$what_participant_group=="",]
canada<-canada[!canada$partnership_outside_before=="",]
canada<-canada[!canada$what_participant_group=="Non-academic <10yrs",]
canada<-canada[!canada$what_participant_group=="Non-academic >10yrs",]
canada<-canada[!canada$what_participant_group=="Post doc",]
canada<-droplevels(canada)
head(canada)

## using table to count cases of each category
sum.b4<-data.frame(table(canada$partnership_outside_before, canada$what_participant_group))
sum.b4

b4.mod1<-(glm(Freq ~ Var1*Var2, sum.b4, family="poisson"))
b4.mod2<-(glm(Freq ~ Var1 +Var2, sum.b4, family="poisson"))
visreg(b4.mod1, "Var2",by="Var1", scale="response", ylab="Number of responses", xlab="Gender")
summary(b4.mod1)
anova(b4.mod1, b4.mod2, test="Chi")

#*******************************************************************
#******************* Significant P = 0.02978 ***********************
#*******************************************************************
#removing low responses made it significant p=0.001058
#current

cur<-subset(part2.b.a, select=c("Location","Country",'Country_work', "what_participant_group","partnership_outside"))
canada<-cur[cur$Country_work=="Canada" | (!(cur$Country_work=="Canada") & 
                                            cur$Country_work=="" & cur$Country=="Canada"),]
canada$what_participant_group<-revalue(canada$what_participant_group, c("Senior academic researcher with >10 years of experience applying for research grants"="Senior academic >10 yrs",
                                                                        'Non-academic researcher conducting or managing research in industry or government with >10 years of experience'='Non-academic >10yrs', 
                                                                        'Early career academic researcher with <10 years experience applying for research grants since completion of PhD' = 'Early academic <10yrs',
                                                                        'Postdoctoral fellow or research assistant with experience applying for research grants, or anticipating the need to apply for grants in the near future'="Post doc",
                                                                        'Non-academic researcher conducting or managing research in industry or government with <10 years of experience'='Non-academic <10yrs'))

canada<-canada[!canada$what_participant_group=="",]
canada<-canada[!canada$partnership_outside=="",]
canada<-canada[!canada$what_participant_group=="Non-academic <10yrs",]
canada<-canada[!canada$what_participant_group=="Non-academic >10yrs",]
canada<-canada[!canada$what_participant_group=="Post doc",]
canada<-droplevels(canada)
head(canada)

## using table to count cases of each category
sum.cur<-data.frame(table(canada$partnership_outside, canada$what_participant_group))
sum.cur

cur.mod1<-(glm(Freq ~ Var1*Var2, sum.cur, family="poisson"))
cur.mod2<-(glm(Freq ~ Var1 +Var2, sum.cur, family="poisson"))
visreg(cur.mod1, "Var2",by="Var1", scale="response", ylab="Number of responses", xlab="Gender")
summary(cur.mod1)
anova(cur.mod1, cur.mod2, test="Chi")

#*******************************************************************
#****************** Not Significant P=0.06016 **********************
#*******************************************************************
# take out low responses no sig p=0.1465

#is it ok that I spit up the time periods? - geoff thinks so

#--------------------#--------------------#--------------------
#### Part2. Question 2. Level of partnership outside academia -change
#--------------------#--------------------#--------------------
head(canada)
p.change<-subset(part2.change, select=c("Location","Country",'Country_work', "what_participant_group", "partnership_change_10yrs"))

canada<-p.change[p.change$Country_work=="Canada" | (!(p.change$Country_work=="Canada") & 
                                                      p.change$Country_work=="" & p.change$Country=="Canada"),]

canada<-canada[!canada$what_participant_group=="",]
canada<-canada[!canada$partnership_change_10yrs=="Can't comment (new researcher)",]
canada<-canada[!canada$partnership_change_10yrs=="",]
canada$what_participant_group<-revalue(canada$what_participant_group, c("Senior academic researcher with >10 years of experience applying for research grants"="Senior academic >10 yrs",
                                                                        'Non-academic researcher conducting or managing research in industry or government with >10 years of experience'='Non-academic >10yrs', 
                                                                        'Early career academic researcher with <10 years experience applying for research grants since completion of PhD' = 'Early academic <10yrs',
                                                                        'Postdoctoral fellow or research assistant with experience applying for research grants, or anticipating the need to apply for grants in the near future'="Post doc",
                                                                        'Non-academic researcher conducting or managing research in industry or government with <10 years of experience'='Non-academic <10yrs'))
canada<-canada[!canada$what_participant_group=="Non-academic <10yrs",]
canada<-canada[!canada$what_participant_group=="Non-academic >10yrs",]
canada<-canada[!canada$what_participant_group=="Post doc",]
canada<-droplevels(canada)

## using table to count cases of each category
sum.p.change<-data.frame(table(canada$partnership_change_10yrs, canada$what_participant_group))
sum.p.change

p.change.mod1<-(glm(Freq ~ Var1*Var2, sum.p.change, family="poisson"))
p.change.mod2<-(glm(Freq ~ Var1 +Var2, sum.p.change, family="poisson"))
visreg(p.change.mod1, "Var2",by="Var1", scale="response", ylab="Number of responses", xlab="Gender")
summary(p.change.mod1)
anova(p.change.mod1, p.change.mod2, test="Chi")

#*******************************************************************
#*******************Significant P < 2.2e-16 ***********************
#*******************************************************************
#only significance is that senior did not answer "cant comment" as the model expected which of course the didnt so removed can't comment
#when can't comment removed not sig p=0.4492
#with cant comment and low answers removed no sig p=0.3109


#--------------------#--------------------#--------------------
#### Part2. Question 4. Reason for change
#--------------------#--------------------#--------------------
head(canada)
p.reason<-subset(part2.reason, select=c("Location","Country",'Country_work', "what_participant_group", "reason_partnership_change_interest", "reason_partnership_change_career",
                                        "reason_partnership_change_socially", "reason_partnership_change_funding", "reason_partnership_change_other"))

canada<-p.reason[p.reason$Country_work=="Canada" | (!(p.reason$Country_work=="Canada") & 
                                                      p.reason$Country_work=="" & p.reason$Country=="Canada"),]

canada<-canada[!canada$what_participant_group=="",]
canada<-canada[!(is.na(canada$reason_partnership_change_interest)& is.na(canada$reason_partnership_change_career) & is.na(canada$reason_partnership_change_socially) &
                   is.na(canada$reason_partnership_change_funding) & is.na(canada$reason_partnership_change_other)),]
canada$what_participant_group<-revalue(canada$what_participant_group, c("Senior academic researcher with >10 years of experience applying for research grants"="Senior academic >10 yrs",
                                                                        'Non-academic researcher conducting or managing research in industry or government with >10 years of experience'='Non-academic >10yrs', 
                                                                        'Early career academic researcher with <10 years experience applying for research grants since completion of PhD' = 'Early academic <10yrs',
                                                                        'Postdoctoral fellow or research assistant with experience applying for research grants, or anticipating the need to apply for grants in the near future'="Post doc",
                                                                        'Non-academic researcher conducting or managing research in industry or government with <10 years of experience'='Non-academic <10yrs'))
canada<-canada[!canada$what_participant_group=="Non-academic <10yrs",]
canada<-canada[!canada$what_participant_group=="Non-academic >10yrs",]
canada<-canada[!canada$what_participant_group=="Post doc",]
canada<-droplevels(canada)

require(tidyr)
canada.long<-gather(canada, reason, value, -Location, -what_participant_group, -Country_work, -Country)
head(canada.long)
canada.long<-canada.long[!(is.na(canada.long$value)),]

## using table to count cases of each category
sum.p.reason<-data.frame(table(canada.long$reason, canada.long$value, canada.long$what_participant_group))
sum.p.reason

p.reason.mod1<-(glm(Freq ~ Var1*Var3, sum.p.reason, family="poisson"))
p.reason.mod2<-(glm(Freq ~ Var1 +Var3, sum.p.reason, family="poisson"))
visreg(p.reason.mod1, "Var3",by="Var1", scale="response", ylab="Number of responses", xlab="Gender")
summary(p.reason.mod1)
anova(p.reason.mod1, p.reason.mod2, test="Chi")

#Geoff thinks this is ok even though you are making more indepent answers than original


#*******************************************************************
#*******************Not Significant P = 0.05313*********************
#*******************************************************************
#removed low responses not sif p = 0.125


#--------------------#--------------------#--------------------
#### Part2. Question 5. View of change
#--------------------#--------------------#--------------------
head(canada)
p.view<-subset(part2.view, select=c("Location","Country",'Country_work', "what_participant_group", "view_change_partnership"))

canada<-p.view[p.view$Country_work=="Canada" | (!(p.view$Country_work=="Canada") & 
                                                  p.view$Country_work=="" & p.view$Country=="Canada"),]
canada<-canada[!canada$what_participant_group=="",]
canada<-canada[!canada$view_change_partnership=="",]
canada$what_participant_group<-revalue(canada$what_participant_group, c("Senior academic researcher with >10 years of experience applying for research grants"="Senior academic >10 yrs",
                                                                        'Non-academic researcher conducting or managing research in industry or government with >10 years of experience'='Non-academic >10yrs', 
                                                                        'Early career academic researcher with <10 years experience applying for research grants since completion of PhD' = 'Early academic <10yrs',
                                                                        'Postdoctoral fellow or research assistant with experience applying for research grants, or anticipating the need to apply for grants in the near future'="Post doc",
                                                                        'Non-academic researcher conducting or managing research in industry or government with <10 years of experience'='Non-academic <10yrs'))
canada<-canada[!canada$what_participant_group=="Non-academic <10yrs",]
canada<-canada[!canada$what_participant_group=="Non-academic >10yrs",]
canada<-canada[!canada$what_participant_group=="Post doc",]
canada<-droplevels(canada)

## using table to count cases of each category
sum.p.view<-data.frame(table(canada$view_change_partnership, canada$what_participant_group))
sum.p.view

p.view.mod1<-(glm(Freq ~ Var1*Var2, sum.p.view, family="poisson"))
p.view.mod2<-(glm(Freq ~ Var1 +Var2, sum.p.view, family="poisson"))
visreg(p.view.mod1, "Var2",by="Var1", scale="response", ylab="Number of responses", xlab="Gender")
summary(p.view.mod1)
anova(p.view.mod1, p.view.mod2, test="Chi")

#*******************************************************************
#*******************Not Significant P = 0.4098 *********************
#*******************************************************************
#low responses removed not sig p = 0.3916

#--------------------#--------------------#--------------------
#### Part3. Question 1. Grant apps types 2006-2010, 2011-2015
#--------------------#--------------------#--------------------
head(part3.grants.long)

#dont know how  


#--------------------#--------------------#--------------------
#### Part3. Question 2. Successful grant apps types 2006-2010, 2011-2015
#--------------------#--------------------#--------------------

#2006-2010
head(part3.success.long)

g.success<-subset(part3.success.long, select=c("Location","Country",'Country_work', "what_participant_group", "type", "percent"))

canada<-g.success[g.success$Country_work=="Canada" | (!(g.success$Country_work=="Canada") & 
                                                        g.success$Country_work=="" & g.success$Country=="Canada"),]
canada$what_participant_group<-revalue(canada$what_participant_group, c("Senior academic researcher with >10 years of experience applying for research grants"="Senior academic >10 yrs",
                                                                        'Non-academic researcher conducting or managing research in industry or government with >10 years of experience'='Non-academic >10yrs', 
                                                                        'Early career academic researcher with <10 years experience applying for research grants since completion of PhD' = 'Early academic <10yrs',
                                                                        'Postdoctoral fellow or research assistant with experience applying for research grants, or anticipating the need to apply for grants in the near future'="Post doc",
                                                                        'Non-academic researcher conducting or managing research in industry or government with <10 years of experience'='Non-academic <10yrs'))

#clean up names
canada$type<-revalue(canada$type, c("successful_grants_11_15_applied"="Applied 2011-2015",
                                    'successful_grants_11_15_fundamental'='Fundamental 2011-2015',
                                    'successful_grants_11_15_use' = 'Use-Inspired 2011-2015',
                                    "successful_grants_6_10_applied"="Applied 2006-2010",
                                    "successful_grants_6_10_fundamental"="Fundamental 2006-2010",
                                    "successful_grants_6_10_use"="Use-Inspired 2006-2010"))

canada<-canada[!canada$what_participant_group=="",]
head(canada)
unique(canada$type.g)
canada$type<-as.character(canada$type)
canada$year<-  str_split_fixed(canada$type, ' ', 2)[,2]
canada$type.g<-  str_split_fixed(canada$type, ' ', 2)[,1]
head(canada)


canada<-canada[!canada$year=="2011-2015",]
canada<-droplevels(canada)
tail(canada)

#make decimal 
canada$percent<-canada$percent/100
head(canada)
#unique(type.r.long$percent)
n.percent<-length(canada$percent)
canada$percent_trans<-(canada$percent*(n.percent-1)+0.5)/n.percent

mod1<-betareg(percent_trans ~ what_participant_group * type.g, canada)
summary(mod1)
plot(mod1)
mod2<-betareg(percent_trans ~ what_participant_group + type.g, canada)
AIC(mod1, mod2)

#*******************************************************************
#******************* Not Significant - warning message ******************************
#*******************************************************************
# mod 2 fits better

#2011-2015
head(part3.success.long)

g.success<-subset(part3.success.long, select=c("Location","Country",'Country_work', "what_participant_group", "type", "percent"))

canada<-g.success[g.success$Country_work=="Canada" | (!(g.success$Country_work=="Canada") & 
                                                        g.success$Country_work=="" & g.success$Country=="Canada"),]
canada$what_participant_group<-revalue(canada$what_participant_group, c("Senior academic researcher with >10 years of experience applying for research grants"="Senior academic >10 yrs",
                                                                        'Non-academic researcher conducting or managing research in industry or government with >10 years of experience'='Non-academic >10yrs', 
                                                                        'Early career academic researcher with <10 years experience applying for research grants since completion of PhD' = 'Early academic <10yrs',
                                                                        'Postdoctoral fellow or research assistant with experience applying for research grants, or anticipating the need to apply for grants in the near future'="Post doc",
                                                                        'Non-academic researcher conducting or managing research in industry or government with <10 years of experience'='Non-academic <10yrs'))
#clean up names
canada$type<-revalue(canada$type, c("successful_grants_11_15_applied"="Applied 2011-2015",
                                    'successful_grants_11_15_fundamental'='Fundamental 2011-2015',
                                    'successful_grants_11_15_use' = 'Use-Inspired 2011-2015',
                                    "successful_grants_6_10_applied"="Applied 2006-2010",
                                    "successful_grants_6_10_fundamental"="Fundamental 2006-2010",
                                    "successful_grants_6_10_use"="Use-Inspired 2006-2010"))

canada<-canada[!canada$what_participant_group=="",]
canada$type<-as.character(canada$type)
canada$year<-  str_split_fixed(canada$type, ' ', 2)[,2]
canada$type.g<-  str_split_fixed(canada$type, ' ', 2)[,1]
canada<-canada[!canada$year=="2006-2010",]
canada<-droplevels(canada)
tail(canada)

#make decimal 
canada$percent<-canada$percent/100
head(canada)
#unique(type.r.long$percent)
n.percent<-length(canada$percent)
canada$percent_trans<-(canada$percent*(n.percent-1)+0.5)/n.percent

mod1<-betareg(percent_trans ~ what_participant_group * type.g, canada)
summary(mod1)
plot(mod1)
mod2<-betareg(percent_trans ~ what_participant_group + type.g, canada)
AIC(mod1, mod2)
?betareg.fit
#*******************************************************************
#******************* Not Significant - warning message  ******************************
#*******************************************************************
#mod2 fits better


#--------------------#--------------------#--------------------
#### Part3. Question 3. Importance of suggesting practical applications 2006-2010, 2011-2015
#--------------------#--------------------#--------------------
head(part3.prac.long)


#dont know how - could test together  - I know how to do the years seperated


#--------------------#--------------------#--------------------
#### Part3. Question 4. Importance of having partners 2006-2010, 2011-2015
#--------------------#--------------------#--------------------
head(part3.part.long)


#dont know how - could test together  - I know how to do the years seperated

#--------------------#--------------------#--------------------
#### Part3. Question 5. Distribution of funding 2006-2010, 2011-2015
#--------------------#--------------------#--------------------
#2011-2015
head(p3_master)
funding.c<-subset(p3_master, select=c("Location","Country",'Country_work', "what_participant_group", "survey", "year", "For.Profit", "Government" , "Internal", "Non.governmental",
                                      "Other"))
funding.c<-subset(funding.c, select = c("Location","Country",'Country_work', "what_participant_group","year","For.Profit", "Government" , "Internal", "Non.governmental",
                                        "Other"))
canada<-funding.c[funding.c$Country_work=="Canada" | (!(funding.c$Country_work=="Canada") & 
                                                        funding.c$Country_work=="" & funding.c$Country=="Canada"),]
canada$what_participant_group<-revalue(canada$what_participant_group, c("Senior academic researcher with >10 years of experience applying for research grants"="Senior academic >10 yrs",
                                                                        'Non-academic researcher conducting or managing research in industry or government with >10 years of experience'='Non-academic >10yrs', 
                                                                        'Early career academic researcher with <10 years experience applying for research grants since completion of PhD' = 'Early academic <10yrs',
                                                                        'Postdoctoral fellow or research assistant with experience applying for research grants, or anticipating the need to apply for grants in the near future'="Post doc",
                                                                        'Non-academic researcher conducting or managing research in industry or government with <10 years of experience'='Non-academic <10yrs'))
canada<-canada[!canada$what_participant_group=="",]
canada<-canada[!canada$year=="6_10",]
canada<-droplevels(canada)
head(canada,20)

#put into long form
funding.c.long<-gather(canada, type, percent, -Location, -Country, -Country_work, -what_participant_group, -year)
head(funding.c.long)
hist(funding.c.long$percent_trans)
#make decimal 
funding.c.long$percent<-funding.c.long$percent/100
head(funding.c.long,20)
#unique(type.r.long$percent)
n.percent<-length(funding.c.long$percent)
funding.c.long$percent_trans<-(funding.c.long$percent*(n.percent-1)+0.5)/n.percent

mod1<-betareg(percent_trans ~ what_participant_group * type, funding.c.long)
summary(mod1)
plot(mod2)
mod2<-betareg(percent_trans ~ what_participant_group + type, funding.c.long)
AIC(mod1, mod2)

#*******************************************************************
#*********************** There are a few times there is significance *******************************
#*******************************************************************
# mod1 fits better

#2006-2010
head(p3_master)
funding.c<-subset(p3_master, select=c("Location","Country",'Country_work', "what_participant_group", "survey", "year", "For.Profit", "Government" , "Internal", "Non.governmental",
                                      "Other"))
funding.c<-subset(funding.c, select = c("Location","Country",'Country_work', "what_participant_group","year","For.Profit", "Government" , "Internal", "Non.governmental",
                                        "Other"))
canada<-funding.c[funding.c$Country_work=="Canada" | (!(funding.c$Country_work=="Canada") & 
                                                        funding.c$Country_work=="" & funding.c$Country=="Canada"),]
canada$what_participant_group<-revalue(canada$what_participant_group, c("Senior academic researcher with >10 years of experience applying for research grants"="Senior academic >10 yrs",
                                                                        'Non-academic researcher conducting or managing research in industry or government with >10 years of experience'='Non-academic >10yrs', 
                                                                        'Early career academic researcher with <10 years experience applying for research grants since completion of PhD' = 'Early academic <10yrs',
                                                                        'Postdoctoral fellow or research assistant with experience applying for research grants, or anticipating the need to apply for grants in the near future'="Post doc",
                                                                        'Non-academic researcher conducting or managing research in industry or government with <10 years of experience'='Non-academic <10yrs'))
canada<-canada[!canada$what_participant_group=="",]
canada<-canada[!canada$year=="11_15",]
canada<-droplevels(canada)
head(canada,20)

#put into long form
funding.p.long<-gather(canada, type, percent, -Location, -Country, -Country_work, -what_participant_group, -year)
head(funding.p.long)
hist(funding.p.long$percent_trans)
#make decimal 
funding.p.long$percent<-funding.p.long$percent/100
head(funding.p.long)
#unique(type.r.long$percent)
n.percent<-length(funding.p.long$percent)
funding.p.long$percent_trans<-(funding.p.long$percent*(n.percent-1)+0.5)/n.percent

mod1<-betareg(percent_trans ~ what_participant_group * type, funding.p.long)
summary(mod1)
plot(mod2)
mod2<-betareg(percent_trans ~ what_participant_group + type, funding.p.long)
AIC(mod1, mod2)

#*******************************************************************
#*********************** Significances *******************************
#*******************************************************************
# mod1 fits better

#--------------------#--------------------#--------------------
#### Part3. Question 6. Success rate change for types 2006-2010, 2011-2015
#--------------------#--------------------#--------------------
head(part3.change)

#dont know how to do it all together   could do each type seperate


#--------------------#--------------------#--------------------
#### Part4. Question 1. Research priority - fundamental
#--------------------#--------------------#--------------------

important<-subset(part4, select=c("Location","Country",'Country_work', "what_participant_group","opinion_fundamental_important"))

canada<-important[important$Country_work=="Canada" | (!(important$Country_work=="Canada") & important$Country_work=="" & important$Country=="Canada"),]
canada<-canada[!canada$opinion_fundamental_important=="",]
canada<-canada[!canada$what_participant_group=="",]
canada$opinion_fundamental_important<-factor(canada$opinion_fundamental_important, levels(canada$opinion_fundamental_important)[c(6,5,4,3,2,1)])

canada$what_participant_group<-revalue(canada$what_participant_group, c("Senior academic researcher with >10 years of experience applying for research grants"="Senior academic >10 yrs",
                                                                        'Non-academic researcher conducting or managing research in industry or government with >10 years of experience'='Non-academic >10yrs', 
                                                                        'Early career academic researcher with <10 years experience applying for research grants since completion of PhD' = 'Early academic <10yrs',
                                                                        'Postdoctoral fellow or research assistant with experience applying for research grants, or anticipating the need to apply for grants in the near future'="Post doc",
                                                                        'Non-academic researcher conducting or managing research in industry or government with <10 years of experience'='Non-academic <10yrs'))
canada<-canada[!canada$what_participant_group=="Non-academic <10yrs",]
canada<-canada[!canada$what_participant_group=="Non-academic >10yrs",]
canada<-canada[!canada$what_participant_group=="Post doc",]
canada<-droplevels(canada)
head(canada)
## using table to count cases of each category
sum.important<-data.frame(table(canada$opinion_fundamental_important, canada$what_participant_group))
# remove non-responses
#sum.important<-sum.important[!sum.important$Var2=="",]
#sum.important<-sum.important[!sum.important$Var1=="",]
sum.important<-sum.important[!sum.important$Var2=="Other",]
sum.important<-sum.important[!sum.important$Var2=="",]
head(sum.important)

#temp<-with(canada, table(opinion_fundamental_important, gender))
#chisq.test(temp)

priority.mod1<-(glm(Freq ~ Var1*Var2, sum.important, family="poisson"))
priority.mod2<-(glm(Freq ~ Var1 +Var2, sum.important, family="poisson"))
visreg(priority.mod1, "Var2",by="Var1", scale="response", ylab="Number of responses", xlab="Gender")
summary(priority.mod1)
head(priority.mod2)
anova(priority.mod1, priority.mod2, test="Chi")

#*******************************************************************
#*******************Not Significant P = 0.5901 *********************
#*******************************************************************
#low responses removed not sig p = 0.6842


#--------------------#--------------------#--------------------
#### Part4. Question 2. Change in research priority 
#--------------------#--------------------#--------------------

priority<-subset(part4, select=c("Location","Country",'Country_work', "what_participant_group", "high_priority_fundamental", "high_priority_use_inspired", "high_priority_applied", 
                                 "high_priority_no_change"))

canada<-priority[priority$Country_work=="Canada" | (!(priority$Country_work=="Canada") & priority$Country_work=="" & priority$Country=="Canada"),]
canada<-canada[!canada$what_participant_group=="",]
canada$what_participant_group<-revalue(canada$what_participant_group, c("Senior academic researcher with >10 years of experience applying for research grants"="Senior academic >10 yrs",
                                                                        'Non-academic researcher conducting or managing research in industry or government with >10 years of experience'='Non-academic >10yrs', 
                                                                        'Early career academic researcher with <10 years experience applying for research grants since completion of PhD' = 'Early academic <10yrs',
                                                                        'Postdoctoral fellow or research assistant with experience applying for research grants, or anticipating the need to apply for grants in the near future'="Post doc",
                                                                        'Non-academic researcher conducting or managing research in industry or government with <10 years of experience'='Non-academic <10yrs'))

#canada<-canada[!canada$what_participant_group=="Non-academic <10yrs",]
#canada<-canada[!canada$what_participant_group=="Non-academic >10yrs",]
#canada<-canada[!canada$what_participant_group=="Post doc",]
canada<-droplevels(canada)
head(canada)
## switch to long format

priority.long<-gather(canada, what.type, higher.priority, -Location, -what_participant_group, -Country, -Country_work)
# remove non-responses (n = 1066)

priority.long<-priority.long[!is.na(priority.long$higher.priority),]

stage.perceive<-data.frame(table( priority.long$what_participant_group , priority.long$what.type))

stage.perceive$Var2<-revalue(stage.perceive$Var2, c("high_priority_fundamental"="Fundamental",
                                                      'high_priority_use_inspired'='Use-inspired', 'high_priority_applied' = 'Applied',
                                                      'high_priority_no_change'="No change"))
stage.perceive

#priority.mod<-(glm(Freq ~ Var1*Var2, gender.perceive, family="poisson"))
#visreg(priority.mod, "Var2",by="Var1", scale="response", ylab="No. of responses (scaled by gender)", xlab="Gender")

priority.mod1<-(glm(Freq ~ Var1*Var2, stage.perceive, family="poisson"))
priority.mod2<-(glm(Freq ~ Var1 +Var2, stage.perceive, family="poisson"))
visreg(priority.mod1, "Var2",by="Var1", scale="response", ylab="Number of responses", xlab="Gender")
summary(priority.mod1)

anova(priority.mod1, priority.mod2, test="Chi")

#*******************************************************************
#*******************Not Significant P = 0.8265 *********************
#*******************************************************************
#low responses removed no sig p = 0.388

#--------------------#--------------------#--------------------
#### Part4. Question 3. Change in research funding 
#--------------------#--------------------#--------------------

availability.change<-subset(part4, select=c("Location", "Country",'Country_work', "what_participant_group", "available_funding_fundamental",  
                                            "available_funding_use_inspired", "available_funding_applied"))

canada<-availability.change[availability.change$Country_work=="Canada" | (!(availability.change$Country_work=="Canada") & availability.change$Country_work=="" & availability.change$Country=="Canada"),]
canada<-canada[!canada$what_participant_group=="",]
canada$what_participant_group<-revalue(canada$what_participant_group, c("Senior academic researcher with >10 years of experience applying for research grants"="Senior academic >10 yrs",
                                                                        'Non-academic researcher conducting or managing research in industry or government with >10 years of experience'='Non-academic >10yrs', 
                                                                        'Early career academic researcher with <10 years experience applying for research grants since completion of PhD' = 'Early academic <10yrs',
                                                                        'Postdoctoral fellow or research assistant with experience applying for research grants, or anticipating the need to apply for grants in the near future'="Post doc",
                                                                        'Non-academic researcher conducting or managing research in industry or government with <10 years of experience'='Non-academic <10yrs'))
head(availability.ca)
## switch to long format

availability.change.long.ca<-gather(canada, what.type, level, -what_participant_group, -Location, -Country, -Country_work)
availability.ca<-with(availability.change.long.ca, data.frame(table(what.type, what_participant_group, level)))
?with
#remove non response
availability.ca<-availability.ca[!availability.ca$level=="",]

availability.ca$what.type<-revalue(availability.ca$what.type, c("available_funding_fundamental"="Fundamental",
                                                                'available_funding_use_inspired'='Use-inspired', 'available_funding_applied' = 'Applied'))

f.change.mod1<-(glm(Freq ~ what.type*level*what_participant_group, availability.ca, family="poisson"))
f.change.mod2<-(glm(Freq ~ what.type +level+what_participant_group, availability.ca, family="poisson"))
visreg(f.change.mod1, "what_participant_group",by="what.type", scale="response", ylab="Number of responses", xlab="Gender")


anova(f.change.mod1, f.change.mod2, test="Chi")

#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#@@@@@@@@@@@@@@@@@@@@@@@@@@Not Done - dont know if it is right@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

#--------------------#--------------------#--------------------
#### Part4. Question 4. Research - next generation
#--------------------#--------------------#--------------------

next.generation<-subset(part4, select=c("Location", "Country","Country_work", "what_participant_group","next_generation"))
# remove non-response
next.generation<-next.generation[!next.generation$next_generation=="",]
canada<-canada[!canada$what_participant_group=="",]
canada<-next.generation[next.generation$Country_work=="Canada" | (!(next.generation$Country_work=="Canada") & next.generation$Country_work=="" & next.generation$Country=="Canada"),]
canada$what_participant_group<-revalue(canada$what_participant_group, c("Senior academic researcher with >10 years of experience applying for research grants"="Senior academic >10 yrs",
                                                                        'Non-academic researcher conducting or managing research in industry or government with >10 years of experience'='Non-academic >10yrs', 
                                                                        'Early career academic researcher with <10 years experience applying for research grants since completion of PhD' = 'Early academic <10yrs',
                                                                        'Postdoctoral fellow or research assistant with experience applying for research grants, or anticipating the need to apply for grants in the near future'="Post doc",
                                                                        'Non-academic researcher conducting or managing research in industry or government with <10 years of experience'='Non-academic <10yrs'))
canada<-canada[!canada$what_participant_group=="Non-academic <10yrs",]
canada<-canada[!canada$what_participant_group=="Non-academic >10yrs",]
canada<-canada[!canada$what_participant_group=="Post doc",]
canada<-droplevels(canada)
head(canada)
## using table to count cases of each category
impact<-data.frame(table(canada$next_generation, canada$what_participant_group))
impact

impact.mod1<-(glm(Freq ~ Var1*Var2, impact, family="poisson"))
impact.mod2<-(glm(Freq ~ Var1 +Var2, impact, family="poisson"))
visreg(impact.mod1, "Var2",by="Var1", scale="response", ylab="Number of responses", xlab="Career stage")
summary(impact.mod1)
anova(impact.mod1, impact.mod2, test="Chi")

#*******************************************************************
#*******************Not Significant P = 0.1962 **********************
#*******************************************************************
#removed low responses no sig p = 0.3326
