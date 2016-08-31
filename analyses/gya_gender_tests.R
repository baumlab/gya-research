
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
part3.grants<-read.csv(file = "data/gya-part3.grants.csv")
part3.change<-read.csv(file="data/gya-part3.change.csv")
part3.success.long<-read.csv(file="data/gya-part3.success.long.csv")
part3.prac.long<-read.csv(file="data/gya-part3.prac.long.csv")
part3.prac.app<-read.csv(file="data/gya-part3.prac.app.csv")
part3.part.long<-read.csv(file="data/gya-part3.part.long.csv")
part3.part<-read.csv(file="data/gya-part3.part.csv")
p3_master.long<-read.csv(file="data/gya-p3_master.long.csv")
p3_master<-read.csv(file="data/gya-p3_master.csv")
research.type<-read.csv(file="data/gya-research-cleaned.csv")

## load required packages
require(gridExtra); require(tidyr); require(ggplot2); require(stringr);require(RColorBrewer); require(colorRamps); require(plotrix); require(plyr); require(visreg); require(DirichletReg); require(rgl); require(dr); require(betareg)


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
head(canada)
## using table to count cases of each category
sum.change<-data.frame(table(canada$changed_10yrs, canada$gender))
sum.change

change.mod1<-(glm(Freq ~ Var1*Var2, sum.change, family="poisson"))
change.mod2<-(glm(Freq ~ Var1 +Var2, sum.change, family="poisson"))
visreg(change.mod1, "Var2",by="Var1", scale="response", ylab="Number of responses", xlab="Gender")
summary(change.mod1)
head(change.mod2)
anova(change.mod1, change.mod2, test="Chi")

#*******************************************************************
#*******************Significant P = 0.008***************************
#*******************************************************************

#--------------------#--------------------#--------------------
#### Part1. Question 1 & 3. Proportions of type of research current and past                     -percents
#--------------------#--------------------#--------------------

install.packages("betareg")
#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
#from meeting with James

### turn into long form, divide percents by 100, transform the data, use beta regression (response variable is bounded by 0-1)
mod1<-betareg(percent ~ gender*category, dataset)
mod2<-betareg(percent ~ gender + category, dataset)
anova(mod1, mod2)

#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

head(research.type)

research<-subset(research.type, select=c("Location","Country",'Country_work', "gender", "percent_fundemental_research_current", "percent_Use_inspired_Research_current", 
                                         "percent_Applied_Research_current"))
canada<-research[research$Country_work=="Canada" | (!(research$Country_work=="Canada") & 
                                                      research$Country_work=="" & research$Country=="Canada"),]

canada<-canada[!canada$gender=="Other",]
canada<-canada[!canada$gender=="",]
canada<-droplevels(canada)
head(canada)

type.r.long<-gather(canada, type, percent, -Location, -Country, -Country_work, -gender)
head(type.r.long)

#make decimal 
type.r.long$percent<-type.r.long$percent/100
#head(type.r.long, 20)
#unique(type.r.long$percent)
n.percent<-length(type.r.long$percent)
type.r.long$percent_trans<-(type.r.long$percent*(n.percent-1)+0.5)/n.percent

mod1<-betareg(percent_trans ~ gender * type, type.r.long)
mod2<-betareg(percent_trans ~ gender + type, type.r.long)
AIC(mod1, mod2)

#*******************************************************************
#******************* Not Significant - mod2 fits *******************
#*******************************************************************

#now past
head(research.type)
p.research<-subset(research.type, select=c("Location","Country",'Country_work', "gender", "percent_Fundamental_Research_past", "percent_Use_inspired_Research_past", 
                                        "percent_Applied_Research_past"))
canada<-p.research[p.research$Country_work=="Canada" | (!(p.research$Country_work=="Canada") & 
                                                         p.research$Country_work=="" & p.research$Country=="Canada"),]
canada<-canada[!canada$gender=="Other",]
canada<-canada[!canada$gender=="",]
canada<-droplevels(canada)
head(canada)

type.rp.long<-gather(canada, type.p, percent, -Location, -Country, -Country_work, -gender)
head(type.rp.long)

#make decimal 
type.rp.long$percent<-type.rp.long$percent/100
head(type.rp.long, 20)
unique(type.rp.long$percent)
#tranform data
n.percent<-length(type.rp.long$percent)
type.rp.long$percent_trans<-(type.rp.long$percent*(n.percent-1)+0.5)/n.percent


mod1<-betareg(percent_trans ~ gender * type, type.rp.long)
mod2<-betareg(percent_trans ~ gender + type, type.rp.long)
AIC(mod1, mod2)

#*******************************************************************
#*******************Not Significant  *******************************
#*******************************************************************
# Mod2 fits better


###Meetings with Geoff###############################################
#compositional data and currently you can not analyze comp data with 0s in it so the cheat way is to change 0s to 0.0000001 etc 
#Geoff says since they sum to 100 it makes it hard to analyze
#isometric log transformation you will end up with two columns and then run a regression on one column 
#ordination techniques?
#read more into using linear regression with comp data
##do this 
#dirichlet regression look it up for comp data - tranform the data (isometric) and then then analyze using multiple variable linear regression models
#DirichReg(Y ~ depth + I(depth^2), inputData_train)
# y - last three columns and depth would be gender
#y<-canada[,5:7]
#head(y)
#DR_data(y, trafo = TRUE, base = 1 )
#canada$y<-DR_data(canada[,5:7])
#canada1<-DirichReg(y~ gender, canada, model = c("common"))
#canada1
#summary(canada1)
#*******************************************************************
#*******************Not Significant*********************************
#*******************************************************************
#now past
#head(research.type)
#p.research<-subset(research.type, select=c("Location","Country",'Country_work', "gender", "percent_Fundamental_Research_past", "percent_Use_inspired_Research_past", 
#                                        "percent_Applied_Research_past"))
#canada<-p.research[p.research$Country_work=="Canada" | (!(p.research$Country_work=="Canada") & 
#                                                         p.research$Country_work=="" & p.research$Country=="Canada"),]
#canada<-canada[!canada$gender=="Other",]
#canada<-canada[!canada$gender=="",]
#canada<-droplevels(canada)
#head(canada)
#y<-canada[,5:7]
#head(y)
#DR_data(y, trafo = TRUE, base = 1 )
#canada$y<-DR_data(canada[,5:7])
#canada2<-DirichReg(y~ gender, canada, model = c("common"))
#canada2
#summary(canada2)
#*******************************************************************
#*******************Not Significant*********************************
#*******************************************************************
##trying this version
#dont do this version unless you are a person who understands multi nomial reggression models
#y<-canada[,5:7]
#head(y)
#DR_data(y, trafo = TRUE, base = 1 )
#canada$y<-DR_data(canada[,5:7])
#canada1<-DirichReg(y~gender | 1, canada, model = "alternative", base = 3)
#canada2<-DirichReg(y~gender|gender, canada, model = "alternative", base = 3)
#anova(canada1, canada2)
#precision seems to be the same for both so take the simpler model and investigate the parameters
#summary(canada1)
#predict(canada1, type="response", newdata=data.frame(gender=c("Male", "Female")))
#precision model = significate P<2e-16
####################################################################

#--------------------#--------------------#--------------------
#### Part1. Question 4. Reason for change
#--------------------#--------------------#--------------------
head(canada)
reason<-subset(research.change, select=c("Location","Country",'Country_work', "gender", "Main_reason_change_interest_related", "Main_reason_change_Career_related", 
                                         "Main_reason_change_Funding_related", "Main_reason_change_Socially_related", "Main_reason_change_Other"))
canada<-reason[reason$Country_work=="Canada" | (!(reason$Country_work=="Canada") & 
                                                  reason$Country_work=="" & reason$Country=="Canada"),]

canada<-canada[!canada$gender=="Other",]
canada<-canada[!canada$gender=="",]
canada<-canada[!(is.na(canada$Main_reason_change_interest_related)& is.na(canada$Main_reason_change_Career_related) & is.na(canada$Main_reason_change_Funding_related) &
                       is.na(canada$Main_reason_change_Socially_related) & is.na(canada$Main_reason_change_Other)),]
canada<-droplevels(canada)

require(tidyr)
canada.long<-gather(canada, reason, value, -Location, -gender, -Country_work, -Country)
head(canada.long)
canada.long<-canada.long[!(is.na(canada.long$value)),]

## using table to count cases of each category
sum.reason<-data.frame(table(canada.long$reason, canada.long$value, canada.long$gender))
sum.reason

reason.mod1<-(glm(Freq ~ Var1*Var3, sum.reason, family="poisson"))
reason.mod2<-(glm(Freq ~ Var1 +Var3, sum.reason, family="poisson"))
visreg(reason.mod1, "Var3",by="Var1", scale="response", ylab="Number of responses", xlab="Gender")
summary(reason.mod1)
head(reason.mod1)
anova(reason.mod1, reason.mod2, test="Chi")

#@@@@@@@@@@@@@@@@@@@@@@@@Need to check @@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#Geoff thinks this is ok even though you are making more indepent answers than original

#*******************************************************************
#*******************Not Significant P = 0.7561**********************
#*******************************************************************

#--------------------#--------------------#--------------------
#### Part1. Question 5. View of change
#--------------------#--------------------#--------------------

head(canada)
view<-subset(part1.view, select=c("Location","Country",'Country_work', "gender", "view_change_of_type"))

canada<-view[view$Country_work=="Canada" | (!(view$Country_work=="Canada") & 
                                              view$Country_work=="" & view$Country=="Canada"),]

canada<-canada[!canada$gender=="Other",]
canada<-canada[!canada$gender=="",]
canada<-canada[!canada$view_change_of_type=="",]
canada<-droplevels(canada)

## using table to count cases of each category
sum.view<-data.frame(table(canada$view_change_of_type, canada$gender))
sum.view

view.mod1<-(glm(Freq ~ Var1*Var2, sum.view, family="poisson"))
view.mod2<-(glm(Freq ~ Var1 +Var2, sum.view, family="poisson"))
visreg(view.mod1, "Var2",by="Var1", scale="response", ylab="Number of responses", xlab="Gender")
anova(view.mod1, view.mod2, test="Chi")

#*******************************************************************
#*******************Not Significant P = 0.9749**********************
#*******************************************************************

#--------------------#--------------------#--------------------
#### Part2. Question 1 & 3. Level of partnership outside academia -current and before
#--------------------#--------------------#--------------------
head(part2.b.a)

#before

b4<-subset(part2.b.a, select=c("Location","Country",'Country_work', "gender","partnership_outside_before"))
canada<-b4[b4$Country_work=="Canada" | (!(b4$Country_work=="Canada") & 
                                          b4$Country_work=="" & b4$Country=="Canada"),]

canada<-canada[!canada$gender=="Other",]
canada<-canada[!canada$gender=="",]
canada<-canada[!canada$partnership_outside_before=="",]
canada<-droplevels(canada)
head(canada)

## using table to count cases of each category
sum.b4<-data.frame(table(canada$partnership_outside_before, canada$gender))
sum.b4

b4.mod1<-(glm(Freq ~ Var1*Var2, sum.b4, family="poisson"))
b4.mod2<-(glm(Freq ~ Var1 +Var2, sum.b4, family="poisson"))
visreg(b4.mod1, "Var2",by="Var1", scale="response", ylab="Number of responses", xlab="Gender")
anova(b4.mod1, b4.mod2, test="Chi")

#*******************************************************************
#*******************Not Significant P = 0.8977**********************
#*******************************************************************

#current

cur<-subset(part2.b.a, select=c("Location","Country",'Country_work', "gender","partnership_outside"))
canada<-cur[cur$Country_work=="Canada" | (!(cur$Country_work=="Canada") & 
                                            cur$Country_work=="" & cur$Country=="Canada"),]

canada<-canada[!canada$gender=="Other",]
canada<-canada[!canada$gender=="",]
canada<-canada[!canada$partnership_outside=="",]
canada<-droplevels(canada)
head(canada)

## using table to count cases of each category
sum.cur<-data.frame(table(canada$partnership_outside, canada$gender))
sum.cur

cur.mod1<-(glm(Freq ~ Var1*Var2, sum.cur, family="poisson"))
cur.mod2<-(glm(Freq ~ Var1 +Var2, sum.cur, family="poisson"))
visreg(cur.mod1, "Var2",by="Var1", scale="response", ylab="Number of responses", xlab="Gender")
anova(cur.mod1, cur.mod2, test="Chi")

#*******************************************************************
#*******************Not Significant P = 0.297***********************
#*******************************************************************


#--------------------#--------------------#--------------------
#### Part2. Question 2. Level of partnership outside academia -change
#--------------------#--------------------#--------------------
head(canada)
p.change<-subset(part2.change, select=c("Location","Country",'Country_work', "gender", "partnership_change_10yrs"))

canada<-p.change[p.change$Country_work=="Canada" | (!(p.change$Country_work=="Canada") & 
                                                      p.change$Country_work=="" & p.change$Country=="Canada"),]

canada<-canada[!canada$gender=="Other",]
canada<-canada[!canada$gender=="",]
canada<-canada[!canada$partnership_change_10yrs=="",]
canada<-droplevels(canada)

## using table to count cases of each category
sum.p.change<-data.frame(table(canada$partnership_change_10yrs, canada$gender))
sum.p.change

p.change.mod1<-(glm(Freq ~ Var1*Var2, sum.p.change, family="poisson"))
p.change.mod2<-(glm(Freq ~ Var1 +Var2, sum.p.change, family="poisson"))
visreg(p.change.mod1, "Var2",by="Var1", scale="response", ylab="Number of responses", xlab="Gender")
anova(p.change.mod1, p.change.mod2, test="Chi")

#*******************************************************************
#*******************Significant P = 0.0007814***********************
#*******************************************************************

#--------------------#--------------------#--------------------
#### Part2. Question 4. Reason for change
#--------------------#--------------------#--------------------
head(canada)
p.reason<-subset(part2.reason, select=c("Location","Country",'Country_work', "gender", "reason_partnership_change_interest", "reason_partnership_change_career",
                                        "reason_partnership_change_socially", "reason_partnership_change_funding", "reason_partnership_change_other"))

canada<-p.reason[p.reason$Country_work=="Canada" | (!(p.reason$Country_work=="Canada") & 
                                                      p.reason$Country_work=="" & p.reason$Country=="Canada"),]

canada<-canada[!canada$gender=="Other",]
canada<-canada[!canada$gender=="",]
canada<-canada[!(is.na(canada$reason_partnership_change_interest)& is.na(canada$reason_partnership_change_career) & is.na(canada$reason_partnership_change_socially) &
                   is.na(canada$reason_partnership_change_funding) & is.na(canada$reason_partnership_change_other)),]
canada<-droplevels(canada)

require(tidyr)
canada.long<-gather(canada, reason, value, -Location, -gender, -Country_work, -Country)
canada.long
canada.long<-canada.long[!(is.na(canada.long$value)),]

## using table to count cases of each category
sum.p.reason<-data.frame(table(canada.long$reason, canada.long$value, canada.long$gender))
sum.p.reason

p.reason.mod1<-(glm(Freq ~ Var1*Var3, sum.p.reason, family="poisson"))
p.reason.mod2<-(glm(Freq ~ Var1 +Var3, sum.p.reason, family="poisson"))
visreg(p.reason.mod1, "Var3",by="Var1", scale="response", ylab="Number of responses", xlab="Gender")
anova(p.reason.mod1, p.reason.mod2, test="Chi")

#@@@@@@@@@@@@@@@@@@@@@@@@Need to check @@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#Geoff thinks this is ok even though you are making more indepent answers than original


#*******************************************************************
#*******************Not Significant P = 0.7743**********************
#*******************************************************************

#--------------------#--------------------#--------------------
#### Part2. Question 5. View of change
#--------------------#--------------------#--------------------
head(canada)
p.view<-subset(part2.view, select=c("Location","Country",'Country_work', "gender", "view_change_partnership"))

canada<-p.view[p.view$Country_work=="Canada" | (!(p.view$Country_work=="Canada") & 
                                                  p.view$Country_work=="" & p.view$Country=="Canada"),]

canada<-canada[!canada$gender=="Other",]
canada<-canada[!canada$gender=="",]
canada<-canada[!canada$view_change_partnership=="",]
canada<-droplevels(canada)

## using table to count cases of each category
sum.p.view<-data.frame(table(canada$view_change_partnership, canada$gender))
sum.p.view

p.view.mod1<-(glm(Freq ~ Var1*Var2, sum.p.view, family="poisson"))
p.view.mod2<-(glm(Freq ~ Var1 +Var2, sum.p.view, family="poisson"))
visreg(p.view.mod1, "Var2",by="Var1", scale="response", ylab="Number of responses", xlab="Gender")
anova(p.view.mod1, p.view.mod2, test="Chi")

#*******************************************************************
#*******************Not Significant P = 0.6478**********************
#*******************************************************************


#--------------------#--------------------#--------------------
#### Part3. Question 1. Grant apps types 2006-2010, 2011-2015
#--------------------#--------------------#--------------------

head(part3.grants.long)

g.types<-subset(part3.grants.long, select=c("Location","Country",'Country_work', "gender", "number", "type.grant"))

canada<-g.types[g.types$Country_work=="Canada" | (!(g.types$Country_work=="Canada") & 
                                                    g.types$Country_work=="" & g.types$Country=="Canada"),]

canada<-canada[!canada$gender=="Other",]
canada<-canada[!canada$gender=="",]
canada<-droplevels(canada)
head(canada)

g.types.ca<-with(canada, data.frame(table(type.grant, gender, number)))
head(g.types.ca,20)

g.types.ca$type.grant<-revalue(g.types.ca$type.grant, c("external_pi_grant_11_15_applied"="Applied 2011-2015",
                                                                'external_pi_grant_11_15_fundamental'='Fundamental 2011-2015',
                                                                'external_pi_grant_11_15_use' = 'Use-Inspired 2011-2015',
                                                                "external_pi_grant_6_10_applied"="Applied 2006-2010",
                                                                "external_pi_grant_6_10_fundamental"="Fundamental 2006-2010",
                                                                "external_pi_grant_6_10_use"="Use-Inspired 2006-2010"))
head(g.types.ca, 20)


g.type.mod1<-(glm(Freq ~ type.grant*number*gender, g.types.ca, family="poisson"))
g.type.mod2<-(glm(Freq ~ type.grant*number+gender, g.types.ca, family="poisson"))
g.type.mod3<-(glm(Freq ~ type.grant+number*gender, g.types.ca, family="poisson"))
g.type.mod4<-(glm(Freq ~ type.grant +number+gender, g.types.ca, family="poisson"))
#visreg(g.type.mod1), "gender",by="what.type", scale="response", ylab="Number of responses", xlab="Gender")
anova(g.type.mod1, g.type.mod3, test="Chi")
AIC(g.type.mod1, g.type.mod2, g.type.mod3, g.type.mod4)

prat.app.mod1<-(glm(Freq ~ year*level*gender, prat.app.ca, family="poisson"))
prat.app.mod2<-(glm(Freq ~ year*level+gender, prat.app.ca, family="poisson"))
prat.app.mod3<-(glm(Freq ~ year+level*gender, prat.app.ca, family="poisson"))
prat.app.mod4<-(glm(Freq ~ year+level+gender, prat.app.ca, family="poisson"))
visreg(prat.app.mod1, "gender",by="year", scale="response", ylab="Number of responses", xlab="Gender")
anova(prat.app.mod1, prat.app.mod3, test="Chi")
AIC(prat.app.mod1, prat.app.mod2, prat.app.mod3, prat.app.mod4)

#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#@@@@@@@@@@@@@@@@@@@@@@@@@@Not Done - dont know if it is right@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#@@@@@@@@@@@@@@@There are 4 values if I test all of them together so I am going to try seperating the years@@@@@@@@@@
#@@@@@@@@@@@ James thinks to split the types and then do three models with Freq~ year*gender* @@@@@@@@@@@@@@@@@@@@@@@
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

#*******************************************************************
#*******************                      ************************
#*******************************************************************

####seperating years

#2006-2010

head(part3.grants)

g.types.p<-subset(part3.grants, select=c("Location","Country",'Country_work', "gender", "external_pi_grant_6_10_fundamental", "external_pi_grant_6_10_use", "external_pi_grant_6_10_applied"))

canada<-g.types.p[g.types.p$Country_work=="Canada" | (!(g.types.p$Country_work=="Canada") & 
                                                        g.types.p$Country_work=="" & g.types.p$Country=="Canada"),]

canada<-canada[!canada$gender=="Other",]
canada<-canada[!canada$gender=="",]
canada<-droplevels(canada)
head(canada)

#change to long form
require(tidyr)
g.type.p.long<-gather(canada, type, number, -Location, -gender,-Country_work, -Country)
head(g.type.p.long)

## using table to count cases of each category
sum.g.type.p<-data.frame(table(g.type.p.long$type, g.type.p.long$number, g.type.p.long$gender))
sum.g.type.p

type.p.mod1<-(glm(Freq ~ Var1*Var3*Var2, sum.g.type.p, family="poisson"))
type.p.mod2<-(glm(Freq ~ Var1*Var3+Var2, sum.g.type.p, family="poisson"))
type.p.mod3<-(glm(Freq ~ Var1+Var3*Var2, sum.g.type.p, family="poisson"))
type.p.mod4<-(glm(Freq ~ Var1 +Var3+Var2, sum.g.type.p, family="poisson"))
visreg(type.p.mod2, "Var3",by="Var1", scale="response", ylab="Number of responses", xlab="Gender")
anova(type.p.mod1, type.p.mod2, test="Chi")
AIC(type.p.mod1, type.p.mod2, type.p.mod3, type.p.mod4)
?AIC

#@@@@@@@@@@@@@@@@@@@@@@@@Need to check @@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#Geoff thinks this is ok even though you are making more indepent answers than original
#do I include Var2

#*******************************************************************
#*******************             **********************
#*******************************************************************

#2011-2015

head(part3.grants)

g.types.c<-subset(part3.grants, select=c("Location","Country",'Country_work', "gender", "external_pi_grant_11_15_fundamental", "external_pi_grant_11_15_use", "external_pi_grant_11_15_applied"))

canada<-g.types.c[g.types.c$Country_work=="Canada" | (!(g.types.c$Country_work=="Canada") & 
                                                        g.types.c$Country_work=="" & g.types.c$Country=="Canada"),]

canada<-canada[!canada$gender=="Other",]
canada<-canada[!canada$gender=="",]
canada<-droplevels(canada)
head(canada)

#change to long form
require(tidyr)
g.type.c.long<-gather(canada, type, number, -Location, -gender,-Country_work, -Country)
head(g.type.c.long)

## using table to count cases of each category
sum.g.type.c<-data.frame(table(g.type.c.long$type, g.type.c.long$number, g.type.c.long$gender))
sum.g.type.c

type.c.mod1<-(glm(Freq ~ Var1*Var3*Var2, sum.g.type.c, family="poisson"))
type.c.mod2<-(glm(Freq ~ Var1*Var3+Var2, sum.g.type.c, family="poisson"))
type.c.mod3<-(glm(Freq ~ Var1+Var3*Var2, sum.g.type.c, family="poisson"))
type.c.mod4<-(glm(Freq ~ Var1 +Var3+Var2, sum.g.type.c, family="poisson"))
visreg(type.p.mod2, "Var3",by="Var1", scale="response", ylab="Number of responses", xlab="Gender")
anova(type.p.mod1, type.p.mod3, test="Chi")



#*******************************************************************
#*******************                   **********************
#*******************************************************************

#James' way
head(part3.grants.long)

g.types<-subset(part3.grants.long, select=c("Location","Country",'Country_work', "gender", "number", "type.grant"))

canada<-g.types[g.types$Country_work=="Canada" | (!(g.types$Country_work=="Canada") & 
                                                    g.types$Country_work=="" & g.types$Country=="Canada"),]

canada<-canada[!canada$gender=="Other",]
canada<-canada[!canada$gender=="",]
canada<-droplevels(canada)
head(canada)

## using table to count cases of each category
g.types.ca<-with(canada, data.frame(table(type.grant, gender, number)))
head(g.types.ca,20)

#clean up names
g.types.ca$type.grant<-revalue(g.types.ca$type.grant, c("external_pi_grant_11_15_applied"="Applied 2011-2015",
                                                        'external_pi_grant_11_15_fundamental'='Fundamental 2011-2015',
                                                        'external_pi_grant_11_15_use' = 'Use-Inspired 2011-2015',
                                                        "external_pi_grant_6_10_applied"="Applied 2006-2010",
                                                        "external_pi_grant_6_10_fundamental"="Fundamental 2006-2010",
                                                        "external_pi_grant_6_10_use"="Use-Inspired 2006-2010"))
head(g.types.ca)

#select only Applied
g.types.ca$type<-ifelse(grepl("Applied", g.types.ca$type.grant), "Applied", "")
head(g.types.ca)

g.types.applied<-g.types.ca[!g.types.ca$type=="",]
levels(g.types.applied$number)
g.types.applied$type.grant<-as.character(g.types.applied$type.grant)
g.types.applied$year<-  str_split_fixed(g.types.applied$type.grant, ' ', 2)[,2]
#change order of levels
g.types.applied$number<-factor(g.types.applied$number, levels(g.types.applied$number)[c(1,2,6,7,3,4,5)])
head(g.types.applied)

g.types.applied.mod1<-(glm(Freq ~ number*year, g.types.applied, family="poisson"))
summary(g.types.applied.mod1)
plot(g.types.applied.mod1)
g.types.applied.mod2<-(glm(Freq ~ number*gender, g.types.applied, family="poisson"))
summary(g.types.applied.mod2)
plot(g.types.applied.mod2)
anova(g.types.applied.mod1, g.types.applied.mod2, test="Chi")
AIC(g.types.applied.mod1,g.types.applied.mod2, g.types.applied.mod3,g.types.applied.mod4)
#plot data to look at
ggplot(g.types.applied, aes(number, Freq)) + geom_point()
ggplot(g.types.applied, aes(number, Freq, col=gender, shape=gender)) + geom_point()
ggplot(g.types.applied, aes(number, Freq, col=gender, shape=year)) + geom_point()
#*******************************************************************
#*******************there is significantly more applied grants in 2011-2015 than 2006-2010 and gender did not signifcantly impact the number of grants **********************
#*******************************************************************

#--------------------#--------------------#--------------------
#### Part3. Question 2. Successful grant apps types 2006-2010, 2011-2015   -percents
#--------------------#--------------------#--------------------
#2006-2010
head(part3.success.long)

g.success<-subset(part3.success.long, select=c("Location","Country",'Country_work', "gender", "type", "percent"))

canada<-g.success[g.success$Country_work=="Canada" | (!(g.success$Country_work=="Canada") & 
                                                        g.success$Country_work=="" & g.success$Country=="Canada"),]

canada<-canada[!canada$gender=="Other",]
canada<-canada[!canada$gender=="",]
canada$year<-ifelse(grepl("11_15", canada$type), "2011-2015", "2006-2010")
canada<-canada[!canada$year=="2011-2015",]
canada<-droplevels(canada)
tail(canada)

#make decimal 
canada$percent<-canada$percent/100
head(canada)
#unique(type.r.long$percent)
n.percent<-length(canada$percent)
canada$percent_trans<-(canada$percent*(n.percent-1)+0.5)/n.percent

mod1<-betareg(percent_trans ~ gender * type, canada)
summary(mod1)
plot(mod1)
mod2<-betareg(percent_trans ~ gender + type, canada)
AIC(mod1, mod2)

#*******************************************************************
#******************* Not Significant  ******************************
#*******************************************************************
# mod2 fits better

#2011-2015
head(part3.success.long)

g.success<-subset(part3.success.long, select=c("Location","Country",'Country_work', "gender", "type", "percent"))

canada<-g.success[g.success$Country_work=="Canada" | (!(g.success$Country_work=="Canada") & 
                                                        g.success$Country_work=="" & g.success$Country=="Canada"),]

canada<-canada[!canada$gender=="Other",]
canada<-canada[!canada$gender=="",]
canada$year<-ifelse(grepl("11_15", canada$type), "2011-2015", "2006-2010")
canada<-canada[!canada$year=="2006-2010",]
canada<-droplevels(canada)
tail(canada)

#make decimal 
canada$percent<-canada$percent/100
head(canada)
#unique(type.r.long$percent)
n.percent<-length(canada$percent)
canada$percent_trans<-(canada$percent*(n.percent-1)+0.5)/n.percent

mod1<-betareg(percent_trans ~ gender * type, canada)
summary(mod1)
plot(mod1)
mod2<-betareg(percent_trans ~ gender + type, canada)
AIC(mod1, mod2)

#*******************************************************************
#******************* Not Significant  ******************************
#*******************************************************************
# the models are within 2 but mod 2 has fewer df

#--------------------#--------------------#--------------------
#### Part3. Question 3. Importance of suggesting practical applications 2006-2010, 2011-2015
#--------------------#--------------------#--------------------
head(part3.prac.long)

prac.app<-subset(part3.prac.long, select=c("Location","Country",'Country_work', "gender", "year", "level"))

canada<-prac.app[prac.app$Country_work=="Canada" | (!(prac.app$Country_work=="Canada") & 
                                                      prac.app$Country_work=="" & prac.app$Country=="Canada"),]

canada<-canada[!canada$gender=="Other",]
canada<-canada[!canada$gender=="",]
canada<-droplevels(canada)
head(canada)

prac.app.ca<-with(canada, data.frame(table(year, gender, level)))
head(prac.app.ca,20)

prac.app.ca$year<-revalue(prac.app.ca$year, c("practical_applications_important_11_15"="2011-2015",
                                                'practical_applications_important_6_10'='2006-2010'))
head(prac.app.ca)

prac.app.mod1<-(glm(Freq ~ year*level*gender, prac.app.ca, family="poisson"))
prac.app.mod2<-(glm(Freq ~ year+level*gender, prac.app.ca, family="poisson"))
prac.app.mod3<-(glm(Freq ~ year*level+gender, prac.app.ca, family="poisson"))
prac.app.mod4<-(glm(Freq ~ year +level+gender, prac.app.ca, family="poisson"))
visreg(g.success.mod1, "gender",by="year", scale="response", ylab="Number of responses", xlab="Gender")
anova(prac.app.mod1, prac.app.mod2, test="Chi")
AIC(prac.app.mod1, prac.app.mod2, prac.app.mod3, prac.app.mod4)

#*******************************************************************
#*******************Not Significant P = 0.06032*********************
#*******************************************************************


#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#@@@@@@@@@@@@@@@@@@@@@@@@@@Not Done - dont know if it is right@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#@@@@@@@@@@@@@@@@@@@@@@@@@@do I need to test each time period seperate?@@@@@@@@@@
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#update after meeting with Geoff can test together if you follow above format but it is ok to test seperate for the question we are asking.  If you want to know the difference between the years then do above
#look up 3 way contingency table

##########trying seperating time periods and not in long form##########

head(part3.prac.app)

prac.app<-subset(part3.prac.app, select=c("Location","Country",'Country_work', "gender", "practical_applications_important_11_15"))

canada<-prac.app[prac.app$Country_work=="Canada" | (!(prac.app$Country_work=="Canada") & 
                                                      prac.app$Country_work=="" & prac.app$Country=="Canada"),]

canada<-canada[!canada$gender=="Other",]
canada<-canada[!canada$gender=="",]
canada<-canada[!canada$practical_applications_important_11_15=="",]
canada<-droplevels(canada)
head(canada)

## using table to count cases of each category
sum.prac.app<-data.frame(table(canada$practical_applications_important_11_15, canada$gender))
sum.prac.app

prac.app.mod1<-(glm(Freq ~ Var1*Var2, sum.prac.app, family="poisson"))
prac.app.mod2<-(glm(Freq ~ Var1 +Var2, sum.prac.app, family="poisson"))
visreg(prac.app.mod1, "Var2",by="Var1", scale="response", ylab="Number of responses", xlab="Gender")
anova(prac.app.mod1, prac.app.mod2, test="Chi")

#*******************************************************************
#*******************Not Significant P = 0.05336*********************
#*******************************************************************

## now past

head(part3.prac.app)

prac.app.p<-subset(part3.prac.app, select=c("Location","Country",'Country_work', "gender", "practical_applications_important_6_10"))

canada<-prac.app.p[prac.app.p$Country_work=="Canada" | (!(prac.app.p$Country_work=="Canada") & 
                                                          prac.app.p$Country_work=="" & prac.app.p$Country=="Canada"),]

canada<-canada[!canada$gender=="Other",]
canada<-canada[!canada$gender=="",]
canada<-canada[!canada$practical_applications_important_6_10=="",]
canada<-droplevels(canada)
head(canada)

## using table to count cases of each category
sum.prac.app.p<-data.frame(table(canada$practical_applications_important_6_10, canada$gender))
sum.prac.app.p

prac.app.p.mod1<-(glm(Freq ~ Var1*Var2, sum.prac.app.p, family="poisson"))
prac.app.p.mod2<-(glm(Freq ~ Var1 +Var2, sum.prac.app.p, family="poisson"))
visreg(prac.app.p.mod1, "Var2",by="Var1", scale="response", ylab="Number of responses", xlab="Gender")
anova(prac.app.p.mod1, prac.app.p.mod2, test="Chi")

#*******************************************************************
#*******************Not Significant P = 0.1495**********************
#*******************************************************************

#--------------------#--------------------#--------------------
#### Part3. Question 4. Importance of having partners 2006-2010, 2011-2015
#--------------------#--------------------#--------------------
head(part3.part.long)

prat.app<-subset(part3.part.long, select=c("Location","Country",'Country_work', "gender", "year", "level"))

canada<-prat.app[prat.app$Country_work=="Canada" | (!(prat.app$Country_work=="Canada") & 
                                                      prat.app$Country_work=="" & prat.app$Country=="Canada"),]

canada<-canada[!canada$gender=="Other",]
canada<-canada[!canada$gender=="",]
canada<-droplevels(canada)
head(canada)

prat.app.ca<-with(canada, data.frame(table(year, gender, level)))
head(prat.app.ca,20)

prat.app.ca$year<-revalue(prat.app.ca$year, c("include_nonacademia_partners_success_11_15"="2011-2015",
                                              'include_nonacademia_partners_success_6_10'='2006-2010'))
head(prat.app.ca)

prat.app.mod1<-(glm(Freq ~ year*level*gender, prat.app.ca, family="poisson"))
prat.app.mod2<-(glm(Freq ~ year*level+gender, prat.app.ca, family="poisson"))
prat.app.mod3<-(glm(Freq ~ year+level*gender, prat.app.ca, family="poisson"))
prat.app.mod4<-(glm(Freq ~ year+level+gender, prat.app.ca, family="poisson"))
visreg(prat.app.mod1, "gender",by="year", scale="response", ylab="Number of responses", xlab="Gender")
anova(prat.app.mod1, prat.app.mod3, test="Chi")
AIC(prat.app.mod1, prat.app.mod2, prat.app.mod3, prat.app.mod4)

#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#@@@@@@@@@@@@@@@@@@@@@@@@@@Not Done - dont know if it is right@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#@@@@@@@@@@@@@@@@@@@@@@@@@@do I need to test each time period seperate?@@@@@@@@@@
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

#see part3 question 3

#*******************************************************************
#*******************Significant ************************************
#*******************************************************************


##########trying seperating time periods and not in long form##########

head(part3.part)

part.app<-subset(part3.part, select=c("Location","Country",'Country_work', "gender", "include_nonacademia_partners_success_11_15"))

canada<-part.app[part.app$Country_work=="Canada" | (!(part.app$Country_work=="Canada") & 
                                                      part.app$Country_work=="" & part.app$Country=="Canada"),]

canada<-canada[!canada$gender=="Other",]
canada<-canada[!canada$gender=="",]
canada<-canada[!canada$include_nonacademia_partners_success_11_15=="",]
canada<-droplevels(canada)
head(canada)

## using table to count cases of each category
sum.part.app<-data.frame(table(canada$include_nonacademia_partners_success_11_15, canada$gender))
sum.part.app

part.app.mod1<-(glm(Freq ~ Var1*Var2, sum.part.app, family="poisson"))
part.app.mod2<-(glm(Freq ~ Var1 +Var2, sum.part.app, family="poisson"))
visreg(part.app.mod1, "Var2",by="Var1", scale="response", ylab="Number of responses", xlab="Gender")
anova(part.app.mod1, part.app.mod2, test="Chi")

#*******************************************************************
#*******************Significant P = 0.008456 ***********************
#*******************************************************************

## now past

head(part3.part)

part.app.p<-subset(part3.part, select=c("Location","Country",'Country_work', "gender", "include_nonacademia_partners_success_6_10"))

canada<-part.app.p[part.app.p$Country_work=="Canada" | (!(part.app.p$Country_work=="Canada") & 
                                                          part.app.p$Country_work=="" & part.app.p$Country=="Canada"),]

canada<-canada[!canada$gender=="Other",]
canada<-canada[!canada$gender=="",]
canada<-canada[!canada$include_nonacademia_partners_success_6_10=="",]
canada<-droplevels(canada)
head(canada)

## using table to count cases of each category
sum.part.app.p<-data.frame(table(canada$include_nonacademia_partners_success_6_10, canada$gender))
sum.part.app.p

part.app.p.mod1<-(glm(Freq ~ Var1*Var2, sum.part.app.p, family="poisson"))
part.app.p.mod2<-(glm(Freq ~ Var1 +Var2, sum.part.app.p, family="poisson"))
visreg(part.app.p.mod1, "Var2",by="Var1", scale="response", ylab="Number of responses", xlab="Gender")
anova(part.app.p.mod1, part.app.p.mod2, test="Chi")

#*******************************************************************
#*******************Significant P = 0.01222 ************************
#*******************************************************************


#--------------------#--------------------#--------------------
#### Part3. Question 5. Distribution of funding 2006-2010, 2011-2015                     - percents
#--------------------#--------------------#--------------------
#2011-2015
head(p3_master)
funding.c<-subset(p3_master, select=c("Location","Country",'Country_work', "gender", "survey", "year", "For.Profit", "Government" , "Internal", "Non.governmental",
                                         "Other"))
funding.c<-subset(funding.c, select = c("Location","Country",'Country_work', "gender","year","For.Profit", "Government" , "Internal", "Non.governmental",
                                        "Other"))
canada<-funding.c[funding.c$Country_work=="Canada" | (!(funding.c$Country_work=="Canada") & 
                                                        funding.c$Country_work=="" & funding.c$Country=="Canada"),]
canada<-canada[!canada$gender=="Other",]
canada<-canada[!canada$gender=="",]
canada<-canada[!canada$year=="6_10",]
canada<-droplevels(canada)
head(canada,20)

#put into long form
funding.c.long<-gather(canada, type, percent, -Location, -Country, -Country_work, -gender, -year)
head(funding.c.long)
hist(funding.c.long$percent_trans)
#make decimal 
funding.c.long$percent<-funding.c.long$percent/100
tail(funding.c.long,20)
#unique(type.r.long$percent)
n.percent<-length(funding.c.long$percent)
funding.c.long$percent_trans<-(funding.c.long$percent*(n.percent-1)+0.5)/n.percent

mod1<-betareg(percent_trans ~ gender * type, funding.c.long)
summary(mod1)
plot(mod2)
mod2<-betareg(percent_trans ~ gender + type, funding.c.long)
AIC(mod1, mod2)

#*******************************************************************
#*********************** Significant *******************************
#*******************************************************************
# mod1 fits better

#2006-2010
head(p3_master)
funding.c<-subset(p3_master, select=c("Location","Country",'Country_work', "gender", "survey", "year", "For.Profit", "Government" , "Internal", "Non.governmental",
                                      "Other"))
funding.c<-subset(funding.c, select = c("Location","Country",'Country_work', "gender","year","For.Profit", "Government" , "Internal", "Non.governmental",
                                        "Other"))
canada<-funding.c[funding.c$Country_work=="Canada" | (!(funding.c$Country_work=="Canada") & 
                                                        funding.c$Country_work=="" & funding.c$Country=="Canada"),]
canada<-canada[!canada$gender=="Other",]
canada<-canada[!canada$gender=="",]
canada<-canada[!canada$year=="11_15",]
canada<-droplevels(canada)
head(canada,20)

#put into long form
funding.p.long<-gather(canada, type, percent, -Location, -Country, -Country_work, -gender, -year)
head(funding.p.long)
hist(funding.p.long$percent_trans)
#make decimal 
funding.p.long$percent<-funding.p.long$percent/100
tail(funding.p.long,20)
#unique(type.r.long$percent)
n.percent<-length(funding.p.long$percent)
funding.p.long$percent_trans<-(funding.p.long$percent*(n.percent-1)+0.5)/n.percent

mod1<-betareg(percent_trans ~ gender * type, funding.p.long)
summary(mod1)
plot(mod2)
mod2<-betareg(percent_trans ~ gender + type, funding.p.long)
AIC(mod1, mod2)

#*******************************************************************
#*********************** Significant *******************************
#*******************************************************************
# mod1 fits better


############################old############################################
#could be considered categorical or continous - working off of it being continous right now
#current
#head(p3_master)
#funding.c<-subset(p3_master, select=c("Location","Country",'Country_work', "gender", "survey", "year", "For.Profit", "Government" , "Internal", "Non.governmental",
#                                         "Other"))
#canada<-funding.c[funding.c$Country_work=="Canada" | (!(funding.c$Country_work=="Canada") & 
#                                                        funding.c$Country_work=="" & funding.c$Country=="Canada"),]
#canada<-canada[!canada$gender=="Other",]
#canada<-canada[!canada$gender=="",]
#canada<-canada[!canada$year=="6_10",]
#canada<-droplevels(canada)
#head(canada,20)
#canada<-subset(canada, select=c("Location","Country",'Country_work', "gender", "For.Profit", "Government" , "Internal", "Non.governmental",
#                                     "Other"))
#y<-canada[,5:9]
#head(y)
#DR_data(y, trafo = TRUE, base = 1 )
#canada$y<-DR_data(canada[,5:9])
#canada1<-DirichReg(y~ gender, canada, model = c("common"))
#canada1
#summary(canada1)
#predict(canada1, type="response", newdata = data.frame(gender=c("Male", "Female")))
#predict(canada1, type="response", newdata=data.frame(gender=c("Male", "Female")))
#now past
##########not done############


#--------------------#--------------------#--------------------
#### Part3. Question 6. Success rate change for types 2006-2010, 2011-2015
#--------------------#--------------------#--------------------
head(part3.change)

#do I test all of them together or seperate

#together

p3.change<-subset(part3.change, select=c("Location", "Country",'Country_work', "gender", "success_change_10yrs_fundamental",  
                                            "success_change_10yrs_use", "success_change_10yrs_applied"))

canada<-p3.change[p3.change$Country_work=="Canada" | (!(p3.change$Country_work=="Canada") & p3.change$Country_work=="" & p3.change$Country=="Canada"),]
canada<-canada[!canada$gender=="Other",]
canada<-canada[!canada$gender=="",]
canada<-droplevels(canada)
head(canada)

## switch to long format

p3.change.ca.long<-gather(canada, what.type, level, -gender, -Location, -Country, -Country_work)
head(p3.change.ca.long)
p3.change.ca<-with(p3.change.ca.long, data.frame(table(what.type, gender, level)))
head(p3.change.ca)
#remove non response
p3.change.ca<-p3.change.ca[!p3.change.ca$level=="",]

p3.change.ca$what.type<-revalue(p3.change.ca$what.type, c("success_change_10yrs_fundamental"="Fundamental",
                                                                'success_change_10yrs_use'='Use-inspired', 'success_change_10yrs_applied' = 'Applied'))
p3.change.ca

p3.change.mod1<-(glm(Freq ~ what.type*level*gender, p3.change.ca, family="poisson"))
p3.change.mod2<-(glm(Freq ~ what.type*level+gender, p3.change.ca, family="poisson"))
p3.change.mod3<-(glm(Freq ~ what.type+level*gender, p3.change.ca, family="poisson"))
p3.change.mod4<-(glm(Freq ~ what.type +level+gender, p3.change.ca, family="poisson"))
visreg(p3.change.mod1, "gender",by="what.type", scale="response", ylab="Number of responses", xlab="Gender")
anova(p3.change.mod1, p3.change.mod3, test="Chi")

#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#@@@@@@@@@@@@@@@@@@@@@@@@@@Not Done - dont know if it is right@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

#*******************************************************************
#*******************Not sig - ask Geoff       ************************
#*******************************************************************

# now seperate
#applied

head(part3.change)

p3.change.a<-subset(part3.change, select=c("Location", "Country",'Country_work', "gender", "success_change_10yrs_applied"))

canada<-p3.change.a[p3.change.a$Country_work=="Canada" | (!(p3.change.a$Country_work=="Canada") & p3.change.a$Country_work=="" & p3.change.a$Country=="Canada"),]
canada<-canada[!canada$gender=="Other",]
canada<-canada[!canada$gender=="",]
canada<-canada[!canada$success_change_10yrs_applied=="",]
canada<-droplevels(canada)
head(canada)

## using table to count cases of each category
change.a.sum<-data.frame(table(canada$success_change_10yrs_applied, canada$gender))
change.a.sum

change.a.mod1<-(glm(Freq ~ Var1*Var2, change.a.sum, family="poisson"))
change.a.mod2<-(glm(Freq ~ Var1 +Var2, change.a.sum, family="poisson"))
visreg(change.a.mod1, "Var2",by="Var1", scale="response", ylab="Number of responses", xlab="Gender")
anova(change.a.mod1, change.a.mod2, test="Chi")


#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#@@@@@@@@@@@@@@@@@@@@@@@@@@Not Done - dont know if it is right@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

#*******************************************************************
#*******************Not Significant P = 0.5555 *********************
#*******************************************************************

#fundamental

head(part3.change)

p3.change.f<-subset(part3.change, select=c("Location", "Country",'Country_work', "gender", "success_change_10yrs_fundamental"))

canada<-p3.change.f[p3.change.f$Country_work=="Canada" | (!(p3.change.f$Country_work=="Canada") & p3.change.f$Country_work=="" & p3.change.f$Country=="Canada"),]
canada<-canada[!canada$gender=="Other",]
canada<-canada[!canada$gender=="",]
canada<-canada[!canada$success_change_10yrs_fundamental=="",]
canada<-droplevels(canada)
head(canada)

## using table to count cases of each category
change.f.sum<-data.frame(table(canada$success_change_10yrs_fundamental, canada$gender))
change.f.sum

change.f.mod1<-(glm(Freq ~ Var1*Var2, change.f.sum, family="poisson"))
change.f.mod2<-(glm(Freq ~ Var1 +Var2, change.f.sum, family="poisson"))
visreg(change.f.mod1, "Var2",by="Var1", scale="response", ylab="Number of responses", xlab="Gender")
anova(change.f.mod1, change.f.mod2, test="Chi")


#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#@@@@@@@@@@@@@@@@@@@@@@@@@@Not Done - dont know if it is right@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

#*******************************************************************
#*******************Significant P = 0.0267 *************************
#*******************************************************************

#use-inspired

head(part3.change)

p3.change.i<-subset(part3.change, select=c("Location", "Country",'Country_work', "gender", "success_change_10yrs_use"))

canada<-p3.change.i[p3.change.i$Country_work=="Canada" | (!(p3.change.i$Country_work=="Canada") & p3.change.i$Country_work=="" & p3.change.i$Country=="Canada"),]
canada<-canada[!canada$gender=="Other",]
canada<-canada[!canada$gender=="",]
canada<-canada[!canada$success_change_10yrs_use=="",]
canada<-droplevels(canada)
head(canada)

## using table to count cases of each category
change.i.sum<-data.frame(table(canada$success_change_10yrs_use, canada$gender))
change.i.sum

change.i.mod1<-(glm(Freq ~ Var1*Var2, change.i.sum, family="poisson"))
change.i.mod2<-(glm(Freq ~ Var1 +Var2, change.i.sum, family="poisson"))
visreg(change.i.mod1, "Var2",by="Var1", scale="response", ylab="Number of responses", xlab="Gender")
anova(change.i.mod1, change.i.mod2, test="Chi")


#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#@@@@@@@@@@@@@@@@@@@@@@@@@@Not Done - dont know if it is right@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

#*******************************************************************
#*******************Not Significant P = 0.0.8903********************
#*******************************************************************

#--------------------#--------------------#--------------------
#### Part4. Question 1. Research priority - fundamental
#--------------------#--------------------#--------------------
head(important)
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

#temp<-with(canada, table(opinion_fundamental_important, gender))
#chisq.test(temp)

priority.mod1<-(glm(Freq ~ Var1*Var2, sum.important, family="poisson"))
priority.mod2<-(glm(Freq ~ Var1 +Var2, sum.important, family="poisson"))
visreg(priority.mod1, "Var2",by="Var1", scale="response", ylab="Number of responses", xlab="Gender")
summary(priority.mod1)
head(priority.mod2)
anova(priority.mod1, priority.mod2, test="Chi")

#*******************************************************************
#*******************Not Significant P = 0.1823**********************
#*******************************************************************

#--------------------#--------------------#--------------------
#### Part4. Question 2. Change in research priority 
#--------------------#--------------------#--------------------

priority<-subset(part4, select=c("Location","Country",'Country_work', "gender", "high_priority_fundamental", "high_priority_use_inspired", "high_priority_applied", 
                                 "high_priority_no_change"))

canada<-priority[priority$Country_work=="Canada" | (!(priority$Country_work=="Canada") & priority$Country_work=="" & priority$Country=="Canada"),]
canada<-canada[!canada$gender=="Other",]
canada<-canada[!canada$gender=="",]
canada<-droplevels(canada)
head(canada)
head(gender.perceive)
## switch to long format

priority.long<-gather(canada, what.type, higher.priority, -Location, -gender, -Country, -Country_work)
# remove non-responses (n = 1066)

priority.long<-priority.long[!is.na(priority.long$higher.priority),]

gender.perceive<-data.frame(table( priority.long$gender , priority.long$what.type))

gender.perceive$Var2<-revalue(gender.perceive$Var2, c("high_priority_fundamental"="Fundamental",
                                                                'high_priority_use_inspired'='Use-inspired', 'high_priority_applied' = 'Applied',
                                                          'high_priority_no_change'="No change"))

#priority.mod<-(glm(Freq ~ Var1*Var2, gender.perceive, family="poisson"))
#visreg(priority.mod, "Var2",by="Var1", scale="response", ylab="No. of responses (scaled by gender)", xlab="Gender")

priority.mod1<-(glm(Freq ~ Var1*Var2, gender.perceive, family="poisson"))
priority.mod2<-(glm(Freq ~ Var1 +Var2, gender.perceive, family="poisson"))
visreg(priority.mod1, "Var2",by="Var1", scale="response", ylab="Number of responses", xlab="Gender")
summary(priority.mod1)

anova(priority.mod1, priority.mod2, test="Chi")

#*******************************************************************
#*******************Not Significant P = 0.254**********************
#*******************************************************************

#--------------------#--------------------#--------------------
#### Part4. Question 3. Change in research funding 
#--------------------#--------------------#--------------------

availability.change<-subset(part4, select=c("Location", "Country",'Country_work', "gender", "available_funding_fundamental",  
                                            "available_funding_use_inspired", "available_funding_applied"))

canada<-availability.change[availability.change$Country_work=="Canada" | (!(availability.change$Country_work=="Canada") & availability.change$Country_work=="" & availability.change$Country=="Canada"),]
canada<-canada[!canada$gender=="Other",]
canada<-canada[!canada$gender=="",]
canada<-droplevels(canada)
head(canada)

## switch to long format

availability.change.long.ca<-gather(canada, what.type, level, -gender, -Location, -Country, -Country_work)
head(availability.change.long.ca)
availability.ca<-with(availability.change.long.ca, data.frame(table(what.type, gender, level)))
head(availability.ca)
#remove non response
availability.ca<-availability.ca[!availability.ca$level=="",]

availability.ca$what.type<-revalue(availability.ca$what.type, c("available_funding_fundamental"="Fundamental",
                                                                'available_funding_use_inspired'='Use-inspired', 'available_funding_applied' = 'Applied'))

f.change.mod1<-(glm(Freq ~ what.type*level*gender, availability.ca, family="poisson"))
f.change.mod2<-(glm(Freq ~ what.type*level+gender, availability.ca, family="poisson"))
f.change.mod3<-(glm(Freq ~ what.type+level*gender, availability.ca, family="poisson"))
f.change.mod4<-(glm(Freq ~ what.type +level+gender, availability.ca, family="poisson"))
visreg(f.change.mod1, "gender",by="what.type", scale="response", ylab="Number of responses", xlab="Gender")
anova(f.change.mod1, f.change.mod3, test="Chi")

#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#@@@@@@@@@@@@@@@@@@@@@@@@@@Not Done - dont know if it is right@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

#*******************************************************************
#*******************not sig  - ask Geoff ************************
#*******************************************************************


#--------------------#--------------------#--------------------
#### Part4. Question 4. Research - next generation
#--------------------#--------------------#--------------------

next.generation<-subset(part4, select=c("Location", "Country","Country_work", "gender","next_generation"))
# remove non-response
next.generation<-next.generation[!next.generation$next_generation=="",]
canada<-next.generation[next.generation$Country_work=="Canada" | (!(next.generation$Country_work=="Canada") & next.generation$Country_work=="" & next.generation$Country=="Canada"),]

canada<-canada[!canada$gender=="Other",]
canada<-canada[!canada$gender=="",]
canada<-droplevels(canada)
head(canada)
## using table to count cases of each category
impact<-data.frame(table(canada$next_generation, canada$gender))
impact

impact.mod1<-(glm(Freq ~ Var1*Var2, impact, family="poisson"))
impact.mod2<-(glm(Freq ~ Var1 +Var2, impact, family="poisson"))
visreg(impact.mod1, "Var2",by="Var1", scale="response", ylab="Number of responses", xlab="Gender")
anova(impact.mod1, impact.mod2, test="Chi")

#*******************************************************************
#*******************Not Significant P = 0.1735**********************
#*******************************************************************

