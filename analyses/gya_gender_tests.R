


#--------------------#--------------------#--------------------
#### Part4. Question 1. Research priority - fundamental
#--------------------#--------------------#--------------------

## order bars from very important (left) to can't comment (right)
important<-subset(part4, select=c("Location","Country",'Country_work', "gender","opinion_fundamental_important"))

canada<-important[important$Country_work=="Canada" | (!(important$Country_work=="Canada") & important$Country_work=="" & important$Country=="Canada"),]

## using table to count cases of each category
sum.important<-data.frame(table(canada$opinion_fundamental_important, canada$gender))
# remove non-responses
sum.important<-sum.important[!sum.important$Var2=="",]
sum.important<-sum.important[!sum.important$Var1=="",]

perceive<-aggregate(Freq~Var1, sum.important, sum)


### colour palette

myColors<-(brewer.pal(9, "Blues"))
myColors<-(myColors[c(9, 7, 6, 4, 2)])
colScale <- scale_fill_manual(name = "Var1",values = myColors)


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

priority.long<-priority.long[!priority.long$higher.priority=="",]

per.change<-aggregate(higher.priority~what.type, priority.long, sum)

#gender.perceive<-aggregate(higher.priority ~ gender + what.type, priority.long, sum)
# turn to percents
#male<-subset(gender.perceive, gender=="Male")
#female<-subset(gender.perceive, gender=="Female")
#other<-subset(gender.perceive, gender=="Other")
#gender.perceive$higher.priority<-ifelse(gender.perceive$gender=='Male',(gender.perceive$higher.priority/sum(male$higher.priority))*100, #gender.perceive$higher.priority)
#gender.perceive$higher.priority<-ifelse(gender.perceive$gender=='Female',(gender.perceive$higher.priority/sum(female$higher.priority))*100, #gender.perceive$higher.priority)
#gender.perceive$higher.priority<-ifelse(gender.perceive$gender=='Other',(gender.perceive$higher.priority/sum(other$higher.priority))*100, #gender.perceive$higher.priority)

# change order of the levels
per.change$what.type<-as.factor(per.change$what.type)
per.change$what.type<-factor(per.change$what.type, levels(per.change$what.type)[c(2,4,1,3)])

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
