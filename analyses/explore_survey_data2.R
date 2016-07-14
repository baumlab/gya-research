

## Read in and explore the GYA data

setwd("/Users/jpwrobinson/Documents/git_repos/gya-research")

setwd("/Users/kristinatietjen/Documents/git_hub/gya-research")



## read data
survey<-read.csv("data/July-7-2016-7pm-Toronto_simplified.csv", header=TRUE)
#colnames(survey)<-as.character(unlist(survey[1,]))
#survey<-survey[-1,]

head(survey)
## remove some unnecessary columns
#survey$Username<-NULL
#survey$'Updated At'<-NULL
#survey'Number of Saves'<-NULL

## check number of complete vs. incomplete
table(survey$Status)

### keep complete data only
survey<-survey[survey$Status=="Complete",]

# add 'date column'
#require(stringr)
#survey$date<-str_split_fixed(survey$'Completed At', " ", 1)

colnames(survey)
#############
####Part5####
#############

survey<-subset(survey, select=c("Location",  "gender", "field_research", "Country_work", "PhD_Year", "what_participant_group"))


states<-c("California","New York", "Pennsylvania", "Nebraska", "Massachusetts", "Vermont","Texas",
          "Michigan", "Maryland", "Florida", "Washington", "Oregon", "Nevada", "Minnesota", "Arizona",
          "Wisconsin", "Virginia", "Utah", "Ohio", "North Carolina", "New Jersey", "New Hampshire", 
          "Maine", "Louisiana", "Indiana", "Hawaii", "Alabama", "Tennessee", "Oklahoma", "New Mexico",
          "Missouri", "Mississippi", "Iowa", "Delaware", "Colorado", "Illinois")

prov<-c("Ontario", "Quebec", "British Columbia", "Alberta", "Nova Scotia", "New Brunswick", 
        "Newfoundland and Labrador", "Manitoba", "Saskatchewan", "Prince Edward Island", "Yukon Territory", 
        "Nunavut")

survey$Country<-as.character(survey$Location)
survey$Country<-ifelse(survey$Country%in%states, 'USA', survey$Country)
survey$Country<-ifelse(survey$Country%in%prov, 'Canada', survey$Country)
survey$Country<-as.factor(survey$Country)

###saved


##############
#####Part1####
##############

## read in non-subsetted data again
survey<-read.csv("data/July-7-2016-7pm-Toronto_simplified.csv", header=TRUE)
### keep complete data only
survey<-survey[survey$Status=="Complete",]


survey$Country<-as.character(survey$Location)
survey$Country<-ifelse(survey$Country%in%states, 'USA', survey$Country)
survey$Country<-ifelse(survey$Country%in%prov, 'Canada', survey$Country)
survey$Country<-as.factor(survey$Country)

## 1. change all relevant variables to characters.......
survey$percent_Applied_Research_past<-as.character(survey$percent_Applied_Research_past)
survey$percent_Applied_Research_current<-as.character(survey$percent_Applied_Research_current)
survey$percent_fundemental_research_current<-as.character(survey$percent_fundemental_research_current)
survey$percent_Fundamental_Research_past<-as.character(survey$percent_Fundamental_Research_past)
survey$percent_Use_inspired_Research_current<-as.character(survey$percent_Use_inspired_Research_current)
survey$percent_Use_inspired_Research_past<-as.character(survey$percent_Use_inspired_Research_past)

colnames(survey)






require(stringr)
## remove all % symbols 
#survey$percent_Applied_Research_past<-str_replace_all(survey$percent_Applied_Research_past, "[%]", "")
survey$percent_Applied_Research_current<-str_replace_all(survey$percent_Applied_Research_current, "[%]", "")
survey$percent_Use_inspired_Research_current<-str_replace_all(survey$percent_Use_inspired_Research_current, "[%]", "")
survey$percent_fundemental_research_current<-str_replace_all(survey$percent_fundemental_research_current, "[%]", "")
#survey$percent_Fundamental_Research_past
#survey$percent_Use_inspired_Research_past

survey<-subset(survey, select=c("Location","Country",  "gender", "percent_fundemental_research_current", "percent_Applied_Research_current", 
                                "percent_Use_inspired_Research_current"))





fundemental<-aggregate(Location ~ percent_fundemental_research_current, survey, length)
applied<-aggregate(Location ~ percent_Applied_Research_current, survey, length)
use<-aggregate(Location ~ percent_Use_inspired_Research_current, survey, length)
## everything's fixed. % are gone.

### now we are going to standardise the % for each survey
## 1. if every category is blank, turn to NA
head(survey, 20)

survey$percent_fundemental_research_current[survey$percent_fundemental_research_current=="" & 
              survey$percent_Applied_Research_current=="" & survey$percent_Use_inspired_Research_current==""]<-NA
survey$percent_Applied_Research_current[is.na(survey$percent_fundemental_research_current) & 
              survey$percent_Applied_Research_current=="" & survey$percent_Use_inspired_Research_current==""]<-NA
survey$percent_Use_inspired_Research_current[is.na(survey$percent_fundemental_research_current) &
              is.na(survey$percent_Applied_Research_current) & survey$percent_Use_inspired_Research_current==""]<-NA
  
## 2. categories with answers and blanks, turn blanks to 0
survey$percent_fundemental_research_current[survey$percent_fundemental_research_current==""]<-0
survey$percent_Applied_Research_current[survey$percent_Applied_Research_current==""]<-0
survey$percent_Use_inspired_Research_current[survey$percent_Use_inspired_Research_current==""]<-0

survey<-survey[!is.na(survey$percent_fundemental_research_current),]
survey<-survey[!(survey$percent_fundemental_research_current==0 & survey$percent_Applied_Research_current==0 & survey$percent_Use_inspired_Research_current==0),]

## 3. Standardise % values > 100 in total
survey$percent_Applied_Research_current<-as.numeric(survey$percent_Applied_Research_current)
survey$percent_fundemental_research_current<-as.numeric(survey$percent_fundemental_research_current)
survey$percent_Use_inspired_Research_current<-as.numeric(survey$percent_Use_inspired_Research_current)
survey$total_research<-rowSums(survey[,4:6])

head(survey[survey$total_research>100,])
## need to change 64 surveys that are over 100%
survey$percent_Applied_Research_current<-ifelse(survey$total_research>100,(survey$percent_Applied_Research_current/survey$total_research)*100, survey$percent_Applied_Research_current)
survey$percent_fundemental_research_current<-ifelse(survey$total_research>100,(survey$percent_fundemental_research_current/survey$total_research)*100, survey$percent_fundemental_research_current)
survey$percent_Use_inspired_Research_current<-ifelse(survey$total_research>100,(survey$percent_Use_inspired_Research_current/survey$total_research)*100, survey$percent_Use_inspired_Research_current)

## switch to long format
require(tidyr)
survey.long<-gather(survey, type, percent, -Location, -gender, -Country)





#####do same as above but for past data

## read in non-subsetted data again
survey<-read.csv("data/July-7-2016-7pm-Toronto_simplified.csv", header=TRUE)
### keep complete data only
survey<-survey[survey$Status=="Complete",]


survey$Country<-as.character(survey$Location)
survey$Country<-ifelse(survey$Country%in%states, 'USA', survey$Country)
survey$Country<-ifelse(survey$Country%in%prov, 'Canada', survey$Country)
survey$Country<-as.factor(survey$Country)

## 1. change all relevant variables to characters.......
survey$percent_Applied_Research_past<-as.character(survey$percent_Applied_Research_past)
survey$percent_Applied_Research_current<-as.character(survey$percent_Applied_Research_current)
survey$percent_fundemental_research_current<-as.character(survey$percent_fundemental_research_current)
survey$percent_Fundamental_Research_past<-as.character(survey$percent_Fundamental_Research_past)
survey$percent_Use_inspired_Research_current<-as.character(survey$percent_Use_inspired_Research_current)
survey$percent_Use_inspired_Research_past<-as.character(survey$percent_Use_inspired_Research_past)

colnames(survey)



require(stringr)
## remove all % symbols 
survey$percent_Applied_Research_past<-str_replace_all(survey$percent_Applied_Research_past, "[%]", "")
#survey$percent_Applied_Research_current<-str_replace_all(survey$percent_Applied_Research_current, "[%]", "")
#survey$percent_Use_inspired_Research_current<-str_replace_all(survey$percent_Use_inspired_Research_current, "[%]", "")
#survey$percent_fundemental_research_current<-str_replace_all(survey$percent_fundemental_research_current, "[%]", "")
survey$percent_Fundamental_Research_past<-str_replace_all(survey$percent_Fundamental_Research_past, "[%]", "")
survey$percent_Use_inspired_Research_past<-str_replace_all(survey$percent_Use_inspired_Research_past, "[%]", "")

survey<-subset(survey, select=c("Location","Country",  "gender", "percent_Applied_Research_past", "percent_Fundamental_Research_past", 
                                "percent_Use_inspired_Research_past"))

## everything's fixed. % are gone.

### now we are going to standardise the % for each survey
## 1. if every category is blank, turn to NA
head(survey, 20)

survey$percent_Fundamental_Research_past[survey$percent_Fundamental_Research_past=="" & 
                                              survey$percent_Applied_Research_past=="" & survey$percent_Use_inspired_Research_past==""]<-NA
survey$percent_Applied_Research_past[is.na(survey$percent_Fundamental_Research_past) & 
                                          survey$percent_Applied_Research_past=="" & survey$percent_Use_inspired_Research_past==""]<-NA
survey$percent_Use_inspired_Research_past[is.na(survey$percent_Fundamental_Research_past) &
                                               is.na(survey$percent_Applied_Research_past) & survey$percent_Use_inspired_Research_past==""]<-NA

## 2. categories with answers and blanks, turn blanks to 0
survey$percent_Fundamental_Research_past[survey$percent_Fundamental_Research_past==""]<-0
survey$percent_Applied_Research_past[survey$percent_Applied_Research_past==""]<-0
survey$percent_Use_inspired_Research_past[survey$percent_Use_inspired_Research_past==""]<-0

survey<-survey[!is.na(survey$percent_Fundamental_Research_past),]
survey<-survey[!(survey$percent_Fundamental_Research_past==0 & survey$percent_Applied_Research_past==0 & survey$percent_Use_inspired_Research_past==0),]

## 3. Standardise % values > 100 in total
survey$percent_Applied_Research_past<-as.numeric(survey$percent_Applied_Research_past)
survey$percent_Fundamental_Research_past<-as.numeric(survey$percent_Fundamental_Research_past)
survey$percent_Use_inspired_Research_past<-as.numeric(survey$percent_Use_inspired_Research_past)
survey$total_research<-rowSums(survey[,4:6])

head(survey[survey$total_research>100,])
## need to change 64 surveys that are over 100%
survey$percent_Applied_Research_past<-ifelse(survey$total_research>100,(survey$percent_Applied_Research_past/survey$total_research)*100, survey$percent_Applied_Research_past)
survey$percent_Fundamental_Research_past<-ifelse(survey$total_research>100,(survey$percent_Fundamental_Research_past/survey$total_research)*100, survey$percent_Fundamental_Research_past)
survey$percent_Use_inspired_Research_past<-ifelse(survey$total_research>100,(survey$percent_Use_inspired_Research_past/survey$total_research)*100, survey$percent_Use_inspired_Research_past)

head(survey[survey$total_research>100,])

## switch to long format
require(tidyr)
survey.long.past<-gather(survey, type_past, percent_past, -Location, -gender, -Country)


#### yes/no have the portions changed and why

#read in non subsetted data again
survey<-read.csv("data/July-7-2016-7pm-Toronto_simplified.csv", header=TRUE)
### keep complete data only
survey<-survey[survey$Status=="Complete",]

states<-c("California","New York", "Pennsylvania", "Nebraska", "Massachusetts", "Vermont","Texas",
          "Michigan", "Maryland", "Florida", "Washington", "Oregon", "Nevada", "Minnesota", "Arizona",
          "Wisconsin", "Virginia", "Utah", "Ohio", "North Carolina", "New Jersey", "New Hampshire", 
          "Maine", "Louisiana", "Indiana", "Hawaii", "Alabama", "Tennessee", "Oklahoma", "New Mexico",
          "Missouri", "Mississippi", "Iowa", "Delaware", "Colorado", "Illinois")

prov<-c("Ontario", "Quebec", "British Columbia", "Alberta", "Nova Scotia", "New Brunswick", 
        "Newfoundland and Labrador", "Manitoba", "Saskatchewan", "Prince Edward Island", "Yukon Territory", 
        "Nunavut")

survey$Country<-as.character(survey$Location)
survey$Country<-ifelse(survey$Country%in%states, 'USA', survey$Country)
survey$Country<-ifelse(survey$Country%in%prov, 'Canada', survey$Country)
survey$Country<-as.factor(survey$Country)


survey.change<-subset(survey, select=c("Location","Country",  "gender", "changed_10yrs", "Main_reason_change_interest_related", 
                                "Main_reason_change_Career_related", "Main_reason_change_Funding_related","Main_reason_change_Socially_related",
                                "Main_reason_change_Other"))












############ save file as csv#############################

write.csv(survey, file="data/gya-country-responses.csv")

write.csv(survey.long, file="data/gya-surveys-cleaned-research.csv")

write.csv(survey.long.past, file="data/gya-surveys-cleaned-research-past.csv")

write.csv(survey.change, file="data/gya-change-reason.csv")
