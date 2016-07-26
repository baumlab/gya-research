

## Read in and explore the GYA data

setwd("/Users/jpwrobinson/Documents/git_repos/gya-research")

setwd("/Users/kristinatietjen/Documents/git_hub/gya-research")



## read data

survey.all<-read.csv("data/Jul 18 2016 1149am - Hamilton.csv", header=TRUE)
dim(survey.all)

suveycols<-c("Status","Location"	,"what_participant_group",
  "percent_fundemental_research_current",	"percent_Use_inspired_Research_current"	,
  "percent_Applied_Research_current",	"changed_10yrs",	"percent_Fundamental_Research_past"	,
  "percent_Use_inspired_Research_past",	"percent_Applied_Research_past",	"Main_reason_change_interest_related",
  "Main_reason_change_Career_related",	"Main_reason_change_Funding_related",	"Main_reason_change_Socially_related",
  "Main_reason_change_Other",	"Main_reason_change_Other_text",	"view_change_of_type",
  "partnership_outside",	"partnership_change_10yrs",	"partnership_outside_before",	
  "reason_partnership_change_interest",	"reason_partnership_change_career",	"reason_partnership_change_socially",
  "reason_partnership_change_funding",	"reason_partnership_change_other",	"reason_partnership_change_other_text",
  "view_change_partnership",	"external_pi_grant_11_15_fundamental",	"external_pi_grant_11_15_use",
  "external_pi_grant_11_15_applied","external_pi_grant_6_10_fundamental",	"external_pi_grant_6_10_use",
  "external_pi_grant_6_10_applied",	"successful_grants_11_15_fundamental",	"successful_grants_11_15_use",
  "successful_grants_11_15_applied",	"successful_grants_6_10_fundamental",	"successful_grants_6_10_use",
  "successful_grants_6_10_applied",	"practical_applications_important_11_15",	"practical_applications_important_6_10",	
  "include_nonacademia_partners_success_11_15", "include_nonacademia_partners_success_6_10",
  "distribution_funding_11_15_internal",	"distriution_funding_11_15_government",	"distriution_funding_11_15_for_profit",	
  "distriution_funding_11_15_nongov",	"distriution_funding_11_15_other",	"distriution_funding_11_15_other_text",
  "distriution_funding_6_10_internal",	"distriution_funding_6_10_government",	"distriution_funding_6_10_for_profit",
  "distriution_funding_6_10_nongov",	"distriution_funding_6_10_other",	"distriution_funding_6_10_other_text",
  "success_change_10yrs_fundamental",	"success_change_10yrs_use",	"success_change_10yrs_applied",
  "opinion_fundamental_important",	"high_priority_fundamental",	"high_priority_use_inspired",
  "high_priority_applied",	"high_priority_no_change",	"high_priority_comments",	"available_funding_fundamental",
  "available_funding_use_inspired",	"available_funding_applied",	"next_generation",	"next_generation_Comments",
  "field_research",	"PhD_Year",	"Country_work",	"gender")
## remove some unnecessary columns
survey.all$'Username'<-NULL
survey.all$'Updated_At'<-NULL
survey.all$'Number_of_Saves'<-NULL
survey.all$'Internal_ID'<-NULL
survey.all$'Language'<-NULL
survey.all$'Created_At'<-NULL
survey.all$'GET_Variables'<-NULL
survey.all$'Referrer'<-NULL
survey.all$'Weighted_Score'<-NULL
survey.all$'Completion_Time'<-NULL
survey.all$'IP_Address'<-NULL
survey.all$'Invite_Code'<-NULL
survey.all$'Invite_Email'<-NULL
survey.all$'Invite_Name'<-NULL
survey.all$'Collector'<-NULL
survey.all$'final_comments'<-NULL
##change column names

colnames(survey.all)<-suveycols

## check number of complete vs. incomplete
table(survey.all$Status)

### keep complete data only
survey<-survey.all[survey.all$Status=="Complete",]
dim(survey)
### change Canada and USA
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
head (survey)

write.csv(survey, file="data/gya-without-incomplete.csv")

#############
####Part5####
#############

survey.what<-subset(survey, select=c("Location",  "gender", "field_research", "Country_work", "PhD_Year", "what_participant_group"))

states<-c("California","New York", "Pennsylvania", "Nebraska", "Massachusetts", "Vermont","Texas",
          "Michigan", "Maryland", "Florida", "Washington", "Oregon", "Nevada", "Minnesota", "Arizona",
          "Wisconsin", "Virginia", "Utah", "Ohio", "North Carolina", "New Jersey", "New Hampshire", 
          "Maine", "Louisiana", "Indiana", "Hawaii", "Alabama", "Tennessee", "Oklahoma", "New Mexico",
          "Missouri", "Mississippi", "Iowa", "Delaware", "Colorado", "Illinois")

prov<-c("Ontario", "Quebec", "British Columbia", "Alberta", "Nova Scotia", "New Brunswick", 
        "Newfoundland and Labrador", "Manitoba", "Saskatchewan", "Prince Edward Island", "Yukon Territory", 
        "Nunavut")

survey.what$Country<-as.character(survey.what$Location)
survey.what$Country<-ifelse(survey.what$Country%in%states, 'USA', survey.what$Country)
survey.what$Country<-ifelse(survey.what$Country%in%prov, 'Canada', survey.what$Country)
survey.what$Country<-as.factor(survey.what$Country)

###saved


##############
#####Part1####
##############

## read in non-subsetted data again
survey<-read.csv("data/gya-without-incomplete.csv", header=TRUE)



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

survey.type<-subset(survey, select=c("Location","Country",  "gender", "percent_fundemental_research_current", "percent_Applied_Research_current", 
                                "percent_Use_inspired_Research_current"))





fundemental<-aggregate(Location ~ percent_fundemental_research_current, survey.type, length)
applied<-aggregate(Location ~ percent_Applied_Research_current, survey.type, length)
use<-aggregate(Location ~ percent_Use_inspired_Research_current, survey.type, length)
## everything's fixed. % are gone.

### now we are going to standardise the % for each survey
## 1. if every category is blank, turn to NA
head(survey.type, 20)

survey$percent_fundemental_research_current[survey.type$percent_fundemental_research_current=="" & 
                                              survey.type$percent_Applied_Research_current=="" & survey.type$percent_Use_inspired_Research_current==""]<-NA
survey$percent_Applied_Research_current[is.na(survey.type$percent_fundemental_research_current) & 
                                          
                                          survey.type$percent_Applied_Research_current=="" & survey.type$percent_Use_inspired_Research_current==""]<-NA
survey$percent_Use_inspired_Research_current[is.na(survey.type$percent_fundemental_research_current) &
              is.na(survey.type$percent_Applied_Research_current) & survey.type$percent_Use_inspired_Research_current==""]<-NA
  
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
survey<-read.csv("data/Jul 18 2016 1149am - Hamilton.csv", header=TRUE)
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


#### how do you view this cahnge in the type of research?
survey<-read.csv(file="data/gya-without-incomplete.csv")
colnames(survey)
part1.view<-subset(survey, select=c("Location","Country",  "gender", "view_change_of_type"))






#############
####Part4####
#############

#read in non subsetted data again
survey<-read.csv("data/gya-without-incomplete.csv", header=TRUE)
### keep complete data only
survey<-survey[survey$Status=="Complete",]

colnames(survey)

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


survey.part4<-subset(survey, select=c("Location","Country", "gender","opinion_fundamental_important",  
                               "high_priority_fundamental", "high_priority_use_inspired", "high_priority_applied", 
                               "high_priority_no_change", "available_funding_fundamental", "available_funding_use_inspired", "available_funding_applied",
                               "next_generation"))

#######################
#######Part 2##########
#######################

#read in unsubsetted data again
survey<-read.csv("data/gya-without-incomplete.csv", header=TRUE)
part2<-subset(survey, select=c("Country","Country_work", "gender", "Location", "partnership_outside", "partnership_change_10yrs",
                               "partnership_outside_before", "reason_partnership_change_interest", "reason_partnership_change_career", 
                               "reason_partnership_change_socially",  "reason_partnership_change_funding", "reason_partnership_change_other",
                               "view_change_partnership"))

#### level of partnership that your reserach currently has outside of academia before and after
part2.b.a<-subset(part2, select=c("Country", "gender", "Location", "partnership_outside","partnership_outside_before"))
#remove non responses
part2.b.a<-part2.b.a[!part2.b.a$partnership_outside=="",]
part2.b.a<-part2.b.a[!part2.b.a$partnership_outside_before=="",]

####Level of partnership change in past 10 yrs
part2.change<-subset(part2, select=c("Country","Country_work", "gender", "Location","partnership_change_10yrs"))
#remove non responses
part2.change<-part2.change[!part2.change$partnership_change_10yrs=="",]

####Reason for change
part2.reason<-subset(part2, select=c("Country", "gender", "Location","reason_partnership_change_interest", "reason_partnership_change_career", 
                                     "reason_partnership_change_socially",  "reason_partnership_change_funding", "reason_partnership_change_other"))

#### View of Change
part2.view<-subset(part2, select=c("Country", "gender", "Location","view_change_partnership"))

#remove non responses
part2.view<-part2.view[!part2.view$view_change_partnership=="",]


##################
##### Part 3 #####
##################

#read in non subsetted data again
survey<-read.csv("data/gya-without-incomplete.csv", header=TRUE)
#subset data needed for this part
part3<-subset(survey, select=c("Country","Country_work", "gender", "Location","external_pi_grant_11_15_fundamental" , "external_pi_grant_11_15_use" ,              
                                "external_pi_grant_11_15_applied",            "external_pi_grant_6_10_fundamental",        
                               "external_pi_grant_6_10_use"     ,            "external_pi_grant_6_10_applied"   ,         
                               "successful_grants_11_15_fundamental"  ,      "successful_grants_11_15_use"     ,          
                               "successful_grants_11_15_applied",            "successful_grants_6_10_fundamental"   ,     
                               "successful_grants_6_10_use"  ,               "successful_grants_6_10_applied"   ,         
                              "practical_applications_important_11_15"  ,   "practical_applications_important_6_10" ,    
                                "include_nonacademia_partners_success_11_15", "include_nonacademia_partners_success_6_10" ,
                                "distribution_funding_11_15_internal" ,       "distriution_funding_11_15_government" ,     
                                "distriution_funding_11_15_for_profit",       "distriution_funding_11_15_nongov" ,         
                                "distriution_funding_11_15_other"   ,         "distriution_funding_11_15_other_text",      
                                "distriution_funding_6_10_internal" ,         "distriution_funding_6_10_government" ,      
                               "distriution_funding_6_10_for_profit" ,       "distriution_funding_6_10_nongov"  ,         
                               "distriution_funding_6_10_other"   ,          "distriution_funding_6_10_other_text" ,      
                                "success_change_10yrs_fundamental"  ,        "success_change_10yrs_use"   ,               
                               "success_change_10yrs_applied"))


## number of grant applications 
part3.grants<-subset(part3, select = c("Country","Country_work", "gender", "Location","external_pi_grant_11_15_fundamental" , "external_pi_grant_11_15_use" ,              
                                          "external_pi_grant_11_15_applied", "external_pi_grant_6_10_fundamental",        
                                       "external_pi_grant_6_10_use"     ,            "external_pi_grant_6_10_applied"))
##clean data
#get ranges to not be dates
part3.grants<-data.frame(lapply(part3.grants, function(x) {  gsub("6-Apr", "4-6", x)  }))
part3.grants<-data.frame(lapply(part3.grants, function(x) {  gsub("3-Jan", "1-3", x)  }))
part3.grants<-data.frame(lapply(part3.grants, function(x) {  gsub("9-Jul", "7-9", x)  }))
part3.grants<-data.frame(lapply(part3.grants, function(x) {  gsub("12-Oct", "10-12", x)  }))

unique(part3.grants$external_pi_grant_6_10_use)

# fill blanks with 0 for people who were too lazy to click 0
part3.grants$external_pi_grant_11_15_fundamental[part3.grants$external_pi_grant_11_15_fundamental==""]<-0
part3.grants$external_pi_grant_11_15_use[part3.grants$external_pi_grant_11_15_use==""]<-0
part3.grants$external_pi_grant_11_15_applied[part3.grants$external_pi_grant_11_15_applied==""]<-0
part3.grants$external_pi_grant_6_10_fundamental[part3.grants$external_pi_grant_6_10_fundamental==""]<-0
part3.grants$external_pi_grant_6_10_use[part3.grants$external_pi_grant_6_10_use==""]<-0
part3.grants$external_pi_grant_6_10_applied[part3.grants$external_pi_grant_6_10_applied==""]<-0

head(part3.grants)

#change to long format
require(tidyr)
part3.grants.long<-gather(part3.grants, type.grant, number, -Location, -gender, -Country, -Country_work)
head(part3.grants.long)











## percentage of successful grants

part3.success<-subset(part3, select = c("Country", "gender", "Location","successful_grants_11_15_fundamental"  ,      "successful_grants_11_15_use"     ,          
                                        "successful_grants_11_15_applied",            "successful_grants_6_10_fundamental"   ,     
                                        "successful_grants_6_10_use"  ,               "successful_grants_6_10_applied" ))

head(part3.success)





## important to suggest practical applications

part3.prac.app<-subset(part3, select = c("Country", "gender", "Location","practical_applications_important_11_15"  ,   "practical_applications_important_6_10" ))










##important to include partners from for profit or non gov sectors
part3.part<-subset(part3, select = c("Country", "gender", "Location","include_nonacademia_partners_success_11_15", "include_nonacademia_partners_success_6_10" ))













## distribution of funding
part3.funding<-subset(part3, select = c("Country", "gender", "Location","distribution_funding_11_15_internal" ,       "distriution_funding_11_15_government" ,     
                                        "distriution_funding_11_15_for_profit",       "distriution_funding_11_15_nongov" ,         
                                        "distriution_funding_11_15_other"   ,      
                                        "distriution_funding_6_10_internal" ,         "distriution_funding_6_10_government" ,      
                                        "distriution_funding_6_10_for_profit" ,       "distriution_funding_6_10_nongov"  ,         
                                        "distriution_funding_6_10_other"))






## grant success rates change over past 10 yrs
part3.change<-subset(part3, select = c("Country", "gender", "Location", "success_change_10yrs_fundamental"  ,        "success_change_10yrs_use"   ,               
                                       "success_change_10yrs_applied"))
head(part3.change)










############ save file as csv#############################

write.csv(survey.what, file="data/gya-country-responses.csv")

write.csv(survey.long, file="data/gya-surveys-cleaned-research.csv")

write.csv(survey.long.past, file="data/gya-surveys-cleaned-research-past.csv")

write.csv(survey.change, file="data/gya-change-reason.csv")

write.csv(part1.view, file="data/gya-part1.view.csv", row.names = FALSE)

write.csv(survey.part4, file="data/gya-survey-part4.csv")

write.csv(part2.b.a, file="data/gya-part2.before.after.csv")

write.csv(part2.change, file="data/gya-part2.change.csv")

write.csv(part2.reason, file="data/gya-part2.reason.csv", row.names=FALSE)

write.csv(part2.view, file="data/gya-part2.view.csv", row.names = FALSE)

write.csv(part3.grants.long, file="data/gya-part3.grants.long.csv", row.names = FALSE)

write.csv(part3.change, file="data/gya-part3.change.csv", row.names = FALSE)

