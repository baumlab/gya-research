

## Read in and explore the GYA data

setwd("/Users/jpwrobinson/Documents/git_repos/gya-research")

setwd("/Users/kristinatietjen/Documents/git_hub/gya-research")



## read data

survey.all<-read.csv("data/Survey-Responses-Oct.3.850pm-Toronto.csv", header=TRUE)


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

#########     #################     ###############
####Not all states are listed above #######
#########     #################     ###############

prov<-c("Ontario", "Quebec", "British Columbia", "Alberta", "Nova Scotia", "New Brunswick", 
        "Newfoundland and Labrador", "Manitoba", "Saskatchewan", "Prince Edward Island", "Yukon Territory", 
        "Nunavut")

survey$Country<-as.character(survey$Location)
survey$Country<-ifelse(survey$Country%in%states, 'USA', survey$Country)
survey$Country<-ifelse(survey$Country%in%prov, 'Canada', survey$Country)
survey$Country<-as.factor(survey$Country)

write.csv(survey, file="data/gya-without-incomplete.csv", row.names=FALSE)

##Create a csv of just canadian responses for Megan
Canada<-survey[survey$Country_work=="Canada" | (!(survey$Country_work=="Canada") & 
                                                  survey$Country_work=="" & survey$Country=="Canada"),]
head(Canada)
write.csv(Canada, file="data/gya-only-Canada.csv", row.names = FALSE)


#how many of each 
colnames(survey)
Canada<-survey[survey$Country_work=="Canada" | (!(survey$Country_work=="Canada") & 
                                                 survey$Country_work=="" & survey$Country=="Canada"),]
gender<-table(Canada$gender)
gender

dicipline<-table(Canada$field_research)
dicipline

career.stage<-table(Canada$what_participant_group)
career.stage

Canada$what_participant_group<-revalue(Canada$what_participant_group, c("Senior academic researcher with >10 years of experience applying for research grants"="Senior academic >10 yrs",
                                                                        'Non-academic researcher conducting or managing research in industry or government with >10 years of experience'='Non-academic >10yrs', 
                                                                        'Early career academic researcher with <10 years experience applying for research grants since completion of PhD' = 'Early academic <10yrs',
                                                                        'Postdoctoral fellow or research assistant with experience applying for research grants, or anticipating the need to apply for grants in the near future'="Post doc",
                                                                        'Non-academic researcher conducting or managing research in industry or government with <10 years of experience'='Non-academic <10yrs'))

g.careerstage<-table(Canada$gender, Canada$what_participant_group)
g.careerstage

g.dicipline<-table(Canada$gender, Canada$field_research)
g.dicipline

dicipline.careerstage<-table(Canada$what_participant_group, Canada$field_research)
dicipline.careerstage

current<-subset(Canada, select=c("percent_Applied_Research_current", "percent_Use_inspired_Research_current", "percent_fundemental_research_current"))
current.res<-current[!(current$percent_Applied_Research_current=="")& (current$percent_Use_inspired_Research_current=="") &(current$percent_fundemental_research_current==""),]
current.res<-droplevels(current.res)
head(current.res$percent_Applied_Research_current)
length(current.res$percent_Applied_Research_current)

distribution<-subset(Canada, select=c("distribution_funding_11_15_internal" ,       "distriution_funding_11_15_government"    ,  
                                     "distriution_funding_11_15_for_profit"  ,     "distriution_funding_11_15_nongov"     ,      "distriution_funding_11_15_other"  ,         
                                "distriution_funding_6_10_internal"  ,        "distriution_funding_6_10_government"    ,   
                                    "distriution_funding_6_10_for_profit"   ,     "distriution_funding_6_10_nongov"   ,         "distriution_funding_6_10_other"))
head(distribution$distriution_funding_6_10_government)
distribution<-distribution[!(distribution$distriution_funding_6_10_government=="New researcher (no funding in these years)"),]
distribution<-droplevels(distribution)
head(distribution$distriution_funding_6_10_government)
length(distribution$distriution_funding_6_10_government)


#############
####Part5####
#############
canada<-subset(survey, Country=="Canada")
temp<-survey[survey$Country_work=="Canada" | (!(survey$Country_work=="Canada") & survey$Country_work=="" & survey$Country=="Canada"),]
dim(canada); dim(temp)
table(temp$field_research)

ca<-subset(canada, select = c("Location", "Country", "Country_work") )
survey.what<-subset(survey, select=c("Location", "Country", "gender", "field_research", "Country_work", "PhD_Year", "what_participant_group"))


###saved


##############
#####Part1####
##############

## read in non-subsetted data again
survey<-read.csv("data/gya-without-incomplete.csv", header=TRUE)

part1<-subset(survey, select = c("Location", "Country", "Country_work", "gender","what_participant_group"  , "field_research",   "percent_fundemental_research_current"  ,     "percent_Use_inspired_Research_current"   ,  
                                  "percent_Applied_Research_current"   ,        "changed_10yrs"        ,                      "percent_Fundamental_Research_past"         ,
                                  "percent_Use_inspired_Research_past"  ,       "percent_Applied_Research_past"    ,          "Main_reason_change_interest_related" ,      
                                 "Main_reason_change_Career_related" ,         "Main_reason_change_Funding_related" ,        "Main_reason_change_Socially_related" ,      
                                  "Main_reason_change_Other"  ,                 "Main_reason_change_Other_text"   ,           "view_change_of_type"))

## 1. change all relevant variables to characters.......
part1$percent_Applied_Research_past<-as.character(part1$percent_Applied_Research_past)
part1$percent_Applied_Research_current<-as.character(part1$percent_Applied_Research_current)
part1$percent_fundemental_research_current<-as.character(part1$percent_fundemental_research_current)
part1$percent_Fundamental_Research_past<-as.character(part1$percent_Fundamental_Research_past)
part1$percent_Use_inspired_Research_current<-as.character(part1$percent_Use_inspired_Research_current)
part1$percent_Use_inspired_Research_past<-as.character(part1$percent_Use_inspired_Research_past)

require(stringr)
## remove all % symbols 

part1$percent_Applied_Research_current<-str_replace_all(part1$percent_Applied_Research_current, "[%]", "")
part1$percent_Use_inspired_Research_current<-str_replace_all(part1$percent_Use_inspired_Research_current, "[%]", "")
part1$percent_fundemental_research_current<-str_replace_all(part1$percent_fundemental_research_current, "[%]", "")
part1$percent_Applied_Research_past<-str_replace_all(part1$percent_Applied_Research_past, "[%]", "")
part1$percent_Fundamental_Research_past<-str_replace_all(part1$percent_Fundamental_Research_past, "[%]", "")
part1$percent_Use_inspired_Research_past<-str_replace_all(part1$percent_Use_inspired_Research_past, "[%]", "")

survey.type<-subset(part1, select = c("Location","Country","Country_work",  "gender","what_participant_group"  , "field_research","percent_fundemental_research_current"  ,     "percent_Use_inspired_Research_current"   ,  
                                      "percent_Applied_Research_current"   ,      "percent_Fundamental_Research_past"         ,
                                      "percent_Use_inspired_Research_past"  ,       "percent_Applied_Research_past"))

#survey.type<-subset(part1, select=c("Location","Country","Country_work",  "gender", "percent_fundemental_research_current", "percent_Applied_Research_current", "percent_Use_inspired_Research_current"))

#fundemental<-aggregate(Location ~ percent_fundemental_research_current, survey.type, length)
#applied<-aggregate(Location ~ percent_Applied_Research_current, survey.type, length)
#use<-aggregate(Location ~ percent_Use_inspired_Research_current, survey.type, length)
## everything's fixed. % are gone.

### now we are going to standardise the % for each survey
## 1. if every category is blank, turn to NA
survey.type$percent_fundemental_research_current[survey.type$percent_fundemental_research_current=="" & 
                                              survey.type$percent_Applied_Research_current=="" & survey.type$percent_Use_inspired_Research_current==""]<-NA
survey.type$percent_Applied_Research_current[is.na(survey.type$percent_fundemental_research_current) & 
                                          survey.type$percent_Applied_Research_current=="" & survey.type$percent_Use_inspired_Research_current==""]<-NA
survey.type$percent_Use_inspired_Research_current[is.na(survey.type$percent_fundemental_research_current) &
              is.na(survey.type$percent_Applied_Research_current) & survey.type$percent_Use_inspired_Research_current==""]<-NA
survey.type$percent_Fundamental_Research_past[survey.type$percent_Fundamental_Research_past=="" & 
                                               survey.type$percent_Applied_Research_past=="" & survey.type$percent_Use_inspired_Research_past==""]<-NA
survey.type$percent_Applied_Research_past[is.na(survey.type$percent_Fundamental_Research_past) & 
                                           survey.type$percent_Applied_Research_past=="" & survey.type$percent_Use_inspired_Research_past==""]<-NA
survey.type$percent_Use_inspired_Research_past[is.na(survey.type$percent_Fundamental_Research_past) &
                                                is.na(survey.type$percent_Applied_Research_past) & survey.type$percent_Use_inspired_Research_past==""]<-NA

## 2. categories with answers and blanks, turn blanks to 0
survey.type$percent_fundemental_research_current[survey.type$percent_fundemental_research_current==""]<-0
survey.type$percent_Applied_Research_current[survey.type$percent_Applied_Research_current==""]<-0
survey.type$percent_Use_inspired_Research_current[survey.type$percent_Use_inspired_Research_current==""]<-0

survey.type$percent_Fundamental_Research_past[survey.type$percent_Fundamental_Research_past==""]<-0
survey.type$percent_Applied_Research_past[survey.type$percent_Applied_Research_past==""]<-0
survey.type$percent_Use_inspired_Research_past[survey.type$percent_Use_inspired_Research_past==""]<-0

survey.type<-survey.type[!is.na(survey.type$percent_Fundamental_Research_past),]
survey.type<-survey.type[!(survey.type$percent_Fundamental_Research_past==0 & survey.type$percent_Applied_Research_past==0 & survey.type$percent_Use_inspired_Research_past==0),]

survey.type<-survey.type[!is.na(survey.type$percent_fundemental_research_current),]
survey.type<-survey.type[!(survey.type$percent_fundemental_research_current==0 & survey.type$percent_Applied_Research_current==0 & survey.type$percent_Use_inspired_Research_current==0),]

## 3. Standardise % values > 100 in total

survey.type$percent_Applied_Research_current<-as.numeric(survey.type$percent_Applied_Research_current)
survey.type$percent_fundemental_research_current<-as.numeric(survey.type$percent_fundemental_research_current)
survey.type$percent_Use_inspired_Research_current<-as.numeric(survey.type$percent_Use_inspired_Research_current)
colnames(survey.type)
survey.type$total_research<-rowSums(survey.type[,7:9])

head(survey.type[survey.type$total_research>100,])

survey.type$percent_Applied_Research_past<-as.numeric(survey.type$percent_Applied_Research_past)
survey.type$percent_Fundamental_Research_past<-as.numeric(survey.type$percent_Fundamental_Research_past)
survey.type$percent_Use_inspired_Research_past<-as.numeric(survey.type$percent_Use_inspired_Research_past)
colnames(survey.type)
survey.type$total_research<-rowSums(survey.type[,10:12])

head(survey.type[survey.type$total_research>100,])
## need to change 64 surveys that are over 100%
survey.type$percent_Applied_Research_current<-ifelse(survey.type$total_research>100,(survey.type$percent_Applied_Research_current/survey.type$total_research)*100, survey.type$percent_Applied_Research_current)
survey.type$percent_fundemental_research_current<-ifelse(survey.type$total_research>100,(survey.type$percent_fundemental_research_current/survey.type$total_research)*100, survey.type$percent_fundemental_research_current)
survey.type$percent_Use_inspired_Research_current<-ifelse(survey.type$total_research>100,(survey.type$percent_Use_inspired_Research_current/survey.type$total_research)*100, survey.type$percent_Use_inspired_Research_current)

survey.type$percent_Applied_Research_past<-ifelse(survey.type$total_research>100,(survey.type$percent_Applied_Research_past/survey.type$total_research)*100, survey.type$percent_Applied_Research_past)
survey.type$percent_Fundamental_Research_past<-ifelse(survey.type$total_research>100,(survey.type$percent_Fundamental_Research_past/survey.type$total_research)*100, survey.type$percent_Fundamental_Research_past)
survey.type$percent_Use_inspired_Research_past<-ifelse(survey.type$total_research>100,(survey.type$percent_Use_inspired_Research_past/survey.type$total_research)*100, survey.type$percent_Use_inspired_Research_past)

head(survey.type[survey.type$total_research>100,])

dim(survey.type)
## switch to long format
require(tidyr)
survey.long<-gather(survey.type, type, percent, -Location, -gender, -Country_work, -Country, -what_participant_group, -field_research, -total_research)
tail(survey.long)

write.csv(survey.long, file="data/4.1a.nubmer.calculations.csv", row.names=FALSE)

#######################################
#####do same as above but for past data

## read in non-subsetted data again
#survey<-read.csv("data/gya-without-incomplete.csv", header=TRUE)

## 1. change all relevant variables to characters.......
#survey$percent_Applied_Research_past<-as.character(survey$percent_Applied_Research_past)
#survey$percent_Applied_Research_current<-as.character(survey$percent_Applied_Research_current)
#survey$percent_fundemental_research_current<-as.character(survey$percent_fundemental_research_current)
#survey$percent_Fundamental_Research_past<-as.character(survey$percent_Fundamental_Research_past)
#survey$percent_Use_inspired_Research_current<-as.character(survey$percent_Use_inspired_Research_current)
#survey$percent_Use_inspired_Research_past<-as.character(survey$percent_Use_inspired_Research_past)

#require(stringr)
## remove all % symbols 
#survey$percent_Applied_Research_past<-str_replace_all(survey$percent_Applied_Research_past, "[%]", "")
#survey$percent_Applied_Research_current<-str_replace_all(survey$percent_Applied_Research_current, "[%]", "")
#survey$percent_Use_inspired_Research_current<-str_replace_all(survey$percent_Use_inspired_Research_current, "[%]", "")
#survey$percent_fundemental_research_current<-str_replace_all(survey$percent_fundemental_research_current, "[%]", "")
#survey$percent_Fundamental_Research_past<-str_replace_all(survey$percent_Fundamental_Research_past, "[%]", "")
#survey$percent_Use_inspired_Research_past<-str_replace_all(survey$percent_Use_inspired_Research_past, "[%]", "")

#part1.past<-subset(survey, select=c("Location","Country",  "gender", "percent_Applied_Research_past", "percent_Fundamental_Research_past", 
#                                "percent_Use_inspired_Research_past"))

## everything's fixed. % are gone.

### now we are going to standardise the % for each survey
## 1. if every category is blank, turn to NA
#part1.past$percent_Fundamental_Research_past[part1.past$percent_Fundamental_Research_past=="" & 
#                                             part1.past$percent_Applied_Research_past=="" & part1.past$percent_Use_inspired_Research_past==""]<-NA
#part1.past$percent_Applied_Research_past[is.na(part1.past$percent_Fundamental_Research_past) & 
#                                          part1.past$percent_Applied_Research_past=="" & part1.past$percent_Use_inspired_Research_past==""]<-NA
#part1.past$percent_Use_inspired_Research_past[is.na(part1.past$percent_Fundamental_Research_past) &
#                                               is.na(part1.past$percent_Applied_Research_past) & part1.past$percent_Use_inspired_Research_past==""]<-NA

## 2. categories with answers and blanks, turn blanks to 0
#part1.past$percent_Fundamental_Research_past[part1.past$percent_Fundamental_Research_past==""]<-0
#part1.past$percent_Applied_Research_past[part1.past$percent_Applied_Research_past==""]<-0
#part1.past$percent_Use_inspired_Research_past[part1.past$percent_Use_inspired_Research_past==""]<-0

#part1.past<-part1.past[!is.na(part1.past$percent_Fundamental_Research_past),]
#part1.past<-part1.past[!(part1.past$percent_Fundamental_Research_past==0 & part1.past$percent_Applied_Research_past==0 & part1.past$percent_Use_inspired_Research_past==0),]

## 3. Standardise % values > 100 in total
#part1.past$percent_Applied_Research_past<-as.numeric(part1.past$percent_Applied_Research_past)
#part1.past$percent_Fundamental_Research_past<-as.numeric(part1.past$percent_Fundamental_Research_past)
#part1.past$percent_Use_inspired_Research_past<-as.numeric(part1.past$percent_Use_inspired_Research_past)
#part1.past$total_research<-rowSums(part1.past[,4:6])

#head(part1.past[part1.past$total_research>100,])
## need to change 64 surveys that are over 100%
#part1.past$percent_Applied_Research_past<-ifelse(part1.past$total_research>100,(part1.past$percent_Applied_Research_past/part1.past$total_research)*100, part1.past$percent_Applied_Research_past)
#part1.past$percent_Fundamental_Research_past<-ifelse(part1.past$total_research>100,(part1.past$percent_Fundamental_Research_past/part1.past$total_research)*100, part1.past$percent_Fundamental_Research_past)
#part1.past$percent_Use_inspired_Research_past<-ifelse(part1.past$total_research>100,(part1.past$percent_Use_inspired_Research_past/part1.past$total_research)*100, part1.past$percent_Use_inspired_Research_past)

#head(part1.past[part1.past$total_research>100,])

## switch to long format
#require(tidyr)
#survey.long.past<-gather(part1.past, type_past, percent_past, -Location, -gender, -Country)


#### yes/no have the portions changed and why

## read in non-subsetted data again
survey<-read.csv("data/gya-without-incomplete.csv", header=TRUE)

survey.change<-subset(survey, select=c("Location","Country","Country_work",  "gender","what_participant_group"  , "field_research", "changed_10yrs", "Main_reason_change_interest_related", 
                                "Main_reason_change_Career_related", "Main_reason_change_Funding_related","Main_reason_change_Socially_related",
                                "Main_reason_change_Other"))


#### how do you view this cahnge in the type of research?
survey<-read.csv(file="data/gya-without-incomplete.csv")

part1.view<-subset(survey, select=c("Location","Country","Country_work",  "gender","what_participant_group"  , "field_research", "view_change_of_type"))






#############
####Part4####
#############

#read in non subsetted data again
survey<-read.csv("data/gya-without-incomplete.csv", header=TRUE)

survey.part4<-subset(survey, select=c("Location","Country","Country_work", "gender","what_participant_group"  , "field_research","opinion_fundamental_important",  
                               "high_priority_fundamental", "high_priority_use_inspired", "high_priority_applied", 
                               "high_priority_no_change", "available_funding_fundamental", "available_funding_use_inspired", "available_funding_applied",
                               "next_generation"))

#######################
#######Part 2##########
#######################

#read in unsubsetted data again
survey<-read.csv("data/gya-without-incomplete.csv", header=TRUE)
part2<-subset(survey, select=c("Country","Country_work", "gender","what_participant_group"  , "field_research", "Location", "partnership_outside", "partnership_change_10yrs",
                               "partnership_outside_before", "reason_partnership_change_interest", "reason_partnership_change_career", 
                               "reason_partnership_change_socially",  "reason_partnership_change_funding", "reason_partnership_change_other",
                               "view_change_partnership"))

#### level of partnership that your reserach currently has outside of academia before and after
part2.b.a<-subset(part2, select=c("Country","Country_work", "gender","what_participant_group"  , "field_research", "Location", "partnership_outside","partnership_outside_before"))
#remove non responses
part2.b.a<-part2.b.a[!part2.b.a$partnership_outside=="",]
part2.b.a<-part2.b.a[!part2.b.a$partnership_outside_before=="",]

####Level of partnership change in past 10 yrs
part2.change<-subset(part2, select=c("Country","Country_work", "gender","what_participant_group"  , "field_research", "Location","partnership_change_10yrs"))
#remove non responses
part2.change<-part2.change[!part2.change$partnership_change_10yrs=="",]

####Reason for change
part2.reason<-subset(part2, select=c("Country","Country_work", "gender","what_participant_group", "field_research", "Location","reason_partnership_change_interest", "reason_partnership_change_career", 
                                     "reason_partnership_change_socially",  "reason_partnership_change_funding", "reason_partnership_change_other"))

#### View of Change
part2.view<-subset(part2, select=c("Country","Country_work", "gender","what_participant_group", "field_research", "Location","view_change_partnership"))

#remove non responses
part2.view<-part2.view[!part2.view$view_change_partnership=="",]


##################
##### Part 3 #####
##################

#read in non subsetted data again
survey<-read.csv("data/gya-without-incomplete.csv", header=TRUE)
#subset data needed for this part
part3<-subset(survey, select=c("Country","Country_work", "gender","what_participant_group", "field_research", "Location","external_pi_grant_11_15_fundamental" , "external_pi_grant_11_15_use" ,              
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
part3.grants<-subset(part3, select = c("Country","Country_work", "gender", "Location","what_participant_group", "field_research","external_pi_grant_11_15_fundamental" , "external_pi_grant_11_15_use" ,              
                                          "external_pi_grant_11_15_applied", "external_pi_grant_6_10_fundamental",        
                                       "external_pi_grant_6_10_use"     ,            "external_pi_grant_6_10_applied"))
##clean data
#get ranges to not be dates
part3.grants<-data.frame(lapply(part3.grants, function(x) {  gsub("6-Apr", "4-6", x)  }))
part3.grants<-data.frame(lapply(part3.grants, function(x) {  gsub("3-Jan", "1-3", x)  }))
part3.grants<-data.frame(lapply(part3.grants, function(x) {  gsub("9-Jul", "7-9", x)  }))
part3.grants<-data.frame(lapply(part3.grants, function(x) {  gsub("12-Oct", "10-12", x)  }))

# fill blanks with 0 for people who were too lazy to click 0
part3.grants$external_pi_grant_11_15_fundamental[part3.grants$external_pi_grant_11_15_fundamental==""]<-0
part3.grants$external_pi_grant_11_15_use[part3.grants$external_pi_grant_11_15_use==""]<-0
part3.grants$external_pi_grant_11_15_applied[part3.grants$external_pi_grant_11_15_applied==""]<-0
part3.grants$external_pi_grant_6_10_fundamental[part3.grants$external_pi_grant_6_10_fundamental==""]<-0
part3.grants$external_pi_grant_6_10_use[part3.grants$external_pi_grant_6_10_use==""]<-0
part3.grants$external_pi_grant_6_10_applied[part3.grants$external_pi_grant_6_10_applied==""]<-0

head(part3.grants)

#saved

#change to long format
require(tidyr)
part3.grants.long<-gather(part3.grants, type.grant, number, -Location, -gender, -Country, -Country_work, -what_participant_group, -field_research)
head(part3.grants.long)

#saved


## percentage of successful grants

part3.success<-subset(part3, select = c("Country","Country_work", "gender", "Location","what_participant_group", "field_research","successful_grants_11_15_fundamental"  ,      "successful_grants_11_15_use"     ,          
                                        "successful_grants_11_15_applied",            "successful_grants_6_10_fundamental"   ,     
                                        "successful_grants_6_10_use"  ,               "successful_grants_6_10_applied" ))

#remove non responses
part3.success<-part3.success[!(part3.success$successful_grants_11_15_fundamental=="" & part3.success$successful_grants_11_15_use=="" & part3.success$successful_grants_11_15_applied=="" &
                               part3.success$successful_grants_6_10_fundamental=="" &part3.success$successful_grants_6_10_use=="" &part3.success$successful_grants_6_10_applied==""),]
#remove %
part3.success$successful_grants_11_15_fundamental<-str_replace_all(part3.success$successful_grants_11_15_fundamental, "[%]", "")
part3.success$successful_grants_11_15_use<-str_replace_all(part3.success$successful_grants_11_15_use, "[%]", "")
part3.success$successful_grants_11_15_applied<-str_replace_all(part3.success$successful_grants_11_15_applied, "[%]", "")
part3.success$successful_grants_6_10_fundamental<-str_replace_all(part3.success$successful_grants_6_10_fundamental, "[%]", "")
part3.success$successful_grants_6_10_use<-str_replace_all(part3.success$successful_grants_6_10_use, "[%]", "")
part3.success$successful_grants_6_10_applied<-str_replace_all(part3.success$successful_grants_6_10_applied, "[%]", "")

#change to long form
require(tidyr)
part3.success.long<-gather(part3.success, type, percent, -Location,-Country_work, -gender, -Country, -what_participant_group, -field_research)

#remove blanks and "no need for aplications for this research type"
part3.success.long<-part3.success.long[!(part3.success.long$percent==""),]
part3.success.long<-part3.success.long[!(part3.success.long$percent=="No need for applications for this research type"),]
#saved



## important to suggest practical applications

part3.prac.app<-subset(part3, select = c("Country","Country_work", "gender", "Location","what_participant_group", "field_research","practical_applications_important_11_15"  ,   "practical_applications_important_6_10" ))
head(part3.prac.app)
#saved
colnames(part3.prac.app)
#change to long form
require(tidyr)
part3.prac.long<-gather(part3.prac.app, year, level, -Location, -gender,-Country_work, -Country, -what_participant_group, -field_research)
#remove non responses
part3.prac.long<-part3.prac.long[!(part3.prac.long$level==""),]

#saved


##important to include partners from for profit or non gov sectors
part3.part<-subset(part3, select = c("Country","Country_work", "gender", "Location","what_participant_group", "field_research","include_nonacademia_partners_success_11_15", "include_nonacademia_partners_success_6_10" ))
#saved
#change to long form
require(tidyr)
part3.part.long<-gather(part3.part, year, level, -Location, -gender,-Country_work, -Country, -what_participant_group, -field_research)

#remove non responses
part3.part.long<-part3.part.long[!(part3.part.long$level==""),]

#saved


## distribution of funding
part3.funding<-subset(part3, select = c("Country", "Country_work", "gender", "Location","what_participant_group", "field_research","distribution_funding_11_15_internal" ,       "distriution_funding_11_15_government" ,     
                                        "distriution_funding_11_15_for_profit",       "distriution_funding_11_15_nongov" ,         
                                        "distriution_funding_11_15_other"   ,      
                                        "distriution_funding_6_10_internal" ,         "distriution_funding_6_10_government" ,      
                                        "distriution_funding_6_10_for_profit" ,       "distriution_funding_6_10_nongov"  ,         
                                        "distriution_funding_6_10_other"))

## 1. change all relevant variables to characters.......
part3.funding$distribution_funding_11_15_internal<-as.character(part3.funding$distribution_funding_11_15_internal)
part3.funding$distriution_funding_11_15_government<-as.character(part3.funding$distriution_funding_11_15_government)
part3.funding$distriution_funding_11_15_for_profit<-as.character(part3.funding$distriution_funding_11_15_for_profit)
part3.funding$distriution_funding_11_15_nongov<-as.character(part3.funding$distriution_funding_11_15_nongov)
part3.funding$distriution_funding_11_15_other<-as.character(part3.funding$distriution_funding_11_15_other)
part3.funding$distriution_funding_6_10_internal<-as.character(part3.funding$distriution_funding_6_10_internal)
part3.funding$distriution_funding_6_10_government<-as.character(part3.funding$distriution_funding_6_10_government)
part3.funding$distriution_funding_6_10_for_profit<-as.character(part3.funding$distriution_funding_6_10_for_profit)
part3.funding$distriution_funding_6_10_nongov<-as.character(part3.funding$distriution_funding_6_10_nongov)
part3.funding$distriution_funding_6_10_other<-as.character(part3.funding$distriution_funding_6_10_other)

require(stringr)

## remove all % symbols
part3.funding$distribution_funding_11_15_internal<-str_replace_all(part3.funding$distribution_funding_11_15_internal, "[%]", "")
part3.funding$distriution_funding_11_15_government<-str_replace_all(part3.funding$distriution_funding_11_15_government, "[%]", "")
part3.funding$distriution_funding_11_15_for_profit<-str_replace_all(part3.funding$distriution_funding_11_15_for_profit, "[%]", "")
part3.funding$distriution_funding_11_15_nongov<-str_replace_all(part3.funding$distriution_funding_11_15_nongov, "[%]", "")
part3.funding$distriution_funding_11_15_other<-str_replace_all(part3.funding$distriution_funding_11_15_other, "[%]", "")
part3.funding$distriution_funding_6_10_internal<-str_replace_all(part3.funding$distriution_funding_6_10_internal, "[%]", "")
part3.funding$distriution_funding_6_10_government<-str_replace_all(part3.funding$distriution_funding_6_10_government, "[%]", "")
part3.funding$distriution_funding_6_10_for_profit<-str_replace_all(part3.funding$distriution_funding_6_10_for_profit, "[%]", "")
part3.funding$distriution_funding_6_10_nongov<-str_replace_all(part3.funding$distriution_funding_6_10_nongov, "[%]", "")
part3.funding$distriution_funding_6_10_other<-str_replace_all(part3.funding$distriution_funding_6_10_other, "[%]", "")

### now we are going to standardise the % for each survey

## add unique survey ID to dataframe
part3.funding$survey<-c(1:dim(part3.funding)[1])

## 1. remove responses were a person said new researcher inthe 2006-2010 section (some people selected new researcher for one category but then a percentage for another so I had to get rid more than I thought)
part3.funding<-gather(part3.funding, question, value, -Country, -gender, -Country_work, -Location, -survey, -what_participant_group, -field_research)
part3.funding$year<-ifelse(grepl("11_15", part3.funding$question), "11_15", "6_10")

part3.funding$info<-ifelse((part3.funding$year=="6_10" & part3.funding$value=="New researcher (no funding in these years)"),"remove", "keep")
survey.remove<-part3.funding$survey[part3.funding$info=="remove"]
part3.funding<-part3.funding[!(part3.funding$year=="6_10" & part3.funding$survey %in% survey.remove),]
part3.funding<-spread(part3.funding, question, value)


p3_6.10<-part3.funding[part3.funding$year=="6_10",]
p3_6.10<-p3_6.10[,c(1:9, 15:19)]
p3_11.15<-part3.funding[part3.funding$year=="11_15",]
p3_11.15<-p3_11.15[,c(1:14)]

## 2. if every category is blank, turn to NA
p3_11.15$distribution_funding_11_15_internal[p3_11.15$distribution_funding_11_15_internal=="" & 
                                                     p3_11.15$distriution_funding_11_15_government=="" & p3_11.15$distriution_funding_11_15_for_profit=="" &
                                                     p3_11.15$distriution_funding_11_15_nongov=="" & p3_11.15$distriution_funding_11_15_other==""]<-NA
p3_11.15$distriution_funding_11_15_government[is.na(p3_11.15$distribution_funding_11_15_internal) & 
                                                 p3_11.15$distriution_funding_11_15_government=="" & p3_11.15$distriution_funding_11_15_for_profit=="" &
                                                   p3_11.15$distriution_funding_11_15_nongov=="" & p3_11.15$distriution_funding_11_15_other==""]<-NA
p3_11.15$distriution_funding_11_15_for_profit[is.na(p3_11.15$distribution_funding_11_15_internal) & 
                                                     is.na(p3_11.15$distriution_funding_11_15_government) & p3_11.15$distriution_funding_11_15_for_profit=="" &
                                                     p3_11.15$distriution_funding_11_15_nongov=="" & p3_11.15$distriution_funding_11_15_other==""]<-NA
p3_11.15$distriution_funding_11_15_nongov[is.na(p3_11.15$distribution_funding_11_15_internal) & 
                                                     is.na(p3_11.15$distriution_funding_11_15_government) & is.na(p3_11.15$distriution_funding_11_15_for_profit) &
                                                     p3_11.15$distriution_funding_11_15_nongov=="" & p3_11.15$distriution_funding_11_15_other==""]<-NA
p3_11.15$distriution_funding_11_15_other[is.na(p3_11.15$distribution_funding_11_15_internal) & 
                                                 is.na(p3_11.15$distriution_funding_11_15_government) & is.na(p3_11.15$distriution_funding_11_15_for_profit) &
                                                 is.na(p3_11.15$distriution_funding_11_15_nongov) & p3_11.15$distriution_funding_11_15_other==""]<-NA
p3_6.10$distriution_funding_6_10_internal[p3_6.10$distriution_funding_6_10_internal=="" & 
                                                    p3_6.10$distriution_funding_6_10_government=="" & p3_6.10$distriution_funding_6_10_for_profit=="" &
                                                    p3_6.10$distriution_funding_6_10_nongov=="" & p3_6.10$distriution_funding_6_10_other==""]<-NA
p3_6.10$distriution_funding_6_10_government[is.na(p3_6.10$distriution_funding_6_10_internal) & 
                                                  p3_6.10$distriution_funding_6_10_government=="" & p3_6.10$distriution_funding_6_10_for_profit=="" &
                                                  p3_6.10$distriution_funding_6_10_nongov=="" & p3_6.10$distriution_funding_6_10_other==""]<-NA
p3_6.10$distriution_funding_6_10_for_profit[is.na(p3_6.10$distriution_funding_6_10_internal) & 
                                                    is.na(p3_6.10$distriution_funding_6_10_government) & p3_6.10$distriution_funding_6_10_for_profit=="" &
                                                    p3_6.10$distriution_funding_6_10_nongov=="" & p3_6.10$distriution_funding_6_10_other==""]<-NA
p3_6.10$distriution_funding_6_10_nongov[is.na(p3_6.10$distriution_funding_6_10_internal) & 
                                                    is.na(p3_6.10$distriution_funding_6_10_government) & is.na(p3_6.10$distriution_funding_6_10_for_profit) &
                                                    p3_6.10$distriution_funding_6_10_nongov=="" & p3_6.10$distriution_funding_6_10_other==""]<-NA
p3_6.10$distriution_funding_6_10_other[is.na(p3_6.10$distriution_funding_6_10_internal) & 
                                                is.na(p3_6.10$distriution_funding_6_10_government) & is.na(p3_6.10$distriution_funding_6_10_for_profit) &
                                                is.na(p3_6.10$distriution_funding_6_10_nongov) & p3_6.10$distriution_funding_6_10_other==""]<-NA

## 3. categories with answers and blanks, turn blanks to 0
p3_11.15$distribution_funding_11_15_internal[p3_11.15$distribution_funding_11_15_internal==""]<-0
p3_11.15$distriution_funding_11_15_government[p3_11.15$distriution_funding_11_15_government==""]<-0
p3_11.15$distriution_funding_11_15_for_profit[p3_11.15$distriution_funding_11_15_for_profit==""]<-0
p3_11.15$distriution_funding_11_15_nongov[p3_11.15$distriution_funding_11_15_nongov==""]<-0
p3_11.15$distriution_funding_11_15_other[p3_11.15$distriution_funding_11_15_other==""]<-0
p3_6.10$distriution_funding_6_10_internal[p3_6.10$distriution_funding_6_10_internal==""]<-0
p3_6.10$distriution_funding_6_10_government[p3_6.10$distriution_funding_6_10_government==""]<-0
p3_6.10$distriution_funding_6_10_for_profit[p3_6.10$distriution_funding_6_10_for_profit==""]<-0
p3_6.10$distriution_funding_6_10_nongov[p3_6.10$distriution_funding_6_10_nongov==""]<-0
p3_6.10$distriution_funding_6_10_other[p3_6.10$distriution_funding_6_10_other==""]<-0

# remove NAs
p3_11.15<-p3_11.15[!(is.na(p3_11.15$distribution_funding_11_15_internal)& is.na(p3_11.15$distriution_funding_11_15_government) & is.na(p3_11.15$distriution_funding_11_15_for_profit) &
                           is.na(p3_11.15$distriution_funding_11_15_nongov) & is.na(p3_11.15$distriution_funding_11_15_other)),]
p3_6.10<-p3_6.10[!(is.na(p3_6.10$distriution_funding_6_10_internal)& is.na(p3_6.10$distriution_funding_6_10_government) & is.na(p3_6.10$distriution_funding_6_10_for_profit) &
                                 is.na(p3_6.10$distriution_funding_6_10_nongov) & is.na(p3_6.10$distriution_funding_6_10_other)),]

## 4. Standardise % values > 100 in total
p3_11.15$distribution_funding_11_15_internal<-as.numeric((p3_11.15$distribution_funding_11_15_internal))
p3_11.15$distriution_funding_11_15_government<-as.numeric((p3_11.15$distriution_funding_11_15_government))
p3_11.15$distriution_funding_11_15_for_profit<-as.numeric((p3_11.15$distriution_funding_11_15_for_profit))
p3_11.15$distriution_funding_11_15_nongov<-as.numeric((p3_11.15$distriution_funding_11_15_nongov))
p3_11.15$distriution_funding_11_15_other<-as.numeric((p3_11.15$distriution_funding_11_15_other))
p3_6.10$distriution_funding_6_10_internal<-as.numeric((p3_6.10$distriution_funding_6_10_internal))
p3_6.10$distriution_funding_6_10_government<-as.numeric((p3_6.10$distriution_funding_6_10_government))
p3_6.10$distriution_funding_6_10_for_profit<-as.numeric((p3_6.10$distriution_funding_6_10_for_profit))
p3_6.10$distriution_funding_6_10_nongov<-as.numeric((p3_6.10$distriution_funding_6_10_nongov))
p3_6.10$distriution_funding_6_10_other<-as.numeric((p3_6.10$distriution_funding_6_10_other))

## need to change surveys that are over 100%
p3_11.15$total.funding<-rowSums(p3_11.15[,10:14])
p3_6.10$total.funding<-rowSums(p3_6.10[,10:14])

head(p3_11.15[p3_11.15$total.funding>100,])

p3_11.15$distribution_funding_11_15_internal<-ifelse(p3_11.15$total.funding>100,(p3_11.15$distribution_funding_11_15_internal/p3_11.15$total.funding)*100, p3_11.15$distribution_funding_11_15_internal)
p3_11.15$distriution_funding_11_15_government<-ifelse(p3_11.15$total.funding>100,(p3_11.15$distriution_funding_11_15_government/p3_11.15$total.funding)*100, p3_11.15$distriution_funding_11_15_government)
p3_11.15$distriution_funding_11_15_for_profit<-ifelse(p3_11.15$total.funding>100,(p3_11.15$distriution_funding_11_15_for_profit/p3_11.15$total.funding)*100, p3_11.15$distriution_funding_11_15_for_profit)
p3_11.15$distriution_funding_11_15_nongov<-ifelse(p3_11.15$total.funding>100,(p3_11.15$distriution_funding_11_15_nongov/p3_11.15$total.funding)*100, p3_11.15$distriution_funding_11_15_nongov)
p3_11.15$distriution_funding_11_15_other<-ifelse(p3_11.15$total.funding>100,(p3_11.15$distriution_funding_11_15_other/p3_11.15$total.funding)*100, p3_11.15$distriution_funding_11_15_other)
p3_6.10$distriution_funding_6_10_internal<-ifelse(p3_6.10$total.funding>100,(p3_6.10$distriution_funding_6_10_internal/p3_6.10$total.funding)*100, p3_6.10$distriution_funding_6_10_internal)
p3_6.10$distriution_funding_6_10_government<-ifelse(p3_6.10$total.funding>100,(p3_6.10$distriution_funding_6_10_government/p3_6.10$total.funding)*100, p3_6.10$distriution_funding_6_10_government)
p3_6.10$distriution_funding_6_10_for_profit<-ifelse(p3_6.10$total.funding>100,(p3_6.10$distriution_funding_6_10_for_profit/p3_6.10$total.funding)*100, p3_6.10$distriution_funding_6_10_for_profit)
p3_6.10$distriution_funding_6_10_nongov<-ifelse(p3_6.10$total.funding>100,(p3_6.10$distriution_funding_6_10_nongov/p3_6.10$total.funding)*100, p3_6.10$distriution_funding_6_10_nongov)
p3_6.10$distriution_funding_6_10_other<-ifelse(p3_6.10$total.funding>100,(p3_6.10$distriution_funding_6_10_other/p3_6.10$total.funding)*100, p3_6.10$distriution_funding_6_10_other)

head(p3_6.10[p3_6.10$total.funding>100,])

colnames(p3_11.15)[colnames(p3_11.15)=="distriution_funding_11_15_for_profit"]<-"For-Profit"
colnames(p3_11.15)[colnames(p3_11.15)=="distriution_funding_11_15_government"]<-"Government"
colnames(p3_11.15)[colnames(p3_11.15)=="distribution_funding_11_15_internal"]<-"Internal"
colnames(p3_11.15)[colnames(p3_11.15)=="distriution_funding_11_15_nongov"]<-"Non-governmental"
colnames(p3_11.15)[colnames(p3_11.15)=="distriution_funding_11_15_other"]<-"Other"

colnames(p3_6.10)[colnames(p3_6.10)=="distriution_funding_6_10_for_profit"]<-"For-Profit"
colnames(p3_6.10)[colnames(p3_6.10)=="distriution_funding_6_10_government"]<-"Government"
colnames(p3_6.10)[colnames(p3_6.10)=="distriution_funding_6_10_internal"]<-"Internal"
colnames(p3_6.10)[colnames(p3_6.10)=="distriution_funding_6_10_nongov"]<-"Non-governmental"
colnames(p3_6.10)[colnames(p3_6.10)=="distriution_funding_6_10_other"]<-"Other"

head(p3_6.10)

p3_master<-rbind(p3_6.10, p3_11.15)

head(p3_master)

## switch to long format
require(tidyr)
p3_master.long<-gather(p3_master, type, percent,-info, -year, -survey, -total.funding, -Location, -gender, -Country_work, -Country, -what_participant_group, -field_research)

head(p3_master.long)






## grant success rates change over past 10 yrs
part3.change<-subset(part3, select = c("Country", "Country_work", "gender", "Location","what_participant_group", "field_research", "success_change_10yrs_fundamental"  ,        "success_change_10yrs_use"   ,               
                                       "success_change_10yrs_applied"))
head(part3.change)










############ save file as csv#############################

write.csv(survey.what, file="data/gya-country-responses.csv", row.names = FALSE)

write.csv(survey.long, file="data/gya-surveys-cleaned-research.csv", row.names = FALSE)

write.csv(survey.type, file="data/gya-research-cleaned.csv", row.names = FALSE)

#write.csv(survey.long.past, file="data/gya-surveys-cleaned-research-past.csv", row.names = FALSE)

write.csv(survey.change, file="data/gya-change-reason.csv", row.names = FALSE)

write.csv(part1.view, file="data/gya-part1.view.csv", row.names = FALSE)

write.csv(survey.part4, file="data/gya-survey-part4.csv", row.names = FALSE)

write.csv(part2.b.a, file="data/gya-part2.before.after.csv", row.names = FALSE)

write.csv(part2.change, file="data/gya-part2.change.csv", row.names = FALSE)

write.csv(part2.reason, file="data/gya-part2.reason.csv", row.names=FALSE)

write.csv(part2.view, file="data/gya-part2.view.csv", row.names = FALSE)

write.csv(part3.grants.long, file="data/gya-part3.grants.long.csv", row.names = FALSE)

write.csv(part3.grants, file="data/gya-part3.grants.csv", row.names = FALSE)

write.csv(part3.change, file="data/gya-part3.change.csv", row.names = FALSE)

write.csv(part3.success.long, file="data/gya-part3.success.long.csv", row.names = FALSE)

write.csv(part3.prac.long, file="data/gya-part3.prac.long.csv", row.names = FALSE)

write.csv(part3.prac.app, file="data/gya-part3.prac.app.csv", row.names = FALSE)

write.csv(part3.part.long, file="data/gya-part3.part.long.csv", row.names = FALSE)

write.csv(part3.part, file="data/gya-part3.part.csv", row.names = FALSE)

write.csv(p3_master.long, file="data/gya-p3_master.long.csv", row.names = FALSE)

write.csv(p3_master, file="data/gya-p3_master.csv", row.names = FALSE)

