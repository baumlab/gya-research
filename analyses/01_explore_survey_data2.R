rm(list=ls())

dev.off()

## Read in and explore the GYA data

#setwd("/Users/jpwrobinson/Documents/git_repos/gya-research")

setwd("/Users/kristinatietjen/Documents/git_hub/gya-research")

require(plyr); require(stringr); require(tidyr); require(plyr)

## read data

#survey.all<-read.csv("data/Survey-Responses-Oct.3.850pm-Toronto.csv", header=TRUE)
survey.all<-read.csv("data/responses_kt_4Jan18.csv", header=TRUE)

#change col names again to shorten them even more (was changed before reading into R)

suveycols<-c("Status", "Internal_ID", "Language",	"Created_At",	"Updated_At", "Location",	"Username",	"GET_Variables",
             "Number_of_Saves",	"Weighted_Score",	"Completion_Time",	"Invite_Code",	"Invite_Email",	"Invite_Name",	"Collector", "what_participant_group",
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
  "field_research",	"PhD_Year",	"Country_work",	"gender", 'final_comments')

##change column names      

colnames(survey.all)<-suveycols

## remove some unnecessary columns
survey.all$'Username'<-NULL
#survey.all$'Updated_At'<-NULL  # needed to cut the data off at 8 Nov  well remove this col after that
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
#survey.all$'final_comments'<-NULL


## check number of complete vs. incomplete
table(survey.all$Status)    # 2918 

### keep complete data only
survey<-survey.all[survey.all$Status=="Complete",]
dim(survey)

### change Canada and USA
#########     #################     ###############

####Not all states are listed - only the ones that coorespond to responses in the survey #######
#unique(survey$location)   #check that this is the complete list every time importing new data  - also make sure that Georgia is Georgia the state and not the country
states<-c("California","New York", "Pennsylvania", "Nebraska", "Massachusetts", "Vermont","Texas",
          "Michigan", "Maryland", "Florida", "Washington", "Oregon", "Nevada", "Minnesota", "Arizona",
          "Wisconsin", "Virginia", "Utah", "Ohio", "North Carolina", "New Jersey", "New Hampshire", 
          "Maine", "Louisiana", "Indiana", "Hawaii", "Alabama", "Tennessee", "Oklahoma", "New Mexico",
          "Iowa", "Colorado", "Illinois", "Connecticut", "Georgia", "Rhode Island",
          "Kansas", "Idaho", "District of Columbia")


#does not include northwest territories as it is not in the survey  - check each time
prov<-c("Ontario", "Quebec", "British Columbia", "Alberta", "Nova Scotia", "New Brunswick", 
        "Newfoundland and Labrador", "Manitoba", "Saskatchewan", "Prince Edward Island", "Yukon Territory", 
        "Nunavut")
##create column that has the country name in it but change the US states and the CA provinces to just be USA and Canada
survey$Country<-as.character(survey$Location)
survey$Country<-ifelse(survey$Country%in%states, 'United States', survey$Country)
survey$Country<-ifelse(survey$Country%in%prov, 'Canada', survey$Country)
survey$Country<-as.factor(survey$Country)

# 
# # look at what people said their country of work was compared to where the location was
# survey.countries <- survey
# survey.countries <- survey.countries[ , c(3, 73,75)]
# head(survey.countries)
# write.csv(survey.countries, file = "data/gya-country_compare.csv", row.names = FALSE)

###<this is being coded after the Canadian report was finished>###
# create a column nation that will have what country we will use them for following this protocol:
# use what the put in the Country_work column
# if that doesnt make sense (i.e. antartica)
# then use what is in Country column (country is from the location column but updated to have USA and Canada instead of individual state and province names)
# if that still doesnt make sense or there isnt a value there then put in 'other' category


# Assign the new variable nation all values from Country_work.
survey$nation <- as.character(survey$Country_work)

#check
nation <- as.data.frame(survey)
nation <- nation[ , c(3, 73,76, 77)]

# If there was a blank then use the information from the Country column that derived from the IP address column
survey$nation <- ifelse(survey$nation == "", as.character(survey$Country), survey$nation)

#check
nation <- as.data.frame(survey)
nation <- nation[ , c(3, 73,76, 77)]

#now to fix the ones that are obviously not correct - start with antarctica - no one recieves funding from there
survey$nation <- ifelse(survey$nation == "Antarctica", as.character(survey$Country), survey$nation)

#check
nation <- as.data.frame(survey)
nation <- nation[ , c(3, 73,76, 77)]

#now make a csv so we can go through and find more 'wrong' answers
write.csv(nation, file = "data/gya-nations_look.csv", row.names = FALSE)

#now to fix the ones that are obviously not correct - the rest
survey$nation <- ifelse((survey$nation == "Bangladesh" & survey$Country == "Japan"), as.character(survey$Country), survey$nation)
survey$nation <- ifelse((survey$nation == "Barbados" & survey$Country == "Canada"), as.character(survey$Country), survey$nation)
survey$nation <- ifelse((survey$nation == "Benin" & survey$Country == "Germany"), as.character(survey$Country), survey$nation)
survey$nation <- ifelse((survey$nation == "Central African Republic" & survey$Country == "Canada"), as.character(survey$Country), survey$nation)
survey$nation <- ifelse((survey$nation == "Chad" & survey$Country == "Canada"), as.character(survey$Country), survey$nation)
survey$nation <- ifelse((survey$nation == "Ethiopia" & survey$Country == "United Kingdom"), as.character(survey$Country), survey$nation)
survey$nation <- ifelse((survey$nation == "Gabon" & survey$Country == "United States"), as.character(survey$Country), survey$nation)
survey$nation <- ifelse((survey$nation == "Indonesia" & survey$Country == "Netherlands"), as.character(survey$Country), survey$nation)
survey$nation <- ifelse((survey$nation == "Iran" & survey$Country == "Canada"), as.character(survey$Country), survey$nation)
survey$nation <- ifelse((survey$nation == "Marshall Islands" & survey$Country == "Malta"), as.character(survey$Country), survey$nation)
survey$nation <- ifelse((survey$nation == "Mauritius" & survey$Country == "Netherlands"), as.character(survey$Country), survey$nation)
survey$nation <- ifelse((survey$nation == "Nigeria" & survey$Country == "United Kingdom"), as.character(survey$Country), survey$nation)
survey$nation <- ifelse((survey$nation == "India" & survey$Country == "Netherlands"), as.character(survey$Country), survey$nation)
survey$nation <- ifelse((survey$nation == "India" & survey$Country == "Canada"), as.character(survey$Country), survey$nation)
survey$nation <- ifelse((survey$nation == "India" & survey$Country == "Switzerland"), as.character(survey$Country), survey$nation)
survey$nation <- ifelse((survey$nation == "India" & survey$Country == "Germany"), as.character(survey$Country), survey$nation)
survey$nation <- ifelse((survey$nation == "Uruguay" & survey$Country == "United States"), as.character(survey$Country), survey$nation)
survey$nation <- ifelse((survey$nation == "Uruguay" & survey$Country == "Canada"), as.character(survey$Country), survey$nation)
survey$nation <- ifelse((survey$nation == "Uruguay" & survey$Country == "Iceland"), as.character(survey$Country), survey$nation)
survey$nation <- ifelse((survey$nation == "Uruguay" & survey$Country == "France"), as.character(survey$Country), survey$nation)
survey$nation <- ifelse((survey$nation == "Korea" & survey$Country == "Korea"), "Korea, South", survey$nation)

#check
nation2 <- as.data.frame(survey)
nation2 <- nation2[ , c(3, 73,76, 77)]

#now make a csv so we can go through and check one last time
write.csv(nation2, file = "data/gya-nations_check.csv", row.names = FALSE)


#add in a column for developed/developing 
developed<-c("Canada","Australia", "Israel", "Barbados","Russia","Germany", "United Kingdom", "Netherlands", "Japan","United States", "Taiwan","New Zealand","France", "Switzerland", "Poland",              
             "Portugal", "Italy",  "Belgium",  "Norway", "Finland", "Greece","Cyprus",  "Hungary", "Spain","Singapore", "Korea, South","Romania", "Denmark","Austria", "Sweden",                
             "Estonia", "Malta", "Iceland" )

developing<-c("Brazil","South Africa", "Mauritius", "Uruguay","Turkey","Indonesia", "Morocco", "India", "Bangladesh","Ghana", "Malaysia","Vietnam","Nigeria", "Egypt", "Montenegro",              
              "China", "Serbia",  "Kenya",  "Central African Republic", "Chad", "Chile","Argentina",  "Mexico", "Nepal","Benin", "Ethiopia","Lesotho", "Nicaragua","Philippines", "Colombia",                
              "Mozambique", "Dominican Republic", "Lebanon","Gabon", "Cameroon", "Uganda", "Iran", "Sudan", "Thailand", "Marshall Islands" ) 

survey$class <- ifelse(survey$nation%in%developed, "developed", "developing")

## add unique survey ID to dataframe
survey$id<-c(1:dim(survey)[1])

#make a csv of the complete responses 
write.csv(survey, file="data/gya-without-incomplete.csv", row.names=FALSE)

## create a table with country, OCED category, category (developing/developed), # of responses
#get number of responses for each country
table<-aggregate(gender ~ nation, survey, length)
table <- table[!table$nation == "",]
table <- droplevels(table)

# add column with OECD category
highincome <- c("Canada","Australia", "Israel", "Barbados","Russia","Germany", "United Kingdom", "Netherlands", "Japan","United States", "Taiwan","New Zealand","France", "Switzerland", "Poland",              
                "Portugal", "Italy",  "Belgium",  "Norway", "Finland", "Greece","Cyprus",  "Hungary", "Spain","Singapore", "Korea, South","Romania", "Denmark","Austria", "Sweden",                
                "Estonia", "Malta", "Iceland" )

highermid <- c("Brazil","South Africa", "Mauritius", "Uruguay","Turkey","Malaysia","Montenegro", "China", "Serbia",  "Chile","Argentina", 
               "Mexico", "Colombia", "Dominican Republic","Lebanon","Gabon","Iran","Thailand", "Marshall Islands" )

lowermid <- c("Indonesia", "Morocco", "India", "Ghana","Vietnam", "Nigeria","Egypt","Nicaragua", "Philippines","Cameroon")

lowincome <- c("Bangladesh","Kenya","Central African Republic","Chad", "Nepal","Benin","Ethiopia","Lesotho","Mozambique","Uganda","Sudan")

table$oecd <- "High Income"
table$oecd <- ifelse(table$nation%in%highermid, "Higher Middle Income", table$oecd)
table$oecd <- ifelse(table$nation%in%lowermid, "Lower Middle Income", table$oecd)
table$oecd <- ifelse(table$nation%in%lowincome, "Low Income", table$oecd)

#check
table

# add column with collapsed category
table$class <- ifelse(table$nation%in%developed, "Developed", "Developing")

# change column names
tablenames <- c("Country", "Number of Responses", "OECD Category", "Collapsed Category")
colnames(table)<-tablenames
table

#save csv
write.csv(table, "data/category_&_responses_table.csv")


################!!!!!!!!!!!!!!!!!!!!######################
##Create a csv of just canadian responses for Megan
#Canada<-survey[survey$Country_work=="Canada" | (!(survey$Country_work=="Canada") & 
#                                                 survey$Country_work=="" & survey$Country=="Canada"),]
#head(Canada)
#write.csv(Canada, file="data/gya-only-Canada.csv", row.names = FALSE)

###%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
###^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
### Cleaning and subsetting data 
###^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
###%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# all subsetted csvs saved at bottom of all the code

#############
####Part5####
#############
#canada<-subset(survey, Country=="Canada")
#temp<-survey[survey$Country_work=="Canada" | (!(survey$Country_work=="Canada") & survey$Country_work=="" & survey$Country=="Canada"),]
#dim(canada); dim(temp)
#table(temp$field_research)

#ca<-subset(canada, select = c("Location", "Country", "Country_work") )


survey.what<-subset(survey, select=c("Location", "nation", "Country", "gender", "field_research", "Country_work", "PhD_Year", "what_participant_group","id", "class"))


###saved


##############
#####Part1####
##############

## read in non-subsetted data again
survey<-read.csv("data/gya-without-incomplete.csv", header=TRUE)

part1<-subset(survey, select = c("Location","nation", "Country", "Country_work", "gender","what_participant_group"  , "field_research",   "percent_fundemental_research_current"  ,    
                                 "percent_Use_inspired_Research_current"   ,  
                                  "percent_Applied_Research_current"   ,        "changed_10yrs"        ,                      "percent_Fundamental_Research_past"         ,
                                  "percent_Use_inspired_Research_past"  ,       "percent_Applied_Research_past"    ,          "Main_reason_change_interest_related" ,      
                                 "Main_reason_change_Career_related" ,         "Main_reason_change_Funding_related" ,        "Main_reason_change_Socially_related" ,      
                                  "Main_reason_change_Other"  ,                 "Main_reason_change_Other_text"   ,           "view_change_of_type","id"))

## 1. change all relevant variables to characters.......
part1$percent_Applied_Research_past<-as.character(part1$percent_Applied_Research_past)
part1$percent_Applied_Research_current<-as.character(part1$percent_Applied_Research_current)
part1$percent_fundemental_research_current<-as.character(part1$percent_fundemental_research_current)
part1$percent_Fundamental_Research_past<-as.character(part1$percent_Fundamental_Research_past)
part1$percent_Use_inspired_Research_current<-as.character(part1$percent_Use_inspired_Research_current)
part1$percent_Use_inspired_Research_past<-as.character(part1$percent_Use_inspired_Research_past)

## remove all % symbols 

part1$percent_Applied_Research_current<-str_replace_all(part1$percent_Applied_Research_current, "[%]", "")
part1$percent_Use_inspired_Research_current<-str_replace_all(part1$percent_Use_inspired_Research_current, "[%]", "")
part1$percent_fundemental_research_current<-str_replace_all(part1$percent_fundemental_research_current, "[%]", "")
part1$percent_Applied_Research_past<-str_replace_all(part1$percent_Applied_Research_past, "[%]", "")
part1$percent_Fundamental_Research_past<-str_replace_all(part1$percent_Fundamental_Research_past, "[%]", "")
part1$percent_Use_inspired_Research_past<-str_replace_all(part1$percent_Use_inspired_Research_past, "[%]", "")

survey.type<-subset(part1, select = c("Location", "nation","id", "Country","Country_work",  "gender","what_participant_group"  , "field_research","percent_fundemental_research_current"  ,     "percent_Use_inspired_Research_current"   ,  
                                      "percent_Applied_Research_current"   ,      "percent_Fundamental_Research_past"         ,
                                      "percent_Use_inspired_Research_past"  ,       "percent_Applied_Research_past"))
dim(survey.type)  #2918   14
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
dim(survey.type)  #2918   14
#remove surveys that skipped this question
#remove if they skipped the 'past' part of the question so we can see only the the ones that changed
survey.type.noblank<-survey.type[!is.na(survey.type$percent_Fundamental_Research_past),]
survey.type.wvalue<-survey.type.noblank[!(survey.type.noblank$percent_Fundamental_Research_past==0 & survey.type.noblank$percent_Applied_Research_past==0 & survey.type.noblank$percent_Use_inspired_Research_past==0),]
dim(survey.type.wvalue)   #1190   14   - 1728 rows removed
#keep the empty ones to add in later
survey.napast <- survey.type[is.na(survey.type$percent_Fundamental_Research_past),]
dim(survey.napast)  #1726
survey.pastblank <- survey.type.noblank[(survey.type.noblank$percent_Fundamental_Research_past==0 & survey.type.noblank$percent_Applied_Research_past==0 & survey.type.noblank$percent_Use_inspired_Research_past==0),]
dim(survey.pastblank)  # 2  

#now current
survey.type.bothtime<-survey.type.wvalue[!is.na(survey.type.wvalue$percent_fundemental_research_current),]  
dim(survey.type.bothtime)  #1190  no rows removed
survey.type.bothtime.values<-survey.type.bothtime[!(survey.type.bothtime$percent_fundemental_research_current==0 & survey.type.bothtime$percent_Applied_Research_current==0 & survey.type.bothtime$percent_Use_inspired_Research_current==0),]  # didnt remove any rows
dim(survey.type.bothtime.values)  #1190
#keep the just current ones to add back in later
#no rows were removed from above code so nothing the save to put back in


## 3. Standardise % values > 100 in total

survey.type.bothtime.values$percent_Applied_Research_current<-as.numeric(survey.type.bothtime.values$percent_Applied_Research_current)
survey.type.bothtime.values$percent_fundemental_research_current<-as.numeric(survey.type.bothtime.values$percent_fundemental_research_current)
survey.type.bothtime.values$percent_Use_inspired_Research_current<-as.numeric(survey.type.bothtime.values$percent_Use_inspired_Research_current)
colnames(survey.type.bothtime.values)
survey.type.bothtime.values$total_research<-rowSums(survey.type.bothtime.values[,9:11])

head(survey.type.bothtime.values[survey.type.bothtime.values$total_research>100,])

survey.type.bothtime.values$percent_Applied_Research_past<-as.numeric(survey.type.bothtime.values$percent_Applied_Research_past)
survey.type.bothtime.values$percent_Fundamental_Research_past<-as.numeric(survey.type.bothtime.values$percent_Fundamental_Research_past)
survey.type.bothtime.values$percent_Use_inspired_Research_past<-as.numeric(survey.type.bothtime.values$percent_Use_inspired_Research_past)
colnames(survey.type.bothtime.values)
survey.type.bothtime.values$total_research<-rowSums(survey.type.bothtime.values[,12:14])

head(survey.type.bothtime.values[survey.type.bothtime.values$total_research>100,])
## need to change surveys that are over 100%
survey.type.bothtime.values$percent_Applied_Research_current<-ifelse(survey.type.bothtime.values$total_research>100,(survey.type.bothtime.values$percent_Applied_Research_current/survey.type.bothtime.values$total_research)*100, survey.type.bothtime.values$percent_Applied_Research_current)
survey.type.bothtime.values$percent_fundemental_research_current<-ifelse(survey.type.bothtime.values$total_research>100,(survey.type.bothtime.values$percent_fundemental_research_current/survey.type.bothtime.values$total_research)*100, survey.type.bothtime.values$percent_fundemental_research_current)
survey.type.bothtime.values$percent_Use_inspired_Research_current<-ifelse(survey.type.bothtime.values$total_research>100,(survey.type.bothtime.values$percent_Use_inspired_Research_current/survey.type.bothtime.values$total_research)*100, survey.type.bothtime.values$percent_Use_inspired_Research_current)

survey.type.bothtime.values$percent_Applied_Research_past<-ifelse(survey.type.bothtime.values$total_research>100,(survey.type.bothtime.values$percent_Applied_Research_past/survey.type.bothtime.values$total_research)*100, survey.type.bothtime.values$percent_Applied_Research_past)
survey.type.bothtime.values$percent_Fundamental_Research_past<-ifelse(survey.type.bothtime.values$total_research>100,(survey.type.bothtime.values$percent_Fundamental_Research_past/survey.type.bothtime.values$total_research)*100, survey.type.bothtime.values$percent_Fundamental_Research_past)
survey.type.bothtime.values$percent_Use_inspired_Research_past<-ifelse(survey.type.bothtime.values$total_research>100,(survey.type.bothtime.values$percent_Use_inspired_Research_past/survey.type.bothtime.values$total_research)*100, survey.type.bothtime.values$percent_Use_inspired_Research_past)

head(survey.type.bothtime.values[survey.type.bothtime.values$total_research>100,])

dim(survey.type.bothtime.values)
## switch to long format
survey.long<-gather(survey.type.bothtime.values, type, percent, -Location, -gender, -id, -nation, -Country_work, -Country, -what_participant_group, -field_research, -total_research)
tail(survey.long)

#saved

### now need to add back in the rows that did not have answers so can add to master final dataset
survey.wide <- spread(survey.long, type, percent)
head(survey.wide)
dim(survey.wide)  #1190   15
colnames(survey.wide)

head(survey.napast)
colnames(survey.wide)

part1.survey.wnapast<-join_all(list(survey.wide, survey.napast), by = "id", type = "full", match = "first")
dim(part1.survey.wnapast)  #2916   15
head(part1.survey.wnapast)
colnames(part1.survey.wnapast)
n_occur <- data.frame(table(part1.survey.wnapast$id))
dup<-n_occur[n_occur$Freq > 1,] 

head(survey.pastblank)
colnames(survey.pastblank)
dim(survey.pastblank)  #2  14

part1.survey.all<-join_all(list(part1.survey.wnapast, survey.pastblank), by = "id", type = "full", match = "first")
dim(part1.survey.all)  #2918   15
head(part1.survey.all)
colnames(part1.survey.all)
n_occur <- data.frame(table(part1.survey.all$id))
dup<-n_occur[n_occur$Freq > 1,] 


#### yes/no have the portions changed and why

## read in non-subsetted data again
survey<-read.csv("data/gya-without-incomplete.csv", header=TRUE)

survey.change<-subset(survey, select=c("Location","Country","Country_work", "nation",  "gender","what_participant_group"  , "field_research", "changed_10yrs", "Main_reason_change_interest_related", 
                                "Main_reason_change_Career_related", "Main_reason_change_Funding_related","Main_reason_change_Socially_related",
                                "Main_reason_change_Other","id"))
#saved

#### how do you view this change in the type of research?
survey<-read.csv(file="data/gya-without-incomplete.csv")

part1.view<-subset(survey, select=c("Location","Country","Country_work", "nation",  "gender","what_participant_group"  , "field_research", "view_change_of_type","id"))

#saved




#############
####Part4####
#############

#read in non subsetted data again
survey<-read.csv("data/gya-without-incomplete.csv", header=TRUE)

survey.part4<-subset(survey, select=c("Location","Country","Country_work", "nation", "gender","what_participant_group"  , "field_research","opinion_fundamental_important",  
                               "high_priority_fundamental", "high_priority_use_inspired", "high_priority_applied", 
                               "high_priority_no_change", "available_funding_fundamental", "available_funding_use_inspired", "available_funding_applied",
                               "next_generation","id"))
#saved
#######################
#######Part 2##########
#######################

#read in unsubsetted data again
survey<-read.csv("data/gya-without-incomplete.csv", header=TRUE)
part2<-subset(survey, select=c("Country","Country_work", "nation", "gender","what_participant_group"  , "field_research", "Location", "partnership_outside", "partnership_change_10yrs",
                               "partnership_outside_before", "reason_partnership_change_interest", "reason_partnership_change_career", 
                               "reason_partnership_change_socially",  "reason_partnership_change_funding", "reason_partnership_change_other",
                               "view_change_partnership","id"))
#saved

#### level of partnership that your research currently has outside of academia before and after
part2.b.a.all<-subset(part2, select=c("Country","Country_work", "nation", "gender","what_participant_group"  , "field_research", "Location", "partnership_outside","partnership_outside_before","id"))
#saved
#remove non responses
part2.b.a<-part2.b.a.all[!part2.b.a.all$partnership_outside=="",]
part2.b.a<-part2.b.a[!part2.b.a$partnership_outside_before=="",]
#saved

####Level of partnership change in past 10 yrs
part2.change.all<-subset(part2, select=c("Country","Country_work", "nation", "gender","what_participant_group"  , "field_research", "Location","partnership_change_10yrs","id"))
#remove non responses
part2.change<-part2.change.all[!part2.change.all$partnership_change_10yrs=="",]
#saved

####Reason for change
part2.reason<-subset(part2, select=c("Country","Country_work", "nation", "gender","what_participant_group", "field_research", "Location","reason_partnership_change_interest", "reason_partnership_change_career", 
                                     "reason_partnership_change_socially",  "reason_partnership_change_funding", "reason_partnership_change_other","id"))
#saved

#### View of Change
part2.view.all<-subset(part2, select=c("Country","Country_work", "nation", "gender","what_participant_group", "field_research", "Location","view_change_partnership","id"))
#saved

#remove non responses
part2.view<-part2.view.all[!part2.view.all$view_change_partnership=="",]
#saved

##################
##### Part 3 #####
##################

#read in non subsetted data again
survey<-read.csv("data/gya-without-incomplete.csv", header=TRUE)
#subset data needed for this part
part3<-subset(survey, select=c("Country","Country_work", "nation", "gender","what_participant_group", "field_research", "Location","external_pi_grant_11_15_fundamental" , "external_pi_grant_11_15_use" ,              
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
                               "success_change_10yrs_applied","id"))

#saved

## number of grant applications 
part3.grants<-subset(part3, select = c("Country","Country_work", "gender", "nation", "Location","what_participant_group", "field_research","external_pi_grant_11_15_fundamental" , "external_pi_grant_11_15_use" ,              
                                          "external_pi_grant_11_15_applied", "external_pi_grant_6_10_fundamental",        
                                       "external_pi_grant_6_10_use"     ,            "external_pi_grant_6_10_applied","id"))
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
part3.grants.long<-gather(part3.grants, type.grant, number, -Location, -nation, -gender, -Country, -Country_work, -what_participant_group, -field_research, -id)
head(part3.grants.long)

#saved


## percentage of successful grants

part3.success<-subset(part3, select = c("Country","Country_work", "gender", "nation", "Location","what_participant_group", "field_research","successful_grants_11_15_fundamental"  ,      "successful_grants_11_15_use"     ,          
                                        "successful_grants_11_15_applied",            "successful_grants_6_10_fundamental"   ,     
                                        "successful_grants_6_10_use"  ,               "successful_grants_6_10_applied" ,"id"))

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
part3.success.long<-gather(part3.success, type, percent, -Location,-Country_work, -nation, -gender, -Country, -what_participant_group, -field_research, -id)

#remove blanks and "no need for aplications for this research type"
part3.success.long<-part3.success.long[!(part3.success.long$percent==""),]
part3.success.long<-part3.success.long[!(part3.success.long$percent=="No need for applications for this research type"),]
#saved

##create a version with the non responses included and not changed into long form to add to the big final data set
part3.success.all<-subset(part3, select = c("Country","Country_work", "gender", "nation", "Location","what_participant_group", "field_research","successful_grants_11_15_fundamental"  ,      "successful_grants_11_15_use"     ,          
                                        "successful_grants_11_15_applied",            "successful_grants_6_10_fundamental"   ,     
                                        "successful_grants_6_10_use"  ,               "successful_grants_6_10_applied" ,"id"))

#remove %
part3.success.all$successful_grants_11_15_fundamental<-str_replace_all(part3.success.all$successful_grants_11_15_fundamental, "[%]", "")
part3.success.all$successful_grants_11_15_use<-str_replace_all(part3.success.all$successful_grants_11_15_use, "[%]", "")
part3.success.all$successful_grants_11_15_applied<-str_replace_all(part3.success.all$successful_grants_11_15_applied, "[%]", "")
part3.success.all$successful_grants_6_10_fundamental<-str_replace_all(part3.success.all$successful_grants_6_10_fundamental, "[%]", "")
part3.success.all$successful_grants_6_10_use<-str_replace_all(part3.success.all$successful_grants_6_10_use, "[%]", "")
part3.success.all$successful_grants_6_10_applied<-str_replace_all(part3.success.all$successful_grants_6_10_applied, "[%]", "")




## important to suggest practical applications

part3.prac.app<-subset(part3, select = c("Country","Country_work", "nation", "gender", "Location","what_participant_group", "field_research","practical_applications_important_11_15"  ,   "practical_applications_important_6_10" ,"id"))
head(part3.prac.app)
#saved
colnames(part3.prac.app)
#change to long form
part3.prac.long<-gather(part3.prac.app, year, level, -Location, -gender, -nation, -Country_work, -Country, -what_participant_group, -field_research, -id)
#remove non responses
part3.prac.long<-part3.prac.long[!(part3.prac.long$level==""),]

#saved


##important to include partners from for profit or non gov sectors
part3.part<-subset(part3, select = c("Country","Country_work", "gender", "nation", "Location","what_participant_group", "field_research","include_nonacademia_partners_success_11_15", "include_nonacademia_partners_success_6_10" ,"id"))
#saved
#change to long form
part3.part.long<-gather(part3.part, year, level, -Location, -nation, -gender,-Country_work, -Country, -what_participant_group, -field_research, -id)

#remove non responses
part3.part.long<-part3.part.long[!(part3.part.long$level==""),]

#saved


## distribution of funding
part3.funding<-subset(part3, select = c("Country", "Country_work", "gender", "nation", "Location","what_participant_group", "field_research","distribution_funding_11_15_internal" ,       "distriution_funding_11_15_government" ,     
                                        "distriution_funding_11_15_for_profit",       "distriution_funding_11_15_nongov" ,         
                                        "distriution_funding_11_15_other"   ,      
                                        "distriution_funding_6_10_internal" ,         "distriution_funding_6_10_government" ,      
                                        "distriution_funding_6_10_for_profit" ,       "distriution_funding_6_10_nongov"  ,         
                                        "distriution_funding_6_10_other","id"))

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

## 1. remove responses where a person said new researcher in the 2006-2010 section (some people selected new researcher for one category but then a percentage for another so I had to get rid more than I thought)
part3.funding<-gather(part3.funding, question, value, -Country, -gender, -nation, -Country_work, -Location, -survey, -what_participant_group, -field_research, -id)
part3.funding$year<-ifelse(grepl("11_15", part3.funding$question), "11_15", "6_10")
part3.funding$info<-ifelse((part3.funding$year=="6_10" & part3.funding$value=="New researcher (no funding in these years)"),"remove", "keep")
survey.remove<-part3.funding$survey[part3.funding$info=="remove"]
part3.funding.nonew<-part3.funding[!(part3.funding$year=="6_10" & part3.funding$survey %in% survey.remove),]
part3.funding.nonew<-spread(part3.funding.nonew, question, value)

# keep these to add them back in later for the final big dataset
part3.funding.new<-part3.funding[(part3.funding$year=="6_10" & part3.funding$info =="remove"),]  
part3.funding.new<-spread(part3.funding.new, question, value)

# split the two years so they can be standardized
p3_6.10<-part3.funding.nonew[part3.funding.nonew$year=="6_10",]
p3_6.10<-p3_6.10[,c(1:11, 17:21)]
p3_11.15<-part3.funding.nonew[part3.funding.nonew$year=="11_15",]
p3_11.15<-p3_11.15[,c(1:16)]

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
p3_11.15.nona<-p3_11.15[!(is.na(p3_11.15$distribution_funding_11_15_internal)& is.na(p3_11.15$distriution_funding_11_15_government) & is.na(p3_11.15$distriution_funding_11_15_for_profit) &
                           is.na(p3_11.15$distriution_funding_11_15_nongov) & is.na(p3_11.15$distriution_funding_11_15_other)),]
p3_6.10.nona<-p3_6.10[!(is.na(p3_6.10$distriution_funding_6_10_internal)& is.na(p3_6.10$distriution_funding_6_10_government) & is.na(p3_6.10$distriution_funding_6_10_for_profit) &
                                 is.na(p3_6.10$distriution_funding_6_10_nongov) & is.na(p3_6.10$distriution_funding_6_10_other)),]

# keep the NAs to put back in later for the final big dataset
p3.11.15.na<-p3_11.15[(is.na(p3_11.15$distribution_funding_11_15_internal)& is.na(p3_11.15$distriution_funding_11_15_government) & is.na(p3_11.15$distriution_funding_11_15_for_profit) &
                             is.na(p3_11.15$distriution_funding_11_15_nongov) & is.na(p3_11.15$distriution_funding_11_15_other)),]
p3_6.10.na<-p3_6.10[(is.na(p3_6.10$distriution_funding_6_10_internal)& is.na(p3_6.10$distriution_funding_6_10_government) & is.na(p3_6.10$distriution_funding_6_10_for_profit) &
                           is.na(p3_6.10$distriution_funding_6_10_nongov) & is.na(p3_6.10$distriution_funding_6_10_other)),]


## 4. Standardise % values > 100 in total
p3_11.15.nona$distribution_funding_11_15_internal<-as.numeric((p3_11.15.nona$distribution_funding_11_15_internal))
p3_11.15.nona$distriution_funding_11_15_government<-as.numeric((p3_11.15.nona$distriution_funding_11_15_government))
p3_11.15.nona$distriution_funding_11_15_for_profit<-as.numeric((p3_11.15.nona$distriution_funding_11_15_for_profit))
p3_11.15.nona$distriution_funding_11_15_nongov<-as.numeric((p3_11.15.nona$distriution_funding_11_15_nongov))
p3_11.15.nona$distriution_funding_11_15_other<-as.numeric((p3_11.15.nona$distriution_funding_11_15_other))
p3_6.10.nona$distriution_funding_6_10_internal<-as.numeric((p3_6.10.nona$distriution_funding_6_10_internal))
p3_6.10.nona$distriution_funding_6_10_government<-as.numeric((p3_6.10.nona$distriution_funding_6_10_government))
p3_6.10.nona$distriution_funding_6_10_for_profit<-as.numeric((p3_6.10.nona$distriution_funding_6_10_for_profit))
p3_6.10.nona$distriution_funding_6_10_nongov<-as.numeric((p3_6.10.nona$distriution_funding_6_10_nongov))
p3_6.10.nona$distriution_funding_6_10_other<-as.numeric((p3_6.10.nona$distriution_funding_6_10_other))

## need to change surveys that are over 100%
p3_11.15.nona$total.funding<-rowSums(p3_11.15.nona[,12:16])
p3_6.10.nona$total.funding<-rowSums(p3_6.10.nona[,12:16])

head(p3_11.15.nona[p3_11.15.nona$total.funding>100,])

p3_11.15.nona$distribution_funding_11_15_internal<-ifelse(p3_11.15.nona$total.funding>100,(p3_11.15.nona$distribution_funding_11_15_internal/p3_11.15.nona$total.funding)*100, p3_11.15.nona$distribution_funding_11_15_internal)
p3_11.15.nona$distriution_funding_11_15_government<-ifelse(p3_11.15.nona$total.funding>100,(p3_11.15.nona$distriution_funding_11_15_government/p3_11.15.nona$total.funding)*100, p3_11.15.nona$distriution_funding_11_15_government)
p3_11.15.nona$distriution_funding_11_15_for_profit<-ifelse(p3_11.15.nona$total.funding>100,(p3_11.15.nona$distriution_funding_11_15_for_profit/p3_11.15.nona$total.funding)*100, p3_11.15.nona$distriution_funding_11_15_for_profit)
p3_11.15.nona$distriution_funding_11_15_nongov<-ifelse(p3_11.15.nona$total.funding>100,(p3_11.15.nona$distriution_funding_11_15_nongov/p3_11.15.nona$total.funding)*100, p3_11.15.nona$distriution_funding_11_15_nongov)
p3_11.15.nona$distriution_funding_11_15_other<-ifelse(p3_11.15.nona$total.funding>100,(p3_11.15.nona$distriution_funding_11_15_other/p3_11.15.nona$total.funding)*100, p3_11.15.nona$distriution_funding_11_15_other)
p3_6.10.nona$distriution_funding_6_10_internal<-ifelse(p3_6.10.nona$total.funding>100,(p3_6.10.nona$distriution_funding_6_10_internal/p3_6.10.nona$total.funding)*100, p3_6.10.nona$distriution_funding_6_10_internal)
p3_6.10.nona$distriution_funding_6_10_government<-ifelse(p3_6.10.nona$total.funding>100,(p3_6.10.nona$distriution_funding_6_10_government/p3_6.10.nona$total.funding)*100, p3_6.10.nona$distriution_funding_6_10_government)
p3_6.10.nona$distriution_funding_6_10_for_profit<-ifelse(p3_6.10.nona$total.funding>100,(p3_6.10.nona$distriution_funding_6_10_for_profit/p3_6.10.nona$total.funding)*100, p3_6.10.nona$distriution_funding_6_10_for_profit)
p3_6.10.nona$distriution_funding_6_10_nongov<-ifelse(p3_6.10.nona$total.funding>100,(p3_6.10.nona$distriution_funding_6_10_nongov/p3_6.10.nona$total.funding)*100, p3_6.10.nona$distriution_funding_6_10_nongov)
p3_6.10.nona$distriution_funding_6_10_other<-ifelse(p3_6.10.nona$total.funding>100,(p3_6.10.nona$distriution_funding_6_10_other/p3_6.10.nona$total.funding)*100, p3_6.10.nona$distriution_funding_6_10_other)

head(p3_6.10.nona[p3_6.10.nona$total.funding>100,])

colnames(p3_11.15.nona)[colnames(p3_11.15.nona)=="distriution_funding_11_15_for_profit"]<-"For-Profit"
colnames(p3_11.15.nona)[colnames(p3_11.15.nona)=="distriution_funding_11_15_government"]<-"Government"
colnames(p3_11.15.nona)[colnames(p3_11.15.nona)=="distribution_funding_11_15_internal"]<-"Internal"
colnames(p3_11.15.nona)[colnames(p3_11.15.nona)=="distriution_funding_11_15_nongov"]<-"Non-governmental"
colnames(p3_11.15.nona)[colnames(p3_11.15.nona)=="distriution_funding_11_15_other"]<-"Other"

colnames(p3_6.10.nona)[colnames(p3_6.10.nona)=="distriution_funding_6_10_for_profit"]<-"For-Profit"
colnames(p3_6.10.nona)[colnames(p3_6.10.nona)=="distriution_funding_6_10_government"]<-"Government"
colnames(p3_6.10.nona)[colnames(p3_6.10.nona)=="distriution_funding_6_10_internal"]<-"Internal"
colnames(p3_6.10.nona)[colnames(p3_6.10.nona)=="distriution_funding_6_10_nongov"]<-"Non-governmental"
colnames(p3_6.10.nona)[colnames(p3_6.10.nona)=="distriution_funding_6_10_other"]<-"Other"

head(p3_6.10.nona)

p3_master<-rbind(p3_6.10.nona, p3_11.15.nona)

head(p3_master)

## switch to long format
p3_master.long<-gather(p3_master, type, percent,-info, -year, -survey, -total.funding, -nation, -Location, -gender, -id, -Country_work, -Country, -what_participant_group, -field_research)

head(p3_master.long)

#saved

## now going to get this data ready to be added back to the rest of the data for a final big dataset
# fix the year back into the title
p3_master.long$type.year <- paste("distribution_funding",p3_master.long$year, p3_master.long$type,  sep = "_")

head(p3_master.long)

#get rid of columns that might confusing the spreading
p3_master.long$'year' <- NULL
p3_master.long$'type' <- NULL
p3_master.long$'info' <- NULL
p3_master.long$'total.funding' <- NULL
p3_master.long$'survey' <- NULL

head(p3_master.long)

#spread it back out
p3.master.wide <- spread(p3_master.long, type.year, percent)

head(p3.master.wide)

## now add back in the new researcher responses and na's
#compare column names
colnames(p3.master.wide)
colnames(part3.funding.new)
colnames(p3_6.10.na)
colnames(p3.11.15.na)

#need to change the column names to match
colnames(p3.master.wide)[colnames(p3.master.wide)=="distribution_funding_11_15_For-Profit"]<-"distriution_funding_11_15_for_profit"
colnames(p3.master.wide)[colnames(p3.master.wide)=="distribution_funding_11_15_Government"]<-"distriution_funding_11_15_government"
colnames(p3.master.wide)[colnames(p3.master.wide)=="distribution_funding_11_15_Internal"]<-"distribution_funding_11_15_internal"
colnames(p3.master.wide)[colnames(p3.master.wide)=="distribution_funding_11_15_Non-governmental"]<-"distriution_funding_11_15_nongov"
colnames(p3.master.wide)[colnames(p3.master.wide)=="distribution_funding_11_15_Other"]<-"distriution_funding_11_15_other"
colnames(p3.master.wide)[colnames(p3.master.wide)=="distribution_funding_6_10_For-Profit"]<-"distriution_funding_6_10_for_profit"
colnames(p3.master.wide)[colnames(p3.master.wide)=="distribution_funding_6_10_Government"]<-"distriution_funding_6_10_government"
colnames(p3.master.wide)[colnames(p3.master.wide)=="distribution_funding_6_10_Internal"]<-"distriution_funding_6_10_internal"
colnames(p3.master.wide)[colnames(p3.master.wide)=="distribution_funding_6_10_Non-governmental"]<-"distriution_funding_6_10_nongov"
colnames(p3.master.wide)[colnames(p3.master.wide)=="distribution_funding_6_10_Other"]<-"distriution_funding_6_10_other"

colnames(p3.master.wide)

#get rid of extra columns
colnames(part3.funding.new)
part3.funding.new$'survey' <- NULL
part3.funding.new$'year' <- NULL
part3.funding.new$'info' <- NULL

colnames(p3_6.10.na)
p3_6.10.na$'survey' <- NULL
p3_6.10.na$'year' <- NULL
p3_6.10.na$'info' <- NULL

colnames(p3.11.15.na)
p3.11.15.na$'survey' <- NULL
p3.11.15.na$'year' <- NULL
p3.11.15.na$'info' <- NULL

#check
dim(p3.master.wide)   #2883  18
dim(part3.funding.new)  #1055  13
dim(p3_6.10.na)  #126   13
dim(p3.11.15.na)  #39  13

#they are different types so need to make the percents into characters so they can be joined
p3.master.wide$distribution_funding_11_15_internal<-as.character((p3.master.wide$distribution_funding_11_15_internal))
p3.master.wide$distriution_funding_11_15_government<-as.character((p3.master.wide$distriution_funding_11_15_government))
p3.master.wide$distriution_funding_11_15_for_profit<-as.character((p3.master.wide$distriution_funding_11_15_for_profit))
p3.master.wide$distriution_funding_11_15_nongov<-as.character((p3.master.wide$distriution_funding_11_15_nongov))
p3.master.wide$distriution_funding_11_15_other<-as.character((p3.master.wide$distriution_funding_11_15_other))
p3.master.wide$distriution_funding_6_10_internal<-as.character((p3.master.wide$distriution_funding_6_10_internal))
p3.master.wide$distriution_funding_6_10_government<-as.character((p3.master.wide$distriution_funding_6_10_government))
p3.master.wide$distriution_funding_6_10_for_profit<-as.character((p3.master.wide$distriution_funding_6_10_for_profit))
p3.master.wide$distriution_funding_6_10_nongov<-as.character((p3.master.wide$distriution_funding_6_10_nongov))
p3.master.wide$distriution_funding_6_10_other<-as.character((p3.master.wide$distriution_funding_6_10_other))

# now add in the 'new researcher' rows
head(part3.funding.new)
tail(part3.funding.new)
dim(part3.funding.new)  #1055  13
#only has 6-10 columns 

require(plyr)
p3.master.withnew<-join_all(list(p3.master.wide, part3.funding.new), by = "id", type = "full", match = "first")
dim(p3.master.withnew)  #2886   30
head(p3.master.withnew)
colnames(p3.master.withnew)
n_occur <- data.frame(table(p3.master.withnew$id))
dup<-n_occur[n_occur$Freq > 1,] 

# combine the 6-10 columns to the 11-15 df
head(p3_6.10.na)
head(p3.11.15.na)

#compared to see how many surveys are the same one in each one
unique(p3_6.10.na$id)
unique(p3.11.15.na$id)
# ones that are in 11-15 and not in 6-10
# 6, 7, 38, 293, 334, 2441, 2854  so it should add 7 rows

require(dplyr)
p3_bothtime_na <- full_join(p3_6.10.na, p3.11.15.na)
dim(p3_bothtime_na) #133  18   - added 7 rows!
#looked at the data and it looks like it worked
 
#now add them into the master file
p3.master.all<-join_all(list(p3.master.withnew, p3_bothtime_na), by = "id", type = "full", match = "first")

dim(p3.master.all)  # 2918  18   - added 133 rows
colnames(p3.master.all)
head(p3.master.all)  #looks good
tail(p3.master.all)  # looks good
n_occur.all <- data.frame(table(p3.master.all$id))
dup.all<-n_occur.all[n_occur.all$Freq > 1,] 
#all looks good

#check number of rows with master before cleaning
dim(part3.)  #2918   39
dim(p3.master.all)  # 2918  18






## grant success rates change over past 10 yrs
part3.change<-subset(part3, select = c("Country", "Country_work", "gender", "nation", "Location","what_participant_group", "field_research", "success_change_10yrs_fundamental"  ,        "success_change_10yrs_use"   ,               
                                       "success_change_10yrs_applied","id"))
head(part3.change)
#saved









############ save file as csv#############################

write.csv(survey.what, file="data/gya-country-responses.csv", row.names = FALSE)

write.csv(survey.long, file="data/gya-surveys-cleaned-research.csv", row.names = FALSE)

write.csv(survey.type.bothtime.values, file="data/gya-research-cleaned.csv", row.names = FALSE)

#write.csv(survey.long.past, file="data/gya-surveys-cleaned-research-past.csv", row.names = FALSE)

write.csv(survey.change, file="data/gya-change-reason.csv", row.names = FALSE)

write.csv(part1.view, file="data/gya-part1.view.csv", row.names = FALSE)

write.csv(survey.part4, file="data/gya-survey-part4.csv", row.names = FALSE)

write.csv(part2.b.a, file="data/gya-part2.before.after.csv", row.names = FALSE)  # had no response rows removed

write.csv(part2.b.a.all, file="data/gya-part2.before.after.csv", row.names = FALSE)   # same as above just has all of the rows

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

#<------------------------------------------------------------------------------------------>
#********************************************************************************************
#               Make a final csv with all of the data combined and cleaned
#********************************************************************************************
#<------------------------------------------------------------------------------------------>

# get a list of all of the columns that are needed
colnames(survey)

head(survey.what)  # this will be the base data frame that the rest will be added to 
dim(survey.what)  # 2918   10
final.survey <- survey.what
#includes columns 3,4,71,72,73,75,76,77,78,79

##each data set will be added in one by one

#columns 5,6,7,9,10,11
head(part1.survey.all)
dim(part1.survey.all)  #2918
final.survey$percent_Applied_Research_current<-part1.survey.all$percent_Applied_Research_current[match(final.survey$id, part1.survey.all$id)]
final.survey$percent_Applied_Research_past<-part1.survey.all$percent_Applied_Research_past[match(final.survey$id, part1.survey.all$id)]
final.survey$percent_fundemental_research_current<-part1.survey.all$percent_fundemental_research_current[match(final.survey$id, part1.survey.all$id)]
final.survey$percent_Fundamental_Research_past<-part1.survey.all$percent_Fundamental_Research_past[match(final.survey$id, part1.survey.all$id)]
final.survey$percent_Use_inspired_Research_current<-part1.survey.all$percent_Use_inspired_Research_current[match(final.survey$id, part1.survey.all$id)]
final.survey$percent_Use_inspired_Research_past<-part1.survey.all$percent_Use_inspired_Research_past[match(final.survey$id, part1.survey.all$id)]
head(final.survey)
dim(final.survey)  #2918   16 - added 6 columns -correct

#columns 8, 12-17
head(survey.change)
dim(survey.change)  #2918 
final.survey$changed_10yrs<-survey.change$changed_10yrs[match(final.survey$id, survey.change$id)]
final.survey$Main_reason_change_interest_related<-survey.change$Main_reason_change_interest_related[match(final.survey$id, survey.change$id)]
final.survey$Main_reason_change_Career_related<-survey.change$Main_reason_change_Career_related[match(final.survey$id, survey.change$id)]
final.survey$Main_reason_change_Funding_related<-survey.change$Main_reason_change_Funding_related[match(final.survey$id, survey.change$id)]
final.survey$Main_reason_change_Socially_related<-survey.change$Main_reason_change_Socially_related[match(final.survey$id, survey.change$id)]
final.survey$Main_reason_change_Other<-survey.change$Main_reason_change_Other[match(final.survey$id, survey.change$id)]
head(final.survey)
dim(final.survey)  #2918   22   - added 6 columns which is right

#columns  17  (comment column)
final.survey$Main_reason_change_Other_text<-survey$Main_reason_change_Other_text[match(part1.view$id, survey$id)]
head(final.survey)
dim(final.survey)  #2918  23 - added 1 column

#column  18
head(part1.view)
dim(part1.view)    #2918    
final.survey$view_change_of_type<-part1.view$view_change_of_type[match(part1.view$id, survey.change$id)]
head(final.survey)
dim(final.survey)   #2918   24   - added 1 column - correct

#columns  19, 21
head(part2.b.a.all)
dim(part2.b.a.all) #2918  
final.survey$partnership_outside<-part2.b.a.all$partnership_outside[match(part2.b.a.all$id, survey.change$id)]
final.survey$partnership_outside_before<-part2.b.a.all$partnership_outside_before[match(part2.b.a.all$id, survey.change$id)]
head(final.survey)
dim(final.survey) # 2818  26 - added 2 columns - correct

#column 20
head(part2.change.all)
dim(part2.change.all)  #2918
final.survey$partnership_change_10yrs<-part2.change.all$partnership_change_10yrs[match(part2.change.all$id, survey.change$id)]
head(final.survey)
dim(final.survey) #2918   27  - added 1 column - correct

#columns  22-26
head(part2.reason)
dim(part2.reason)  #2918
final.survey$reason_partnership_change_interest<-part2.reason$reason_partnership_change_interest[match(part2.reason$id, survey.change$id)]
final.survey$reason_partnership_change_career<-part2.reason$reason_partnership_change_career[match(part2.reason$id, survey.change$id)]
final.survey$reason_partnership_change_socially<-part2.reason$reason_partnership_change_socially[match(part2.reason$id, survey.change$id)]
final.survey$reason_partnership_change_funding<-part2.reason$reason_partnership_change_funding[match(part2.reason$id, survey.change$id)]
final.survey$reason_partnership_change_other<-part2.reason$reason_partnership_change_other[match(part2.reason$id, survey.change$id)]
head(final.survey)
dim(final.survey) # 2918   32   - added 5 columns - correct

#columns  27  (comment column)
final.survey$reason_partnership_change_other_text<-survey$reason_partnership_change_other_text[match(part1.view$id, survey$id)]
head(final.survey)
dim(final.survey)   # 2918  33 - 1 column added

#column 28
head(part2.view.all)
dim(part2.view.all) # 2918
final.survey$view_change_partnership<-part2.view.all$view_change_partnership[match(part2.view.all$id, survey.change$id)]
head(final.survey)
dim(final.survey) #2918  34  - added 1 column - correct

#columns  29 - 34
head(part3.grants.long)
dim(part3.grants.long)  #17508    - in long format
part3.grants.long.wide <- spread(part3.grants.long, type.grant, number)
head(part3.grants.long.wide)
dim(part3.grants.long.wide)  #2918  
final.survey$external_pi_grant_11_15_applied<-part3.grants.long.wide$external_pi_grant_11_15_applied[match(part3.grants.long.wide$id, survey.change$id)]
final.survey$external_pi_grant_11_15_fundamental<-part3.grants.long.wide$external_pi_grant_11_15_fundamental[match(part3.grants.long.wide$id, survey.change$id)]
final.survey$external_pi_grant_11_15_use<-part3.grants.long.wide$external_pi_grant_11_15_use[match(part3.grants.long.wide$id, survey.change$id)]
final.survey$external_pi_grant_6_10_applied<-part3.grants.long.wide$external_pi_grant_6_10_applied[match(part3.grants.long.wide$id, survey.change$id)]
final.survey$external_pi_grant_6_10_fundamental<-part3.grants.long.wide$external_pi_grant_6_10_fundamental[match(part3.grants.long.wide$id, survey.change$id)]
final.survey$external_pi_grant_6_10_use<-part3.grants.long.wide$external_pi_grant_6_10_use[match(part3.grants.long.wide$id, survey.change$id)]
head(final.survey)
dim(final.survey) #2918  40  - added 6 columns - correct

#columns  35-40
head(part3.success.all)
dim(part3.success.all)  #2981
final.survey$successful_grants_11_15_fundamental<-part3.success.all$successful_grants_11_15_fundamental[match(part3.success.all$id, survey.change$id)]
final.survey$successful_grants_11_15_use<-part3.success.all$successful_grants_11_15_use[match(part3.success.all$id, survey.change$id)]
final.survey$successful_grants_11_15_applied<-part3.success.all$successful_grants_11_15_applied[match(part3.success.all$id, survey.change$id)]
final.survey$successful_grants_6_10_fundamental<-part3.success.all$successful_grants_6_10_fundamental[match(part3.success.all$id, survey.change$id)]
final.survey$successful_grants_6_10_use<-part3.success.all$successful_grants_6_10_use[match(part3.success.all$id, survey.change$id)]
final.survey$successful_grants_6_10_applied<-part3.success.all$successful_grants_6_10_applied[match(part3.success.all$id, survey.change$id)]
head(final.survey)
dim(final.survey) #2918   46 - added 6 columns - correct

#columns  41,42
head(part3.prac.app)
dim(part3.prac.app) #2918
final.survey$practical_applications_important_11_15<-part3.prac.app$practical_applications_important_11_15[match(part3.prac.app$id, survey.change$id)]
final.survey$practical_applications_important_6_10<-part3.prac.app$practical_applications_important_6_10[match(part3.prac.app$id, survey.change$id)]
head(final.survey)
dim(final.survey) #2918  48   - added two columns - correct

#columns  43, 44
head(part3.part)
dim(part3.part)  #2918
final.survey$include_nonacademia_partners_success_11_15<-part3.part$include_nonacademia_partners_success_11_15[match(part3.part$id, survey.change$id)]
final.survey$include_nonacademia_partners_success_6_10<-part3.part$include_nonacademia_partners_success_6_10[match(part3.part$id, survey.change$id)]
head(final.survey)
dim(final.survey) #2918  50  - added two columns - correct

#columns  45-56
head(p3.master.all)
dim(p3.master.all) #2918
final.survey$distriution_funding_11_15_for_profit<-p3.master.all$distriution_funding_11_15_for_profit[match(final.survey$id, p3.master.all$id)]
final.survey$distriution_funding_11_15_government<-p3.master.all$distriution_funding_11_15_government[match(final.survey$id, p3.master.all$id)]
final.survey$distribution_funding_11_15_internal<-p3.master.all$distribution_funding_11_15_internal[match(final.survey$id, p3.master.all$id)]
final.survey$distriution_funding_11_15_nongov<-p3.master.all$distriution_funding_11_15_nongov[match(final.survey$id, p3.master.all$id)]
final.survey$distriution_funding_11_15_other<-p3.master.all$distriution_funding_11_15_other[match(final.survey$id, p3.master.all$id)]
final.survey$distriution_funding_6_10_for_profit<-p3.master.all$distriution_funding_6_10_for_profit[match(final.survey$id, p3.master.all$id)]
final.survey$distriution_funding_6_10_government<-p3.master.all$distriution_funding_6_10_government[match(final.survey$id, p3.master.all$id)]
final.survey$distriution_funding_6_10_internal<-p3.master.all$distriution_funding_6_10_internal[match(final.survey$id, p3.master.all$id)]
final.survey$distriution_funding_6_10_nongov<-p3.master.all$distriution_funding_6_10_nongov[match(final.survey$id, p3.master.all$id)]
final.survey$distriution_funding_6_10_other<-p3.master.all$distriution_funding_6_10_other[match(final.survey$id, p3.master.all$id)]
head(final.survey)
dim(final.survey) # 2918   60   - added 10 columns - correct

#columns  50, 56  (comment columns)
final.survey$distriution_funding_11_15_other_text<-survey$distriution_funding_11_15_other_text[match(part1.view$id, survey$id)]
final.survey$distriution_funding_6_10_other_text<-survey$distriution_funding_6_10_other_text[match(part1.view$id, survey$id)]
head(final.survey)
dim(final.survey)  #2918   62 - 2 columns added

#columns  57 - 59
head(part3.change)
dim(part3.change)  #2918
final.survey$success_change_10yrs_fundamental<-part3.change$success_change_10yrs_fundamental[match(part3.change$id, survey.change$id)]
final.survey$success_change_10yrs_use<-part3.change$success_change_10yrs_use[match(part3.change$id, survey.change$id)]
final.survey$success_change_10yrs_applied<-part3.change$success_change_10yrs_applied[match(part3.change$id, survey.change$id)]
head(final.survey)
dim(final.survey) #2918  65  - added 3 columns - correct

#columns  60-69
head(survey.part4)
dim(survey.part4)    #2918
final.survey$opinion_fundamental_important<-survey.part4$opinion_fundamental_important[match(survey.part4$id, survey.change$id)]
final.survey$high_priority_fundamental<-survey.part4$high_priority_fundamental[match(survey.part4$id, survey.change$id)]
final.survey$high_priority_use_inspired<-survey.part4$high_priority_use_inspired[match(survey.part4$id, survey.change$id)]
final.survey$high_priority_applied<-survey.part4$high_priority_applied[match(survey.part4$id, survey.change$id)]
final.survey$high_priority_no_change<-survey.part4$high_priority_no_change[match(survey.part4$id, survey.change$id)]
final.survey$available_funding_fundamental<-survey.part4$available_funding_fundamental[match(survey.part4$id, survey.change$id)]
final.survey$available_funding_use_inspired<-survey.part4$available_funding_use_inspired[match(survey.part4$id, survey.change$id)]
final.survey$available_funding_applied<-survey.part4$available_funding_applied[match(survey.part4$id, survey.change$id)]
final.survey$next_generation<-survey.part4$next_generation[match(survey.part4$id, survey.change$id)]
head(final.survey)
dim(final.survey) # 2918   74   - added 9 columns  - correct

#columns 65, 70, 75  (comment columns)
final.survey$high_priority_comments<-survey$high_priority_comments[match(part1.view$id, survey$id)]
final.survey$next_generation_Comments<-survey$next_generation_Comments[match(part1.view$id, survey$id)]
final.survey$final_comments<-survey$final_comments[match(part1.view$id, survey$id)]
head(final.survey)
dim(final.survey)  #2918  77  3 columns added



#ok now check to make sure everything is there
colnames(final.survey)

#looks like it 
#so now lets fix some of the column names (spelling errors - oops, making them more clear, etc)
colnames(final.survey)[colnames(final.survey)=="percent_Applied_Research_current"]<-"percent_applied_research_current"
colnames(final.survey)[colnames(final.survey)=="percent_Applied_Research_past"]<-"percent_applied_research_past"
colnames(final.survey)[colnames(final.survey)=="percent_fundemental_research_current"]<-"percent_fundamental_research_current"
colnames(final.survey)[colnames(final.survey)=="percent_Fundamental_Research_past"]<-"percent_fundamental_research_past"
colnames(final.survey)[colnames(final.survey)=="percent_Use_inspired_Research_current"]<-"percent_use_inspired_research_current"
colnames(final.survey)[colnames(final.survey)=="percent_Use_inspired_Research_past"]<-"percent_use_inspired_research_past"
colnames(final.survey)[colnames(final.survey)=="Main_reason_change_interest_related"]<-"main_reason_change_interest_related"
colnames(final.survey)[colnames(final.survey)=="Main_reason_change_Career_related"]<-"main_reason_change_career_related"
colnames(final.survey)[colnames(final.survey)=="percent_Applied_Research_current"]<-"percent_applied_research_current"
colnames(final.survey)[colnames(final.survey)=="Main_reason_change_Funding_related"]<-"main_reason_change_funding_related"
colnames(final.survey)[colnames(final.survey)=="Main_reason_change_Socially_related"]<-"main_reason_change_socially_related"
colnames(final.survey)[colnames(final.survey)=="Main_reason_change_Other"]<-"main_reason_change_other"
colnames(final.survey)[colnames(final.survey)=="partnership_outside"]<-"partnership_outside_current"
colnames(final.survey)[colnames(final.survey)=="distriution_funding_11_15_for_profit"]<-"distribution_funding_11_15_for_profit"
colnames(final.survey)[colnames(final.survey)=="distriution_funding_11_15_government"]<-"distribution_funding_11_15_government"
colnames(final.survey)[colnames(final.survey)=="distriution_funding_11_15_nongov"]<-"distribution_funding_11_15_nongov"
colnames(final.survey)[colnames(final.survey)=="distriution_funding_11_15_other"]<-"distribution_funding_11_15_other"
colnames(final.survey)[colnames(final.survey)=="distriution_funding_6_10_for_profit"]<-"distribution_funding_6_10_for_profit"
colnames(final.survey)[colnames(final.survey)=="distriution_funding_6_10_government"]<-"distribution_funding_6_10_government"
colnames(final.survey)[colnames(final.survey)=="distriution_funding_6_10_internal"]<-"distribution_funding_6_10_internal"
colnames(final.survey)[colnames(final.survey)=="distriution_funding_6_10_nongov"]<-"distribution_funding_6_10_nongov"
colnames(final.survey)[colnames(final.survey)=="distriution_funding_6_10_other"]<-"distribution_funding_6_10_other"
colnames(final.survey)[colnames(final.survey)=="distriution_funding_11_15_other_text"]<-"distribution_funding_11_15_other_text"
colnames(final.survey)[colnames(final.survey)=="distriution_funding_6_10_other_text"]<-"distribution_funding_6_10_other_text"
colnames(final.survey)[colnames(final.survey)=="next_generation_Comments"]<-"next_generation_comments"

colnames(final.survey)

# save it!

write.csv(final.survey, "data/gya_survey_clean_final.csv", row.names = FALSE)






