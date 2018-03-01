rm(list=ls())

dev.off()

## Read in and explore the GYA data

#setwd("/Users/jpwrobinson/Documents/git_repos/gya-research")

setwd("/Users/kristinatietjen/Documents/git_hub/gya-research")

require(plyr); require(stringr)

## read data

#survey.w.all<-read.csv("data/Survey-Responses-Oct.3.850pm-Toronto.csv", header=TRUE)
survey.w.all<-read.csv("data/responses_kt_4Jan18.csv", header=TRUE)

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

colnames(survey.w.all)<-suveycols

## remove some unnecessary columns
survey.w.all$'Username'<-NULL
#survey.w.all$'Updated_At'<-NULL  # needed to cut the data off at 8 Nov  well remove this col after that
survey.w.all$'Number_of_Saves'<-NULL
survey.w.all$'Internal_ID'<-NULL
survey.w.all$'Language'<-NULL
survey.w.all$'Created_At'<-NULL
survey.w.all$'GET_Variables'<-NULL
survey.w.all$'Referrer'<-NULL
survey.w.all$'Weighted_Score'<-NULL
survey.w.all$'Completion_Time'<-NULL
survey.w.all$'IP_Address'<-NULL
survey.w.all$'Invite_Code'<-NULL
survey.w.all$'Invite_Email'<-NULL
survey.w.all$'Invite_Name'<-NULL
survey.w.all$'Collector'<-NULL
survey.w.all$'final_comments'<-NULL


## check number of complete vs. incomplete
table(survey.w.all$Status)    #2654 complete but we are going to cut it at 8 Nov - 4Jan18  2918 and maybe not cutting it anymore


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
survey.w.all$Country<-as.character(survey.w.all$Location)
survey.w.all$Country<-ifelse(survey.w.all$Country%in%states, 'USA', survey.w.all$Country)
survey.w.all$Country<-ifelse(survey.w.all$Country%in%prov, 'Canada', survey.w.all$Country)
survey.w.all$Country<-as.factor(survey.w.all$Country)

