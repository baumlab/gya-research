

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


### keep complete data only
survey<-survey[survey$Status=="Complete",]

# add 'date column'
#require(stringr)
#survey$date<-str_split_fixed(survey$'Completed At', " ", 1)

colnames(survey)

survey<-subset(survey, select=c("Location",  "gender", "field_research", "Country_work"))


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

################################
#### Summary statistics ########
################################

locations<-aggregate(gender ~ Country, survey, length)
gender<-aggregate(Location ~ gender, survey, length)
field<-aggregate(Location ~ field_research, survey, length)
country_work<-aggregate(gender ~ Country_work, survey, length)




locations[order(locations$gender, locations$Country, decreasing=TRUE),]


require(ggplot2)
country_work<-transform(country_work, 
          Country_work = reorder(Country_work, gender))
ggplot(country_work, aes(Country_work, gender)) + geom_bar(stat='identity') + 
  theme(axis.text.x=element_text(angle=90)) + labs(y="Number of responses") 


