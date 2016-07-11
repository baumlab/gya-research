

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

survey<-subset(survey, select=c("Location",  "gender", "field_research", "Country_work", "PhD_Year"))


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
survey.table<-table(survey$Country,survey$gender)


locations<-aggregate(gender ~ Country, survey, length)
gender<-aggregate(Location ~ gender, survey, length)
field<-aggregate(Location ~ field_research, survey, length)
country_work<-aggregate(gender ~ Country_work, survey, length)





require(ggplot2)
## Now plotting results
qplot(x=Country, y=gender, data=locations, geom="bar", )










theme_set(theme_bw())

## plot countries with number of responses
ggplot(country_work, aes(x = reorder(Country_work, -gender), gender)) + geom_bar(stat='identity',position = position_dodge(width=0.5)) + 
  theme(axis.text.x=element_blank()) + labs(y="Number of responses",x="")  + 
  geom_text(aes(label=gender), vjust=-0.25) +
geom_text(aes(label=Country_work), angle=90, hjust=-0.5) + lims(y=c(0, 1300)) 

## plot fields of research with number of responses
ggplot(field, aes(x = reorder(field_research, -Location), Location, fill=field_research)) + geom_bar(stat='identity',position = position_dodge(width=0.5)) + 
  theme(axis.text.x=element_blank()) + labs(y="Number of responses",x="")  + 
  geom_text(aes(label=Location), vjust=-0.25) + theme(legend.position=c(0.8, 0.8))
  
  




