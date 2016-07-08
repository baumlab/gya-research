

## Read in and explore the GYA data

setwd("/Users/jpwrobinson/Documents/git_repos/gya-research")

setwd("/Users/kristinatietjen/Documents/gya-research")



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

survey<-subset(survey, select=c("Location",  "gender", "field_research"))

################################
#### Summary statistics ########
################################

locations<-aggregate(gender ~ Location, survey, length)
gender<-aggregate(Location ~ gender, survey, length)
field<-aggregate(Location ~ field_research, survey, length)


head(locations)

locations[order(locations$gender, locations$Location, decreasing=TRUE),]



