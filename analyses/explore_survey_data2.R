

## Read in and explore the GYA data

setwd("/Users/jpwrobinson/Documents/git_repos/gya-research")

setwd("/Users/kristinatietjen/Documents/gya-research")



## read data
survey<-read.csv("data/July-7-2016-7pm-Toronto.csv", header=FALSE)
colnames(survey)<-as.character(unlist(survey[1,]))
survey<-survey[-1,]


## remove some unnecessary columns
survey$Username<-NULL
survey$'Updated At'<-NULL
survey'Number of Saves'<-NULL


### keep complete data only
survey<-survey[survey$Status=="Complete",]

# add 'date column'
#require(stringr)
#survey$date<-str_split_fixed(survey$'Completed At', " ", 1)

colnames(survey)

survey<-subset(survey, select=c("Location", "Created At", "What is your gender?", "What is your field of research?"))
colnames(survey)<-c("Country", "Date", "Gender", "Field")
################################
#### Summary statistics ########
################################

locations<-aggregate(Gender ~ Country, survey, length)
gender<-aggregate(Country ~ Gender, survey, length)
field<-aggregate(Country ~ Field, survey, length)


ggplot(locations, aes(Country, Gender)) + geom_bar(stat="identity")

locations[order(locations$Gender, locations$Country, decreasing=TRUE),]



