

## Read in and explore the GYA data

setwd("/Users/jpwrobinson/Documents/git_repos/gya-research")

setwd("/Users/kristinatietjen/Documents/gya-research")



## read data
survey<-read.csv("data/July-7-2016-7pm-Toronto.csv", header=FALSE)
colnames(survey)<-as.character(survey[1,])

## remove some unnecessary columns
survey$Username<-NULL
survey$'Updated At'<-NULL
survey'Number of Saves'<-NULL


### keep complete data only
survey<-survey[survey$Status="Complete",]

# add 'date column'
survey$date<-str_split_fixed(survey$'Completed At', " ", 1)

################################
#### Summary statistics ########
################################

aggregate(Status ~ Location, survey, function(x)length(unique(x)))
