

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

################################
#### Summary statistics ########
################################
survey.table<-table(survey$Country,survey$gender)



gender<-aggregate(Location ~ gender, survey, length)
head(gender)

country_work<-aggregate(gender ~ Country_work, survey, length)





require(ggplot2)
## Now plotting results
qplot(x=Country, y=gender, data=locations, geom="point", col=Country )

qplot(x=Country, y=gender, data=locations, geom="boxplot" )


qplot(x=field_research, y=Location, data=survey, geom="point", col=field_research )


ggplot(data=field, aes(x=field_research, y=Location, fill=field_research)) + geom_bar(stat='identity')

## change default background
theme_set(theme_bw())

## plot countries with number of responses
ggplot(country_work, aes(x = reorder(Country_work, -gender), gender)) + geom_bar(stat='identity',position = position_dodge(width=0.5)) + 
  theme(axis.text.x=element_blank()) + labs(y="Number of responses",x="")  + 
  geom_text(aes(label=gender), vjust=-0.25) +
geom_text(aes(label=Country_work), angle=90, hjust=-0.5) + lims(y=c(0, 1300)) 








## add a line that saves the plots in a pdf
pdf(file="figures/first_survey_responses.pdf", height=7, width=11)
  
##1 # of responses by country
locations<-aggregate(gender ~ Country, survey, length)

ggplot(country_work, aes(x = reorder(Country_work, -gender), gender)) + geom_bar(stat='identity',position = position_dodge(width=0.5)) + 
  theme(axis.text.x=element_blank()) + labs(y="Number of responses",x="")  + 
  geom_text(aes(label=gender), vjust=-0.25) +
  geom_text(aes(label=Country_work), angle=90, hjust=-0.5) + lims(y=c(0, 1300)) + theme(legend.title=element_text(size=12), 
  legend.text=element_text(size=10), axis.text=element_text(size=14), axis.title=element_text(size=16)) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())



##2a # of responses by field research 
field<-aggregate(Location ~ field_research, survey, length)

ggplot(field, aes(x = reorder(field_research, -Location), Location, fill=field_research)) + geom_bar(stat='identity',position = position_dodge(width=0.5)) + 
  theme(axis.text.x=element_blank()) + labs(y="Number of responses",x="")  + scale_fill_discrete(name="Field of Research") + 
  geom_text(aes(label=Location), vjust=-0.25) + theme(legend.position=c(0.78, 0.8)) + theme(legend.title=element_text(size=12), legend.text=element_text(size=10), axis.text=element_text(size=14), 
  axis.title=element_text(size=14)) + guides(fill=guide_legend(reverse=TRUE)) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())



##2b # of responses by field research for canada
canada<-subset(survey, Country=="Canada")

field<-aggregate(Location ~ field_research, canada, length)


ggplot(field, aes(x = reorder(field_research, -Location), Location, fill=field_research)) + geom_bar(stat='identity',position = position_dodge(width=0.5)) + 
  theme(axis.text.x=element_blank()) + labs(y="Number of responses",x="")  + scale_fill_discrete(name="Field of Research") +
  geom_text(aes(label=Location), vjust=-0.25) + theme(legend.position=c(0.78, 0.8)) + theme(legend.title=element_text(size=12), 
  legend.text=element_text(size=10), axis.text=element_text(size=14), axis.title=element_text(size=16))+ 
  guides(fill=guide_legend(reverse=TRUE)) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())



##3a # of responses by participant group 
experience<-aggregate(Location ~ what_participant_group, survey, length)

ggplot(experience, aes(x = reorder(what_participant_group, -Location), Location, fill=what_participant_group)) + geom_bar(stat='identity',position = position_dodge(width=0.5)) +
  labs(y="Number of responses", x="") +  theme(legend.position="bottom") + theme(axis.text.x=element_blank()) + 
  geom_text(aes(label=Location),size=4, vjust=-0.25) + guides(fill=guide_legend(title=NULL, reverse=TRUE, nrow=6)) + scale_fill_discrete(name="Participant Group") + 
  theme(legend.title=element_text(size=12), legend.text=element_text(size=10), axis.text=element_text(size=14), 
  axis.title=element_text(size=14))+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) 
 

##3b # of responses by participant group for canada
canada<-subset(survey, Country=="Canada")

experience<-aggregate(Location ~ what_participant_group, canada, length)

ggplot(experience, aes(x = reorder(what_participant_group, -Location), Location, fill=what_participant_group)) + geom_bar(stat='identity',position = position_dodge(width=0.5)) +
  labs(y="Number of responses", x="") +  theme(legend.position="bottom") + theme(axis.text.x=element_blank()) + 
  geom_text(aes(label=Location),size=4, vjust=-0.25)+ guides(fill=guide_legend(title=NULL, reverse=TRUE, nrow=6)) + scale_fill_discrete(name="Participant Group") + 
  theme(legend.title=element_text(size=12), legend.text=element_text(size=10), axis.text=element_text(size=14), axis.title=element_text(size=16))+ 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())



## read in non-subsetted data again
survey<-read.csv("data/July-7-2016-7pm-Toronto_simplified.csv", header=TRUE)
## 1. change all relevant variables to characters.......
survey$percent_Applied_Research_past<-as.character(survey$percent_Applied_Research_past)

require(stringr)
## remove all % symbols 
survey$percent_Applied_Research_past<-str_replace_all(survey$percent_Applied_Research_past, "[%]", "")

survey<-subset(survey, select=c("Location",  "gender", "percent_fundemental_research_current", "percent_Applied_Research_current", 
                                "percent_Use_inspired_Research_current"))


##4a type of research 50% or greater of fundemental ---?





##4b canada
canada<-subset(survey, Country=="Canada")



##5a box plot of percent of each type of research by field
fundemental<-aggregate(Location ~ percent_fundemental_research_current, survey, length)
applied<-aggregate(Location ~ percent_Applied_Research_current, survey, length)
use<-aggregate(Location ~ percent_Use_inspired_Research_current, survey, length)

ggplot()



##5b canada
canada<-subset(survey, Country=="Canada")



dev.off()
