# Preliminaries
setwd("C:\\Users\\Karsten\\Dropbox\\ShinyDBSampler\\LabUseCaseStudy")
library(ggplot2)
library(reshape2)
library(psy)
dat <- read.csv("SurveyResults.csv",header=T)

#list of questions that were inversely coded
flipcoded <- c(3,5,6,7,10,11)
# recode to match positive worded response
for (i in flipcoded){
  dat[,i] <- 6-dat[,i]
}
#reshape data
meltdat <- melt(dat[,1:12], measure.vars=1:12)
meltdat$set <- rep(rep(c("Ease","Concepts","Engagement"),4),each=nrow(dat))
meltdat$centered <- as.factor(meltdat$value-3)
meltdat$value <- as.factor(meltdat$value)
meltdat$question.in.set <- rep(1:4,each=3*nrow(dat))





# Questions Related to Ease of Use
easydat <- dat[,c(1,4,7,10,13)]
easydat$q7 <- 6-easydat$q7
easydat$q10 <- 6-easydat$q10
summary(easydat)
easyplotdat <- melt(easydat)
summary(easyplotdat)
qplot(value, geom="bar",data=easyplotdat , facets = variable ~ .) + 
  ggtitle("Ease of Use Question Set Responses") + xlab("Response Value")
qplot(value, geom="bar",data=easyplotdat , facets = Instructor ~ variable)+ 
  ggtitle("Ease of Use Question Set Responses by Instructor") + xlab("Response Value")
# Items have Cronbach's alpha = .696 Acceptable(Nearly Good)
cronbach(easydat[,1:4])


# Questions Related to Engagement with Census Data
conceptdat <- dat[,c(2,5,8,11,13)]
conceptdat$q5 <- 6-conceptdat$q5
conceptdat$q11 <- 6-conceptdat$q11
summary(conceptdat)
conceptplotdat <- melt(conceptdat)
summary(conceptplotdat)
qplot(value, geom="bar",data=conceptplotdat , facets = variable ~ .) + 
  ggtitle("Sampling Concepts Question Set Responses") + xlab("Response Value")
qplot(value, geom="bar",data=conceptplotdat , facets = Instructor ~ variable)+ 
  ggtitle("Sampling Concepts Question Set Responses By Instructors") + xlab("Response Value")
# Items have Cronbach's alpha = .538  Poor
cronbach(conceptdat[,1:4])



# Questions Related to Connection to Course Concepts 
engagedat <- dat[,c(3,6,9,12,13)]
engagedat$q3 <- 6-engagedat$q3
engagedat$q6 <- 6-engagedat$q6
summary(engagedat)
engageplotdat <- melt(engagedat)
summary(engageplotdat)
qplot(value, geom="bar",data=engageplotdat , facets = variable ~ .) + 
  ggtitle("Data Engagement Question Set Responses") + xlab("Response Value")
qplot(value, geom="bar",data=engageplotdat , facets = Instructor ~ variable)+ 
  ggtitle("Data Engagement Question Set Responses by Instructor") + xlab("Response Value")

# Items have Cronbach's alpha = .719   Good
cronbach(engagedat[,1:4])




