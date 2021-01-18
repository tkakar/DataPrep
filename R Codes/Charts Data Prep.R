# ###### Significance Test
fullReport(groupsTime, colnames(groupsTime[3]), "group")
fullReport(acc, colnames(acc[3]), "group")
# 
# ####### Basic Statistics
summary(TriageAction$accVisual)
sd(TriageAction$accVisual)
describe(groupsTime)

# To make sure that plots are in same order (Visual first then Table, turn the Condition/group Column to character)

############# Unrelated Drug Report (Overview)
acc<- Unrelated
names(acc) = c("Id", "Visual", "Table")
acc <- melt(acc, id.vars = "Id", value.name="Count")
names(acc)[names(acc) == 'variable'] <- "group"
acc <- acc[!is.na(acc[3]), ]
acc$group <- as.character(acc$group)
yRange <- c(min(acc[3]), max(acc[3]))
ciplot(acc, colnames(acc[3]), "group",yRange, yRange, 0, "Unrelated Drug Count")
fullReport(acc, colnames(acc[3]), "group")
reportES(acc,"Count","group")
plotParticipants (acc, acc$Id, acc$Count,acc$group, "Identification of Unrelated Issues","Participants","Unrelated Issues (#)" )


############overviewQ1
ovq <- overviewQ1[,c("workerId", "Condition","Acc")]
names(ovq) = c("Id", "group", "acc")
yRange <- c(min(ovq[3]), max(ovq[3]))
ciplotPercentage(ovq, colnames(ovq[3]), "group",yRange, yRange, 0,"Completeness Acc")
plotParticipants (ovq, ovq$Id, ovq$acc,ovq$group, "Report completeness detection Accuracy","Participants","Accuracy" , "")
fullReport(ovq, colnames(ovq[3]), "group")
reportES(ovq,"acc", "group")

########### Q2 plot (investigatve Report)
ovq <- overviewQ3Data[,c("workerId", "Condition","Acc")]
# ovq <- overviewQ3[,c("workerId", "Condition","OldAcc")]
names(ovq) = c("Id", "group", "acc")
yRange <- c(min(ovq[3]), max(ovq[3]))
ciplotPercentage(ovq, colnames(ovq[3]), "group",yRange, yRange, 0, "Investigative")
plotParticipants (ovq, ovq$Id, ovq$acc,ovq$group, "Accuracy - Detecting Most Investigative Reports","Participants","Accuracy" , "")
fullReport(ovq, colnames(ovq[3]), "group")
reportES(ovq,"acc", "group")

########### Overview Q1 & Q2 Overall plot 
ovq <- overviewOverAll[,c("workerId", "Condition","avg")]
# ovq <- overviewQ3[,c("workerId", "Condition","OldAcc")]
names(ovq) = c("Id", "group", "acc")
yRange <- c(min(ovq[3]), max(ovq[3]))
ciplotPercentage(ovq, colnames(ovq[3]), "group",yRange, yRange, 0, "Overview Overall")
# plotParticipants (ovq, ovq$Id, ovq$acc,ovq$group, "Accuracy - Detecting Most Investigative Reports","Participants","Accuracy" , "")
fullReport(ovq, colnames(ovq[3]), "group")
reportES(ovq,"acc", "group")


############# Pattern Accuracy OVERALL
acc<- pTotal[,c("workerId","Condition","Acc")]
names(acc) = c("Id", "group", "Acc")
yRange <- c(min(acc[3]), max(acc[3]))
acc$group <- as.character(acc$group)
ciplotPercentage(acc, colnames(acc[3]), "group",yRange, yRange, 0, "PatternAcc")
fullReport(acc, colnames(acc[3]), "group")
reportES(acc,"Acc", "group")
plotParticipants (acc, acc$Id, acc$Acc,acc$group, "Pattern Accuracy","Participants","Mean Accuracy" )


############# Pattern Accuracy Q0
acc<- pData[,c("workerId","Condition","Q0")]
names(acc) = c("Id", "group", "Acc")
yRange <- c(min(acc[3]), max(acc[3]))
acc$group <- as.character(acc$group)
ciplotPercentage(acc, colnames(acc[3]), "group",yRange, yRange, 0, "PatternQ0")
fullReport(acc, colnames(acc[3]), "group")
reportES(acc,"Acc", "group")
plotParticipants (acc, acc$Id, acc$Acc,acc$group, "Pattern Accuracy","Participants"," Q0Accuracy" )

############# Pattern Accuracy Q2
acc<- pData[,c("workerId","Condition","Q1")]
names(acc) = c("Id", "group", "Acc")
yRange <- c(min(acc[3]), max(acc[3]))
acc$group <- as.character(acc$group)
ciplotPercentage(acc, colnames(acc[3]), "group",yRange, yRange, 0, "PatternQ1")
fullReport(acc, colnames(acc[3]), "group")
reportES(acc,"Acc", "group")
plotParticipants (acc, acc$Id, acc$Acc,acc$group, "Pattern Accuracy","Participants","Q1Accuracy" )

############# Pattern Accuracy Q3
acc<- pData[,c("workerId","Condition","Q2")]
names(acc) = c("Id", "group", "Acc")
acc$Acc <- as.numeric(acc$Acc)
yRange <- c(min(acc[3]), max(acc[3]))
acc$group <- as.character(acc$group)
ciplotPercentage(acc, colnames(acc[3]), "group",yRange, yRange, 0, "PatternQ2")
fullReport(acc, colnames(acc[3]), "group")
reportES(acc,"Acc", "group")
plotParticipants (acc, acc$Id, acc$Acc,acc$group, "Pattern Accuracy","Participants","Q2Accuracy" )


#############  Triage Time
groupsTime<- TriageTime
names(groupsTime) = c("Id", "Visual", "Table")
groupsTime <- melt(groupsTime, id.vars = "Id", value.name="Time")
names(groupsTime)[names(groupsTime) == 'variable'] <- "group"
groupsTime <- groupsTime[!is.na(groupsTime[3]), ]
yRange <- c(min(groupsTime[3]), max(groupsTime[3]))
groupsTime$group <- as.character(groupsTime$group)
ciplot(groupsTime, colnames(groupsTime[3]), "group",yRange, yRange, 0, "TriageTime")
fullReport(groupsTime, colnames(groupsTime[3]), "group")
plotParticipants (groupsTime, groupsTime$Id, groupsTime$Time,groupsTime$group, "Time for Triage","Participants","Time in Minutes" )
reportES(groupsTime, "Time","group")

############# group data based on Triage Accuracy
acc<- TriageAction[,c("workerId",  "Condition", "acc")]
names(acc) = c("Id", "group", "acc")
names(acc)[names(acc) == 'variable'] <- "group"
acc <- acc[!is.na(acc[3]), ]
yRange <- c(min(acc[3]), max(acc[3]))
ciplotPercentage(acc, colnames(acc[3]), "group",yRange, yRange, 0, "TriageAcc")
fullReport(acc, colnames(acc[3]), "group")
plotParticipants (acc, acc$Id, acc$acc,acc$group, "Triage Accuracy","Participants"," Accuracy" )
reportES(acc, "acc","group")

############# group data based on Triage Precision
acc<- TriageAction[,c("workerId",  "Condition", "precision")]
names(acc) = c("Id", "group", "acc")
names(acc)[names(acc) == 'variable'] <- "group"
acc <- acc[!is.na(acc[3]), ]
yRange <- c(min(acc[3]), max(acc[3]))
ciplotPercentage(acc, colnames(acc[3]), "group",yRange, yRange, 0, "TriagePres")
fullReport(acc, colnames(acc[3]), "group")
plotParticipants (acc, acc$Id, acc$acc,acc$group, "Triage Precision","Participants"," Precision" )
reportES(acc, "acc","group")

############# group data based on Triage Recall
acc<- TriageAction[,c("workerId",  "Condition", "recall")]
names(acc) = c("Id", "group", "acc")
names(acc)[names(acc) == 'variable'] <- "group"
acc <- acc[!is.na(acc[3]), ]
yRange <- c(min(acc[3]), max(acc[3]))
ciplotPercentage(acc, colnames(acc[3]), "group",yRange, yRange, 0, "TriageRecall")
fullReport(acc, colnames(acc[3]), "group")
plotParticipants (acc, acc$Id, acc$acc,acc$group, "Triage Recall","Participants"," Recall" )
reportES(acc, "acc","group")

# ############# group data based on Triage Fscore
acc<- TriageAction[,c("workerId",  "Condition", "acc")]
names(acc) = c("Id", "group", "acc")
names(acc)[names(acc) == 'variable'] <- "group"
acc <- acc[!is.na(acc[3]), ]
yRange <- c(min(acc[3]), max(acc[3]))
ciplotPercentage(acc, colnames(acc[3]), "group",yRange, yRange, 0, "Fscore")
fullReport(acc, colnames(acc[3]), "group")
plotParticipants (acc, acc$Id, acc$acc,acc$group, "Triage Fscore","Participants"," Fscore" )
reportES(acc, "acc","group")

#############  Task Time
names(TaskTime) = c("Id", "Visual", "Table")
TaskTime <- melt(TaskTime, id.vars = "Id", value.name="Time")
names(TaskTime)[names(TaskTime) == 'variable'] <- "group"
TaskTime <- TaskTime[!is.na(TaskTime[3]), ]
# groupsTime[3] <- round(groupsTime[3], digits = 0)
yRange <- c(min(TaskTime[3]), max(TaskTime[3]))
TaskTime$group <- as.character(TaskTime$group)
ciplot(TaskTime, colnames(TaskTime[3]), "group",yRange, yRange, 0, "TaskTime")
plotParticipants (TaskTime, TaskTime$Id, TaskTime$Time,TaskTime$group, "Task Time","Participants","Time in Minutes" , "Time")
fullReport(TaskTime, colnames(TaskTime[3]), "group")
reportES(TaskTime,"Time", "group")

############ RecallTime

acc <- RecallTime
names(acc) = c("Id", "Visual", "Table")
acc <- melt(acc, id.vars = "Id", value.name="Time")
names(acc)[names(acc) == 'variable'] <- "group"
acc <- acc[!is.na(acc[3]), ]
acc$group <- as.character(acc$group)
yRange <- c(min(acc[3]), max(acc[3]))
ciplot(acc, colnames(acc[3]), "group",yRange, yRange, 0, "recallTime")
fullReport(acc, colnames(acc[3]), "group")
plotParticipants (acc, acc$Id, acc$acc,acc$group, "Time","Participants"," recallTime" )
reportES(acc, "Time","group")

######################### Questions each one at once
names(QuestionTimeTotal) = c("Id", "Visual", "Table")
QuestionTimeTotal <- melt(QuestionTimeTotal, id.vars = "Id", value.name="Time")
names(QuestionTimeTotal)[names(QuestionTimeTotal) == 'variable'] <- "group"
QuestionTimeTotal <- QuestionTimeTotal[!is.na(QuestionTimeTotal[3]), ]
QuestionTimeTotal$group <- as.character(QuestionTimeTotal$group)
# groupsTime[3] <- round(groupsTime[3], digits = 0)
yRange <- c(min(QuestionTimeTotal[3]), max(QuestionTimeTotal[3]))
ciplot(QuestionTimeTotal, colnames(QuestionTimeTotal[3]), "group",yRange, yRange, 0, "QuestionTimeTotal")
plotParticipants (QuestionTimeTotal, QuestionTimeTotal$Id, QuestionTimeTotal$Time,QuestionTimeTotal$group, "QuestionTimeTotal Time","Participants","Time in Minutes" , "Time")
###### Significance Test
fullReport(QuestionTimeTotal, colnames(QuestionTimeTotal[3]), "group")
reportES(QuestionTimeTotal,"Time", "group")

################ Reports Tab
names(reportsTab) = c("Id", "Table", "Visual")
PatternTime <- melt(reportsTab, id.vars = "Id", value.name="Times")
names(PatternTime)[names(PatternTime) == 'variable'] <- "group"
PatternTime <- PatternTime[!is.na(PatternTime[3]), ]
# groupsTime[3] <- round(groupsTime[3], digits = 0)
yRange <- c(min(PatternTime[3]), max(PatternTime[3]))
ciplot(PatternTime, colnames(PatternTime[3]), "group",yRange, yRange, 0, "Reports Tab Opened")
plotParticipants (PatternTime, PatternTime$Id, PatternTime$Times,PatternTime$group, "# of Times Reports Viewed","Participants","#" , "")
fullReport(PatternTime, colnames(PatternTime[3]), "group")

####### Help Reports 
df<- helpClicked[,c(1,4,5)]
names(df) = c("Id", "Visual", "Table")
df <- melt(df, id.vars = "Id", value.name="Count")
names(df)[names(df) == 'variable'] <- "group"
df <- df[!is.na(df[3]), ]
df$group <- as.character(df$group)
yRange <- c(min(df[3]), max(df[3]))
ciplot(df, colnames(df[3]), "group",yRange, yRange, 0, "HelpClicked") ##### Error due to all Zero's perhaps
fullReport(df, colnames(df[3]), "group")
reportES(df,"Count","group")
plotParticipants (df, df$Id, df$Count,df$group, "# of Times Help Clicked","Participants","#" , "")
df <-  cbind(df, helpClicked[,c(4,5)])
df <- df[,c(1,3,2,4,5)]
names(df) <-  c('workerId', 'Acc','Cond','Vis','diff')
plotBarSlopes (df, "Overall Help Clicked")


########### Conf Rating
PatternTime <- confRating
names(PatternTime) = c("Id", "group", "Rating")
PatternTime <- PatternTime[!is.na(PatternTime[3]), ]
yRange <- c(min(PatternTime[3]), max(PatternTime[3]))
ciplot(PatternTime, colnames(PatternTime[3]), "group",yRange, yRange, 0, "Conf Ratings")
plotParticipants (confRating, confRating$workerId, confRating$avgRating,confRating$Condition, "Confidence (1-least, 7 most)","Participants","Rating" , "Time")
fullReport(PatternTime,  colnames(PatternTime[3]), "group")
reportES(PatternTime,"Rating", "group")

########### Ease Rating
PatternTime <- easeRating
names(PatternTime) = c("Id", "group", "Rating")
PatternTime <- PatternTime[!is.na(PatternTime[3]), ]
yRange <- c(min(PatternTime[3]), max(PatternTime[3]))
ciplot(PatternTime, colnames(PatternTime[3]), "group",yRange, yRange, 0, "Ease Ratings")
plotParticipants (easeRating, easeRating$workerId, easeRating$avgRating,easeRating$Condition, "Perceived Ease (1-least, 7 most)","Participants","Rating" , "Time")
fullReport(PatternTime,  colnames(PatternTime[3]), "group")
reportES(PatternTime,"Rating", "group")


######################### OvrViewTime
names(OvrViewTime) = c("Id", "Visual", "Table")
OvrViewTime <- melt(OvrViewTime, id.vars = "Id", value.name="Time")
names(OvrViewTime)[names(OvrViewTime) == 'variable'] <- "group"
OvrViewTime <- OvrViewTime[!is.na(OvrViewTime[3]), ]
# groupsTime[3] <- round(groupsTime[3], digits = 0)
yRange <- c(min(OvrViewTime[3]), max(OvrViewTime[3]))
OvrViewTime$group <- as.character(OvrViewTime$group)
ciplot(OvrViewTime, colnames(OvrViewTime[3]), "group",0, 0, 0, "OverViewTime")
plotParticipants (OvrViewTime, OvrViewTime$Id, OvrViewTime$Time,OvrViewTime$group, "Overview Time","Participants","Time in Minutes" , "Time")
fullReport(OvrViewTime, colnames(OvrViewTime[3]), "group")
reportES(OvrViewTime,"Time","group")

######################### Pattern Time
names(PatternTime) = c("Id", "Visual", "Table")
PatternTime <- melt(PatternTime, id.vars = "Id", value.name="Time")
names(PatternTime)[names(PatternTime) == 'variable'] <- "group"
PatternTime <- PatternTime[!is.na(PatternTime[3]), ]
# groupsTime[3] <- round(groupsTime[3], digits = 0)
yRange <- c(min(PatternTime[3]), max(PatternTime[3]))
PatternTime$group <- as.character(PatternTime$group)
ciplot(PatternTime, colnames(PatternTime[3]), "group",yRange, yRange, 0, "PatternTime")
plotParticipants (PatternTime, PatternTime$Id, PatternTime$Time,PatternTime$group, "Pattern Time","Participants","Time in Minutes" , "Time")
###### Significance Test
fullReport(PatternTime, colnames(PatternTime[3]), "group")
reportES(PatternTime,"Time","group")


############ Explore Time
names(ExploreTime) = c("Id", "Visual", "Table")
# summary(ExploreTime$Visual)
ExploreTime <- melt(ExploreTime, id.vars = "Id", value.name="Time")
names(ExploreTime)[names(ExploreTime) == 'variable'] <- "group"
ExploreTime <- ExploreTime[!is.na(ExploreTime[3]), ]
yRange <- c(min(ExploreTime[3]), max(ExploreTime[3]))
ciplot(ExploreTime, colnames(ExploreTime[3]), "group",yRange, yRange, 0, "ExploreTime")
plotParticipants (ExploreTime, ExploreTime$Id, ExploreTime$Time,ExploreTime$group, "Explore Time","Participants","Time in Minutes" , "Time")
###### Significance Test
fullReport(ExploreTime, colnames(ExploreTime[3]), "group")
reportES(ExploreTime,"Time","group")


############ Story Time
names(StoryTime) = c("Id", "Visual", "Table")
# summary(StoryTime$Visual)
StoryTime <- melt(StoryTime, id.vars = "Id", value.name="Time")
names(StoryTime)[names(StoryTime) == 'variable'] <- "group"
StoryTime <- StoryTime[!is.na(StoryTime[3]), ]
yRange <- c(min(StoryTime[3]), max(StoryTime[3]))
ciplot(StoryTime, colnames(StoryTime[3]), "group",yRange, yRange, 0, "StoryTime")
plotParticipants (StoryTime, StoryTime$Id, StoryTime$Time,StoryTime$group, "Story Time","Participants","Time in Minutes" , "Time")
###### Significance Test
fullReport(StoryTime, colnames(StoryTime[3]), "group")
reportES(StoryTime,"Time","group")


# ##############  Triage Comments count 
ovq <- TriageComments
plotBarSlopes (ovq, "Triage Comments Count")
plotSlopeParticipants (ovq, ovq$Id, ovq$acc,ovq$group, "Accuracy - Detecting the Most Investigative Report","Participants","Accuracy" , "")


