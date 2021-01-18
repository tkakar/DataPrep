library(reshape2)
library(qpcR)
library(rgl)
library(plyr)
library(dplyr)
library(zoo)
library(tidyr)
library(strex)
library(stringr)
library(matrixStats)
######## to not convert date variables to e^ etc.. 
options(scipen=999)

setwd("~/SumRe-project/analysis/R scripts/R Functions")
# setwd("~/SumRe-project/analysis/R scripts/Data DSRG")
# All_Data<- read.csv("../data.csv",header=T, na.strings=c("","NA"))
All_Data<- read.csv("data.csv",header=T, na.strings=c("","NA"))
T1 <- All_Data
######### converting any null values to NA
T1[T1 == "null"] <- NA
######### Removing any columsn with empty/null values
# T1 <- T1[,colSums(is.na(T1))<nrow(T1)]
T1<-T1[!(T1$workerId =="" | T1$workerId=="DEBUG" | is.na(T1$workerId)),]

T1<-T1[rowSums(!is.na(T1)) > 5,]
T1$workerId <- changeToParticipant(T1)
# T1<-T1[!(T1$workerId =="P11"),]

################ Experiment Time ################
Experiment <- T1[,c("workerId","time_diff_experiment", "outTotalTime","intotalTime")]
# Experiment <- All_Data[,c("workerId","time_diff_experiment", "outTotalTime","intotalTime")]
Experiment[is.na(Experiment)] <- 0
Experiment[,c(2,3,4)] <- round(milliToMin(Experiment[,c(2,3,4)] ), 1)
Experiment$netTime <- Experiment[2] - Experiment[3]
Experiment$netTime <- data.frame(Experiment[,5])
summary (Experiment$netTime)
# Exp<- Experiment[Experiment$time_diff_experiment<100 & Experiment$time_diff_experiment > 20,]


################ Training Analysis ################
training <- T1[,c("workerId","time_diff_TrainingVideoTime","time_diff_TrainingTime", "time_diff_TrainingTableTime","time_diff_TrainingVisualTime" )]
######## Remove any rows with all NA values
training <- training[rowSums(is.na(training)) != ncol(training),]
###### convert time to seconds.
training[,-1] <- apply(training[,-1], 2, milliToMin)

# ############### Demo & recall ##########
DemoData <- T1[,c("workerId","age","sex","degree","cb","screen_size", "vis_experience","profession","comment1","comment2", "comment3", "comment4","comment7","preference", "fast", 'engage','appeal','accurate','easy','enjoy', "FinalFeedback")]
write.csv(DemoData, "../prolificCSVs/DemoData.csv",  na="" )

RecallData <- T1[,c("workerId","Set1", "Set2","time_diff_RecallVisual", "QuestionIVisual","QuestionCVisual", "VisualRecallfeedback",
                    "time_diff_RecallTable", "QuestionCTable", "QuestionITable", "TableRecallfeedback","time_diff_RecallTime")]
write.csv(RecallData, "../prolificCSVs/Recall.csv",  na="" )
InteractionTable <- T1[,c("workerId","tableConc", "tableDrug", "tableEllipses","tableHelp","tableHelpOverviewQ","tableHelpSearchQ","tableHelpPatternQ","tableHelpExploreQ","backVideoV1","layoutVideoV1")] 

InteractionViz<- T1[,c("workerId","iconsExpanded", "Drug", "timelineEllipses", "profileEllipses",
                       "helpClicked", "UOnset", "POnset", "NOnset", "nNME", "NME", "SAdr", "NAdr","missing", "SOutcome",
                       "NOutcome", "PDechal", "NDechal", "UDechal", "URechal", "NRechal", "PRechal", "History", "conc", "loop", "history","out","inside","helpClickedOverviewQ","helpClickedSearchQ","helpClickedPatternQ","helpClickedExploreQ", "backVideoV2","layoutVideoV2")] 

mean(InteractionTable[,-c("workerId")], na.rm=TRUE)
intrData <- data.frame(ID=InteractionTable[,1], table=rowMeans(InteractionTable[,-1]))
intrData$visual <- rowMeans(InteractionViz[,-1])



RecallTime <- T1[,c("workerId","time_diff_RecallVisual", "time_diff_RecallTable")]
RecallTime[,c(2,3)] <- milliToMin(RecallTime[,c(2,3)])

######### Rating Data ######## with recall the firs two
tRating <- T1[,c("workerId","V1OverviewQease","V1OverviewQconf","V1PatternQease","V1PatternQconf","V1StoryQease","V1StoryQconf","TableRecallconf", "TableRecallease")]  #"V1SearchQease","V1SearchQconf"
vRating <- T1[,c("workerId","V2OverviewQease","V2OverviewQconf","V2PatternQease","V2PatternQconf","V2StoryQease","V2StoryQconf","VisualRecallconf", "VisualRecallease")]
# Rating <- cbind(vRating, tRating)
# write.csv(Rating, "Rating.csv" )
vRating$Condition <- "Visual"
tRating$Condition <- "Table"
names(tRating) <- c("workerId","OverviewQease","OverviewQconf","PatternQease","PatternQconf","StoryQease","StoryQconf" ,"Recallease", "Recallconf", "Condition")
names(vRating) <- c("workerId","OverviewQease","OverviewQconf","PatternQease","PatternQconf","StoryQease","StoryQconf","Recallease","Recallconf","Condition")
Rating <- rbind(vRating, tRating)
confRating <-Rating[,c("workerId","OverviewQconf","PatternQconf","StoryQconf","Recallconf","Condition")]
easeRating <-Rating[,c("workerId","OverviewQease","PatternQease","StoryQease","Recallease","Condition")]

###### if there are any NAs
ind <- which(is.na(confRating[,2:4]), arr.ind=TRUE)
ind[,2] <- ind[,2]+1   ###### Which returns column index 1 less than the actual number
confRating[ind] <- rowMeans(confRating[,2:4],na.rm = TRUE)[ind[,1]]
confRating$avgRating <- round(rowMeans(confRating[,2:5]),2)
confRating <- confRating[,c(1,6,7)]

###### if there are any NAs
ind <- which(is.na(easeRating[,2:5]), arr.ind=TRUE)
ind[,2] <- ind[,2]+1   ###### Which returns column index 1 less than the actual number
easeRating[ind] <- rowMeans(easeRating[,2:5],na.rm = TRUE)[ind[,1]]
easeRating$avgRating <- round(rowMeans(easeRating[,2:5]),2)
easeRating <- easeRating[,c(1,6,7)]
############### Triage Time and Comments  ##########
TriageTime <- T1[,c("workerId","time_diff_Task1TriageTimeV2","time_diff_Task1TriageTimeV1")]
TriageTime[,c(2,3)] <- milliToMin(TriageTime[,c(2,3)]) # for CI this is fine
#for slope charts
tT <- T1[,c("workerId","Set1", "Set2","time_diff_Task1TriageTimeV1")]
tV <- T1[,c("workerId","Set1", "Set2","time_diff_Task1TriageTimeV2")]
tV$Condition <- "Visual"
tT$Condition <- "Table"
tV <- getVis(tV)
tT <- getVis(tT)

names(tT) =c("workerId","Set1", "Set2","Time", "Condition","Vis")
names(tV) =c("workerId","Set1", "Set2","Time", "Condition","Vis")

tT$diff <-  ifelse(tV$Time==0 & tT$Time==0, 100, tV$Time - tT$Time)
tV$diff <-  ifelse(tV$Time==0 & tT$Time==0, 100, tV$Time - tT$Time)

TriageT<- rbind.fill (tV, tT)
TriageT[,c(4,7)] <- round(milliToMin(TriageT[,c(4,7)]),2)

TriageT <- TriageT[,c("workerId", "Time", "Condition", "Vis", "diff")]
plotBarSlopes(TriageT, "TriageTime")

############## Triage Action

TriageActionVisual <-T1[,c("workerId","Set1", "Set2", "V2reportsNo","V2Action1","V2Action2","V2Action3","V2Action4","V2Action5","V2Action6","V2Action7", "V2Action8")]
TriageActionVisual <- getDataSet (TriageActionVisual,"V2")

TriageActionTable <- T1[,c("workerId","Set1", "Set2", "V1reportsNo","V1Action1","V1Action2","V1Action3","V1Action4","V1Action5","V1Action6","V1Action7", "V1Action8")]
TriageActionTable <- getDataSet (TriageActionTable,"V1")

action <- cbind(TriageActionVisual, TriageActionTable)
# write.csv(action, "../prolificCSVs/action.csv" )

####### replacing columnnames with R
names(TriageActionTable) = sapply(names(TriageActionTable), function(n) gsub("V1Action","R", n))
names(TriageActionVisual) = sapply(names(TriageActionVisual), function(n) gsub("V2Action","R", n))
# 
# ####### dropping columns
# TriageActionTable <- TriageActionTable[ , !(names(TriageActionTable) %in% c("d1","d2", "Set1","Set2","V1reportsNo"))]
# TriageActionVisual <- TriageActionVisual[ , !(names(TriageActionVisual) %in% c("d1","d2","Set1","Set2","V2reportsNo"))]

######## Calculate accuracy of answers
TriageActionVisual <- getVis(TriageActionVisual)
TriageActionTable <- getVis(TriageActionTable)

TriageActionTable <- TriageActionTable %>% 
  mutate(R1 = ifelse(as.character(R1) == "I", "true", "false"))
TriageActionTable <- TriageActionTable %>% 
  mutate(R5 = ifelse(as.character(R5) == "I", "true", "false"))
TriageActionTable <- TriageActionTable %>% 
  mutate(R6 = ifelse(as.character(R6) == "I", "true", "false"))
TriageActionTable <- TriageActionTable %>% 
  mutate(R8 = ifelse(as.character(R8) == "I", "true", "false"))
TriageActionTable <- TriageActionTable %>% 
  mutate(R2 = ifelse(as.character(R2) == "N", "true", "false"))
TriageActionTable <- TriageActionTable %>% 
  mutate(R3 = ifelse(as.character(R3) == "N", "true", "false"))
TriageActionTable <- TriageActionTable %>% 
  mutate(R4 = ifelse(as.character(R4) == "N", "true", "false"))
TriageActionTable <- TriageActionTable %>% 
  mutate(R7 = ifelse(as.character(R7) == "N", "true", "false"))

TriageActionVisual <- TriageActionVisual %>% 
  mutate(R1 = ifelse(as.character(R1) == "I", "true", "false"))
TriageActionVisual <- TriageActionVisual %>% 
  mutate(R5 = ifelse(as.character(R5) == "I", "true", "false"))
TriageActionVisual <- TriageActionVisual %>% 
  mutate(R6 = ifelse(as.character(R6) == "I", "true", "false"))
TriageActionVisual <- TriageActionVisual %>% 
  mutate(R8 = ifelse(as.character(R8) == "I", "true", "false"))
TriageActionVisual <- TriageActionVisual %>% 
  mutate(R2 = ifelse(as.character(R2) == "N", "true", "false"))
TriageActionVisual <- TriageActionVisual %>% 
  mutate(R3 = ifelse(as.character(R3) == "N", "true", "false"))
TriageActionVisual <- TriageActionVisual %>% 
  mutate(R4 = ifelse(as.character(R4) == "N", "true", "false"))
TriageActionVisual <- TriageActionVisual %>% 
  mutate(R7 = ifelse(as.character(R7) == "N", "true", "false"))

TriageActionVisual <- TriageActionVisual %>% mutate(count_na = rowSums(is.na(.)))
TriageActionTable <- TriageActionTable %>% mutate(count_na = rowSums(is.na(.)))


TriageActionVisual$TP <- rowSums(TriageActionVisual[,c(5,9,10,12)] =='true', na.rm = TRUE)
TriageActionVisual$FP <- rowSums(TriageActionVisual[,c(5,9,10,12)] =='false', na.rm = TRUE)
TriageActionVisual$TN <- rowSums(TriageActionVisual[,c(6,7,8,11)] =='true', na.rm = TRUE)
TriageActionVisual$FN <- rowSums(TriageActionVisual[,c(6,7,8,11)] =='false', na.rm = TRUE)

TriageActionTable$TP <- rowSums(TriageActionTable[,c(5,9,10,12)] =='true', na.rm = TRUE)
TriageActionTable$FP <- rowSums(TriageActionTable[,c(5,9,10,12)] =='false', na.rm = TRUE)
TriageActionTable$TN <- rowSums(TriageActionTable[,c(6:8,11)] =='true', na.rm = TRUE)
TriageActionTable$FN <- rowSums(TriageActionTable[,c(6,7,8,11)] =='false', na.rm = TRUE)

TriageActionVisual <- computePreRecallFScore (TriageActionVisual)
TriageActionTable <- computePreRecallFScore (TriageActionTable)

mean(TriageActionTable$Fscore, na.rm=TRUE)
mean(TriageActionVisual$Fscore)

TriageActionVisual$acc <- rowSums(TriageActionVisual[,5:12] =='true', na.rm = TRUE)/(8-TriageActionVisual$count_na)
TriageActionTable$acc<- rowSums(TriageActionTable[,5:12] =='true', na.rm = TRUE)/(8-TriageActionTable$count_na)

TriageActionVisual$Condition <- "Visual"
TriageActionTable$Condition <- "Table"

mean(TriageActionVisual$acc)
mean(TriageActionTable$acc)

#for slope charts
tT <- TriageActionTable[,c("workerId", "acc", "Condition", "Vis", "precision", 'recall','Fscore')]
tV <- TriageActionVisual[,c("workerId", "acc", "Condition", "Vis","precision", 'recall','Fscore')]

tT$diff <-  ifelse(tV$acc==0 & tT$acc==0, 100, tV$acc - tT$acc)
tV$diff <-  ifelse(tV$acc==0 & tT$acc==0, 100, tV$acc - tT$acc)

TriageAction<- rbind.fill (tV, tT)

TriageAction <- TriageAction[,c("workerId", "acc", "Condition", "Vis", "diff", "precision", 'recall','Fscore')]
plotBarSlopes(TriageAction, "TriageAction Acc")


#############################  Task1Time ######################
"time_diff_demoQual"
TaskTime <- T1[,c("workerId","time_diff_Task1TimeV2", "time_diff_Task1TimeV1")]
TaskTime[,c(2,3)] <- milliToMin(TaskTime[,c(2,3)] )


#############################  QuestionsTime ######################
OvrViewTime <- T1[,c("workerId","time_diff_T1QueV2OverviewQ", "time_diff_T1QueV1OverviewQ")]
OvrViewTime[,c(2,3)] <- milliToMin(OvrViewTime[,c(2,3)])


PatternTime <- T1[,c("workerId","time_diff_T1QueV2PatternQ", "time_diff_T1QueV1PatternQ")]
PatternTime[,c(2,3)] <- milliToMin(PatternTime[,c(2,3)] )


ExploreTime <- T1[,c("workerId","time_diff_T1QueV2ExploreQ", "time_diff_T1QueV1ExploreQ")]
ExploreTime[,c(2,3)] <- milliToMin(ExploreTime[,c(2,3)] )


StoryTime <- T1[,c("workerId","time_diff_T1QueV2StoryQ", "time_diff_T1QueV1StoryQ")]
StoryTime[,c(2,3)] <- milliToMin(StoryTime[,c(2,3)] )


QuestionTimeTotal <- T1[,c("workerId","time_diff_totalQuestionTimeV2", "time_diff_totalQuestionTimeV1")]
QuestionTimeTotal[,c(2,3)] <- milliToMin(QuestionTimeTotal[,c(2,3)] )

# ExploreTime <- ExploreTime[rowSums(!is.na(ExploreTime))>1,]
# ExploreReports <- T1[,c("workerId","V2reportTabOpenedExploreQ", "V1reportTabOpenedExploreQ")]
######################### Interactions Data #######################

tableReportsOpened <- T1[,c("workerId","V1reportsOpened","V1reportTabOpenedSearchQ", "V1reportTabOpenedPatternQ","V1reportTabOpenedStoryQ" ,"V1reportTabOpenedExploreQ", "V1reportTabOpenedOverviewQ")]
visualReportsOpened <- T1[,c("workerId","V2reportsOpened","V2reportTabOpenedSearchQ", "V2reportTabOpenedPatternQ", "V2reportTabOpenedStoryQ","V2reportTabOpenedExploreQ", "V2reportTabOpenedOverviewQ")]

reportsTab <- tableReportsOpened[,c("workerId", "V1reportsOpened")]
reportsTab <- cbind(reportsTab, visualReportsOpened[,c( "V2reportsOpened")])

# ReportsOpened <- cbind(tableReportsOpened,visualReportsOpened)
reportsTab <- reportsTab[,c(1,3,2)]
# write.csv(ReportsOpened, "../prolificCSVs/reportsTab.csv" )

####################### Explore Questions
exploreQuestionT <- T1[,c("workerId", "Set1","Set2", "V1ExploreQ0", "V1StoryQ0", "V1reportsNo")]
exploreQuestionT <- getDataSet(exploreQuestionT,"V1")
exploreQuestionT <- getVis(exploreQuestionT)
exploreQuestionV <- T1[,c("workerId", "Set1","Set2","V2ExploreQ0", "V2StoryQ0",'V2reportsNo')]
exploreQuestionV <- getDataSet(exploreQuestionV,"V2")
exploreQuestionV <- getVis(exploreQuestionV)
explore <- cbind(exploreQuestionV, exploreQuestionT)
# write.csv(explore, "../prolificCSVs/explore.csv" )

####################  Pattern

patternQuestionT <- T1[,c("workerId", "V1reportsNo","Set1","Set2", "V1PatternQ0", "V1PatternQ1", "V1PatternQ2")]
patternQuestionT <- getDataSet(patternQuestionT,"V1")

patternQuestionV <- T1[,c("workerId","V2reportsNo", "Set1","Set2","V2PatternQ0", "V2PatternQ1", "V2PatternQ2")]
patternQuestionV <- getDataSet(patternQuestionV,"V2")

patternQuestionT <- getVis(patternQuestionT)
patternQuestionV <- getVis(patternQuestionV)
pattern <- cbind(patternQuestionV, patternQuestionT)


####################  Overview  Q1
Q1T <- T1[,c("workerId", "reportsNo"="V1reportsNo","Set1","Set2","Reason"="V1OverviewQ1", "V1OverviewQ0R1","V1OverviewQ0R2","V1OverviewQ0R3","V1OverviewQ0R4","V1OverviewQ0R5","V1OverviewQ0R6","V1OverviewQ0R7", "V1OverviewQ0R8")]
overviewQ1T <- prepareOverQ(Q1T,"V1")

Q1V <- T1[,c("workerId", "reportsNo"="V2reportsNo","Set1","Set2","Reason"="V2OverviewQ1", "V2OverviewQ0R1","V2OverviewQ0R2","V2OverviewQ0R3","V2OverviewQ0R4","V2OverviewQ0R5","V2OverviewQ0R6","V2OverviewQ0R7","V2OverviewQ0R8")]
overviewQ1V <- prepareOverQ(Q1V, "V2")

####### compute penalty for wrong data (anything that is not 1,3)
overviewQ1V <- computepenalty (overviewQ1V,overviewQ1V[ ,grepl("[R][1-8]" , names(overviewQ1V), ignore.case=FALSE)], c(1,3))
overviewQ1T <- computepenalty (overviewQ1T,overviewQ1T[ ,grepl("[R][1-8]" , names(overviewQ1T), ignore.case=FALSE)], c(1,3))


#compute accuracy
overviewQ1V <- computeQ1Acc(overviewQ1V,overviewQ1V[ ,grepl("[R][1-8]" , names(overviewQ1V), ignore.case=FALSE)])
overviewQ1T <- computeQ1Acc(overviewQ1T,overviewQ1T[ ,grepl("[R][1-8]" , names(overviewQ1T), ignore.case=FALSE)])

mean(overviewQ1T$acc)
mean(overviewQ1V$acc)

overviewQ1V$Condition <- "Visual"
overviewQ1T$Condition <- "Table"
overviewQ1V <- getVis(overviewQ1V)
overviewQ1T <- getVis(overviewQ1T)

names(overviewQ1T) <- c("workerId","R1", "R2" ,"reportsNo" , "Set1","Set2", "Reason", "dataT", "Acc","Condition","Vis")
names(overviewQ1V) <- c("workerId","R1", "R2" ,"reportsNo" , "Set1","Set2", "Reason", "dataT", "Acc", "Condition", "Vis")

overviewQ1T$diff <-  ifelse(overviewQ1V$Acc==0 & overviewQ1T$Acc==0, 100, overviewQ1V$Acc - overviewQ1T$Acc)
overviewQ1V$diff <-  ifelse(overviewQ1V$Acc==0 & overviewQ1T$Acc==0, 100, overviewQ1V$Acc - overviewQ1T$Acc)

overviewQ1<- rbind.fill (overviewQ1V, overviewQ1T)
# overviewQ1<- cbind (overviewQ1V, overviewQ1T)
# write.csv(overviewQ1, "OverviewQ1.csv" )

## For slope charts
overviewQ1 <- overviewQ1[,c("workerId","Acc", "Condition", "Vis", "diff")]
# plotLineSlope (overviewQ1, '', "Accuracy for Completeness Question")
# plotLineSlope(overviewQ1, 'facet', "Accuracy for Completeness Question")
plotBarSlopes(overviewQ1, "Completeness Question's Acc Difference")


################# Counting Unrelated Issues as Investigative

Q2T <- T1[,c("workerId", "V1reportsNo","Set1","Set2","V1OverviewQ3", "V1OverviewQ2R1","V1OverviewQ2R2","V1OverviewQ2R3","V1OverviewQ2R4","V1OverviewQ2R5","V1OverviewQ2R6","V1OverviewQ2R7","V1OverviewQ2R8")]
overviewQ2T <- prepareOverQ(Q2T,"V1")

Q2V <- T1[,c("workerId", "V2reportsNo","Set1","Set2","V2OverviewQ3", "V2OverviewQ2R1","V2OverviewQ2R2","V2OverviewQ2R3","V2OverviewQ2R4","V2OverviewQ2R5","V2OverviewQ2R6","V2OverviewQ2R7","V2OverviewQ2R8")]
overviewQ2V <- prepareOverQ(Q2V, "V2")

overviewQ2V$Condition <- "Visual"
overviewQ2T$Condition <- "Table"

overviewQ2V <- getVis(overviewQ2V)
overviewQ2T <- getVis(overviewQ2T)

# overviewQ2T<- overviewQ2T[,c("workerId","R1","R2","R3","R4")]
####### count of unrelated drug (R6 & R8) per participant, and avoid converting it into factors
overviewQ2T$Count <- as.numeric(as.character(ifelse(apply(overviewQ2T, 1, function(r) { any(trimws(r) %in% c(5)) & any(trimws(r) %in% c(8))}), 2,
                      ifelse(apply(overviewQ2T, 1, function(r) { any(trimws(r) %in% c(5)) | any(trimws(r) %in% c(8))}), 1,0))))


# overviewQ2V<- overviewQ2V[,c("workerId","R1","R2","R3","R4","R5")]
####### count of unrelated drug (R5 & R8) per participant (having percentage, 2/2=1, 1/2 = 0.5 fpr plotting)
overviewQ2V$Count <- as.numeric(as.character(ifelse(apply(overviewQ2V, 1, function(r) { any(trimws(r) %in% c(5)) & any(trimws(r) %in% c(8))}), 2,
                      ifelse(apply(overviewQ2V, 1, function(r) { any(trimws(r) %in% c(5)) | any(trimws(r) %in% c(8))}), 1,0))))

Unrelated<-  data.frame(overviewQ2V$workerId, overviewQ2V$Count, overviewQ2T$Count)
names(Unrelated) =c("workerId","vCount", "tCount")

overviewQ2T$diff <-  ifelse(overviewQ2V$Count==0 & overviewQ2T$Count==0, 100, overviewQ2V$Count - overviewQ2T$Count)
overviewQ2V$diff <-  ifelse(overviewQ2V$Count==0 & overviewQ2T$Count==0, 100, overviewQ2V$Count - overviewQ2T$Count)

# for slope charts
overviewQ2<- rbind.fill (overviewQ2V, overviewQ2T)
overviewQ2 <- overviewQ2[,c("workerId", "Count", "Condition", "Vis", "diff")]
colnames(overviewQ2)[2] ="Acc"
# plotLineSlope (overviewQ2, '', "Identification of Unrelated Issues")
# plotLineSlope(overviewQ2, 'facet', "Identification of Unrelated Issues (#)")
plotBarSlopes(overviewQ2, "Identification of Unrelated Issues (Table-Vis) Difference")
# Unrelated <- data.frame(cbind(tData$workerId, as.numeric(as.character(vData$vCount)), as.numeric(as.character(tData$tCount))))


################### Question 3 ---- Accuracy of Investigative Reports
overviewQ3T <- T1[,c("workerId", "V1reportsNo","Set1","Set2","V1OverviewQ3", "V1OverviewQ2R1","V1OverviewQ2R2","V1OverviewQ2R3","V1OverviewQ2R4","V1OverviewQ2R5","V1OverviewQ2R6","V1OverviewQ2R7","V1OverviewQ2R8")]
overviewQ2T <- prepareOverQ3(overviewQ3T,"V1")

overviewQ3V <- T1[,c("workerId", "V2reportsNo","Set1","Set2","V2OverviewQ3", "V2OverviewQ2R1","V2OverviewQ2R2","V2OverviewQ2R3","V2OverviewQ2R4","V2OverviewQ2R5","V2OverviewQ2R6","V2OverviewQ2R7","V2OverviewQ2R8")]
overviewQ2V <- prepareOverQ3(overviewQ3V, "V2")

overviewQ3<- cbind (overviewQ3V, overviewQ3T)
# write.csv(overviewQ3, "OverviewQ3.csv" )

#### Export, calculate penalty manually and then import again
overviewQ3<- read.csv("OverviewQ3Analyzed.csv",header=T, na.strings=c("","NA"))
overviewQ3$Condition<- as.character(overviewQ3$Condition) 
overviewQ3 <- overviewQ3[rowSums(!is.na(overviewQ3))>1,]
# overviewQ3 <- overviewQ3[!(overviewQ3$workerId=="P11"),]
overviewQ3Data <- overviewQ3[,c("workerId", "Acc", "Condition")] ##### for CI and other charts
qt <- overviewQ3[,c("workerId", "Acc", "Condition", "diff")]
plotBarSlopes(qt, "Investigative Diff")

####################### Overview Question Comments

QT <- T1[,c("workerId", "V1reportsNo","Set1","Set2","V1OverviewQ1","V1OverviewQ3","V1OverviewQ5")]
QV <- T1[,c("workerId", "reportsNo"="V2reportsNo","Set1","Set2","V2OverviewQ1","V2OverviewQ3","V2OverviewQ5")]

QV$Condition <- "Visual"
QT$Condition <- "Table"

QV <- getVis(QV)
QT <- getVis(QT)

names(QT) <- c("workerId","reportsNo" , "Set1","Set2", "Q1", "Q3", "Q5","Condition", "Vis")
names(QV) <- c("workerId","reportsNo" , "Set1","Set2", "Q1", "Q3", "Q5","Condition", "Vis")
QTV<- rbind.fill (QV, QT)
write.csv(QTV, "OverviewQual.csv" )

################### Ease Conf Feedback #######
feedbackT <- T1[,c("workerId", "V1PatternQfeedback","V1OverviewQfeedback","TableRecallfeedback")]
feedbackV <- T1[,c("workerId","V2PatternQfeedback","V2OverviewQfeedback","VisualRecallfeedback")]


##################  Help Clicked "Help for both - in Visual, help is hidden so it may effect the results"##########
helpClickedReal <- T1[,c("workerId","Set1","Set2","helpClicked","tableHelp")]
helpClickedReal <- getVis(helpClickedReal)
helpClickedReal$diff <-  helpClickedReal$helpClicked - helpClickedReal$tableHelp
helpClickedReal <- helpClickedReal[,c(1,4:7)]


##################  Help Clicked "Table Help and Visual Icons are similar looking so comping these two" ##########
helpClicked <- T1[,c("workerId","Set1","Set2","iconsExpanded","tableHelp")]
helpClicked <- getVis(helpClicked)
helpClicked$diff <-  helpClicked$iconsExpanded - helpClicked$tableHelp
write.csv(helpClicked, "helpClicked.csv" )
# helpClicked <- helpClicked[,c(1,4:7)]
plotBarSlopes(helpClicked, "Help Diff")

############## Pattern Accuracy for charts after data is scored manually
pData<- read.csv("patternAnalyzed.csv",header=T, na.strings=c("","NA"))
pData$Condition<- as.character(pData$Condition) 
pData <- pData[rowSums(!is.na(pData))>1,]
# pData <- pData[!(pData$workerId=="P11"),]
pTotal <- pData[,c("workerId", "Acc", "Condition")] ##### for CI and other charts
colnames(pData)[8] <- "diff"
pt <- pData[,c("workerId", "Acc", "Condition", "Vis", "diff")]
plotBarSlopes(pt, "pattern Diff")


OverviewQ1Export <- cbind(overviewQ1V, overviewQ1T)
write.csv(OverviewQ1Export, "../prolificCSVs/OverviewQ1.csv",  na="" )

write.csv(Rating, "../prolificCSVs/Rating.csv",  na="" )

overviewOverAll <- overviewQ3Data[,c('workerId', 'Acc', 'Condition')]
overviewOverAll <-  cbind(overviewOverAll, 'Q1'= overviewQ1$Acc)          
overviewOverAll$avg <-  (overviewOverAll$Acc+ overviewOverAll$Q1) /2
