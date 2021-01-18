############ Functions used for survey data analysis ############

################ Convert ms to sec ###############
milliToSec <-function(df){
  if(is.numeric(df)) 
    df <- df/1000 
  else 
    df<- as.numeric(unlist(df))/1000
  df
} 

########## Compute Recall
computePreRecallFScore <- function(df){
  df$precision <- df$TP / (df$TP + df$FP)
  df$recall <-  df$TP / (df$TP + df$FN)
  df$Fscore <-  (2 * df$precision * df$recall)/(df$precision + df$recall)
  df
}

################ Convert ms to min ###############
milliToMin <-function(df){
  if(is.numeric(df)) 
    df <- df/60000 
  else 
    df<- as.numeric(unlist(df))/60000
  df
} 

####### To get only column names having some string in their name ##########
mergeNonNA <- function (string, df){
  x <- df[ , grepl(string , names(df), ignore.case=TRUE )]
  # x <-data.frame(t(apply(x, 1, function(x) x[!is.na(x)])))
  x <-  cbind(Id =df$workerId, x)
  x <-  cbind(Set1 =df$Set1, x)
  x <-  cbind(Set2 =df$Set2, x)
}

##### function that replaces column names having a substring 'name' with the 'replacement' string
replaceColNames <- function (df, st, replacement){
  str_replace_all(colnames(df), st, replacement)
}

##### to remove user IDs and change with P1, P2 ... for plot readability
changeToParticipant <-function(df){
  rownames(df) <- seq(length=nrow(df))
  workerId <- paste("P", rownames(df), sep="")
  workerId
}


########## To prepare overview questions data
prepareOverQ <- function (Q1, st){
  Q1<- Q1[rowSums(!is.na(Q1))>1,]
  Q2 <- Q1 [,c(1,6:13)]
  
  Q2<- Q2 %>% gather("key","value",-workerId) %>% 
    filter(!is.na(value)) %>% # reverse engineer original data (if the original had NAs, you'll need this row to remove them)
    group_by(workerId) %>%
    mutate(key=paste0("R",row_number())) %>% # replace key with minimal number of keys
    spread(key,value) # spread again
  
  Q1 <- Q1[order(Q1$workerId),]
  Q2 <- Q2[order(Q2$workerId),]
  
  Q1<- data.frame(cbind(data.frame(Q2), Q1[,c(2:5)]))
  
  Q <- getDataSet(Q1,st)
  Q
} 

##### Overview Q3 formating for analysis 
prepareOverQ3 <- function (Q1, st){
  Q1<- Q1[rowSums(!is.na(Q1))>1,]
  Q2 <- Q1 [,c(1,6:13)]
  
  Q2<- Q2 %>% gather("key","value",-workerId) %>% 
    filter(!is.na(value)) %>% # reverse engineer original data (if the original had NAs, you'll need this row to remove them)
    group_by(workerId) %>%
    mutate(key=paste0("R",row_number())) %>% # replace key with minimal number of keys
    spread(key,value) # spread again
  
  Q1 <- Q1[order(Q1$workerId),]
  Q2 <- Q2[order(Q2$workerId),]
  
  Q1<- data.frame(cbind(data.frame(Q2), Q1[,c(2:6)]))
  
  Q <- getDataSet(Q1,st)
  Q
} 

#### Extract the dataset used
getDataSet <- function(Q1, st){
  Q1$d1  <- mapply(gsub, st, "", x=Q1$Set1 )
  Q1$d2  <- mapply(gsub, st, "", x=Q1$Set2 )
  Q1[c("d1", "d2")]  <- lapply(Q1[c("d1", "d2")], function(x) ifelse(grepl ("V", x), "",x) )
  Q1 <-Q1 %>% replace_na(list(d1 = "", d2 = "")) %>% unite(dataT, d1, d2, remove = FALSE, sep = "")
  Q1 <- Q1[ , !(names(Q1) %in% c("d1","d2"))]
  Q1
}

#### Extract the condition used
getVis <- function(Q1){
  Q1$V1  <- mapply(grepl, "V1", "", x=Q1$Set1 )
  Q1$V2  <- mapply(grepl, "V2", "", x=Q1$Set1 )
  Q1$Vis <- ifelse( Q1[,c("V1")],"TableFirst", "VisualFirst")
  Q1 <- Q1[ , !(names(Q1) %in% c("V1","V2"))]
  Q1
}

###### oDf is origianl data. df are the subset columns (only reports R1,R2 ..), ans is the correct answer
computepenalty <- function (oDf, df, ans){
  oDf$p <- apply(df, 1, function(r) {ifelse(any(trimws(r) %in% ans), as.numeric(sum(!is.na(r))-1) , as.numeric(sum(!is.na(r))))}); 
  oDf
}

###### oDf is origianl data. df are the subset columns (only reports R1,R2 ..), ans is the correct answer
computePenaltyNew <- function (oDf, df, ans){
  # oDf$p <- apply(df, 1, function(r) {ifelse(any(trimws(r) %in% ans), as.numeric(sum(!is.na(r))-1) , as.numeric(sum(!is.na(r))))});
  
  oDf$p <- apply(df, 1, function(r) {  print (r); ifelse(any(trimws(r) %in% c(1)) & any(trimws(r) %in% c(3)) & sum(!is.na(r) & r!='')==2, 0 ,
                                            ifelse( any(trimws(r) %in% c(1,3)) & sum(!is.na(r))==2, 1, as.numeric(sum(!is.na(r)))))}); 
  oDf
}

####### Q1 Accuracy - Rules: R1< full score, R1,4,7 = 0.9, if both 4,7 =0.5, if only one from 4,7, then 0.25, etc.
computeQ1Acc <- function (df, Q1){
  df<- mutate(df, acc = ifelse(apply(Q1, 1, function(r) { any(trimws(r) %in% c(3)) & any(trimws(r) %in% c(1))  & sum(!is.na(r))==2}),
                               1,
                               ifelse(apply(Q1, 1, function(r)  any(trimws(r) %in% c(1,3)) & sum(!is.na(r))==1),
                                      0.5, 
                                      ifelse(apply(Q1, 1, function(r) any(trimws(r) %in% c(1)) & any(trimws(r) %in% c(3))  &  sum(!is.na(r))>2), 
                                             0.75,
                                             ifelse(apply(Q1, 1, function(r) {any(trimws(r) %in% c(1)) & any(trimws(r) %in% c(3))  & any(trimws(r) %in% c(2,4,6,5,7))}),
                                                    0.5-df$p*0.1,
                                                    ifelse(apply(Q1, 1, function(r) {any(trimws(r) %in% c(1,3)) & any(trimws(r) %in% c(2,4,6,5,7))}),
                                                           0.25-df$p*0.1, 0 
                         
                                                           ))
                                      ))
  ))
  
  df$acc <- lapply(df$acc, function(x){ifelse(x<0, 0, x)}) #### negative numbers to 0
  df<- as.data.frame(lapply(df , unlist))
  df <- df[, !(names(df) %in% c("p"))] # remove the name column
  df
}


##### Penalty calculations for wrong answers
computeQ2Penalty <- function (oDf, df){
  oDf$p <- apply(df, 1, function(r) {
       ifelse(all(trimws(r[!is.na(r)]) %in% all(c(1,4,7))), 0.75,
           ifelse((any(trimws(r[!is.na(r)]) %in% c(1)) & any(trimws(r[!is.na(r)]) %in% c(4))) | (any(trimws(r[!is.na(r)]) %in% c(1)) & any(trimws(r[!is.na(r)]) %in% c(7))) | (any(trimws(r[!is.na(r)]) %in% c(7)) & any(trimws(r[!is.na(r)]) %in% c(4))), 
                  0.5,
                  ifelse(any(trimws(r[!is.na(r)]) %in% c(1,4,7)), 0.25, 0)
           )
    )
  }); 
   
  oDf
}


#######  Computing Accuracy for Q2.
computeQ2Acc <- function (Q){
  ####### computing accuracy
  x<- t(apply(Q[,c(2:4)], 1, function(r) (r %in% c(2,3,5,6))))
  
  Q$acc <- ifelse(rowCounts(x) >=2,1,
                            ifelse(rowCounts(x) ==1, 0.5, 0 ))
  Q
}


#######  Computing Accuracy for Q3. Rules - R1< full score, R1,4,7 = 0.9, if both 4,7 =0.5, if only one from 4,7, then 0.25, etc.
computeQ3Acc <- function (df, Q1){
 df<- mutate(df, acc = ifelse(apply(Q1, 1, function(r) { any(trimws(r) %in% c(1)) & sum(!is.na(r))==1}),
                              1,
                              ifelse(apply(Q1, 1, function(r)  any(trimws(r) %in% c(7)) & any(trimws(r) %in% c(4)) & sum(!is.na(r))==2),
                                     0.5, 
                                     ifelse(apply(Q1, 1, function(r) any(trimws(r) %in% c(1)) & any(trimws(r) %in% c(7)) & any(trimws(r) %in% c(4)) &  sum(!is.na(r))==3), 
                                            0.9,
                                            ifelse(apply(Q1, 1, function(r) {!any(trimws(r) %in% c(1)) & any(trimws(r) %in% c(7,4) &  sum(!is.na(r))==1)}),
                                                   0.25,
                                                   ifelse(apply(Q1, 1, function(r) {any(trimws(r) %in% c(1)) & any(trimws(r) %in% c(7,4) &  sum(!is.na(r))==2)}),
                                                          0.75, 
                                                          ifelse(apply(Q1, 1, function(r){ 
                                                            # t <- ifelse(any(trimws(r) %in% c(1)), as.numeric(sum(!is.na(r))-1) , as.numeric(sum(!is.na(r)))) ;
                                                            any(trimws(r) %in% c(1)) & !any(trimws(r) %in% c(4,7)) & sum(!is.na(r))>1}),
                                                            # 'xx',0
                                                            0.5-df$p*0.1, 
                                                            ifelse(apply(Q1, 1, function(r){
                                                              # py<-ifelse(any(trimws(r) %in% c(4,7)), as.numeric(sum(!is.na(r))-1) , as.numeric(sum(!is.na(r))));
                                                              any(trimws(r) %in% c(4,7)) & any(trimws(r) %in% c(2,3,6,5))}),
                                                              0.25-df$p*0.1, 0)
                                                          )))
                                     ))
  ))
 
 df$acc <- lapply(df$acc, function(x){print(x); ifelse(x<0, 0, x)})
 df<- as.data.frame(lapply(df , unlist))
 df <- df[, !(names(df) %in% c("p"))] # remove the name column
 df
}



