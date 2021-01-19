library(boot)
library(ggplot2)
library (dplyr)
library(Hmisc)
library(readr)
library(magrittr)
library(ggrepel)
library(scales)
library(bootES)
# library(psych)


# T2Time_bkp <- T2Time
# T2Time[T2Time== "null"] <- NA

fullReport <- function(data, y, group, yRange=0, paired=TRUE){
  data['group_'] <- data[group]
  data['y_'] <- data[y]
  
  wt <- wilcox.test(y_ ~ group_, data, conf.int=TRUE,paired=paired)
  print(wt)
}

plotParticipants <- function(df, X, Y, group, title,xlabel, ylabel, ystat){
  group.colors <- c("Visual" = "#998EC3", "Table"= "#F1A340")
  p <-  ggplot(df) +
        aes(x = X , y=Y, colour = group, group=group) +
        ggtitle(title) +
        # geom_jitter(position = position_jitter(width = 0.2, height = 0))+
        geom_line(position=position_jitter(width = 0.2, height = 0)) +
        scale_color_manual(values= group.colors, name = group)+
        labs(x = xlabel, y = ylabel) +
        theme_bw() +
        theme(legend.title = element_blank()) 
        theme(plot.title = element_text(hjust = 0.5))
  
  p
  ggsave(paste( title,".pdf" ,sep=""), width =7, height = 3, dpi = 120)
}

mean.fun <- function(D, d) {
  return( mean(D[d]) )
}


reportES <- function(data, attr, group) {
  # if(group=="Visual"){
  b <- bootES(data, 
              data.col=attr, 
              group.col=group, 
              contrast=c(Visual=1,Table=-1), 
              effect.type="cohens.d"
  )
  
  cat( "d=",round( b$t0, 2), "~", 
       "[", round( b$bounds[1], 2), ",", 
       round( b$bounds[2], 2), "]", 
       sep="")
  # }
}


######## Confidence Interval ############ 
ciplot <- function(data, y, x, yRange, xRange, gap=0, cTitle) {
  group.colors <- c("Visual" = "#998EC3", "Table"= "#F1A340")
  data['x_'] <- data[x]
  data['y_'] <- data[y]
  data[['x_']] <- factor(data[['x_']])
  groups <- group_by_(data, 'x_')
  # print (y)debugonce
  # yRange <- c(min(data[y]), max(data[y]))
  
  
  # So far the only way to enable string as param, ###### change na.omit if problem occurse
  groupedData <- dplyr::summarize(groups,
                                  mean=mean(y_),
                                  UCI= boot.ci(na.omit(boot(y_, statistic = mean.fun, R=1000, sim="ordinary")))$bca[,5],
                                  LCI= boot.ci(na.omit(boot(y_, statistic = mean.fun, R=1000, sim="ordinary")))$bca[,4])

  # print(UCI)
  df <- data.frame(
    trt = factor(groupedData[[1]]),
    resp = groupedData[["mean"]],
    group = factor(groupedData[[1]]),
    upper = c(groupedData[["UCI"]]),
    lower = c(groupedData[["LCI"]])
  )
  
  print (df)
  p <- ggplot(df, aes(trt, resp, color = group))
  p <- p + scale_color_manual(values=group.colors, name = y)
  p <- p + theme(axis.title=element_text(size=20), axis.text=element_text(size=20))
  p <- p + geom_pointrange(aes(ymin = lower, ymax = upper))
  p <- p + expand_limits(y = yRange)
  # p <- p + ylim(0, 30) ##### uncomment when want similar scale/gap for Time information
  # p <- p + scale_y_continuous(limits=c(1, 7), breaks=c(1:7)) #### uncomment for EAse and Conf, and comment expand_limit line
  p <- p + scale_y_continuous(limits=c(0, 10), breaks=seq(0,10,by=2))
  # p <- p + scale_y_continuous(limits=c(0, 6), breaks=seq(0,6,by=1))
  # p <- p + scale_x_discrete(expand=c(0,2))
  p <- p + ylab("")
  p <- p + xlab("")
  p <- p + geom_errorbar(aes(ymin = lower, ymax = upper, color=group), width = 0.2, size=1)
  p <- p + coord_flip()
  p <- p + theme_bw()
  p <- p + theme(plot.title=element_text(hjust=0))
  p <- p + theme(panel.border=element_blank())
  p <- p + theme(panel.grid.minor=element_blank())
  p <- p + theme(axis.ticks=element_blank())
  p <- p + theme(axis.text.y = element_blank())
  p <- p+  theme(axis.text.x = element_text(size = 14,  colour = "black" ))
  p <- p + theme(legend.position = "none")

  p
  ggsave(paste( cTitle, y,".pdf" ,sep=""), width =5, height = 1, dpi = 120)
}


######## Confidence Interval with %s ############ 
ciplotPercentage <- function(data, y, x, yRange, xRange, gap=0, cTitle) {
  # data <- ddply(data, c('group'))
  # data %>% arrange(desc(group))
  group.colors <- c("Visual" = "#998EC3", "Table"= "#F1A340")
  data['x_'] <- data[x]
  data['y_'] <- data[y]
  data[['x_']] <- factor(data[['x_']])
  groups <- group_by_(data, 'x_')
  # print (y)debugonce
  # yRange <- c(min(data[y]), max(data[y]))
  
  
  # So far the only way to enable string as param, ###### change na.omit if problem occurse
  groupedData <- dplyr::summarize(groups,
                                  mean=mean(y_),
                                  UCI= boot.ci(na.omit(boot(y_, statistic = mean.fun, R=1000, sim="ordinary")))$bca[,5],
                                  LCI= boot.ci(na.omit(boot(y_, statistic = mean.fun, R=1000, sim="ordinary")))$bca[,4])
  
  # print(UCI)
  df <- data.frame(
    trt = factor(groupedData[[1]]),
    resp = groupedData[["mean"]],
    group = factor(groupedData[[1]]),
    upper = c(groupedData[["UCI"]]),
    lower = c(groupedData[["LCI"]])
  )
  
  print (df)
  
  
  p <- ggplot(df, aes(trt, resp, color = group))
  p <- p + scale_color_manual(values=group.colors, name = y)
  p <- p + theme(axis.title=element_text(size=20), axis.text=element_text(size=18))
  p <- p + geom_pointrange(aes(ymin = lower, ymax = upper))
  p <- p + expand_limits(y = yRange)
  p <- p + ylab("")
  p <- p + xlab("")
  p <- p + scale_y_continuous(labels =percent)
  p <- p + geom_errorbar(aes(ymin = lower, ymax = upper, color=group), width = 0.2, size=1)
  p <- p + coord_flip()
  p <- p + theme_bw()
  p <- p + theme(plot.title=element_text(hjust=0))
  p <- p + theme(panel.border=element_blank())
  p <- p + theme(panel.grid.minor=element_blank())
  p <- p + theme(axis.ticks=element_blank())
  p <- p + theme(axis.text.y = element_blank())
  p <- p+  theme(axis.text.x = element_text(size = 14,  colour = "black" ))
  p <-p + theme(legend.position = "none")
  # p <- p + scale_color_discrete()
  p
  ggsave(paste( cTitle, y,".pdf" ,sep=""), width =5, height = 1, dpi = 120)
}


plotSlopeParticipants <- function(df, X, Y, group, title,xlabel, ylabel, ystat){
  group.colors <- c("Visual" = "#998EC3", "Table"= "#F1A340")
  
  p <- ggplot(df) +
    # aes(x = Y , y=X, colour = group, group=group) +
    ggtitle(title) +
    geom_point(aes(x = as.factor(group), y = X, group = Y, color = group), size = 3) +
    geom_line(aes(x = as.factor(group), y = X, group = Y, color = group), size = 1) +
    scale_color_manual(values= group.colors, name = group)+
    labs(x = xlabel, y = ylabel) +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5))
  p
  ggsave(paste( title, ".pdf" ,sep=""), width =4, height = 7, dpi = 120)
}


######## Notes - Alpha is used for transparency - different color shades for different values of a variable
### flag if we need facets or not
plotLineSlope <- function(df, flag, chartTitle) {
  group.colors <- c("VisualFirst" = "#998EC3", "TableFirst"= "#F1A340")
  df <- df[df$diff !=100,]
  p <- ggplot(data = df,
              aes(x = Condition, y = Acc, group = workerId, color=Vis)) +
    # geom_jitter(width=0, height=1)+
    geom_line(aes(color = Vis, alpha=1, 
                  linetype=diff>0),  size = 1) 
  
  ifelse(flag=='facet', p <- p+ facet_grid(.~Vis) ,p)
  
  # geom_point(aes(color = Vis, y = jitter(Acc, 5)), size = 4) #can add shape=diff to change shape of points+
  
  p<- p+ geom_text_repel(data = df %>% filter(Condition == "Table"), 
                         aes(label = paste0(workerId, " - ", Acc)) , 
                         hjust = 1, 
                         fontface = "bold", 
                         size = 4,
                         direction="y",
                         nudge_x = -0.5,
                         # vjust=3,
                         # box.padding = 0.7,
                         
                         segment.size = 0.2) +
    geom_text_repel(data = df %>% filter(Condition == "Visual"), 
                    aes(label = paste0(workerId, " - ", Acc)) , 
                    hjust = 0, 
                    fontface = "bold", 
                    size = 4,
                    direction="y",
                    nudge_x = 0.5, 
                    segment.size = 0.2) +
    # move the x axis labels up top
    scale_x_discrete(position = "top") +
    theme_bw() +
    theme(panel.border     = element_blank()) +
    theme(axis.title.y     = element_blank()) +
    theme(axis.text.y      = element_blank()) +
    theme(panel.grid.major.y = element_blank()) +
    theme(panel.grid.minor.y = element_blank()) +
    theme(axis.title.x     = element_blank()) +
    theme(panel.grid.major.x = element_blank()) +
    theme(axis.text.x.top      = element_text(size=12)) +
    theme(axis.ticks       = element_blank()) +
    theme(panel.spacing = unit(2, "lines")) +
    theme(plot.title       = element_text(size=14, face = "bold", hjust = 0.5)) +
    theme(plot.subtitle    = element_text(hjust = 0.5)) +
    labs(title = chartTitle) +
    scale_color_manual(values= group.colors, name = "Table 1st or 2nd") +
    scale_alpha_continuous(guide = "none")
  p
  ggsave(paste( chartTitle,flag, ".pdf" ,sep=""), width =4, height = 7, dpi = 120)
}

############# Barcharts as slope graphs


plotBarSlopes <- function(df, plotTitle){
  df <- df[df$diff !=100,]
  df$Cond <- ifelse(df$diff > 0, "Visual Wins","Table Wins")
  # print(df$Cond)
  group.colors <- c("Visual Wins" = "#998EC3", "Table Wins"= "#F1A340")
  p <-  ggplot(df,aes(x=workerId, y=diff,fill=Cond)) + geom_bar(stat = "identity", width=0.4, position=position_dodge()) + coord_flip()+
    ggtitle(plotTitle) + theme_bw() + theme(aspect.ratio = 2/1.3) +
    theme( axis.title.y=element_blank()) + 
    theme( axis.title.x=element_blank()) + 
    theme( plot.title=element_blank()) + 
    scale_fill_manual(values=group.colors)+
    theme(axis.text=element_text(size=9, color = "black"))+
    theme(legend.position = "none")
  
  ggsave(paste( plotTitle, ".pdf" ,sep=""), width =3, height = 5, dpi = 120)
}

plotBarSlopesOld <- function(df, plotTitle){
  group.colors <- c("VisualFirst" = "#998EC3", "TableFirst"= "#F1A340")
  df <- df[df$diff !=100,]
  p <-  ggplot(df,aes(x=workerId, y=diff,fill=Vis)) + geom_bar(stat = "identity", width=0.4, position=position_dodge()) + coord_flip()+
    ggtitle(plotTitle) + theme_bw() + theme(aspect.ratio = 2/1.3) +#+  facet_grid(.~Condition) + #+ theme_grey(base_size=15) +
    theme( axis.title.y=element_blank()) + scale_fill_manual(values=group.colors, name = c("Layout Order"))+ 
    theme(plot.title = element_text(size=12, hjust=-0.2), axis.text=element_text(size=12))+
    theme(legend.position = "none")
  
  ggsave(paste( plotTitle, ".pdf" ,sep=""), width =3, height = 5, dpi = 120)
}
