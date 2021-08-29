library(tidyr)
library(dplyr)
library(tidyverse)
library(cluster)
library(aod)
library(ggplot2)
library(caret)
library(pROC)
# Import Data ----------------------------
# Data taken from https://www.kaggle.com/nathanlauga/nba-games
setwd('/Users/brian/Documents/2020Fall/Machine Learning/ML_Project_NBA_Plays/Rawdata/archive')
dat <- read.csv(file = 'games.csv', header = TRUE)
dat=na.omit(dat) #removes nas


SeasonStatsBeforeEachGame<-function(dat,current_date,hometeam,season,visitor){
  current_date=current_date
  hometeam=hometeam
  visitor=visitor
  season=season
  season_1=as.numeric(season)-1
  #looking at data within current season
  dat=subset(dat,SEASON %in% c(season, season_1))
  #looking at all games before current game
  before_current_date=dat[(dat$GAME_DATE_EST<current_date),]
  
  #home team at home stats
  home_team_home_before_date=before_current_date[(before_current_date$HOME_TEAM_ID==hometeam),] #all games by the home team AT home before a certain day
  keeps_home=c('HOME_TEAM_ID','PTS_home','FG_PCT_home','FT_PCT_home','FG3_PCT_home','AST_home','REB_home',
               'PTS_away','FG_PCT_away','FT_PCT_away','FG3_PCT_away','AST_away','REB_away','HOME_TEAM_WINS') 
  home_team_home_averages=colMeans(home_team_home_before_date[,keeps_home, drop = FALSE], na.rm=TRUE)
  
  
  #visitor team as visitor season stats
  visitor_team_away_before_date=before_current_date[(before_current_date$VISITOR_TEAM_ID==visitor),]
  keeps_visitor=c('VISITOR_TEAM_ID','PTS_away','FG_PCT_away','FT_PCT_away','FG3_PCT_away','AST_away','REB_away',
                  'PTS_home','FG_PCT_home','FT_PCT_home','FG3_PCT_home','AST_home','REB_home','HOME_TEAM_WINS') 
  visitor_team_away_averages=colMeans(visitor_team_away_before_date[,keeps_visitor, drop = FALSE], na.rm=TRUE)
  #rename visitor stats
  names(visitor_team_away_averages)<-paste0('Visitor_', names(visitor_team_away_averages))
  
  #combine home and visitor stats together
  current_season_averages=c(home_team_home_averages,visitor_team_away_averages)
  #rename HOME_TEAM_WINS to at HOME_WIN_PCT and ROAD_WIN_PCT
  names(current_season_averages)[names(current_season_averages)=='HOME_TEAM_WINS'] <-"HOME_WIN_PCT"
  names(current_season_averages)[names(current_season_averages)=='Visitor_HOME_TEAM_WINS'] <-"ROAD_WIN_PCT"
  current_season_averages["ROAD_WIN_PCT"]=abs(current_season_averages["ROAD_WIN_PCT"]-1) #road win percentage, is 1- visitor_home_win_percentage
  
  #shows average home team stats at home (how much you score)
  #shows average away team stats against home team at home (how much do away teams score on you )
  #shows average visiting team stats while away (how much does visiting team scores while visiting)
  #shows average stats of home team against our visiting team (how much visiting teams get scored on while visiting)
  #HOME_TEAM_WINS how much does the home team win while at home
  #Visitor_HOME_TEAM_WINS how often does the visitor team loss visitor games
  
  current_season_averages=c(date=current_date,current_season_averages)
  return(current_season_averages)}



# Uses for loop and SeasonStatsBeforeEachGame to generate dataset stats before each game-----------
before_game_stats=c(SeasonStatsBeforeEachGame(dat,'2016-04-01','1610612744','2015','1610612744'),label=0) #initialize array
for (i in 1:nrow(dat)){
  game=dat[i,]
  x=SeasonStatsBeforeEachGame(dat,game$GAME_DATE_EST,game$HOME_TEAM_ID,game$SEASON,game$VISITOR_TEAM_ID)
  x=c(x, label=as.numeric(game["HOME_TEAM_WINS"]))
  x=c(x,plus_minus=(dat[i,]$PTS_home-dat[i,]$PTS_away))
  before_game_stats=rbind(before_game_stats,x)
  rownames(before_game_stats)[i+1]=game$GAME_ID
}
before_game_stats=before_game_stats[-1,]
write(before_game_stats,'rerunning_previous_seasons.csv')


# Cleaning up data for ML ---------------------
data_clean=na.omit(before_game_stats)
drop_col=c("date","HOME_TEAM_ID","Visitor_VISITOR_TEAM_ID")  
data_clean=data_clean[,!(colnames(data_clean)%in%drop_col)]
labs= as.matrix(data_clean[,'label'])
names(labs)=c('label') 
#drop label column
drop_lab=c("label")
data_clean_nolabel=data_clean[,!(colnames(data_clean)%in%drop_lab)]

#splitting my dataset into training and testing set----
df2 <- data.frame(apply(data_clean_nolabel, 2, function(x) as.numeric(as.character(x))))


normalize<- function(x){
  return((x-min(x))/max(x)-min(x))
}
scaled_df=df2
#scaled_df=scale(df2)
scaled_df=as.data.frame(scaled_df)
scaled_df$X=labs


#labs=df2$label
#scaled_df=apply(df2,2, normalize)
#df2$label=labs
df2<-df2[complete.cases(df2),]

#knn----
df_knn = na.omit(data.frame(apply(scaled_df, 2, function(x) as.numeric(as.character(x)))))
names(df_knn)[names(df_knn) == 'X'] <- 'label'

set.seed(101) # Set Seed so that same sample can be reproduced in future also
# Now Selecting 75% of data as sample from total 'n' rows of the data  
sample <- sample.int(n = nrow(df_knn), size = floor(.75*nrow(df_knn)), replace = F)
train <- df_knn[sample, ]
test  <- df_knn[-sample, ]

ctrl <- trainControl(method="repeatedcv",repeats = 3) 

knnFit <- train( label~PTS_home+FG_PCT_home+FT_PCT_home+
                   FG3_PCT_home+AST_home+REB_home+
                   PTS_away+ FG_PCT_away+FT_PCT_away+         
                   FG3_PCT_away+AST_away+REB_away+            
                   HOME_WIN_PCT+Visitor_PTS_away+Visitor_FG_PCT_away+
                   Visitor_FT_PCT_away+Visitor_FG3_PCT_away+Visitor_AST_away+    
                   Visitor_REB_away+Visitor_PTS_home+Visitor_FG_PCT_home+
                   Visitor_FT_PCT_home+Visitor_FG3_PCT_home+Visitor_AST_home+  
                   Visitor_REB_home+ROAD_WIN_PCT,   
                 data = train, 
                 method = "knn", 
                 trControl = ctrl, 
                 tuneLength = 20)                
plot(knnFit)                
knnFit
knnPredict=predict(knnFit, newdata=test)
knnPredict
test_roc = roc(test$label ~ knnPredict, plot = TRUE, print.auc = TRUE)

predicted_labels<- ifelse(knnPredict>0.5, 1, 0)
confusionMatrix(as.factor(predicted_labels), as.factor(test$label), positive = "1")


######training logistic regression model-------
mylogit<-glm(label~PTS_home+FG_PCT_home+FT_PCT_home+
               FG3_PCT_home+AST_home+REB_home+
               PTS_away+ FG_PCT_away+FT_PCT_away+         
               FG3_PCT_away+AST_away+REB_away+            
               HOME_WIN_PCT+Visitor_PTS_away+Visitor_FG_PCT_away+
               Visitor_FT_PCT_away+Visitor_FG3_PCT_away+Visitor_AST_away+    
               Visitor_REB_away+Visitor_PTS_home+Visitor_FG_PCT_home+
               Visitor_FT_PCT_home+Visitor_FG3_PCT_home+Visitor_AST_home+  
               Visitor_REB_home+ROAD_WIN_PCT,       
             data=as.data.frame(train),family='binomial')
predicted_lr<-predict(mylogit,test, type = "response")
#turn our predicted probabilities into a classification 
predicted_labels_lr<- ifelse(predicted_lr>0.5, 1, 0)
confusionMatrix(as.factor(predicted_labels_lr), as.factor(test$label), positive = "1")
# plot ROC and AUC
test_roc = roc(test$label~ predicted_lr, plot = TRUE, print.auc = TRUE)

