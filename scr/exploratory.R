# Import Library ------------------------- 
library(tidyr)
library(dplyr)
library(tidyverse)
library(cluster)
library(aod)
library(ggplot2)
# Import Data ----------------------------
# Data taken from https://www.kaggle.com/nathanlauga/nba-games
setwd('/Users/brian/Documents/2020Fall/Machine Learning/ML_Project_NBA_Plays/Rawdata/archive')
dat <- read.csv(file = 'games.csv', header = TRUE)
dat=na.omit(dat) #removes nas

# Creating New Data Set ---------------------
#creates a function that returns the average stats of the hometeam as the hometeam, 
#and visitor team as visitor team,  during a giving season 

SeasonStatsBeforeEachGame<-function(dat,current_date,hometeam,season,visitor){
  current_date=current_date
  hometeam=hometeam
  visitor=visitor
  season=season
  #looking at data within current season
  dat=dat[dat$SEASON==season,]
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
  before_game_stats=rbind(before_game_stats,x)
  rownames(before_game_stats)[i+1]=game$GAME_ID
}
before_game_stats=before_game_stats[-1,]
save=before_game_stats

#cleaning up our new dataset


# Data Exploration -----------------------
df=as.data.frame(dat)
df$HOME_TEAM_WINS=as.factor(df$HOME_TEAM_WINS)
df$SEASON=as.factor(df$SEASON)
ggplot(df,aes(y=PTS_home, x=PTS_away, color=HOME_TEAM_WINS))+geom_point()
ggplot(df,aes(y=AST_home, x=AST_away, color=HOME_TEAM_WINS))+geom_point()
ggplot(df,aes(y=REB_home, x=REB_away, color=HOME_TEAM_WINS))+geom_point()
df_2003v2018=df[df$SEASON=="2003"|df$SEASON=="2018",]
ggplot(df_2003v2018,aes(x=PTS_home, color=SEASON))+geom_histogram(fill="white",alpha=0.25,position = "identity")
ggplot(df_2003v2018,aes(x=FG3_PCT_home*, color=SEASON))+geom_histogram(fill="white",alpha=0,position = "identity")

# two density plot taken from here https://stackoverflow.com/questions/3541713/how-to-plot-two-histograms-together-in-r
carrots=df$PTS_home
cukes=df$PTS_away
densCarrot <- density(carrots)
densCuke <- density(cukes)
## calculate the range of the graph
xlim <- range(densCuke$x,densCarrot$x)
ylim <- range(0,densCuke$y, densCarrot$y)
#pick the colours
carrotCol <- rgb(1,0,0,0.2)
cukeCol <- rgb(0,0,1,0.2)
## plot the carrots and set up most of the plot parameters
plot(densCarrot, xlim = xlim, ylim = ylim, xlab = 'Points Scored',
     main = 'Distribution of Points Scored by Home and Away Team', 
     panel.first = grid())
#put our density plots in
polygon(densCarrot, density = -1, col = carrotCol)
polygon(densCuke, density = -1, col = cukeCol)
## add a legend in the corner
legend('topleft',c('PTS_home','PTS_away'),
       fill = c(carrotCol, cukeCol), bty = 'n',
       border = NA)

ggplot(df,aes(x=factor(1),fill=HOME_TEAM_WINS))+geom_bar(width=1)+coord_polar("y")


# only looking at gsw, bos and knicks from 2014-2017
df_2015=df[,]
df_2015=df[df$SEASON==2014|df$SEASON==2015|df$SEASON==2016,]
gsw_celtics_nyk=c('1610612744','1610612738','1610612752')
df_2015=df_2015[df_2015$HOME_TEAM_ID==1610612744|df_2015$HOME_TEAM_ID==1610612738|df_2015$HOME_TEAM_ID==1610612752,]
#home games won
df_2015$HOME_TEAM_ID=as.factor(df_2015$HOME_TEAM_ID)
ggplot(df_2015,aes(y=PTS_home, x=PTS_away, color=HOME_TEAM_ID))+geom_point()+
  geom_abline(intercept = 0, slope = 1)+
  scale_color_manual(labels = c("Celtics", "Warriors","Knicks"), values = c("Light Green", "Blue","Orange"))

#away games
df_2015=df[df$SEASON==2014|df$SEASON==2015|df$SEASON==2016,]
df_2015_away=df_2015[df_2015$VISITOR_TEAM_ID==1610612744|df_2015$VISITOR_TEAM_ID==1610612738|df_2015$VISITOR_TEAM_ID==1610612752,]
df_2015_away$VISITOR_TEAM_ID=as.factor(df_2015_away$VISITOR_TEAM_ID)
ggplot(df_2015_away,aes(y=PTS_home, x=PTS_away, color=VISITOR_TEAM_ID))+geom_point()+
  geom_abline(intercept = 0, slope = 1)+scale_color_manual(labels = c("Celtics", "Warriors","Knicks"), values = c("Light Green", "Blue","Orange"))


#data exploration with seasonstats before each game
dat=dat[dat$SEASON==2015,] # run the loop on this set of dat
df_before=as.data.frame(before_game_stats)
df_before$HOME_WIN_PCT=as.numeric(df_before$HOME_WIN_PCT)
df_before$ROAD_WIN_PCT=as.numeric(df_before$ROAD_WIN_PCT)
ggplot(df_before,aes(y=HOME_WIN_PCT, x=ROAD_WIN_PCT,color=label))+
  geom_point()+
  labs(x="Road Team Road Win %",y="Home Team Home Win %")

 




# Cleaning up data for ML ---------------------
data_clean=na.omit(before_game_stats)
drop_col=c("date","HOME_TEAM_ID","Visitor_VISITOR_TEAM_ID")  
data_clean=data_clean[,!(colnames(data_clean)%in%drop_col)]
labs= as.matrix(data_clean[,'label'])
names(labs)=c('label') 
drop_lab=c("label")
data_clean_nolabel=data_clean[,!(colnames(data_clean)%in%drop_lab)]

# Hierachical cluster --------------------------
 #subsetting data by season
#average linkage
hierarchical_dist <- dist(data_clean, method = "euclidean")
tree_average <- hclust(hierarchical_dist, method="average")
tree_average_k2<- cutree(tree_average, k =2)

#single linkage
tree_single <- hclust(hierarchical_dist, method="single")
tree_single_k2<- cutree(tree_single, k =2)

#complete linkage
tree_complete <- hclust(hierarchical_dist, method="complete")
tree_complete_k2<- cutree(tree_complete, k =2) 

# CONTINGUENCY TABBLES ----------------------------
#average
average_labs_actu<-labs[names(tree_average_k2),] #true labes
average_labs_predict<-replace(tree_average_k2,tree_average_k2==2,0)
count_correct_average=0

for (x in 1:length(average_labs_actu)){
  if (average_labs_actu[x]==average_labs_predict[x]){
    count_correct_average=count_correct_average+1
  }
}

#single
single_labs_actu<-labs[names(tree_single_k2),] #true labes
single_labs_predict<-replace(tree_single_k2,tree_single_k2==2,0)
count_correct_single=0

for (x in 1:length(single_labs_actu)){
  if (single_labs_actu[x]==single_labs_predict[x]){
    count_correct_single=count_correct_single+1
  }
}
#complete
complete_labs_actu<-labs[names(tree_complete_k2),] #true labes
complete_labs_predict<-replace(tree_complete_k2,tree_complete_k2==2,0)
count_correct_complete=0

for (x in 1:length(complete_labs_actu)){
  if (complete_labs_actu[x]==complete_labs_predict[x]){
    count_correct_complete=count_correct_complete+1
  }
}

#combing into one table
contingency_table=rbind(count_correct_average, count_correct_single,count_correct_complete)
incorrect=apply(contingency_table,2, function(x) length(labs)-x)
contingency_table=cbind(contingency_table,incorrect)
#rename col/rows
colnames(contingency_table)=c('Correct','Incorrect')
rownames(contingency_table)=c('Average Linkage','Single Linkage','Complete Linkage')

print(contingency_table)


#kmeans ------------------------
kmeans_1<-kmeans(data_clean,center=2,nstart=10)
#gap statistic 
gapkm <- clusGap(data_clean, FUN = kmeans, nstart = 20, K.max = 10, B = 60)
plot(gapkm)

#logistic regression-------------
mylogit<-glm(label~PTS_home+PTS_away+Visitor_PTS_away+Visitor_PTS_home,data=as.data.frame(data_clean),family='binomial')
