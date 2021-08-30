#PCA

setwd('/Users/brian/Documents/2020Fall/Machine Learning/ML_Project_NBA_Plays/Rawdata/archive')
before_game_stats <- read.csv2(file = 'before_game_stats.csv', header = TRUE)
# Cleaning up data for ML ---------------------
data_clean=na.omit(before_game_stats)
drop_col=c("date","HOME_TEAM_ID","Visitor_VISITOR_TEAM_ID")  
data_clean=data_clean[,!(colnames(data_clean)%in%drop_col)]
labs= as.matrix(data_clean[,'label'])
names(labs)=c('label') 
#drop label column
drop_lab=c("label")
data_clean_nolabel=data_clean[,!(colnames(data_clean)%in%drop_lab)]


####PCA------
df_pca <- na.omit(data.frame(apply(data_clean, 2, function(x) as.numeric(as.character(x)))))
pca_label<- df_pca$label
drops <- c("plus_minus", "label")
df_pca=df_pca[ , !(names(df_pca) %in% drops)]

#PCA perfers centered data
scaled_df<-apply(df_pca,2,scale)
#calcualte eigenvalues & eigenvectors
cov_matrix <- cov(scaled_df)
eigen_df <- eigen(cov_matrix)
str(eigen_df)
#extract the loadings
(phi <- eigen_df$vectors[,1:2])

phi <- -phi
row.names(phi)=colnames(scaled_df)
colnames(phi) <- c("PC1", "PC2")

# Calculate Principal Components scores
PC1 <- as.matrix(scaled_df) %*% phi[,1]
PC2 <- as.matrix(scaled_df) %*% phi[,2]

# Create data frame with Principal Components scores
PC <- data.frame(label = pca_label, PC1, PC2)


ggplot(PC, aes(PC1, PC2)) + 
  modelr::geom_ref_line(h = 0) +
  modelr::geom_ref_line(v = 0) +
  geom_point(aes(colour=factor(label)))+
  xlab("First Principal Component") + 
  ylab("Second Principal Component") + 
  ggtitle("First Two Principal Components of NBA season stats")


