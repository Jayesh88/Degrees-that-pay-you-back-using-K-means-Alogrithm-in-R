#loading essential libraries
library(tidyr)
library(dplyr)
library(readr)
library(ggplot2)
library(cluster)
library(factoextra)
##### PART 1
# read csv
ed_csv<-read.csv("C:/Users/HP PC/Documents/degrees-that-pay-back.csv")
View(ed_csv)
# new column names
new_col<-c("College.Major","Starting.Median.Salary","Mid.Career.Median.Salary","Career.Percent.Growth", "Percentile.10", "Percentile.25", "Percentile.75","Percentile.90")
# change column names
colnames(ed_csv)<-new_col
# view first few from the top
degrees<-head(ed_csv)
summary(degrees)


##### PART 2
# data Cleaning
degrees_clean<-mutate_at(ed_csv,vars(-College.Major),function(x) as.numeric(gsub("[\\$,]","",x)))

dvide_career_gwth<-mutate_at(degrees_clean,vars(Career.Percent.Growth),funs(Career.Percent.Growth/100))

#### Part 3
# Elbow Method
# Take colums from main set
k_means_data<-data.frame(degrees_clean$Starting.Median.Salary,degrees_clean$Mid.Career.Median.Salary,degrees_clean$Percentile.10,degrees_clean$Percentile.90)
# Scale the above columns
scle_data<-scale(k_means_data)
#using factoextra for visualizing the optimal number of clusters
elbow_method<-fviz_nbclust(k_means_data, FUNcluster = kmeans,method = "wss")


### Part 4
# silhouette method
# How wll each points fit in cluster
elbow_method<-fviz_nbclust(k_means_data, FUNcluster = kmeans,method = "silhouette")

### Part 5
# gap staticstics
gap_stat<-clusGap(k_means_data,FUNcluster = kmeans,nstart=25,K.max = 10,B=50)

gap_stat_method<-fviz_gap_stat(gap_stat)


### Part 6
# K- means Algorithm
set.seed(111)
num_clusters<-3
k_means<-kmeans(k_means_data,centers = num_clusters,iter.max = 15,nstart = 25)
#degrees_labled<-data.frame(degrees_clean,k_means$cluster)    #degrees_clean$clusters<-k_means$cluster
degrees_labled<-degrees_clean %>%
  mutate(clusters=k_means$cluster)


### Part 7
# Visualizing the clusters using ggplots
career_grwth<-ggplot(degrees_labled, aes(x=Starting.Median.Salary,y=Mid.Career.Median.Salary, color=clusters))+
  geom_point()+expand_limits(y=0)+
  ggtitle("Salary Representation")+xlab("Starting salary")+ylab("Mid-Career Salary")+
  scale_x_continuous(labels = scales::dollar) +scale_y_continuous(labels=scales::dollar)

career_grwth

### Part 8
# Deeperdive into the clusters
selct_col<-mutate_at(degrees_labled,vars(-Career.Percent.Growth,-Starting.Median.Salary))

degrees_perc<- degrees_labled %>%
  select(College.Major,Mid.Career.Median.Salary,Percentile.10,Percentile.25,Percentile.75,Percentile.90,clusters) %>%
  gather(key = "percentile",value = "salary",-c(College.Major,clusters)) %>%
  mutate(percentile=factor(percentile, levels=c('Mid.Career.Median.Salary','Percentile.10','Percentile.25','Percentile.75','Percentile.90')))


### Part 9
# Libral arts cluster for cluster 1
cluster_1<-degrees_perc %>%
  filter(clusters==1) %>%
  ggplot(aes(x=percentile , y=salary, group=College.Major, color=College.Major)) +
  geom_point() +
  geom_line() +
  theme(axis.text.x = element_text(size=7, angle=25))


### Part 10
# The goldilocks cluster for cluster 2
cluster_2<-degrees_perc %>%
  filter(clusters==2) %>%
  ggplot(aes(x=percentile , y=salary, group=College.Major, color=College.Major)) +
  geom_point() +
  geom_line() +
  theme(axis.text.x = element_text(size=7, angle=25))

### Part 11
#  cluster 3
cluster_3<-degrees_perc %>%
  filter(clusters==3) %>%
  ggplot(aes(x=percentile , y=salary, group=College.Major, color=College.Major)) +
  geom_point() +
  geom_line() +
  theme(axis.text.x = element_text(size=7, angle=25))


### Part 12
sort_data<-arrange(degrees_labled,desc(Career.Percent.Growth))

# Top two majors wth higest carrer growth
head(sort_data,n=2)




