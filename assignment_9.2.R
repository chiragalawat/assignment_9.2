setwd("C:\\Users\\CHIRAG\\Downloads\\acadgild\\assignments")

library(readr)
Weightlift <- read.csv("C:/Users/Seshan/Desktop/sv R related/acadgild/assignments/session 18
                       Assign/Weight lift.csv",sep=',',header=F)
#problem: find out natual grouping
df<-Weightlift
#df=Weightlift <- read.csv("C:/Users/Seshan/Desktop/sv R related/acadgild/assignments/session 18
Assign/Weightlift.csv",sep=',',header=F)
View(df)
#df = read.csv("https://archive.ics.uci.edu/ml/machine-learningdatabases/00273/Example_WearableComputing_weight_lifting_exercises_biceps_curl_variations.csv",s
ep=',',header=F)
#problem: find out natual grouping
#View(df)
head(df)
str(df)
set.seed(1234)
ind = sample(1:nrow(df),0.8*nrow(df),replace 
head(df_train1)
class(df_train1)
# screeplot approach to decide the number of clusters
km = kmeans(df_train1,1)
km$withinss
km$tot.withinss
km = kmeans(df_train1,2)
km$withinss
km$tot.withinss
km = kmeans(df_train1,3)
km$withinss
km$tot.withinss
km = kmeans(df_train1,4)
km$withinss
km$tot.withinss
km = kmeans(df_train1,5)
km$withinss
km$tot.withinss
km = kmeans(df_train1,6)
km$withinss
km$tot.withinss
km = kmeans(df_train1,7)
km$withinss
km$tot.withinss
km = kmeans(df_train1,8)
km$withinss
km$tot.withinss
km = kmeans(df_train1,9)
km$withinss
km$tot.withinss
km = kmeans(df_train1,10)
km$withinss
km$tot.withinss
dev.off()
sumsq=NULL
for (i in 1:10)
sumsq[i] = sum(kmeans(df_train,centers=i,
iter.max = 1000,
nstart=i,
algorithm='Forgy')$withinss)
plot(1:10,sumsq,type='b', main='Screeplot showing within group sum of squares')
km = kmeans(df_train1,3)
km$withinss
km$tot.withinss
class(km$cluster)
summary(km)
km$centers
as.numeric(km$cluster)
length(km$cluster)
dim(df_train)
class(df_train)
df_train$cl <- km$cluster
head(df_train)
# profiles of clusters
aggregate(df_train[,1:8],list(df_train[,9]),mean)
table(df$V1)
library(cluster)
clusplot(df_train,df_train$cl,cex=0.9,color=T,shade=T, labels=4,lines=0)
#HC clustering or Hierarchical Clustering
# distance (euclidean, manhattan, cosine distance)
# Divisive method (top down)
# Agglomorative method (bottom up)
df_train = df_train[,-9]
head(df_train)
# compute the distance metrix
d1 <- dist(df_train,method='euclidean')
summary(d1)
# HC
fit <- hclust(d1,method = 'ward.D2')
plot(fit)
# single, double, average, ward, ward.D2
# agglomorative method
fit <- agnes(d1,metric='euclidean',method = 'ward')
plot(fit)
# divisive method
fit <- diana(d1,metric='euclidean')
plot(fit)
library(ggdendro)
if(require(cluster)){
fit<- agnes(d1, metric = "manhattan", stand = TRUE)
dg <- as.dendrogram(fit)
ggdendrogram(dg)

fit <- diana(d1, metric = "manhattan", stand = TRUE)
dg <- as.dendrogram(fit)
ggdendrogram(dg)
}


> class(df_train1)
[1] "matrix"
> # screeplot approach to decide the number of clusters
> km = kmeans(df_train1,1)
> km$withinss
[1] 1833
> km$tot.withinss
[1] 1833
>
> km = kmeans(df_train1,2)
> km$withinss
[1] 685.8406 600.9150
> km$tot.withinss
[1] 1286.756
>
> km = kmeans(df_train1,3)
> km$withinss
[1] 346.0231 298.1925 336.7207
> km$tot.withinss
[1] 980.9363
>
> km = kmeans(df_train1,4)
> km$withinss
[1] 275.1233 210.9994 209.2629 216.7761
> km$tot.withinss
[1] 912.1616
>
> km = kmeans(df_train1,5)
> km$withinss
[1] 176.7833 119.2848 138.2213 309.2322 112.0491
> km$tot.withinss
[1] 855.5707
>
> km = kmeans(df_train1,6)
> km$withinss
[1] 172.84625 114.91540 137.94960 143.94848 94.49965 149.72160
> km$tot.withinss
[1] 813.881
>
> km = kmeans(df_train1,7)
> km$withinss
[1] 74.27123 78.06204 79.70444 58.01375 81.96519 298.19248 96.74012
> km$tot.withinss
[1] 766.9492
>
> km = kmeans(df_train1,8)
> km$withinss
[1] 33.42481 196.45068 124.67682 58.35021 126.40989 34.71128 77.80059
[8] 70.22846
> km$tot.withinss
[1] 722.0527
>
> km = kmeans(df_train1,9)
> km$withinss
[1] 44.271295 78.062037 110.262048 107.569538 4.597058 47.484979 138.221
277
[8] 37.955215 104.487311
> km$tot.withinss
[1] 672.9108
>
> km = kmeans(df_train1,10)
> km$withinss
[1] 50.42743 57.36509 74.27123 52.76306 39.50401 81.96519 233.91216
[8] 23.12942 30.22351 33.42481
> km$tot.withinss
[1] 676.9859
> dev.off()
null device
1
> sumsq=NULL
> for (i in 1:10)
+ sumsq[i] = sum(kmeans(df_train,centers=i,
+ iter.max = 1000,
+ nstart=i,
+ algorithm='Forgy')$withinss)
Error in do_one(nmeth) : NA/NaN/Inf in foreign function call (arg 1)
In addition: Warning message:
In storage.mode(x) <- "double" : NAs introduced by coercion
> plot(1:10,sumsq,type='b', main='Screeplot showing within group sum of squar
es')
> km = kmeans(df_train1,3)
> km$withinss
[1] 346.0231 336.7207 298.1925
> km$tot.withinss
[1] 980.9363
>
> class(km$cluster)
[1] "integer"
>
> summary(km)


> df_train$cl <- km$cluster
Error in `$<-.data.frame`(`*tmp*`, cl, value = c(`21` = 2L, `111` = 1L, :
replacement has 142 rows, data has 3220
>
> head(df_train)

library(ggdendro)
if(require(cluster)){
fit<- agnes(d1, metric = "manhattan", stand = TRUE)
dg <- as.dendrogram(fit)
ggdendrogram(dg)

fit <- diana(d1, metric = "manhattan", stand = TRUE)
dg <- as.dendrogram(fit)
ggdendrogram(dg)
}