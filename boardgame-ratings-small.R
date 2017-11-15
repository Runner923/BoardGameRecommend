library(arules)
library(arulesViz)
#library(Matrix)  if needed
datdir<-"C:/Users/Charles/Documents/PDXDataSciRecommender/"
setwd(datdir)
dat<-read.csv(paste(datdir,"boardgame-ratings.csv",sep=""))
dat<-dat[order(dat$UserID,dat$gameID),]
usergrping<-grouping(dat$UserID)
userid.ends<-attr(usergrping,"ends")
userid.starts<-c(1,userid.ends[1:(length(userid.ends)-1)]+1)
userid.counts<-diff(userid.starts)
sglusr_rows<-userid.starts[which(userid.counts==1)]
datsgl<-dat[sglusr_rows,]
# check the data
chk1<-length(datsgl$UserID)
chk2<-length(unique(datsgl$UserID))
isTRUE(chk1==chk2)
# convert to factors
dat[,"UserID"]<-factor(dat[,"UserID"])
dat[,"gameID"]<-factor(dat[,"gameID"])
#discretize ratings
dat[,"rating"]<-discretize(dat$rating,method="interval",categories=5)
translist<-lapply(1:length(userid.ends),function(n){
  rws<-userid.starts[n]:userid.ends[n]
  x<-dat$gameID[rws]
})
datrans<-as(translist,"transactions")
# find a support level
sup<-1000/nrow(datrans)
itemsets <- apriori(datrans, parameter = list(target = "frequent",
                                            supp=sup, minlen = 3))
inspect(head(sort(itemsets), n=10))
quality(itemsets)$lift <- interestMeasure(itemsets, measure="lift", trans = datrans)
inspect(head(sort(itemsets, by = "lift"), n=10))
#
plot(head(sort(itemsets, by = "lift"), n=50), method = "graph", control=list(cex=.8))
