nursery <- read.csv("nursery.data", header=FALSE)
View(nursery)
colnames(nursery)[1]='Parents'
colnames(nursery)[2]='has_nurs'
colnames(nursery)[2]='Has_nurs'
colnames(nursery)[3]='Form'
colnames(nursery)[4]='Children'
colnames(nursery)[5]='Housing'
colnames(nursery)[6]='Finance'
colnames(nursery)[7]='Social'
colnames(nursery)[8]='Health'
colnames(nursery)[9]='Class'
summary(nursery)
library(arules)
rules<-apriori(nursery,parameter = list(minlen=2,supp=0.01,conf=0.8),appearance = list(rhs=c("Class=not_recom","Class=recommend","Class=very_recom","Class=priority","Class=spec_prior","Has_nurs=proper","Health=recommended","Parents=pretentious" ),default="lhs"))
quality(rules)<-round(quality(rules),digits = 3)
rules.sorted<-sort(rules,by="lift")
inspect(rules.sorted)
subset.matrix<-is.subset(rules.sorted,rules.sorted)
subset.matrix[lower.tri(subset.matrix,diag=T)]<-NA
redundant<-colSums(subset.matrix,na.rm = T)>=1
which(redundant)
rules.pruned<-rules.sorted[!redundant]
rules.sorted<-sort(rules,by="confidence")
inspect(rules.sorted)
library(arulesViz)
plot(rules.pruned)
plot(rules.pruned,method="grouped")
plot(rules.pruned,method="paracoord",control = list(recorder=TRUE))