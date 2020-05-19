attach(train)
amountsurvived = length(which(train$Survived == 1))
amountmensurvived = length(which(train$Survived == 1 & train$Sex =="male"))
amountmensurvived
aamountwomensurvived = length(which(train$Survived == 1 & train$Sex =="female"))
aamountwomensurvived

#Q2 i) a pie chart will be the best to display the data seeing it is A 
# proportionality porblem a pie chart is accually perfect for it 
forpiechart <- c(amountmensurvived ,aamountwomensurvived)
pielabels <- c("Male" , "Female")
pct = c(round((amountmensurvived / amountsurvived)*100), round((aamountwomensurvived / amountsurvived)*100))
lbls <- paste(pielabels, pct)
lbls <- paste(lbls,"%",sep="")
pie(forpiechart,labels = lbls, col=rainbow(length(lbls)),main="Men and women who survived")
legend("topright", c("Male", "Female"), cex=0.8,fill=rainbow(length(forpiechart)))
#Question 3
Proportionwomensurving <- xtabs(~Survived+Sex,data = train)
props<- prop.table(Proportionwomensurving,1)*100
props<-round(props)
#this answer is in % 
props
#Question b
Proportionwomensurvingperclass <- xtabs(~Survived+Pclass+Sex,data = train)
surving<- prop.table(Proportionwomensurvingperclass,1)*100
surving<-round(surving)
#this answer is in % 
surving
#Question C
attach(test)
# Classification Tree with rpart
library(rpart)
datainframe <- as.matrix(train)
# grow tree 
na.omit(datainframe)

fit <- rpart(datainframe[,2] ~ +datainframe[,3] + datainframe[,5],
             method="class", data=test)

printcp(fit) # display the results 
plotcp(fit) # visualize cross-validation results 
summary(fit) # detailed summary of splits

# plot tree 
plot(fit, uniform=TRUE, 
     main="Classification Tree for who will is more likeley to survive")
text(fit, use.n=TRUE, all=TRUE, cex=.8)

# create attractive postscript plot of tree 
post(fit, file = "C:/Users/User/Desktop/anlaysis/newplot.csv", 
     title = "Classification Tree for who will is more likeley to survive")

#Question D
# Classification Tree with rpart
library(rpart)
attach(train)
datainframe <- as.matrix(train)
# grow tree 
na.omit(datainframe)
fit <- rpart(datainframe[,2] ~ +datainframe[,3] + datainframe[,5]+ datainframe[,6],
             method="class", data=test)

printcp(fit) # display the results 
plotcp(fit) # visualize cross-validation results 
summary(fit) # detailed summary of splits

# plot tree 
plot(fit, uniform=TRUE, 
     main="Classification Tree for who will is more likeley to survive")
text(fit, use.n=TRUE, all=TRUE, cex=.8)

# create attractive postscript plot of tree 
post(fit, file = "C:/Users/User/Desktop/anlaysis/text.txt", 
     title = "Classification Tree for who will is more likeley to survive")
#these are the row ids of the females that has a chance to survive 


