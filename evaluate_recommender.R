rm(list=ls())
library(caret); 
library(class);library(leaps); library(Hmisc); library(plyr); library(devtools); library(ggplot2); # Data visualization
library(readr); # CSV file I/O, e.g. the read_csv function; 
library(data.table); # Using data.table to speed up runtime
library(dplyr); library(knitr); library(arules); 
library(reshape2); library(bit64); library(showtext);library(zoo); library(dummy);library(tidyr); 
require(data.table);
library(doParallel); registerDoParallel(cores = 4)

# load the data

rec.out<- fread("~/Northwestern University/PREDICT 498/Recommender/Evaluation/recommender_output2.25.17.csv", head=T, colClasses = c("character", "character", "integer"))
head(rec.out)
rec.out<- as.data.table(rec.out)
setkey(rec.out, User_ID)

cat.labels<- fread("~/Northwestern University/PREDICT 498/Recommender/Evaluation/recommender_category_key.csv", head=T, colClasses = c("character", "character", "character", "character"))
head(cat.labels)
cat.labels<- as.data.table(cat.labels)
cat.labels<- cat.labels[,c(1:3)]
setkey(cat.labels, Category_ID)
recs.labeled<- left_join(rec.out, cat.labels, by="Category_ID")
head(recs.labeled)
recs.labeled <- data.table(recs.labeled)
setkey(recs.labeled, User_ID)

rec.test <- read.csv("~/Northwestern University/PREDICT 498/Recommender/Evaluation/recommender_testdata2.25.17.csv", head=T, colClasses = c("character", "character", "numeric"))
head(rec.test)
rec.test<- as.data.table(rec.test)
setkey(rec.test, Category_ID)

all.rec<- left_join(rec.test, rec.out, c("User_ID" = "User_ID", "Category_ID" = "Category_ID"))
summary(all.rec)
str(all.rec)
all.rec$User_ID<- as.factor(all.rec$User_ID)
all.rec<-all.rec[complete.cases(all.rec),]
summary(all.rec)
all.rec$strong.rec.score<- ifelse((all.rec$Rec_Score >.2), 1, 0)
all.rec$positive.score<- ifelse(all.rec$actual_score >0, 1, 0)
summary(all.rec)
all.rec$success <- ifelse((all.rec$strong.rec.score == 1 & all.rec$positive.score ==1), 1, 0)
all.rec$nrecommends= ifelse(all.rec$strong.rec.score >0, 1, 0 )
summary(all.rec)
total.recommends <- sum(all.rec$nrecommends)
total.success = sum(all.rec$success)
total.success
total.nrecommends
all.rec$revenue.rate <- ifelse((all.rec$success/all.rec$nrecommends >0), 1,0)
all.rec<-all.rec[complete.cases(all.rec),]
all.rec$revenue.rate
summary(all.rec$revenue.rate)
all.rec$cost = .1
all.rec$cost.eq=all.rec$cost *all.rec$nrecommends
all.rec$paid = .25
profit <- ((all.rec$success * all.rec$paid) - (all.rec$nrecommends*all.rec$cost))
sum(profit)
# calculate ordered profit function using average donation = $14.50 and mailing cost = $2

profit.calc <- cumsum((all.rec$revenue.rate*all.rec$success)-(all.rec$nrecommends*all.rec$cost))
plot(profit.calc, main="Profit With Additional Recommendations", col="blue", xlab = 'Recommendations', ylab = 'Profit') # see how profits change as more mailings are made

n.mail.valid <- which.max(profit.calc) # number of recommendations that maximizes profits
c(n.mail.valid, max(profit.calc)) # report number of recommendations and maximum profit
