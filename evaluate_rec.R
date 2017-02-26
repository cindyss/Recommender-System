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

rec.train <- fread("~/Northwestern University/PREDICT 498/Recommender/Evaluation/raw_data_train2.25.csv", head=T, colClasses = c("character", "character", "numeric"))#
head(rec.train)
rec.train<- as.data.table(rec.train)
setkey(rec.train, Category_ID)
cat.labels<- fread("~/Northwestern University/PREDICT 498/Recommender/Evaluation/recommender_category_key.csv", head=T, colClasses = c("character", "character", "character"))
head(cat.labels)
cat.labels<- as.data.table(cat.labels)
setkey(cat.labels, Category_ID)
train.labeled<- left_join(rec.train, cat.labels, by="Category_ID")
head(train.labeled)
train.labeled <- data.table(train.labeled)
setkey(train.labeled, User_ID)

rec.out<- fread("~/Northwestern University/PREDICT 498/Recommender/Evaluation/recommender_output2.25.17.csv", head=T, colClasses = c("character", "character", "integer"))
head(rec.out)
rec.out<- as.data.table(rec.out)
setkey(rec.out, User_ID)
train.rec<- left_join(train.labeled, rec.out, c("User_ID" = "User_ID", "Category_ID" = "Category_ID"))
head(train.rec)
train.rec <- data.table(train.rec)
setkey(train.rec, Category_ID)
train.rec <- unique(train.rec)
rec.test <- read.csv("~/Northwestern University/PREDICT 498/Recommender/Evaluation/recommender_testdata2.25.17.csv", head=T, colClasses = c("character", "character", "numeric"))
head(rec.test)
rec.test<- as.data.table(rec.test)
setkey(rec.test, Category_ID)
rec.test<- left_join(rec.test, cat.labels, by="Category_ID")  
head(rec.test)
head(rec.out)
all.rec<- left_join(rec.test, rec.out, c("User_ID" = "User_ID", "Category_ID" = "Category_ID"))
summary(all.rec)
all.rec<-all.rec[complete.cases(all.rec),]
good.rec.score<- ifelse((all.rec$Rec_Score >2), 1, 0)
rec.accuracy<- ifelse((all.rec$actual_score >0), 1, 0)
summary(all.rec)
success <- ifelse((good.rec.score == 1 & rec.accuracy == 1), 1, 0)
nrecommends= ifelse(good.rec.score ==1, 1,0 )
nrecommends <- sum(nrecommends)
accuracy = sum(success)
accuracy
nrecommends
accuracy.rate <- accuracy/nrecommends
accuracy.rate
cost = .1
paid = 1.00
profit <- ((accuracy * paid) - (nrecommends*cost))
profit
           
