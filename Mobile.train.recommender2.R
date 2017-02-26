############################### TRAINING DATA SET ###########################################
rm(list=ls())
library(lars); library(caret); library(e1071); library(DataExplorer);library(Rserve)
library(class);library(leaps); library(Hmisc); library(plyr); library(devtools); library(ggplot2); # Data visualization
library(readr); # CSV file I/O, e.g. the read_csv function; 
library(lubridate); library(date) # date functions
library(data.table); # Using data.table to speed up runtime
library(repr); # Use repr to adjust plot size
library(profvis); library(dplyr); library(ggmap); library(knitr); library(arules); library(scales) #for scaling dates
library(reshape2); library(bit64); library(showtext);library(zoo); library(dummy);library(tidyr); library(mice); library(stringr)
require(data.table)
library(doParallel); registerDoParallel(cores = 4)
memory.limit(size=60000)
collapsecategory <- function(x,p){
  levels_len = length(levels(x))
  levels(x)[levels_len+1] = 'Other'
  y= table(x)/length(x)
  y1 = as.vector(y)
  y2 = names(y)
  y2_len = length(y2)
  for(i in 1:y2_len){
    if(y1[i]<=p){
      x[x == y2[i]] = 'Other'
    }
  }
  x <- droplevels(x)
  x
}

# load the data
mobile.train.big <- fread("~/Northwestern University/PREDICT 498/DATA/CJ.mobile.train.csv", head=T)
mobile.train.big$gender <- ifelse(mobile.train.big$gender == 'M', 1, 0)
summary(mobile.train.big)
names(mobile.train.big)
mobile.train.big$device_id <- as.character(mobile.train.big$device_id)
mobile.train.big$app_id <- as.factor(mobile.train.big$app_id)
mobile.train.big$app_id <- collapsecategory(mobile.train.big$app_id, p = .01)
mobile.train.big$category <- as.factor(mobile.train.big$category)
mobile.train.big$category <- collapsecategory(mobile.train.big$category, p = .01)
mobile.train.big$big_category <- NULL
mobile.train.big$phone_brand <- as.factor(mobile.train.big$phone_brand)
mobile.train.big$device_model <- as.factor(mobile.train.big$device_model)
mobile.train.big$label_id <- as.factor(mobile.train.big$label_id)
dim(mobile.train.big)
mobile.train.big <- mobile.train.big[,-60]
mobile.train.big$fishing=ifelse(mobile.train.big$label_id== '11', 1, 0)
mobile.train.big$label_id = ifelse(mobile.train.big$label_id == '11', NULL, mobile.train.big$label_id )
mobile.train.big$call=ifelse(mobile.train.big$label_id== '175', 1, 0)
mobile.train.big$label_id = ifelse(mobile.train.big$label_id == '175', NULL, mobile.train.big$label_id )
mobile.train.big$label_id <- collapsecategory(mobile.train.big$label_id, p = .01)
summary(mobile.train.big)
setkey(mobile.train.big, NULL)
mobile.train.big <- unique(mobile.train.big)
dim(mobile.train.big)


cor(mobile.train.big$gender, mobile.train.big$call)
mysample <- mobile.train.big[sample(1:nrow(mobile.train.big), 5000,
                          replace=FALSE),] 
lm.fit <- glm(gender~phone_brand+timestamp_unique_count+flag_1+flag_3+big_category_unique_count+event_count+app_id_unique_count,data=mysample)   #ran that removed low signficance +
summary(lm.fit)


Rserve ()
rm(list=ls())
library(proxy)
library(recommenderlab)
library(lars); library(caret); library(e1071); library(lsa)
library(class);library(leaps); library(Hmisc); library(plyr); library(devtools); library(ggplot2); # Data visualization
library(readr); # CSV file I/O, e.g. the read_csv function;
library(lubridate); library(date) # date functions
library(data.table); # Using data.table to speed up runtime
library(repr); # Use repr to adjust plot size
library(profvis); library(dplyr); library(ggmap); library(knitr); library(arules); library(scales) #for scaling dates
library(reshape2); library(bit64); library(showtext);library(zoo); library(dummy);library(tidyr); library(mice); library(stringr)
require(data.table)
#library(doParallel); registerDoParallel(cores = 4)
#memory.limit(size=60000)
mobile_devs <- fread("~/gender_age_train/gender_age_train.csv", head=T) # load the aggregated data file
mobile_devs$User_ID <-sprintf("%05d", 1:nrow(mobile_devs))
head(mobile_devs)
mobile_devs <- mobile_devs[, c(5,1:4)]
setkey(mobile_devs, device_id)
mobile_devs$device_id <- as.character(mobile_devs$device_id)
#mobile_devs <- mobile_devs[sample(1:nrow(mobile_devs), 100,
 #                        replace=FALSE),] 
summary(mobile_devs)
mobile_events <- fread("~/events.csv/events.csv", head=T) # load the aggregated data file
head(mobile_events)
mobile_events <- mobile_events[,c(1:2)]
mobile_events$device_id <- as.character(mobile_events$device_id)
head(mobile_events)
setkey(mobile_events, device_id)
mobile2 <- merge(mobile_devs, mobile_events, by = 'device_id')
head(mobile2)
setkey(mobile2, event_id)
mobile_app_events <- fread("~/app_events.csv/app_events.csv", head=T) # load the aggregated data file
head(mobile_app_events)
app_id_ind<-unique(mobile_app_events$app_id)
app_id_ind<-as.character(app_id_ind)
n=length(app_id_ind)
app_number<-sprintf("%05d", 1:n)
app_ind<- cbind(app_id_ind, app_number)
colnames(app_ind)[1]<- "app_id"
colnames(app_ind)[2]<- "app_number"
summary(app_ind)
app_ind<- data.table(app_ind)
app_ind$app_id<- as.character(app_ind$app_id)
head(app_ind)
mobile_app_events <- mobile_app_events[,c(1:2)]
mobile_app_events$app_id<- as.character(mobile_app_events$app_id)
setkey(mobile_app_events, app_id)
setkey(app_ind, app_id)
head(mobile_app_events)
mobile_app_events<- merge(mobile_app_events, app_ind, by = "app_id")
head(mobile_app_events)
setkey(mobile_app_events, event_id)
mobile2 <- merge(mobile2, mobile_app_events, by='event_id')
summary(mobile2)
str(mobile2)

rm(mobile_events, mobile_devs, mobile_app_events, app_ind)
setkey(mobile2, NULL)
names(mobile2)
mobile2<-unique(mobile2)
dim(mobile2)
collapsecategory <- function(x,p){
  levels_len = length(levels(x))
  levels(x)[levels_len+1] = 'Other'
  y= table(x)/length(x)
  y1 = as.vector(y)
  y2 = names(y)
  y2_len = length(y2)
  for(i in 1:y2_len){
    if(y1[i]<=p){
      x[x == y2[i]] = 'Other'
    }
  }
  x <- droplevels(x)
  x
}
mobile2$app_id <- as.factor(mobile2$app_id)
mobile2$app_number<-as.factor(mobile2$app_number)
setkey(mobile2, NULL)
mobile2<- unique(mobile2)
#Make flags for most important apps
mobile2$app85699<- ifelse(mobile2$app_id == '-9059268695761085699', 1, 0)
mobile2$app12353<- ifelse(mobile2$app_id == '-8845947943331312353', 1, 0)
mobile2$app13972<- ifelse(mobile2$app_id == '-4601684156658213972', 1, 0)
mobile2$app14790<- ifelse(mobile2$app_id == '-5123934752064414790', 1, 0)
mobile2$app2167<- ifelse(mobile2$app_id == '839532574074472167', 1, 0)
mobile2$app2087<- ifelse(mobile2$app_id == '-653184325026622087', 1, 0)
mobile2$app2359<- ifelse(mobile2$app_id == '8730359466281022359', 1, 0)
mobile2$app25125<- ifelse(mobile2$app_id == '-1633857974709225125', 1, 0)
mobile2$app2574<- ifelse(mobile2$app_id == '3210769422517172574', 1, 0)
mobile2$app2612<- ifelse(mobile2$app_id == '8351542909845102612', 1, 0)
mobile2$app2665<- ifelse(mobile2$app_id == '1776232365776742665', 1, 0)
mobile2$app2671<- ifelse(mobile2$app_id == '6027707876068712671', 1, 0)
mobile2$app2858<- ifelse(mobile2$app_id == '-7377004479023402858', 1, 0)
mobile2$app39155<- ifelse(mobile2$app_id == '-4303111315590939155', 1, 0)
mobile2$app4238<- ifelse(mobile2$app_id == '3683147815759994238', 1, 0)
mobile2$app4338<- ifelse(mobile2$app_id == '2323199900431934338', 1, 0)
mobile2$app45571<- ifelse(mobile2$app_id == '-7509233974245045571', 1, 0)
mobile2$app46293<- ifelse(mobile2$app_id == '-1486216271740346293', 1, 0)
mobile2$app4701<- ifelse(mobile2$app_id == '7348340405200054701', 1, 0)
mobile2$app53300<- ifelse(mobile2$app_id == '-2382345161972953300', 1, 0)
mobile2$app5457<- ifelse(mobile2$app_id == '-7585329432531805457', 1, 0)
mobile2$app5642<- ifelse(mobile2$app_id == '7851187388862225642', 1, 0)
mobile2$app5899<- ifelse(mobile2$app_id == '8597107160462085899', 1, 0)
mobile2$app5961<- ifelse(mobile2$app_id == '4423440205377475961', 1, 0)
mobile2$app6122<- ifelse(mobile2$app_id == '3717049149426646122', 1, 0)
mobile2$app6364<- ifelse(mobile2$app_id == '6666573794781566364', 1, 0)
mobile2$app6381<- ifelse(mobile2$app_id == '5502156814092246381', 1, 0)
mobile2$app6384<- ifelse(mobile2$app_id == '-653184325015346384', 1, 0)
mobile2$app6658<- ifelse(mobile2$app_id == '-364933151656676658', 1, 0)
mobile2$app6883<- ifelse(mobile2$app_id == '8463724738272896883', 1, 0)
mobile2$app71252<- ifelse(mobile2$app_id == '-8065284662787171252', 1, 0)
mobile2$app7177<- ifelse(mobile2$app_id == '4710843757088657177', 1, 0)
mobile2$app7258<- ifelse(mobile2$app_id == '-6689096779184407258', 1, 0)
mobile2$app7347<- ifelse(mobile2$app_id == '7460082553072507347', 1, 0)
mobile2$app7372<- ifelse(mobile2$app_id == '-5720078949152207372', 1, 0)
mobile2$app7392<- ifelse(mobile2$app_id == '7314680456634697392', 1, 0)
mobile2$app74955<- ifelse(mobile2$app_id == '-4905950346553874955', 1, 0)
mobile2$app77482<- ifelse(mobile2$app_id == '-5305696816021977482', 1, 0)
mobile2$app78005<- ifelse(mobile2$app_id == '-2569104289038278005', 1, 0)
mobile2$app7991<- ifelse(mobile2$app_id == '6607018907660377991', 1, 0)
mobile2$app85647<- ifelse(mobile2$app_id == '-2534478848022085647', 1, 0)
mobile2$app88123<- ifelse(mobile2$app_id == '-5839858269967688123', 1, 0)
mobile2$app8828<- ifelse(mobile2$app_id == '-525778058644348828', 1, 0)
mobile2$app9590<- ifelse(mobile2$app_id == '5536354020515989590', 1, 0)
mobile2$app9611<- ifelse(mobile2$app_id == '7320912293625979611', 1, 0)
mobile2$app96897<- ifelse(mobile2$app_id == '-1814577379445496897', 1, 0)
mobile2$app99227<- ifelse(mobile2$app_id == '-1676142018957199227', 1, 0)

#Change to top 100 apps
mobile2$a00005  <- ifelse(mobile2$app_number =='00005', 1,0)
mobile2$a00001  <- ifelse(mobile2$app_number =='00001', 1,0)
mobile2$a00016  <- ifelse(mobile2$app_number =='00016', 1,0)
mobile2$a00013<- ifelse(mobile2$app_number=='00013', 1,0)
mobile2$a00037<- ifelse(mobile2$app_number =='00037', 1,0)
mobile2$a00178<- ifelse(mobile2$app_number =='00178', 1,0)
mobile2$a00015  <- ifelse(mobile2$app_number =='00015', 1,0)
mobile2$a00012  <- ifelse(mobile2$app_number =='00012', 1,0)
mobile2$a00036  <- ifelse(mobile2$app_number =='00036', 1,0)
mobile2$a00166<- ifelse(mobile2$app_number=='00166', 1,0)
mobile2$a00002  <- ifelse(mobile2$app_number =='00002', 1,0)
mobile2$a00025  <- ifelse(mobile2$app_number =='00025', 1,0)
mobile2$a00289  <- ifelse(mobile2$app_number =='00289', 1,0)
mobile2$a00127<- ifelse(mobile2$app_number=='00127', 1,0)
mobile2$a00020<- ifelse(mobile2$app_number =='00020', 1,0)
mobile2$a00173<- ifelse(mobile2$app_number =='00173', 1,0)
mobile2$a00077  <- ifelse(mobile2$app_number =='00077', 1,0)
mobile2$a00206  <- ifelse(mobile2$app_number =='00206', 1,0)
mobile2$a00035  <- ifelse(mobile2$app_number =='00035', 1,0)
mobile2$a00010<- ifelse(mobile2$app_number =='00010', 1,0)
mobile2$a00201<- ifelse(mobile2$app_number =='00201', 1,0)
mobile2$a00044  <- ifelse(mobile2$app_number =='00044', 1,0)
mobile2$a00066  <- ifelse(mobile2$app_number =='00066', 1,0)
mobile2$a00071  <- ifelse(mobile2$app_number =='00071', 1,0)
mobile2$a00017<- ifelse(mobile2$app_number=='00017', 1,0)
mobile2$a02410<- ifelse(mobile2$app_number =='02410', 1,0)
mobile2$a00051<- ifelse(mobile2$app_number =='00051', 1,0)
mobile2$a00093  <- ifelse(mobile2$app_number =='00093', 1,0)
mobile2$a00169  <- ifelse(mobile2$app_number =='00169', 1,0)
mobile2$a00103  <- ifelse(mobile2$app_number =='00103', 1,0)
mobile2$a00552<- ifelse(mobile2$app_number=='00552', 1,0)
mobile2$a00610<- ifelse(mobile2$app_number =='00610', 1,0)
mobile2$a00108<- ifelse(mobile2$app_number =='00108', 1,0)
mobile2$a00305   <- ifelse(mobile2$app_number =='00305', 1,0)
mobile2$a00167   <- ifelse(mobile2$app_number =='00167', 1,0)
mobile2$a00921   <- ifelse(mobile2$app_number =='00921', 1,0)
mobile2$a00162 <- ifelse(mobile2$app_number=='00162', 1,0)
mobile2$a00608 <- ifelse(mobile2$app_number =='00608', 1,0)
mobile2$a00815 <- ifelse(mobile2$app_number =='00815', 1,0)
mobile2$a00046  <- ifelse(mobile2$app_number =='00046', 1,0)
mobile2$a00210  <- ifelse(mobile2$app_number =='00210', 1,0)
mobile2$a00262  <- ifelse(mobile2$app_number =='00262', 1,0)
mobile2$a00393 <- ifelse(mobile2$app_number=='00393', 1,0)
mobile2$a00518 <- ifelse(mobile2$app_number =='00518', 1,0)
mobile2$a00128 <- ifelse(mobile2$app_number =='00128', 1,0)
mobile2$a00309   <- ifelse(mobile2$app_number =='00309', 1,0)
mobile2$a01919   <- ifelse(mobile2$app_number =='01919', 1,0)
mobile2$a01077 <- ifelse(mobile2$app_number=='01077', 1,0)
mobile2$a00104 <- ifelse(mobile2$app_number =='00104', 1,0)
mobile2$a00182 <- ifelse(mobile2$app_number =='00182', 1,0)
mobile2$a00301  <- ifelse(mobile2$app_number =='00301', 1,0)
mobile2$a00117  <- ifelse(mobile2$app_number =='00117', 1,0)
mobile2$a00151  <- ifelse(mobile2$app_number =='00151', 1,0)
mobile2$a00221 <- ifelse(mobile2$app_number=='00221', 1,0)
mobile2$a00135 <- ifelse(mobile2$app_number =='00135', 1,0)
mobile2$a00381 <- ifelse(mobile2$app_number =='00381', 1,0)
mobile2$a00816   <- ifelse(mobile2$app_number =='00816', 1,0)
mobile2$a00141   <- ifelse(mobile2$app_number =='00141', 1,0)
mobile2$a00205 <- ifelse(mobile2$app_number=='00205', 1,0)
mobile2$a00159 <- ifelse(mobile2$app_number =='00159', 1,0)
mobile2$a00153 <- ifelse(mobile2$app_number =='00153', 1,0)
mobile2$a00003  <- ifelse(mobile2$app_number =='00003', 1,0)
mobile2$a00014  <- ifelse(mobile2$app_number =='00014', 1,0)
mobile2$a00343  <- ifelse(mobile2$app_number =='00343', 1,0)
mobile2$a00363 <- ifelse(mobile2$app_number=='00363', 1,0)
mobile2$a00215 <- ifelse(mobile2$app_number =='00284', 1,0)
mobile2$a00525 <- ifelse(mobile2$app_number =='00525', 1,0)
mobile2$a00132   <- ifelse(mobile2$app_number =='00132', 1,0)
mobile2$a00170   <- ifelse(mobile2$app_number =='00170', 1,0)
mobile2$a00007 <- ifelse(mobile2$app_number=='00007', 1,0)
mobile2$a00106<- ifelse(mobile2$app_number =='00106', 1,0)
mobile2$a00039 <- ifelse(mobile2$app_number =='00039', 1,0)
mobile2$a00477  <- ifelse(mobile2$app_number =='00477', 1,0)
mobile2$a00018  <- ifelse(mobile2$app_number =='00018', 1,0)
mobile2$a00060  <- ifelse(mobile2$app_number =='00060', 1,0)
mobile2$a00040 <- ifelse(mobile2$app_number=='00040', 1,0)
mobile2$a00186 <- ifelse(mobile2$app_number =='00186', 1,0)
mobile2$a00371 <- ifelse(mobile2$app_number =='00371', 1,0)
mobile2$a00090   <- ifelse(mobile2$app_number =='00090', 1,0)
mobile2$a00112   <- ifelse(mobile2$app_number =='00112', 1,0)
mobile2$a00403 <- ifelse(mobile2$app_number=='00403', 1,0)
mobile2$a00129 <- ifelse(mobile2$app_number =='00129', 1,0)
mobile2$a00138  <- ifelse(mobile2$app_number =='00138', 1,0)
mobile2$a00341  <- ifelse(mobile2$app_number =='00341', 1,0)
mobile2$a00158  <- ifelse(mobile2$app_number =='00158', 1,0)
mobile2$a00123 <- ifelse(mobile2$app_number=='00123', 1,0)
mobile2$a00061 <- ifelse(mobile2$app_number =='00061', 1,0)
mobile2$a00052 <- ifelse(mobile2$app_number =='00052', 1,0)
mobile2$a00342   <- ifelse(mobile2$app_number =='00342', 1,0)
mobile2$a00069   <- ifelse(mobile2$app_number =='00069', 1,0)
mobile2$a00144 <- ifelse(mobile2$app_number=='00144', 1,0)
mobile2$a00160 <- ifelse(mobile2$app_number =='00160', 1,0)
mobile2$a00357 <- ifelse(mobile2$app_number =='00357', 1,0)
mobile2$a00021  <- ifelse(mobile2$app_number =='00021', 1,0)
mobile2 <-data.table(mobile2)
summary(mobile2$app_number)
#setkey(mobile2, NULL)
#mobile2 <- unique(mobile2)
summary(mobile2)
#Calculate app usage by UserID
by_device_app_id_count <- mobile2 %>%
  group_by(
    User_ID
  ) %>%
  dplyr::summarize(
    app85699=sum(app85699, na.rm=TRUE),
    app12353=sum(app12353, na.rm=TRUE),
    app13972=sum(app13972, na.rm=TRUE),
    app14790=sum(app14790, na.rm=TRUE),
    app2167=sum(app2167, na.rm=TRUE),
    app2087=sum(app2087, na.rm=TRUE),
    app2359=sum(app2359, na.rm=TRUE),
    app25125=sum(app25125, na.rm=TRUE),
    app2574=sum(app2574, na.rm=TRUE),
    app2612=sum(app2612, na.rm=TRUE),
    app2665=sum(app2665, na.rm=TRUE),
    app2671=sum(app2671, na.rm=TRUE),
    app2858=sum(app2858, na.rm=TRUE),
    app39155=sum(app39155, na.rm=TRUE),
    app4238=sum(app4238, na.rm=TRUE),
    app4338=sum(app4338, na.rm=TRUE),
    app45571=sum(app45571, na.rm=TRUE),
    app46293=sum(app46293, na.rm=TRUE),
    app4701=sum(app4701, na.rm=TRUE),
    app53300=sum(app53300, na.rm=TRUE),
    app5457=sum(app5457, na.rm=TRUE),
    app5642=sum(app5642, na.rm=TRUE),
    app5899=sum(app5899, na.rm=TRUE),
    app5961=sum(app5961, na.rm=TRUE),
    app6122=sum(app6122, na.rm=TRUE),
    app6364=sum(app6364, na.rm=TRUE),
    app6381=sum(app6381, na.rm=TRUE),
    app6384=sum(app6384, na.rm=TRUE),
    app6658=sum(app6658, na.rm=TRUE),
    app6883=sum(app6883, na.rm=TRUE),
    app71252=sum(app71252, na.rm=TRUE),
    app7177=sum(app7177, na.rm=TRUE),
    app7258=sum(app7258, na.rm=TRUE),
    app7347=sum(app7347, na.rm=TRUE),
    app7372=sum(app7372, na.rm=TRUE),
    app7392=sum(app7392, na.rm=TRUE),
    app74955=sum(app74955, na.rm=TRUE),
    app77482=sum(app77482, na.rm=TRUE),
    app78005=sum(app78005, na.rm=TRUE),
    app7991=sum(app7991, na.rm=TRUE),
    app85647=sum(app85647, na.rm=TRUE),
    app88123=sum(app88123, na.rm=TRUE),
    app8828=sum(app8828, na.rm=TRUE),
    app9590=sum(app9590, na.rm=TRUE),
    app9611=sum(app9611, na.rm=TRUE),
    app96897=sum(app96897, na.rm=TRUE),
    app99227=sum(app99227, na.rm=TRUE),
    a00005  =sum( a00005  , na.rm=TRUE),
    a00001  =sum( a00001  , na.rm=TRUE),
    a00002  =sum( a00002  , na.rm=TRUE),
    a00003  =sum( a00003  , na.rm=TRUE),
    a00007 =sum( a00007 , na.rm=TRUE),
    a00010=sum( a00010, na.rm=TRUE),
    a00012=sum( a00012, na.rm=TRUE),
    a00013=sum( a00013, na.rm=TRUE),
    a00014  =sum( a00014  , na.rm=TRUE),
    a00015  =sum( a00015  , na.rm=TRUE),
    a00016  =sum( a00016  , na.rm=TRUE),
    a00017=sum( a00017, na.rm=TRUE),
    a00018  =sum( a00018  , na.rm=TRUE),
    a00020=sum( a00020, na.rm=TRUE),
    a00021  =sum( a00021  , na.rm=TRUE),
    a00025  =sum( a00025  , na.rm=TRUE),
    a00035  =sum( a00035  , na.rm=TRUE),
    a00036  =sum( a00036  , na.rm=TRUE),
    a00037=sum( a00037, na.rm=TRUE),
    a00039 =sum( a00039 , na.rm=TRUE),
    a00040 =sum( a00040 , na.rm=TRUE),
    a00044  =sum( a00044  , na.rm=TRUE),
    a00046  =sum( a00046  , na.rm=TRUE),
    a00051=sum( a00051, na.rm=TRUE),
    a00052 =sum( a00052 , na.rm=TRUE),
    a00060  =sum( a00060  , na.rm=TRUE),
    a00061 =sum( a00061 , na.rm=TRUE),
    a00066  =sum( a00066  , na.rm=TRUE),
    a00069   =sum( a00069   , na.rm=TRUE),
    a00071  =sum( a00071  , na.rm=TRUE),
    a00077  =sum( a00077  , na.rm=TRUE),
    a00090   =sum( a00090   , na.rm=TRUE),
    a00093  =sum( a00093  , na.rm=TRUE),
    a00103  =sum( a00103  , na.rm=TRUE),
    a00104 =sum( a00104 , na.rm=TRUE),
    a00106=sum( a00106, na.rm=TRUE),
    a00108=sum( a00108, na.rm=TRUE),
    a00112 =sum( a00112 , na.rm=TRUE),
    a00117  =sum( a00117  , na.rm=TRUE),
    a00123 =sum( a00123 , na.rm=TRUE),
    a00127=sum( a00127, na.rm=TRUE),
    a00128 =sum( a00128 , na.rm=TRUE),
    a00129 =sum( a00129 , na.rm=TRUE),
    a00132   =sum( a00132   , na.rm=TRUE),
    a00135 =sum( a00135 , na.rm=TRUE),
    a00138  =sum( a00138  , na.rm=TRUE),
    a00141   =sum( a00141   , na.rm=TRUE),
    a00144 =sum( a00144 , na.rm=TRUE),
    a00151  =sum( a00151  , na.rm=TRUE),
    a00153 =sum( a00153 , na.rm=TRUE),
    a00158  =sum( a00158  , na.rm=TRUE),
    a00159 =sum( a00159 , na.rm=TRUE),
    a00160 =sum( a00160 , na.rm=TRUE),
    a00162 =sum( a00162 , na.rm=TRUE),
    a00166=sum( a00166, na.rm=TRUE),
    a00167   =sum( a00167   , na.rm=TRUE),
    a00169  =sum( a00169  , na.rm=TRUE),
    a00170   =sum( a00170   , na.rm=TRUE),
    a00173=sum( a00173, na.rm=TRUE),
    a00178=sum( a00178, na.rm=TRUE),
    a00182 =sum( a00182 , na.rm=TRUE),
    a00186 =sum( a00186 , na.rm=TRUE),
    a00201=sum( a00201, na.rm=TRUE),
    a00205 =sum( a00205 , na.rm=TRUE),
    a00206  =sum( a00206  , na.rm=TRUE),
    a00210  =sum( a00210  , na.rm=TRUE),
    a00215 =sum( a00215 , na.rm=TRUE),
    a00221 =sum( a00221 , na.rm=TRUE),
    a00262  =sum( a00262  , na.rm=TRUE),
    a00289  =sum( a00289  , na.rm=TRUE),
    a00301  =sum( a00301  , na.rm=TRUE),
    a00305   =sum( a00305   , na.rm=TRUE),
    a00309   =sum( a00309   , na.rm=TRUE),
    a00341  =sum( a00341  , na.rm=TRUE),
    a00342   =sum( a00342   , na.rm=TRUE),
    a00343  =sum( a00343  , na.rm=TRUE),
    a00357 =sum( a00357 , na.rm=TRUE),
    a00363 =sum( a00363 , na.rm=TRUE),
    a00371 =sum( a00371 , na.rm=TRUE),
    a00381 =sum( a00381 , na.rm=TRUE),
    a00393 =sum( a00393 , na.rm=TRUE),
    a00403 =sum( a00403 , na.rm=TRUE),
    a00477  =sum( a00477  , na.rm=TRUE),
    a00518 =sum( a00518 , na.rm=TRUE),
    a00525 =sum( a00525 , na.rm=TRUE),
    a00552=sum( a00552, na.rm=TRUE),
    a00608 =sum( a00608 , na.rm=TRUE),
    a00610=sum( a00610, na.rm=TRUE),
    a00815 =sum( a00815 , na.rm=TRUE),
    a00816   =sum( a00816   , na.rm=TRUE),
    a00921   =sum( a00921   , na.rm=TRUE),
    a01077 =sum( a01077 , na.rm=TRUE),
    a01919   =sum( a01919   , na.rm=TRUE),
    a02410=sum( a02410, na.rm=TRUE)
  )

mobile2$app85699	<- NULL
mobile2$app12353	<- NULL
mobile2$app13972	<- NULL
mobile2$app14790	<- NULL
mobile2$app2167	<- NULL
mobile2$app2087	<- NULL
mobile2$app2359	<- NULL
mobile2$app25125	<- NULL
mobile2$app2574	<- NULL
mobile2$app2612	<- NULL
mobile2$app2665	<- NULL
mobile2$app2671	<- NULL
mobile2$app2858	<- NULL
mobile2$app39155	<- NULL
mobile2$app4238	<- NULL
mobile2$app4338	<- NULL
mobile2$app45571	<- NULL
mobile2$app46293	<- NULL
mobile2$app4701	<- NULL
mobile2$app53300	<- NULL
mobile2$app5457	<- NULL
mobile2$app5642	<- NULL
mobile2$app5899	<- NULL
mobile2$app5961	<- NULL
mobile2$app6122	<- NULL
mobile2$app6364	<- NULL
mobile2$app6381	<- NULL
mobile2$app6384	<- NULL
mobile2$app6658	<- NULL
mobile2$app6883	<- NULL
mobile2$app71252	<- NULL
mobile2$app7177	<- NULL
mobile2$app7258	<- NULL
mobile2$app7347	<- NULL
mobile2$app7372	<- NULL
mobile2$app7392	<- NULL
mobile2$app74955	<- NULL
mobile2$app77482	<- NULL
mobile2$app78005	<- NULL
mobile2$app7991	<- NULL
mobile2$app85647	<- NULL
mobile2$app88123	<- NULL
mobile2$app8828	<- NULL
mobile2$app9590	<- NULL
mobile2$app9611	<- NULL
mobile2$app96897	<- NULL
mobile2$app99227	<- NULL
mobile2$a00005<- NULL
mobile2$a00001<- NULL
mobile2$a00016<- NULL
mobile2$a00013<- NULL
mobile2$a00037<- NULL
mobile2$a00178<- NULL
mobile2$a00015<- NULL
mobile2$a00012<- NULL
mobile2$a00036<- NULL
mobile2$a00166<- NULL
mobile2$a00002<- NULL
mobile2$a00025<- NULL
mobile2$a00289<- NULL
mobile2$a00127<- NULL
mobile2$a00020<- NULL
mobile2$a00173<- NULL
mobile2$a00077<- NULL
mobile2$a00206<- NULL
mobile2$a00035<- NULL
mobile2$a00010<- NULL
mobile2$a00201<- NULL
mobile2$a00044<- NULL
mobile2$a00066<- NULL
mobile2$a00071<- NULL
mobile2$a00017<- NULL
mobile2$a02410<- NULL
mobile2$a00051<- NULL
mobile2$a00093<- NULL
mobile2$a00169<- NULL
mobile2$a00103<- NULL
mobile2$a00552<- NULL
mobile2$a00610<- NULL
mobile2$a00108<- NULL
mobile2$a00305<- NULL
mobile2$a00167<- NULL
mobile2$a00921<- NULL
mobile2$a00162<- NULL
mobile2$a00608<- NULL
mobile2$a00815<- NULL
mobile2$a00046<- NULL
mobile2$a00210<- NULL
mobile2$a00262<- NULL
mobile2$a00393<- NULL
mobile2$a00518<- NULL
mobile2$a00128<- NULL
mobile2$a00309<- NULL
mobile2$a01919<- NULL
mobile2$a01077<- NULL
mobile2$a00104<- NULL
mobile2$a00182<- NULL
mobile2$a00301<- NULL
mobile2$a00117<- NULL
mobile2$a00151<- NULL
mobile2$a00221<- NULL
mobile2$a00135<- NULL
mobile2$a00381<- NULL
mobile2$a00816<- NULL
mobile2$a00141<- NULL
mobile2$a00205<- NULL
mobile2$a00159<- NULL
mobile2$a00153<- NULL
mobile2$a00003<- NULL
mobile2$a00014<- NULL
mobile2$a00343<- NULL
mobile2$a00363<- NULL
mobile2$a00215<- NULL
mobile2$a00525<- NULL
mobile2$a00132<- NULL
mobile2$a00170<- NULL
mobile2$a00007<- NULL
mobile2$a00106<- NULL
mobile2$a00039<- NULL
mobile2$a00477<- NULL
mobile2$a00018<- NULL
mobile2$a00060<- NULL
mobile2$a00040<- NULL
mobile2$a00186<- NULL
mobile2$a00371<- NULL
mobile2$a00090<- NULL
mobile2$a00112<- NULL
mobile2$a00403<- NULL
mobile2$a00129<- NULL
mobile2$a00138<- NULL
mobile2$a00341<- NULL
mobile2$a00158<- NULL
mobile2$a00123<- NULL
mobile2$a00061<- NULL
mobile2$a00052<- NULL
mobile2$a00342<- NULL
mobile2$a00069<- NULL
mobile2$a00144<- NULL
mobile2$a00160<- NULL
mobile2$a00357<- NULL
mobile2$a00021<- NULL


mobile2$group <- NULL
mobile2$device_id <- NULL
setkey(mobile2, NULL)
mobile2 <- unique(mobile2)
dim(mobile2)

head(mobile2)
mobile.apps2 <- merge(mobile2, by_device_app_id_count, by = 'User_ID')
mobile.apps2 <- unique(mobile.apps2)
dim(mobile.apps2)
names(mobile.apps2)

app_labels <- fread("~/app_labels.csv/app_labels.csv", head=T) # load the aggregated data file
head(app_labels)
app_labels$app_id <- as.character(app_labels$app_id)
str(app_labels)
summary(app_labels)
setkey(app_labels, app_id)
mobile.l <- merge(mobile.apps2, app_labels, by = 'app_id', allow.cartesian=TRUE)
head(mobile.l)

setkey(mobile.l, NULL)
mobile.l<-unique(mobile.l)
rm(mobile2, by_device_app_id_count, mobile.apps2,app_labels)
mobile.l$label_id <- as.factor(mobile.l$label_id)
summary(mobile.l)

##### Combining correlated labels
summary(mobile.l$label_id==918)
summary(mobile.l$label_id==927)


mobile.l$l918<-ifelse(mobile.l$label_id==918, 1,0)
mobile.l$l927<-ifelse(mobile.l$label_id==927,1,0)
mobile.l$l918.927<- ifelse((mobile.l$label_id==918), ( 0.607362* mobile.l$l918) + ( 0.607362 * mobile.l$l927), 0)
mobile.l$l918<-NULL
mobile.l$l927=NULL
summary(mobile.l$l918.927)

mobile.l$l731<-ifelse(mobile.l$label_id==731, 1, 0)
mobile.l$l732<-ifelse(mobile.l$label_id==732,1,0)
mobile.l$l731.732<- ifelse((mobile.l$label_id==731),( 0.9993352* mobile.l$l731) +  (0.9993352 * mobile.l$l732), 0)
summary(mobile.l$l731.732)
mobile.l$l731=NULL
mobile.l$l732=NULL


mobile.l$l222<-ifelse(mobile.l$label_id==222,1,0)
mobile.l$l279<-ifelse(mobile.l$label_id==279,1,0)
mobile.l$l222.279<- ifelse((mobile.l$label_id==222),( 0.7083629* mobile.l$l222) + ( 0.7083629 * mobile.l$l279), 0)
summary(mobile.l$l222.279)
mobile.l$l279=NULL
mobile.l$l222=NULL

###########################
mobile.l$l255<-ifelse(mobile.l$label_id==255,1,0)
mobile.l$l749<-ifelse(mobile.l$label_id==749,1,0)
mobile.l$l255.749<- ifelse((mobile.l$label_id==255),(0.9135169*mobile.l$l255)+ (0.9135169*mobile.l$l749), 0)
mobile.l$l749=NULL
mobile.l$l747<-ifelse(mobile.l$label_id==747,1,0)
mobile.l$l255.747<- ifelse((mobile.l$label_id==255),(0.949056*mobile.l$l255)+ (0.949056*mobile.l$l747), 0)
mobile.l$l255=NULL
mobile.l$l747=NULL
mobile.l$l255.749<-NULL
summary(mobile.l$l255.747)


mobile.l$l818<-ifelse(mobile.l$label_id==818,1,0)
mobile.l$l820<-ifelse(mobile.l$label_id==820,1,0)
mobile.l$l818.820<- ifelse((mobile.l$label_id==818),(0.7292563 * mobile.l$l818) + (0.7292563*mobile.l$l820), 0)
mobile.l$l818=NULL
mobile.l$l820=NULL
summary(mobile.l$l818.820)

mobile.l$l937<-ifelse(mobile.l$label_id==937,1,0)
mobile.l$l933<-ifelse(mobile.l$label_id==933,1,0)
mobile.l$l937.933<- ifelse((mobile.l$label_id==937),((0.8314258 * mobile.l$l937) + (0.8314258*mobile.l$l933)), 0)
mobile.l$l937=NULL
mobile.l$l933=NULL
summary(mobile.l$l937.933)

mobile.l$l14<-ifelse(mobile.l$label_id==14,1,0)
mobile.l$l21<-ifelse(mobile.l$label_id==21,1,0)
mobile.l$l2<-ifelse(mobile.l$label_id==2,1,0)
mobile.l$l14.21.2 <- ifelse((mobile.l$label_id==14), (mobile.l$l14+ mobile.l$l21+ mobile.l$l2), 0)
summary(mobile.l$l14.21.2)
mobile.l$l21=NULL
mobile.l$l14=NULL
mobile.l$l2=NULL



mobile.l$l737<-ifelse(mobile.l$label_id==737,1,0)
mobile.l$l738<-ifelse(mobile.l$label_id==738,1,0)
mobile.l$l737.738<- ifelse((mobile.l$label_id==737),(0.9845231*mobile.l$l737) + (0.9845231*mobile.l$l738), 0)
summary(mobile.l$l737.738)
mobile.l$l737=NULL
mobile.l$l738=NULL



mobile.l$l204<-ifelse(mobile.l$label_id==204,1,0)
mobile.l$l205<-ifelse(mobile.l$label_id==205,1,0)
mobile.l$l204.205<- ifelse((mobile.l$label_id==204),(mobile.l$l204*0.6806337) +(0.6806337*mobile.l$l205), 0)
summary(mobile.l$l204.205)
mobile.l$l204=NULL
mobile.l$l205=NULL


mobile.l$l552<-ifelse(mobile.l$label_id==552,1,0)
mobile.l$l555<-ifelse(mobile.l$label_id==555,1,0)
mobile.l$l552.555<- ifelse((mobile.l$label_id==552),(mobile.l$l552+ mobile.l$l555), 0)
summary(mobile.l$l552.555)
mobile.l$l552=NULL
mobile.l$l555=NULL


mobile.l$l939<-ifelse(mobile.l$label_id==939,1,0)
mobile.l$l940<-ifelse(mobile.l$label_id==940,1,0)
mobile.l$l939.940<- ifelse((mobile.l$label_id==939),(mobile.l$l939+ mobile.l$l940), 0)
summary(mobile.l$l939.940)
mobile.l$l939=NULL
mobile.l$l940=NULL


mobile.l$l941<-ifelse(mobile.l$label_id==941,1,0)
mobile.l$l942<-ifelse(mobile.l$label_id==942,1,0)
mobile.l$l941.942<- ifelse((mobile.l$label_id==941),(mobile.l$l941+ mobile.l$l942), 0)
summary(mobile.l$l941.942)
mobile.l$l941=NULL
mobile.l$l942=NULL


mobile.l$l954<-ifelse(mobile.l$label_id==954,1,0)
mobile.l$l955<-ifelse(mobile.l$label_id==955,1,0)
mobile.l$l954.955<- ifelse((mobile.l$label_id==954),(mobile.l$l954+ mobile.l$l955), 0)
summary(mobile.l$l954.955)
mobile.l$l954=NULL
mobile.l$l955=NULL


mobile.l$l959<-ifelse(mobile.l$label_id==959,1,0)
mobile.l$l960<-ifelse(mobile.l$label_id==960,1,0)
mobile.l$l959.960<- ifelse((mobile.l$label_id==959),(mobile.l$l959+ mobile.l$l960), 0)
summary(mobile.l$l959.960)
mobile.l$l959=NULL
mobile.l$l960=NULL



####################################
mobile.l$l7746	<- ifelse(mobile.l$label_id==46	,1,0)
mobile.l$l1003	<- ifelse(mobile.l$label_id==1003	,1,0)
mobile.l$l1007	<- ifelse(	mobile.l$label_id==1007	,1,0)
mobile.l$l1008	<- ifelse(	mobile.l$label_id==1008	,1,0)
mobile.l$l1012	<- ifelse(	mobile.l$label_id==1012	,1,0)
mobile.l$l1015	<- ifelse(	mobile.l$label_id==1015	,1,0)
mobile.l$l128	<- ifelse(	mobile.l$label_id==128	,1,0)
mobile.l$l172	<- ifelse(	mobile.l$label_id==172	,1,0)
mobile.l$l179	<- ifelse(	mobile.l$label_id==179	,1,0)
mobile.l$l251	<- ifelse(	mobile.l$label_id==251	,1,0)
mobile.l$l252	<- ifelse(	mobile.l$label_id==252	,1,0)
mobile.l$l254	<- ifelse(	mobile.l$label_id==254	,1,0)
mobile.l$l262	<- ifelse(	mobile.l$label_id==262	,1,0)
mobile.l$l263	<- ifelse(	mobile.l$label_id==263	,1,0)
mobile.l$l302	<- ifelse(	mobile.l$label_id==302	,1,0)
mobile.l$l303	<- ifelse(	mobile.l$label_id==303	,1,0)
mobile.l$l306	<- ifelse(	mobile.l$label_id==306	,1,0)
mobile.l$l405	<- ifelse(	mobile.l$label_id==405	,1,0)
mobile.l$l548	<- ifelse(	mobile.l$label_id==548	,1,0)
mobile.l$l549	<- ifelse(	mobile.l$label_id==549	,1,0)
mobile.l$l564	<- ifelse(	mobile.l$label_id==564	,1,0)
mobile.l$l691	<- ifelse(	mobile.l$label_id==691	,1,0)
mobile.l$l704	<- ifelse(	mobile.l$label_id==704	,1,0)
mobile.l$l710	<- ifelse(	mobile.l$label_id==710	,1,0)
mobile.l$l713	<- ifelse(	mobile.l$label_id==713	,1,0)
mobile.l$l714	<- ifelse(	mobile.l$label_id==714	,1,0)
mobile.l$l721	<- ifelse(	mobile.l$label_id==721	,1,0)
mobile.l$l724	<- ifelse(	mobile.l$label_id==724	,1,0)
mobile.l$l730	<- ifelse(	mobile.l$label_id==730	,1,0)
mobile.l$l751	<- ifelse(	mobile.l$label_id==751	,1,0)
mobile.l$l756	<- ifelse(	mobile.l$label_id==756	,1,0)
mobile.l$l757	<- ifelse(	mobile.l$label_id==757	,1,0)
mobile.l$l759	<- ifelse(	mobile.l$label_id==759	,1,0)
mobile.l$l761	<- ifelse(	mobile.l$label_id==761	,1,0)
mobile.l$l773	<- ifelse(	mobile.l$label_id==773	,1,0)
mobile.l$l775	<- ifelse(	mobile.l$label_id==775	,1,0)
mobile.l$l777	<- ifelse(	mobile.l$label_id==777	,1,0)
mobile.l$l779	<- ifelse(	mobile.l$label_id==779	,1,0)
mobile.l$l780	<- ifelse(	mobile.l$label_id==780	,1,0)
mobile.l$l781	<- ifelse(	mobile.l$label_id==781	,1,0)
mobile.l$l782	<- ifelse(	mobile.l$label_id==782	,1,0)
mobile.l$l783	<- ifelse(	mobile.l$label_id==783	,1,0)
mobile.l$l786	<- ifelse(	mobile.l$label_id==786	,1,0)
mobile.l$l787	<- ifelse(	mobile.l$label_id==787	,1,0)
mobile.l$l959	<- ifelse(	mobile.l$label_id==959	,1,0)
mobile.l$l960	<- ifelse(	mobile.l$label_id==960	,1,0)
mobile.l$l61	<- ifelse(	mobile.l$label_id == 61	,1,0)
mobile.l$l89	<- ifelse(	mobile.l$label_id == 89	,1,0)
mobile.l$l97	<- ifelse(	mobile.l$label_id == 97	,1,0)
mobile.l$l98	<- ifelse(	mobile.l$label_id == 98	,1,0)
mobile.l$l99	<- ifelse(	mobile.l$label_id == 99	,1,0)
mobile.l$l104	<- ifelse(	mobile.l$label_id == 104	,1,0)
mobile.l$l134	<- ifelse(	mobile.l$label_id == 134	,1,0)
mobile.l$l138	<- ifelse(	mobile.l$label_id == 138	,1,0)
mobile.l$l139	<- ifelse(	mobile.l$label_id == 139	,1,0)
mobile.l$l154	<- ifelse(	mobile.l$label_id == 154	,1,0)
mobile.l$l156	<- ifelse(	mobile.l$label_id == 156	,1,0)
mobile.l$l159	<- ifelse(	mobile.l$label_id == 159	,1,0)
mobile.l$l166	<- ifelse(	mobile.l$label_id == 166	,1,0)
mobile.l$l181	<- ifelse(	mobile.l$label_id == 181	,1,0)
mobile.l$l183	<- ifelse(	mobile.l$label_id == 183	,1,0)
mobile.l$l188	<- ifelse(	mobile.l$label_id == 188	,1,0)
mobile.l$l189	<- ifelse(	mobile.l$label_id == 189	,1,0)
mobile.l$l190	<- ifelse(	mobile.l$label_id == 190	,1,0)
mobile.l$l191	<- ifelse(	mobile.l$label_id == 191	,1,0)
mobile.l$l192	<- ifelse(	mobile.l$label_id == 192	,1,0)
mobile.l$l193	<- ifelse(	mobile.l$label_id == 193	,1,0)
mobile.l$l196	<- ifelse(	mobile.l$label_id == 196	,1,0)
mobile.l$l198	<- ifelse(	mobile.l$label_id == 198	,1,0)
mobile.l$l207	<- ifelse(	mobile.l$label_id == 207	,1,0)
mobile.l$l211	<- ifelse(	mobile.l$label_id == 211	,1,0)
mobile.l$l212	<- ifelse(	mobile.l$label_id == 212	,1,0)
mobile.l$l213	<- ifelse(	mobile.l$label_id == 213	,1,0)
mobile.l$l214	<- ifelse(	mobile.l$label_id == 214	,1,0)
mobile.l$l217	<- ifelse(	mobile.l$label_id == 217	,1,0)
mobile.l$l218	<- ifelse(	mobile.l$label_id == 218	,1,0)
mobile.l$l220	<- ifelse(	mobile.l$label_id == 220	,1,0)
mobile.l$l221	<- ifelse(	mobile.l$label_id == 221	,1,0)
mobile.l$l222	<- ifelse(	mobile.l$label_id == 222	,1,0)
mobile.l$l224	<- ifelse(	mobile.l$label_id == 224	,1,0)
mobile.l$l226	<- ifelse(	mobile.l$label_id == 226	,1,0)
mobile.l$l227	<- ifelse(	mobile.l$label_id == 227	,1,0)
mobile.l$l228	<- ifelse(	mobile.l$label_id == 228	,1,0)
mobile.l$l230	<- ifelse(	mobile.l$label_id == 230	,1,0)
mobile.l$l231	<- ifelse(	mobile.l$label_id == 231	,1,0)
mobile.l$l232	<- ifelse(	mobile.l$label_id == 232	,1,0)
mobile.l$l233	<- ifelse(	mobile.l$label_id == 233	,1,0)
mobile.l$l234	<- ifelse(	mobile.l$label_id == 234	,1,0)
mobile.l$l237	<- ifelse(	mobile.l$label_id == 237	,1,0)
mobile.l$l238	<- ifelse(	mobile.l$label_id == 238	,1,0)
mobile.l$l246	<- ifelse(	mobile.l$label_id == 246	,1,0)
mobile.l$l247	<- ifelse(	mobile.l$label_id == 247	,1,0)
mobile.l$l255	<- ifelse(	mobile.l$label_id == 255	,1,0)
mobile.l$l256	<- ifelse(	mobile.l$label_id == 256	,1,0)
mobile.l$l258	<- ifelse(	mobile.l$label_id == 258	,1,0)
mobile.l$l259	<- ifelse(	mobile.l$label_id == 259	,1,0)
mobile.l$l260	<- ifelse(	mobile.l$label_id == 260	,1,0)
mobile.l$l261	<- ifelse(	mobile.l$label_id == 261	,1,0)
mobile.l$l266	<- ifelse(	mobile.l$label_id == 266	,1,0)
mobile.l$l267	<- ifelse(	mobile.l$label_id == 267	,1,0)
mobile.l$l273	<- ifelse(	mobile.l$label_id == 273	,1,0)
mobile.l$l274	<- ifelse(	mobile.l$label_id == 274	,1,0)
mobile.l$l316	<- ifelse(	mobile.l$label_id == 316	,1,0)
mobile.l$l194	<- ifelse(	mobile.l$label_id == 194	,1,0)
mobile.l$l195	<- ifelse(	mobile.l$label_id == 195	,1,0)
mobile.l$l184	<- ifelse(	mobile.l$label_id == 184	,1,0)
mobile.l$l158	<- ifelse(	mobile.l$label_id == 158	,1,0)
mobile.l$l185	<- ifelse(	mobile.l$label_id == 185	,1,0)
mobile.l$l186	<- ifelse(	mobile.l$label_id == 186	,1,0)
mobile.l$l103	<- ifelse(	mobile.l$label_id == 103	,1,0)
mobile.l$l100	<- ifelse(	mobile.l$label_id == 100	,1,0)
mobile.l$l65	<- ifelse(	mobile.l$label_id == 65	,1,0)
mobile.l$l169	<- ifelse(	mobile.l$label_id == 169	,1,0)
mobile.l$l170	<- ifelse(	mobile.l$label_id == 170	,1,0)
mobile.l$l954.955= ifelse((mobile.l$l954.955> 0) & (mobile.l$l954.955<1), 1, mobile.l$l954.955)
mobile.l$l918.927=ifelse((mobile.l$l918.927> 0) & (mobile.l$l918.927<1), 1, mobile.l$l918.927)
mobile.l$l959.960= ifelse((mobile.l$l959.960> 0) & (mobile.l$l959.960<1), 1, mobile.l$l959.960)
mobile.l$l731.732= ifelse((mobile.l$l731.732> 0) & (mobile.l$l731.732<1), 1, mobile.l$l731.732)
mobile.l$l222.279= ifelse((mobile.l$l222.279> 0) & (mobile.l$l222.279<1), 1, mobile.l$l222.279)
mobile.l$l255.747= ifelse((mobile.l$l255.747> 0) & (mobile.l$l255.747<1), 1, mobile.l$l255.747)
mobile.l$l818.820= ifelse((mobile.l$l818.820> 0) & (mobile.l$l818.820<1), 1, mobile.l$l818.820)
mobile.l$l941.942= ifelse((mobile.l$l941.942> 0) & (mobile.l$l941.942<1), 1, mobile.l$l941.942)
mobile.l$l939.940= ifelse((mobile.l$l939.940> 0) & (mobile.l$l939.940<1), 1, mobile.l$l939.940)
mobile.l$l937.933= ifelse((mobile.l$l937.933> 0) & (mobile.l$l937.933<1), 1, mobile.l$l937.933)
mobile.l$l552.555= ifelse((mobile.l$l552.555> 0) & (mobile.l$l552.555<1), 1, mobile.l$l552.555)
mobile.l$l204.205= ifelse((mobile.l$l204.205> 0) & (mobile.l$l204.205<1), 1, mobile.l$l204.205)
mobile.l$l737.738= ifelse((mobile.l$l737.738> 0) & (mobile.l$l737.738<1), 1, mobile.l$l737.738)
mobile.l$l14.21.2= ifelse((mobile.l$l14.21.2> 0) & (mobile.l$l14.21.2<1), 1, mobile.l$l14.21.2)

by_device_label_id_count <- mobile.l %>%
  group_by(
    User_ID
  ) %>%
  dplyr::summarize(
    l954.955= sum(l954.955, na.rm = TRUE),
    l918.927=sum(l918.927, na.rm = TRUE),
    l959.960= sum(l959.960, na.rm = TRUE),
    l731.732= sum(l731.732, na.rm = TRUE),
    l222.279= sum(l222.279, na.rm = TRUE),
    l255.747= sum(l255.747, na.rm = TRUE),
    l818.820= sum(l818.820, na.rm = TRUE),
    l941.942= sum(l941.942, na.rm = TRUE),
    l939.940= sum(l939.940, na.rm = TRUE),
    l937.933= sum(l937.933, na.rm = TRUE),
    l552.555= sum(l552.555, na.rm = TRUE),
    l204.205= sum(l204.205, na.rm = TRUE),
    l737.738= sum(l737.738, na.rm = TRUE),
    l14.21.2= sum(l14.21.2, na.rm = TRUE),
    l7746	= sum(l7746, na.rm = TRUE),
    l1003=sum(l1003,na.rm=TRUE),
    l1007=sum(l1007,na.rm=TRUE),
    l1008=sum(l1008,na.rm=TRUE),
    l1012=sum(l1012,na.rm=TRUE),
    l1015=sum(l1015,na.rm=TRUE),
    l128=sum(l128,na.rm=TRUE),
    l172=sum(l172,na.rm=TRUE),
    l179=sum(l179,na.rm=TRUE),
    l251=sum(l251,na.rm=TRUE),
    l252=sum(l252,na.rm=TRUE),
    l254=sum(l254,na.rm=TRUE),
    l262=sum(l262,na.rm=TRUE),
    l263=sum(l263,na.rm=TRUE),
    l302=sum(l302,na.rm=TRUE),
    l303=sum(l303,na.rm=TRUE),
    l306=sum(l306,na.rm=TRUE),
    l405=sum(l405,na.rm=TRUE),
    l548=sum(l548,na.rm=TRUE),
    l549=sum(l549,na.rm=TRUE),
    l564=sum(l564,na.rm=TRUE),
    l691=sum(l691,na.rm=TRUE),
    l704=sum(l704,na.rm=TRUE),
    l710=sum(l710,na.rm=TRUE),
    l713=sum(l713,na.rm=TRUE),
    l714=sum(l714,na.rm=TRUE),
    l721=sum(l721,na.rm=TRUE),
    l724=sum(l724,na.rm=TRUE),
    l730=sum(l730,na.rm=TRUE),
    l751=sum(l751,na.rm=TRUE),
    l756=sum(l756,na.rm=TRUE),
    l757=sum(l757,na.rm=TRUE),
    l759=sum(l759,na.rm=TRUE),
    l761=sum(l761,na.rm=TRUE),
    l773=sum(l773,na.rm=TRUE),
    l775=sum(l775,na.rm=TRUE),
    l777=sum(l777,na.rm=TRUE),
    l779=sum(l779,na.rm=TRUE),
    l780=sum(l780,na.rm=TRUE),
    l781=sum(l781,na.rm=TRUE),
    l782=sum(l782,na.rm=TRUE),
    l783=sum(l783,na.rm=TRUE),
    l786=sum(l786,na.rm=TRUE),
    l787=sum(l787,na.rm=TRUE),
    l959=sum(l959,na.rm=TRUE),
    l960=sum(l960,na.rm=TRUE),
    l61=sum(l61, na.rm=TRUE),
    l89=sum(l89, na.rm=TRUE),
    l97=sum(l97, na.rm=TRUE),
    l98=sum(l98, na.rm=TRUE),
    l99=sum(l99, na.rm=TRUE),
    l104=sum(l104, na.rm=TRUE),
    l134=sum(l134, na.rm=TRUE),
    l138=sum(l138, na.rm=TRUE),
    l139=sum(l139, na.rm=TRUE),
    l154=sum(l154, na.rm=TRUE),
    l156=sum(l156, na.rm=TRUE),
    l159=sum(l159, na.rm=TRUE),
    l166=sum(l166, na.rm=TRUE),
    l181=sum(l181, na.rm=TRUE),
    l183=sum(l183, na.rm=TRUE),
    l188=sum(l188, na.rm=TRUE),
    l189=sum(l189, na.rm=TRUE),
    l190=sum(l190, na.rm=TRUE),
    l191=sum(l191, na.rm=TRUE),
    l192=sum(l192, na.rm=TRUE),
    l193=sum(l193, na.rm=TRUE),
    l198=sum(l198, na.rm=TRUE),
    l207=sum(l207, na.rm=TRUE),
    l211=sum(l211, na.rm=TRUE),
    l212=sum(l212, na.rm=TRUE),
    l213=sum(l213, na.rm=TRUE),
    l214=sum(l214, na.rm=TRUE),
    l217=sum(l217, na.rm=TRUE),
    l220=sum(l220, na.rm=TRUE),
    l221=sum(l221, na.rm=TRUE),
    l222=sum(l222, na.rm=TRUE),
    l224=sum(l224, na.rm=TRUE),
    l226=sum(l226, na.rm=TRUE),
    l227=sum(l227, na.rm=TRUE),
    l230=sum(l230, na.rm=TRUE),
    l231=sum(l231, na.rm=TRUE),
    l232=sum(l232, na.rm=TRUE),
    l233=sum(l233, na.rm=TRUE),
    l234=sum(l234, na.rm=TRUE),
    l237=sum(l237, na.rm=TRUE),
    l238=sum(l238, na.rm=TRUE),
    l246=sum(l246, na.rm=TRUE),
    l247=sum(l247, na.rm=TRUE),
    l255=sum(l255, na.rm=TRUE),
    l256=sum(l256, na.rm=TRUE),
    l258=sum(l258, na.rm=TRUE),
    l259=sum(l259, na.rm=TRUE),
    l260=sum(l260, na.rm=TRUE),
    l261=sum(l261, na.rm=TRUE),
    l266=sum(l266, na.rm=TRUE),
    l267=sum(l267, na.rm=TRUE),
    l273=sum(l273, na.rm=TRUE),
    l274=sum(l274, na.rm=TRUE),
    l316=sum(l316, na.rm=TRUE),
    l194=sum(l194, na.rm=TRUE),
    l195=sum(l195, na.rm=TRUE),
    l184=sum(l184, na.rm=TRUE),
    l158=sum(l158, na.rm=TRUE),
    l185=sum(l185, na.rm=TRUE),
    l186=sum(l186, na.rm=TRUE),
    l103=sum(l103, na.rm=TRUE),
    l100=sum(l100, na.rm=TRUE),
    l65=sum(l65, na.rm=TRUE),
    l169=sum(l169, na.rm=TRUE),
    l170=sum(l170, na.rm=TRUE)
    
  )

mobile.l$l954.955<- NULL 
mobile.l$l918.927<- NULL
mobile.l$l959.960<- NULL 
mobile.l$l731.732<- NULL    
mobile.l$l222.279<- NULL
mobile.l$l255.747<- NULL
mobile.l$l818.820<- NULL 
mobile.l$l941.942<- NULL 
mobile.l$l939.940<- NULL
mobile.l$l937.933<- NULL
mobile.l$l552.555<- NULL
mobile.l$l204.205<- NULL 
mobile.l$l737.738<- NULL 
mobile.l$l14.21.2<- NULL
mobile.l$l7746	<- NULL
mobile.l$l1003	<- NULL
mobile.l$l1007	<- NULL
mobile.l$l1008	<- NULL
mobile.l$l1012	<- NULL
mobile.l$l1015	<- NULL
mobile.l$l128	<- NULL
mobile.l$l172	<- NULL
mobile.l$l179	<- NULL
mobile.l$l251	<- NULL
mobile.l$l252	<- NULL
mobile.l$l254	<- NULL
mobile.l$l262	<- NULL
mobile.l$l263	<- NULL
mobile.l$l302	<- NULL
mobile.l$l303	<- NULL
mobile.l$l306	<- NULL
mobile.l$l405	<- NULL
mobile.l$l548	<- NULL
mobile.l$l549	<- NULL
mobile.l$l564	<- NULL
mobile.l$l691	<- NULL
mobile.l$l704	<- NULL
mobile.l$l710	<- NULL
mobile.l$l713	<- NULL
mobile.l$l714	<- NULL
mobile.l$l721	<- NULL
mobile.l$l724	<- NULL
mobile.l$l730	<- NULL
mobile.l$l751	<- NULL
mobile.l$l756	<- NULL
mobile.l$l757	<- NULL
mobile.l$l759	<- NULL
mobile.l$l761	<- NULL
mobile.l$l773	<- NULL
mobile.l$l775	<- NULL
mobile.l$l777	<- NULL
mobile.l$l779	<- NULL
mobile.l$l780	<- NULL
mobile.l$l781	<- NULL
mobile.l$l782	<- NULL
mobile.l$l783	<- NULL
mobile.l$l786	<- NULL
mobile.l$l787	<- NULL
mobile.l$l959	<- NULL
mobile.l$l960	<- NULL
mobile.l$l61<-	NULL
mobile.l$l89<-	NULL
mobile.l$l97<-	NULL
mobile.l$l98<-	NULL
mobile.l$l99<-	NULL
mobile.l$l104<-	NULL
mobile.l$l134<-	NULL
mobile.l$l138<-	NULL
mobile.l$l139<-	NULL
mobile.l$l154<-	NULL
mobile.l$l156<-	NULL
mobile.l$l159<-	NULL
mobile.l$l166<-	NULL
mobile.l$l181<-	NULL
mobile.l$l183<-	NULL
mobile.l$l188<-	NULL
mobile.l$l189<-	NULL
mobile.l$l190<-	NULL
mobile.l$l191<-	NULL
mobile.l$l192<-	NULL
mobile.l$l193<-	NULL
mobile.l$l196<-	NULL
mobile.l$l198<-	NULL
mobile.l$l207<-	NULL
mobile.l$l211<-	NULL
mobile.l$l212<-	NULL
mobile.l$l213<-	NULL
mobile.l$l214<-	NULL
mobile.l$l217<-	NULL
mobile.l$l218<-	NULL
mobile.l$l220<-	NULL
mobile.l$l221<-	NULL
mobile.l$l222<-	NULL
mobile.l$l224<-	NULL
mobile.l$l226<-	NULL
mobile.l$l227<-	NULL
mobile.l$l228<-	NULL
mobile.l$l230<-	NULL
mobile.l$l231<-	NULL
mobile.l$l232<-	NULL
mobile.l$l233<-	NULL
mobile.l$l234<-	NULL
mobile.l$l237<-	NULL
mobile.l$l238<-	NULL
mobile.l$l246<-	NULL
mobile.l$l247<-	NULL
mobile.l$l255<-	NULL
mobile.l$l256<-	NULL
mobile.l$l258<-	NULL
mobile.l$l259<-	NULL
mobile.l$l260<-	NULL
mobile.l$l261<-	NULL
mobile.l$l266<-	NULL
mobile.l$l267<-	NULL
mobile.l$l273<-	NULL
mobile.l$l274<-	NULL
mobile.l$l316<-	NULL
mobile.l$l194<-	NULL
mobile.l$l195<-	NULL
mobile.l$l184<-	NULL
mobile.l$l158<-	NULL
mobile.l$l185<-	NULL
mobile.l$l186<-	NULL
mobile.l$l103<-	NULL
mobile.l$l100<-	NULL
mobile.l$l65	<-NULL
mobile.l$l169<-	NULL
mobile.l$l170<-	NULL


#Merge counts of labels ids and remove those label_ids
mobile3 <- merge(mobile.l, by_device_label_id_count, by = 'User_ID')
summary(mobile3)
dim(mobile3)
setkey(mobile3, label_id)

mobile_bigs <- read.csv("~/label_categories.csv/label_categories_big3.csv", head=T) # load the aggregated data file
head(mobile_bigs)
mobile_bigs$label_id <- as.character(mobile_bigs$label_id)
mobile_bigs<- data.table(mobile_bigs)
summary(mobile_bigs)
setkey(mobile_bigs, label_id)
mobile4 <- merge(mobile3, mobile_bigs, by = 'label_id', allow.cartesian=TRUE)
head(mobile4)
names(mobile4)
mobile4<-mobile4[,c(2,1,275,3:273)]
head(mobile4)
names(mobile4)
summary(mobile4$big_category)
mobile4$Aviation	<- ifelse(mobile4$big_category == 'Air and Aviation', 1,0)
mobile4$Arts_Culture	<- ifelse(mobile4$big_category == 'Arts and Culture', 1,0)
mobile4$Auto	<- ifelse(mobile4$big_category == 'Auto', 1,0)
mobile4$Banking_Finance	<- ifelse(mobile4$big_category == 'Banking and Finance', 1,0)
mobile4$Business	<- ifelse(mobile4$big_category == 'Business', 1,0)
mobile4$Education	<- ifelse(mobile4$big_category == 'Education', 1,0)
mobile4$Family_Children	<- ifelse(mobile4$big_category == 'Family and Children', 1,0)
mobile4$Entertainment	<- ifelse(mobile4$big_category == 'Entertainment', 1,0)
mobile4$Fashion	<- ifelse(mobile4$big_category == 'Fashion', 1,0)
mobile4$Home	<- ifelse(mobile4$big_category == 'Home and Housewares', 1,0)
mobile4$Household_chores	<- ifelse(mobile4$big_category == 'Household Chores', 1,0)
mobile4$Food	<- ifelse(mobile4$big_category == 'Food', 1,0)
mobile4$Gambling	<- ifelse(mobile4$big_category == 'Gambling', 1,0)
mobile4$Medical_health	<- ifelse(mobile4$big_category == 'Medical and Health', 1,0)
mobile4$P2P	<- ifelse(mobile4$big_category == 'P2P', 1,0)
mobile4$Beauty	<- ifelse(mobile4$big_category == 'Personal Care and Beauty', 1,0)
mobile4$Reading_puzzles	<- ifelse(mobile4$big_category == 'Reading and Puzzles', 1,0)
mobile4$Real_estate	<- ifelse(mobile4$big_category == 'Real Estate and Insurance', 1,0)
mobile4$Risk	<- ifelse(mobile4$big_category == 'Risk', 1,0)
mobile4$Shopping	<- ifelse(mobile4$big_category == 'Shopping', 1,0)
mobile4$Social	<- ifelse(mobile4$big_category == 'Social', 1,0)
mobile4$Violence	<- ifelse(mobile4$big_category == 'Violence', 1,0)
mobile4$Sports	<- ifelse(mobile4$big_category == 'Sports', 1,0)
mobile4$Technology	<- ifelse(mobile4$big_category == 'Technology', 1,0)
mobile4$Travel	<- ifelse(mobile4$big_category == 'Travel', 1,0)
mobile4$Game	<- ifelse(mobile4$big_category == 'Game', 1,0)
                                   
by_device_bigcat_count <- mobile4 %>%
  group_by(
    User_ID
  ) %>%
  dplyr::summarize(
    Aviation=sum(Aviation, na.rm=TRUE),
    Arts_Culture=sum(Arts_Culture, na.rm=TRUE),
    Auto=sum(Auto, na.rm=TRUE),
    Banking_Finance=sum(Banking_Finance, na.rm=TRUE),
    Business=sum(Business, na.rm=TRUE),
    Education=sum(Education, na.rm=TRUE),
    Family_Children=sum(Family_Children, na.rm=TRUE),
    Entertainment=sum(Entertainment, na.rm=TRUE),
    Fashion=sum(Fashion, na.rm=TRUE),
    Home=sum(Home, na.rm=TRUE),
    Household_chores=sum(Household_chores, na.rm=TRUE),
    Food=sum(Food, na.rm=TRUE),
    Gambling=sum(Gambling, na.rm=TRUE),
    Medical_health=sum(Medical_health, na.rm=TRUE),
    P2P=sum(P2P, na.rm=TRUE),
    Beauty=sum(Beauty, na.rm=TRUE),
    Reading_puzzles=sum(Reading_puzzles, na.rm=TRUE),
    Real_estate=sum(Real_estate, na.rm=TRUE),
    Risk=sum(Risk, na.rm=TRUE),
    Shopping=sum(Shopping, na.rm=TRUE),
    Social=sum(Social, na.rm=TRUE),
    Violence=sum(Violence, na.rm=TRUE),
    Sports=sum(Sports, na.rm=TRUE),
    Technology=sum(Technology, na.rm=TRUE),
    Travel=sum(Travel, na.rm=TRUE),
    Game=sum(Game, na.rm=TRUE)
        )

mobile4$Aviation	<- NULL
mobile4$Arts_Culture<- NULL
mobile4$Auto <- NULL
mobile4$Banking_Finance	<- NULL
mobile4$Business	<- NULL
mobile4$Education	<- NULL
mobile4$Family_Children	<- NULL
mobile4$Entertainment	<- NULL
mobile4$Fashion	<- NULL
mobile4$Home	<- NULL
mobile4$Household_chores	<- NULL
mobile4$Food	<- NULL
mobile4$Gambling	<- NULL
mobile4$Medical_health	<- NULL
mobile4$P2P	<- NULL
mobile4$Beauty	<- NULL
mobile4$Reading_puzzles	<- NULL
mobile4$Real_estate	<- NULL
mobile4$Risk	<- NULL
mobile4$Shopping	<- NULL
mobile4$Social	<- NULL
mobile4$Violence	<- NULL
mobile4$Sports	<- NULL
mobile4$Technology	<- NULL
mobile4$Travel	<- NULL
mobile4$Game	<- NULL


#Merge counts of labels ids and remove those label_ids
mobile.rec <- merge(mobile4, by_device_bigcat_count, by = 'User_ID')
summary(mobile.rec)
names(mobile.rec)
dim(mobile.rec)
set.seed(1306) # set random number generator seed to enable
# repeatability of results
n <- dim(mobile.rec)[1]
names(mobile.rec)
mobile.rec<- mobile.rec[,c(1,9:300)]
names(mobile.rec)
dim(mobile.rec)
head(mobile.rec)
setkey(mobile.rec, NULL)
mobile.rec<- unique(mobile.rec)
dim(mobile.rec)
str(mobile.rec)


################################### Scale and Center Data ###########################################

recommender.train  <- mobile.rec
recommender.train$app2359<-ifelse(recommender.train$app2359 == 0, NULL, recommender.train$app2359)
recommender.train$app99227<-ifelse(recommender.train$app99227 == 0, NULL, recommender.train$app99227)
recommender.train$app25125<-ifelse(recommender.train$app25125 == 0, NULL, recommender.train$app25125)
recommender.train$app2574<-ifelse(recommender.train$app2574 == 0, NULL, recommender.train$app2574)
recommender.train$app2671<-ifelse(recommender.train$app2671 == 0, NULL, recommender.train$app2671)
recommender.train$app46293<-ifelse(recommender.train$app46293 == 0, NULL, recommender.train$app46293)
recommender.train$app5899<-ifelse(recommender.train$app5899 == 0, NULL, recommender.train$app5899)
recommender.train$app6384<-ifelse(recommender.train$app6384 == 0, NULL, recommender.train$app6384)
recommender.train$app6658<-ifelse(recommender.train$app6658 == 0, NULL, recommender.train$app6658)
recommender.train$app6883<-ifelse(recommender.train$app6883 == 0, NULL, recommender.train$app6883)
recommender.train$app7392<-ifelse(recommender.train$app7392 == 0, NULL, recommender.train$app7392)
recommender.train$app74955<-ifelse(recommender.train$app74955 == 0, NULL, recommender.train$app74955)
recommender.train$app78005<-ifelse(recommender.train$app78005 == 0, NULL, recommender.train$app78005)
recommender.train$app8828<-ifelse(recommender.train$app8828 == 0, NULL, recommender.train$app8828)
recommender.train$app9590<-ifelse(recommender.train$app9590 == 0, NULL, recommender.train$app9590)
recommender.train$app14790<-ifelse(recommender.train$app14790 == 0, NULL, recommender.train$app14790)
recommender.train$app9590<ifelse(recommender.train$app9590 == 0, NULL, recommender.train$app9590)
recommender.train$l217<-ifelse(recommender.train$l217 == 0, NULL, recommender.train$l217)

data.train <- recommender.train
names(data.train)
x.train <- data.train[,c(2:276)]

str(x.train)
names(data.train)
r.train <- data.train[,1] #gender
head(data.train)
n.train.r <- length(r.train) # 487862


x.train.mean <- apply(x.train, 2, mean)
x.train.mean
x.train.sd <- apply(x.train, 2, sd)

x.train.std <- t((t(x.train)-x.train.mean)/x.train.sd) # standardize to have zero mean and unit sd
summary(x.train.std)
apply(x.train.std, 2, mean) # check zero mean
apply(x.train.std, 2, sd) # check unit sd

data.train.std.a <- data.frame(User_ID=r.train, x.train.std) # to add User id back after center and scale
#recommender.train  <- cbind(data.train$User_ID,  data.train.std.a)
names(data.train.std.a)
dim(data.train.std.a)
summary(data.train.std.a)
recommender.train<- data.train.std.a
names(recommender.train)


recommender.train<- recommender.train[,c(1:276)]
head(recommender.train)


################################### Reformat for Mahout ###########################################

names(recommender.train)
mahout.train <- recommender.train
names(mahout.train)
setkey(mahout.train, NULL)
mahout.train<- unique(mahout.train)

colnames(mahout.train)[2] <-   '85699'  #  "app85699'  #  "
colnames(mahout.train)[3] <-   '12353'  #  "app12353'  #  "
colnames(mahout.train)[4] <-   '13972'  #  "app13972'  #  "
colnames(mahout.train)[5] <-   '42167'  #  "app2167'  #  "
colnames(mahout.train)[6] <-   '42087'  #  "app2087'  #  "
colnames(mahout.train)[7] <-   '42612'  #  "app2612'  #  "
colnames(mahout.train)[8] <-   '42665'  #  "app2665'  #  "
colnames(mahout.train)[9] <-   '42858'  #  "app2858'  #  "
colnames(mahout.train)[10] <-   '39155'  #  "app39155'  #  "
colnames(mahout.train)[11] <-   '44238'  #  "app4238'  #  "
colnames(mahout.train)[12] <-   '44338'  #  "app4338'  #  "
colnames(mahout.train)[13] <-   '45571'  #  "app45571'  #  "
colnames(mahout.train)[14] <-   '44701'  #  "app4701'  #  "
colnames(mahout.train)[15] <-   '53300'  #  "app53300'  #  "
colnames(mahout.train)[16] <-   '45457'  #  "app5457'  #  "
colnames(mahout.train)[17] <-   '45642'  #  "app5642'  #  "
colnames(mahout.train)[18] <-   '45961'  #  "app5961'  #  "
colnames(mahout.train)[19] <-   '46122'  #  "app6122'  #  "
colnames(mahout.train)[20] <-   '46364'  #  "app6364'  #  "
colnames(mahout.train)[21] <-   '46381'  #  "app6381'  #  "
colnames(mahout.train)[22] <-   '71252'  #  "app71252'  #  "
colnames(mahout.train)[23] <-   '47177'  #  "app7177'  #  "
colnames(mahout.train)[24] <-   '47258'  #  "app7258'  #  "
colnames(mahout.train)[25] <-   '47347'  #  "app7347'  #  "
colnames(mahout.train)[26] <-   '47372'  #  "app7372'  #  "
colnames(mahout.train)[27] <-   '477482'  #  "app77482'  #  "
colnames(mahout.train)[28] <-   '47991'  #  "app7991'  #  "
colnames(mahout.train)[29] <-   '85647'  #  "app85647'  #  "
colnames(mahout.train)[30] <-   '88123'  #  "app88123'  #  "
colnames(mahout.train)[31] <-   '49611'  #  "app9611'  #  "
colnames(mahout.train)[32] <-   '496897'  #  "app96897'  #  "
colnames(mahout.train)[33] <-   '300005'  #  "a00005'  #  "
colnames(mahout.train)[34] <-   '300001'  #  "a00001'  #  "
colnames(mahout.train)[35] <-   '300002'  #  "a00002'  #  "
colnames(mahout.train)[36] <-   '300003'  #  "a00003'  #  "
colnames(mahout.train)[37] <-   '300007'  #  "a00007'  #  "
colnames(mahout.train)[38] <-   '300010'  #  "a00010'  #  "
colnames(mahout.train)[39] <-   '300012'  #  "a00012'  #  "
colnames(mahout.train)[40] <-   '300013'  #  "a00013'  #  "
colnames(mahout.train)[41] <-   '300014'  #  "a00014'  #  "
colnames(mahout.train)[42] <-   '300015'  #  "a00015'  #  "
colnames(mahout.train)[43] <-   '300016'  #  "a00016'  #  "
colnames(mahout.train)[44] <-   '300017'  #  "a00017'  #  "
colnames(mahout.train)[45] <-   '300018'  #  "a00018'  #  "
colnames(mahout.train)[46] <-   '300020'  #  "a00020'  #  "
colnames(mahout.train)[47] <-   '300021'  #  "a00021'  #  "
colnames(mahout.train)[48] <-   '300025'  #  "a00025'  #  "
colnames(mahout.train)[49] <-   '300035'  #  "a00035'  #  "
colnames(mahout.train)[50] <-   '300036'  #  "a00036'  #  "
colnames(mahout.train)[51] <-   '300037'  #  "a00037'  #  "
colnames(mahout.train)[52] <-   '300039'  #  "a00039'  #  "
colnames(mahout.train)[53] <-   '300040'  #  "a00040'  #  "
colnames(mahout.train)[54] <-   '300044'  #  "a00044'  #  "
colnames(mahout.train)[55] <-   '300046'  #  "a00046'  #  "
colnames(mahout.train)[56] <-   '300051'  #  "a00051'  #  "
colnames(mahout.train)[57] <-   '300052'  #  "a00052'  #  "
colnames(mahout.train)[58] <-   '300060'  #  "a00060'  #  "
colnames(mahout.train)[59] <-   '300061'  #  "a00061'  #  "
colnames(mahout.train)[60] <-   '300066'  #  "a00066'  #  "
colnames(mahout.train)[61] <-   '300069'  #  "a00069'  #  "
colnames(mahout.train)[62] <-   '300071'  #  "a00071'  #  "
colnames(mahout.train)[63] <-   '300077'  #  "a00077'  #  "
colnames(mahout.train)[64] <-   '300090'  #  "a00090'  #  "
colnames(mahout.train)[65] <-   '300093'  #  "a00093'  #  "
colnames(mahout.train)[66] <-   '300103'  #  "a00103'  #  "
colnames(mahout.train)[67] <-   '300104'  #  "a00104'  #  "
colnames(mahout.train)[68] <-   '300106'  #  "a00106'  #  "
colnames(mahout.train)[69] <-   '300108'  #  "a00108'  #  "
colnames(mahout.train)[70] <-   '300112'  #  "a00112'  #  "
colnames(mahout.train)[71] <-   '300117'  #  "a00117'  #  "
colnames(mahout.train)[72] <-   '300123'  #  "a00123'  #  "
colnames(mahout.train)[73] <-   '300127'  #  "a00127'  #  "
colnames(mahout.train)[74] <-   '300128'  #  "a00128'  #  "
colnames(mahout.train)[75] <-   '300129'  #  "a00129'  #  "
colnames(mahout.train)[76] <-   '300132'  #  "a00132'  #  "
colnames(mahout.train)[77] <-   '300135'  #  "a00135'  #  "
colnames(mahout.train)[78] <-   '300138'  #  "a00138'  #  "
colnames(mahout.train)[79] <-   '300141'  #  "a00141'  #  "
colnames(mahout.train)[80] <-   '300144'  #  "a00144'  #  "
colnames(mahout.train)[81] <-   '300151'  #  "a00151'  #  "
colnames(mahout.train)[82] <-   '300153'  #  "a00153'  #  "
colnames(mahout.train)[83] <-   '300158'  #  "a00158'  #  "
colnames(mahout.train)[84] <-   '300159'  #  "a00159'  #  "
colnames(mahout.train)[85] <-   '300160'  #  "a00160'  #  "
colnames(mahout.train)[86] <-   '300162'  #  "a00162'  #  "
colnames(mahout.train)[87] <-   '300166'  #  "a00166'  #  "
colnames(mahout.train)[88] <-   '300167'  #  "a00167'  #  "
colnames(mahout.train)[89] <-   '300169'  #  "a00169'  #  "
colnames(mahout.train)[90] <-   '300170'  #  "a00170'  #  "
colnames(mahout.train)[91] <-   '300173'  #  "a00173'  #  "
colnames(mahout.train)[92] <-   '300178'  #  "a00178'  #  "
colnames(mahout.train)[93] <-   '300182'  #  "a00182'  #  "
colnames(mahout.train)[94] <-   '300186'  #  "a00186'  #  "
colnames(mahout.train)[95] <-   '300201'  #  "a00201'  #  "
colnames(mahout.train)[96] <-   '300205'  #  "a00205'  #  "
colnames(mahout.train)[97] <-   '300206'  #  "a00206'  #  "
colnames(mahout.train)[98] <-   '300210'  #  "a00210'  #  "
colnames(mahout.train)[99] <-   '300215'  #  "a00215'  #  "
colnames(mahout.train)[100] <-   '300221'  #  "a00221'  #  "
colnames(mahout.train)[101] <-   '300262'  #  "a00262'  #  "
colnames(mahout.train)[102] <-   '300289'  #  "a00289'  #  "
colnames(mahout.train)[103] <-   '300301'  #  "a00301'  #  "
colnames(mahout.train)[104] <-   '300305'  #  "a00305'  #  "
colnames(mahout.train)[105] <-   '300309'  #  "a00309'  #  "
colnames(mahout.train)[106] <-   '300341'  #  "a00341'  #  "
colnames(mahout.train)[107] <-   '300342'  #  "a00342'  #  "
colnames(mahout.train)[108] <-   '300343'  #  "a00343'  #  "
colnames(mahout.train)[109] <-   '300357'  #  "a00357'  #  "
colnames(mahout.train)[110] <-   '300363'  #  "a00363'  #  "
colnames(mahout.train)[111] <-   '300371'  #  "a00371'  #  "
colnames(mahout.train)[112] <-   '300381'  #  "a00381'  #  "
colnames(mahout.train)[113] <-   '300393'  #  "a00393'  #  "
colnames(mahout.train)[114] <-   '300403'  #  "a00403'  #  "
colnames(mahout.train)[115] <-   '300477'  #  "a00477'  #  "
colnames(mahout.train)[116] <-   '300518'  #  "a00518'  #  "
colnames(mahout.train)[117] <-   '300525'  #  "a00525'  #  "
colnames(mahout.train)[118] <-   '300552'  #  "a00552'  #  "
colnames(mahout.train)[119] <-   '300608'  #  "a00608'  #  "
colnames(mahout.train)[120] <-   '300610'  #  "a00610'  #  "
colnames(mahout.train)[121] <-   '300815'  #  "a00815'  #  "
colnames(mahout.train)[122] <-   '300816'  #  "a00816'  #  "
colnames(mahout.train)[123] <-   '300921'  #  "a00921'  #  "
colnames(mahout.train)[124] <-   '301077'  #  "a01077'  #  "
colnames(mahout.train)[125] <-   '301919'  #  "a01919'  #  "
colnames(mahout.train)[126] <-   '302410'  #  "a02410'  #  "
colnames(mahout.train)[127] <-   '954955'  #  "l954.955'  #  "
colnames(mahout.train)[128] <-   '918927'  #  "l918.927'  #  "
colnames(mahout.train)[129] <-   '959960'  #  "l959.960'  #  "
colnames(mahout.train)[130] <-   '731732'  #  "l731.732'  #  "
colnames(mahout.train)[131] <-   '222279'  #  "l222.279'  #  "
colnames(mahout.train)[132] <-   '255747'  #  "l255.747'  #  "
colnames(mahout.train)[133] <-   '818820'  #  "l818.820'  #  "
colnames(mahout.train)[134] <-   '941942'  #  "l941.942'  #  "
colnames(mahout.train)[135] <-   '939940'  #  "l939.940'  #  "
colnames(mahout.train)[136] <-   '937933'  #  "l937.933'  #  "
colnames(mahout.train)[137] <-   '552555'  #  "l552.555'  #  "
colnames(mahout.train)[138] <-   '204205'  #  "l204.205'  #  "
colnames(mahout.train)[139] <-   '737738'  #  "l737.738'  #  "
colnames(mahout.train)[140] <-   '14212'  #  "l14.21.2'  #  "
colnames(mahout.train)[141] <-   '55546'  #  "l46'  #  "
colnames(mahout.train)[142] <-   '51003'  #  "l1003'  #  "
colnames(mahout.train)[143] <-   '51007'  #  "l1007'  #  "
colnames(mahout.train)[144] <-   '51008'  #  "l1008'  #  "
colnames(mahout.train)[145] <-   '51012'  #  "l1012'  #  "
colnames(mahout.train)[146] <-   '51015'  #  "l1015'  #  "
colnames(mahout.train)[147] <-   '55128'  #  "l128'  #  "
colnames(mahout.train)[148] <-   '55172'  #  "l172'  #  "
colnames(mahout.train)[149] <-   '55179'  #  "l179'  #  "
colnames(mahout.train)[150] <-   '55251'  #  "l251'  #  "
colnames(mahout.train)[151] <-   '55252'  #  "l252'  #  "
colnames(mahout.train)[152] <-   '55254'  #  "l254'  #  "
colnames(mahout.train)[153] <-   '55262'  #  "l262'  #  "
colnames(mahout.train)[154] <-   '55263'  #  "l263'  #  "
colnames(mahout.train)[155] <-   '55302'  #  "l302'  #  "
colnames(mahout.train)[156] <-   '55303'  #  "l303'  #  "
colnames(mahout.train)[157] <-   '55306'  #  "l306'  #  "
colnames(mahout.train)[158] <-   '55405'  #  "l405'  #  "
colnames(mahout.train)[159] <-   '55548'  #  "l548'  #  "
colnames(mahout.train)[160] <-   '55549'  #  "l549'  #  "
colnames(mahout.train)[161] <-   '55564'  #  "l564'  #  "
colnames(mahout.train)[162] <-   '55691'  #  "l691'  #  "
colnames(mahout.train)[163] <-   '55704'  #  "l704'  #  "
colnames(mahout.train)[164] <-   '55710'  #  "l710'  #  "
colnames(mahout.train)[165] <-   '55713'  #  "l713'  #  "
colnames(mahout.train)[166] <-   '55714'  #  "l714'  #  "
colnames(mahout.train)[167] <-   '55721'  #  "l721'  #  "
colnames(mahout.train)[168] <-   '55724'  #  "l724'  #  "
colnames(mahout.train)[169] <-   '55730'  #  "l730'  #  "
colnames(mahout.train)[170] <-   '55751'  #  "l751'  #  "
colnames(mahout.train)[171] <-   '55756'  #  "l756'  #  "
colnames(mahout.train)[172] <-   '55757'  #  "l757'  #  "
colnames(mahout.train)[173] <-   '55759'  #  "l759'  #  "
colnames(mahout.train)[174] <-   '55761'  #  "l761'  #  "
colnames(mahout.train)[175] <-   '55773'  #  "l773'  #  "
colnames(mahout.train)[176] <-   '55775'  #  "l775'  #  "
colnames(mahout.train)[177] <-   '55777'  #  "l777'  #  "
colnames(mahout.train)[178] <-   '55779'  #  "l779'  #  "
colnames(mahout.train)[179] <-   '55780'  #  "l780'  #  "
colnames(mahout.train)[180] <-   '55781'  #  "l781'  #  "
colnames(mahout.train)[181] <-   '55782'  #  "l782'  #  "
colnames(mahout.train)[182] <-   '55783'  #  "l783'  #  "
colnames(mahout.train)[183] <-   '55786'  #  "l786'  #  "
colnames(mahout.train)[184] <-   '55787'  #  "l787'  #  "
colnames(mahout.train)[185] <-   '55959'  #  "l959'  #  "
colnames(mahout.train)[186] <-   '55960'  #  "l960'  #  "
colnames(mahout.train)[187] <-   '55061'  #  "l61'  #  "
colnames(mahout.train)[188] <-   '55089'  #  "l89'  #  "
colnames(mahout.train)[189] <-   '55097'  #  "l97'  #  "
colnames(mahout.train)[190] <-   '55098'  #  "l98'  #  "
colnames(mahout.train)[191] <-   '55099'  #  "l99'  #  "
colnames(mahout.train)[192] <-   '55104'  #  "l104'  #  "
colnames(mahout.train)[193] <-   '55134'  #  "l134'  #  "
colnames(mahout.train)[194] <-   '55138'  #  "l138'  #  "
colnames(mahout.train)[195] <-   '55139'  #  "l139'  #  "
colnames(mahout.train)[196] <-   '55154'  #  "l154'  #  "
colnames(mahout.train)[197] <-   '55156'  #  "l156'  #  "
colnames(mahout.train)[198] <-   '55159'  #  "l159'  #  "
colnames(mahout.train)[199] <-   '55166'  #  "l166'  #  "
colnames(mahout.train)[200] <-   '55181'  #  "l181'  #  "
colnames(mahout.train)[201] <-   '55183'  #  "l183'  #  "
colnames(mahout.train)[202] <-   '55188'  #  "l188'  #  "
colnames(mahout.train)[203] <-   '55189'  #  "l189'  #  "
colnames(mahout.train)[204] <-   '55190'  #  "l190'  #  "
colnames(mahout.train)[205] <-   '55191'  #  "l191'  #  "
colnames(mahout.train)[206] <-   '55192'  #  "l192'  #  "
colnames(mahout.train)[207] <-   '55193'  #  "l193'  #  "
colnames(mahout.train)[208] <-   '55198'  #  "l198'  #  "
colnames(mahout.train)[209] <-   '55207'  #  "l207'  #  "
colnames(mahout.train)[210] <-   '55211'  #  "l211'  #  "
colnames(mahout.train)[211] <-   '55212'  #  "l212'  #  "
colnames(mahout.train)[212] <-   '55213'  #  "l213'  #  "
colnames(mahout.train)[213] <-   '55214'  #  "l214'  #  "
colnames(mahout.train)[214] <-   '55220'  #  "l220'  #  "
colnames(mahout.train)[215] <-   '55221'  #  "l221'  #  "
colnames(mahout.train)[216] <-   '55222'  #  "l222'  #  "
colnames(mahout.train)[217] <-   '55224'  #  "l224'  #  "
colnames(mahout.train)[218] <-   '55226'  #  "l226'  #  "
colnames(mahout.train)[219] <-   '55227'  #  "l227'  #  "
colnames(mahout.train)[220] <-   '55230'  #  "l230'  #  "
colnames(mahout.train)[221] <-   '55231'  #  "l231'  #  "
colnames(mahout.train)[222] <-   '55232'  #  "l232'  #  "
colnames(mahout.train)[223] <-   '55233'  #  "l233'  #  "
colnames(mahout.train)[224] <-   '55234'  #  "l234'  #  "
colnames(mahout.train)[225] <-   '55237'  #  "l237'  #  "
colnames(mahout.train)[226] <-   '55238'  #  "l238'  #  "
colnames(mahout.train)[227] <-   '55246'  #  "l246'  #  "
colnames(mahout.train)[228] <-   '55247'  #  "l247'  #  "
colnames(mahout.train)[229] <-   '55255'  #  "l255'  #  "
colnames(mahout.train)[230] <-   '55256'  #  "l256'  #  "
colnames(mahout.train)[231] <-   '55258'  #  "l258'  #  "
colnames(mahout.train)[232] <-   '55259'  #  "l259'  #  "
colnames(mahout.train)[233] <-   '55260'  #  "l260'  #  "
colnames(mahout.train)[234] <-   '55261'  #  "l261'  #  "
colnames(mahout.train)[235] <-   '55266'  #  "l266'  #  "
colnames(mahout.train)[236] <-   '55267'  #  "l267'  #  "
colnames(mahout.train)[237] <-   '55273'  #  "l273'  #  "
colnames(mahout.train)[238] <-   '55274'  #  "l274'  #  "
colnames(mahout.train)[239] <-   '55316'  #  "l316'  #  "
colnames(mahout.train)[240] <-   '55194'  #  "l194'  #  "
colnames(mahout.train)[241] <-   '55195'  #  "l195'  #  "
colnames(mahout.train)[242] <-   '55184'  #  "l184'  #  "
colnames(mahout.train)[243] <-   '55158'  #  "l158'  #  "
colnames(mahout.train)[244] <-   '55185'  #  "l185'  #  "
colnames(mahout.train)[245] <-   '55186'  #  "l186'  #  "
colnames(mahout.train)[246] <-   '55103'  #  "l103'  #  "
colnames(mahout.train)[247] <-   '55100'  #  "l100'  #  "
colnames(mahout.train)[248] <-   '55065'  #  "l65'  #  "
colnames(mahout.train)[249] <-   '55169'  #  "l169'  #  "
colnames(mahout.train)[250] <-   '55170'  #  "l170'  #  "
colnames(mahout.train)[251] <-   '7701'  #  "Aviation'  #  "
colnames(mahout.train)[252] <-   '7702'  #  "Arts_Culture'  #  "
colnames(mahout.train)[253] <-   '7703'  #  "Auto'  #  "
colnames(mahout.train)[254] <-   '7704'  #  "Banking_Finance'  #  "
colnames(mahout.train)[255] <-   '7705'  #  "Business'  #  "
colnames(mahout.train)[256] <-   '7706'  #  "Education'  #  "
colnames(mahout.train)[257] <-   '7707'  #  "Family_Children'  #  "
colnames(mahout.train)[258] <-   '7708'  #  "Entertainment'  #  "
colnames(mahout.train)[259] <-   '7709'  #  "Fashion'  #  "
colnames(mahout.train)[260] <-   '7710'  #  "Home'  #  "
colnames(mahout.train)[261] <-   '7711'  #  "Household_chores'  #  "
colnames(mahout.train)[262] <-   '7712'  #  "Food'  #  "
colnames(mahout.train)[263] <-   '7713'  #  "Gambling'  #  "
colnames(mahout.train)[264] <-   '7714'  #  "Medical_health'  #  "
colnames(mahout.train)[265] <-   '7715'  #  "P2P'  #  "
colnames(mahout.train)[266] <-   '7716'  #  "Beauty'  #  "
colnames(mahout.train)[267] <-   '7717'  #  "Reading_puzzles'  #  "
colnames(mahout.train)[268] <-   '7718'  #  "Real_estate'  #  "
colnames(mahout.train)[269] <-   '7719'  #  "Risk'  #  "
colnames(mahout.train)[270] <-   '7720'  #  "Shopping'  #  "
colnames(mahout.train)[271] <-   '7721'  #  "Social'  #  "
colnames(mahout.train)[272] <-   '7722'  #  "Violence'  #  "
colnames(mahout.train)[273] <-   '7723'  #  "Sports'  #  "
colnames(mahout.train)[274] <-   '7724'  #  "Technology'  #  "
colnames(mahout.train)[275] <-   '7725'  #  "Travel'  #  "
colnames(mahout.train)[276] <-   '7726'  #  "Game'  #  "

names(mahout.train)
bad <- sapply(mahout.train, function(x) all(is.nan(x)))
mahout.train <- mahout.train[,!bad]
mahout.train <- melt(mahout.train, id.vars="User_ID")
mahout.train <- setkey(mahout2, NULL)
mahout.train <- unique(mahout2)
names(mahout.train)
dim(mahout.train)
summary(mahout.train)

my.sample <- sample(n, round(n/4)) # randomly sample 75% train, and 25% test
mahout.test <- mahout.train[my.sample,]
mahout.test<- unique(mahout.train)

names(mahout.test)
dim(mahout.test)
summary(mahout.test)
head(mahout.test, 500)
mahout.test <- melt(mahout.test, id.vars="User_ID")
mahout.test <- setkey(mahout.test, NULL)
mahout.test <- unique(mahout.test)

mahout2<-mahout2[complete.cases(mahout2),]


mahout.train2<-mahout2[-my.sample,]
mahout.train2<- unique(mahout.train)
head(mahout.test)
mahout.train2<-mahout.train[complete.cases(mahout.train),]
summary(mahout.train2)
mahout.test<-mahout.test[complete.cases(mahout.test),]
head(mahout.test)
summary(mahout.test)
write_csv(mahout.train2, "CJ.mahout_app_trainv2.2.25.17.csv")
write_csv(mahout.test, "CJ.mahout_app_testv2.2.25.17.csv")
