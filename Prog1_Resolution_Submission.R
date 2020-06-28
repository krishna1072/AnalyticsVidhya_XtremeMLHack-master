rm(list=ls());gc()
setwd('K:/RHack/Xtreme ML Hack/')

# loading libraries
library(readr)
library(dplyr)
library(tidyr)
library(magrittr)
library(lubridate)
library(dummies)
library(htmltab)
library(zoo)
library(xgboost)

# reading files
Resolution_Pre_2017 <- read_csv('./Inputs/Train_BG1IL20/Train/Resolution_Pre_2017.csv')
 
# Create holidays data file from web
Holidays <- NULL
for(y in 2010:2017){
  url <- paste0("https://www.timeanddate.com/holidays/spain/",y)
  pulledholidays <- htmltab(doc = url) %>% filter(row_number() !=1) %>%
    mutate(HolidayDate = as.Date(paste0(as.character(y),'-',gsub('[0-9 ]','',Date),'-',gsub('[a-zA-Z ]','',Date)),'%Y-%b-%d')) %>%
    rename(Holiday_Type = `Holiday Type`) %>%
    select(HolidayDate,Holiday_Type) %>%
    mutate(HolidayFlag=1) %>%
    tbl_df()
  
  Holidays <- bind_rows(Holidays,pulledholidays)
  
}

HolidaysData <- Holidays %>% mutate(Holiday_Type = gsub('[ \\/]','_',Holiday_Type)) %>%
  rename(Date=HolidayDate) %>%
  data.frame() %>%
  dummy.data.frame(sep='_')  %>%
  group_by(Date) %>%
  summarise_all(funs(max(.)))

# Checking Date ranges from each files
range(Resolution_Pre_2017$Date)

# Normalise column names
Resolution_Pre_2017 <- Resolution_Pre_2017 %>% rename_(.dots=setNames(names(.), gsub("\\.", "_", names(.))))

Category_types <- unique(Resolution_Pre_2017$Category)
Subject_types <- unique(Resolution_Pre_2017$Subject)

CatSublist <- apply(expand.grid(Category_types=Category_types,Subject_types=Subject_types),1,function(x) paste0(x[1],'#',x[2]))

# Pre-processing data
# Summarizing at Date , Category & Subject
Pre1 <- Resolution_Pre_2017 %>% 
  group_by( Date,Category,Subject) %>% 
  summarise(Resolution = sum(Resolution)) %>% 
  arrange(Date,Category,Subject)  %>% 
  ungroup()

# Make sure the data is available for each date in date range
DateDataClean <- function(type,Indata){
  DateData <- data.frame(Date = as.Date(as.Date('2010-01-01'):as.Date('2017-03-15'),origin='1970-01-01'))
  Indata <- Indata %>% filter(paste0(Category,'#',Subject) == type) %>%
    mutate(Resolution = as.numeric(Resolution)) %>%
    right_join(DateData,by='Date') %>%
    mutate(Cat = type,Resolution = as.numeric(if_else(is.na(Resolution),0,Resolution))) %>%
    select(-Category,-Subject)
  return(Indata)
}

Pre1 <- bind_rows(lapply(CatSublist,function(x) DateDataClean(type = x,Indata=Pre1)))
Pre1 <- Pre1 %>% separate(Cat, c("Category", "Subject"),remove=T,sep = "#")

Pre1 <- Pre1 %>%
        left_join(HolidaysData,by='Date') %>%
        mutate_at(vars(Holiday_Type_Clock_change_Daylight_Saving_Time:HolidayFlag),funs(replace(.,is.na(.),0))) %>%
        select(Date,Resolution,Category,Subject,Holiday_Type_National_holiday)

#Create Lag Variables 
Pre1_LagVars <- Pre1 %>% 
  mutate(CatSublist = paste0(Category,'#',Subject)) %>%
  select(CatSublist,Date,Resolution) %>%
  mutate(DayOfTheWeek=as.factor(wday(Date))) %>% 
  arrange(CatSublist,Date) %>% 
  group_by(CatSublist) %>% 
  mutate(PerYear_1DayChg = (Resolution-lag(Resolution))/lag(Resolution)) %>% 
  mutate(Per2Year_Chg = (Resolution-lag(Resolution,n=365))/lag(Resolution,n=365)) %>% 
  mutate(Prev1Year_7_lag = rollapply(data=Resolution, width = 7, FUN = median, align = "right", fill = NA, na.rm = T)) %>% 
  mutate(Prev1Year_7_lead = rollapply(data=Resolution, width = 7, FUN = median, align = "left", fill = NA, na.rm = T)) %>% 
  mutate(Prev1Year_7_Center = rollapply(data=Resolution, width = 14, FUN = median, align = c("center"), fill = NA, na.rm = T)) %>% 
  group_by(CatSublist,DayOfTheWeek) %>% 
  arrange(CatSublist,DayOfTheWeek) %>% 
  mutate(SameWeekDayLas6MResolution_Median = rollapply(data=as.numeric(ifelse(Resolution!=0,Resolution,NA)), width = 26, FUN = median, align = "right", fill = NA, na.rm = T)) %>%
  ungroup() %>%
  select(CatSublist,Date,Resolution,Prev1Year_7_lag,Prev1Year_7_lead,Prev1Year_7_Center,PerYear_1DayChg,Per2Year_Chg,SameWeekDayLas6MResolution_Median) %>%
  arrange(CatSublist,Date)

Pre1 <- Pre1 %>% 
  mutate(CatSublist = paste0(Category,'#',Subject)) %>%
  filter(Date >= as.Date('2011-01-01')) %>% 
  mutate(Date1 = Date - 365) %>%
  left_join(Pre1_LagVars %>% rename(Date1 = Date),by=c('Date1','CatSublist')) %>%
  rename(Resolution = Resolution.x,Resolution_365_Lag =Resolution.y) %>%
  select(-Date1,-CatSublist) %>%
  mutate_at(vars(Resolution:SameWeekDayLas6MResolution_Median),funs(replace(.,is.infinite(.) | is.nan(.),NA)))

Pre2 <- Pre1 %>%  
  rename(START_DATE = Date) %>% 
  separate(START_DATE, c("Year",'Month', "Date"),remove=F) %>%
  mutate(Month=as.factor(Month),Year=as.numeric(Year),Date=as.factor(Date),DayOfTheMonth=as.factor(day(START_DATE)),DayOfTheWeek=as.factor(wday(START_DATE))) %>%
  as.data.frame() %>% 
  dummy.data.frame(sep='_') %>% 
  tbl_df() 

Pre2 <- Pre1 %>% select(Category,Subject) %>%
  bind_cols(Pre2) %>%
  select(Category,Subject,START_DATE,everything())

Pre2 %T>% {
  #Train dataset until dec 31 2016
  filter(.,START_DATE <= as.Date('2016-12-31')) %>% 
    select(-START_DATE,-Category,-Subject) ->> Train
  filter(.,START_DATE <= as.Date('2016-12-31')) %>% 
    select(START_DATE,Category,Subject) ->> TrainIDs
  #Test dataset from Jan 1 2017
  filter(.,START_DATE >= as.Date('2017-01-01')) %>% 
    select(-START_DATE,-Category,-Subject) ->> Predictions
  filter(.,START_DATE >= as.Date('2017-01-01')) %>% 
    select(START_DATE,Category,Subject) ->> PredictionsIDs
  
}

rm(Resolution_Pre_2017,Holidays,HolidaysData,Pre1,Pre1_LagVars,Pre2,pulledholidays);gc()

#Build Model
dtrain <- xgb.DMatrix(data = Train %>% select(-Resolution) %>% as.matrix(),label=Train %>%  select(Resolution) %>% as.matrix(),missing=NA)
dpred <- xgb.DMatrix(data = Predictions %>% select(-Resolution) %>% as.matrix(),label=Predictions %>%  select(Resolution) %>% as.matrix(),missing=NA)

param <- list(booster = 'gbtree', # gbtree(default)/gblinear
              nthread = 8,
              eta = 0.1,
              max_depth = 5,
              colsample_bytree=0.8,
              min_child_weight=5,
              subsample = 0.8,
              objective = "reg:linear",
              seed = 2017,
              eval_metric = "rmse")

fit_cv <- xgb.cv(params=param,data=dtrain,nrounds=100000,nfold=3,early_stopping_rounds=3,verbose=1)
ntree <- fit_cv$best_iteration
fit_xgb <- xgb.train(params=param,data=dtrain,nrounds=ntree,verbose=1)

# Identifying Important features( Top 25)
importance_matrix <- xgb.importance(feature_names=colnames(dtrain),model = fit_xgb)
print(importance_matrix[1:25])
xgb.plot.importance(importance_matrix[1:25])

#Generating a submission file                         
PredCategoryOrder <- c('Tecnical Claim','Consultation','Request','Commercial Claim','Non Compliance')

PredSubjectOrder <-c('Escape','Technical management','Others','Paving','Water is missing','Customer Data Modification','Modifications Payments / Collections','Customer Care','Water quality','Lack of pressure',
'Defective installation','Sewerage','Billing cycle','Closing Application','Duplicate Documents','Supplies','Facilities','Offer acceptance','Invoiced consumption','Invoice Modifications','Infrastructures',
'Damage','Contractual conditions','-','Invoice charges','Complain','Official Complaint','Payment','Smart Metering','Quality service','Data Protection and Comunic.Publi.',
'Mod. Commercial Data','Charter Commitments','Appointment','Service point trading','GDPR','Business Cycle')

PredictionsToWrite <- PredictionsIDs %>% mutate(Resolution = predict(fit_xgb,dpred)) %>%
  rename(Date=START_DATE) %>% 
  mutate(Category = factor(Category, levels = PredCategoryOrder),
         Subject = factor(Subject, levels = PredSubjectOrder)) %>%
  arrange(Category,Subject,Date) %>% 
  mutate(ID = row_number())

write_csv(PredictionsToWrite %>% select(ID,Resolution),'./Ouputs/Submission/Resolution.csv')
