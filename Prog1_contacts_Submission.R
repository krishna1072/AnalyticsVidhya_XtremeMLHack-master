rm(list=ls());gc()
setwd('K:/RHack/Xtreme ML Hack/')

# loading libraries
library(readr)
library(dplyr)
library(tidyr)
library(magrittr)
library(lubridate)
library(dummies)
library(zoo)
library(htmltab)
library(ggplot2)
library(xgboost)

# reading files
Contacts_Pre_2017 <- read_csv('./Inputs/Train_BG1IL20/Train/Contacts_Pre_2017.csv')
 
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
  rename(START_DATE=HolidayDate) %>%
  data.frame() %>%
  dummy.data.frame(sep='_')  %>%
  group_by(START_DATE) %>%
  summarise_all(funs(max(.)))

# Checking Date ranges from each files
range(Contacts_Pre_2017$START.DATE)

# Checking whether contact start date and end date are same
Contacts_Pre_2017[Contacts_Pre_2017$START.DATE != Contacts_Pre_2017$END.DATE,]

# Normalise contracts column names
Contacts_Pre_2017 <- Contacts_Pre_2017 %>% rename_(.dots=setNames(names(.), gsub("\\.", "_", names(.))))

contact_types <- unique(Contacts_Pre_2017$CONTACT_TYPE)

# Pre-processing data
# Summarizing at CONTACT_TYPE,START_DATE
Pre1 <- Contacts_Pre_2017 %>% 
  group_by( CONTACT_TYPE,START_DATE) %>% 
  summarise(Contacts = sum(Contacts)) %>% 
  arrange(CONTACT_TYPE,START_DATE)  %>% 
  ungroup()

# Checking the distribution
ggplot(Pre1,aes(CONTACT_TYPE,Contacts)) + geom_boxplot() + coord_flip()
ggplot(Pre1,aes(x=START_DATE,y=Contacts)) + geom_line() + facet_wrap(~CONTACT_TYPE,scales='free_y',nrow=5, ncol=2)
ggplot(Pre1,aes(Contacts)) + geom_histogram() + facet_wrap(~CONTACT_TYPE,scales='free_y',nrow=5, ncol=2)

# Make sure the data is available for each date in date range
DateDataClean <- function(type,Indata){
  DateData <- data.frame(START_DATE = as.Date(as.Date('2010-01-01'):as.Date('2017-03-15'),origin='1970-01-01'))
  Indata <- Indata %>% filter(CONTACT_TYPE == type) %>%
    mutate(Contacts = as.numeric(Contacts)) %>%
    right_join(DateData,by='START_DATE') %>%
    mutate(CONTACT_TYPE = type,Contacts = as.numeric(if_else(is.na(Contacts),0,Contacts)))
  return(Indata)
}

Pre1 <- bind_rows(lapply(contact_types,function(x) DateDataClean(type = x,Indata=Pre1)))

Pre1 <- Pre1 %>%
        left_join(HolidaysData,by='START_DATE') %>%
        mutate_at(vars(Holiday_Type_Clock_change_Daylight_Saving_Time:HolidayFlag),funs(replace(.,is.na(.),0))) %>%
        select(START_DATE,Contacts,CONTACT_TYPE,Holiday_Type_National_holiday)

Pre1_LagVars <- Pre1 %>% 
  select(CONTACT_TYPE,START_DATE,Contacts) %>% 
  group_by(CONTACT_TYPE,START_DATE) %>% 
  summarise(Contacts=sum(Contacts)) %>% 
  mutate(DayOfTheWeek=as.factor(wday(START_DATE))) %>% 
  arrange(CONTACT_TYPE,START_DATE) %>% 
  group_by(CONTACT_TYPE) %>% 
  mutate(PerYear_1DayChg = (Contacts-lag(Contacts))/lag(Contacts)) %>% 
  mutate(Per2Year_Chg = (Contacts-lag(Contacts,n=365))/lag(Contacts,n=365)) %>% 
  mutate(Prev1Year_7_lag = rollapply(data=Contacts, width = 7, FUN = median, align = "right", fill = NA, na.rm = T)) %>% 
  mutate(Prev1Year_7_lead = rollapply(data=Contacts, width = 7, FUN = median, align = "left", fill = NA, na.rm = T)) %>% 
  mutate(Prev1Year_7_Center = rollapply(data=Contacts, width = 14, FUN = median, align = c("center"), fill = NA, na.rm = T)) %>% 
  group_by(CONTACT_TYPE,DayOfTheWeek) %>% 
  arrange(CONTACT_TYPE,DayOfTheWeek) %>% 
  mutate(SameWeekDayLas6MContacts_Median = rollapply(data=as.numeric(ifelse(Contacts!=0,Contacts,NA)), width = 26, FUN = median, align = "right", fill = NA, na.rm = T)) %>%
  ungroup() %>%
  select(CONTACT_TYPE,START_DATE,Contacts,Prev1Year_7_lag,Prev1Year_7_lead,Prev1Year_7_Center,PerYear_1DayChg,Per2Year_Chg,SameWeekDayLas6MContacts_Median) %>%
  arrange(CONTACT_TYPE,START_DATE)

Pre1 <- Pre1 %>% 
  filter(START_DATE >= as.Date('2011-01-01')) %>% 
  mutate(START_DATE1 = START_DATE - 365) %>%
  left_join(Pre1_LagVars %>% rename(START_DATE1 = START_DATE),by=c('START_DATE1','CONTACT_TYPE')) %>%
  rename(Contacts = Contacts.x,Contacts_365_Lag =Contacts.y) %>%
  select(-START_DATE1) %>%
  mutate_at(vars(Contacts:SameWeekDayLas6MContacts_Median),funs(replace(.,is.infinite(.) | is.nan(.),NA)))

# Checking the distribution
ggplot(Pre1,aes(CONTACT_TYPE,Contacts)) + geom_boxplot() + coord_flip()
ggplot(Pre1,aes(x=START_DATE,y=Contacts)) + geom_line() + facet_wrap(~CONTACT_TYPE,scales='free_y',nrow=5, ncol=2)
ggplot(Pre1,aes(Contacts)) + geom_histogram() + facet_wrap(~CONTACT_TYPE,scales='free_y',nrow=5, ncol=2)

Pre2 <- Pre1 %>%  
  mutate(CONTACT_TYPE = gsub('[- .]','_',CONTACT_TYPE)) %>% 
  separate(START_DATE, c("Year",'Month', "Date"),remove=F) %>%
  mutate(Month=as.factor(Month),Year=as.numeric(Year),DayOfTheMonth=as.factor(Date),DayOfTheWeek=as.factor(wday(START_DATE))) %>%
  select(-Date) %>%
  as.data.frame() %>% 
  dummy.data.frame(sep='_') %>% 
  tbl_df() 

Pre2 <- Pre1 %>% select(CONTACT_TYPE) %>% 
  bind_cols(Pre2) %>%
  select(CONTACT_TYPE,START_DATE,everything())
                   
Pre2 %T>% {
  #Train dataset until dec 31 2016
  filter(.,START_DATE <= as.Date('2016-12-31')) %>% 
    select(-START_DATE,-CONTACT_TYPE) ->> Train
  filter(.,START_DATE <= as.Date('2016-12-31')) %>% 
    select(START_DATE,CONTACT_TYPE) ->> TrainIDs
  #Test dataset from Jan 1 2017
  filter(.,START_DATE >= as.Date('2017-01-01')) %>% 
     select(-START_DATE,-CONTACT_TYPE) ->> Predictions
  filter(.,START_DATE >= as.Date('2017-01-01')) %>% 
    select(START_DATE,CONTACT_TYPE) ->> PredictionsIDs
}
                         
rm(Contacts_Pre_2017,Holidays,HolidaysData,Pre1,Pre1_LagVars,Pre2,pulledholidays);gc()
                       
#Build model
dtrain <- xgb.DMatrix(data = Train %>% select(-Contacts) %>% as.matrix(),label=Train %>%  select(Contacts) %>% as.matrix(),missing=NA)
dpred <- xgb.DMatrix(data = Predictions %>% select(-Contacts) %>% as.matrix(),label=Predictions %>%  select(Contacts) %>% as.matrix(),missing=NA)

param <- list(booster = 'gbtree',
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
PredContactOrder <- c('Mail - Recieved','Web - Input','Call - Input','Internal Management','Visit','Mail - Input','Fax - Input','Fax Acknowledgement - Input',
  'Installation Report - Input','Tweet - Input')

PredictionsToWrite <- PredictionsIDs %>% mutate(Contacts = predict(fit_xgb,dpred)) %>%
  rename(Date=START_DATE,CONTACT.TYPE=CONTACT_TYPE) %>% 
  mutate(CONTACT.TYPE = factor(CONTACT.TYPE, levels = PredContactOrder)) %>%
  arrange(CONTACT.TYPE,Date) %>% 
  mutate(ID = row_number())

write_csv(PredictionsToWrite %>% select(ID,Contacts),'./Ouputs/Submission/Contacts.csv')
