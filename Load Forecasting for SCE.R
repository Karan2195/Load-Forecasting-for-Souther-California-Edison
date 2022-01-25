
################################## Load Forecasting ################################

### KARAN SAXENA

######################## Import Libraries ##########################################
library(tidyverse)
library(forecast)
library(lubridate)
library(Holidays)
library(prophet)
library(xlsx)

################################## Import data & Inspect ###################################################################

data = read.csv("CompetitionData.csv")

head(data) 
tail(data) 
str(data)

#################### Change date column format from character to date ######################################################

data$Date = dmy(data$Date)

data$Day= day(data$Date)
data$Month = month(data$Date)
data$Year = year(data$Date)
data$Weekday = weekdays(data$Date)
data$Weekday = factor(data$Weekday)
data$Day = factor(data$Day)

########################################### Add Holidays #########################################################

Holiday= holidays(years=2008:2012, type = "USFed")

Holidays = data.frame(Date=Holiday,Type="H")

data = left_join(data,Holidays,by = "Date")
data$Type = factor(data$Type)

str(data)

###################### Create a time column: ###################################

data$time = 1: (dim(data)[1])

################################### Plot of Load ##################################

data %>% ggplot(aes(x=time,y=Load)) + geom_line()+
  theme_bw() 

################################## Plot of Temp ###################################

data %>% ggplot(aes(x=time,y=Temperature)) + geom_line()+
  theme_bw() 


################################## One day #######################################

data %>%  slice(1:(24)) %>% ggplot(aes(x=time,y=Load)) + geom_line()+
  geom_point() + theme_bw() 

################################ Two Weeks ####################################

data %>%  slice(1:(24 * 14)) %>% ggplot(aes(x=time,y=Load)) + geom_line()+
  geom_point() + theme_bw() 

################################ Create a Trend Column #########################

data$trend = data$time

data$Actual = data$Load

data$LoadLag1 = lag(data$Load,1)
data$LoadLag24 = lag(data$Load,24)
data$LoadLag25 = lag(data$Load,25)
data$LoadLag48 = lag(data$Load,48)
data$LoadLag49 = lag(data$Load,49)
data$LoadLag50 = lag(data$Load,50)

####################################################################################################
#          Divide data into training and testing 
####################################################################################################

# Scenario 1

# Training is Year 2008 to 2011
# Testing is the full year of 2012

train = subset(data, Year >= 2008 & Year <= 2010)
train %>% head
test = subset(data, Year == 2011)
test %>% head

str(train)
######################################## Linear Model 1 ########################################

M1 = lm(Load ~ trend + Weekday, data=train)
summary(M1)

# Store fitted values in data

train$M1 = M1$fitted.values

# Plot data and overlay predicted values

train %>% slice(1:(24*14)) %>% 
  ggplot(aes(x=time,y=Load)) + geom_line()+
  geom_line(aes(x=time,y=M1),col="red")


######################################## Linear Model 2 ########################################

M2 = lm(Load ~ trend + Weekday + Hour, data=train)
summary(M2)

# Store fitted values in data

train$M2 = M2$fitted.values

# Plot data and overlay predicted values

train %>% slice(1:(24*14)) %>% 
  ggplot(aes(x=time,y=Load)) + geom_line()+
  geom_line(aes(x=time,y=M2),col="red")

######################################## Linear Model 3 ########################################

M3 = lm(Load ~ trend + Weekday + Hour + Weekday*Hour, data=train)
summary(M3)

############################# Store fitted values in data ####################################

train$M3 = M3$fitted.values

############### Plot data and overlay predicted values ########################################

train %>% slice(1:(24*14)) %>% 
  ggplot(aes(x=time,y=Load)) + geom_line()+
  geom_line(aes(x=time,y=M3),col="red")

######################################## Linear Model 4 ########################################

##### Add temp square and temp cube to the data
train$temp_sq = train$Temperature**2
train$temp_cube = train$Temperature**3
test$temp_sq = test$Temperature**2
test$temp_cube = test$Temperature**3

str(train)

M4 = lm(Load ~ trend + Weekday + Hour + Weekday*Hour + Temperature*Hour + Temperature*Month 
        + temp_sq + temp_sq*Hour + temp_sq*Month, data=train)
summary(M4)

############################# Store fitted values in data ####################################

train$M4 = M4$fitted.values

############### Plot data and overlay predicted values ########################################

train %>% slice(1:(24*14)) %>% 
  ggplot(aes(x=time,y=Load)) + geom_line()+
  geom_line(aes(x=time,y=M4),col="red")


train$M4.residuals = M4$residuals

# Plot Residuals
train %>% ggplot(aes(x=time,y=M4.residuals)) + geom_line()+ 
  theme_bw()+  geom_hline(yintercept=0)

########################################### ACF & PACF Graphs of residuals ###########################

tsdisplay(train$M4.residuals,lag.max = 60)


## Looking at the ACF graph it indicates significant lags and the PACF indicates Lag1 & Lag24, Lag25
## Lag49, Lag50 which are added to the initial data

######################################## Linear Model 5 ###############################################

str(train)

train = train %>% select(trend,Weekday,Day,Hour,Temperature,Month,Actual,LoadLag1,Load,LoadLag24,LoadLag25,LoadLag48,
                         time,Day,M1,M2,M3,M4,Date,Year,temp_sq,temp_cube,LoadLag49,LoadLag50)

M5 = lm(Load ~ trend + Weekday + Hour + Day*Hour + Weekday*Hour + Temperature*Hour + Temperature*Month 
        + temp_sq + temp_cube +temp_sq*Hour +temp_cube*Hour+temp_sq*Month + temp_cube*Month 
        + LoadLag1 + LoadLag24 + LoadLag25 + LoadLag48 + LoadLag49 + LoadLag50, data=train)

summary(M5)

############################### Store fitted values in data ########################################

train %>% tail

### Add Fitted Values
train$M5[!is.na(train$LoadLag1) & 
           !is.na(train$LoadLag24) &!is.na(train$LoadLag25)& 
           !is.na(train$LoadLag48) & !is.na(train$LoadLag49)& !is.na(train$LoadLag50)]=M5$fitted.values

####################### Add residuals #########################################################

train$M5residuals[!is.na(train$LoadLag1) & 
                    !is.na(train$LoadLag24) &!is.na(train$LoadLag25)& 
                    !is.na(train$LoadLag48) & !is.na(train$LoadLag49)& !is.na(train$LoadLag50)]=M5$residuals


####################### Plot data and overlay Fitted values ###################################

train %>% slice(1:(24*28)) %>% 
  ggplot(aes(x=time,y=Load)) + geom_line()+
  geom_line(aes(x=time,y=M5),col="red")


tsdisplay(train$M5residuals,lag.max = 60)



MAPE = c(mean(abs((train$Load-train$M5)/train$Load),na.rm = TRUE)*100)
MAPE

######################### Linear Model 5 Test Set Prediction ####################################################
tail(train)
head(test)
tail(test)
str(train)
str(test)

train = train %>% select(trend,Weekday,Day,Hour,Temperature,Month,LoadLag1,Load,LoadLag24,LoadLag25,LoadLag48,
                         time,Day,Date,Year,temp_sq,temp_cube,LoadLag49,LoadLag50,M5)
test$M5 = NA

test = test %>% select(trend,Weekday,Day,Hour,Temperature,Month,LoadLag1,Load,LoadLag24,LoadLag25,LoadLag48,
                       time,Day,Date,Year,temp_sq,temp_cube,LoadLag49,LoadLag50,M5)

data_final1 = rbind(train,test)


for(i in 26305:35064){
  data_final1$LoadLag1[i] =  ifelse(is.na(data_final1$Load[i-1]),data_final1$Load[i-1],data_final1$M5[i-1])
  data_final1$LoadLag24[i] = ifelse(is.na(data_final1$Load[i-24]),data_final1$Load[i-24],data_final1$M5[i-24])
  data_final1$LoadLag25[i] = ifelse(is.na(data_final1$Load[i-25]),data_final1$Load[i-25],data_final1$M5[i-25])
  data_final1$LoadLag48[i] = ifelse(is.na(data_final1$Load[i-48]),data_final1$Load[i-48],data_final1$M5[i-48])
  data_final1$LoadLag49[i] = ifelse(is.na(data_final1$Load[i-49]),data_final1$Load[i-49],data_final1$M5[i-49])
  data_final1$LoadLag50[i] = ifelse(is.na(data_final1$Load[i-50]),data_final1$Load[i-50],data_final1$M5[i-50])
  data_final1$M5[i] =  predict(M5,newdata=data_final1[i,])
}


MAPE = c(mean(abs((data_final1$Load-data_final1$M5)/data_final1$Load),na.rm = TRUE)*100)

MAPE

#################################  Multiple Seasonality Decomposition #############################################################
# Reference Taken From: 
# https://github.com/Sasidhar-Sirivella/Energy-Consumption-Forecasting---Multiple-Seasonality/blob/master/RCode_Energy%20Consumption.R

# SPECIFY Training set end data:
y = 2010

# determine length of training set:
n_train = dim(data[(data$Year <= y),])[1]
n_train

# SPECIFY Testing set start and end dates

# start 
y1 = 2011

# End
y2 = 2011

# determine length of testing set:

n_test = dim(data[(data$Year == y1),])[1]
n_test
n_train

# create a multiple seasonality time series:

y.ts=msts(na.omit(data$Load)[1:(n_train+n_test)],seasonal.periods = c(24,7*24,365.25*24))

# Build a model on the training set

length(head(y.ts,n_train))

M = mstl(head(y.ts,n_train)) 

autoplot(M) + ggtitle("Series Decomp Autoplot")

# Forecast on the testing set using Different Menthods

########################### ARIMA ##########################################################

MF = forecast(M,method="arima",h=n_test,level=95)

autoplot(MF) + autolayer(MF$fitted)

######################################### Naive ####################################################

MF1 = forecast(M,method="naive",h=n_test,level=95)

autoplot(MF1) + autolayer(MF1$fitted)

### ETS ###
MF2 = forecast(M,method="ets",h=n_test,level=95)

autoplot(MF2)+ autolayer(MF2$fitted)

accuracy(MF1,tail(y.ts,n_test))


################################### Seasonal Naive ################################################

### snaive Model ###

M1=snaive(head(y.ts,n_train),h=n_test,lambda="auto")

MF4 = forecast(M1,h=n_test)

autoplot(MF4) + autolayer(MF4$fitted)

accuracy(MF4,tail(y.ts,n_test))

##################################### Comparing the accuracies #############################################

naive_results = accuracy(MF1,tail(y.ts,n_test))
ets_result = accuracy(MF2,tail(y.ts,n_test))
arima_results= accuracy(MF,tail(y.ts,n_test))
Snaive_results = accuracy(MF4,tail(y.ts,n_test))

train_accuracy_metrics = rbind(accuracy(MF,tail(y.ts,n_test))[1,c("RMSE","MAPE")],
                               accuracy(MF1,tail(y.ts,n_test))[1,c("RMSE","MAPE")],
                               accuracy(MF2,tail(y.ts,n_test))[1,c("RMSE","MAPE")],
                               accuracy(MF4,tail(y.ts,n_test))[1,c("RMSE","MAPE")])

Training_accuracy= data.frame(Train_Model = c("Arima","Naive","ETS","SNaive"),
                              train_accuracy_metrics)


test_accuracy_metrics = rbind(round(accuracy(MF,tail(y.ts,n_test))[2,c("RMSE","MAPE")]),
                              round(accuracy(MF1,tail(y.ts,n_test))[2,c("RMSE","MAPE")]),
                              round(accuracy(MF2,tail(y.ts,n_test))[2,c("RMSE","MAPE")]),
                              round(accuracy(MF4,tail(y.ts,n_test))[2,c("RMSE","MAPE")]))

Testing_accuracy= data.frame(Test_Model = c("Arima","Naive","ETS","SNaive"),
                             test_accuracy_metrics)

############################## Comparing the models with help of plots ############################################

autoplot(y.ts, series = "Original data") +
  geom_line(size = 1) +
  autolayer(MF, PI = FALSE, size = 1,
            series = "arima") +
  autolayer(MF1, PI = FALSE, size = 1,
            series = "naive") +
  autolayer(MF2, PI = FALSE, size = 1,
            series = "ETS") +
  autolayer(MF4, PI = FALSE, size = 1,
            series = "SNaive") +
  ggtitle("Forecast from ARIMA, Naive, ETS and SNaive")

############################################## Prophet Model ####################################################
# Reference Taken From:-
#https://facebook.github.io/prophet/docs/quick_start.html#r-api

?prophet
?make_future_dataframe

str(data)


train2 = train %>% select(Date,Load,Temperature,temp_sq,temp_cube) 
test2 = test %>% select(Date,Hour,Load,Temperature)
colnames(train2) = c("ds","y","temp","temp_sq","temp_cube")
dim(train2)[1]

m= prophet()
m = add_regressor(m, 'temp')
m = add_regressor(m, 'temp_sq')
m= add_regressor(m, 'temp_cube')
m = add_country_holidays(m, country_name = 'US')


m = prophet(train2, yearly.seasonality = T,daily.seasonality = T, weekly.seasonality = T,
            holidays.prior.scale = .05)

summary(m)

future <- make_future_dataframe(m, periods = 24*365,freq= 3600)
tail(future)
forecast <- predict(m, future)
tail(forecast[c('ds', 'yhat', 'yhat_lower', 'yhat_upper')])

plot(m, forecast)

prophet_plot_components(m, forecast)

######################################### Neural Net #######################################################

train
tail(test)

# SPECIFY Training set end data:
y = 2010

# determine length of training set:

n_train = dim(data[(data$Year <= y),])[1]
n_train

# SPECIFY Testing set start and end dates

# start 
y1 = 2011

# End
y2 = 2011

# determine length of testing set:

n_test = dim(data[(data$Year == y1),])[1]
n_test
n_train

# create a multiple seasonality time series:

y.ts=msts(na.omit(data$Load)[1:(n_train+n_test)],seasonal.periods = c(24,7*24,365.25*24))

# Build a model on the training set

length(head(y.ts,n_train))

?nnetar

ns=1:2 # Non Seasonal
s=1:2 # Seasonal
hl=1:2 # Hidden Layer

Results = data.frame(p=NA,P=NA,size=NA,RMSE=NA,MAPE=NA)

for (i in ns) {
  for (j in s){
    for (k in hl){
      NN= nnetar(head(y.ts,n_train),p=i,P=j,size=k,lamda='auto')
      NNF= forecast(NN,h= n_test)
      Results = rbind(Results,c(i,j,k,accuracy(NNF,tail(y.ts,n_test))[2,'RMSE'],accuracy(NNF,tail(y.ts,n_test))[2,'MAPE']))
      print(c(i,j,k))
    }
  }
}

Results

###################################### Champion Model is Regression #######################################################

# Training is Year 2008 to 2011
# Testing is the full year of 2012

train1 = filter(data, Year >= 2008 & Year <= 2011)
train1 %>% head
test1 = filter(data, Year == 2012)
test1 %>% head
str(train1)

train1$temp_sq = train1$Temperature**2
train1$temp_cube = train1$Temperature**3
test1$temp_sq = test1$Temperature**2
test1$temp_cube = test1$Temperature**3

str(train1)

train1 = train1 %>% select(trend,Weekday,Day,Hour,Temperature,Month,LoadLag1,Load,LoadLag24,LoadLag25,LoadLag48,
                           time,Day,Date,Year,temp_sq,temp_cube,LoadLag49,LoadLag50)

M_champ = lm(Load ~ trend + Weekday + Hour + Day*Hour + Weekday*Hour + Temperature*Hour + Temperature*Month 
             + temp_sq + temp_cube +temp_sq*Hour +temp_cube*Hour+temp_sq*Month + temp_cube*Month +
               LoadLag1+ LoadLag24 + LoadLag25 + LoadLag48 + LoadLag49 + LoadLag50, na.action = na.omit,data=train1)
summary(M_champ)

# Store fitted values in data

train %>% tail


train1$M[!is.na(train1$LoadLag1) & 
           !is.na(train1$LoadLag24) &!is.na(train1$LoadLag25)& 
           !is.na(train1$LoadLag48) & !is.na(train1$LoadLag49)& !is.na(train1$LoadLag50)]=M_champ$fitted.values

# Plot data and overlay predicted values

train1 %>% slice(1:(24*28)) %>% 
  ggplot(aes(x=time,y=Load)) + geom_line()+
  geom_line(aes(x=time,y=M_champ),col="red")


MAPE = c(mean(abs((train1$Load-train1$MFitted)/train1$Load),na.rm = TRUE)*100)

MAPE
################################ Champion Model Forecast for 2012 ###################################################
tail(train1)
head(test1)
tail(test1)
str(train1)
str(test1)

train1 = train1 %>% select(trend,Weekday,Day,Hour,Temperature,Month,LoadLag1,Load,LoadLag24,LoadLag25,LoadLag48,
                           time,Day,Date,Year,temp_sq,temp_cube,LoadLag49,LoadLag50,M)
test1$M = NA

test1 = test1 %>% select(trend,Weekday,Day,Hour,Temperature,Month,LoadLag1,Load,LoadLag24,LoadLag25,LoadLag48,
                         time,Day,Date,Year,temp_sq,temp_cube,LoadLag49,LoadLag50,M)
data_final = rbind(train1,test1)

i=35065 # First testing observation number

data_final$M[i] =  predict(M_champ,newdata=data_final[i,])

for(i in 35066:43848){
  data_final$LoadLag1[i] =  ifelse(is.na(data_final$Load[i-1]),data_final$M[i-1],data_final$Load[i-1])
  data_final$LoadLag24[i] = ifelse(is.na(data_final$Load[i-24]),data_final$M[i-24],data_final$Load[i-24])
  data_final$LoadLag25[i] = ifelse(is.na(data_final$Load[i-25]),data_final$M[i-25],data_final$Load[i-25])
  data_final$LoadLag48[i] = ifelse(is.na(data_final$Load[i-48]),data_final$M[i-48],data_final$Load[i-48])
  data_final$LoadLag49[i] = ifelse(is.na(data_final$Load[i-49]),data_final$M[i-49],data_final$Load[i-49])
  data_final$LoadLag50[i] = ifelse(is.na(data_final$Load[i-50]),data_final$M[i-50],data_final$Load[i-50])
  data_final$M[i] =  predict(M_champ,newdata=data_final[i,])
  data_final[i,"LowerBound"] = predict(M_champ,newdata = data_final[i,], interval = "prediction")[2] 
  data_final[i,"UpperBound"] = predict(M_champ,newdata  = data_final[i,], interval = "prediction")[3]
}

data_final %>% tail(1000)

data_final$TrainTest = "Train"
data_final$TrainTest[data_final$Year == 2012] = "Test"

data_final %>% ggplot(aes(x=time,y=Load)) +geom_line(size=1.25) + 
  geom_line(aes(x=time,y=M,color= TrainTest),size=1.25)  + theme_bw()

######################################### Write to Excel #######################################################

data_final %>% head(51)

final = data_final %>% select(Date,Hour,Temperature, Load, M)

write.xlsx(final, file = "Load_Predictions.xlsx", sheetName = "Load", 
           col.names = TRUE, row.names = TRUE, append = FALSE)

#################################################################################################################
