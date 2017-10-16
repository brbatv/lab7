## ----include=FALSE-------------------------------------------------------
library(devtools)
library(caret)
library(nycflights13)
library(dplyr)
#install_github("brbatv/lab7",force=TRUE)
library(lab7ab)

## ----echo=FALSE,warning=FALSE--------------------------------------------
colnames(weather)

## ----echo=FALSE,warning=FALSE--------------------------------------------
colnames(flights)

## ----echo=FALSE,warning=FALSE--------------------------------------------
flights_weather <- left_join(flights, weather,by = c("year", "month", "day", "origin", "hour", "time_hour"))

## ----warning=FALSE-------------------------------------------------------
database <- select(flights_weather,month,day, sched_dep_time,sched_arr_time, dep_delay, arr_delay,temp,dewp,wind_dir, wind_speed,wind_gust,precip,pressure,visib)

## ----warning=FALSE-------------------------------------------------------
library(caret)
set.seed(3456)
database<-na.omit(database)

trainIndex = createDataPartition(database$arr_delay, 
                       p=0.8, list=FALSE,times=1)
train = database[trainIndex,]
notrain = database[-trainIndex,]
validationIndex = createDataPartition(notrain$arr_delay, 
                       p=0.75, list=FALSE,times=1)
validation=database[validationIndex,]

test = notrain[-validationIndex,]

## ----warning=FALSE-------------------------------------------------------
#root mean squared arror
rmse <- function(error){
    sqrt(mean(error^2))
}

#formula for ridge regression
formula=arr_delay~month+day+sched_dep_time+sched_arr_time+dep_delay+temp+dewp+wind_dir+ wind_speed+wind_gust+precip+pressure+visib 

#training ridge regression for different values of lambda
mod<-ridgereg$new(formula=formula, data=train,lambda=10^8)

Y_validation<-validation$arr_delay
Y_predicted <- mod$predict(newdata=validation)

#calculating the error
error= Y_predicted - Y_validation


RMSE_check<-rmse(error)

## ----echo=FALSE,warning=FALSE--------------------------------------------

best_lambda=10^8

mod<-ridgereg$new(formula=formula, data=train,lambda=best_lambda)  
  
Y_test<-test$arr_delay
Y_predicted2 <- mod$predict(newdata=test)

error2= Y_predicted2 - Y_test

RMSE_best_lambda<-rmse(error2)
RMSE_best_lambda

