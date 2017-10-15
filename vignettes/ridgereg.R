## ----include=FALSE-------------------------------------------------------
library(devtools)
#install_github("brbatv/lab7",force=TRUE)
library(lab7ab)
library(MASS)
library(mlbench)
library(caret)
library(leaps)
data("BostonHousing")

## ------------------------------------------------------------------------
training_data <- createDataPartition(BostonHousing$crim,p = 0.55)

training <- BostonHousing[training_data$Resample1, ]
test <- BostonHousing[-training_data$Resample1, ]

## ----results:hide--------------------------------------------------------
full_model <- lm(crim~.,data=training)


## ----include=FALSE-------------------------------------------------------

step <- stepAIC(full_model, direction="backward")

## ----eval=FALSE,warning=FALSE--------------------------------------------
#  
#  step <- stepAIC(full_model, direction="backward")

## ----warning=FALSE-------------------------------------------------------
step$anova # display results 

## ----warning=FALSE-------------------------------------------------------
lm1 <- caret::train(crim ~ ., data=training, method = 'leapForward')
lm1
summary(lm1)

