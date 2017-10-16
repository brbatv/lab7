## ----include=FALSE-------------------------------------------------------
set.seed(27)
library(devtools)
#install_github("brbatv/lab7",force=TRUE)
library(lab7ab)
library(MASS)
library(mlbench)
library(caret)
library(leaps)
data("BostonHousing")

## ------------------------------------------------------------------------
set.seed(27)
training_data <- createDataPartition(BostonHousing$crim,p = 0.55)

training <- BostonHousing[training_data$Resample1, ]
test <- BostonHousing[-training_data$Resample1, ]

## ----results:hide--------------------------------------------------------
set.seed(27)
full_model <- lm(crim~.,data=training)


## ----include=FALSE-------------------------------------------------------
set.seed(27)
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

## ------------------------------------------------------------------------
set.seed(27)
ridgeex <- list(type="Regression", 
              library="lab7ab",
              loop=NULL,
              prob=NULL,
              parameters=data.frame(parameter="lambda",class="numeric",label="lambda"),
              grid=function(y,x, len=NULL, search="grid")
              {
                data.frame(lambda = seq(0,200,by=5))
              },
              fit=function (x, y, wts, param, lev, last, classProbs, ...) 
              {
                dat <- if (is.data.frame(x)) 
                  x
                else as.data.frame(x)
                dat$.outcome <- y
                out <- ridgereg$new(formula=.outcome ~ ., data = dat, lambda=param$lambda, ...)
                out
              },
              predict = function (modelFit, newdata, submodels = NULL,preProc=NULL) 
              { 
                if (!is.data.frame(newdata)) 
                  newdata <- as.data.frame(newdata)
                
                modelFit$predict(newdata)
              })



testo<-caret::train(crim ~ ., data=training, method = ridgeex)
testo

