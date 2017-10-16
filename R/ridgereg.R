## final functions wich will be exported in the package

#'A RC class to use ridge regression
#'
#'@field formula formula
#'@field data data frame
#'@field lambda lambda
#'
#'@details Please look at the vignette. 
#'@examples r <- ridgereg$new(Petal.Length~Sepal.Width+Sepal.Length, data=iris,lambda=2)
#'@exportClass ridgereg
#'@export ridgereg
#'@importFrom  methods new


#data<-iris
#formula<-Petal.Length ~ Sepal.Width + Sepal.Length
#formula<-Petal.Length ~ Species

#a<-lm.ridge(formula,iris,lambda = 2,standardize = FALSE)
#a$coef

#ridgereg$new(formula=Petal.Length ~ Sepal.Width + Sepal.Length,data=iris,lambda=2)

ridgereg<-setRefClass("ridgereg",fields=list(formula="formula",beta_ridge="matrix",y_hat="matrix",name_of_data_input="character",lambda="numeric"),
                     methods=list(
                       initialize=function(formula,data,lambda)
                       { 
                         name_of_data_input<<-deparse(substitute(data))
                         formula<<-formula
                         lambda<<-lambda
                         
                         dependent_variable_name<-all.vars(formula)[1]
                         Y<-data[[dependent_variable_name]] # vector of dependent variable
                         Y<-as.matrix(Y,ncol=1,nrow=length(Y))
                         X<-model.matrix(formula,data)[,-1] # matric of independent variables without intercept beta0
                         n<-dim(X)[2]
                         
                         X_norm<-matrix()
                         X_norm<-scale(X,center=TRUE,scale=TRUE) #standardized covariates
                         
                         beta_ridge <<- t(solve((t(X_norm) %*% X_norm) + (lambda*diag(rep(1,n))))%*%t(X_norm)%*%Y) #formula for beta_ridge
                         
                         y_hat <<- X %*% t(beta_ridge)                                               
                         
                       } ,
                       print=function()
                       {
                         "Prints the input and the coefficients in a user-friedly way"
                         cat("Call: \n")
                         cat(paste0("ridgereg(formula = ",format(formula),", data = ",name_of_data_input,", lambda = ",format(lambda),")\n\n"))
                         
                         cat("Coefficients: \n")
                         cat(" ")
                         cat(colnames(beta_ridge))
                         cat(" ")
                         cat(sep="\n")
                         cat(sep="      ",beta_ridge)
                       },
                       predict=function(datanew=NULL)
                       {"Returns predicted values"
                         if(is.null(datanew)){
                           return(y_hat)
                         }else{
                           X_new<-model.matrix(formula,datanew)[,-1]
                           X_norm_new<-data.frame(scale(X_new,center=TRUE,scale=TRUE))
                           y_hat_new <- X_new %*% t(beta_ridge)
                           return(y_hat_new)
                         }
                       },
                       coef=function()
                       {"Returns ridge regression regression coefficients"
                         return(beta_ridge)
                       }
                       
                     ))


# 
# predict=function(datanew=NULL)
# {"Returns predicted values"
#   if(is.null(datanew)){
#     return(y_hat)
#   }else{
#     X_new<-model.matrix(formula,datanew)[,-1]
#     X_norm_new<-data.frame(scale(X_new,center=TRUE,scale=TRUE))
#     y_hat_new <- X_new %*% t(beta_ridge)
#     return(y_hat_new)
#   }
# },