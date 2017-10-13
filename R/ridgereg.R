## final functions wich will be exported in the package

#'A RC class to use ridge regression
#'
#'@field 
#'
#'@details Please look at the vignette. 
#'@examples my_address<-address$new("Linkoping university")
#'@exportClass ridgereg
#'@export ridgereg

#data<-iris
#formula<-Petal.Length ~ Sepal.Width + Sepal.Length
#formula<-Petal.Length ~ Species

#a<-lm.ridge(formula,iris,lambda = 2,standardize = FALSE)
#a$coef

#ridgereg$new(formula=Petal.Length ~ Sepal.Width + Sepal.Length,data=iris,lambda=2)

ridgereg<-setRefClass("ridgereg",fields=list(formula="formula",beta_ridge="matrix",y_hat="matrix",name_of_data_input="character"),
                     methods=list(
                       initialize=function(formula,data,lambda)
                       { 
                         name_of_data_input<<-deparse(substitute(data))
                         formula<<-formula
                         
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
                         cat(paste0("ridgereg(formula = ",format(formula),", data = ",name_of_data_input,")\n\n"))
                         
                         cat("Coefficients: \n")
                         cat(" ")
                         cat(names(beta_ridge))
                         cat(" ")
                         cat(sep="\n")
                         cat(sep="      ",beta_ridge)
                       }
                       
                     ))