## final functions wich will be exported in the package

#'A RC class to use ridge regression
#'
#'@field 
#'
#'@details Please look at the vignette. 
#'@examples my_address<-address$new("Linkoping university")
#'@exportClass ridgereg
#'@export ridgereg

ridgereg<-setRefClass("ridgereg",fields=list(formula="forumula",beta_ridge="numeric",y_hat="numeric",name_of_data_input="character"),
                     methods=list(
                       initialize=function(formula,data,lambda)
                       { 
                         name_of_data_input<<-deparse(substitute(data))
                         formula<<-formula
                         
                         
                         
                         
                       },
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