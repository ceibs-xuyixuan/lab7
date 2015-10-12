#' Function for ridge regression 
#' @param formula formula.
#' @param data data.frame.
#' @return A linreg object.
#' @field formula A formula
#' @field data A data.frame
#' @field bhat A matrix
#' @field yhat A matrix
#' @examples 
#' ridgereg1 <- ridgereg(formula = Petal.Length ~ Species, data = iris)
#' ridgereg1$print()
#' ridgereg1$coeff()
#' ridgereg1$pred()  
#' ## We decide to use QR decompositon.
ridgereg <- setRefClass("rigdereg",
                      fields=list(formula="formula", 
                                  data = "data.frame", 
                                  bhat = "matrix",
                                  yhat = "vector",
                                  lambda = "numeric"),
                      
                      methods = list(
                        initialize = function(formula, data){
                          .self$formula <- formula
                          .self$data <- data
                          X <- model.matrix(formula, data)
                          y <- data[,all.vars(formula)[1]]
                          .self$lambda <- runif(1, 0, 0.01)
                          X_norm <- X
                          for(i in 2:ncol(X)){
                            X_norm[,i] <- (X[,i] - mean(X[,i])) / sqrt(var(X[,i]))
                          }
                          X_blabla <- sqrt(.self$lambda) * diag(ncol(X_norm))
                          X_bind <- rbind(X,X_blabla)
                          y_bind <- c(y, rep(0, ncol(X_norm)))
                          qr_Xb <- qr(X_bind)
                          .self$bhat <- solve(qr.R(qr_Xb)) %*% t(qr.Q(qr_Xb)) %*% y_bind
                          .self$yhat <- X_norm %*% .self$bhat
#                           .self$bhat <-  solve(t(X) %*% X + (lambda*I(ncol(X)))) %*% t(X) %*% y
#                           .self$yhat <- X %*% .self$bhat
                        },
                        
                        print = function(){
                          writeLines("Call:")
                          f1 <- as.character(.self$formula)
                          writeLines(c("lm.ridge(formula =", f1[2],f1[1],f1[3], ",data=iris)"), sep=" ")
                          writeLines("\n")
                          writeLines("Coefficients:")
                          t(.self$bhat)
                        },
                        
                        coeff = function(){
                          writeLines("Coefficientes:")
                          return(c(.self$bhat[1],.self$bhat[2], .self$bhat[3]))
                        },
                        
                        pred = function(){
                          writeLines("Predicted values:")
                          return(.self$yhat)
                        }
                        
                      )
)
