##Confirmatory Factor Analysis Model for Sympathy and Anger

library(lavaan)

lower <- '
           1 
           0.721 1
           0.731 0.599 1
           -0.368 -0.25 -0.374 1
           -0.362 -0.222 -0.335 0.722 1
           -0.283 -0.255 -0.312 0.744 0.699 1
'
name <- paste("X", seq(1:6), sep = "")
cormat <- getCov(lower,names=c(name))


###### Sympathyanger
model <- ' 
     #### measurement models
          Symp =~ X1 + X2 + X3
          Anger =~ X4 + X5 + X6

'
fit <- lavaan::cfa(model, sample.cov = cormat,sample.nobs = 138,
                   likelihood = "wishart", std.lv=T)
summary(fit,rsquare=T,standardized=TRUE)




