library(lavaan)
library(ivreg)
lower <- '
3792.439
164.169   115.218
554.762    33.217   119.409
166.905   -24.385    44.721    62.252
379.754    38.651    56.171   -16.020    71.886'

covmat <- getCov(lower,names = c("x1","x2","x3","y1","y2"))

model <- ' 
             y1 ~ y2+x1
             y2 ~ x2+x3
             #x1 ~ x2+x3
           #  x2 ~  x3
            ##x1 ~ x2+x3
             ##x2~~x3
             y1 ~~ y2
            # x1 ~~ x2
            # x2 ~~ x3
            # x1~~x3
            # y2 ~~ x1
          
         '
model <- ' 
             y1 ~ y2+x1
             y2 ~ x1+x2+x3
             x1 ~ x2+x3
           #  x2 ~  x3
            ##x1 ~ x2+x3
             ##x2~~x3
             y1 ~~ y2
            # y2 ~~ x1
          
         '

# Estimate the model using instrumental variables
fit <- lavaan::cfa(model,sample.cov = covmat,
                   sample.nobs = 23,std.lv=T,likelihood = "wishart")
summary(fit, standardized=TRUE,rsquare=T)




model <- ' 
             y1 ~ y2+x2+x3
             y2 ~ x1+x2+x3
             x2 ~ x1+x3
             x3~  x1+x2

             y1 ~~ y2

         '
fit <- lavaan::cfa(model,sample.cov = covmat,
                   sample.nobs = 23,likelihood = "wishart")
summary(fit, standardized=TRUE,rsquare=T)


