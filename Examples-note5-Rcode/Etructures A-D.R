library(lavaan)

######## EtructureA Factor Loadings, Factor Correlation, Error Variances Invariant

lower1 <- '
          63.382 
          70.984 110.237 
          41.710 52.747 60.584 
          30.218 37.489 36.392 32.295
'

lower2 <- ' 
          67.898 
          72.301 107.330 
          40.549 55.347 63.203 
          28.976 38.896 39.261 35.403
'

covmat1 <- getCov(lower1,names = c("VERBAL40","VERBAL50","MATH35", "MATH25") )
covmat2 <- getCov(lower2,names = c("VERBAL40","VERBAL50","MATH35", "MATH25") )
model <- '
          Verbal =~ VERBAL40 + VERBAL50
          Math =~ MATH35 + MATH25
'
fit_A <- lavaan::cfa(model, sample.cov=list(covmat1,covmat2),
            sample.nobs=c(865,900),std.lv=T,likelihood = "wishart",
            group.equal = c( "loadings","lv.variances", "lv.covariances", "residuals"))

summary(fit_A,standardized=TRUE,rsquare=T)


######### EtructureB Factor Correlation and Error Variances Invariant
fit_B <- lavaan::cfa(model, sample.cov=list(covmat1,covmat2),
                     sample.nobs=c(865,900),std.lv=T,likelihood = "wishart",
                     group.equal = c("lv.variances", "lv.covariances", "residuals"))

summary(fit_B,standardized=TRUE,rsquare=T)



######### EtructureC Factor Correlation Invariant
fit_C <- lavaan::cfa(model, sample.cov=list(covmat1,covmat2),
                     sample.nobs=c(865,900),std.lv=T,likelihood = "wishart",
                     group.equal = c("lv.covariances"))

summary(fit_C,standardized=TRUE,rsquare=T)


######### EtructureD Factor Loadings and Factor Correlation Invariant

fit_D <- lavaan::cfa(model, sample.cov=list(covmat1,covmat2),
                     sample.nobs=c(865,900),std.lv=T,likelihood = "wishart",
                     group.equal = c("loadings", "lv.variances", "lv.covariances"))

summary(fit_D,standardized=TRUE,rsquare=T)



