library(lavaan)
lower1 <- '5.86 3.12 3.32 35.28 23.85 622.09 4.02 2.14 29.42 5.33 
2.99 2.55 19.20 3.17 4.64 35.30 26.91 465.62 31.22 23.38 546.01'
lower2 <- '8.20 3.47 4.36 45.65 22.58 611.63 6.39 3.16 44.62 7.32 
3.22 3.77 23.47 3.33 4.02 45.58 22.01 548.00 40.99 21.43 585.14'
lower3 <- '5.74 1.35 2.49 39.24 12.73 535.30 4.94 1.65 37.36 5.39 
1.67 2.32 15.71 1.85 3.06 40.11 12.94 496.86 38.09 14.91 538.76'


covmat1 <- getCov(lower1,names = c("SOFED", "SOMED", "SOFOC", "FAFED", "MOMED", "FAFOC"))
covmat2 <- getCov(lower2,names = c("SOFED", "SOMED", "SOFOC", "FAFED", "MOMED", "FAFOC"))
covmat3 <- getCov(lower3,names = c("SOFED", "SOMED", "SOFOC", "FAFED", "MOMED", "FAFOC"))

modelA <- '
            Fed =~ FAFED + SOFED
            Med =~ MOMED + SOMED
            Foc =~ FAFOC + SOFOC
            
            SOMED ~~ c(a1,a2,a3)*SOMED
            SOFOC ~~ c(b1,b2,b3)*SOFOC
            SOFED ~~ c(c1,c2,c3)*SOFED
            
          
'

fit_A <- lavaan::cfa(modelA, sample.cov = list(covmat1,covmat2,covmat3), 
                     sample.nobs = c(80,80,80),likelihood = "wishart",
                     group.equal = c("lv.variances","lv.covariances","residuals"))

summary(fit_A, standardized=TRUE,rsquare=T)


##### Socio-characer B
modelB <- '
            Fed =~ FAFED + SOFED
            Med =~ MOMED + SOMED
            Foc =~ FAFOC + SOFOC
            
            SOMED ~~ c(a1,a2,a3)*SOMED
            SOFOC ~~ c(b1,b2,b3)*SOFOC
            SOFED ~~ c(c1,c2,c3)*SOFED
            SOMED ~~ c(NA,NA,0)*SOFED
            
          
'

fit_B <- lavaan::cfa(modelB, sample.cov = list(covmat1,covmat2,covmat3), 
                     sample.nobs = c(80,80,80),likelihood = "wishart",
                     group.equal = c("lv.variances","lv.covariances","residuals"))

summary(fit_B, standardized=TRUE,rsquare=T)








