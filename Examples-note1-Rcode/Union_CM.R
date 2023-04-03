##Union Sentiment of Textile Workers
library(lavaan)
#### Union A

lower <- '  
   14.610
   -5.250  11.017
   -8.057  11.087  31.971
   -0.482   0.677   1.559   1.021
  -18.857  17.861  28.250   7.139 215.662'

covmat <- getCov(lower,names=c("Y1","Y2","Y3","X1","X2"))

modelA <- '
           Y1 ~ X2
           Y2 ~ X2 + Y1
           Y3 ~ X1 +Y1 +Y2

'
fit_A <- lavaan::cfa(modelA,sample.cov = covmat, 
                     sample.nobs = 173,likelihood = "wishart")
summary(fit_A, fit.measures = T, rsquare = T)

####### Union B: the same as Union A, but using different data form 



