library("lavaan")
library("tidyverse")

npv <- read.csv("~/Downloads/Note2/npv.csv")
npv <- npv[,c(1:9)]

colnames(npv)



##### npv1: Analyze Correlations


model_npv1 <- '
          Visual =~ VISPERC + CUBES + LOZENGES + SCCAPS
          Verbal =~ PARCOMP + SENCOMP + WORDMEAN
          Speed =~ ADDITION + COUNTDOT + SCCAPS
          
          '
corr_npv1 = cor(npv)

fit_npv1 = lavaan::cfa(model_npv1,sample.cov = corr_npv1,sample.nobs = 145,std.lv=TRUE,
               likelihood = "wishart")


summary(fit_npv1, standardized=TRUE,rsquare = T)
parameterEstimates(fit_npv1)

#### "se" in correlation matrix of independent variables
##### npv2 & 2a: Analyze Correlations + Robust estimation

model_npv2 <- '
          Visual =~ VISPERC + CUBES + LOZENGES + SCCAPS
          Verbal =~ PARCOMP + SENCOMP + WORDMEAN
          Speed =~ ADDITION + COUNTDOT + SCCAPS
          
          '

npv_std <- npv %>% mutate_all(~(scale(.) %>% as.vector))
fit_npv2 = cfa(model_npv2,data = npv_std,std.lv=TRUE,likelihood = "wishart",estimator = "MLR")
#summary(fit_npv2, standardized=TRUE,rsquare = T)
parameterEstimates(fit_npv2)



##### npv3 & 4: Robust Diagonally Weighted Least Squares

model_npv3  <- '
          Visual =~ VISPERC + CUBES + LOZENGES + SCCAPS
          Verbal =~ PARCOMP + SENCOMP + WORDMEAN
          Speed =~ ADDITION + COUNTDOT + SCCAPS
          
          '
#For the DWLS, lavaan also provides ‘robust’ variants: WLSM, WLSMVS, WLSMV
fit_npv4 = cfa(model_npv3,data = npv_std,std.lv=TRUE, estimator = "wlsm")
summary(fit_npv4, standardized=TRUE,rsquare = T)
parameterEstimates(fit_npv4)
