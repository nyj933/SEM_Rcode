
#### Drinking and Driving

library(lavaan)
library(tidyverse)

urlfile="https://raw.github.com/nyj933/SEM_Rcode/main/Examples-note4-Rcode/drinkdata.csv"
drink <- read.csv(urlfile)
drink <- drink[,c(1:16)]
drink_std <- drink %>% mutate_at(colnames(drink), ~(scale(.) %>% as.vector))

######## drink11a

model11a <- '
             Attitude =~ X1 + X2 + X3 + X4 + X5
'
fit11a <- lavaan::cfa(model11a,data=drink_std,
                      likelihood = "wishart",std.lv=T,estimator="MLR")
summary(fit11a,standardized=TRUE,rsquare=T,fit.measures=T)



######## drink12a
model12a <- '
             Attitude =~ X1 + X2 + X3 + X4 + X5
             Norms =~ X6 + X7 + X8
'
fit12a <- lavaan::cfa(model12a,sample.cov = cormat, sample.nobs = 756,
                      likelihood = "wishart",std.lv=T)
summary(fit12a,standardized=TRUE,rsquare=T,fit.measures=T)

######## drink13a
model13a <- '
             Attitude =~ X1 + X2 + X3 + X4 + X5
             Norms =~ X6 + X7 + X8
             Control =~ X9 + X10 + X11 + X12
'
fit12a <- lavaan::cfa(model13a, data = drink_std,
                      likelihood = "wishart",std.lv=T)
summary(fit13a,standardized=TRUE,rsquare=T,fit.measures=T)

######## drink14a

model14a <- '
             Intention =~ Y1 + Y2
             Behavior =~ Y3 + Y4
             Attitude =~ X1 + X2 + X3 + X4 + X5
             Norms =~ X6 + X7 + X8
             Control =~ X9 + X10 + X11 + X12
             
'

fit14a <- lavaan::cfa(model14a, data = drink_std,
                      likelihood = "wishart",std.lv=T)
summary(fit14a,standardized=TRUE,rsquare=T,fit.measures=T)

######## drink15a

model15a <- '
             Intention =~ Y1 + Y2
             Behavior =~ Y3 + Y4
             Attitude =~ X1 + X2 + X3 + X4 + X5
             Norms =~ X6 + X7 + X8
             Control =~ X9 + X10 + X11 + X12
             
             Intention ~ Attitude + Norms + Control
             Behavior ~ Intention
'
             
fit15a <- lavaan::cfa(model15a, data = drink_std,
                      likelihood = "wishart",std.lv=T)
summary(fit15a,rsquare=T,standardized=TRUE)             
             
        





