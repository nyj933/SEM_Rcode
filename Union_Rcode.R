
library(lavaan)
library(tidyverse)

union <- read.csv("Downloads/Note1/Union.csv")
union <- union[,c(1:5)]

#### Union A:

cov_matrix <- cov(union)

model <- '
          Y1 ~ X2
          Y2 ~ X2+Y1
          Y3 ~ X1+Y1+Y2

'
fit <- sem(model, sample.cov = cov_matrix, sample.nobs = 173, std.lv=TRUE,likelihood = "wishart")
summary(fit, fit.measures=TRUE,rsquare=T)

fit_B <- sem(model,data = union,std.lv=TRUE,likelihood = "wishart")
summary(fit_B, standardized=TRUE,rsquare = T)


efaex <- read.csv("~/Downloads/Note2/efaex.csv")
efaex <- efaex[,c(1:5)]

## change





