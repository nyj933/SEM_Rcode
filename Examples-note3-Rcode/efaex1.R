
library(lavaan)
npv <- read.csv("~/Downloads/Examples-note3/npv.csv")
npv = npv[,c(1:9)]

var.names <- colnames(npv)
fit <- efa(data = npv[,var.names], nfactors = 1:3)
summary(fit)
