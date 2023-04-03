
######## Input: Raw data

library(lavaan)
#### Union A
urlfile="https://raw.github.com/nyj933/SEM_Rcode/main/Examples-note1-Rcode/union.dat"
union <- read.table(urlfile)

colnames(union)=c("Y1","Y2","Y3","X1","X2")
modelA <- '
           Y1 ~ X2
           Y2 ~ X2 + Y1
           Y3 ~ X1 +Y1 +Y2

'
fit_A <- lavaan::cfa(modelA,data = union,likelihood = "wishart")
summary(fit_A, fit.measures = T, rsquare = T)

####### Union B: the same as Union A, but using different data form



