##Prediction of Grade Averages

library(lavaan)

PredictG <- read.table("~/Downloads/Examples-note1/Predict.dat", header=F, sep= " ")

colnames(PredictG) = c("GRAVEREQ", "GRAVELEC","KNOWLEDG", "IQPREVYR", "EDMOTIV")

model1 <- '
       
         GRAVEREQ ~ KNOWLEDG + IQPREVYR + EDMOTIV
         GRAVELEC ~ KNOWLEDG + IQPREVYR + EDMOTIV
'
fit1 = lavaan::sem(model1,data = PredictG,std.lv=T,likelihood = "wishart")
summary(fit1, standardized=TRUE,rsquare=T)

model2 <- '
       
         GRAVEREQ ~ KNOWLEDG + IQPREVYR + EDMOTIV
         GRAVELEC ~ KNOWLEDG + IQPREVYR + EDMOTIV
         
         GRAVEREQ ~~ GRAVELEC
'
fit2 = lavaan::sem(model1,data = PredictG,std.lv=T,likelihood = "wishart")
summary(fit2, standardized=TRUE,rsquare=T)



