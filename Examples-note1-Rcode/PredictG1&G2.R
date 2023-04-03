##Prediction of Grade Averages

library(lavaan)

urlfile="https://raw.github.com/nyj933/SEM_Rcode/main/Examples-note1-Rcode/Predict.DAT"
PredictG <- read.table(urlfile, header=F, sep= " ")

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



