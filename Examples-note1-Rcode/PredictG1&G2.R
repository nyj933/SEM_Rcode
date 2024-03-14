##Prediction of Grade Averages

library(lavaan)

urlfile="https://raw.github.com/nyj933/SEM_Rcode/main/Examples-note1-Rcode/Predict.DAT"
PredictG <- read.table(urlfile, header=T, sep= " ")

colnames(PredictG) = c("GRAVEREQ", "GRAVELEC","KNOWLEDG", "IQPREVYR", "EDMOTIV")
cov(PredictG)

model1 <- '
       
         GRAVEREQ ~ KNOWLEDG + IQPREVYR + EDMOTIV
         GRAVELEC ~ KNOWLEDG + IQPREVYR + EDMOTIV
         
         GRAVEREQ ~~ 0*GRAVELEC
         
'
fit1 = lavaan::cfa(model1, data= PredictG,likelihood = "wishart",
                   std.lv=T,fixed.x=F)
summary(fit1, standardized=TRUE,rsquare=T,fit.measures = TRUE)


model2 <- '
       
         GRAVEREQ ~ KNOWLEDG + IQPREVYR + EDMOTIV
         GRAVELEC ~ KNOWLEDG + IQPREVYR + EDMOTIV
         
         GRAVEREQ ~~ GRAVELEC
'
fit2 = lavaan::sem(model2,data = PredictG,std.lv=T,likelihood = "wishart")
summary(fit2, standardized=TRUE,rsquare=T,fit.measures = TRUE)




