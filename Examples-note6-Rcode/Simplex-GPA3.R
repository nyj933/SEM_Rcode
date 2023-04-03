
#SIMPLEX Model for GPA Model 3 - Testing stationarity 
library(lavaan)
library(tidyverse)

urlfile="https://raw.github.com/nyj933/SEM_Rcode/main/Examples-note6-Rcode/humphreys.csv"
dat = read.csv(urlfile)
dat <- dat[,1:10]

model <- ' 
  #### "=~" : measurement model
            gpa1 =~ 1*GPA1
            gpa2 =~ 1*GPA2
            gpa3 =~ 1*GPA3
            gpa4 =~ 1*GPA4
            gpa5 =~ 1*GPA5
            gpa6 =~ 1*GPA6
            gpa7 =~ 1*GPA7
            gpa8 =~ 1*GPA8
            Hscores =~ 1*HSR
            Hscores =~ ACT
    
    ### Regression
    
            gpa1 ~ Hscores
            gpa2 ~ b1*gpa1
            gpa3 ~ b1*gpa2
            gpa4 ~ b1*gpa3
            gpa5 ~ b1*gpa4
            gpa6 ~ b1*gpa5
            gpa7 ~ b1*gpa6
            gpa8 ~ b1*gpa7
            
    ### Variance
            
            GPA1 ~~ v1*GPA1
            GPA2 ~~ v2*GPA2
            GPA3 ~~ v3*GPA3
            GPA4 ~~ v4*GPA4
            GPA5 ~~ v5*GPA5
            GPA6 ~~ v6*GPA6
            GPA7 ~~ v7*GPA7
            GPA8 ~~ v8*GPA8
            gpa7 ~~ v9*gpa7
            gpa8 ~~ v9*gpa8
      
'

GPA.out <- lavaan(model, data = dat,auto.var = TRUE,likelihood = "wishart")
summary(GPA.out, fit.measures=TRUE, rsquare=T,standardized=T)







