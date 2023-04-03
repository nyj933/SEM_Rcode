library(lavaan)
urlfile="https://raw.github.com/nyj933/SEM_Rcode/main/Examples-note6-Rcode/psavar.csv"
psavar = read.csv(urlfile)
psavar = psavar[,c(1:6)]

psavar[psavar==-9] = NA

####Linear Growth Curve for psavar Data
model1 <- '

           a =~ 1*PSA0 + 1*PSA3 + 1*PSA6 + 1*PSA9 + 1*PSA12
           b =~ 0*PSA0 + 3*PSA3 + 6*PSA6 + 9*PSA9 + 12*PSA12
           
           
           PSA0 ~~ v1*PSA0
           PSA3 ~~ v1*PSA3
           PSA6 ~~ v1*PSA6
           PSA9 ~~ v1*PSA9
           PSA12 ~~ v1*PSA12


'
fit1 <- growth(model1, data=psavar,missing="FIML", 
               likelihood = "wishart",
               #likelihood = "normal",
               meanstructure = T,
               information = "expected")

summary(fit1,standardized=TRUE,rsquare=T)


######## Linear Growth Curve with Covariate for psavar Data

model2 <- '

           a =~ 1*PSA0 + 1*PSA3 + 1*PSA6 + 1*PSA9 + 1*PSA12
           b =~ 0*PSA0 + 3*PSA3 + 6*PSA6 + 9*PSA9 + 12*PSA12
           
           a~ Age
           b ~ Age
           
           PSA0 ~~ v1*PSA0
           PSA3 ~~ v1*PSA3
           PSA6 ~~ v1*PSA6
           PSA9 ~~ v1*PSA9
           PSA12 ~~ v1*PSA12
           a ~~ b

           

'
fit2 <- growth(model2, data=psavar,missing="FIML", 
               likelihood = "wishart",
               ##likelihood = "normal" (for latent variable: std.err )
               meanstructure = T,
               information = "expected")

summary(fit2,standardized=TRUE,rsquare=T)
lavInspect(fit2,"implied")






