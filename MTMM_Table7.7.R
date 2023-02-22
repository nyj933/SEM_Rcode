
library(lavaan)
library(tidyverse)

### input: correlation matrix
## 4 traits and 3 methods
## 12 variables: X11 X12 X13 X21 X22 X23 X31 X32 X33 X41 X42 X43

lower <- '
1.0
.479 1.0   
.462 .467 1.0 
.409 .292 .265 1.0 
.330 .382 .298 .763 1.0 
.284 .335 .368 .662 .690 1.0  
.467 .395 .324 .317 .371 .255 1.0 
.333 .508 .321 .244 .403 .268 .711 1.0  
.272 .419 .406 .216 .312 .335 .675 .663 1.0
.490 .371 .337 .353 .268 .210 .323 .207 .185 1.0
.337 .468 .342 .210 .281 .195 .227 .281 .231 .654 1.0
.276 .366 .424 .163 .187 .230 .176 .111 .238 .604 .563 1.0

'
class(cormat)
## X21=T1M1, X22=T1M2, X23=T1M3......

cormat <- getCov(lower,  names=c("T4M1", "T4M2", "T4M3", 
                                 "T1M1", "T1M2", "T1M3", 
                                 "T2M1", "T2M2", "T2M3", 
                                 "T3M1", "T3M2", "T3M3"))

### 9 variables are used in the model (3 traits and 3 methods)
## X21 X22 X23 X31 X32 X33 X41 X42 X43
model <- ' 
           T1  =~ T1M1 + T1M2 + T1M3
           T2  =~ T2M1 + T2M2 + T2M3
           T3  =~ T3M1 + T3M2 + T3M3
           
           M1  =~  T1M1 + T2M1 + T3M1
           M2  =~  T1M2 + T2M2 + T3M2
           M3  =~  T1M3 + T2M3 + T3M3

           # uncorrelated terms
           ## let T1-T3 and M1-M3 be uncorrelated
           T1~~0*M1 
           T1~~0*M2
           T1~~0*M3
           T2~~0*M1
           T2~~0*M2
           T2~~0*M3
           T3~~0*M1
           T3~~0*M2
           T3~~0*M3
           ## M1-M3 are uncorrelated
           M1~~0*M2 
           M1~~0*M3
           M2~~0*M3
           
           
'



MTMM.out <- cfa(model, sample.cov = cormat, sample.nobs = 400, std.lv=TRUE,likelihood = "wishart")
summary(MTMM.out, fit.measures=TRUE,rsquare=T)

### Duplicate Table 7.7

library(htmlTable)
library(magrittr)


table7.7 <- sprintf("%.3f", round(cbind(output$PE$est[1:9],output$PE$est[c(10,13,16,11,14,17,12,15,18)],c(0.773,0.849,0.684,
                                      0.773,0.795,0.765,
                                      0.981,0.657,0.583),
      output$PE$est[1:9]^2,output$PE$est[c(10,13,16,11,14,17,12,15,18)]^2, output$PE$std.lv[31:39]),3))


table7.7 %>% 
  htmlTable(ctable = c("double"),
            header =  paste(c("Traits","Methods","Reliability",
                              "Traits","Methods","Error")),
            rnames = paste(c("T1M1", "T1M2", "T1M3", 
                             "T2M1", "T2M2", "T2M3", 
                             "T3M1", "T3M2", "T3M3")),
            cgroup = c("Factor Loadings and R^2", "Variance Components"),
            n.cgroup = c(3, 3),
            css.cell = c("width: 75;","width: 75;","width: 0;","width: 75;","width: 75;","width: 75;")) 





#################################
#### replace T1-T3 by House, Income, and contacts 
#### replace M1-M3 by 10points, 100points,Category

cormat <- getCov(lower,  names=c("Gw10pts", "Gw100pts", "GwCat",  
                                 "Hw10pts", "Hw100pts", "HwCat", 
                                 "Iw10pt", "Iw100pts", "IwCat", 
                                 "Cw10pt", "Cw100pts", "CwCat"))

### 9 variables are used in the model (3 traits and 3 methods)

model <- ' 
           House  =~ Hw10pts + Hw100pts + HwCat
           Income  =~ Iw10pt + Iw100pts + IwCat
           Contacts  =~ Cw10pt + Cw100pts + CwCat
           
           points10  =~  Hw10pts + Iw10pt + Cw10pt
           points100  =~  Hw100pts + Iw100pts + Cw100pts
           Category  =~  HwCat + IwCat + CwCat

           # uncorrelated terms
           ## let House-Contacts and 10points-Category be uncorrelated
           House~~0*points10
           House~~0*points100
           House~~0*Category
           Income~~0*points10
           Income~~0*points100
           Income~~0*Category
           Contacts~~0*points10
           Contacts~~0*points100
           Contacts~~0*Category
           ## 10points-Category are uncorrelated
           points10~~0*points100
           points10~~0*Category
           points100~~0*Category
           
           
'


MTMM.out <- cfa(model, sample.cov = cormat, sample.nobs = 400, std.lv=TRUE,likelihood = "wishart")
output = summary(MTMM.out, fit.measures=TRUE,rsquare=T, standardized=TRUE)

### Duplicate Table 7.7

table7.7 <- sprintf("%.3f", round(cbind(output$PE$est[1:9],
                                        output$PE$est[c(10,13,16,11,14,17,12,15,18)],
                                        c(0.773,0.849,0.684,
                                          0.773,0.795,0.765,
                                          0.981,0.657,0.583),
                                        output$PE$est[1:9]^2,output$PE$est[c(10,13,16,11,14,17,12,15,18)]^2, 
                                        output$PE$std.lv[31:39]),3))

table7.7 %>% 
  htmlTable(header =  paste(c("Traits","Methods","Reliability",
                              "Traits","Methods","Error")),
            rnames = paste(c("Housew10pts", "Housew100pts", "HousewCat", 
                             "Incomew10pt", "Incomew100pts", "IncomewCat", 
                             "Contactsw10pt", "Contactsw100pts", "ContactswCat")),
            cgroup = c("Factor Loadings and R^2", "Variance Components"),
            n.cgroup = c(3, 3),
            css.cell = c("width: 75;","width: 75;","width: 0;","width: 75;","width: 75;","width: 75;")) 

#install.packages("tidyverse",type = "binary")
