library(lavaan)
urlfile="https://raw.github.com/nyj933/SEM_Rcode/main/Examples-note6-Rcode/DYAD.csv"
dyad = read.csv(urlfile)
dyad = dyad[,c(1:10)]
dyad[dyad==-9] = NA

############ dyad1a
model1a <- '
          HInt =~ 1*HQUAL1 + 1*HQUAL2 + 1*HQUAL3 + 1*HQUAL4 + 1*HQUAL5
          HSlope =~ 0*HQUAL1 + 1*HQUAL2 + 2*HQUAL3 + 3*HQUAL4 + 4*HQUAL5
          WInt =~ 1*WQUAL1 + 1*WQUAL2 + 1*WQUAL3 + 1*WQUAL4 + 1*WQUAL5
          WSlope =~ 0*WQUAL1 + 1*WQUAL2 + 2*WQUAL3 + 3*WQUAL4 + 4*WQUAL5
          
          
          HQUAL1 ~~ a*HQUAL1
          HQUAL2 ~~ a*HQUAL2
          HQUAL3 ~~ a*HQUAL3
          HQUAL4 ~~ a*HQUAL4
          HQUAL5 ~~ a*HQUAL5
          
          WQUAL1 ~~ b*WQUAL1
          WQUAL2 ~~ b*WQUAL2
          WQUAL3 ~~ b*WQUAL3
          WQUAL4 ~~ b*WQUAL4
          WQUAL5 ~~ b*WQUAL5
          
          HInt ~~ 0*WInt
          HInt ~~ 0*WSlope
          HSlope ~~ 0*WInt
          HSlope ~~ 0*WSlope
          
'

fit1a <- lavaan::growth(model1a, data=dyad, ,missing="FIML",
               likelihood = "wishart", 
               meanstructure = T,
               information = "expected")
summary(fit1a,standardized=TRUE,rsquare=T)



########### dyad2a

model2a <- '
          HInt =~ 1*HQUAL1 + 1*HQUAL2 + 1*HQUAL3 + 1*HQUAL4 + 1*HQUAL5
          HSlope =~ 0*HQUAL1 + 1*HQUAL2 + 2*HQUAL3 + 3*HQUAL4 + 4*HQUAL5
          WInt =~ 1*WQUAL1 + 1*WQUAL2 + 1*WQUAL3 + 1*WQUAL4 + 1*WQUAL5
          WSlope =~ 0*WQUAL1 + 1*WQUAL2 + 2*WQUAL3 + 3*WQUAL4 + 4*WQUAL5
          
          HInt ~ c*1
          HSlope ~ 1
          WInt ~ c*1
          WSlope ~ 1
          
          HQUAL1 ~~ a*HQUAL1
          HQUAL2 ~~ a*HQUAL2
          HQUAL3 ~~ a*HQUAL3
          HQUAL4 ~~ a*HQUAL4
          HQUAL5 ~~ a*HQUAL5
          
          WQUAL1 ~~ b*WQUAL1
          WQUAL2 ~~ b*WQUAL2
          WQUAL3 ~~ b*WQUAL3
          WQUAL4 ~~ b*WQUAL4
          WQUAL5 ~~ b*WQUAL5
          
          HInt ~~ 0*WInt
          HInt ~~ 0*WSlope
          HSlope ~~ 0*WInt
          HSlope ~~ 0*WSlope
          

'

fit2a <- lavaan::growth(model2a, data=dyad, ,
                        missing="FIML",
                        likelihood = "wishart", 
                        meanstructure = T,
                        information = "expected")
summary(fit2a,standardized=TRUE,rsquare=T)

########### dyad3a

model3a <- '
          HInt =~ 1*HQUAL1 + 1*HQUAL2 + 1*HQUAL3 + 1*HQUAL4 + 1*HQUAL5
          HSlope =~ 0*HQUAL1 + 1*HQUAL2 + 2*HQUAL3 + 3*HQUAL4 + 4*HQUAL5
          WInt =~ 1*WQUAL1 + 1*WQUAL2 + 1*WQUAL3 + 1*WQUAL4 + 1*WQUAL5
          WSlope =~ 0*WQUAL1 + 1*WQUAL2 + 2*WQUAL3 + 3*WQUAL4 + 4*WQUAL5
          
          HInt ~ 1
          HSlope ~ c*1
          WInt ~ 1
          WSlope ~ c*1
          
          HQUAL1 ~~ a*HQUAL1
          HQUAL2 ~~ a*HQUAL2
          HQUAL3 ~~ a*HQUAL3
          HQUAL4 ~~ a*HQUAL4
          HQUAL5 ~~ a*HQUAL5
          
          WQUAL1 ~~ b*WQUAL1
          WQUAL2 ~~ b*WQUAL2
          WQUAL3 ~~ b*WQUAL3
          WQUAL4 ~~ b*WQUAL4
          WQUAL5 ~~ b*WQUAL5
          
          HInt ~~ 0*WInt
          HInt ~~ 0*WSlope
          HSlope ~~ 0*WInt
          HSlope ~~ 0*WSlope
          

'

fit3a <- lavaan::growth(model3a, data=dyad, ,
                        missing="FIML",
                        likelihood = "wishart", 
                        meanstructure = T,
                        information = "expected")
summary(fit3a,standardized=TRUE,rsquare=T)

########### dyad4a

model4a <- '
          HInt =~ 1*HQUAL1 + 1*HQUAL2 + 1*HQUAL3 + 1*HQUAL4 + 1*HQUAL5
          HSlope =~ 0*HQUAL1 + 1*HQUAL2 + 2*HQUAL3 + 3*HQUAL4 + 4*HQUAL5
          WInt =~ 1*WQUAL1 + 1*WQUAL2 + 1*WQUAL3 + 1*WQUAL4 + 1*WQUAL5
          WSlope =~ 0*WQUAL1 + 1*WQUAL2 + 2*WQUAL3 + 3*WQUAL4 + 4*WQUAL5
          
          
          HQUAL1 ~~ a*HQUAL1
          HQUAL2 ~~ a*HQUAL2
          HQUAL3 ~~ a*HQUAL3
          HQUAL4 ~~ a*HQUAL4
          HQUAL5 ~~ a*HQUAL5
          
          WQUAL1 ~~ b*WQUAL1
          WQUAL2 ~~ b*WQUAL2
          WQUAL3 ~~ b*WQUAL3
          WQUAL4 ~~ b*WQUAL4
          WQUAL5 ~~ b*WQUAL5
          
          HInt ~~ 0*WInt
          HInt ~~ 0*WSlope
          HSlope ~~ 0*WInt
          HSlope ~~ 0*WSlope
          
          HInt ~~ c*HInt
          WInt ~~ c*WInt
          

'
fit4a <- lavaan::growth(model4a, data=dyad, ,
                        missing="FIML",
                        likelihood = "wishart", 
                        meanstructure = T,
                        information = "expected")
summary(fit4a,standardized=TRUE,rsquare=T)

########### dyad5a
model5a <- '
          HInt =~ 1*HQUAL1 + 1*HQUAL2 + 1*HQUAL3 + 1*HQUAL4 + 1*HQUAL5
          HSlope =~ 0*HQUAL1 + 1*HQUAL2 + 2*HQUAL3 + 3*HQUAL4 + 4*HQUAL5
          WInt =~ 1*WQUAL1 + 1*WQUAL2 + 1*WQUAL3 + 1*WQUAL4 + 1*WQUAL5
          WSlope =~ 0*WQUAL1 + 1*WQUAL2 + 2*WQUAL3 + 3*WQUAL4 + 4*WQUAL5
          
          
          HQUAL1 ~~ a*HQUAL1
          HQUAL2 ~~ a*HQUAL2
          HQUAL3 ~~ a*HQUAL3
          HQUAL4 ~~ a*HQUAL4
          HQUAL5 ~~ a*HQUAL5
          
          WQUAL1 ~~ b*WQUAL1
          WQUAL2 ~~ b*WQUAL2
          WQUAL3 ~~ b*WQUAL3
          WQUAL4 ~~ b*WQUAL4
          WQUAL5 ~~ b*WQUAL5
          
          HInt ~~ 0*WInt
          HInt ~~ 0*WSlope
          HSlope ~~ 0*WInt
          HSlope ~~ 0*WSlope
          
          HSlope ~~ c*HSlope
          WSlope ~~ c*WSlope
          

'
fit5a <- lavaan::growth(model5a, data=dyad,,
                        missing="FIML",
                        likelihood = "wishart", 
                        meanstructure = T,
                        information = "expected")
summary(fit5a,standardized=TRUE,rsquare=T)


