library(lavaan)

urlfile="https://raw.github.com/nyj933/SEM_Rcode/main/Examples-note8-Rcode/jsp_aggregated.csv"
jsp = read.csv(urlfile)
jsp <- jsp[,c(1:6)]
jsp[jsp==-9] = NA


model1a <- '

             Math =~  math1 + math2 + math3
             Eng =~  eng1 + eng2 + eng3

'
fit1 <- lavaan::cfa(model1a, data=jsp,missing="FIML", 
               likelihood = "wishart",
               std.lv=T)
summary(fit1,standardized=TRUE,rsquare=T)

model2a <- '

             Math =~  math1 + math2 + math3
             Eng =~  eng1 + eng2 + eng3
             
             math1 ~~ eng1
             math2 ~~ math1
             math3 ~~ math2
             
             eng2 ~~ eng1
             eng3 ~~ eng2
          
'
fit2 <- lavaan::cfa(model2a, data=jsp,missing="FIML", 
                    likelihood = "wishart",
                    std.lv=T)
summary(fit2,standardized=TRUE,rsquare=T)



