
### Estimating MTMM model by Diagonally Weighted Least Squares

library(lavaan)
urlfile="https://raw.github.com/nyj933/SEM_Rcode/main/Examples-note4-Rcode/leader1.txt"
leader1 <- read.csv(urlfile,sep = "")

###### leader 2 

model2 <-'
           SELF =~ SELFtas + SELFdev + SELFrol
           SUP =~ SUPtas + SUPdev + SUProl
           SUB =~ SUBtas + SUBdev + SUBrol
           tas =~ SELFtas + SUPtas + SUBtas
           dev =~ SELFdev + SUPdev + SUBdev
           rol =~ SELFrol + SUProl + SUBrol
           
           SELF ~~ 0 * tas
           SELF ~~ 0 * dev
           SELF ~~ 0 * rol
           SUP ~~ 0 * tas
           SUP ~~ 0 * dev
           SUP ~~ 0 * rol
           SUB ~~ 0 * tas
           SUB ~~ 0 * dev
           SUB ~~ 0 * rol
           

'
fit2 <- lavaan::sem(data = leader1, model = model2, 
                    ordered = colnames(leader1), 
                    estimator = 'WLSMV',
                    std.lv=T)
summary(fit2,standardized=TRUE,rsquare=T,fit.measures=T)



