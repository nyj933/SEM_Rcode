
##### Modified Model for Performance and Satisfaction
library(lavaan)

urlfile="https://raw.github.com/nyj933/SEM_Rcode/main/Examples-note4-Rcode/Bagizzi.DAT"
bagizzi <- readLines(urlfile)
lower_corr <- bagizzi[1:8]
corrmat <- getCov(lower_corr,names = c("PERFORMM", "JBSATIS1", "JBSATIS2", 
                                       "ACHMOT1", "ACHMOT2", 
                                       "T_S_S_E1", "T_S_S_E2", "VERBINTM"))
std <- as.numeric(unlist(strsplit(bagizzi[9], " ")))

#### BagozziA

model_A <- '
          Perform =~ PERFORMM
          Jobsatis =~ JBSATIS1 +JBSATIS2
          Achmot =~ ACHMOT1 + ACHMOT2 
          T_s_s_e =~ T_S_S_E1 + T_S_S_E2
          Verbint =~ VERBINTM
          
          Perform ~ T_s_s_e
          Jobsatis ~ Perform + Achmot + Verbint
          
          PERFORMM  ~~ PERFORMM  
          #VERBINTM  ~~ VERBINTM
        
 '
fit_A <- lavaan::sem(model_A, sample.cov = corrmat, sample.nobs = 122,
           likelihood = "wishart")
summary(fit_A, standardized=TRUE,rsquare=T,fit.measures=T)


######## BagozziB




model_B <- '
          Perform =~ PERFORMM
          Jobsatis =~ JBSATIS1 + JBSATIS2
          Achmot =~ ACHMOT1 + ACHMOT2 
          T_s_s_e =~ T_S_S_E1 + T_S_S_E2
          Verbint =~ VERBINTM
          
          Perform ~ T_s_s_e
          Jobsatis ~ Perform + Achmot + Verbint
          
          PERFORMM ~~ 0 * PERFORMM
          VERBINTM ~~ 1.998 * VERBINTM
          
          
          
 '
fit_B <- sem(model_B, sample.cov = corrmat, sample.nobs = 122,
              likelihood = "wishart")
summary(fit_B, standardized=TRUE,rsquare=T)




