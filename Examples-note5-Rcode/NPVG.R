library(lavaan)
library(readr)

urlfile="https://raw.github.com/nyj933/SEM_Rcode/main/Examples-note5-Rcode/NPVG(R).DAT"
npvg = read_lines(urlfile)

readCorrMatrix <- function(data){
  j=0
  name=c()
  output=list()
  for (i in 1:length(data)){
    if (grepl("Correlations", data[i], fixed=TRUE)){
      j = j+1
      corr = getCov(data[(i+1):(i+2)],names = c("VISPERC","CUBES","PAPFORM",
                                             "GENINFO", "SENTCOM", "WORDCLAS",
                                             "FIGREC", "OBJNUM", "NUMFIG"))
      
      name = c(name,data[i]) 
      output[[j]] = corr/100
    }
    names(output) = name
  }
 return(output)
}

readCovMatrix <- function(data,cormat){
  cor2cov <- function(R, S) {
    sweep(sweep(R, 1, S, "*"), 2, S, "*")
  }
  j=0
  name=c()
  output=list()
  for (i in 1:length(data)){
    if (grepl("deviations", data[i], fixed=TRUE)){
      j = j+1
      std = data[(i+1)]
      name = c(name,data[i]) 
      std_split = unlist(strsplit(std, " "))
      output[[j]] = round(cor2cov(cormat[[j]],as.numeric(std_split)/100),3) 
    names(output) = name
  }}
  return(output)
  }
  
  

readMeanMatrix <- function(data){
  j=0
  name=c()
  output=list()
  for (i in 1:length(data)){
    if (grepl("Means", data[i], fixed=TRUE)){
      j = j+1
      mean = data[(i+1)]
      name = c(name,data[i]) 
      mean_split = unlist(strsplit(mean, " "))
      output[[j]] = as.numeric(mean_split)/100
    }
    names(output) = name
  }
  return(output)
}
cormat = readCorrMatrix(npvg)
mean = readMeanMatrix(npvg)
covmat = readCovMatrix(npvg,cormat)

model <- '
           Space =~ VISPERC + CUBES +  PAPFORM + FIGREC
           Verbal =~ GENINFO + SENTCOM + WORDCLAS
           Memory =~ FIGREC + OBJNUM + NUMFIG
           
           Space ~~ Space
           Verbal ~~ Verbal
           Memory ~~ Memory
           
           Space ~~ Verbal
           Verbal ~~ Memory
           Memory ~~ Space
           
           
  
'

fit <- lavaan::cfa(model, sample.cov = covmat, sample.nobs = c(77,79,74,71),
             sample.mean = mean,likelihood = "wishart", 
             group.equal = c("residuals",
                             "intercepts",
                             "loadings"))
summary(fit, standardized=TRUE,rsquare=T)





