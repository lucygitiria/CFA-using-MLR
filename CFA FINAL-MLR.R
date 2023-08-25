##  PACKAGES NEEDED
################################################################################
library(readxl)
library(lavaan)
library(psych)
library(tidyverse)
library(polycor)
library(kableExtra)
library(apaTables)
library(corrplot)
library(semTools) # for additional functions in SEM
library(semPlot) # for path diagram
################################################################################
#LOAD DATA
################################################################################
getwd()
setwd("D:/MA ED Measurement Evaluation and Assessment")
NSD2687_1_no_1 <- read_excel("THESIS 2022/R scripts/NSD2687-1-no 1.xlsx")
#View(NSD2687_1_no_1)

#DATA PREPARATION
Data<-NSD2687_1_no_1
preli.df<-Data[c(6,13,14,93:102)]
apply(preli.df[1:13],2,table,exclude=NULL)#A glance at the subset data
preli.df[1:13][preli.df[1:13]==9999]<-NA # change 9999 to NAs
preli.df[1:13][preli.df[1:13]==999]<-NA # change 999 to NAs

#  SUBSET 6 STUDY PROGRAMS
AKADM<-preli.df[preli.df$Utd_type == 'Ã˜KADM',]   # business and adminstration-Enterprising /Ã˜KADM/
DATAIT<-preli.df[preli.df$Utd_type == 'DATA-IT',] # Conventional
KUNST<-preli.df[preli.df$Utd_type == 'KUNST',]    # Artistic
SIVING<-preli.df[preli.df$Utd_type == 'SIVING',]  # Investigative
SYKEPLEIE<-preli.df[preli.df$Utd_type == 'SYKEPLEIE',] #Social
TEKN.FAG<-preli.df[preli.df$Utd_type == 'TEKN-FAG',]   #Realistic

# New Dataset
#Put all data frames into list
list.dfprel <- list(SIVING, TEKN.FAG, KUNST,SYKEPLEIE,AKADM,DATAIT)

#Merge all data frames in list
ANALYSIS.DATApreli<-Reduce(function(x, y) merge(x, y, all=TRUE), list.dfprel)
#Remove NAs
DF.ANALYSIS<-ANALYSIS.DATApreli[complete.cases(ANALYSIS.DATApreli[1:13]),]

#Subset LO items
items_data<-DF.ANALYSIS[c(4:13)]

## CFA Data
CFA_DATA<-items_data
################################################################################
## STEP 1. DATA SCREENING
################################################################################
describe(DF.ANALYSIS) #Descriptive statistics

describe(CFA_DATA)

#*Multivariate statistics*
  
mardia(CFA_DATA) #multivariate skweness and kurtosis

#b1p =  5.89   skew =  6628.6  with probability  <=  0
#b2p =  156.93   kurtosis =  97.98  with probability <=  0
#Data has a non normal distribution. Thus use robust estimator
#Fit CFA using Robust ML (MLR)**
#We will use MLR in the analysis- . MLR (robust ML), suitable for complete and 
#incomplete, non-normal data (Rosseel, 2018).

################################################################################```
# **STEP 2 SPECIFY AND FIT CFA MODELS**
################################################################################```
#One factor- Using MLR
unimodel.A <- "Learning_Outcomes =~ Laerutb_teori_13 + Laerutb_metforsk_13+
Laerutb_egenerf_13+Laerutb_fagspes_13+Laerutb_refleks_13 + Laerutb_samarb_13 + 
Laerutb_muntkom_13 + Laerutb_skriftkom_13+Laerutb_tenke_13+Laerutb_selvst_13"

unimodel.A.fit = lavaan::cfa(unimodel.A, data = CFA_DATA, estimator = "MLR")

summary(unimodel.A.fit, fit.measures = T, standardized = T)
fitMeasures(unimodel.A.fit, c("gfi","cfi","tli","rmsea","srmr"))

#Poor fit of the model to data.

###############################################################################
##STEP 3.** MODEL REVISION**
################################################################################```

##Local fit indices**

#UnStandardized Residual matrix
lavResiduals(unimodel.A.fit)# "cor.bentler" table-cases of above 0.1

#mi
modindices(unimodel.A.fit)

#suggested adjustments
#Laerutb_metforsk_13	~~	Laerutb_egenerf_13	2199.639
#Laerutb_samarb_13	~~	Laerutb_muntkom_13	811.174	
#Laerutb_metforsk_13- has problems with many items

#Revision 1
unimodel.B <- "Learning_Outcomes =~ Laerutb_teori_13 + Laerutb_metforsk_13+
Laerutb_egenerf_13+Laerutb_fagspes_13+Laerutb_refleks_13 + Laerutb_samarb_13 + 
Laerutb_muntkom_13 + Laerutb_skriftkom_13+Laerutb_tenke_13+Laerutb_selvst_13
#Covariances
Laerutb_metforsk_13	~~	Laerutb_egenerf_13"

unimodel.B.fit = lavaan::cfa(unimodel.B, data = CFA_DATA, estimator = "MLR")

summary(unimodel.B.fit, fit.measures = T, standardized = T)
fitMeasures(unimodel.B.fit, c("gfi","cfi","tli","rmsea","srmr"))

#Fit is still marginal

#Revision 2
unimodel.C <- "Learning_Outcomes =~ Laerutb_teori_13 + Laerutb_metforsk_13+
Laerutb_egenerf_13+Laerutb_fagspes_13+Laerutb_refleks_13 + Laerutb_samarb_13 + 
Laerutb_muntkom_13 + Laerutb_skriftkom_13+Laerutb_tenke_13+Laerutb_selvst_13
#Covariances
Laerutb_metforsk_13	~~	Laerutb_egenerf_13
Laerutb_samarb_13	~~	Laerutb_muntkom_13"

unimodel.C.fit = lavaan::cfa(unimodel.C, data = CFA_DATA, estimator = "MLR")

summary(unimodel.C.fit, fit.measures = T, standardized = T)
fitMeasures(unimodel.C.fit, c("gfi","cfi","tli","rmsea","srmr"))

#Check again the Local fit indices	
#UnStandardized Residual matrix
lavResiduals(unimodel.C.fit)# "cor.bentler" table-cases of above 0.1

#mi
modindices(unimodel.C.fit)

#Plot the path Diagram

semPaths(unimodel.C.fit, 'path', 'std', style = 'lisrel',
         edge.color = 'black', intercepts = F)

#**TRY REMOVING METFORSK and covary Muntkom and samarbeid- using MLR Estimator

mlr.data<-CFA_DATA[c(1,3:10)] #9 items

unimodel_MLR1 <- "Learning_Outcomes =~ Laerutb_teori_13+
Laerutb_egenerf_13+Laerutb_fagspes_13+Laerutb_refleks_13 + Laerutb_samarb_13 + 
Laerutb_muntkom_13 + Laerutb_skriftkom_13+Laerutb_tenke_13+Laerutb_selvst_13
#covariance
Laerutb_samarb_13~~Laerutb_muntkom_13"

unimodel_MLR1_fit <- lavaan::cfa(unimodel_MLR1, mlr.data,estimator = "MLR")       

summary(unimodel_MLR1_fit, fit.measures = T, standardized = T)
fitMeasures(unimodel_MLR1_fit, c("gfi","cfi","tli","rmsea","srmr"))

#Check the Local fit indices
#UnStandardized Residual matrix
lavResiduals(unimodel_MLR1_fit)# "cor.bentler" table-cases of above 0.1

#Residuals OK. no cases above 0.1
#RMSEA still at the margin. 

#Plot the path Diagram
semPaths(unimodel_MLR1_fit, 'path', 'std', style = 'lisrel',
         edge.color = 'black', intercepts = F)


##  **TWO FACTOR MODEL**

#Two factor (10 items)
Model2F_MLR <- "Skills Achievement =~ Laerutb_samarb_13+Laerutb_muntkom_13+
                                     Laerutb_refleks_13 + Laerutb_tenke_13+
                                     Laerutb_skriftkom_13+Laerutb_selvst_13+
                                     Laerutb_fagspes_13
               Knowledge Achievement =~ Laerutb_teori_13 +Laerutb_metforsk_13+
                                     Laerutb_egenerf_13 "

Model2F_MLR_fit <-lavaan:: cfa(unimodel_MLR1, CFA_DATA,estimator = "MLR")

summary(Model2F_MLR_fit, standardized = TRUE, fit.measures = TRUE,rsquare = TRUE)
fitMeasures(Model2F_MLR_fit, c("gfi","cfi","tli","rmsea","srmr"))

fitMeasures(Model2F_MLR_fit, c("gfi","cfi","tli","rmsea","srmr"))

#RMSEA(0.085) is beyond the cutpoint.

#mi
modindices(Model2F_MLR_fit)

#UnStandardized Residual matrix
lavResiduals(unimodel.C.fit)# "cor.bentler" table-cases of above 0.1
#None of adjustments proposed adjustments can be implemented.
################################################################################```

