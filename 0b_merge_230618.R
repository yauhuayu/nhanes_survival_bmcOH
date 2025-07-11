# *****************************************************************************************
# September 2018
# 
# ** PUBLIC-USE LINKED MORTALITY FOLLOW-UP THROUGH DECEMBER 31, 2015 **
#
# 
# *****************************************************************************************   

# ** loop thru survey

library(dplyr)
library(data.table)

survey <- sprintf("NHANES_%d_%d", c(1999,2001,2003,2005,2007,2009,2011,2013,2015,2017),c(2000,2002,2004,2006,2008,2010,2012,2014,2016,2018))
srvyin <- paste0(survey,"_cnvt.txt")

dt <- fread(srvyin[1],sep="\t",header=T)

for (i in 2:length(survey)) {
      tmp <- fread(srvyin[i],sep="\t",header=T)
      print(dim(tmp))
      print(length(unique(tmp$seqn)))
      dt <- rbind(dt,tmp)
}

dim(dt)           # 101316

# [1] "eligstat"     "mortstat"     "ucod_leading" "diabetes"     "hyperten"     "permth_int"  
# [7] "permth_exm"   "seqn"

colnames(dt) <- c("mort_elig","mortstat","mort_leading","mort_diab","mort_htn","permth_int","permth_exm","SEQN")


# Variable frequencies
# ---- mort_elig ----
#  1 = Eligible; 2 = under age 18, not available; 3 = Ineligible
table(dt$mort_elig)
#        1     	2     	3 
#		 59062 	42114   140 

# ---- mortstat ----  Final Mortality Status
#  0 = alive; 1 = assumed deceased; NA (ineligible or under age 18)
table(dt$mortstat, useNA="ifany")
#      0     1  <NA> 
#  52327  6735 42254 


# ---- mort_leading ----  Underlying Cause of Death
# 1 = Diseases of heart (I00-I09, I11, I13, I20-I51)
# 2 = Malignant neoplasms (C00-C97)
# 3 = Chronic lower respiratory diseases (J40-J47)
# 4 = Accidents (unintentional injuries) (V01-X59, Y85-Y86)
# 5 = Cerebrovascular diseases (I60-I69)
# 6 = Alzheimer's disease (G30)
# 7 = Diabetes mellitus (E10-E14)
# 8 = Influenza and pneumonia (J09-J18)
# 9 = Nephritis, nephrotic syndrome and nephrosis (N00-N07, N17-N19, N25-N27)
# 10 = All other causes (residual)
# NA = Ineligible, under age 18, assumed alive, or no cause of death data
table(dt$mort_leading, useNA="ifany")
#    1     2     3     4     5     6     7     8     9    10  <NA> 
# 1212  1433   245   195   261   149   161   117   114  2831 94598 
# sum = 6718

# ---- mort_diab ----  Diabetes Flag from Multiple Cause of Death (MCOD)
# 0 = No - Condition not listed as a multiple cause of death
# 1 = Yes - Condition listed as a multiple cause of death
# NA
table(dt$mort_diab, useNA="ifany")
#    0     1  <NA> 
# 5958   760 94598 

# ---- mort_htn ----  Hypertension Flag from Multiple Cause of Death (MCOD)
# 0 = No; 1 = Yes; NA
table(dt$mort_htn, useNA="ifany")
#    0     1  <NA> 
# 5726   992 94598 



write.table(dt,"d4_9918_mort.txt",sep="\t",col.names=T,row.names=F,quote=F)



