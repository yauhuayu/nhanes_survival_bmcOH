# *****************************************************************************************
# September 2018
# 
# ** PUBLIC-USE LINKED MORTALITY FOLLOW-UP THROUGH DECEMBER 31, 2015 **
#
# The following R code can be used to read the fixed-width format ASCII public-use Linked
# Mortality Files (LMFs) from a stored location into a R data frame.  Basic frequencies
# are also produced.  
# 
# NOTE:   With the exception of linkage eligibility-status (ELIGSTAT), the other discrete
#         variables (e.g. MORTSTAT) are designated as integers.  We provide the definitions
#         of the variable values in the comments but leave it up to the user to decide 
#         whether integer or factor variables is/are preferred for their analyses.  
#
# 
# *****************************************************************************************   

# ** loop thru survey

library(readr)
library(dplyr)
library(data.table)

# the location where the .DAT file is saved:
#setwd("C:/PUBLIC USE DATA")
# remove all objects from the R environment
# rm(list=ls())


survey <- sprintf("NHANES_%d_%d", c(1999,2001,2003,2005,2007,2009,2011,2013),c(2000,2002,2004,2006,2008,2010,2012,2014))
srvyin <- paste0(survey,"_MORT_2015_PUBLIC.dat")  

srvyout <- paste0(survey,"_cnvt.txt")



for (i in 1:length(survey)) {

# read in the fixed-width format ASCII file
dsn <- read_fwf(file=srvyin[i],
                col_types = "ciiiiiiiddii",
                fwf_cols(publicid = c(1,14),
                         eligstat = c(15,15),
                         mortstat = c(16,16),
                         ucod_leading = c(17,19),
                         diabetes = c(20,20),
                         hyperten = c(21,21),
                         dodqtr = c(22,22),
                         dodyear = c(23,26),
                         wgt_new = c(27,34),
                         sa_wgt_new = c(35,42),
                         permth_int = c(43,45),
                         permth_exm = c(46,48)
                ),
                na = "."
)

# create the ID (SEQN) for the NHANES surveys
dsn$seqn <- substr(dsn$publicid,1,5)
# NOTE:   SEQN is the unique ID for NHANES.

#Drop NHIS variables
dsn <- select(dsn, -publicid)
dsn <- select(dsn, -dodqtr)
dsn <- select(dsn, -dodyear)
dsn <- select(dsn, -wgt_new)
dsn <- select(dsn, -sa_wgt_new)


# Structure and contents of data
str(dsn)


# Variable frequencies

#ELIGSTAT: Eligibility Status for Mortality Follow-up
table(dsn$eligstat)
#1 = "Eligible"
#2 = "Under age 18, not available for public release"
#3 = "Ineligible"

#MORTSTAT: Final Mortality Status
table(dsn$mortstat, useNA="ifany")
# 0 = Assumed alive
# 1 = Assumed deceased
# <NA> = Ineligible or under age 18

#UCOD_LEADING: Underlying Cause of Death: Recode
table(dsn$ucod_leading, useNA="ifany")
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
# <NA> = Ineligible, under age 18, assumed alive, or no cause of death data

#DIABETES: Diabetes Flag from Multiple Cause of Death (MCOD)
table(dsn$diabetes, useNA="ifany")
# 0 = No - Condition not listed as a multiple cause of death
# 1 = Yes - Condition listed as a multiple cause of death
# <NA> = Assumed alive, under age 18, ineligible for mortality follow-up, or MCOD not available

#HYPERTEN: Hypertension Flag from Multiple Cause of Death (MCOD)
table(dsn$hyperten, useNA="ifany")
# 0 = No - Condition not listed as a multiple cause of death
# 1 = Yes - Condition listed as a multiple cause of death
# <NA> = Assumed alive, under age 18, ineligible for mortality follow-up, or MCOD not available


write.table(dsn,srvyout[i],sep="\t",col.names=T,row.names=F,quote=F)

}


