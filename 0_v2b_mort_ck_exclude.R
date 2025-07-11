# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# * 1999-2014 
# ---------------------------------------
#
#   NoT and mortality / metabolic traits

# ~~~~ load required library ~~~~~~~

library(MASS)
library(dplyr)
library(survival)


# ~~~~ load func and global vars ~~~~~~~
source("0_a_func_tn_plot.R")

cvd.vars <- c("SEQN","gender","age","incPovR","educ3","race4","nvrsmk","currsmk","pastsmk","smk.cat3",
              "smkpkyrs","waist","bmi","htn2","pt.dm","tot.cvd","stroke","exercise","fmhxmi",
              "totchol","hdl","tg","ldl","h_chol","hba1c","LBXGLU","sbp","dbp")
dent.vars <- c("SEQN","ck.gr","tot.dtc","edentulous","ant.dtc","post.dtc","inc.dtc","can.dtc","pre.dtc","mol.dtc","dentvisit","tot.imp")
bmd.vars <- c("SEQN","hipFrc","wristFrc","spnFrc","LS","fneck","op_dx","op_tx") 


d.cvd <- read.table("d1_demo_smk_dm_cvd.txt",header=T,sep="\t")[,cvd.vars]
d.dent <- read.table("d2_9918_dent_ohq.txt",header=T,sep="\t")
d.mort <- read.table("d4_9914_mort.txt",header=T,sep="\t")                     # 82091
d.mort <- d.mort[!is.na(d.mort$mortstat),]                                     # 47279
d.bmd <- read.table("d3_9918_fem_bmd.txt",header=T,sep="\t")[,bmd.vars]
 
dim(d.cvd)        # 108060   25 
dim(d.dent)       # 82292    11 
dim(d.mort)       # 47279	 8                        # * there are 47279 with mortality data
dim(d.bmd)        # 24970    8                        # with valid femoral neck

d.use <- merge(d.mort,d.dent, by="SEQN",all.x=T,as.is=T,na=c("NA","."))        # 47279
d.use <- merge(d.use,d.cvd, by="SEQN",all.x=T,as.is=T,na=c("NA","."))          # 47279
d.use <- merge(d.use,d.bmd, by="SEQN",all.x=T,as.is=T,na=c("NA","."))          # 24970

# *** not using the fneck due to too limited sample size (and repetitive of what has already been published)
sum(is.na(d.use$fneck))                     # 29962
nrow(d.use)-sum(is.na(d.use$fneck)) 		# 17317

# *** 
sum(is.na(d.use$tot.dtc))                   # 7181 no dental examination
d.use.d <- d.use[!is.na(d.use$tot.dtc),]    # 40098
dim(d.use.d)

sum(d.use.d$tot.imp>0)                       # 418 / 40098 = 1.04%



# ***
mort.cvd <- rep(0,nrow(d.use))
mort.cvd[!is.na(d.use$mort_leading) & d.use$mort_leading==1] <- 1         # 859
mort.ca <- rep(0,nrow(d.use))
mort.ca[!is.na(d.use$mort_leading) & d.use$mort_leading==2] <- 1          # 1070
mort.accident <- rep(0,nrow(d.use))
mort.accident[!is.na(d.use$mort_leading) & d.use$mort_leading==4] <- 1    # 170
mort.stk <- rep(0,nrow(d.use.d))
mort.stk[!is.na(d.use$mort_leading) & d.use$mort_leading==5] <- 1         # 194
mort.dm <- rep(0,nrow(d.use))
mort.dm[!is.na(d.use$mort_leading) & d.use$mort_leading==6] <- 1          # 114

nu.mort <- cbind(mort.cvd,mort.ca,mort.accident,mort.stk,mort.dm)
#mort.vars <- colnames(nu.mort)[2:ncol(nu.mort)]
mort.vars <- c(colnames(nu.mort),"mort_diab","mort_htn","mortstat")

# ** choose the maximum surv time
mx.time <- apply(d.use[,c("permth_int","permth_exm")],1,max)


d.ck <- data.frame(cbind(d.use,nu.mort,mx.time))


# ** ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ **
dt.sel <- rep(1,nrow(d.ck))

sum(is.na(d.ck$tot.dtc))                     # 7181
dt.sel[is.na(d.ck$tot.dtc)] <- 0         
d.use2 <- d.ck[!is.na(d.ck$tot.dtc),]
dim(d.use2)                                  # 40098

sum(is.na(d.ck$pt.dm))                       # 21
dt.sel[is.na(d.ck$pt.dm)] <- 0         
d.use2 <- d.use2[!is.na(d.use2$pt.dm),]
dim(d.use2)                                  # 40077

sum(is.na(d.ck$tot.cvd))                      
sum(is.na(d.use2$tot.cvd))                   # 3541
dt.sel[is.na(d.ck$tot.cvd)] <- 0         
d.use2 <- d.use2[!is.na(d.use2$tot.cvd),]
dim(d.use2)                                  # 36536
      
sum(is.na(d.ck$htn2))                            
sum(is.na(d.use2$htn2))                      # 0
dt.sel[is.na(d.ck$htn2)] <- 0         
d.use2 <- d.use2[!is.na(d.use2$htn2),]
dim(d.use2)                                  

sum(is.na(d.ck$stroke))                      
sum(is.na(d.use2$stroke))                    # 35
dt.sel[is.na(d.ck$stroke)] <- 0         
d.use2 <- d.use2[!is.na(d.use2$stroke),]
dim(d.use2)                                  # 36501       

sum(is.na(d.ck$smk.cat3))                      
sum(is.na(d.use2$smk.cat3))                  # 28
dt.sel[is.na(d.ck$smk.cat3)] <- 0         
d.use2 <- d.use2[!is.na(d.use2$smk.cat3),]
dim(d.use2)                                  # 36473      

# gender, exercise no missing values

sum(is.na(d.ck$smk.cat3))                      
sum(is.na(d.use2$smk.cat3))                  # 28
dt.sel[is.na(d.ck$smk.cat3)] <- 0         
d.use2 <- d.use2[!is.na(d.use2$smk.cat3),]

sum(is.na(d.ck$bmi))                      
sum(is.na(d.use2$bmi))                       # 566
dt.sel[is.na(d.ck$bmi)] <- 0         
d.use2 <- d.use2[!is.na(d.use2$bmi),]

sum(is.na(d.ck$incPovR))                      
sum(is.na(d.use2$incPovR))                   # 2808
dt.sel[is.na(d.ck$incPovR)] <- 0         
d.use2 <- d.use2[!is.na(d.use2$incPovR),]

sum(is.na(d.ck$educ3))                      
sum(is.na(d.use2$educ3))                     # 28
dt.sel[is.na(d.ck$educ3)] <- 0         
d.use2 <- d.use2[!is.na(d.use2$educ3),]
dim(d.use2)                                  # 33071


ck.exclude <- data.frame(cbind(d.ck,dt.sel))  # 40098 --> 33071


# ** ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ **

i.ck.d <- rep(0,nrow(d.use))
i.ck.d[match(d.use$SEQN,d.use2$SEQN,nomatch=0)>0] <- 1

i.ck.dt <- data.frame(cbind(d.ck,i.ck.d))
ck.v.cont <- c("age","bmi","mx.time")
ck.v.cate <- c("gender","currsmk","pastsmk","nvrsmk","educ3","race4",mort.vars)
ck.v.tab <- table2.out("i.ck.d",i.ck.dt,ck.v.cont,ck.v.cate,2)
#write.table(ck.v.tab,"T0_exclude_2x.txt",sep="\t",quote=F,col.names=T,row.names=T)

