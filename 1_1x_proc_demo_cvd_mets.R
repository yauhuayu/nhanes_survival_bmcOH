
library(foreign)

dir.name <- "./1999-2008/"
dir.2 <- "./2009-2018/"

# ---------------------------------------------------------------------------------------
# ** BMI **                                                                                                ------  ** BMI ** ------
#
# BMDSTATS - Body Measures Component Status Code
# BMXWT - Weight (kg)
# BMXHT - Standing Height (cm)
# BMXWAIST - Waist Circumference (cm)
# BMXBMI - Body Mass Index (kg/m**2)
# ** create bmi category yourself 
#
# ----------------------------------------------------------------------------------------

bmx.vars <- c("BMXWT","BMXHT","BMXWAIST","BMXBMI")
bmi.vars <- c("weight","height","waist","bmi")

bmx.d1 <- read.xport(sprintf("%sBMX.xpt",dir.name))[,c("SEQN",bmx.vars)]
bmx.d2 <- read.xport(sprintf("%sBMX_B.xpt",dir.name))[,c("SEQN",bmx.vars)]
bmx.d3 <- read.xport(sprintf("%sBMX_C.xpt",dir.name))[,c("SEQN",bmx.vars)]
bmx.d4 <- read.xport(sprintf("%sBMX_D.xpt",dir.name))[,c("SEQN",bmx.vars)]
bmx.d5 <- read.xport(sprintf("%sBMX_E.xpt",dir.name))[,c("SEQN",bmx.vars)]
bmx.d6 <- read.xport(sprintf("%sBMX_F.xpt",dir.2))[,c("SEQN",bmx.vars)]
bmx.d7 <- read.xport(sprintf("%sBMX_G.xpt",dir.2))[,c("SEQN",bmx.vars)]
bmx.d8 <- read.xport(sprintf("%sBMX_H.xpt",dir.2))[,c("SEQN",bmx.vars)]
bmx.d9 <- read.xport(sprintf("%sBMX_I.xpt",dir.2))[,c("SEQN",bmx.vars)]
bmx.d10 <- read.xport(sprintf("%sBMX_J.xpt",dir.2))[,c("SEQN",bmx.vars)]
d.bmx <- rbind(bmx.d1,bmx.d2,bmx.d3,bmx.d4,bmx.d5,bmx.d6,bmx.d7,bmx.d8,bmx.d9,bmx.d10)
colnames(d.bmx) <- c("SEQN",bmi.vars)


# ---------------------------------------------------------------------------------------
# ** CVD **                                                                                                  ------  ** CVD ** ------
#
#	•	MCQ300A - Close relative had heart attack?
#       MCQ260GA - Blood relative-heart attack-mother; MCQ260GB - Blood relative-heart attack-father
#	•	MCQ160B - Ever told had congestive heart failure
#	•	MCQ160C - Ever told you had coronary heart disease
#	•	MCQ160D - Ever told you had angina/angina pectoris
#	•	MCQ160E - Ever told you had heart attack
#	•	MCQ160F - Ever told you had a stroke
#
#   do i need to add data of stroke?
# ---------------------------------------------------------------------------------------

mcq1.vars <- c("MCQ260GA","MCQ260GB","MCQ160B","MCQ160C","MCQ160D","MCQ160E","MCQ160F")
mcq2.vars <- c("MCQ300A","MCQ160B","MCQ160C","MCQ160D","MCQ160E","MCQ160F")
cvd.vars <- c("fmhxmi","hfailure","CHD","angina","hattack","stroke")

mcq.d1 <- read.xport(sprintf("%sMCQ.xpt",dir.name))[,c("SEQN",mcq1.vars)]
mcq.d2 <- read.xport(sprintf("%sMCQ_B.xpt",dir.name))[,c("SEQN",mcq1.vars)]
mcq.d3 <- read.xport(sprintf("%sMCQ_C.xpt",dir.name))[,c("SEQN",mcq1.vars)]
mcq.d4 <- read.xport(sprintf("%sMCQ_D.xpt",dir.name))[,c("SEQN",mcq2.vars)]
mcq.d5 <- read.xport(sprintf("%sMCQ_E.xpt",dir.name))[,c("SEQN",mcq2.vars)]
mcq.d6 <- read.xport(sprintf("%sMCQ_F.xpt",dir.2))[,c("SEQN",mcq2.vars)]
mcq.d7 <- read.xport(sprintf("%sMCQ_G.xpt",dir.2))[,c("SEQN",mcq2.vars)]
mcq.d8 <- read.xport(sprintf("%sMCQ_H.xpt",dir.2))[,c("SEQN",mcq2.vars)]
mcq.d9 <- read.xport(sprintf("%sMCQ_I.xpt",dir.2))[,c("SEQN",mcq2.vars)]
mcq.d10 <- read.xport(sprintf("%sMCQ_J.xpt",dir.2))[,c("SEQN",mcq2.vars)]

d.mcq1 <- rbind(mcq.d1,mcq.d2,mcq.d3)
fmhxcvd <- d.mcq1[,2]
momcvd <- d.mcq1[,3]
fmhxcvd[fmhxcvd==99] <- NA
fmhxcvd[momcvd>0] <- 1
d.mcq1 <- cbind(d.mcq1[,1],fmhxcvd,d.mcq1[,4:8])
colnames(d.mcq1) <- c("SEQN",cvd.vars)
d.mcq2 <- rbind(mcq.d4,mcq.d5,mcq.d6,mcq.d7,mcq.d8,mcq.d9,mcq.d10)
colnames(d.mcq2) <- c("SEQN",cvd.vars)
d.mcq <- rbind(d.mcq1,d.mcq2)

tmp <- d.mcq[,c(2:7)] 
tmp[tmp==2] <- 0
tmp[tmp==7] <- NA
tmp[tmp==9] <- NA
d.mcq <- cbind(d.mcq[,1],tmp)
tot.cvd <- apply(d.mcq[,3:6],1,sum)
tot.cvd[tot.cvd>1] <- 1
colnames(d.mcq) <- c("SEQN",cvd.vars)
d.mcq <- cbind(d.mcq,tot.cvd)


# ===== merge =====                                                                                    ------~~~ cvd.dt ~~~------
cvd.dt <- merge(d.mcq,d.bmx,by="SEQN",all.x=T,as.is=T,na=c("NA","."))
dim(cvd.dt)                                 															   # 96811 12



# ---------------------------------------------------------------------------------------
# ** lipid ** 														             ---- ** lipid ** ----
# 
# LBXTC - Total Cholesterol( mg/dL)
# LBDHDD - Direct HDL-Cholesterol (mg/dL)
# LBDHDL - (1999-2002) LBXHDD (2003-2004)
# LBXTR - Triglyceride (mg/dL)
# LBDLDL - LDL-cholesterol (mg/dL)
#            
# ---------------------------------------------------------------------------------------

lipid.vars <- c("totchol","hdl","tg","ldl")

tchdl.d1 <- read.xport(sprintf("%sLAB13.xpt",dir.name))[,c("SEQN","LBXTC","LBDHDL")]
trldl.d1 <- read.xport(sprintf("%sLAB13AM.xpt",dir.name))[,c("SEQN","LBXTR","LBDLDL")]
lipid.d1 <- merge(tchdl.d1,trldl.d1,by="SEQN",all.x=T,as.is=T)
tchdl.d2 <- read.xport(sprintf("%sL13_B.xpt",dir.name))[,c("SEQN","LBXTC","LBDHDL")]
trldl.d2 <- read.xport(sprintf("%sL13AM_B.xpt",dir.name))[,c("SEQN","LBXTR","LBDLDL")]
lipid.d2 <- merge(tchdl.d2,trldl.d2,by="SEQN",all.x=T,as.is=T)
tchdl.d3 <- read.xport(sprintf("%sL13_C.xpt",dir.name))[,c("SEQN","LBXTC","LBXHDD")]
trldl.d3 <- read.xport(sprintf("%sL13AM_C.xpt",dir.name))[,c("SEQN","LBXTR","LBDLDL")]
lipid.d3 <- merge(tchdl.d3,trldl.d3,by="SEQN",all.x=T,as.is=T)
colnames(lipid.d3) <- colnames(lipid.d2)
d.lipid1 <- rbind(lipid.d1,lipid.d2,lipid.d3)
colnames(d.lipid1) <- c("SEQN",lipid.vars)

tc.d4 <- read.xport(sprintf("%sTCHOL_D.xpt",dir.name))[,c("SEQN","LBXTC")]
tc.d5 <- read.xport(sprintf("%sTCHOL_E.xpt",dir.name))[,c("SEQN","LBXTC")]
tc.d6 <- read.xport(sprintf("%sTCHOL_F.xpt",dir.2))[,c("SEQN","LBXTC")]
tc.d7 <- read.xport(sprintf("%sTCHOL_G.xpt",dir.2))[,c("SEQN","LBXTC")]
tc.d8 <- read.xport(sprintf("%sTCHOL_H.xpt",dir.2))[,c("SEQN","LBXTC")]
tc.d9 <- read.xport(sprintf("%sTCHOL_I.xpt",dir.2))[,c("SEQN","LBXTC")]
tc.d10 <- read.xport(sprintf("%sTCHOL_J.xpt",dir.2))[,c("SEQN","LBXTC")]
tc.dt <- rbind(tc.d4,tc.d5,tc.d6,tc.d7,tc.d8,tc.d9,tc.d10)
hdl.d4 <- read.xport(sprintf("%sHDL_D.xpt",dir.name))[,c("SEQN","LBDHDD")]
hdl.d5 <- read.xport(sprintf("%sHDL_E.xpt",dir.name))[,c("SEQN","LBDHDD")]
hdl.d6 <- read.xport(sprintf("%sHDL_F.xpt",dir.2))[,c("SEQN","LBDHDD")]
hdl.d7 <- read.xport(sprintf("%sHDL_G.xpt",dir.2))[,c("SEQN","LBDHDD")]
hdl.d8 <- read.xport(sprintf("%sHDL_H.xpt",dir.2))[,c("SEQN","LBDHDD")]
hdl.d9 <- read.xport(sprintf("%sHDL_I.xpt",dir.2))[,c("SEQN","LBDHDD")]
hdl.d10 <- read.xport(sprintf("%sHDL_J.xpt",dir.2))[,c("SEQN","LBDHDD")]
hdl.dt <- rbind(hdl.d4,hdl.d5,hdl.d6,hdl.d7,hdl.d8,hdl.d9,hdl.d10)
trldl.d4 <- read.xport(sprintf("%sTRIGLY_D.xpt",dir.name))[,c("SEQN","LBXTR","LBDLDL")]
trldl.d5 <- read.xport(sprintf("%sTRIGLY_E.xpt",dir.name))[,c("SEQN","LBXTR","LBDLDL")]
trldl.d6 <- read.xport(sprintf("%sTRIGLY_F.xpt",dir.2))[,c("SEQN","LBXTR","LBDLDL")]
trldl.d7 <- read.xport(sprintf("%sTRIGLY_G.xpt",dir.2))[,c("SEQN","LBXTR","LBDLDL")]
trldl.d8 <- read.xport(sprintf("%sTRIGLY_H.xpt",dir.2))[,c("SEQN","LBXTR","LBDLDL")]
trldl.d9 <- read.xport(sprintf("%sTRIGLY_I.xpt",dir.2))[,c("SEQN","LBXTR","LBDLDL")]
trldl.d10 <- read.xport(sprintf("%sTRIGLY_J.xpt",dir.2))[,c("SEQN","LBXTR","LBDLDL")]
trldl.dt <- rbind(trldl.d4,trldl.d5,trldl.d6,trldl.d7,trldl.d8,trldl.d9,trldl.d10)
d.lipid2 <- merge(tc.dt,hdl.dt,by="SEQN",all.x=T,as.is=T)
d.lipid2 <- merge(d.lipid2,trldl.dt,by="SEQN",all.x=T,as.is=T)
colnames(d.lipid2) <- c("SEQN",lipid.vars)


# ===== lipid =====                                                                                    ------~~~ lipid.dt ~~~------
lipid.dt <- rbind(d.lipid1,d.lipid2)     

# ** create vars **
h_chol <- rep(0,nrow(lipid.dt))
h_chol[lipid.dt $tot.chol>=240] <- 1
h_chol[is.na(lipid.dt $tot.chol)] <- NA     
h_chol[lipid.dt $hdl<40] <- 1  
h_chol[lipid.dt $tot.chol<240 & lipid.dt $hdl>=40] <- 0  
lipid.dt <- cbind(lipid.dt,h_chol)
dim(lipid.dt)                # 82539  6



# ---------------------------------------------------------------------------------------
# ** hypertension ** 															                      ---- ** hypertension ** ----
# 
# BPXSY1 - Systolic: Blood pres (1st rdg) mm Hg
# BPXDI1 - Diastolic: Blood pres (1st rdg) mm Hg
#
# BPQ040A - Taking prescription for hypertension
# BPQ050A - Now taking prescribed medicine for HBP
#
# ---------------------------------------------------------------------------------------

bp.vars <- c(sprintf("BPXSY%d",1:4),sprintf("BPXDI%d",1:4))
bp.d1 <- read.xport(sprintf("%sBPX.xpt",dir.name))[,c("SEQN",bp.vars)] 
bp.d2 <- read.xport(sprintf("%sBPX_B.xpt",dir.name)) [,c("SEQN",bp.vars)]
bp.d3 <- read.xport(sprintf("%sBPX_C.xpt",dir.name)) [,c("SEQN",bp.vars)]
bp.d4 <- read.xport(sprintf("%sBPX_D.xpt",dir.name)) [,c("SEQN",bp.vars)]
bp.d5 <- read.xport(sprintf("%sBPX_E.xpt",dir.name)) [,c("SEQN",bp.vars)]
bp.d6 <- read.xport(sprintf("%sBPX_F.xpt",dir.2)) [,c("SEQN",bp.vars)]
bp.d7 <- read.xport(sprintf("%sBPX_G.xpt",dir.2)) [,c("SEQN",bp.vars)]
bp.d8 <- read.xport(sprintf("%sBPX_H.xpt",dir.2)) [,c("SEQN",bp.vars)]
bp.d9 <- read.xport(sprintf("%sBPX_I.xpt",dir.2)) [,c("SEQN",bp.vars)]
bp.d10 <- read.xport(sprintf("%sBPX_J.xpt",dir.2)) [,c("SEQN",bp.vars)]
bp.dt <- rbind(bp.d1,bp.d2,bp.d3,bp.d4,bp.d5,bp.d6,bp.d7,bp.d8,bp.d9,bp.d10)

# ** create vars **
htn <- rep(0,nrow(bp.dt))
sbp <- apply(bp.dt[,sprintf("BPXSY%d",1:4)],1,mean,na.rm=T)
dbp <- apply(bp.dt[,sprintf("BPXDI%d",1:4)],1,mean,na.rm=T)
sbp[sbp==0] <- NA
dbp[dbp==0] <- NA
htn[sbp>=120 | dbp >=80] <- 1
htn[sbp>=140 | dbp >=90] <- 2 
htn[sbp>=160 | dbp >=100] <- 3 
htn[sbp>=180 | dbp >=110] <- 4 
htn[is.na(sbp)] <- NA 
htn[is.na(dbp)] <- NA 
out.bp <- cbind(bp.dt[,"SEQN"],sbp,dbp,htn)
dim(out.bp)                                                       # 96766 4
colnames(out.bp) <- c("SEQN","sbp","dbp","htn")

# ** redundant ck
head(bp.dt[htn==4 & !is.na(htn),c(sprintf("BPXSY%d",1:4),sprintf("BPXDI%d",1:4))])


# ~~~~~~~~~~ BPQ vars ~~~~~~~
bpq.vars <- c("BPQ040A","BPQ050A")
bpq.d1 <- read.xport(sprintf("%sBPQ.xpt",dir.name))[,c("SEQN",bpq.vars)]
bpq.d2 <- read.xport(sprintf("%sBPQ_B.xpt",dir.name))[,c("SEQN",bpq.vars)] 
bpq.d3 <- read.xport(sprintf("%sBPQ_C.xpt",dir.name))[,c("SEQN",bpq.vars)] 
bpq.d4 <- read.xport(sprintf("%sBPQ_D.xpt",dir.name))[,c("SEQN",bpq.vars)] 
bpq.d5 <- read.xport(sprintf("%sBPQ_E.xpt",dir.name))[,c("SEQN",bpq.vars)] 
bpq.d6 <- read.xport(sprintf("%sBPQ_F.xpt",dir.2))[,c("SEQN",bpq.vars)] 
bpq.d7 <- read.xport(sprintf("%sBPQ_G.xpt",dir.2))[,c("SEQN",bpq.vars)] 
bpq.d8 <- read.xport(sprintf("%sBPQ_H.xpt",dir.2))[,c("SEQN",bpq.vars)] 
bpq.d9 <- read.xport(sprintf("%sBPQ_I.xpt",dir.2))[,c("SEQN",bpq.vars)] 
bpq.d10 <- read.xport(sprintf("%sBPQ_J.xpt",dir.2))[,c("SEQN",bpq.vars)] 
bpq.dt <- rbind(bpq.d1,bpq.d2,bpq.d3,bpq.d4,bpq.d5,bpq.d6,bpq.d7,bpq.d8,bpq.d9,bpq.d10)
ck <- bpq.dt[,2:3]
ck[ck==2] <- 0
ck[ck==9 | ck==7] <-NA
bpq.htn <- rep(0,nrow(bpq.dt))
bpq.htn[ck$BPQ040A==1] <- 1
bpq.htn[ck$BPQ050A==1] <- 1
bpq.htn[is.na(ck$BPQ050A)] <- NA             # current meds for hypertension
out.bpq <- cbind(bpq.dt[,"SEQN"],bpq.htn)
colnames(out.bpq) <- c("SEQN","bpq.htn")

out.bp <- merge(out.bp,out.bpq,by="SEQN",all.x=T,as.is=T,na=c("NA","NaN","."))
dim(out.bp)                                                         # 96766   5              
table(out.bp$htn,out.bp$bpq.htn)

htn2 <- rep(0,nrow(out.bp))
htn2[out.bp$bpq.htn==1] <- 1
htn2[out.bp$htn>0] <- 1
table(htn2,out.bp$htn)

# ===== HTN =====                                                                                          ------~~~ htn.dt ~~~------
htn.dt <- cbind(out.bp,htn2)
dim(htn.dt)                                                        # 96766   6                                                                                           

# ---------------------------------------------------------------------------------------
# ** Diabetes **                                                                                              ------  ** DM ** ------
#
#		•	DIQ010 - Doctor told you have diabetes  (1=yes, 2=no, 3=borderline)
#		•	DID040 - Age when first told you had diabetes
#		•	DIQ050 - Taking insulin now
#       •	DIQ070 - Take diabetic pills to lower blood sugar
#	    •   LBXGLU - Fasting Glucose (mg/dL)
#   	    •	LBXIN - Insulin (uU/mL)
#      	•	LBXGLT - Two Hour Glucose(OGTT) (mg/dL)
#
# ---------------------------------------------------------------------------------------

diq.vars2 <- c("dm_hx","dm_age","dm_tx_ins","dm_tx_pill")

dm.d1 <- read.xport(sprintf("%sDIQ.xpt",dir.name))[,c("SEQN","DIQ010","DIQ040G","DIQ050","DIQ070")]
colnames(dm.d1) <- c("SEQN",diq.vars2)
dm.d2 <- read.xport(sprintf("%sDIQ_B.xpt",dir.name))[,c("SEQN","DIQ010","DID040G","DIQ050","DIQ070")]
colnames(dm.d2) <- c("SEQN",diq.vars2)
dm.d3 <- read.xport(sprintf("%sDIQ_C.xpt",dir.name))[,c("SEQN","DIQ010","DID040G","DIQ050","DIQ070")]
colnames(dm.d3) <- c("SEQN",diq.vars2)
dm.d4 <- read.xport(sprintf("%sDIQ_D.xpt",dir.name))[,c("SEQN","DIQ010","DID040","DIQ050","DID070")]
colnames(dm.d4) <- c("SEQN",diq.vars2)
dm.d5 <- read.xport(sprintf("%sDIQ_E.xpt",dir.name))[,c("SEQN","DIQ010","DID040","DIQ050","DID070")]
colnames(dm.d5) <- c("SEQN",diq.vars2)
dm.d6 <- read.xport(sprintf("%sDIQ_F.xpt",dir.2))[,c("SEQN","DIQ010","DID040","DIQ050","DIQ070")]
dm.d7 <- read.xport(sprintf("%sDIQ_G.xpt",dir.2))[,c("SEQN","DIQ010","DID040","DIQ050","DIQ070")]
dm.d8 <- read.xport(sprintf("%sDIQ_H.xpt",dir.2))[,c("SEQN","DIQ010","DID040","DIQ050","DIQ070")]
dm.d9 <- read.xport(sprintf("%sDIQ_I.xpt",dir.2))[,c("SEQN","DIQ010","DID040","DIQ050","DIQ070")]
dm.d10 <- read.xport(sprintf("%sDIQ_J.xpt",dir.2))[,c("SEQN","DIQ010","DID040","DIQ050","DIQ070")]
dm.dt1 <- rbind(dm.d1,dm.d2,dm.d3,dm.d4,dm.d5)
dm.dt2 <- rbind(dm.d6,dm.d7,dm.d8,dm.d9,dm.d10)
colnames(dm.dt2) <- c("SEQN",diq.vars2)
dm.dt <- rbind(dm.dt1,dm.dt2)

# ** create vars **
dm.dt$dm_hx[dm.dt$dm_hx==9 | dm.dt$dm_hx==7] <- NA

dm_tx <- rep(NA,nrow(dm.dt))
dm_tx[dm.dt$dm_tx_ins==2] <- 0
dm_tx[dm.dt$dm_tx_ins==1] <- 1
dm_tx[dm.dt$dm_tx_pill==1] <- 1
table(dm_tx)

dm.dt <- cbind(dm.dt,dm_tx)


# ** laboratory definition **
glu.vars <- c("LBXGLU","LBXIN")
glu.d1 <- read.xport(sprintf("%sLAB10AM.xpt",dir.name))[,c("SEQN",glu.vars)]
glu.d2 <- read.xport(sprintf("%sL10AM_B.xpt",dir.name))[,c("SEQN",glu.vars)]
glu.d3 <- read.xport(sprintf("%sL10AM_C.xpt",dir.name))[,c("SEQN",glu.vars)]
glu.d4 <- read.xport(sprintf("%sGLU_D.xpt",dir.name))[,c("SEQN",glu.vars)]
glu.d5 <- read.xport(sprintf("%sGLU_E.xpt",dir.name))[,c("SEQN",glu.vars)]
glu.d6 <- read.xport(sprintf("%sGLU_F.xpt",dir.2))[,c("SEQN",glu.vars)]
glu.d7 <- read.xport(sprintf("%sGLU_G.xpt",dir.2))[,c("SEQN",glu.vars)]
glu.d8a <- read.xport(sprintf("%sGLU_H.xpt",dir.2))[,c("SEQN","LBXGLU")]
glu.d8b <- read.xport(sprintf("%sINS_H.xpt",dir.2))[,c("SEQN","LBXIN")]
glu.d8 <- merge(glu.d8a, glu.d8b, by="SEQN",all.x=T,as.is=T,na=c("NA","."))
glu.d9a <- read.xport(sprintf("%sGLU_I.xpt",dir.2))[,c("SEQN","LBXGLU")]
glu.d9b <- read.xport(sprintf("%sINS_I.xpt",dir.2))[,c("SEQN","LBXIN")]
glu.d9 <- merge(glu.d9a, glu.d9b, by="SEQN",all.x=T,as.is=T,na=c("NA","."))
glu.d10a <- read.xport(sprintf("%sGLU_J.xpt",dir.2))[,c("SEQN","LBXGLU")]
glu.d10b <- read.xport(sprintf("%sINS_J.xpt",dir.2))[,c("SEQN","LBXIN")]
glu.d10 <- merge(glu.d10a, glu.d10b, by="SEQN",all.x=T,as.is=T,na=c("NA","."))

glu.dt <- rbind(glu.d1,glu.d2,glu.d3,glu.d4,glu.d5,glu.d6,glu.d7,glu.d8,glu.d9,glu.d10)

dm_lab <- rep(0,nrow(glu.dt))
dm_lab[glu.dt$LBXGLU>126 | glu.dt$LBXGLU>140]<-1
table(dm_lab)
glu.dt <- cbind(glu.dt,dm_lab)

dm.dt <- merge(dm.dt,glu.dt,by="SEQN",all.x=T,as.is=T,na=c("NA","."))

pt.dm <- dm.dt$dm_hx
pt.dm[dm.dt$dm_tx==1] <- 1
pt.dm[dm.dt$dm_lab==1] <- 1
pt.dm[pt.dm>1] <- 0
table(pt.dm)
dm.dt <- cbind(dm.dt,pt.dm)

out.dm <- dm.dt[,c("SEQN","LBXGLU","LBXIN","pt.dm")]


# ** HbA1c **
hb.d1 <- read.xport(sprintf("%sLAB10.xpt",dir.name))
hb.d2 <- read.xport(sprintf("%sL10_B.xpt",dir.name))
hb.d3 <- read.xport(sprintf("%sL10_C.xpt",dir.name))
hb.d4 <- read.xport(sprintf("%sGHB_D.xpt",dir.name))
hb.d5 <- read.xport(sprintf("%sGHB_E.xpt",dir.name))
hb.d6 <- read.xport(sprintf("%sGHB_F.xpt",dir.2))
hb.d7 <- read.xport(sprintf("%sGHB_G.xpt",dir.2))
hb.d8 <- read.xport(sprintf("%sGHB_H.xpt",dir.2))
hb.d9 <- read.xport(sprintf("%sGHB_I.xpt",dir.2))
hb.d10 <- read.xport(sprintf("%sGHB_J.xpt",dir.2))

hb.dt <- rbind(hb.d1,hb.d2,hb.d3,hb.d4,hb.d5,hb.d6,hb.d7,hb.d8,hb.d9,hb.d10)
colnames(hb.dt) <- c("SEQN","hba1c")

# ===== diabetes =====                                                                                          ------~~~ diab.dt ~~~------diab.dt <- merge(out.dm,hb.dt,by="SEQN",all.x=T,as.is=T,na=c("NA","."))     
dim(diab.dt)          							  # 96811   5                


# -------------------------------------------------------------------------
# ~~    Insulin Resistance   ~~ 																				** HOMA-IR **
# fasting serum insulin (mU/L) * fasting plasma glucose (mmol/L)/22.5
# -------------------------------------------------------------------------

homa_ir <- diab.dt$LBXIN*diab.dt$LBXGLU/22.5
ln_ir <- log(homa_ir)

# ===== diabetes =====                                                                                          ------~~~ diab.dt ~~~------
diab.dt <- cbind(diab.dt,ln_ir)
dim(diab.dt)            						  # 96811   6                




# ------------------------------------------------------------------------
# ** demograhics **
# ------------------------------------------------------------------------

dir.name <- "./1999-2008/"
demo.vars <- c("SEQN","RIAGENDR","RIDAGEYR","RIDRETH1","DMDEDUC2","INDFMPIR")

demo.d1 <- read.xport(sprintf("%sDEMO.xpt",dir.name))[,demo.vars]
demo.d2 <- read.xport(sprintf("%sDEMO_B.xpt",dir.name))[,demo.vars]
demo.d3 <- read.xport(sprintf("%sDEMO_C.xpt",dir.name))[,demo.vars]
demo.d4 <- read.xport(sprintf("%sDEMO_D.xpt",dir.name))[,demo.vars]
demo.d5 <- read.xport(sprintf("%sDEMO_E.xpt",dir.name))[,demo.vars]

dir.2 <- "./2009-2018/"
demo.d6 <- read.xport(sprintf("%sDEMO_F.xpt",dir.2))[,demo.vars]
demo.d7 <- read.xport(sprintf("%sDEMO_G.xpt",dir.2))[,demo.vars]
demo.d8 <- read.xport(sprintf("%sDEMO_H.xpt",dir.2))[,demo.vars]
demo.d9 <- read.xport(sprintf("%sDEMO_I.xpt",dir.2))[,demo.vars]
demo.d10 <- read.xport(sprintf("%sDEMO_J.xpt",dir.2))[,demo.vars]

dat.demo <- rbind(demo.d1,demo.d2,demo.d3,demo.d4,demo.d5,demo.d6,demo.d7,demo.d8,demo.d9,demo.d10)
colnames(dat.demo) <- c("SEQN","gender","age","race","educ","incPovR")

# ===== demo =====                                                                                          ------~~~ demo.dt ~~~------
demo.dt <- dat.demo                                   
dim(demo.dt)            			               # 101316   6                


# ----------------------- modify vars  -----------------------
d.use <- demo.dt

educ3 <- d.use$educ                # 1= below high school, 2= high school, 3= college above
educ3[d.use$educ==7 | d.use$educ==9 ] <-NA
educ3[d.use$educ==2 ] <- 1
educ3[d.use$educ==3 ] <- 2
educ3[d.use$educ==4 | d.use$educ==5 ] <-3
table(educ3,d.use$educ)

educ2 <- educ3
educ2[educ3<3] <- 0
educ2[educ3==3] <-1

race4 <- d.use$race
race4[d.use$race==9 ] <-NA
race4[d.use$race <=2] <- 3     # hispanic
race4[d.use$race ==3] <- 1     # white
race4[d.use$race ==4] <- 2     # black
race4[d.use$race >=5] <- 4     # other
table(race4,d.use$race)

incGr <- d.use$incPovR
incGr[d.use$incPovR<=1 ] <-1
incGr[d.use$incPovR >1 & d.use$incPovR <=2] <- 2 
incGr[d.use$incPovR >2 & d.use$incPovR <=4] <- 3            
incGr[d.use$incPovR >=4 ] <- 4

demo.dt <- cbind(demo.dt,educ3,educ2,race4,incGr)

# ===== demo =====                                                                                          ------~~~ demo.dt ~~~------
dim(demo.dt)                   # 101316   10          																						               



# ------------------------------------------------------------------------
#
#SMQ020 - Smoked at least 100 cigarettes in life
#SMQ040 - Do you now smoke cigarettes
#
#SMD055 - Age last smoked cigarettes regularly             
#SMD030 - Age started smoking cigarettes regularly
#SMD057 - # cigarettes smoked per day when quit
#
#SMD641 - # days smoked cigs during past 30 days
#SMD650 - Avg # cigarettes/day during past 30 days
#
# ------------------------------------------------------------------------

smk.vars <- c("SEQN","SMQ020","SMQ040","SMD030","SMD057","SMD055")             # d1,d2 miss SMK641,SMD650
smk.vars2 <- c("SEQN","SMQ020","SMQ040","SMD030","SMD057","SMD641","SMD650","SMD055")  
smk.vars3 <- c("SEQN","SMQ020","SMQ040","SMD030","SMD057","SMD641","SMD650")  
  
smk.d1 <- read.xport(sprintf("%sSMQ.xpt",dir.name))[smk.vars]
smk.d2 <- read.xport(sprintf("%sSMQ_B.xpt",dir.name))[smk.vars]
smk.d3 <- read.xport(sprintf("%sSMQ_C.xpt",dir.name))[smk.vars2]
smk.d4 <- read.xport(sprintf("%sSMQ_D.xpt",dir.name))[smk.vars2]
smk.d5 <- read.xport(sprintf("%sSMQ_E.xpt",dir.name))[smk.vars2]

smk.d6 <- read.xport(sprintf("%sSMQ_F.xpt",dir.2))[smk.vars2]
smk.d7 <- read.xport(sprintf("%sSMQ_G.xpt",dir.2))[smk.vars2]
smk.d8 <- read.xport(sprintf("%sSMQ_H.xpt",dir.2))[smk.vars2]
smk.d9 <- read.xport(sprintf("%sSMQ_I.xpt",dir.2))[smk.vars2]
smk.d10 <- read.xport(sprintf("%sSMQ_J.xpt",dir.2))[smk.vars3]


d1.smk <- rbind(smk.d1,smk.d2)
d1.smk <- cbind(d1.smk,rep(NA,nrow(d1.smk)),rep(NA,nrow(d1.smk)))
colnames(d1.smk) <- smk.vars2

d2.smk <- rbind(smk.d3,smk.d4,smk.d5,smk.d6,smk.d7,smk.d8,smk.d9)
d10.smk <- cbind(smk.d10,rep(NA,nrow(smk.d10)))
colnames(d10.smk) <- smk.vars2
d2.smk <- rbind(d2.smk,d10.smk)

d.smk <- rbind(d1.smk,d2.smk)
d.smk.seqn <- d.smk$SEQN
d.smk[d.smk==777] <- NA
d.smk[d.smk==999] <- NA
d.smk[d.smk==7] <- NA
d.smk[d.smk==9] <- NA
dim(d.smk)
d.smk[,"SEQN"] <- d.smk.seqn

# ** create vars **
smk.nvr <- rep(NA,nrow(d.smk))
smk.nvr[d.smk$SMQ020==2 & !is.na(d.smk$SMQ020)] <- 1
smk.nvr[d.smk$SMQ020==1 & !is.na(d.smk$SMQ020)] <- 0
smk.nvr[is.na(d.smk$SMQ020)] <- NA           # never-smoker = 1

smk.curr <- rep(0,nrow(d.smk))
smk.curr[d.smk$SMQ040==1] <- 1
smk.curr[d.smk$SMQ040==2] <- 1
smk.curr[is.na(d.smk$SMQ040)] <- NA
smk.curr[smk.nvr==1] <- 0

smk.past <- rep(1,nrow(d.smk))
smk.past[d.smk$SMQ040==1] <- 0
smk.past[d.smk$SMQ040==2] <- 0
smk.past[is.na(d.smk$SMQ040)] <- NA
smk.past[smk.nvr==1] <- 0

smk.cat3 <- rep(NA,nrow(d.smk))
smk.cat3[smk.curr==1] <-3
smk.cat3[smk.past==1] <-2
smk.cat3[smk.nvr==1] <-1


smk.past.yrs <- d.smk$SMD055-d.smk$SMD030
smk.past.pkyrs <- d.smk$SMD057/20*smk.past.yrs 
smk.curr.pkyrs <- d.smk$SMD650/20*d.smk$SMD641/30*12

# ~~~~~~~ redundant ck ~~~~~
ck1 <- !is.na(smk.curr.pkyrs)
ck2 <- !is.na(smk.past.pkyrs)
table(ck1,ck2)

smkpkyrs <- rep(NA,nrow(d.smk))
smkpkyrs[ck1==T] <- smk.curr.pkyrs[ck1==T]
smkpkyrs[ck2==T] <- smk.past.pkyrs[ck2==T]
smkpkyrs[smk.nvr==1] <- 0

out.smk <- cbind(d.smk[,"SEQN"],smk.nvr,smk.curr,smk.past,smk.cat3,smkpkyrs)
colnames(out.smk) <- c("SEQN","nvrsmk","currsmk","pastsmk","smk.cat3","smkpkyrs")

# ===== smk =====                                                                                          ------~~~ smk.dt ~~~------
smk.dt <- out.smk
dim(smk.dt)                             # 64874   6                


				
# ** output temp **   -- demo_smk --																	   ------~~~ demo_smk.dt ~~~------
demo_smk.dt <- merge(demo.dt,out.smk,by="SEQN",all.x=T,as.is=T,na=c("NA","-9",7,9,99,777,999))
dim(demo_smk.dt)                	   # 101316    15                     

write.table(demo_smk.dt,"d0_demo_smk.txt",sep="\t",quote=F, col.names=T, row.names=T, append=F)




# ---------------------------------------------------------------------------------------
# ** Physical activity **             ** JAMA 2018 [pmid = 30418471] **
#
# sedentary: sit 10+ hours a day 
# PAQ480/PAD480 [5]  (no mod or vig activities) 
# PAD480: Daily hours of TV, video or computer use............................1.2        [PAD680]
# PAQ180 [1] (no mod or vig activities) 
# PAQ180: Average level of physical activity each day 
#  (1.  Mainly sit)............................................................1.4 
#  (2.  Walk a lot)............................................................1.5
#  (3.  Carry light loads).....................................................1.6 
#  (4.  Carry heavy loads).....................................................1.8
# PAD590+PAD600   //   PAQ710/715    //   PAD680       [10+] 
#
# moderate (2.5~5hrs=150~300mins): PAD020, PAQ100, PAD320, PAD440
# PAD020: Walked or bicycled over past 30 days to get to/from work, et........4.0       [PAQ635] 
# PAQ100: Tasks in or around home or yard past 30 days........................4.5
# PAD320: Moderate activity over past 30 days                                           [yes/no]
# PAD440: Muscle strengthening activities.....................................4.0
# ** mod (yes/no) (days) (mins): 620/625/630 ; 665/670/675
#
# vigorous (1.25~2.5hrs=75~150mins): PAD200, PAQ560 
# PAD200: Vigorous activity over past 30 days                                           [yes/no]
# PAQ560: Number of times per week play or exercise hard......................7.0
# ** vig (yes/no) (days) (mins): 605/610/615 ; 650/655/660
#
# ---------------------------------------------------------------------------------------

out.vars <- c("SEQN","sed","mod","vig")

paq.d1 <- read.xport(sprintf("%sPAQ.xpt",dir.name))

# vig
paq.d1$PAD200[paq.d1$PAD200 ==3 |paq.d1$PAD200 ==7 | paq.d1$PAD200 ==9] <-NA
paq.d1$PAQ560[paq.d1$PAQ560 ==77 |paq.d1$PAQ560 ==99999] <-NA
d1.vig <- rep(NA,nrow(paq.d1))
d1.vig[!is.na(paq.d1$PAD200) |!is.na(paq.d1$PAQ560) ] <- 0
d1.vig[paq.d1$PAD200==1] <- 1
d1.vig[paq.d1$PAQ560>2 & !is.na(paq.d1$PAQ560)] <- 1

# mod
paq.d1$PAD020[paq.d1$PAD020==3 |paq.d1$PAD020==7 | paq.d1$PAD020==9] <-NA
paq.d1$PAQ100[paq.d1$PAQ100 ==3 |paq.d1$PAQ100 ==7 | paq.d1$PAQ100 ==9] <-NA
paq.d1$PAD320[paq.d1$PAD320 ==3 |paq.d1$PAD320 ==7 | paq.d1$PAD320 ==9] <-NA
paq.d1$PAD440[paq.d1$PAD440 ==3 |paq.d1$PAD440 ==7 | paq.d1$PAD440 ==9] <-NA

biketime <- paq.d1$PAQ050Q
biketime[paq.d1$PAQ050Q==99999 | paq.d1$PAQ050Q==7777] <- NA
biketime[paq.d1$PAQ050U==3 & !is.na(paq.d1$PAQ050U)] <- biketime[paq.d1$PAQ050U==3 & !is.na(paq.d1$PAQ050U)] /4.5
biketime[paq.d1$PAQ050U==1 & !is.na(paq.d1$PAQ050U)] <- biketime[paq.d1$PAQ050U==1 & !is.na(paq.d1$PAQ050U)] *7

tasktime <- paq.d1$PAD120
tasktime[paq.d1$PAD120==99999 |paq.d1$PAD120==7777] <- NA
tasktime[paq.d1$PAQ100==1 & !is.na(paq.d1$PAQ100)] <- tasktime[paq.d1$PAQ100==1 & !is.na(paq.d1$PAQ100)] /4.5

strengthtime <- paq.d1$PAD460
strengthtime[paq.d1$PAD460==99999 | paq.d1$PAD460==7777] <- NA
strengthtime[paq.d1$PAD440==1 & !is.na(paq.d1$PAD440)] <- strengthtime[paq.d1$PAD440==1 & !is.na(paq.d1$PAD440)] /4.5

acttime <- apply(cbind(biketime,tasktime,strengthtime),1,sum,na.rm=T)

d1.mod <- rep(NA,nrow(paq.d1))
d1.mod[!is.na(paq.d1$PAD020) |!is.na(paq.d1$PAQ100) | !is.na(paq.d1$PAD320) |!is.na(paq.d1$PAD440)] <- 0
d1.mod[paq.d1$PAD320==1] <- 1
d1.mod[acttime >=5] <- 1

# sed
paq.d1$PAQ480[paq.d1$PAQ480==99 | paq.d1$PAQ480==77] <- NA
paq.d1$PAQ480[paq.d1$PAQ480==6] <- 0
paq.d1$PAQ180[paq.d1$PAQ180 ==7 |paq.d1$PAQ180 ==9 ] <-NA

d1.sed <- rep(0,nrow(paq.d1))
d1.sed[paq.d1$PAQ480==5 ] <- 1
d1.sed[paq.d1$PAQ180==1 ] <- 1
d1.phys <- cbind(paq.d1$SEQN,d1.sed,d1.mod,d1.vig)

ck <- rep(2,nrow(paq.d1))
ck[d1.mod==1] <- 3
ck[d1.vig==1] <- 4
ck[d1.sed==1] <- 1

d1.phys <- cbind(d1.phys,ck)
colnames(d1.phys) <- c(out.vars,"exercise")


# ~~~~~~~~~~~~~~~~~
paq.d2 <- read.xport(sprintf("%sPAQ_B.xpt",dir.name))
# vig
paq.d2$PAD200[paq.d2$PAD200 ==3 |paq.d2$PAD200 ==7 | paq.d2$PAD200 ==9] <-NA
paq.d2$PAQ560[paq.d2$PAQ560 ==77 |paq.d2$PAQ560 ==99999] <-NA
d2.vig <- rep(NA,nrow(paq.d2))
d2.vig[!is.na(paq.d2$PAD200) |!is.na(paq.d2$PAQ560) ] <- 0
d2.vig[paq.d2$PAD200==1] <- 1
d2.vig[paq.d2$Q560>2 & !is.na(paq.d2$PAQ560)] <- 1

# mod
paq.d2$PAD020[paq.d2$PAD020==3 |paq.d2$PAD020==7 | paq.d2$PAD020==9] <-NA
paq.d2$PAQ100[paq.d2$PAQ100 ==3 |paq.d2$PAQ100 ==7 | paq.d2$PAQ100 ==9] <-NA
paq.d2$PAD320[paq.d2$PAD320 ==3 |paq.d2$PAD320 ==7 | paq.d2$PAD320 ==9] <-NA
paq.d2$PAD440[paq.d2$PAD440 ==3 |paq.d2$PAD440 ==7 | paq.d2$PAD440 ==9] <-NA

biketime <- paq.d2$PAQ050Q
biketime[paq.d2$PAQ050U==3 & !is.na(paq.d2$PAQ050U)] <- biketime[paq.d2$PAQ050U==3 & !is.na(paq.d2$PAQ050U)] /4.5
biketime[paq.d2$PAQ050U==1 & !is.na(paq.d2$PAQ050U)] <- biketime[paq.d2$PAQ050U==1 & !is.na(paq.d2$PAQ050U)] *7

tasktime <- paq.d2$PAD120
tasktime[paq.d2$PAQ100==1 & !is.na(paq.d2$PAQ100)] <- tasktime[paq.d2$PAQ100==1 & !is.na(paq.d2$PAQ100)] /4.5

strengthtime <- paq.d2$PAD460
strengthtime[paq.d2$PAD440==1 & !is.na(paq.d2$PAD440)] <- strengthtime[paq.d2$PAD440==1 & !is.na(paq.d2$PAD440)] /4.5

acttime <- apply(cbind(biketime,tasktime,strengthtime),1,sum,na.rm=T)

d2.mod <- rep(NA,nrow(paq.d2))
d2.mod[!is.na(paq.d2$PAD020) |!is.na(paq.d2$PAQ100) | !is.na(paq.d2$PAD320) |!is.na(paq.d2$PAD440)] <- 0
d2.mod[paq.d2$PAD320==1] <- 1
d2.mod[acttime >=5] <- 1

# sed
paq.d2$PAD480[paq.d2$PAD480==99 | paq.d2$PAD480==77] <- NA
paq.d2$PAD480[paq.d2$PAD480==6] <- 0
paq.d2$PAQ180[paq.d2$PAQ180 ==7 |paq.d2$PAQ180 ==9 ] <-NA
paq.d2$PAD590[paq.d2$PAD590 ==99 | paq.d2$PAD590 ==77] <- NA
paq.d2$PAD590[paq.d2$PAD590 ==6] <- 0
paq.d2$PAD600[paq.d2$PAD600 ==99 | paq.d2$PAD600 ==77] <- NA
paq.d2$PAD600[paq.d2$PAD600 ==6] <- 0
sittime <- paq.d2$PAD590+paq.d2$PAD600

d2.sed <- rep(0,nrow(paq.d2))
d2.sed[paq.d2$PAQ480==5] <- 1
d2.sed[paq.d2$PAQ180==1] <- 1
d2.sed[sittime>=5] <- 1
d2.phys <- cbind(paq.d2$SEQN,d2.sed,d2.mod,d2.vig)

ck <- rep(2,nrow(paq.d2))
ck[d2.mod==1] <- 3
ck[d2.vig==1] <- 4
ck[d2.sed==1] <- 1

d2.phys <- cbind(d2.phys,ck)
colnames(d2.phys) <- c(out.vars,"exercise")


# ~~~~~~~~~~~~~~~~~
paq.d3 <- read.xport(sprintf("%sPAQ_C.xpt",dir.name))
paq.d4 <- read.xport(sprintf("%sPAQ_D.xpt",dir.name))
tmp.dt <- rbind(paq.d3,paq.d4)

# vig
tmp.dt $PAD200[tmp.dt $PAD200 ==3 | tmp.dt $PAD200 ==7 | tmp.dt $PAD200 ==9] <-NA
tmp.dt $PAQ560[tmp.dt $PAQ560 ==77 | tmp.dt $PAQ560 ==99999] <-NA
d34.vig <- rep(NA,nrow(tmp.dt))
d34.vig[!is.na(tmp.dt $PAD200) |!is.na(tmp.dt $PAQ560) ] <- 0
d34.vig[tmp.dt $PAD200==1] <- 1
d34.vig[tmp.dt $PAQ560>2 & !is.na(tmp.dt$PAQ560)] <- 1

# mod
tmp.dt $PAD020[tmp.dt $PAD020==3 | tmp.dt $PAD020==7 | tmp.dt $PAD020==9] <-NA
tmp.dt $PAQ100[tmp.dt $PAQ100 ==3 | tmp.dt $PAQ100 ==7 | tmp.dt $PAQ100 ==9] <-NA
tmp.dt $PAD320[tmp.dt $PAD320 ==3 | tmp.dt $PAD320 ==7 | tmp.dt $PAD320 ==9] <-NA
tmp.dt $PAD440[tmp.dt $PAD440 ==3 | tmp.dt $PAD440 ==7 | tmp.dt $PAD440 ==9] <-NA

biketime <- tmp.dt $PAQ050Q
biketime[tmp.dt$PAQ050U==3 & !is.na(tmp.dt$PAQ050U)] <- biketime[tmp.dt$PAQ050U==3 & !is.na(tmp.dt$PAQ050U)] /4.5
biketime[tmp.dt$PAQ050U==1 & !is.na(tmp.dt$PAQ050U)] <- biketime[tmp.dt$PAQ050U==1 & !is.na(tmp.dt$PAQ050U)] *7

tasktime <- tmp.dt $PAD120
tasktime[tmp.dt $PAQ100==1 & !is.na(tmp.dt $PAQ100)] <- tasktime[tmp.dt $PAQ100==1 & !is.na(tmp.dt$PAQ100)] /4.5

strengthtime <- tmp.dt $PAD460
strengthtime[tmp.dt$PAD440==1 & !is.na(tmp.dt $PAD440)] <- strengthtime[tmp.dt$PAD440==1 & !is.na(tmp.dt$PAD440)] /4.5

acttime <- apply(cbind(biketime,tasktime,strengthtime),1,sum,na.rm=T)

d34.mod <- rep(NA,nrow(tmp.dt))
d34.mod[!is.na(tmp.dt$PAD020) |!is.na(tmp.dt$PAQ100) | !is.na(tmp.dt$PAD320) |!is.na(tmp.dt$PAD440)] <- 0
d34.mod[tmp.dt$PAD020==1 | tmp.dt$PAQ100==1 | tmp.dt$PAD320==1 | tmp.dt$PAD440==1] <- 1
d34.mod[tmp.dt$PAD320==1 & acttime >=5 ] <- 1

# sed
tmp.dt$PAQ180[tmp.dt$PAQ180 ==7 |tmp.dt$PAQ180 ==9 ] <-NA
tmp.dt$PAD590[tmp.dt$PAD590 ==99 | tmp.dt$PAD590 ==77] <- NA
tmp.dt$PAD590[tmp.dt$PAD590 ==6] <- 0
tmp.dt$PAD600[tmp.dt$PAD600 ==99 | tmp.dt$PAD600 ==77] <- NA
tmp.dt$PAD600[tmp.dt$PAD600 ==6] <- 0
sittime <- apply(cbind(tmp.dt$PAD600,tmp.dt$PAD590),1,sum,na.rm=T)

d34.sed <- rep(0,nrow(tmp.dt))
d34.sed[tmp.dt$PAQ180==1] <- 1
d34.sed[sittime>=5] <- 1
d34.phys <- cbind(tmp.dt$SEQN,d34.sed,d34.mod,d34.vig)

ck <- rep(2,nrow(tmp.dt))
ck[d34.mod==1] <- 3
ck[d34.vig==1] <- 4
ck[d34.sed==1] <- 1

d34.phys <- cbind(d34.phys,ck)
colnames(d34.phys) <- c(out.vars,"exercise")


# ~~~~~~~~~~~~~~~~~
paq.d5 <- read.xport(sprintf("%sPAQ_E.xpt",dir.name))
paq.d6 <- read.xport(sprintf("%sPAQ_F.xpt",dir.2))
paq.d6[,2] <- NA
colnames(paq.d6) <- colnames(paq.d5)
tmp.dt <- rbind(paq.d5,paq.d6)

# vig
tmp.dt $PAD660[tmp.dt$PAD660 == 9999 | tmp.dt$PAD660 == 7777 ] <-NA
tmp.dt $PAD615[tmp.dt$PAD615 == 9999 | tmp.dt$PAD615 == 7777 ] <-NA
vigtime <- (apply(cbind(tmp.dt$PAD660,tmp.dt$PAD615),1,sum,na.rm=T))*7
tmp.dt $PAQ560[tmp.dt $PAQ560 ==77 | tmp.dt $PAQ560 ==99 |tmp.dt $PAQ560 ==999] <-NA
d56.vig <- rep(NA,nrow(tmp.dt))
d56.vig[!is.na(vigtime) |!is.na(tmp.dt $PAQ560) ] <- 0
d56.vig[vigtime>=75] <- 1
d56.vig[tmp.dt$PAQ560>2] <- 1

# mod
tmp.dt$PAD645[tmp.dt$PAD645 == 9999 | tmp.dt$PAD645 == 7777] <-NA
tmp.dt$PAD630[tmp.dt$PAD630 == 9999 | tmp.dt$PAD630 == 7777] <-NA
tmp.dt$PAD675[tmp.dt$PAD675 == 9999 | tmp.dt$PAD675 == 7777] <-NA
modtime <- (apply(cbind(tmp.dt$PAD630,tmp.dt$PAD675,tmp.dt$PAD645),1,sum,na.rm=T))*7
d56.mod <- rep(NA,nrow(tmp.dt))
d56.mod[!is.na(modtime) |!is.na(d56.vig) ] <- 0
d56.mod[modtime>=150 & !is.na(modtime)] <- 1
d56.mod[(modtime)/2+vigtime>=75] <- 1

# sed
tmp.dt$PAD680[tmp.dt$PAD680 == 9999 | tmp.dt$PAD680 == 7777 ] <-NA
tmp.dt$PAD590[tmp.dt$PAD590 ==99 | tmp.dt$PAD590 ==77] <- NA
tmp.dt$PAD590[tmp.dt$PAD590 ==6] <- 0
tmp.dt$PAD600[tmp.dt$PAD600 ==99 | tmp.dt$PAD600 ==77] <- NA
tmp.dt$PAD600[tmp.dt$PAD600 ==6] <- 0
sittime <- apply(cbind(tmp.dt$PAD600,tmp.dt$PAD590),1,sum,na.rm=T)

d56.sed <- rep(0,nrow(tmp.dt))
d56.sed[tmp.dt$PAD680>=300 & !is.na(tmp.dt$PAD680)] <- 1
d56.sed[sittime>=5] <- 1
d56.phys <- cbind(tmp.dt$SEQN,d56.sed,d56.mod,d56.vig)

ck <- rep(2,nrow(tmp.dt))
ck[d56.mod==1] <- 3
ck[d56.vig==1] <- 4
ck[d56.sed==1] <- 1

d56.phys <- cbind(d56.phys,ck)
colnames(d56.phys) <- c(out.vars,"exercise")


# ~~~~~~~~~~~~~~~~~~~~~~~~~~
paq.vars2 <- c("SEQN","PAD645","PAD630","PAD675","PAD615","PAD660","PAD680","PAQ710","PAQ715")
paq.d7 <- read.xport(sprintf("%sPAQ_G.xpt",dir.2))[,paq.vars2]
paq.d8 <- read.xport(sprintf("%sPAQ_H.xpt",dir.2))[,paq.vars2]
paq.d9 <- read.xport(sprintf("%sPAQ_I.xpt",dir.2))[,paq.vars2]
paq.d10 <- read.xport(sprintf("%sPAQ_J.xpt",dir.2))[,paq.vars2[1:7]]
paq.d10 <- cbind(paq.d10,rep(NA,nrow(paq.d10)),rep(NA,nrow(paq.d10)))
colnames(paq.d10) <- paq.vars2
tmp.dt <- rbind(paq.d7,paq.d8,paq.d9,paq.d10)

# vig
tmp.dt $PAD660[tmp.dt$PAD660 == 9999 | tmp.dt$PAD660 == 7777 ] <-NA
tmp.dt $PAD615[tmp.dt$PAD615 == 9999 | tmp.dt$PAD615 == 7777 ] <-NA
vigtime <- (apply(cbind(tmp.dt$PAD660,tmp.dt$PAD615),1,sum,na.rm=T))*7
d710.vig <- rep(NA,nrow(tmp.dt))
d710.vig[!is.na(vigtime) ] <- 0
d710.vig[vigtime>=75] <- 1

# mod
tmp.dt$PAD645[tmp.dt$PAD645 == 9999 | tmp.dt$PAD645 == 7777] <-NA
tmp.dt$PAD630[tmp.dt$PAD630 == 9999 | tmp.dt$PAD630 == 7777] <-NA
tmp.dt$PAD675[tmp.dt$PAD675 == 9999 | tmp.dt$PAD675 == 7777] <-NA
modtime <- (apply(cbind(tmp.dt$PAD630,tmp.dt$PAD675,tmp.dt$PAD645),1,sum,na.rm=T))*7
d710.mod <- rep(NA,nrow(tmp.dt))
d710.mod[!is.na(modtime) |!is.na(d710.vig) ] <- 0
d710.mod[modtime>=150 & !is.na(modtime)] <- 1
d710.mod[(modtime)/2+vigtime>=75] <- 1

# sed
tmp.dt$PAD680[tmp.dt$PAD680 == 9999 | tmp.dt$PAD680 == 7777 ] <-NA
tmp.dt$PAQ710[tmp.dt$PAQ710 ==99 | tmp.dt$PAQ710 ==77] <- NA
tmp.dt$PAQ710[tmp.dt$PAQ710 ==8] <- 0
tmp.dt$PAQ715[tmp.dt$PAQ715 ==99 | tmp.dt$PAQ715 ==77] <- NA
tmp.dt$PAQ715[tmp.dt$PAQ715 ==8] <- 0
sittime <- apply(cbind(tmp.dt$PAQ710,tmp.dt$PAQ715),1,sum,na.rm=T)

d710.sed <- rep(0,nrow(tmp.dt))
d710.sed[tmp.dt$PAD680>=300 & !is.na(tmp.dt$PAD680)] <- 1
d710.sed[sittime>=5] <- 1
d710.phys <- cbind(tmp.dt$SEQN,d710.sed,d710.mod,d710.vig)

ck <- rep(2,nrow(tmp.dt))
ck[d710.mod==1] <- 3
ck[d710.vig==1] <- 4
ck[d710.sed==1] <- 1

d710.phys <- cbind(d710.phys,ck)
colnames(d710.phys) <- c(out.vars,"exercise")

phys.dt <- rbind(d1.phys,d2.phys,d34.phys,d56.phys,d710.phys)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# before creating metabolic syndrome, merge all data
#
# * cvd.dt, lipid.dt, htn.dt, diab.dt, demo_smk.dt
#
#
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# ----------------------- merge -----------------------

m.data <- merge(demo_smk.dt,cvd.dt,by="SEQN",all.x=T,as.is=T,na=c("NA","."))
m.data <- merge(m.data,lipid.dt,by="SEQN",all.x=T,as.is=T,na=c("NA","."))
m.data <- merge(m.data,htn.dt[,c("SEQN","htn2","sbp","dbp")],by="SEQN",all.x=T,as.is=T,na=c("NA","."))
m.data <- merge(m.data,diab.dt,by="SEQN",all.x=T,as.is=T,na=c("NA","."))
m.data <- merge(m.data,phys.dt,by="SEQN",all.x=T,as.is=T,na=c("NA","."))

# ** merge **   -- m.data --																			   ------~~~ m.data ~~~------
dim(m.data)     																						   # 108060   43                      


# ---------------------------------------------------------------------------------------
# **  Metabolic syndromes **                                                             
# ---------------------------------------------------------------------------------------

# ** create vars **   -- Abdominal obesity --

ms_obese <- rep(0,nrow(m.data))
ms_obese[m.data$waist>102] <- 1
ms_obese[m.data$waist>88 & m.data$gender==2] <- 1
ms_obese[is.na(m.data$waist)] <- NA

# ** ------- Fasting TG ------

ms_tg <- rep(0,nrow(m.data))
ms_tg[m.data$tg>150] <- 1
ms_tg[is.na(m.data$tg)] <- NA

# ** ------- HDL ------
ms_hdl <- rep(0,nrow(m.data))
ms_hdl[m.data$hdl<40] <- 1
ms_hdl[(m.data$hdl>=40 & m.data$hdl <50) & m.data$gender==2] <- 1
ms_hdl[(m.data$hdl>=40 & m.data$hdl <50) & m.data$gender==1] <- 0
ms_hdl[is.na(m.data$hdl)] <- NA

# ** ------- Fasting GLU ------
ms_glu <- rep(0,nrow(m.data))
ms_glu[m.data$LBXGLU>=110] <- 1
ms_glu[is.na(m.data$LBXGLU)] <- NA
ms_glu[m.data$dm_tx==1] <- 1

# ** ------- Blood Pressure ------
ms_bp <- m.data$htn2


# ** ------- MetSyn total score ------
ms_score <- apply(cbind(ms_obese,ms_tg,ms_hdl,ms_glu,ms_bp),1,sum,na.rm=T)
ck.ms <- apply(apply(cbind(ms_obese,ms_tg,ms_hdl,ms_glu,ms_bp),1,is.na),2,sum)
metsyn <- rep(0,nrow(m.data))
metsyn[ms_score>=3] <- 1
metsyn[ck.ms>=3] <- NA
ck <- 

# === mets.dt === **  						   ------~~~ mets.dt ~~~------
mets.dt <- cbind(m.data$SEQN,ms_obese,ms_tg,ms_hdl,ms_glu,ms_bp,ms_score,ck.ms,metsyn)
colnames(mets.dt) <- c("SEQN",colnames(mets.dt)[2:ncol(mets.dt)])



# ===== merged m.data =====                                                                                        ------~~~ m.data ~~~------
m.data <- cbind(m.data,metsyn)

write.table(m.data,"d1_demo_smk_dm_cvd.txt",sep="\t",quote=F, col.names=T, row.names=T, append=F)







