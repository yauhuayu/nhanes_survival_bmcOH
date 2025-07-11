
library(foreign)

dir.name <- "./1999-2008/"
dir.2 <- "./2009-2018/"

d.demo <- read.table('d0_demo_smk.txt',header=T,sep="\t")          # 92062

# ------------------------------------------------------------------------
#  * osteoporosis questionnaire *
#
#	    OSQ010a - Broken or fractured a hip
#         OSQ010b - Broken or fractured a wrist
#         OSQ010c - Broken or fractured spine
# 
# ------------------------------------------------------------------------

op.vars <- c("SEQN","OSQ010A","OSQ010B","OSQ010C","OSQ060","OSQ070")    
osq.d1 <- read.xport(sprintf("%sOSQ.xpt",dir.name))[op.vars]
osq.d2 <- read.xport(sprintf("%sOSQ_B.xpt",dir.name))[op.vars]
osq.d3 <- read.xport(sprintf("%sOSQ_C.xpt",dir.name))[op.vars]
osq.d4 <- read.xport(sprintf("%sOSQ_D.xpt",dir.name))[op.vars]
osq.d5 <- read.xport(sprintf("%sOSQ_E.xpt",dir.name))[op.vars]

osq.d6 <- read.xport(sprintf("%sOSQ_F.xpt",dir.2))[op.vars]
osq.d8 <- read.xport(sprintf("%sOSQ_H.xpt",dir.2))[c("SEQN","OSQ010A","OSQ010B","OSQ010C","OSQ060","OSQ072")]
colnames(osq.d8) <- c("SEQN","hipFrc","wristFrc","spnFrc","op_dx","op_tx")
osq.d10 <- read.xport(sprintf("%sOSQ_J.xpt",dir.2))[c("SEQN","OSQ010A","OSQ010B","OSQ010C","OSQ060","OSQ072")]
colnames(osq.d10) <- c("SEQN","hipFrc","wristFrc","spnFrc","op_dx","op_tx")


d.osq <- rbind(osq.d1,osq.d2,osq.d3,osq.d4,osq.d5,osq.d6)
colnames(d.osq) <- c("SEQN","hipFrc","wristFrc","spnFrc","op_dx","op_tx")
d.osq <- rbind(d.osq,osq.d8,osq.d10)                     # n= 39348

m.data <- merge(d.demo,d.osq,by="SEQN",all.x=T,as.is=T,na=c("NA","-9",7,9,99,777,999))
dim(m.data)                                              #  101316   



# ------------------------------------------------------------------------
#  * bone density *
#
#  DXXLSBMD - Lumbar Spine BMD (g/cm^2)
#  DXDTOBMD - Total Bone Mineral Density (g/cm^2)
#
# ------------------------------------------------------------------------

dxa.vars <- c("SEQN","DXXLSBMD","DXDTOBMD")

dxa.d1 <- read.xport(sprintf("%sdxx.xpt",dir.name))[,dxa.vars]
ind <- seq(1,nrow(dxa.d1),by=5)
dxa.d1 <- dxa.d1[ind,]
dxa.d2 <- read.xport(sprintf("%sdxx_b.xpt",dir.name))[,dxa.vars]
ind <- seq(1,nrow(dxa.d2),by=5)
dxa.d2 <- dxa.d2[ind,]
dxa.d3 <- read.xport(sprintf("%sdxx_c.xpt",dir.name))[,dxa.vars]
ind <- seq(1,nrow(dxa.d3),by=5)
dxa.d3 <- dxa.d3[ind,]
dxa.d4 <- read.xport(sprintf("%sdxx_d.xpt",dir.name))[,dxa.vars]
ind <- seq(1,nrow(dxa.d4),by=5)
dxa.d4 <- dxa.d4[ind,]

d.dxa <- rbind(dxa.d1,dxa.d2,dxa.d3,dxa.d4)

dxa.d7.tob <- read.xport(sprintf("%sDXX_G.xpt",dir.2))[,dxa.vars]
colnames(dxa.d7.tob) <- c("SEQN","LS","TOB") 
dxa.d8.tob <- read.xport(sprintf("%sDXX_H.xpt",dir.2))[,dxa.vars]
colnames(dxa.d8.tob) <- c("SEQN","LS","TOB") 
dxa.d9.tob <- read.xport(sprintf("%sDXX_I.xpt",dir.2))[,dxa.vars]
colnames(dxa.d9.tob) <- c("SEQN","LS","TOB") 
dxa.d10.tob <- read.xport(sprintf("%sDXX_J.xpt",dir.2))[,dxa.vars]
colnames(dxa.d10.tob) <- c("SEQN","LS","TOB") 

# ~~~~~~~~~~~~~~~~ processing other years ~~~~~~~~~~~~~~~~~~~
#
#	DXXOSBMD - Total spine BMD
#   DXXOSBCC - Total spine BMD invalidity code

spn.vars <- c("SEQN","DXXOSBMD","DXXOSBCC","DXXL1BMD","DXXL2BMD","DXXL3BMD","DXXL4BMD")
dxa.d5.spn <- read.xport(sprintf("%sDXXSPN_E.xpt",dir.name))[,spn.vars]
ckx.d5 <- apply(dxa.d5.spn[,4:7],1,mean,na.rm=T)
dxa.d5.add <- cbind(dxa.d5.spn[,1],ckx.d5,rep(NA,nrow(dxa.d5.spn)))
colnames(dxa.d5.add) <- c("SEQN","LS","TOB") 

dxa.d6.spn <- read.xport(sprintf("%sDXXSPN_F.xpt",dir.2))[,spn.vars]
ckx.d6 <- apply(dxa.d6.spn[,4:7],1,mean,na.rm=T)
dxa.d6.add <- cbind(dxa.d6.spn[,1],ckx.d6,rep(NA,nrow(dxa.d6.spn)))
colnames(dxa.d6.add) <- c("SEQN","LS","TOB") 

dxa.d10.spn <- read.xport(sprintf("%sDXXSPN_J.xpt",dir.2))[,spn.vars]
ckx.d10 <- apply(dxa.d10.spn[,4:7],1,mean,na.rm=T)
dxa.d10.add <- cbind(dxa.d10.spn[,1],ckx.d10,rep(NA,nrow(dxa.d10.spn)))
colnames(dxa.d10.add) <- c("SEQN","LS","TOB")

ck.d10 <- match(dxa.d10.tob$SEQN,dxa.d10.add,nomatch=0)
dxa.d10.tob[ck.d10>0,"TOB"] <- dxa.d10.add[ck.d10,"TOB"]

dxa.add <- rbind(dxa.d5.add,dxa.d6.add,dxa.d7.tob,dxa.d8.tob,dxa.d9.tob,dxa.d10.tob)
colnames(dxa.add) <- colnames(d.dxa)

d.dxa <- rbind(d.dxa,dxa.add)
colnames(d.dxa) <- c("SEQN","LS","TOB")                  
dim(d.dxa)                                               # 67564

m.data <- merge(m.data,d.dxa,by="SEQN",all.x=T,as.is=T,na=c("NA","-9",7,9,99,777,999))
dim(m.data)                                             # 92062 


# ----------------------- add femoral -----------------------
#		DXXFMBCC - Total femur BMD invalidity code
#		DXXOFBMD - Total femur BMD
#		DXXNKBMD - Femoral neck BMD

fem.vars <- c("SEQN","DXXOFBMD","DXXNKBMD","DXXFMBCC")
dxa.d4.fem <- read.xport(sprintf("%sDXXFEM_D.xpt",dir.name))[,fem.vars]
dxa.d5.fem <- read.xport(sprintf("%sDXXFEM_E.xpt",dir.name))[,fem.vars]
dxa.d6.fem <- read.xport(sprintf("%sDXXFEM_F.xpt",dir.2))[,fem.vars]
dxa.d8.fem <- read.xport(sprintf("%sDXXFEM_H.xpt",dir.2))[,fem.vars]
dxa.d10.fem <- read.xport(sprintf("%sDXXFEM_J.xpt",dir.2))[,fem.vars]
dxa.add.fem <- rbind(dxa.d4.fem,dxa.d5.fem,dxa.d6.fem,dxa.d8.fem,dxa.d10.fem)
colnames(dxa.add.fem) <- c("SEQN","totfem","fneck","femck")

# table(dxa.add.fem$DXXFMBCC)    

m.data <- merge(m.data,dxa.add.fem,by="SEQN",all.x=T,as.is=T,na=c("NA","-9",7,9,99,777,999))
dim(m.data)                                            # 101316

# ==== ck hipFrc distribution =====
ck.dat <- m.data
ck.dat$hipFrc[m.data$hipFrc==7 |m.data$hipFrc==9 ] <- NA
ck.dat$hipFrc[m.data$hipFrc==2 ] <- 0
ck.dat$gender[m.data$gender==2 ] <- 0
ck.dat <- ck.dat[m.data$age>=50,]

table(ck.dat$gender,ck.dat$hipFrc)
table(ck.dat$gender,ck.dat$hipFrc)/nrow(ck.dat)*100
chisq.test(ck.dat$gender,ck.dat$hipFrc)


# ==== output ===== with valid fneck =====
out.data <- m.data[m.data$femck==0 & !is.na(m.data$femck),c(1,16:24)]
# 24970

write.table(out.data,"d3_9918_fem_bmd.txt",sep="\t",quote=F, col.names=T, row.names=T, append=F)




# ==== ck hipFrc distribution =====
ck.dat <- m.data[m.data$femck==0 & !is.na(m.data$femck),]
ck.dat$hipFrc[ck.dat$hipFrc==7 | ck.dat$hipFrc==9 ] <- NA
ck.dat$hipFrc[ck.dat$hipFrc==2 ] <- 0
ck.dat$gender[ck.dat$gender==2 ] <- 0                           
ck.dat <- ck.dat[ck.dat$age>=67,]                                 # 4513  (p=0.035)

table(ck.dat$gender,ck.dat$hipFrc)
table(ck.dat$gender,ck.dat$hipFrc)/nrow(ck.dat)*100
chisq.test(ck.dat$gender,ck.dat$hipFrc)




