
library(foreign)

d.demo <- read.table('0_demo_smk.txt',header=T,sep="\t")          # 92062

# ------------------------------------------------------------------------
#  * vitamin D *
#
#  LBXVIDMS - 2007-2014    (total = D2+D3)
#       conversion:  1 nmol/L = 0.40066 ng/mL
#  
#  LBDVIDMS - 2001-2006
#       see note for differences
#       https://wwwn.cdc.gov/nchs/nhanes/vitamind/analyticalnote.aspx
# 
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 


# ------------------------------------------------------------------------
vd.var <- "LBDVIDMS"
vd.var2 <- "LBXVIDMS"

vd.d2 <- read.xport("VID_B.xpt")[,c("SEQN",vd.var)]
vd.d3 <- read.xport("VID_C.xpt")[,c("SEQN",vd.var)]
vd.d4 <- read.xport("VID_D.xpt")[,c("SEQN",vd.var)]
vd.d24 <- rbind(vd.d2,vd.d3,vd.d4)
colnames(vd.d24) <- c("SEQN",vd.var2)

vd.d5 <- read.xport("VID_E.xpt")[,c("SEQN",vd.var2)]
vd.d6 <- read.xport("VID_F.xpt")[,c("SEQN",vd.var2)]
vd.d7 <- read.xport("VID_G.xpt")[,c("SEQN",vd.var2)]
vd.d8 <- read.xport("VID_H.xpt")[,c("SEQN",vd.var2)]
vd.d58 <- rbind(vd.d5,vd.d6,vd.d7,vd.d8)

dt.vitd <- rbind(vd.d24,vd.d58)


# ==== output =====
m.data <- merge(d.demo,dt.vitd,by="SEQN",all.x=T,as.is=T,na=c("NA","-9",7,9,99,777,999))
dim(m.data)                                              #  101316   

write.table(m.data,"d5_0114_vitd.txt",sep="\t",quote=F, col.names=T, row.names=T, append=F)


