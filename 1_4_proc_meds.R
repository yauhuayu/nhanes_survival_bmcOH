
library(foreign)

d.demo <- read.table('0_demo_smk.txt',header=T,sep="\t")          # 92062

# ------------------------------------------------------------------------
#  * medication *
#
#  RXD030 - taken prescription last month (1=yes, 2=no,)
#  RXD240B - standard generic ingredient name
#  NHCODE - standard generic ingredient code
#  RXD295 - number of prescription medicines taken
#  RXDDRGID - generic drug code
# 
# ~~~~~~~~~~~~~~~~~~~~~~~~~~ 
# colnames: c("RXDDRGID","RXDDRUG","RXDINGFL","RXDDCI1A","RXDDCI1B","RXDDCI1C","RXDDCN1A","RXDDCN1B","RXDDCN1C") 
# RXDINGFL: 1=single; 2=multiple ingredients
#
#  * bisphosphonate *
# 1 level ID: 358 = metabolic agents
# 2 level ID: 409 = bone resorption inhibitors
# 3 level ID: 217 = bisphosphonates
#
#  * miscellaneous *
# 3 level ID: 415 = bisphosphonates
#
# ------------------------------------------------------------------------
meds.info.vars <- c("RXDDRGID","RXDDRUG","RXDINGFL","RXDDCI1A","RXDDCI1B","RXDDCI1C",
          "RXDDCN1A","RXDDCN1B","RXDDCN1C") 
meds.info <- read.xport("RXQ_DRUG.xpt")[,meds.info.vars]

#**ind <- grep("BISPHOSPHONATE",unlist(meds.info$RXDDCN1C))             # 3rd level
#ind <- grep("BONE RESORPTION INHIBITOR",unlist(meds.info$RXDDCN1B))  # 2nd level
#meds.info[ind,c(1:7,9)]
#     RXDDRGID                        RXDDRUG RXDINGFL RXDDCI1A RXDDCI1B RXDDCI1C         RXDDCN1A                                 RXDDCN1C
#400    d00599                     ETIDRONATE        1      358      409      217 METABOLIC AGENTS                          BISPHOSPHONATES
#920    d03849                    ALENDRONATE        1      358      409      217 METABOLIC AGENTS                          BISPHOSPHONATES
#1061   d04300                    RISEDRONATE        1      358      409      217 METABOLIC AGENTS                          BISPHOSPHONATES
#1147   d04708                ZOLEDRONIC ACID        1      358      409      217 METABOLIC AGENTS                          BISPHOSPHONATES
#1268   d05357                    IBANDRONATE        1      358      409      217 METABOLIC AGENTS                          BISPHOSPHONATES
#1289   d05526   ALENDRONATE; CHOLECALCIFEROL        2      358      409      217 METABOLIC AGENTS                          BISPHOSPHONATES
#1297   d05630 CALCIUM CARBONATE; RISEDRONATE        2      358      409      217 METABOLIC AGENTS                          BISPHOSPHONATES
#1390   d07640                      DENOSUMAB        1      358      409      415 METABOLIC AGENTS MISCELLANEOUS BONE RESORPTION INHIBITORS

ck.drugs <- c("d00599","d03849","d04300","d04708","d05357","d05526","d05630","d07640")

meds.dt.vars <- c("RXD030","RXD240B","RXDDRGID","RXD295")
meds.dt.vars2 <- c("RXDUSE","RXDDRUG","RXDDRGID","RXDCOUNT","RXDDAYS")

meds.d1 <- read.xport("RXQ_RX.xpt")[,c("SEQN",meds.dt.vars)]
meds.d2 <- read.xport("RXQ_RX_B.xpt")[,c("SEQN",meds.dt.vars)]
meds.d12 <- rbind(meds.d1,meds.d2)
meds.d12 <- cbind(meds.d12,rep(NA,nrow(meds.d12)))
colnames(meds.d12) <- c("SEQN","RXDUSE","RXDDRUG","RXDDRGID","RXDCOUNT","RXDDAYS")
meds.d3 <- read.xport("RXQ_RX_C.xpt")[,c("SEQN",meds.dt.vars2)]
meds.d4 <- read.xport("RXQ_RX_D.xpt")[,c("SEQN",meds.dt.vars2)]
meds.d5 <- read.xport("RXQ_RX_E.xpt")[,c("SEQN",meds.dt.vars2)]
meds.d6 <- read.xport("RXQ_RX_F.xpt")[,c("SEQN",meds.dt.vars2)]
meds.d7 <- read.xport("RXQ_RX_G.xpt")[,c("SEQN",meds.dt.vars2)]
meds.d8 <- read.xport("RXQ_RX_H.xpt")[,c("SEQN",meds.dt.vars2)]
meds.d9 <- read.xport("RXQ_RX_I.xpt")[,c("SEQN",meds.dt.vars2)]
meds.d10 <- read.xport("RXQ_RX_J.xpt")[,c("SEQN",meds.dt.vars2)]

# ======
ck <- meds.d1[grep('d03849',unlist(meds.d1$RXDDRGID)),]       # 33

# ~~~~~~~~~ merge ~~~~~~~
dt.meds.0 <- rbind(meds.d12,meds.d3,meds.d4,meds.d5,meds.d6,meds.d7,meds.d8,meds.d9,meds.d10)
meds.pt <- unique(dt.meds.0$SEQN)
dt.meds.out <- data.frame(cbind(meds.pt,dt.meds.0$RXDCOUNT[match(meds.pt,dt.meds.0$SEQN)]))
colnames(dt.meds.out) <- c("SEQN","RXDCOUNT")

out.bispho <- rep(NA,nrow(dt.meds.out))
#out.bispho.days <- rep(NA,nrow(dt.meds.out))
out.meds.all <- rep(NA,nrow(dt.meds.out))
out.meds.lg <- rep(NA,nrow(dt.meds.out))

for (i in 1:nrow(dt.meds.out)) {
#for (i in 258:265) {
	ck.pt <- meds.pt[i]
	tmp <- dt.meds.0[dt.meds.0$SEQN==ck.pt,]
	print(c(ck.pt,nrow(tmp)))
	out.meds.lg[i] <- nrow(tmp)
	tmp.ids <- tmp$RXDDRGID
	tmp.ids[tmp.ids==""] <- "none"
	
	ck.bispho <- unlist(lapply(tmp.ids,grep,ck.drugs))
	
	if (length(ck.bispho)>0) {
		out.bispho[i] <- 1
	}	
	if (length(tmp.ids)==1 && tmp.ids=="none") {
		out.meds.lg[i] <- 0
		out.bispho[i] <- 0
	}
	#out.bispho.days[i] <- tmp$RXDDAYS[match(ck.drugs[ck.bispho],tmp$RXDDRGID)]
	out.meds.all[i] <- paste(tmp$RXDDRUG,collapse=";")		
}

out.collect <- cbind(dt.meds.out,out.bispho,out.meds.lg,out.meds.all)


# ==== output =====
m.data <- merge(d.demo,out.collect,by="SEQN",all.x=T,as.is=T,na=c("NA","-9",7,9,99,777,999))
dim(m.data)                                              #  101316   

write.table(m.data,"d4_9918_meds.txt",sep="\t",quote=F, col.names=T, row.names=T, append=F)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  cking ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
table(m.data$out.bispho)
#    0     1 
#61280   890
sum(is.na(m.data$out.bispho))
#[1] 39146

total 61280 did not take any meds
out of 40036 (39146+890) who took meds, 890 (2.2%) take any form of 'bone resorption inhibitor'

