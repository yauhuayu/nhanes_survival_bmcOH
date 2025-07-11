
# ------------------------------------------------------------------------
# *   dentition   *
# -- 1: primary, 2: permanent, 
#    3: implant, 4: missing,       5:r.r, 
#    9: not accessible,           "." missing
#
# ------------------------------------------------------------------------

library(foreign)
dir.name <- "./1999-2008/"
dir.2 <- "./2009-2018/"

t.num <- c("02","03","04","05","06","07","08","09","10","11","12","13","14","15","18","19","20","21","22","23","24","25","26","27","28","29","30","31")

dtc.vars <- c("SEQN","OHstatus",sprintf("dtc_%s",t.num))

dent.d1 <- read.xport(sprintf("%sOHXDENT.xpt",dir.name))[,c("SEQN","OHAEXSTS",sprintf("OHX%sTC",t.num))]
colnames(dent.d1) <- dtc.vars
dent.d2 <- read.xport(sprintf("%sOHXDEN_B.xpt",dir.name))[,c("SEQN","OHAEXSTS",sprintf("OHD%sTC",t.num))]
colnames(dent.d2) <- dtc.vars
dent.d3 <- read.xport(sprintf("%sOHXDEN_C.xpt",dir.name))[,c("SEQN","OHAEXSTS",sprintf("OHX%sTC",t.num))]
colnames(dent.d3) <- dtc.vars
dent.d4 <- read.xport(sprintf("%sOHX_D.xpt",dir.name))[,c("SEQN","OHAEXSTS",sprintf("OHX%sHTC",t.num))]
colnames(dent.d4) <- dtc.vars
dent.d5 <- read.xport(sprintf("%sOHX_E.xpt",dir.name))[,c("SEQN","OHAEXSTS",sprintf("OHX%sHTC",t.num))]
colnames(dent.d5) <- dtc.vars
dent.d6 <- read.xport(sprintf("%sOHXDEN_F.xpt",dir.2))[,c("SEQN","OHDEXSTS",sprintf("OHX%sTC",t.num))]
colnames(dent.d6) <- dtc.vars
dent.d7 <- read.xport(sprintf("%sOHXDEN_G.xpt",dir.2))[,c("SEQN","OHDEXSTS",sprintf("OHX%sTC",t.num))]
colnames(dent.d7) <- dtc.vars
dent.d8 <- read.xport(sprintf("%sOHXDEN_H.xpt",dir.2))[,c("SEQN","OHDEXSTS",sprintf("OHX%sTC",t.num))]
colnames(dent.d8) <- dtc.vars
dent.d9 <- read.xport(sprintf("%sOHXDEN_I.xpt",dir.2))[,c("SEQN","OHDEXSTS",sprintf("OHX%sTC",t.num))]
colnames(dent.d9) <- dtc.vars
dent.d10 <- read.xport(sprintf("%sOHXDEN_J.xpt",dir.2))[,c("SEQN","OHDEXSTS",sprintf("OHX%sTC",t.num))]
colnames(dent.d10) <- dtc.vars

d.dent <- rbind(dent.d1,dent.d2,dent.d3,dent.d4,dent.d5,dent.d6,dent.d7,dent.d8,dent.d9,dent.d10)   
table(d.dent$OHstatus)

d.dent <- d.dent[d.dent$OHstatus<3,]
dim(d.dent)									# 82292

d.dtc <- d.dent[,3:ncol(d.dent)]
d.imp <- d.dent[,3:ncol(d.dent)]

d.dtc[d.dtc ==99 | d.dtc ==9 | d.dtc==4 | d.dtc==5 | d.dtc=="." ] <- NA
d.dtc[d.dtc ==1 | d.dtc ==3 ] <- 0
d.dtc[d.dtc ==2] <- 1
tot.dtc <- apply(d.dtc,1,sum,na.rm=T)         

d.imp[d.imp ==99 | d.imp ==9 | d.imp==4 | d.imp==5 | d.imp=="." ] <- NA
d.imp[d.imp==1 | d.imp==2] <- 0
d.imp[d.imp==3] <- 1
tot.imp <- apply(d.imp,1,sum,na.rm=T)
length(tot.imp[tot.imp>0])     # 864

out.dent <- data.frame(cbind(d.dent$SEQN,d.dent$OHstatus,tot.dtc,tot.imp))
colnames(out.dent) <- c("SEQN","OHstatus",colnames(out.dent)[3:4])


# ------------------------------------------------------------------------
#  * process tooth count 
#
# ~~ output vars
# edentulous, tloss4, tls20
# tot.imp, tot.dtc, ant.dtc, post.dtc, inc.dtc, can.dtc, pre.dtc, mol.dtc
# uI.dtc, uC.dtc, uP.dtc, uM.dtc, lI.dtc, lC.dtc, lP.dtc, lM.dtc
# ~~
#
# ------------------------------------------------------------------------

edentulous <- rep(NA,nrow(out.dent))
edentulous[tot.dtc > 0 & !is.na(tot.dtc)] <-0
edentulous[tot.dtc ==0 & !is.na(tot.dtc)] <-1

tloss4 <- rep(NA,nrow(out.dent))
tloss4[tot.dtc >0 & !is.na(tot.dtc)] <-1
tloss4[tot.dtc >=24 & !is.na(tot.dtc)] <-0
tloss4[edentulous==1] <- NA

tls20 <-rep(NA,nrow(out.dent))
tls20[tot.dtc>0 & !is.na(tot.dtc)] <-1
tls20[tot.dtc>=20 & !is.na(tot.dtc)] <-0
tls20[edentulous==1] <- NA

ant.dtc <- apply(d.dtc[,c(5:10,19:24)],1,sum,na.rm=T)    
post.dtc <- apply(d.dtc[,c(1:4,11:18,25:28)],1,sum,na.rm=T) 
inc.dtc <- apply(d.dtc[,c(6:9,20:23)],1,sum,na.rm=T) 
can.dtc <- apply(d.dtc[,c(5,10,19,24)],1,sum,na.rm=T) 
pre.dtc <- apply(d.dtc[,c(3,4,11,12,17,18,25,26)],1,sum,na.rm=T) 
mol.dtc <- apply(d.dtc[,c(1,2,13:16,27,28)],1,sum,na.rm=T) 

uI.dtc <- apply(d.dtc[,c(6:9)],1,sum,na.rm=T) 
uC.dtc <- apply(d.dtc[,c(5,10)],1,sum,na.rm=T) 
uP.dtc <- apply(d.dtc[,c(3,4,11,12)],1,sum,na.rm=T) 
uM.dtc <- apply(d.dtc[,c(1,2,13,14)],1,sum,na.rm=T) 
lI.dtc <- apply(d.dtc[,c(20:23)],1,sum,na.rm=T) 
lC.dtc <- apply(d.dtc[,c(19,24)],1,sum,na.rm=T) 
lP.dtc <- apply(d.dtc[,c(17,18,25,26)],1,sum,na.rm=T)  
lM.dtc <- apply(d.dtc[,c(15,16,27,28)],1,sum,na.rm=T) 

out.dent <- cbind(out.dent,edentulous,tloss4,tls20)
out.dent <- cbind(out.dent,ant.dtc, post.dtc, inc.dtc, can.dtc, pre.dtc, mol.dtc)
out.dent <- cbind(out.dent,uI.dtc, uC.dtc, uP.dtc, uM.dtc, lI.dtc, lC.dtc, lP.dtc, lM.dtc)

ck.gr <- out.dent$SEQN
ck.gr[out.dent$SEQN<=9965] <-1
ck.gr[out.dent$SEQN>9965 & out.dent$SEQN <=21004] <-2
ck.gr[out.dent$SEQN>21004 & out.dent$SEQN <=31126] <-3
ck.gr[out.dent$SEQN>31126 & out.dent$SEQN <=41474] <-4
ck.gr[out.dent$SEQN>41474 & out.dent$SEQN <=51623] <-5
ck.gr[out.dent$SEQN>51623 & out.dent$SEQN <=62160] <-6
ck.gr[out.dent$SEQN>62160 & out.dent$SEQN <=71916] <-7
ck.gr[out.dent$SEQN>71916 & out.dent$SEQN <=83731] <-8
ck.gr[out.dent$SEQN>83731 & out.dent$SEQN <=93702] <-9
ck.gr[out.dent$SEQN>93702 & out.dent$SEQN <=102956] <-10


out.dent <- cbind(out.dent,ck.gr)

# ------------------------------------------------------------------------
# ** oral health questionnaire **                                                                       ------  *  OHQ  * ------
#
# OHQ030 - When did you last visit a dentist?   
#          1=<6m;2=6m-1y;3=1-2yrs;4=2-3yrs;5=3-5yrs;6=5+yrs;7=never;77=refused;99=DK;"."
# OHQ040 - how frequent
#          # 1: more, 2: once per yr, 3: less, 4: sporadic
#
# OHQ835 - Do you think you might have gum disease?
# OHQ845 - Rate the health of your teeth and gums   (1:excellent;2:very good;3:good;4:fair;5:poor)
# OHQ850 - Ever had treatment for gum disease?
# OHQ860 - Ever been told of bone loss around teeth
# OHQ870 - How many days use dental floss/device
#
# ------------------------------------------------------------------------

# --== OHQ vars ==--

ohq.vars <- c("SEQN","OHQ030")
ohq.d1 <- read.xport(sprintf("%sOHQ.xpt",dir.name))[,c("SEQN","OHQ030")]
ohq.d2 <- read.xport(sprintf("%sOHQ_B.xpt",dir.name))[,c("SEQN","OHQ030")]
ohq.d3 <- read.xport(sprintf("%sOHQ_C.xpt",dir.name))[,c("SEQN","OHQ030")]
ohq.d7 <- read.xport(sprintf("%sOHQ_G.xpt",dir.2)) [,c("SEQN","OHQ030")]
ohq.d8 <- read.xport(sprintf("%sOHQ_H.xpt",dir.2)) [,c("SEQN","OHQ030")]
ohq.d9 <- read.xport(sprintf("%sOHQ_I.xpt",dir.2)) [,c("SEQN","OHQ030")]
ohq.d10 <- read.xport(sprintf("%sOHQ_J.xpt",dir.2)) [,c("SEQN","OHQ030")]

d.ohq <- rbind(ohq.d1,ohq.d2,ohq.d3,ohq.d7,ohq.d8,ohq.d9,ohq.d10)
colnames(d.ohq) <- c("SEQN","dentvisit")

d.ohq$dentvisit[d.ohq$dentvisit==9] <- NA
d.ohq$dentvisit[d.ohq$dentvisit==77] <- NA
d.ohq$dentvisit[d.ohq$dentvisit==99] <- NA
d.ohq$dentvisit[d.ohq$dentvisit<=2 & !is.na(d.ohq$dentvisit)] <- 0
d.ohq$dentvisit[d.ohq$dentvisit>=3 & !is.na(d.ohq$dentvisit)] <- 1        # last dentvisit > 1 year


ohq.vars2 <- c("SEQN","OHQ835","OHQ845","OHQ850","OHQ860","OHQ870")
ohq.vars2B <- c("SEQN","selfPD","selfOH","SCRP","BLteeth","fqFloss")
ohq.d6 <- read.xport(sprintf("%sOHQ_F.xpt",dir.2)) [,ohq.vars2]
ohq.d7 <- read.xport(sprintf("%sOHQ_G.xpt",dir.2)) [,ohq.vars2]
ohq.d8 <- read.xport(sprintf("%sOHQ_H.xpt",dir.2)) [,ohq.vars2]
ohq.d9 <- read.xport(sprintf("%sOHQ_I.xpt",dir.2)) [,ohq.vars2]
ohq.d10b <- read.xport(sprintf("%sOHQ_J.xpt",dir.2)) [,ohq.vars2]

d2.ohq <- rbind(ohq.d6,ohq.d7,ohq.d8,ohq.d9,ohq.d10b)
colnames(d2.ohq) <- ohq.vars2B

# --- loose teeth
lt.d6 <- read.xport(sprintf("%sOHQ_F.xpt",dir.2)) [,c("SEQN","OHQ855")]
lt.d7 <- read.xport(sprintf("%sOHQ_G.xpt",dir.2)) [,c("SEQN","OHQ855")]
lt.d8 <- read.xport(sprintf("%sOHQ_H.xpt",dir.2)) [,c("SEQN","OHQ855")]
lt.tmp <- rbind(lt.d6,lt.d7,lt.d8)
colnames(lt.tmp) <- c("SEQN","looseT")

d2.ohq <- merge(d2.ohq,lt.tmp,by="SEQN",all.x=T,as.is=T,na=c("NA","NaN",".","99","9"))
d2.ohq.seqn <- d2.ohq$SEQN
d2.ohq[d2.ohq==9 | d2.ohq==77 | d2.ohq==99 | d2.ohq=="."] <- NA
d2.ohq$SEQN <- d2.ohq.seqn

m.data <- merge(out.dent,d.ohq,by="SEQN",all.x=T,as.is=T,na=c("NA","NaN",".","99","9"))
m.data <- merge(m.data,d2.ohq,by="SEQN",all.x=T,as.is=T,na=c("NA","NaN",".","99","9"))
# 82292 29

out2 <- m.data[,c(1:13,21:29)]
write.table(out2,"d2_9918_dent_ohq.txt",sep="\t",quote=F, col.names=T, row.names=T, append=F)






# ====================================================================================================================

# ====================================================================================================================

# ====================================================================================================================


# ------------------------------------------------------------------------
# *   1999-2004 Periodontal Examination Data   *
#  
#     Num-LAS(MF); Num-LAD(DF);
#     Num-LAM(mid-facial); 
#     for probing:  PCD, PCS, PCM
# ------------------------------------------------------------------------

# ------ ~~ function ~ ------------
mk.ical <-function(list) {
	ans <-sort(c(sprintf("OHD%sLAD",list),sprintf("OHD%sLAS",list)))	
	return(ans)
}

# ------ ~~ function ~ ------------
mk.ipd <-function(list) {
	ans <-sort(c(sprintf("OHD%sPCD",list),sprintf("OHD%sPCS",list)))	
	return(ans)
}

get.t.max <-function(list,cknames) {
  out <- rep(NA,length(t.num))
  str <- as.numeric(as.character(list))
  
  for (i in 1:length(t.num)) {
  	ind <- grep(t.num[i],cknames)
  	tmp <- str[ind]	
  	ck <- sum(is.na(tmp))
  	ck.lg <- length(tmp)
  	ans <- NA
  	if (ck==ck.lg) {
		ans <- NA
	}
	if (ck<ck.lg) {
		ans <- max(tmp,na.rm=T)
	}
	out[i] <- ans
  }	
  print(out)
  return(out)
}

get.pt.pr <- function(list) {
	ans <- c()
	ck <- !is.na(list)
	tmp <- list[ck==T]
	if(sum(ck)>0) {
	    t.ical3 <- length(tmp[tmp>=3])
	    t.ical4 <- length(tmp[tmp>=4])
	    t.ical5 <- length(tmp[tmp>=5])
	    t.ical6 <- length(tmp[tmp>=6])
	    t.ical7 <- length(tmp[tmp>=7])
	    t.pr <- c(t.ical3,t.ical4,t.ical5,t.ical6,t.ical7)/length(tmp)*100
	    t.pr <- format(t.pr,digit=2)
	    t.ct <- c(t.ical3,t.ical4,t.ical5,t.ical6,t.ical7)
	}
	if(sum(ck)==0) {
		t.pr <- c(NA,NA,NA,NA,NA)
		t.ct <- c(NA,NA,NA,NA,NA)
	}
    ans$pr <- t.pr
	ans$ct <- t.ct
	return(ans)
}


get.pt.mxGr <- function(list) {
	ck <- !is.na(list)
	tmp <- list[ck==T]
	t.ind <- 1:28
	t.ind <- t.ind[ck==T]
	
	i.u.inc <- c(6,7,8,9)
	i.u.can <- c(5,10)
	i.u.pre <- c(3,4,11,12)
	i.u.mol <- c(1,2,13,14)
	i.l.inc <- c(20,21,22,23)
	i.l.can <- c(19,24)
	i.l.pre <- c(17,18,25,26)
	i.l.mol <- c(15,16,27,28)
	
	i.inc <- c(i.u.inc,i.l.inc)
	i.can <- c(i.u.can,i.l.can)
	i.pre <- c(i.u.pre,i.l.pre)
	i.mol <- c(i.u.mol,i.l.mol)
	i.ant <- c(i.inc,i.can)
	i.post <- c(i.pre,i.mol)
	i.cusp <- c(i.can,i.pre)
	
	i.ind <- list(i.u.inc,i.u.can,i.u.pre,i.u.mol,i.l.inc,i.l.can,i.l.pre,i.l.mol,i.inc,i.can,i.pre,i.mol,i.ant,i.post,i.cusp)
	i.name <- c("uI","uC","uPre","uM","lI","lC","lPre","lM","Inc","Can","Pre","Mol","Ant","Post","Cusp")
	
	ans <- list()
    for (i in 1:length(i.name)) {
    	gr.ind <- i.ind[[i]]
    	gr.ck <- t.ind %in% gr.ind
    	if (sum(gr.ck)>0) {
    	     ans[[i]] <- max(tmp[gr.ck])
    	}
    	if (sum(gr.ck)==0){
			ans[[i]] <- NA
    	}
    }
	names(ans) <- i.name
	return(ans)
}

# ~~ end of function

# ------------------------------------------------------------------------


# -- 1999-2000 --- read in *
ical.vars <- c("SEQN",sprintf("OHD%sLAS",t.num))
g.rec.fvars <- c("SEQN",sprintf("OHD%sLAM",t.num))   # use CAL
ipd.vars <- c("SEQN",sprintf("OHD%sPCS",t.num)) 

ical.d1 <- read.xport(sprintf("%sOHXPERIO.xpt",dir.name))[,ical.vars] 
ipd.d1 <- read.xport(sprintf("%sOHXPERIO.xpt",dir.name))[,ipd.vars] 
fgrec.d1 <- read.xport(sprintf("%sOHXPERIO.xpt",dir.name))[,g.rec.fvars] 

# -- 2001-2004 --- read in *
ical.vars <- c("SEQN",mk.ical(t.num))
ipd.vars <- c("SEQN",mk.ipd(t.num))
g.rec.fvars <- c("SEQN",sprintf("OHD%sLAM",t.num))   # use CAL

pr.d2 <- merge(read.xport(sprintf("%sOHXPRU_B.xpt",dir.name)),read.xport(sprintf("%sOHXPRL_B.xpt",dir.name)),by="SEQN")
ical.d2 <- pr.d2[,ical.vars] 
ipd.d2 <- pr.d2[,ipd.vars] 
fgrec.d2 <- pr.d2[,g.rec.fvars] 

pr.d3 <- merge(read.xport(sprintf("%sOHXPRU_C.xpt",dir.name)),read.xport(sprintf("%sOHXPRL_C.xpt",dir.name)),by="SEQN")
ical.d3 <- pr.d3[,ical.vars] 
ipd.d3 <- pr.d3[,ipd.vars] 
fgrec.d3 <- pr.d3[,g.rec.fvars] 


# ----------- NOT USED ------------
# d.grec <- rbind(fgrec.d1,fgrec.d2,fgrec.d3)
# d.grec[d.grec==99] <- NA
# mean.grec <- apply(d.grec[,2:ncol(d.grec)],1,mean,na.rm=T)
# d.grec[mean.grec==12 & !is.na(mean.grec),]

# ~~~~~~~~~~~~~~~~~~~~ processing ~~~~~~~~~~~~~~~~~~~~
# ---- 1999-2000
ical.d1[ical.d1==99] <- NA
tmp.d1 <- unlist(ical.d1[,2:ncol(ical.d1)])
dim(tmp.d1) <- c(nrow(ical.d1),28)

ipd.d1[ipd.d1==99] <- NA
t.pd.d1 <- unlist(ipd.d1[,2:ncol(ipd.d1)])
dim(t.pd.d1) <- c(nrow(ipd.d1),28)

# ---- 2001-2004
ical.d4 <- rbind(ical.d2,ical.d3)
ical.d4[ical.d4==99] <- NA
# sum(ical.d4>3&!is.na(ical.d4))        # 
tmp.d4 <-ical.d4[,2:ncol(ical.d4)]

ck.names <- colnames(tmp.d4)
ck.ical.d5 <-apply(tmp.d4,1,get.t.max,ck.names)
tmp.d5 <- t(ck.ical.d5)

tmp.d6 <- rbind(tmp.d1,tmp.d5)
dim(tmp.d6)                      # 20606                  # should be the same as ical.d1,ical.d2,ical.d3
tmp.seqn <- c(ical.d1$SEQN,ical.d2$SEQN,ical.d3$SEQN)



# -------- ipd -------
ipd.d4 <- rbind(ipd.d2,ipd.d3)
ipd.d4[ipd.d4==99] <- NA       
t.pd.d4 <-ipd.d4[,2:ncol(ipd.d4)]

ck.names <- colnames(t.pd.d4)
ck.pd.d5 <- apply(t.pd.d4,1,get.t.max,ck.names)
t.pd.d5 <- t(ck.pd.d5)

t.pd.d6 <- rbind(t.pd.d1,t.pd.d5)
dim(t.pd.d6)                      # 20606                  # should be the same as ical.d1,ical.d2,ical.d3
t.pd.seqn <- c(ipd.d1$SEQN,ipd.d2$SEQN,ipd.d3$SEQN)



# --- ~~ for each patient ~~ --- max iCal, group max, pr3mm, pr5mm
#
pr.ical <- array(NA,dim=c(nrow(tmp.d6),5))
ct.ical <- array(NA,dim=c(nrow(tmp.d6),5))

i.name <- c("uI","uC","uPre","uM","lI","lC","lPre","lM","Inc","Can","Pre","Mol","Ant","Post","Cusp")
mxGr.ical <- array(NA,dim=c(nrow(tmp.d6),length(i.name)))
tmp.pt.mx <- rep(NA,nrow(tmp.d6))

for (r in 1:nrow(tmp.d6)) {
	tmp.in <- tmp.d6[r,]	
	tmp.pt.pr <- get.pt.pr(tmp.in)$pr
	tmp.pt.ct <- get.pt.pr(tmp.in)$ct
	pr.ical[r,] <- as.numeric(as.character(tmp.pt.pr))
	ct.ical[r,] <- as.numeric(as.character(tmp.pt.ct))
	tmp.pt.mxgr <- get.pt.mxGr(tmp.in)
	mxGr.ical[r,] <- unlist(tmp.pt.mxgr)
	tmp.list <- unlist(tmp.pt.mxgr)
	
	ck <- !is.na(tmp.list)
	if(sum(ck)>0) {
			tmp.pt.mx[r] <- max(as.numeric(tmp.list[ck])) }		
}


out.ical1 <- data.frame(cbind(tmp.seqn,pr.ical,ct.ical,tmp.pt.mx,mxGr.ical))
colnames(out.ical1) <- c("SEQN","ical3pr","ical4pr","ical5pr","ical6pr","ical7pr","ical3ct","ical4ct","ical5ct","ical6ct","ical7ct","mx_piCAL",sprintf("mx_%s",i.name))
sum(!is.na(out.ical1$mx_piCAL))    # 



# --- ~~ for each patient ~~ --- max iPD, group max, pr3mm, pr5mm
#
pr.ipd <- array(NA,dim=c(nrow(t.pd.d6),5))
ct.ipd <- array(NA,dim=c(nrow(t.pd.d6),5))

i.name <- c("uI","uC","uPre","uM","lI","lC","lPre","lM","Inc","Can","Pre","Mol","Ant","Post","Cusp")
mxGr.ipd <- array(NA,dim=c(nrow(t.pd.d6),length(i.name)))
tmp.pt.mx <- rep(NA,nrow(t.pd.d6))

for (r in 1:nrow(t.pd.d6)) {
	tmp.in <- t.pd.d6[r,]	
	tmp.pt.pr <- get.pt.pr(tmp.in)$pr
	tmp.pt.ct <- get.pt.pr(tmp.in)$ct
	pr.ipd[r,] <- as.numeric(as.character(tmp.pt.pr))
	ct.ipd[r,] <- as.numeric(as.character(tmp.pt.ct))

	tmp.pt.mxgr <- get.pt.mxGr(as.numeric(as.character(tmp.in)))
	mxGr.ipd[r,] <- unlist(tmp.pt.mxgr)
	tmp.list <- unlist(tmp.pt.mxgr)
	
	ck <- !is.na(tmp.list)
	if(sum(ck)>0) {
			tmp.pt.mx[r] <- max(as.numeric(tmp.list[ck])) }		
}


out.ipd1 <- data.frame(cbind(t.pd.seqn,pr.ipd,ct.ipd,tmp.pt.mx,mxGr.ipd))
colnames(out.ipd1) <- c("SEQN","ipd3pr","ipd4pr","ipd5pr","ipd6pr","ipd7pr","ipd3ct","ipd4ct","ipd5ct","ipd6ct","ipd7ct","mx_piPD",sprintf("mxPD_%s",i.name))
sum(!is.na(out.ipd1$mx_piPD))    



# ~~~~~~~~~~ Figure ~~~~~~~~ redundant ck ~~~~~~~~~~~~~~~~~~~~
# ~~~ 
a = out.ical1$mx_piCAL      # na should be the same
b = out.ipd1$mx_piPD
ck.diff = a-b

d.plot <- as.data.frame(cbind(a,b,ck.diff))
d.plot <- d.plot[!is.na(ck.diff),]
ind.type <- rep(round(d.plot$ck.diff))
ind.type[d.plot$ck.diff <= -2] <- "-2 <"
ind.type[d.plot$ck.diff == -2] <- "-2"
ind.type[d.plot$ck.diff == -1] <- "-1"
ind.type[d.plot$ck.diff == 0] <- "0"
ind.type[d.plot$ck.diff<=4 & d.plot$ck.diff>=1] <-  "1-4"
ind.type[d.plot$ck.diff >= 5] <- ">=5"

ind.diff <- ind.type
ind.diff[ind.type=="-2 <"] <- 1
ind.diff[ind.type=="-2"] <- 2
ind.diff[ind.type=="-1"] <- 3
ind.diff[ind.type=="0"] <- 4
ind.diff[ind.type=="1-4"] <- 5
ind.diff[ind.type==">=5"] <- 6

d.plot <- cbind(d.plot,ind.type,ind.diff)

postscript(file="fig_ck_ipd-ical.ps",paper="letter",horizontal=T)

op <- par(mfrow = c(2, 2))	

# ~~~~~~ [a]  histogram
set.seed(2020)
p1 <- hist(a,main="iCAL")                
p2 <- hist(b,main="iPD")  
p3 <- hist(ck.diff,main="difference")  

# ~~~~~~ [a] ipd ~ ical
plot(a~b,data=d.plot,type="n",cex=1,
       main="correlation of ipd & ical",ylab="mx_piCAL", xlab="mx_piPD",
       frame=F,axes=F,cex.main=1)	
usr <- par("usr")
par(usr=c(usr[1:2],0, 15))
axis(2,at=c(0,2,4,6,8,10,12,15),cex.axis=0.8,las=2)
axis(1,at=c(0,2,4,6,8,10,12,15),cex.axis=0.8,las=2)
legend("topright",c("-2 <","-2","-1","0","1-4",">=5"),pch=c(16,16,16,16,16,16),cex=0.6,bty="n",col=1:6,lty=c(1,2,3,1,2,3))
legend("topleft",c("corr coefficient = 0.54","p<.001"),bty="n")

points(a~b,data=d.plot,pch=16,col=ind.diff,cex=0.6)
lmfit.i <- lm(a~b,data=d.plot)
abline(lmfit.i$coefficient[1],lmfit.i$coefficient[2],col="darkgray",lwd=1.1,lty=1)                

dev.off()



# ------------------------------------------------------------------------
# *   2009-2014 Periodontal Examination Data   *
#  
#     Num-LAS(MF); Num-LAD(DF); Num-LAA(ML); Num-LAP(DL); 
#
#     Num-LAM(mid-facial); Num-LAL(mid-lingual)
#
# ------------------------------------------------------------------------

# ~~ function 
mk.ical <-function(list) {
	ans <-sort(c(sprintf("OHX%sLAD",list),sprintf("OHX%sLAS",list),sprintf("OHX%sLAP",list),sprintf("OHX%sLAA",list)))	
	return(ans)
}

mk.ipd <-function(list) {
	ans <-sort(c(sprintf("OHX%sPCD",list),sprintf("OHX%sPCS",list),sprintf("OHX%sPCP",list),sprintf("OHX%sPCA",list)))	
	return(ans)
}

get.t.max.ical <-function(list) {
	ck <- sum(is.na(list))
	ans <- NA
	if (ck==4) {
		ans <- NA
	}
	if (ck<4) {
		ans <- max(list)
	}
	return(ans)
}

# ~~ end of function
# ------------------------------------------------------------------------

# -- 2009-2014 --- read in *

ical.vars <- c("SEQN",mk.ical(t.num))
ipd.vars <- c("SEQN",mk.ipd(t.num))
g.rec.fvars <- c("SEQN",sprintf("OHX%sLAM",t.num))
g.rec.lvars <- c("SEQN",sprintf("OHX%sLAL",t.num))

ical.t1 <- read.xport(sprintf("%sOHXPER_F.xpt",dir.2))[,ical.vars] 
ipd.t1 <- read.xport(sprintf("%sOHXPER_F.xpt",dir.2))[,ipd.vars] 
fgrec.t1 <- read.xport(sprintf("%sOHXPER_F.xpt",dir.2))[,g.rec.fvars] 
lgrec.t1 <- read.xport(sprintf("%sOHXPER_F.xpt",dir.2))[,g.rec.lvars] 

ical.t2 <- read.xport(sprintf("%sOHXPER_G.xpt",dir.2))[,ical.vars] 
ipd.t2 <- read.xport(sprintf("%sOHXPER_G.xpt",dir.2))[,ipd.vars] 
fgrec.t2 <- read.xport(sprintf("%sOHXPER_G.xpt",dir.2))[,g.rec.fvars] 
lgrec.t2 <- read.xport(sprintf("%sOHXPER_G.xpt",dir.2))[,g.rec.lvars] 

ical.t3 <- read.xport(sprintf("%sOHXPER_H.xpt",dir.2))[,ical.vars] 
ipd.t3 <- read.xport(sprintf("%sOHXPER_H.xpt",dir.2))[,ipd.vars] 
fgrec.t3 <- read.xport(sprintf("%sOHXPER_H.xpt",dir.2))[,g.rec.fvars] 
lgrec.t13<- read.xport(sprintf("%sOHXPER_H.xpt",dir.2))[,g.rec.lvars] 

d0914.ical <- rbind(ical.t1,ical.t2,ical.t3)
d0914.ipd <- rbind(ipd.t1,ipd.t2,ipd.t3)
d0914.fgrec <- rbind(fgrec.t1,fgrec.t2,fgrec.t3)

d0914.ical[d0914.ical==99] <- NA
d0914.ipd[d0914.ipd==99] <- NA


# ~~~~~~~~~~~~~~~~~~~~ processing ~~~~~~~~~~~~~~~~~~~~~  iCAL ~~~~~~~~~~~~~

d.use <- d0914.ical[,2:ncol(d0914.ical)]
ck.names <- colnames(d.use)
ck.ical <- apply(d.use,1,get.t.max,ck.names)
tmp.ical <- t(ck.ical)


# --- ~~ for each patient ~~ --- max iCal, group max, pr3mm, pr5mm
#
pr.ical <- array(NA,dim=c(nrow(tmp.ical),5))
ct.ical <- array(NA,dim=c(nrow(tmp.ical),5))

i.name <- c("uI","uC","uPre","uM","lI","lC","lPre","lM","Inc","Can","Pre","Mol","Ant","Post","Cusp")
mxGr.ical <- array(NA,dim=c(nrow(tmp.ical),length(i.name)))
tmp.pt.mx <- rep(NA,nrow(tmp.ical))

for (r in 1:nrow(tmp.ical)) {
	tmp.in <- tmp.ical[r,]	
	tmp.pt.pr <- get.pt.pr(tmp.in)$pr
	tmp.pt.ct <- get.pt.pr(tmp.in)$ct
	pr.ical[r,] <- as.numeric(as.character(tmp.pt.pr))
	ct.ical[r,] <- as.numeric(as.character(tmp.pt.ct))
	tmp.pt.mxgr <- get.pt.mxGr(tmp.in)
	mxGr.ical[r,] <- unlist(tmp.pt.mxgr)
	tmp.list <- unlist(tmp.pt.mxgr)
	
	ck <- !is.na(tmp.list)
	if(sum(ck)>0) {
			tmp.pt.mx[r] <- max(as.numeric(tmp.list[ck])) }		
}

out.ical0914 <- data.frame(cbind(d0914.ical$SEQN,pr.ical,ct.ical,tmp.pt.mx,mxGr.ical))
colnames(out.ical0914) <- c("SEQN","ical3pr","ical4pr","ical5pr","ical6pr","ical7pr","ical3ct","ical4ct","ical5ct","ical6ct","ical7ct","mx_piCAL",sprintf("mx_%s",i.name))
sum(!is.na(out.ical0914$mx_piCAL))    # 



# ~~~~~~~~~~~~~~~~~~~~ processing ~~~~~~~~~~~~~~~~~~~~~  iPD ~~~~~~~~~~~~~

d.use <- d0914.ipd[,2:ncol(d0914.ipd)]
ck.names <- colnames(d.use)
ck.ipd <- apply(d.use,1,get.t.max,ck.names)
tmp.ipd <- t(ck.ipd)


# --- ~~ for each patient ~~ --- max iPD, group max, pr3mm, pr5mm
#
pr.ipd <- array(NA,dim=c(nrow(tmp.ipd),5))
ct.ipd <- array(NA,dim=c(nrow(tmp.ipd),5))

i.name <- c("uI","uC","uPre","uM","lI","lC","lPre","lM","Inc","Can","Pre","Mol","Ant","Post","Cusp")
mxGr.ipd <- array(NA,dim=c(nrow(tmp.ipd),length(i.name)))
tmp.pt.mx <- rep(NA,nrow(tmp.ipd))

for (r in 1:nrow(tmp.ipd)) {
	tmp.in <- tmp.ipd[r,]	
	tmp.pt.pr <- get.pt.pr(tmp.in)$pr
	tmp.pt.ct <- get.pt.pr(tmp.in)$ct
	pr.ipd[r,] <- as.numeric(as.character(tmp.pt.pr))
	ct.ipd[r,] <- as.numeric(as.character(tmp.pt.ct))
	tmp.pt.mxgr <- get.pt.mxGr(tmp.in)
	mxGr.ipd[r,] <- unlist(tmp.pt.mxgr)
	tmp.list <- unlist(tmp.pt.mxgr)
	
	ck <- !is.na(tmp.list)
	if(sum(ck)>0) {
			tmp.pt.mx[r] <- max(as.numeric(tmp.list[ck])) }		
}

out.ipd0914 <- data.frame(cbind(d0914.ipd$SEQN,pr.ipd,ct.ipd,tmp.pt.mx,mxGr.ipd))
colnames(out.ipd0914) <- c("SEQN","ipd3pr","ipd4pr","ipd5pr","ipd6pr","ipd7pr","ipd3ct","ipd4ct","ipd5ct","ipd6ct","ipd7ct","mx_piPD",sprintf("mxPD_%s",i.name))
sum(!is.na(out.ipd0914$mx_piPD))    # 



# ~~~~~~~~~~ Figure ~~~~~~~~ redundant ck ~~~~~~~~~~~~~~~~~~~~
# ~~~ 
a = out.ical0914$mx_piCAL      # na should be the same
b = out.ipd0914$mx_piPD
ck.diff = a-b
table(round(ck.diff))

# one data with mistake in raw input (SEQN=73723; that there is one "0" in the "OHX02LAD", else is NA)
f.ind = rep(0,length(a))
f.ind[is.na(b)] <- 2
f.ind[is.na(a)] <- 1
out.ical0914[f.ind==2,]
out.ipd0914[f.ind==2,]
d0914.ipd[f.ind==2,]
d0914.ical[f.ind==2,]
r = match("73723",d0914.ical$SEQN)    # row 9485    
m.data[m.data$SEQN=="73723",]
ical.t3[match("73723",as.character(ical.t3$SEQN)),]

# ~~~~~~~~~~ Figure ~~~~~~~~ redundant ck ~~~~~~~~~~~~~~~~~~~~
# correct data
#
out.ical0914[f.ind==2,2:ncol(out.ical0914)] <- NA
sum(!is.na(out.ical0914$mx_piCAL)) 
sum(!is.na(out.ipd0914$mx_piPD)) 


d.plot <- as.data.frame(cbind(a,b,ck.diff))
d.plot <- d.plot[!is.na(ck.diff),]
ind.type <- rep(round(d.plot$ck.diff))
ind.type[d.plot$ck.diff <= -2] <- "-2 <"
ind.type[d.plot$ck.diff == -2] <- "-2"
ind.type[d.plot$ck.diff == -1] <- "-1"
ind.type[d.plot$ck.diff == 0] <- "0"
ind.type[d.plot$ck.diff<=4 & d.plot$ck.diff>=1] <-  "1-4"
ind.type[d.plot$ck.diff >= 5] <- ">=5"

ind.diff <- ind.type
ind.diff[ind.type=="-2 <"] <- 1
ind.diff[ind.type=="-2"] <- 2
ind.diff[ind.type=="-1"] <- 3
ind.diff[ind.type=="0"] <- 4
ind.diff[ind.type=="1-4"] <- 5
ind.diff[ind.type==">=5"] <- 6

d.plot <- cbind(d.plot,ind.type,ind.diff)

postscript(file="fig_ck0914_ipd-ical.ps",paper="letter",horizontal=T)

op <- par(mfrow = c(2, 2))	

# ~~~~~~ [a]  histogram
set.seed(2020)
p1 <- hist(a,main="iCAL")                
p2 <- hist(b,main="iPD")  
p3 <- hist(ck.diff,main="difference")  

# ~~~~~~ [a] ipd ~ ical
plot(a~b,data=d.plot,type="n",cex=1,
       main="correlation of ipd & ical",ylab="mx_piCAL", xlab="mx_piPD",
       frame=F,axes=F,cex.main=1)	
usr <- par("usr")
par(usr=c(usr[1:2],0, 15))
axis(2,at=c(0,2,4,6,8,10,12,15),cex.axis=0.8,las=2)
axis(1,at=c(0,2,4,6,8,10,12,15),cex.axis=0.8,las=2)
legend("topright",c("-2 <","-2","-1","0","1-4",">=5"),pch=c(16,16,16,16,16,16),cex=0.6,bty="n",col=1:6,lty=c(1,2,3,1,2,3))
legend("topleft",c("corr coefficient = 0.70","p<.001"),bty="n")

points(a~b,data=d.plot,pch=16,col=ind.diff,cex=0.6)
lmfit.i <- lm(a~b,data=d.plot)
abline(lmfit.i$coefficient[1],lmfit.i$coefficient[2],col="darkgray",lwd=1.1,lty=1)                

dev.off()


# ------------------------------------------------------------------------

# ~~ end 2009-2014 processing
# ------------------------------------------------------------------------

out.ical1 <- out.ical1[!is.na(out.ical1$mx_piCAL),]
out.ipd1 <- out.ipd1[!is.na(out.ipd1$mx_piPD),]
out.ical0914 <- out.ical0914[!is.na(out.ical0914$mx_piCAL),]
out.ipd0914 <- out.ipd0914[!is.na(out.ipd0914$mx_piPD),]

# ** total data from perio exam
# out.ical1, out.ipd1 [1999-2004]
# out.ical0914, out.ipd0914 [1999-2004]


# ~~~~~~~~~ merge, create needed vars  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

m.ical <- rbind(out.ical1,out.ical0914)
m.ipd <- rbind(out.ipd1,out.ipd0914)
# out.ical1  	14126
# out.ical0914  10713
# m.ical     	24839

cor.test(m.ical$mx_piCAL,m.ipd$mx_piPD)            # what does this mean ??
# corr coefficient = 0.71, p<.001

# needed vars to process CDC-AAP def  (ical3ct,ical4ct,ical5ct,ical6ct; ipd4ct,ipd5ct,ipd6ct)
# also output pr: ical3pr,ical4pr,ical5pr,ical6pr; ipd4pr,ipd5pr,ipd6pr
# also output mx
# names(m.ical)[c(1:5,7:10,12,21:26)]
ind.keep <- c(1:5,7:10,12,21:26)
m2.ical <- m.ical[,ind.keep]
m2.ipd <- m.ipd[,ind.keep]

m.data <- merge(m.data,m2.ical,by="SEQN",all.x=T,as.is=T,na=c("NA","NaN",".","99","9"))      # still 82292        
m.data <- merge(m.data,m2.ipd,by="SEQN",all.x=T,as.is=T,na=c("NA","NaN",".","99","9"))      #         

# ~~~~~~ reduce output
#out.data <- m.data[,c(1:13,22:ncol(m.data))]
write.table(m.data,"d2x_9918_dent_PR_ohq.txt",sep="\t",quote=F, col.names=T, row.names=T, append=F)


# there are 82292 data with dentition
# only 24839 had any mx_piCAL values
# 24839 / 82292 = 30.2% 























# ====================================================================================================================

# ====================================================================================================================

# ====================================================================================================================


# ~~~~~~~~~~ Figure ~~~~~~~~ redundant ck ~~~~~~~~~~~~~~~~~~~~
# correct data
#
a = out.data$mx_piCAL      # na should be the same
b = out.data$mx_piPD
ck.diff = a-b

d.plot <- as.data.frame(cbind(a,b,ck.diff))
d.plot <- d.plot[!is.na(ck.diff),]
ind.type <- rep(round(d.plot$ck.diff))
ind.type[d.plot$ck.diff <= -2] <- "-2 <"
ind.type[d.plot$ck.diff == -2] <- "-2"
ind.type[d.plot$ck.diff == -1] <- "-1"
ind.type[d.plot$ck.diff == 0] <- "0"
ind.type[d.plot$ck.diff<=4 & d.plot$ck.diff>=1] <-  "1-4"
ind.type[d.plot$ck.diff >= 5] <- ">=5"

ind.diff <- ind.type
ind.diff[ind.type=="-2 <"] <- 1
ind.diff[ind.type=="-2"] <- 2
ind.diff[ind.type=="-1"] <- 3
ind.diff[ind.type=="0"] <- 4
ind.diff[ind.type=="1-4"] <- 5
ind.diff[ind.type==">=5"] <- 6

d.plot <- cbind(d.plot,ind.type,ind.diff)

postscript(file="fig_ckall_ipd-ical.ps",paper="letter",horizontal=T)

op <- par(mfrow = c(2, 2))	

# ~~~~~~ [a]  histogram
set.seed(2020)
p1 <- hist(a,main="iCAL")                
p2 <- hist(b,main="iPD")  
p3 <- hist(ck.diff,main="difference")  

# ~~~~~~ [a] ipd ~ ical
plot(a~b,data=d.plot,type="n",cex=1,
       main="correlation of ipd & ical",ylab="mx_piCAL", xlab="mx_piPD",
       frame=F,axes=F,cex.main=1)	
usr <- par("usr")
par(usr=c(usr[1:2],0, 15))
axis(2,at=c(0,2,4,6,8,10,12,15),cex.axis=0.8,las=2)
axis(1,at=c(0,2,4,6,8,10,12,15),cex.axis=0.8,las=2)
legend("topright",c("-2 <","-2","-1","0","1-4",">=5"),pch=c(16,16,16,16,16,16),cex=0.6,bty="n",col=1:6,lty=c(1,2,3,1,2,3))
legend("topleft",c("corr coefficient = 0.71","p<.001"),bty="n")

points(a~b,data=d.plot,pch=16,col=ind.diff,cex=0.6)
lmfit.i <- lm(a~b,data=d.plot)
abline(lmfit.i$coefficient[1],lmfit.i$coefficient[2],col="darkgray",lwd=1.1,lty=1)                

dev.off()

