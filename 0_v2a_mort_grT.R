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

                       # 418 / 40098 = 1.04%


# ***

mort.cvd <- rep(0,nrow(d.use.d))
mort.cvd[!is.na(d.use.d$mort_leading) & d.use.d$mort_leading==1] <- 1         # 859
mort.ca <- rep(0,nrow(d.use.d))
mort.ca[!is.na(d.use.d$mort_leading) & d.use.d$mort_leading==2] <- 1          # 1070
mort.accident <- rep(0,nrow(d.use.d))
mort.accident[!is.na(d.use.d$mort_leading) & d.use.d$mort_leading==4] <- 1    # 170
mort.stk <- rep(0,nrow(d.use.d))
mort.stk[!is.na(d.use.d$mort_leading) & d.use.d$mort_leading==5] <- 1         # 194
mort.dm <- rep(0,nrow(d.use.d))
mort.dm[!is.na(d.use.d$mort_leading) & d.use.d$mort_leading==6] <- 1          # 114

nu.mort <- cbind(mort.cvd,mort.ca,mort.accident,mort.stk,mort.dm)
#mort.vars <- colnames(nu.mort)[2:ncol(nu.mort)]
mort.vars <- c(colnames(nu.mort),"mort_diab","mort_htn","mortstat")

d.ck <- data.frame(cbind(d.use.d,nu.mort))



# ** ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ **
dt.sel <- rep(1,nrow(d.ck))

sum(is.na(d.ck$pt.dm))                       # 21
dt.sel[is.na(d.ck$pt.dm)] <- 0         
d.use2 <- d.ck[!is.na(d.ck$pt.dm),]
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


# ------------------------------------------------------------------------------------------
# ~~~~~~~~ use suggested 0-10; 11-20; 21-28

ck.dat <- data.frame(ck.exclude)

gr09 <- rep(1,nrow(ck.dat))
gr09[ck.dat$tot.dtc>=10] <- 0
table(gr09,ck.dat$tot.dtc)

gr1019 <- rep(1,nrow(ck.dat))
gr1019[ck.dat$tot.dtc>=20] <- 0
gr1019[ck.dat$tot.dtc<10] <- 0
table(gr1019,ck.dat$tot.dtc)

gr2028 <- rep(1,nrow(ck.dat))
gr2028[ck.dat$tot.dtc<20] <- 0
table(gr2028,ck.dat$tot.dtc)

gr20 <- rep(0,nrow(ck.dat))
gr20[ck.dat$tot.dtc<20] <- 1
table(gr20,ck.dat$tot.dtc)

grT.gr <- rep(0,nrow(d.use2))
grT.gr[gr09==1] <- 3
grT.gr[gr1019==1] <- 2
grT.gr[gr2028==1] <- 1
table(grT.gr,ck.dat$tot.dtc)


# ** modify that q1 is the higher NoT
trt.gr = ntile(ck.dat$tot.dtc,3)
table(trt.gr)
table(trt.gr,ck.dat$tot.dtc)

tertile <- trt.gr
tertile[trt.gr==3 ] <- 1
tertile[trt.gr==1 ] <- 3
table(tertile,ck.dat$tot.dtc)                    # 23, 27

# ** create indicator gr
trt.g2 <- trt.gr
trt.g2[trt.gr==1 | trt.gr==3] <- 0
trt.g2[trt.gr==2 ] <- 1
table(trt.g2,tertile)
trt.g3 <- trt.gr
trt.g3[trt.gr==2 | trt.gr==3] <- 0
trt.g3[trt.gr==1 ] <- 1
table(trt.g3,tertile)

# ** choose the maximum surv time
mx.time <- apply(ck.dat[,c("permth_int","permth_exm")],1,max)


# ** combine
ck.dat <- data.frame(cbind(ck.dat,mx.time,gr09,gr1019,gr2028,gr20,grT.gr,tertile,trt.g2,trt.g3))


# ** ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ **
ck.v.cont <- c("age","bmi",dent.vars[c(2:3,5:10)],"fneck","mx.time")
ck.v.cate <- c("gender","currsmk","pastsmk","nvrsmk","ck.gr","race4","edentulous",mort.vars,"grT.gr","tertile")
ck.v.tab <- table2.out("dt.sel",ck.dat,ck.v.cont,ck.v.cate,2)
#write.table(ck.v.tab,"T0_exclude_2.txt",sep="\t",quote=F,col.names=T,row.names=T)

# ** ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ **
# age 20 and above
# after ck exclusion

ck.dat.bk <- ck.dat
d.use2 <- ck.dat[ck.dat$dt.sel==1,]   # ** back up
ck.dat <- ck.dat[ck.dat$dt.sel==1,]

# ***
ck.imp <- rep(0,nrow(ck.dat))
ck.imp[ck.dat$tot.imp>0] <- 1
table(ck.imp)        




# ** ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ **
sum(is.na(d.use2$fneck))                       # 19940
d.use3 <- d.use2[!is.na(d.use2$fneck),]  
dim(d.use3)                                    #            --> 13131


# ** ~~~~~~~~ ck missing values ~~~~~~~~~~~~~~~~~ **
chem <- c("bmi","educ3","smk.cat3","race4","fneck","smkpkyrs")
for (i in 1:length(chem)) {
	ind <- match(chem[i],names(d.use2))
	print(c(chem[i],sum(!is.na(d.use2[,ind])),round(sum(!is.na(d.use2[,ind]))/nrow(d.use2)*100,digit=2)))
}

#[1] "fneck" "13131" "39.71"
#[1] "smkpkyrs" "28448"    "86.02"         # not use 


# ** ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ **

NoT <- d.use2$tot.dtc
col=c(rep("red",10),rep("orange",10),rep("green",9))
bb <- table(NoT)

plot(bb,cex.lab=0.8, frame=F,type="n",
     main=" NoT ",cex.main=0.8,ylab="frequency",xlab="Number of Teeth (NoT)",
     ylim=c(0,10000), xlim=c(0,28),axes=F, col=col  )   

#lines(bb~c(0:28),col="gray",lty=1,lwd=1.2)   
points(bb,col=col,pch=23) 
axis(1,at=c(0,6,10,15,20,24,28),cex.axis=0.75,las=1)
axis(2,at=c(0,500,1000,3000,5000,7500,10000),cex.axis=0.8,las=1)
abline(h=1000,col="gray",lwd=0.8, lty=2)
abline(h=3000,col="gray",lwd=0.8, lty=2)
abline(h=5000,col="gray",lwd=0.8, lty=2)

# ** ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ **

surv.time <- get.mage.out(ck.dat$mx.time,ck.dat$age)
ck2.NoT <- get.mage.out(ck.dat$tot.dtc,ck.dat$age)      
out.x <- data.frame(cbind(surv.time,ck2.NoT)[20:85,])

#plot(surv.time~ck2.NoT,data=out.x,xlim=c(0,28),ylim=c(50,110),xlab="NoT",ylab="follow up months",
#     pch=21, col = c(rep('green',length(55:65)),rep('blue',length(66:75)),rep('red',length(76:85))))

col2 <- c(rep('lightgray',length(20:40)),rep('yellow',length(41:54)),
         rep('green',length(55:65)),rep('blue',length(66:75)),rep('red',length(76:85)))
plot(surv.time~ck2.NoT,data=out.x,xlim=c(0,28),ylim=c(50,110),xlab="NoT",ylab="follow up months",
     pch=21, col =col2 )


lmfit.c <- lm(surv.time~ck2.NoT,data=out.x)
abline(lmfit.c$coefficient[1],lmfit.c$coefficient[2],col="grey",lwd=1.2,lty=3)

legend(5,95, legend = c("age 20~40","age 41~54","age 55~65","age 66~75","age 76~85"),
           pch = c(21,21,21,21,21),col=c("lightgray",'yellow','green','blue','red'),cex=0.7, bty='n')

out.x2 <- cbind(c(20:85),out.x)
colnames(out.x2) <- c("age","survmnts","NoT")

ck.ant <- get.mage.out(d.use2$ant.dtc,d.use2$age)[20:85]
ck.post <- get.mage.out(d.use2$post.dtc,d.use2$age)[20:85]

ck.eden <- c()
for (i in 20:85) {
	tmp <- ck.dat[ck.dat$age==i,]
	a <- table(tmp$edentulous)
	b <- a[2]/nrow(tmp)
	ck.eden <- append(ck.eden,b)
}

ck.age.n <- as.numeric(table(ck.dat$age))
ck.age.n.pr <- ck.age.n/nrow(ck.dat)*100
out.age.n <- sprintf("%d (%.2f)",ck.age.n,ck.age.n.pr)

out.supp <- cbind(out.x2,out.age.n,ck.ant,ck.post,ck.eden*100)
out.suppl <- format(out.supp,digit=3,nsmall=3)
#write.table(out.suppl,"suppl_age_time_NoT.txt",sep="\t",quote=F,col.names=T,row.names=T)


# *** by NoT ***

z.time <- as.numeric(get.mdtc.out(ck.dat$mx.time,ck.dat$tot.dtc))     # NoT 0:28
z.ant <- as.numeric(get.mdtc.out(ck.dat$ant.dtc,ck.dat$tot.dtc)) 
z.post <- as.numeric(get.mdtc.out(ck.dat$post.dtc,ck.dat$tot.dtc))
z.inc <- as.numeric(get.mdtc.out(ck.dat$inc.dtc,ck.dat$tot.dtc))
z.can <- as.numeric(get.mdtc.out(ck.dat$can.dtc,ck.dat$tot.dtc))
z.pre <- as.numeric(get.mdtc.out(ck.dat$pre.dtc,ck.dat$tot.dtc))
z.mol <- as.numeric(get.mdtc.out(ck.dat$mol.dtc,ck.dat$tot.dtc)) 
z.n <- as.numeric(table(ck.dat$tot.dtc))
z.n.pr <- z.n/nrow(ck.dat)*100   
out.z.n <- sprintf("%d (%.2f)",z.n,z.n.pr)

out.z <- data.frame(cbind(0:28,out.z.n,format(cbind(z.time,z.ant,z.post,z.inc,z.can,z.pre,z.mol),digit=3,nsmall=3)))
out.z2 <- out.z[29:1,]

#write.table(out.z2,"suppl_NoT_details.txt",sep="\t",quote=F,col.names=T,row.names=T)

    
# ==========================================================================================
# Table  1   
#
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# ~~~~~~ Table 1
out.var <- "mortstat"  

vars.continuous <- c("age","incPovR","bmi","smkpkyrs","mx.time",
                     "tot.dtc","ant.dtc","post.dtc","inc.dtc","can.dtc","pre.dtc","mol.dtc","fneck")
vars.categorical <- c("gender","race4","educ3","nvrsmk","pastsmk","currsmk","pt.dm","htn2","stroke","tot.cvd",
                      "exercise","edentulous","grT.gr","gr09","gr1019","gr2028","gr20","tertile")		
ck.table <- table2.out(out.var,ck.dat,vars.continuous,vars.categorical,2)

#write.table(ck.table,"T1_mortstat.txt",sep="\t",quote=F,col.names=T,row.names=T)


# ~~~~~~ summary
out.data <- ck.dat

for (i in 1:length(vars.continuous)) {
	varck <- vars.continuous[i]
	m1 <- mean(out.data[,varck],na.rm=T)
	sd <- sd(out.data[,varck],na.rm=T)
	print(c(varck,format(c(m1,sd),digits=2,nsmall=2)))
}

for (i in 1:length(vars.categorical)) {
	varck <- vars.categorical[i]
	print(varck)
	tab <- table(out.data[,varck])
	print(tab)
    print(format(tab/nrow(out.data)*100,digits=2,nsmall=1))
}




# ==========================================================================================
# Table  2   
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# ~~~~~~ Table 2
out.var <- "grT.gr"  

vars.continuous <- c("age","incPovR","bmi","smkpkyrs","mx.time",
                     "tot.dtc","ant.dtc","post.dtc","inc.dtc","can.dtc","pre.dtc","mol.dtc","fneck")
vars.categorical <- c("gender","race4","educ3","nvrsmk","pastsmk","currsmk","pt.dm","htn2","stroke","tot.cvd",
                      "exercise",mort.vars)		
ck.table <- table2.out(out.var,ck.dat,vars.continuous,vars.categorical,3)

write.table(ck.table,"T2_grT_gr.txt",sep="\t",quote=F,col.names=T,row.names=T)





# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# *   Table 3  outcomes 
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

printconf <- function(summary) {
	x.fit <- summary
    conf1 <- sprintf("%.2f (%.2f-%.2f)",x.fit$conf.int[1,1],x.fit$conf.int[1,3],x.fit$conf.int[1,4])
    conf2 <- sprintf("%.2f (%.2f-%.2f)",x.fit$conf.int[2,1],x.fit$conf.int[2,3],x.fit$conf.int[2,4])
    out <- c(conf1,conf2)
    return(out)
} 


ck.dat <- data.frame(d.use2)

#ind <- rep(F,nrow(ck.dat))
#ind[!is.na(ck.dat$mort_leading) & (ck.dat$mort_diab==1 | ck.dat$mort.dm==1)] <-T
#ind[!is.na(ck.dat$mort_leading) & ck.dat$mort_leading==2] <-T
#ind[!is.na(ck.dat$mortstat) & ck.dat$mortstat==0] <-T
#ck.dat <- ck.dat[ind,]
#dim(ck.dat)


# ** endpoint **
endpoint.name <- "mortality"
endpoint.time <- ck.dat$mx.time
endpoint.status <- ck.dat$mortstat


# ** assign outcome **
outcome <- endpoint.status
outcomeT <- endpoint.time
endpoint <- Surv(outcomeT,outcome)

out <- c("n","nevent","HR","Pv")

  # ** model [1] **
  f.1 = c("gr1019","gr09","age","gender","race4","currsmk","pastsmk")
  #f.1 = c("trt.g2","trt.g3","age","gender","race4","currsmk","pastsmk")
  #f.1 = c("edentulous","age","gender","race4","currsmk","pastsmk")
  #f.1 = c("gr20","age","gender","race4","currsmk","pastsmk")
  
   fmla <- as.formula(paste("endpoint",paste(f.1,collapse="+"),sep=" ~ "))
   cox.fit <- coxph(fmla, data=ck.dat)
   x.fit <- summary(cox.fit)
   summary(cox.fit)
   out<-cbind(out,c(x.fit$n,x.fit$nevent,printconf(x.fit)[1],sprintf("%.2e",x.fit$coefficients[1,5])))
   out<-cbind(out,c(x.fit$n,x.fit$nevent,printconf(x.fit)[2],sprintf("%.2e",x.fit$coefficients[2,5])))
   #temp <- cox.zph(cox.fit)               

  # ** model [2] **
  f.1 = c("gr1019","gr09","age","gender","race4","currsmk","pastsmk","bmi","educ3","incPovR","exercise","tot.cvd","pt.dm","stroke","htn2")
  #f.1 = c("trt.g2","trt.g3","age","gender","race4","currsmk","pastsmk","bmi","educ3","incPovR","exercise","tot.cvd","pt.dm","stroke","htn2")
  #f.1 = c("edentulous","age","gender","race4","currsmk","pastsmk","bmi","educ3","incPovR","exercise","tot.cvd","pt.dm","stroke","htn2")
  #f.1 = c("gr20","age","gender","race4","currsmk","pastsmk","bmi","educ3","incPovR","exercise","tot.cvd","pt.dm","stroke","htn2")

  
   fmla <- as.formula(paste("endpoint",paste(f.1,collapse="+"),sep=" ~ "))
   cox.fit <- coxph(fmla, data=ck.dat)
   x.fit <- summary(cox.fit)
   summary(cox.fit)
   out<-cbind(out,c(x.fit$n,x.fit$nevent,printconf(x.fit)[1],sprintf("%.2e",x.fit$coefficients[1,5])))
   out<-cbind(out,c(x.fit$n,x.fit$nevent,printconf(x.fit)[2],sprintf("%.2e",x.fit$coefficients[2,5])))

    # ** model [3] **
  f.1 = c("gr1019","gr09","age","gender","race4","currsmk","pastsmk","bmi","educ3","incPovR","exercise","tot.cvd","pt.dm","stroke","htn2","fneck")
  #f.1 = c("trt.g2","trt.g3","age","gender","race4","currsmk","pastsmk","bmi","educ3","incPovR","exercise","tot.cvd","pt.dm","stroke","htn2","fneck")
  #f.1 = c("edentulous","age","gender","race4","currsmk","pastsmk","bmi","educ3","incPovR","exercise","tot.cvd","pt.dm","stroke","htn2","fneck")
  #f.1 = c("gr20","age","gender","race4","currsmk","pastsmk","bmi","educ3","incPovR","exercise","tot.cvd","pt.dm","stroke","htn2","fneck")
  
   fmla <- as.formula(paste("endpoint",paste(f.1,collapse="+"),sep=" ~ "))
   cox.fit <- coxph(fmla, data=ck.dat)
   x.fit <- summary(cox.fit)
   summary(cox.fit)
   out<-cbind(out,c(x.fit$n,x.fit$nevent,printconf(x.fit)[1],sprintf("%.2e",x.fit$coefficients[1,5])))
   out<-cbind(out,c(x.fit$n,x.fit$nevent,printconf(x.fit)[2],sprintf("%.2e",x.fit$coefficients[2,5])))


  # ** output
  out2 <- out[,2:ncol(out)]
  rownames(out2) <- sprintf("grT_%s",c("n","nevent","HR","Pv"))
  colnames(out2) <- c("model1_gr2","model1_gr3","model2_gr2","model2_gr3","model3_gr2","model3_gr3")
  #colnames(out2) <- sprintf("gr20_%s",c("model1","model2","model3"))

  #out.tertile <- out2
  #write.table(out.tertile,"T_HR_tertile.txt",sep="\t",quote=F,col.names=T,row.names=T)
  
  #out.grT <- out2
  #write.table(out.grT,"T_DM_grT.txt",sep="\t",quote=F,col.names=T,row.names=T)
  
  #out.eden <- out2
  #write.table(out.eden,"T_HR_eden.txt",sep="\t",quote=F,col.names=T,row.names=T)

  #out.gr20 <- out2
  #write.table(out.gr20,"T_HR_gr20.txt",sep="\t",quote=F,col.names=T,row.names=T)






# *******************************************************************************
# plotting
# -------------------------------------------------------------------------------

assign(endpoint.name, Surv(outcomeT,outcome))

 f.1 = c("gr1120","gr010","age","gender","race4","smk.cat3","bmi","educ3","incPovR","exercise","tot.cvd","pt.dm","stroke","htn2")
 fmla <- as.formula(paste("endpoint",paste(f.1,collapse="+"),sep=" ~ "))
 cox.fit <- coxph(fmla, data=ck.dat)
 x.fit <- summary(cox.fit)

x.conf <- format(x.fit$conf.int[1:2,c(1,3,4)],digits=3)
stat.plot1 <- paste(c("HR(95","%",
               sprintf("CI): %s(%s-%s)",x.conf[1,1],x.conf[1,2],x.conf[1,3])),collapse="")
stat.plot2 <- paste(c("HR(95","%",
               sprintf("CI): %s(%s-%s)",x.conf[2,1],x.conf[2,2],x.conf[2,3])),collapse="")

pv.plot <- sprintf("P=%s",format(x.fit$coefficient[1:2,5],digits=2))

# *******************************************************************************
# plotting survival curves
# -------------------------------------------------------------------------------
fit1 <- survfit(as.formula(paste(endpoint.name,"~1")),data=ck.dat,subset=(grT.gr==1))
fit2 <- survfit(as.formula(paste(endpoint.name,"~1")),data=ck.dat,subset=(grT.gr==2))
fit3 <- survfit(as.formula(paste(endpoint.name,"~1")),data=ck.dat,subset=(grT.gr==3))

plot(fit1, fun='event',xscale=1,xmax=210,ylim=c(0,1),mark.time=F, conf.int=F, col=1, xlab="Months of follow-up",main="All-cause mortality (model 2)",cex.main=0.9)
lines(fit2,fun='event',xscale=1,xmax=210,ylim=c(0,1),mark.time=F, lty=2, conf.int=F, col=2, xlab="Months of follow-up")
lines(fit3,fun='event',xscale=1,xmax=210,ylim=c(0,1),mark.time=F, lty=3, conf.int=F, col=3, xlab="Months of follow-up")

legend("topleft",c("NoT20-28 [Reference]",sprintf("NoT10-19 %s %s",stat.plot1,pv.plot[1]),
sprintf("NoT0-9 %s %s",stat.plot2,pv.plot[2])),
lty=c(1,2,3),cex=0.7,text.col =c(1,2,3), bg ='white',bty="n")














# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# * detail of model 3 survival (suppl table)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

ck.dt2 <- ck.dat
ck.dt2$gender[ck.dt2$gender==2] <- 0
fac.race <- as.factor(ck.dt2$race4)
fac.educ <- as.factor(ck.dt2$educ3)
fac.exercise <- as.factor(ck.dt2$exercise)
ck.dt2 <- cbind(ck.dt2,fac.race,fac.educ,fac.exercise)

# ** model [3] **
f.1 = c("gr1019","gr09","age","gender",
       "fac.race","currsmk","pastsmk","bmi","fac.educ","incPovR","fac.exercise","tot.cvd","pt.dm","stroke","htn2","fneck")
fmla <- as.formula(paste("endpoint",paste(f.1,collapse="+"),sep=" ~ "))
cox.fit <- coxph(fmla, data=ck.dt2)
x.fit <- summary(cox.fit)

add <- c()
for (i in 1:nrow(x.fit$conf.int)) {
	tmp <- sprintf("%.2f (%.2f-%.2f)",x.fit$conf.int[i,1],x.fit$conf.int[i,3],x.fit$conf.int[i,4])
    add <- append(add,tmp)
}

pv <- sprintf("%.2e",x.fit$coefficients[,5])
out.4 <- cbind(rownames(x.fit$conf.int),add,pv)
write.table(out.4,"suppl_HR_mod3-2.txt",sep="\t",quote=F,col.names=T,row.names=F)








# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# not used 
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


ck.dtc <- get.mage.out(d.use2$tot.dtc,d.use2$age)

plot(ck.dtc[18:85]~c(18:85),type="n",cex.lab=0.8,
     main="mean NoT by age",cex.main=0.8,ylab="Number of Teeth (NoT)",xlab="age",
     frame=F,xlim=c(18,85),ylim=c(0,28),axes=F)   
axis(1,at=seq(15,85,by=5),cex.axis=0.8,las=1)
axis(2,at=c(0,5,10,15,20,25,28),cex.axis=0.75,las=1)

lines(ck.dtc[18:85]~c(18:85),col='green',lty=1,lwd=1.2)
abline(v=55,col="brown",lwd=0.8, lty=2)
abline(h=20,col="gray",lwd=0.8, lty=2)



# ** ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ **

d.plot <- out.data

postscript(file="fig_profile_dtc.ps",paper="letter",horizontal=T)

op <- par(mfrow = c(2, 2))	


# ~~~~~~ [1]

tot.sbp <- get.mdtc.out(d.plot$sbp[!is.na(d.plot$tot.dtc)],d.plot$tot.dtc[!is.na(d.plot$tot.dtc)])
ant.sbp <- get.mdtc.out(d.plot$sbp[!is.na(d.plot$ant.dtc)],d.plot$ant.dtc[!is.na(d.plot$ant.dtc)])
post.sbp <- get.mdtc.out(d.plot$sbp[!is.na(d.plot$post.dtc)],d.plot$post.dtc[!is.na(d.plot$post.dtc)])
#ya <- min(c(tot.sbp,ant.sbp,post.sbp),na.rm=T)
#yb <- max(c(tot.sbp,ant.sbp,post.sbp),na.rm=T)

plot(tot.sbp~c(0:28),type="n",cex.lab=0.8,
     main="a. Systolic blood pressure",cex.main=0.8,ylab="mmHg",xlab="Number of Teeth (NoT)",
     frame=F,ylim=c(115,145),axes=F)   
axis(1,at=c(0,5,7,10,15,20,25,28),cex.axis=0.75,las=1)
axis(2,at=seq(115,145,by=5),cex.axis=0.8,las=1)

lines(tot.sbp ~c(0:28),col='green',lty=1,lwd=1.2)
lines(ant.sbp ~c(0:28),col='red',lty=2,lwd=1.1)
lines(post.sbp ~c(0:28),col='blue',lty=3,lwd=1.1)

legend(18,145, legend = c("Total NoT (p<.001)","Anterior NoT (p<.001)","Posterior NoT (p<.001)"),
           lty = c(1,2,3),col=c('green','red','blue'),cex=0.7, bty='n')
#abline(v=20,col="gray",lwd=0.8)
#abline(h=130,col="gray",lwd=0.8)

# ~~~~~~ [2]
tot.dbp <- get.mdtc.out(d.plot$dbp[!is.na(d.plot$tot.dtc)],d.plot$tot.dtc[!is.na(d.plot$tot.dtc)])
ant.dbp <- get.mdtc.out(d.plot$dbp[!is.na(d.plot$ant.dtc)],d.plot$ant.dtc[!is.na(d.plot$ant.dtc)])
post.dbp <- get.mdtc.out(d.plot$dbp[!is.na(d.plot$post.dtc)],d.plot$post.dtc[!is.na(d.plot$post.dtc)])
#ya <- min(c(tot.sbp,ant.sbp,post.sbp),na.rm=T)
#yb <- max(c(tot.sbp,ant.sbp,post.sbp),na.rm=T)

plot(tot.dbp~c(0:28),type="n",cex.lab=0.8,
     main="b. Diastolic blood pressure",cex.main=0.8,ylab="mmHg",xlab="Number of Teeth (NoT)",
     frame=F,ylim=c(65,75),axes=F)   
axis(1,at=c(0,5,7,10,15,20,25,28),cex.axis=0.75,las=1)
axis(2,at=seq(65,75,by=2),cex.axis=0.8,las=1)

lines(tot.dbp ~c(0:28),col='green',lty=1,lwd=1.2)
lines(ant.dbp ~c(0:28),col='red',lty=2,lwd=1.1)
lines(post.dbp ~c(0:28),col='blue',lty=3,lwd=1.1)

legend(0,75, legend = c("Total NoT (p=0.009)","Anterior NoT (p=0.011)","Posterior NoT (p=0.014)"),
           lty = c(1,2,3),col=c('green','red','blue'),cex=0.7, bty='n')
#abline(v=20,col="gray",lwd=0.8)



# ~~~~~~ [3]
tot.hba1c <- get.mdtc.out(d.plot$hba1c[!is.na(d.plot$tot.dtc)],d.plot$tot.dtc[!is.na(d.plot$tot.dtc)])
ant.hba1c <- get.mdtc.out(d.plot$hba1c[!is.na(d.plot$ant.dtc)],d.plot$ant.dtc[!is.na(d.plot$ant.dtc)])
post.hba1c <- get.mdtc.out(d.plot$hba1c[!is.na(d.plot$post.dtc)],d.plot$post.dtc[!is.na(d.plot$post.dtc)])

plot(tot.hba1c~c(0:28),type="n",cex.lab=0.8,
     main="c. HbA1c ",cex.main=0.8,ylab="%",xlab="Number of Teeth (NoT)",
     frame=F,ylim=c(5,7),axes=F)   
axis(1,at=c(0,5,7,10,15,20,25,28),cex.axis=0.75,las=1)
axis(2,at=seq(5,7,by=0.4),cex.axis=0.8,las=1)

lines(tot.hba1c ~c(0:28),col='green',lty=1,lwd=1.2)
lines(ant.hba1c ~c(0:28),col='red',lty=2,lwd=1.1)
lines(post.hba1c ~c(0:28),col='blue',lty=3,lwd=1.1)

legend(18,7, legend = c("Total NoT (p<.001)","Anterior NoT (p<.001)","Posterior NoT (p<.001)"),
           lty = c(1,2,3),col=c('green','red','blue'),cex=0.7, bty='n')



# ~~~~~~ [4]
tot.hdl <- get.mdtc.out(d.plot$hdl[!is.na(d.plot$tot.dtc)],d.plot$tot.dtc[!is.na(d.plot$tot.dtc)])
ant.hdl <- get.mdtc.out(d.plot$hdl[!is.na(d.plot$ant.dtc)],d.plot$ant.dtc[!is.na(d.plot$ant.dtc)])
post.hdl <- get.mdtc.out(d.plot$hdl[!is.na(d.plot$post.dtc)],d.plot$post.dtc[!is.na(d.plot$post.dtc)])

plot(tot.hdl~c(0:28),type="n",cex.lab=0.8,
     main="d. HDL ",cex.main=0.8,ylab="mg/dL",xlab="Number of Teeth (NoT)",
     frame=F,ylim=c(48,58),axes=F)   
axis(1,at=c(0,5,7,10,15,20,25,28),cex.axis=0.75,las=1)
axis(2,at=seq(48,58,by=2),cex.axis=0.8,las=1)

lines(tot.hdl ~c(0:28),col='green',lty=1,lwd=1.2)
lines(ant.hdl ~c(0:28),col='red',lty=2,lwd=1.1)
lines(post.hdl ~c(0:28),col='blue',lty=3,lwd=1.1)

legend(0,58, legend = c("Total NoT (p<.001)","Anterior NoT (p<.001)","Posterior NoT (p<.001)"),
           lty = c(1,2,3),col=c('green','red','blue'),cex=0.7, bty='n')


dev.off()




# *********************************************

ck.target <- ck.dat$dbp

ck.a <- lm(ck.target~ck.dat$tot.dtc+ck.dat$age+ck.dat$race+ck.dat$gender+ck.dat$smk.cat3)
pva = round(summary(ck.a)$coefficients[2,4],digit=3)
ck.b <- lm(ck.target~ck.dat$ant.dtc+ck.dat$age+ck.dat$race+ck.dat$gender+ck.dat$smk.cat3)
pvb = round(summary(ck.b)$coefficients[2,4],digit=3)
ck.c <- lm(ck.target~ck.dat$post.dtc+ck.dat$age+ck.dat$race+ck.dat$gender+ck.dat$smk.cat3)
pvc = round(summary(ck.c)$coefficients[2,4],digit=3)
print(c(pva,pvb,pvc))


