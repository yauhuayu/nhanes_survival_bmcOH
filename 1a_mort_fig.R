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
dent.vars <- c("SEQN","ck.gr","tot.dtc","edentulous","ant.dtc","post.dtc","inc.dtc","can.dtc","pre.dtc","mol.dtc","dentvisit")
bmd.vars <- c("SEQN","hipFrc","wristFrc","spnFrc","LS","fneck","op_dx","op_tx") 


d.cvd <- read.table("d1_demo_smk_dm_cvd.txt",header=T,sep="\t")[,cvd.vars]
d.dent <- read.table("d2_9918_dent_ohq.txt",header=T,sep="\t")[,dent.vars]
d.mort <- read.table("d4_9914_mort.txt",header=T,sep="\t")
d.mort <- d.mort[!is.na(d.mort$mortstat),]
d.bmd <- read.table("d3_9918_fem_bmd.txt",header=T,sep="\t")[,bmd.vars]

 
dim(d.cvd)        # 108060   25 
dim(d.dent)       # 82292    11 
dim(d.mort)       # 47279	 8                        # * there are 47279 with mortality data
dim(d.bmd)        # 24970    8                        # with valid femoral neck

d.use <- merge(d.mort,d.dent, by="SEQN",all.x=T,as.is=T,na=c("NA","."))        # 47279
d.use <- merge(d.use,d.cvd, by="SEQN",all.x=T,as.is=T,na=c("NA","."))          # 47279
d.use <- merge(d.use,d.bmd, by="SEQN",all.x=T,as.is=T,na=c("NA","."))          # 24970



# ------------------------------------------------------------------------------------------
# ~~~~~~~~ keep those without missing vars ~~~~~~~~~

ck.dtc <- get.mage.out(d.use$tot.dtc,d.use$age)

plot(ck.dtc[18:85]~c(18:85),type="n",cex.lab=0.8,
     main="mean NoT by age",cex.main=0.8,ylab="Number of Teeth (NoT)",xlab="age",
     frame=F,xlim=c(18,85),ylim=c(0,28),axes=F)   
axis(1,at=seq(15,85,by=5),cex.axis=0.8,las=1)
axis(2,at=c(0,5,10,15,20,25,28),cex.axis=0.75,las=1)

lines(ck.dtc[18:85]~c(18:85),col='green',lty=1,lwd=1.2)
abline(v=55,col="brown",lwd=0.8, lty=2)
abline(h=20,col="gray",lwd=0.8, lty=2)

# ** ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ **
dt.sel <- rep(1,nrow(d.use))
sum(is.na(d.use$tot.dtc))                     # 7181 no dental examination
dt.sel[is.na(d.use$tot.dtc)] <- 0             
sum(d.use$age <55)                            # 29523       [ 14973 left]
dt.sel[d.use$age <55] <- 0   

sum(is.na(d.use$pt.dm))                       # 31
dt.sel[is.na(d.use$pt.dm)] <- 0         
sum(is.na(d.use$totcvd))                      # 0     
dt.sel[is.na(d.use$totcvd)] <- 0                
sum(is.na(d.use$htn2))                        # 2246     
dt.sel[is.na(d.use$htn2)] <- 0                
sum(is.na(d.use$stroke))                      # 5737
dt.sel[is.na(d.use$stroke)] <- 0      

sum(is.na(d.use$fneck))                      # 5737
dt.sel[is.na(d.use$stroke)] <- 0          

ck.exclude <- data.frame(cbind(d.use,dt.sel))  # 14973 --> 14929

ck.v.tab <- table2.out("dt.sel",ck.exclude,ck.v.cont,ck.v.cate,2)
write.table(ck.v.tab,"T0_excluded.txt",sep="\t",quote=F,col.names=T,row.names=T)

# ** ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ **

d.use2 <- d.use[dt.sel>0,]                    # n = 14929


# ** ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ **
chem <- c("totchol","hdl","tg","ldl","hba1c","LBXGLU","sbp","dbp","waist","bmi","educ3","smk.cat3","race4","fmhxmi","fneck")
for (i in 1:length(chem)) {
	ind <- match(chem[i],names(d.use2))
	print(c(chem[i],sum(!is.na(d.use2[,ind])),round(sum(!is.na(d.use2[,ind]))/nrow(d.use2)*100,digit=2)))
}

#[1] "totchol" "14150"   "94.78"  
#[1] "hdl"   "14148" "94.77"
#[1] "tg"    "6993"  "46.84"
#[1] "ldl"   "6705"  "44.91"
#[1] "hba1c" "14381" "96.33"
#[1] "LBXGLU" "7071"   "47.36" 
#[1] "sbp"   "14502" "97.14"
#[1] "dbp"   "14338" "96.04"
#[1] "waist" "14099" "94.44"
#[1] "bmi"   "14534" "97.35"
#[1] "educ3" "14898" "99.79"
#[1] "smk.cat3" "14910"    "99.87"   
#[1] "race4" "14929" "100"  
#[1] "fmhxmi" "9591"   "64.24" 
#[1] "fneck" "6448"  "43.19"


# ------------------------------------------------------------------------------------------
# ~~~~~~~~ create tot.dtc tertiles ~~~~~~~~~

ck.dat <- data.frame(d.use2)
trt.gr = ntile(ck.dat$tot.dtc,3)
table(trt.gr)
table(trt.gr,ck.dat$tot.dtc)

# ** modify that q1 is the higher NoT
tertile <- trt.gr
tertile[trt.gr==3 ] <- 1
tertile[trt.gr==1 ] <- 3
table(tertile,ck.dat$tot.dtc)

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
ck.dat <- data.frame(cbind(d.use2,tertile,trt.g2,trt.g3,mx.time))


NoT <- d.use2$tot.dtc
table(NoT,tertile)
# cut points: 24 (ovlp); 11 (ovlap)
col=c(rep("red",11),rep("orange",13),rep("green",5))
bb <- table(NoT)

plot(bb,cex.lab=0.8, frame=F,type="n",
     main=" NoT ",cex.main=0.8,ylab="frequency",xlab="Number of Teeth (NoT)",
     ylim=c(0,3000), xlim=c(0,28),axes=F, col=col  )   

#lines(bb~c(0:28),col="gray",lty=1,lwd=1.2)   
points(bb,col=col,pch=23) 
axis(1,at=c(0,6,11,15,20,24,28),cex.axis=0.75,las=1)
axis(2,at=c(0,50,100,500,1000,2000,2500,3000),cex.axis=0.8,las=1)




# ~~~~~~~
surv.time <- get.mage.out(ck.dat$mx.time,ck.dat$age)
ck2.NoT <- get.mage.out(ck.dat$tot.dtc,ck.dat$age)      
out.x <- data.frame(cbind(surv.time,ck2.NoT)[55:85,])
plot(surv.time~ck2.NoT,data=out.x,xlim=c(10,22),ylim=c(60,100),xlab="NoT",ylab="follow up months",
     pch=21, col = c(rep('green',length(55:65)),rep('blue',length(66:75)),rep('red',length(76:85))))

lmfit.c <- lm(surv.time~ck2.NoT,data=out.x)
abline(lmfit.c$coefficient[1],lmfit.c$coefficient[2],col="grey",lwd=1.2,lty=3)

legend(10,85, legend = c("age 55~65","age 66~75","age 76~85"),
           pch = c(21,21,21),col=c('green','blue','red'),cex=0.7, bty='n')

out.x2 <- cbind(c(55:85),out.x)
colnames(out.x2) <- c("age","survmnts","NoT")
write.table(out.x2,"suppl_age_time_NoT.txt",sep="\t",quote=F,col.names=T,row.names=T)


    
# ==========================================================================================
# Table  1   
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# ~~~~~~ Table 1
out.var <- "mortstat"  

vars.continuous <- c("age","incPovR","bmi","waist","sbp","dbp","smkpkyrs","totchol","hdl","tg","ldl","hba1c","LBXGLU","mx.time",
                     "tot.dtc","ant.dtc","post.dtc","inc.dtc","can.dtc","pre.dtc","mol.dtc")
vars.categorical <- c("gender","race4","educ3","nvrsmk","pastsmk","currsmk","pt.dm","htn2","stroke","tot.cvd",
                      "fmhxmi","exercise","dentvisit","edentulous","tertile")		
ck.table <- table2.out(out.var,ck.dat,vars.continuous,vars.categorical,2)

write.table(ck.table,"T1_mortstat.txt",sep="\t",quote=F,col.names=T,row.names=T)


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
out.var <- "tertile"  

vars.continuous <- c("age","incPovR","bmi","waist","sbp","dbp","smkpkyrs","totchol","hdl","tg","ldl","hba1c","LBXGLU","mx.time",
                     "tot.dtc","ant.dtc","post.dtc","inc.dtc","can.dtc","pre.dtc","mol.dtc")
vars.categorical <- c("gender","race4","educ3","nvrsmk","pastsmk","currsmk","pt.dm","htn2","stroke","tot.cvd",
                      "fmhxmi","exercise","dentvisit","mortstat","mort_diab","mort_htn","mort_leading")		
ck.table <- table2.out(out.var,ck.dat,vars.continuous,vars.categorical,3)

write.table(ck.table,"T2_tertile.txt",sep="\t",quote=F,col.names=T,row.names=T)





# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# *   Table 3  outcomes 
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# ** endpoint **
endpoint.name <- "mortality"
endpoint.time <- ck.dat$mx.time
endpoint.status <- ck.dat$mortstat


# ** assign outcome **
outcome <- endpoint.status
outcomeT <- endpoint.time
endpoint <- Surv(outcomeT,outcome)

out <- c("n","nevent","HR","U_ci","L_ci","Pv")

  # ** model [1] **
  f.1 = c("trt.g2","trt.g3","age","gender","race4","smk.cat3")
  
   fmla <- as.formula(paste("endpoint",paste(f.1,collapse="+"),sep=" ~ "))
   cox.fit <- coxph(fmla, data=ck.dat)
   x.fit <- summary(cox.fit)
   summary(cox.fit)
   out<-rbind(out,round(c(x.fit$n,x.fit$nevent,x.fit$conf.int[1,c(1,3,4)],x.fit$coefficients[1,5]),digit=4))
   out<-rbind(out,round(c(x.fit$n,x.fit$nevent,x.fit$conf.int[2,c(1,3,4)],x.fit$coefficients[2,5]),digit=4))
   #temp <- cox.zph(cox.fit) 
   #print(temp)                  

  # ** model [2] **
  f.1 = c("trt.g2","trt.g3","age","gender","race4","smk.cat3","smkpkyrs","bmi","educ3","incPovR","exercise")
   fmla <- as.formula(paste("endpoint",paste(f.1,collapse="+"),sep=" ~ "))
   cox.fit <- coxph(fmla, data=ck.dat)
   x.fit <- summary(cox.fit)
   summary(cox.fit)
   out<-rbind(out,round(c(x.fit$n,x.fit$nevent,x.fit$conf.int[1,c(1,3,4)],x.fit$coefficients[1,5]),digit=4))
   out<-rbind(out,round(c(x.fit$n,x.fit$nevent,x.fit$conf.int[2,c(1,3,4)],x.fit$coefficients[2,5]),digit=4))

  # ** model [3] **
  f.1 = c("trt.g2","trt.g3","age","gender","race4","smk.cat3","smkpkyrs","bmi","educ3","incPovR","exercise","tot.cvd","pt.dm","stroke","htn2")
   fmla <- as.formula(paste("endpoint",paste(f.1,collapse="+"),sep=" ~ "))
   cox.fit <- coxph(fmla, data=ck.dat)
   x.fit <- summary(cox.fit)
   summary(cox.fit)
   out<-rbind(out,round(c(x.fit$n,x.fit$nevent,x.fit$conf.int[1,c(1,3,4)],x.fit$coefficients[1,5]),digit=4))
   out<-rbind(out,round(c(x.fit$n,x.fit$nevent,x.fit$conf.int[2,c(1,3,4)],x.fit$coefficients[2,5]),digit=4))

  # ** model [4] **
  f.1 = c("trt.g2","trt.g3","age","gender","race4","smk.cat3","smkpkyrs","bmi","educ3","incPovR","exercise","tot.cvd","pt.dm","stroke","htn2","fneck","hipFrc","totchol","hba1c","sbp","dbp")
   fmla <- as.formula(paste("endpoint",paste(f.1,collapse="+"),sep=" ~ "))
   cox.fit <- coxph(fmla, data=ck.dat)
   x.fit <- summary(cox.fit)
   summary(cox.fit)
   out<-rbind(out,round(c(x.fit$n,x.fit$nevent,x.fit$conf.int[1,c(1,3,4)],x.fit$coefficients[1,5]),digit=4))
   out<-rbind(out,round(c(x.fit$n,x.fit$nevent,x.fit$conf.int[2,c(1,3,4)],x.fit$coefficients[2,5]),digit=4))

  # ** output
  out <- out[2:nrow(out),]
  colnames(out) <- c("n","nevent","HR","U_ci","L_ci","Pv")
  rownames(out) <- c("model1_gr2","model1_gr3","model2_gr2","model2_gr3","model3_gr2","model3_gr3","model4_gr2","model4_gr3")
  write.table(out,"T3_a_HRs.txt",sep="\t",quote=F,col.names=T,row.names=T)



# *******************************************************************************
# plotting
# -------------------------------------------------------------------------------

assign(endpoint.name, Surv(outcomeT,outcome))

  # ** model [3] **
  f.1 = c("trt.g2","trt.g3","age","gender","race4","smk.cat3","smkpkyrs","bmi","educ3","incPovR","exercise","tot.cvd","pt.dm","stroke","htn2")
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
fit1 <- survfit(as.formula(paste(endpoint.name,"~1")),data=ck.dat,subset=(tertile==1))
fit2 <- survfit(as.formula(paste(endpoint.name,"~1")),data=ck.dat,subset=(tertile==2))
fit3 <- survfit(as.formula(paste(endpoint.name,"~1")),data=ck.dat,subset=(tertile==3))

plot(fit1, fun='event',xscale=1,xmax=210,ylim=c(0,1),mark.time=F, conf.int=F, col=1, xlab="Months of follow-up",main=endpoint.name)
lines(fit2,fun='event',xscale=1,xmax=210,ylim=c(0,1),mark.time=F, lty=2, conf.int=F, col=2, xlab="Months of follow-up")
lines(fit3,fun='event',xscale=1,xmax=210,ylim=c(0,1),mark.time=F, lty=3, conf.int=F, col=3, xlab="Months of follow-up")

legend("topleft",c("T1 [Reference]",sprintf("T2 %s %s",stat.plot1,pv.plot[1]),
sprintf("T3 %s %s",stat.plot2,pv.plot[2])),
lty=c(1,2,3),cex=0.7,text.col =c(1,2,3), bg ='white',bty="n")






# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# *   Table 3  outcomes 
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# ---------------------------------------

outcomes <- c("htn2","tot.cvd","pt.dm","stroke")
#out2 <- unlist(lapply(outcomes,paste0,c("_OR","_Pv")))
tmp <- unlist(lapply(c("mod1","mod2","mod3"),paste0,c("_gr2","_gr3")))
out.data <- ck.dat

for (i in 1:length(outcomes)) {

	out.var <- outcomes[i]
	#out.var <- "htn2"
	print(out.var)
	
	# ---- model ----- * 
	f.1 = c("age","gender","race4","smk.cat3","trt.g2","trt.g3")
	fmla.1 <- as.formula(paste(out.var,paste(f.1,collapse="+"),sep=" ~ "))
	fit.1 <- glm(fmla.1,family=binomial(logit),data=out.data)				
	confint <- round(exp(confint(fit.1)),digit=2)	
	pv <- round(summary(fit.1)$coefficient[,4],digit=4)
	or <- paste(round(exp(fit.1$coef),digit=2)," (",confint[,1],"-",confint[,2],")",sep="")
	output <- cbind(or,pv)
	output1 <- output[(nrow(output)-1):nrow(output),]	
	
	f.2 = c("age","gender","race4","smk.cat3","smkpkyrs","bmi","educ3","incPovR","exercise","trt.g2","trt.g3")
	fmla.2 <- as.formula(paste(out.var,paste(f.2,collapse="+"),sep=" ~ "))
	fit.2 <- glm(fmla.2,family=binomial(logit),data=out.data)				
	confint <- round(exp(confint(fit.2)),digit=2)	
	pv <- round(summary(fit.2)$coefficient[,4],digit=4)
	or <- paste(round(exp(fit.2$coef),digit=2)," (",confint[,1],"-",confint[,2],")",sep="")
	output <- cbind(or,pv)
	output2 <- output[(nrow(output)-1):nrow(output),]		
	
	f.3 = c("age","gender","race4","smk.cat3","smkpkyrs","bmi","educ3","incPovR","exercise",
	        "totchol","hba1c","sbp","dbp","trt.g2","trt.g3")
	#f.3 = c("age","gender","race4","smk.cat3","smkpkyrs","bmi","educ3","incPovR","exercise",
	#        "totchol","hba1c","trt.g2","trt.g3")
	fmla.3 <- as.formula(paste(out.var,paste(f.3,collapse="+"),sep=" ~ "))
	fit.3 <- glm(fmla.3,family=binomial(logit),data=out.data)				
	confint <- round(exp(confint(fit.3)),digit=2)	
	pv <- round(summary(fit.3)$coefficient[,4],digit=4)
	or <- paste(round(exp(fit.3$coef),digit=2)," (",confint[,1],"-",confint[,2],")",sep="")
	output <- cbind(or,pv)
	output3 <- output[(nrow(output)-1):nrow(output),]		
	
	tmp <- cbind(tmp,rbind(output1,output2,output3))
}

out2 <- tmp[,2:ncol(tmp)]
colnames(out2) <- unlist(lapply(outcomes,paste0,c("_OR","_Pv")))
rownames(out2) <- unlist(lapply(c("mod1","mod2","mod3"),paste0,c("_gr2","_gr3")))

write.table(out2,"T3_b_model1-3.txt",sep="\t",row.names=T,col.names=T)


# ~~~~~~~~~~~~~~~~~~~~~~ ck trend ~~~~~~~~~~~~~~~~~~~~~~~~~
out.var <- "htn2"

	#f.1 = c("age","gender","race4","smk.cat3","tertile")
	f.1 = c("age","gender","race4","smk.cat3","smkpkyrs","bmi","educ3","incPovR","exercise","tertile")
	#f.1 = c("age","gender","race4","smk.cat3","smkpkyrs","bmi","educ3","incPovR","exercise","totchol","hba1c","tertile")
    #f.1 = c("age","gender","race4","smk.cat3","smkpkyrs","bmi","educ3","incPovR","exercise","totchol","hba1c","sbp","dbp","tertile")
	fmla.1 <- as.formula(paste(out.var,paste(f.1,collapse="+"),sep=" ~ "))
	fit.1 <- glm(fmla.1,family=binomial(logit),data=out.data)	
    summary(fit.1)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# * detail of model 4 survival
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

summary(cox.fit)

Call:
coxph(formula = fmla, data = ck.dat)

  n= 5309, number of events= 807 
   (9620 observations deleted due to missingness)

               coef  exp(coef)   se(coef)      z Pr(>|z|)    
trt.g2    0.2680328  1.3073900  0.1035088  2.589 0.009612 ** 
trt.g3    0.4511560  1.5701261  0.1049883  4.297 1.73e-05 ***
age       0.0780733  1.0812019  0.0057343 13.615  < 2e-16 ***
gender   -0.4283766  0.6515660  0.0860242 -4.980 6.37e-07 ***
race4    -0.1217063  0.8854084  0.0468462 -2.598 0.009377 ** 
smk.cat3  0.1396374  1.1498568  0.0586509  2.381 0.017274 *  
smkpkyrs  0.0020406  1.0020427  0.0013513  1.510 0.131018    
bmi      -0.0479304  0.9532002  0.0085801 -5.586 2.32e-08 ***
educ3    -0.0487252  0.9524428  0.0471719 -1.033 0.301638    
incPovR  -0.1316836  0.8766183  0.0296357 -4.443 8.85e-06 ***
exercise -0.2426739  0.7845273  0.0371451 -6.533 6.44e-11 ***
tot.cvd   0.3182597  1.3747332  0.0842277  3.779 0.000158 ***
pt.dm     0.3734718  1.4527696  0.0993751  3.758 0.000171 ***
stroke    0.3814269  1.4643726  0.1078404  3.537 0.000405 ***
htn2     -0.0485932  0.9525685  0.1247202 -0.390 0.696819    
fneck    -0.8862719  0.4121896  0.3071658 -2.885 0.003910 ** 
hipFrc   -0.1110298  0.8949121  0.2052418 -0.541 0.588528    
totchol  -0.0011732  0.9988275  0.0008979 -1.307 0.191363    
hba1c     0.0411365  1.0419943  0.0417003  0.986 0.323898    
sbp       0.0031886  1.0031937  0.0020348  1.567 0.117108    
dbp       0.0001407  1.0001407  0.0029145  0.048 0.961488    
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

         exp(coef) exp(-coef) lower .95 upper .95
trt.g2      1.3074     0.7649    1.0673    1.6014
trt.g3      1.5701     0.6369    1.2781    1.9289
age         1.0812     0.9249    1.0691    1.0934
gender      0.6516     1.5348    0.5505    0.7712
race4       0.8854     1.1294    0.8077    0.9706
smk.cat3    1.1499     0.8697    1.0250    1.2899
smkpkyrs    1.0020     0.9980    0.9994    1.0047
bmi         0.9532     1.0491    0.9373    0.9694
educ3       0.9524     1.0499    0.8683    1.0447
incPovR     0.8766     1.1407    0.8272    0.9290
exercise    0.7845     1.2747    0.7294    0.8438
tot.cvd     1.3747     0.7274    1.1655    1.6215
pt.dm       1.4528     0.6883    1.1957    1.7652
stroke      1.4644     0.6829    1.1854    1.8090
htn2        0.9526     1.0498    0.7460    1.2163
fneck       0.4122     2.4261    0.2258    0.7526
hipFrc      0.8949     1.1174    0.5985    1.3381
totchol     0.9988     1.0012    0.9971    1.0006
hba1c       1.0420     0.9597    0.9602    1.1307
sbp         1.0032     0.9968    0.9992    1.0072
dbp         1.0001     0.9999    0.9944    1.0059

Concordance= 0.774  (se = 0.009 )
Likelihood ratio test= 871.5  on 21 df,   p=<2e-16
Wald test            = 783.5  on 21 df,   p=<2e-16
Score (logrank) test = 895.1  on 21 df,   p=<2e-16




# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

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


