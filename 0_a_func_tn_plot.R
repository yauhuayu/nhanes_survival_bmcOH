# ==========================================================================================
# [1] createCV <- function(data)    
#                                       -> output 10 fold CV indexes
#
# [2] table2.out <- function(strata,data,vars.continuous,vars.categorical,ncolck) 
#                                       -> output demographic tables 
# [3] get.mdtc.out <- function(data,dtc)
#                                       -> 
# [4] get.mage.out <- function(data,age) 
#                                       -> 
#
# [5] plot.heat.type <- function(data,col,title) 
#
# [6] plot.heat.ical <- function(data,col,title)
#
# ==========================================================================================


# *********  [1]  *********    *********   *********  *********  [1]   *********    *********     *********  *********  [1]    *********  [1]   ********* 
#  
# set cx val ind

createCV <- function(data) {
	ck.dat <- data
	ind.all <- 1:nrow(ck.dat)
	ind.ck <- rep(T,nrow(ck.dat))
	ind.n <- round(0.1*nrow(ck.dat))

	ind.1 <- sample(ind.all[ind.ck],ind.n)
	ind.ck[ind.1] <- F
	ind.2 <- sample(ind.all[ind.ck],ind.n)
	ind.ck[ind.2] <- F
	ind.3 <- sample(ind.all[ind.ck],ind.n)
	ind.ck[ind.3] <- F
	ind.4 <- sample(ind.all[ind.ck],ind.n)
	ind.ck[ind.4] <- F
	ind.5 <- sample(ind.all[ind.ck],ind.n)
	ind.ck[ind.5] <- F
	ind.6 <- sample(ind.all[ind.ck],ind.n)
	ind.ck[ind.6] <- F
	ind.7 <- sample(ind.all[ind.ck],ind.n)
	ind.ck[ind.7] <- F
	ind.8 <- sample(ind.all[ind.ck],ind.n)
	ind.ck[ind.8] <- F
	ind.9 <- sample(ind.all[ind.ck],ind.n)
	ind.ck[ind.9] <- F
	ind.10 <- ind.all[ind.ck]
	#print(length(ind.10))
	cv.ind <- list(ind.1,ind.2,ind.3,ind.4,ind.5,ind.6,ind.7,ind.8,ind.9,ind.10)
	return(cv.ind)
}


create5xV <- function(data) {
	ck.dat <- data
	ind.all <- 1:nrow(ck.dat)
	ind.ck <- rep(T,nrow(ck.dat))
	ind.n <- round(0.2*nrow(ck.dat))

	ind.1 <- sample(ind.all[ind.ck],ind.n)
	ind.ck[ind.1] <- F
	ind.2 <- sample(ind.all[ind.ck],ind.n)
	ind.ck[ind.2] <- F
	ind.3 <- sample(ind.all[ind.ck],ind.n)
	ind.ck[ind.3] <- F
	ind.4 <- sample(ind.all[ind.ck],ind.n)
	ind.ck[ind.4] <- F
	ind.5 <- ind.all[ind.ck]
	#print(length(ind.10))
	cv.ind <- list(ind.1,ind.2,ind.3,ind.4,ind.5)
	return(cv.ind)
}




# *********  [2]  *********    *********      *********  *********  [2]  *********    *********    *********  *********  [2]    *********  [2]   ********* 

table2.out <- function(strata,data,vars.continuous,vars.categorical,ncolck) {
	
	reg.data <- data
	strata.factor <- as.factor(reg.data[,strata])

	N <- data.matrix(table(strata.factor))					# distribution of stratified factor !!!
	cat("N:\n",N,"\n")

	# *** process continuous vars ***
	continuous.var.median <- 
	       lapply( vars.continuous,function(bsln.var) {
	       	       m.rslt <- tapply(reg.data[,bsln.var],strata.factor,mean,na.rm=T)
                   sd.rslt <- tapply(reg.data[,bsln.var],strata.factor,sd,na.rm=T)
                   rslt <- paste(format(m.rslt,digits=2,nsmall=2)," (",format(sd.rslt,digits=1,nsmall=1),")",sep="")
                   rslt })
	names(continuous.var.median) <- vars.continuous


	continuous.var.p <- lapply( vars.continuous,function(bsln.var) {
                                              fmla <- as.formula(paste(bsln.var,"as.factor(strata.factor)",sep="~"))
                                              print(fmla)			
                                              #fit <- lm( fmla, reg.data )
                                              #print(fit.sum <- summary(fit))
                                              #f <- fit.sum$fstatistic
                                              #p <- 1 - pf(f["value"],f["numdf"],f["dendf"])  
                                              fit <- kruskal.test( fmla, reg.data )
                                              p<- fit$p.value      
                                              p })
	names(continuous.var.p) <- vars.continuous


	# *** process categorical vars ***
	multivariate.var.N.fr <- 
	     lapply( vars.categorical,function(multivariate.var) {
                     rslt <- tapply(reg.data[,multivariate.var],strata.factor,function(vals) {
                                        N.cat <- table( vals )
                                        N.tot <- sum(N.cat)
                                        fr <- format(100*N.cat/N.tot,nsmall=1,digits=1)
                                        tmp <- paste(N.cat," (",fr,")",sep="")
                                        names(tmp) <- names(N.cat)
                                        tmp })
                               rslt })
	names(multivariate.var.N.fr) <- vars.categorical

	multivariate.var.p <- sapply( vars.categorical,function(multivariate.var) {
					      rslt <- chisq.test(reg.data[,multivariate.var],strata.factor,simulate.p.value = TRUE)   # ** stratified ** #
					             print(multivariate.var)                                        
					             print(rslt)
                                 p <- rslt$p.value
                                 p })
	names(multivariate.var.p) <- vars.categorical

	m.table.entries <- list()
		for ( multivariate.var in vars.categorical ) {
		   tmp <- matrix(unlist(multivariate.var.N.fr[[multivariate.var]]),ncol=ncolck)  # ** Number of Grp ** #
		   rownames(tmp) <- paste(multivariate.var,names(multivariate.var.N.fr[[multivariate.var]][[1]]),sep=".")
		   colnames(tmp) <- names(multivariate.var.N.fr[[multivariate.var]])
		   m.table.entries[[multivariate.var]] <- tmp
		}

	demographic.table <- as.data.frame(t(N))
	colnames(demographic.table) <- names(demographic.table)
	r.names <- c("N")
	p.values <- ""
	for ( cont.var in vars.continuous ) {
  	  tmp <- continuous.var.median[[cont.var]]
  	  demographic.table <- rbind( demographic.table,tmp)
  	  r.names <- c(r.names,cont.var)
  	  p.values <- c(p.values,format(continuous.var.p[cont.var],digits=2,nsmall=2))
	}

	for ( m.var in vars.categorical ) {
  	  cat(m.var,"\t")
  	  tmp <- m.table.entries[[m.var]]
  	  demographic.table <- rbind( demographic.table,tmp)
  	  r.names <- c(r.names,rownames(tmp))
  	  p.values <- c(p.values,format(multivariate.var.p[m.var],digits=2,nsmall=2),rep("",nrow(tmp)-1))
	}

	rownames(demographic.table) <- r.names
	demographic.table <- cbind( demographic.table,p.values)

	
	col.labels <- sprintf("%s_%s",strata,names(demographic.table))				     # ** Groups' label ** #
	
	colnames(demographic.table) <- col.labels

	print(demographic.table)
	
	return(demographic.table)
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



# *********  [3]  *********  *********     *********     *********  [3]  *********    *********     *********  *********  [3]    *********  [3]   ********* 

get.mdtc.out <- function(data,dtc) {
	ans <- c()
	for (a in 1:29) {
		tmp <- data[dtc==(a-1)]
		ans[a] <- mean(tmp,na.rm=T)
	}		
	return(ans)
}


# *********  [4]  *********    *********     *********    *********  [4]  *********   *********     *********  *********  [4]    *********  [4]   ********* 
get.mage.out <- function(data,age) {
	ans <- c()
	for (a in 1:85) {
		tmp <- data[age==a]
		if (length(tmp)>0) {
			ans[a] <- mean(tmp,na.rm=T)
		}
		else
		ans[a] <- NA
		#print(mean(tmp))
	}		
	return(ans)
}




# *********  [5]  *********    *********     *********   *********  [5]  *********  *********      *********  *********  [5]    *********  [5]   ********* 
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
plot.heat.type <- function(data,col,title) {

data=data

uM.dtc <- get.mdtc.out(data$uM.dtc,data$tot.dtc)/4
uP.dtc <- get.mdtc.out(data$uP.dtc,data$tot.dtc)/4
uC.dtc <- get.mdtc.out(data$uC.dtc,data$tot.dtc)/2
uI.dtc <- get.mdtc.out(data$uI.dtc,data$tot.dtc)/4

lI.dtc <- get.mdtc.out(data$lI.dtc,data$tot.dtc)/4
lC.dtc <- get.mdtc.out(data$lC.dtc,data$tot.dtc)/2
lP.dtc <- get.mdtc.out(data$lP.dtc,data$tot.dtc)/4
lM.dtc <- get.mdtc.out(data$lM.dtc,data$tot.dtc)/4

# =============
library(RColorBrewer)
library(gplots)

d.plot <- rbind(uM.dtc,uP.dtc,uC.dtc,uI.dtc,lI.dtc,lC.dtc,lP.dtc,lM.dtc)
d.plot[is.na(d.plot)] <- 0.000000001

rownames(d.plot) <- c("maxM","maxP","maxC","maxI","mandI","mandC","mandP","mandM")
colnames(d.plot) <- sprintf("Tn_%d",1:28)

mypalette<-brewer.pal(9,"Blues")
       
heatmap.2(d.plot, col=mypalette, Rowv=T,Colv=F,trace="none",main=title,ColSideColors=col,keysize=0.9,key.title="%",key.ylab='',key.xlab='')
}
# --------------------------------------------------------------------------------------------


# *********  [6]   *********   *********   *********   *********  [6]  *********  *********  *********  *********  [6]    *********  [6]   *********
plot.heat.ical <- function(data,col,title) {

data=data

uM.dtc <- get.mdtc.out(data$mx_uM[!is.na(data$mx_uM)],data$tot.dtc[!is.na(data$mx_uM)])
uP.dtc <- get.mdtc.out(data$mx_uP[!is.na(data$mx_uP)],data$tot.dtc[!is.na(data$mx_uP)])
uC.dtc <- get.mdtc.out(data$mx_uC[!is.na(data$mx_uC)],data$tot.dtc[!is.na(data$mx_uC)])
uI.dtc <- get.mdtc.out(data$mx_uI[!is.na(data$mx_uI)],data$tot.dtc[!is.na(data$mx_uI)])

lI.dtc <- get.mdtc.out(data$mx_lI[!is.na(data$mx_lI)],data$tot.dtc[!is.na(data$mx_lI)])
lC.dtc <- get.mdtc.out(data$mx_lC[!is.na(data$mx_lC)],data$tot.dtc[!is.na(data$mx_lC)])
lP.dtc <- get.mdtc.out(data$mx_lP[!is.na(data$mx_lP)],data$tot.dtc[!is.na(data$mx_lP)])
lM.dtc <- get.mdtc.out(data$mx_lM[!is.na(data$mx_lM)],data$tot.dtc[!is.na(data$mx_lM)])

# =============
library(RColorBrewer)
library(gplots)

d.plot <- rbind(uM.dtc,uP.dtc,uC.dtc,uI.dtc,lI.dtc,lC.dtc,lP.dtc,lM.dtc)
d.plot[is.na(d.plot)] <- 0.000000001

rownames(d.plot) <- c("max.M","max.P","max.C","max.I","mand.I","mand.C","mand.P","mand.M")
colnames(d.plot) <- sprintf("TN_%d",1:28)

write.table(t(d.plot),sprintf("T_heat_%s.txt",title),sep="\t",quote=F,col.names=T,row.names=T)

mypalette<-brewer.pal(9,"Greens")
       
heatmap.2(d.plot, cexRow = 0.2 + 1/log10(nrow(d.plot)),cexCol = 0.27 + 1/log10(ncol(d.plot)),col=mypalette, Rowv=T,Colv=F,trace="none",main=title,ColSideColors=col,keysize=0.9,key.title="mean",key.ylab='',key.xlab='')
}
# --------------------------------------------------------------------------------------------

