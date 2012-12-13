# TODO: Add comment
# 
# Author: triffe
###############################################################################
cntriesAll <- matrix(".",nrow = 57, ncol = 10, dimnames = list(Country=c("Armenia", "Argentina", "Burkina Faso", "Benin", "Bolivia", 
        "Brazil", "Congo (Brazzaville)", "Cote Ivoire", "Cameroon", "Colombia", 
        "Costa Rica", "Dominican Republic", "Ecuador", "Ethiopia", "Gabon", 
        "Ghana", "Guinea", "Guatemala", "Honduras", "Haiti", "Hungary", 
        "India", "Jamaica", "Jordan", "Kenya", "Kyrgyz Republic", "Cambodia", 
        "Kazakhstan", "Lesotho", "Morocco", "Madagascar", "Mali", "Mongolia", 
        "Malawi", "Mexico", "Malaysia", "Mozambique", "Namibia", "Niger", 
        "Nigeria", "Nicaragua", "Panama", "Peru", "Philipines", "Puerto Rico", 
        "Portugal", "Rwanda", "Sierra Leone", "Senegal", "Chad", "Togo", 
        "Tanzania", "Uganda", "Venezuela", "Vietnam", "South Africa", 
        "Zimbabwe"), Figure = c("1a","1b","2a","2b","3","4a","4b","5","6","7")))
# countryInd <- matrix(nrow = 57,

slopesFigs <- matrix(nrow=10,ncol=5)
colnames(slopesFigs) <- c("Figure2a","Figure2b","Figure3","Figure5","Figure6")
rownames(slopesFigs) <- 15:24

#setwd("C:/Users/triffe/git/ViennaPaper/ALBERTPAPERS/Figures/FiguresPaperBW")
setwd("/home/triffe/git/ViennaDiagnostics/ALBERTPAPERS/Figures/FiguresPaperBW")
# rethink figures to work in BW. Take into account Edward Tufte's ideas.
#install.packages("devEMF")
#library(devEMF)
#install.packages("Cairo")
#library(Cairo)
library(MethComp)

# a function used to get confidence intervals for Deming linear fits.
# Deming linear fits are used because they are symmetrical with respect to 
# x and y residuals, and therefore allow us to swap the x and y axes 
# without causing inconsistencies in the slopes (when you swap x and y here
# we want the slopes to be the inverse, which is not the case with OLS).
# this is identical to minimizing the sum of the squares of the orthogonal
# residuals = pure awesome.
DemingCI <- function(x,y,xnew,level=95,Nboot=1000){
	if (missing(xnew)){
		xnew <- seq(min(x),max(x),length=100)
	}
	Db <- Deming(x,y,boot=Nboot,keep.boot=TRUE)
	ll <- Db[,1:2] %*% rbind( 1, xnew )
	ll <- apply(ll,2,sort)
	lr <- (1-level/100)/2
	indices <- round(quantile(1:Nboot,probs=c(lr,1-lr)))
	CIs <- cbind(lower=ll[indices[1],],upper=ll[indices[2],])
	return(CIs)
}

#DATA <- read.table("//158.109.138.185/compartit$/jgarcia/viena/figures/FIGURE1_2.tab",header=T,sep="\t",na.strings = ".")
DATA <- read.table("/home/triffe/git/ViennaDiagnostics/ALBERTPAPERS/Figures/DATAfigBW/FIGURE1_2.tab",header=T,sep="\t",na.strings = ".",stringsAsFactors =FALSE)
# head(DATA)
# remove countries with 5-year age groups
#unique(DATA$country)
DATA <- DATA[!DATA$country %in% c("Italy","Palestine","Slovenia"),]
# remove USA, EU countries, per referees (I don't think they should be removed- this isn't a paper about N-S-E-W, it's about marriage timing and levels...
# there are poor countries that marry late and rich countries that marry early...)
DATA <- DATA[!DATA$country %in% c("Switzerland","France","Spain","Austria","Romania","Belarus","United States"),]
cntriesf <- as.character(unique(DATA$country[DATA$SEX=="Female"]))
cntriesm <- as.character(unique(DATA$country[DATA$SEX=="Male"]))
# these will be either smooth values or original, depending on badness,
# see criterion in code unique(DATA$country)
DATAadj <- DATA
femadj <- matrix(0,nrow=length(cntriesf),ncol=3)
colnames(femadj) <- c("prop_union","prop_childless","prop_school")
rownames(femadj) <- cntriesf
DATAadj$prop_childless[DATAadj$AGE<15] <- NA
DATAadj$prop_union[DATAadj$AGE<15] <- NA

# do adjustments, where necessary, females
for (i in 1:length(cntriesf)){
	# first prop_union
	ind <- DATA$country == cntriesf[i] & DATA$SEX == "Female"  & DATA$AGE > 14
	x <- DATA$AGE[ind]
	y <- 100*DATA$prop_union[ind]
	if (sum(sign(diff(y)) == -1,na.rm=TRUE) > 0 & min(diff(y),na.rm=TRUE) < -.5){
		x2 <- x[!is.na(y)]
		y <- smooth.spline(x2,y[!is.na(y)],spar=.4)$y
		DATAadj$prop_union[ind & DATA$AGE %in% x2] <- y/100
		femadj[i,1] <- 1
	}
	y <- 100*(1-DATA$prop_childless[ind])
	if (sum(sign(diff(y)) == -1,na.rm=TRUE) > 0 & min(diff(y),na.rm=TRUE) > -.5){
		x2 <- x[!is.na(y)]
		y <- smooth.spline(x2,y[!is.na(y)],spar=.4)$y
		DATAadj$prop_childless[ind & DATA$AGE %in% x2] <- 1-(y/100)
		femadj[i,2] <- 1
	}
	y <- 100*DATA$prop_school[ind]
	if (sum(sign(diff(y)) == 1,na.rm=TRUE) > 0 & max(diff(y),na.rm=TRUE) > .5){
		x2 <- x[!is.na(y)]
		y <- smooth.spline(x2,y[!is.na(y)],spar=.4)$y
		DATAadj$prop_school[ind & DATA$AGE %in% x2] <- y/100
		femadj[i,3] <- 1
	}
}

# males
maladj <- matrix(0,nrow=length(cntriesm),ncol=2)
colnames(maladj) <- c("prop_union","prop_school")
rownames(maladj) <- cntriesm
for (i in 1:length(cntriesm)){
	# first prop_union
	ind <- DATA$country == cntriesm[i] & DATA$SEX == "Male"  & DATA$AGE > 14
	x <- DATA$AGE[ind]
	y <- 100*DATA$prop_union[ind]
	if (sum(sign(diff(y)) == -1,na.rm=TRUE) > 0 & min(diff(y),na.rm=TRUE) < -.5){
		x2 <- x[!is.na(y)]
		y <- smooth.spline(x2,y[!is.na(y)],spar=.4)$y
		DATAadj$prop_union[ind & DATA$AGE %in% x2] <- y/100
		maladj[i,1] <- 1
	}
	y <- 100*DATA$prop_school[ind]
	if (sum(sign(diff(y)) == 1,na.rm=TRUE) > 0 & max(diff(y),na.rm=TRUE) > .5){
		x2 <- x[!is.na(y)]
		y <- smooth.spline(x2,y[!is.na(y)],spar=.4)$y
		DATAadj$prop_school[ind & DATA$AGE %in% x2] <- y/100
		maladj[i,2] <- 1
	}
}

# some pdfs to save, in order to see the before and after, only affected countries
# the criteria for smoothing and smoothing parameters are always the same
pdf(height=8,width=8,"smooth1femunion.pdf")
par(mfrow=c(2,2))
for (i in 1:length(cntriesf)){
	x <- DATA$AGE[DATA$country == cntriesf[i] & DATA$SEX == "Female"  & DATA$AGE > 14]
	y <- 100*DATA$prop_union[DATA$country == cntriesf[i] & DATA$SEX == "Female"  & DATA$AGE > 14]
	if (sum(sign(diff(y)) == -1,na.rm=TRUE) > 0 & min(diff(y),na.rm=TRUE) < -.5){
		plot(x,y,xlim=c(15,24),ylim=c(0,100),xlab="Age",ylab="% in union",main=cntriesf[i],col="blue",pch=19)
		x <- x[!is.na(y)]
		y <- smooth.spline(x,y[!is.na(y)],spar=.4)$y
		lines(x,y,lwd=2,col="#0000FF70")
	}
}
dev.off()
getwd()
pdf(height=8,width=8,"smooth1femchild.pdf")
par(mfrow=c(2,2))
for (i in 1:length(cntriesf)){
	x <- DATA$AGE[DATA$country == cntriesf[i] & DATA$SEX == "Female" & DATA$AGE > 14]
	y <- 100*(1-DATA$prop_child[DATA$country == cntriesf[i] & DATA$SEX == "Female" & DATA$AGE > 14])
	if (sum(sign(diff(y)) == -1,na.rm=TRUE) > 0 & min(diff(y),na.rm=TRUE) < -.5){
		plot(x,y,xlim=c(15,24),ylim=c(0,100),xlab="Age",ylab="% mother",main=cntriesf[i],col="blue",pch=19)
		x <- x[!is.na(y)]
		y <- smooth.spline(x,y[!is.na(y)],spar=.4)$y
		lines(x,y,lwd=2,col="#0000FF70")
	}
}
dev.off()

pdf(height=8,width=8,"smooth1maleunion.pdf")
par(mfrow=c(2,2))
for (i in 1:length(cntriesm)){
	x <- DATA$AGE[DATA$country == cntriesm[i] & DATA$SEX == "Male"  & DATA$AGE > 14]
	y <- 100*DATA$prop_union[DATA$country == cntriesm[i] & DATA$SEX == "Male"  & DATA$AGE > 14]
	x <- x[!is.na(y)]
	y <- y[!is.na(y)]
	if (length(x) > 3){
		if (sum(sign(diff(y)) == -1,na.rm=TRUE) > 0 & min(diff(y),na.rm=TRUE) < -.5){
			plot(x,y,xlim=c(15,24),ylim=c(0,100),xlab="Age",ylab="% in union",main=cntriesm[i],col="blue",pch=19)
			x <- x[!is.na(y)]
			y <- smooth.spline(x,y[!is.na(y)],spar=.4)$y
			lines(x,y,lwd=2,col="#0000FF70")
		}
	}
}
dev.off()

pdf(height=8,width=8,"smooth1maleschool.pdf")
par(mfrow=c(2,2))
for (i in 1:length(cntriesm)){
	x <- DATA$AGE[DATA$country == cntriesm[i] & DATA$SEX == "Male"  & DATA$AGE > 14]
	y <- 100*DATA$prop_school[DATA$country == cntriesm[i] & DATA$SEX == "Male"  & DATA$AGE > 14]
	x <- x[!is.na(y)]
	y <- y[!is.na(y)]
	if (length(x) > 3){
		if (sum(sign(diff(y)) == 1,na.rm=TRUE) > 0 & max(diff(y),na.rm=TRUE) > .5){
			plot(x,y,xlim=c(15,24),ylim=c(0,100),xlab="Age",ylab="% student",main=cntriesm[i],col="blue",pch=19)
			x <- x[!is.na(y)]
			y <- smooth.spline(x,y[!is.na(y)],spar=.4)$y
			lines(x,y,lwd=2,col="#0000FF70")
		}
	}
}
dev.off()

pdf(height=8,width=8,"smooth1femschool.pdf")
par(mfrow=c(2,2))
for (i in 1:length(cntriesf)){
	x <- DATA$AGE[DATA$country == cntriesf[i] & DATA$SEX == "Female"  & DATA$AGE > 14]
	y <- 100*DATA$prop_school[DATA$country == cntriesf[i] & DATA$SEX == "Female"  & DATA$AGE > 14]
	x <- x[!is.na(y)]
	y <- y[!is.na(y)]
	if (length(x) > 3){
		if (sum(sign(diff(y)) == 1,na.rm=TRUE) > 0 & max(diff(y),na.rm=TRUE) > .5){
			plot(x,y,xlim=c(15,24),ylim=c(0,100),xlab="Age",ylab="% student",main=cntriesf[i],col="blue",pch=19)
			x <- x[!is.na(y)]
			y <- smooth.spline(x,y[!is.na(y)],spar=.4)$y
			lines(x,y,lwd=2,col="#0000FF70")
		}
	}
}
dev.off()

# mark indicator matrix:
cntriesAll[rownames(cntriesAll) %in% rownames(femadj),1] <- "X"
cntriesAll[rownames(cntriesAll) %in% rownames(maladj),2] <- "X"
# females boxplot, Tufte-style
ww <- .09
lfs <- -.25
rgs <- .25
s.lwd <- 1.5
#emf(file="Figure1aBW.emf",width=7,height=7)
pdf(file="Figure1aBW.pdf",width=7,height=7)
plot(NULL,type="n",xlim=c(12,24),ylim=c(0,100),axes=FALSE,xlab="Age",ylab="",family="serif")
par(xpd=TRUE)
for (i in 12:24){
	s5 <- fivenum(100*DATAadj$prop_school[DATAadj$SEX == "Female"  & DATAadj$AGE == i])
	rect(i+lfs+ww,s5[2],i+lfs-ww,s5[4],col=gray(.6),border=NA)
	segments(i+lfs,s5[1],i+lfs,s5[5],col=gray(.6), lwd = s.lwd)
	segments(i+lfs+ww,s5[3],i+lfs-ww,s5[3],col="white", lwd = s.lwd)
	if (i > 14){
		u5 <- fivenum(100*DATAadj$prop_union[DATAadj$SEX == "Female" & DATAadj$AGE == i])
		rect(i+ww,u5[2],i-ww,u5[4],col=gray(.4),border=NA)
		segments(i,u5[1],i,u5[5],col=gray(.4), lwd = s.lwd)
		segments(i+ww,u5[3],i-ww,u5[3],col="white", lwd = s.lwd)
		
		m5 <- fivenum(100*(1-DATAadj$prop_child[DATAadj$SEX == "Female" & DATAadj$AGE == i]))
		rect(i+ww+rgs,m5[2],i-ww+rgs,m5[4],col=gray(.2),border=NA)
		segments(i+rgs,m5[1],i+rgs,m5[5],col=gray(.2), lwd = s.lwd)
		segments(i+ww+rgs,m5[3],i-ww+rgs,m5[3],col="white", lwd = s.lwd)
	}
}
legend(11.4,17,fill=c(gray(.6),gray(.4),gray(.2)),legend=c("student","spouse","mother"),bty="n",border=NA)
segments(11.4,0,11.4,100,col=gray(.5), lwd = s.lwd) # y axis frame
segments(11.4,0,24.5,0,col=gray(.5), lwd = s.lwd) # x axis frame
# y axis ticks:
segments(11.4,(0:5)*20,11.3,(0:5)*20,col=gray(.5), lwd = s.lwd)
text(11.4,(0:5)*20,paste((0:5)*20,"%",sep=""),col=gray(.2),pos=2,cex=.8,family="serif")
segments(12.5:24.5,0,12.5:24.5,-.8,col=gray(.5), lwd = s.lwd)
text(12:24,0,12:24,pos=1,cex=.8,col=gray(.2),family="serif")
dev.off()

# similar thing, but for males:
ww <- .09
lfs <- -.23
rgs <- .23
s.lwd <- 1.5
#emf(file="Figure1bBW.emf",width=7,height=7)
pdf(file="Figure1bBW.pdf",width=7,height=7)
plot(NULL,type="n",xlim=c(12,24),ylim=c(0,100),axes=FALSE,xlab="Age",ylab="",family="serif")
par(xpd=TRUE)
for (i in 12:24){
	s5 <- fivenum(100*DATAadj$prop_school[DATAadj$SEX == "Male"  & DATAadj$AGE == i])
	rect(i+lfs+ww,s5[2],i+lfs-ww,s5[4],col=gray(.6),border=NA)
	segments(i+lfs,s5[1],i+lfs,s5[5],col=gray(.6), lwd = s.lwd)
	segments(i+lfs+ww,s5[3],i+lfs-ww,s5[3],col="white", lwd = s.lwd)
	if (i > 14){
		u5 <- fivenum(100*DATAadj$prop_union[DATAadj$SEX == "Male" & DATAadj$AGE == i])
		rect(i+ww,u5[2],i-ww,u5[4],col=gray(.4),border=NA)
		segments(i,u5[1],i,u5[5],col=gray(.4), lwd = s.lwd)
		segments(i+ww,u5[3],i-ww,u5[3],col="white", lwd = s.lwd)
	}
}
legend(11.4,17,fill=c(gray(.6),gray(.4)),legend=c("student","spouse"),bty="n",border=NA)
segments(11.4,0,11.4,100,col=gray(.5), lwd = s.lwd) # y axis frame
segments(11.4,0,24.5,0,col=gray(.5), lwd = s.lwd) # x axis frame
# y axis ticks:
segments(11.4,(0:5)*20,11.3,(0:5)*20,col=gray(.5), lwd = s.lwd)
text(11.4,(0:5)*20,paste((0:5)*20,"%",sep=""),col=gray(.2),pos=2,cex=.8,family="serif")
segments(12.5:24.5,0,12.5:24.5,-.8,col=gray(.5), lwd = s.lwd)
text(12:24,0,12:24,pos=1,cex=.8,col=gray(.2),family="serif")
dev.off()

########
# now scatterplots
# female education vs union:
# mark indicator matrix:

cntriesAll[rownames(cntriesAll) %in% rownames(femadj),3] <- "X"
cntriesAll[rownames(cntriesAll) %in% rownames(maladj),4] <- "X"
#png(file="Figure2aBW.png",height=7,width=7,units="in",res=300,antialias="gray")
pdf(file="Figure2aBW.pdf",height=7,width=7)
par(xaxs="i",yaxs="i")
plot(NULL,type="n",xlim=c(0,100),ylim=c(0,100),axes=FALSE,xlab="",ylab="",asp=1,col=paste(gray(.5),50,sep=""),family="serif")
for (i in c(16,18,20,22,24)){
	x <- 100 * DATAadj$prop_school[DATAadj$SEX=="Female" & DATAadj$AGE == i]
	y <- 100 * DATAadj$prop_union[DATAadj$SEX=="Female"  & DATAadj$AGE == i]
	xnew <- seq(min(x,na.rm=T),max(x,na.rm=T),length.out=200)
	coefs <- Deming(x,y)[1:2]
	CIs <- DemingCI(x,y,xnew)
	polygon(c(xnew,rev(xnew)),c(CIs[,1],rev(CIs[,2])),col=paste(gray(.8),50,sep=""),border=FALSE)
	segments(xnew[1],coefs[1]+xnew[1]*coefs[2],xnew[200],coefs[1]+xnew[200]*coefs[2],col=gray(.4))
	text(xnew[1],coefs[1]+xnew[1]*coefs[2],i,cex=1.1,col=gray(.2),xpd=TRUE,pos=2,family="serif")
}
segments(-5,(0:5)*20,-6,(0:5)*20,col=gray(.2),xpd=TRUE)
text(-5,(0:5)*20,paste((0:5)*20,"%",sep=""),pos=2,cex=.8,col=gray(.2),xpd=TRUE,family="serif")
segments((0:5)*20,-5,(0:5)*20,-6,col=gray(.2),xpd=TRUE)
text((0:5)*20,-5,paste((0:5)*20,"%",sep=""),pos=1,cex=.8,col=gray(.2),xpd=TRUE,family="serif")
text(50,-12,"Student",pos=1,col=gray(.2),xpd=TRUE,family="serif")
text(-15,50,"Spouse",col=gray(.2),xpd=TRUE,family="serif")
ses <- slopes <- c()
for (i in 15:24){
	x <- 100 * DATAadj$prop_school[DATAadj$SEX=="Female" & DATAadj$AGE == i]
	y <- 100 * DATAadj$prop_union[DATAadj$SEX=="Female"  & DATAadj$AGE == i]
	coefs <- Deming(x,y)[1:2]
	slopes[i-14] <- coefs[2]
}
dev.off()
slopesFigs[,1] <- slopes
#
# males edu and spouse: or just not include this graphic at all, 
# since results are not significant in any age- no overlap = no relation
#png(file="Figure2bBW.png",height=7,width=7,units="in",res=300,antialias="gray")
pdf(file="Figure2bBW.pdf",height=7,width=7)
plot(NULL,type="n",xlim=c(0,100),ylim=c(0,100),axes=FALSE,xlab="",ylab="",asp=1,col=paste(gray(.5),50,sep=""),family="serif")
for (i in c(16,18,20,22,24)){
	x <- 100 * DATAadj$prop_school[DATAadj$SEX=="Male" & DATAadj$AGE == i]
	y	 <- 100 * DATAadj$prop_union[DATAadj$SEX=="Male"  & DATAadj$AGE == i]
	minx <- min(x,na.rm=T) ; maxx <- max(x,na.rm=T)
	coefs <- Deming(x,y)[1:2]
	xnew <- seq(min(x,na.rm=T),max(x,na.rm=T),length.out=200)
	CIs <- DemingCI(x,y,xnew)
	polygon(c(xnew,rev(xnew)),c(CIs[,1],rev(CIs[,2])),col=paste(gray(.8),50,sep=""),border=FALSE)
	segments(xnew[1],coefs[1]+xnew[1]*coefs[2],xnew[200],coefs[1]+xnew[200]*coefs[2],col=gray(.4))
	text(xnew[1],coefs[1]+xnew[1]*coefs[2],i,cex=1.1,col=gray(.2),xpd=TRUE,pos=2,family="serif")
}
segments(-5,(0:5)*20,-6,(0:5)*20,col=gray(.2),xpd=TRUE)
text(-5,(0:5)*20,paste((0:5)*20,"%",sep=""),pos=2,cex=.8,col=gray(.2),xpd=TRUE,family="serif")
segments((0:5)*20,-5,(0:5)*20,-6,col=gray(.2),xpd=TRUE)
text((0:5)*20,-5,paste((0:5)*20,"%",sep=""),pos=1,cex=.8,col=gray(.2),xpd=TRUE,family="serif")
text(50,-12,"Student",pos=1,col=gray(.2),xpd=TRUE,family="serif")
text(-15,50,"Spouse",col=gray(.2),xpd=TRUE,family="serif")
ses <- slopes <- c()
for (i in 15:24){
	x <- 100 * DATAadj$prop_school[DATAadj$SEX=="Male" & DATAadj$AGE == i]
	y	 <- 100 * DATAadj$prop_union[DATAadj$SEX=="Male"  & DATAadj$AGE == i]
	coefs <- Deming(x,y)[1:2]
	slopes[i-14] <- coefs[2]
}
dev.off()
slopesFigs[,2] <- slopes

# females edu and mother
#png(file="Figure3BW.png",height=7,width=7,units="in",res=300,antialias="gray")
cntriesAll[rownames(cntriesAll) %in% rownames(femadj),5] <- "X"
pdf(file="Figure3BW.pdf",height=7,width=7)
plot(NULL,type="n",xlim=c(0,100),ylim=c(0,100),axes=FALSE,xlab="",ylab="",asp=1,col=paste(gray(.5),50,sep=""),family="serif")
for (i in c(8:12)*2){
	x <- 100 * DATAadj$prop_school[DATAadj$SEX=="Female" & DATAadj$AGE == i]
	y <- 100 * (1-DATAadj$prop_child[DATAadj$SEX=="Female"  & DATAadj$AGE == i])
	minx <- min(x,na.rm=T) ; maxx <- max(x,na.rm=T)
	coefs <- Deming(x,y)[1:2]
	xnew <- seq(min(x,na.rm=T),max(x,na.rm=T),length.out=200)
	CIs <- DemingCI(x,y,xnew)
	polygon(c(xnew,rev(xnew)),c(CIs[,1],rev(CIs[,2])),col=paste(gray(.8),50,sep=""),border=FALSE)
	segments(xnew[1],coefs[1]+xnew[1]*coefs[2],xnew[200],coefs[1]+xnew[200]*coefs[2],col=gray(.4))
	text(xnew[1],coefs[1]+xnew[1]*coefs[2],i,cex=1.1,col=gray(.2),xpd=TRUE,pos=2,family="serif")
}
segments(-5,(0:5)*20,-6,(0:5)*20,col=gray(.2),xpd=TRUE)
text(-5,(0:5)*20,paste((0:5)*20,"%",sep=""),pos=2,cex=.8,col=gray(.2),xpd=TRUE,family="serif")
segments((0:5)*20,-5,(0:5)*20,-6,col=gray(.2),xpd=TRUE)
text((0:5)*20,-5,paste((0:5)*20,"%",sep=""),pos=1,cex=.8,col=gray(.2),xpd=TRUE,family="serif")
text(50,-12,"Student",pos=1,col=gray(.2),xpd=TRUE,family="serif")
text(-15,50,"Mother",col=gray(.2),xpd=TRUE,family="serif")
ses <- slopes <- c()
for (i in 15:24){
	x <- 100 * DATAadj$prop_school[DATAadj$SEX=="Female" & DATAadj$AGE == i]
	y <- 100 * (1-DATAadj$prop_child[DATAadj$SEX=="Female"  & DATAadj$AGE == i])
	coefs <- Deming(x,y)[1:2]
	slopes[i-14] <- coefs[2]
}
dev.off()
slopesFigs[,3] <- slopes

# Figure 4
DATA <- read.table("/home/triffe/git/ViennaDiagnostics/ALBERTPAPERS/Figures/DATAfigBW/FIGURE3.tab",header=T,sep="\t",na.strings = ".")
DATA$country <- as.character(DATA$country)
DATA$SEX <- as.character(DATA$SEX)

DATA <- DATA[!DATA$country %in% c("Italy","Palestine","Slovenia"),]
DATA <- DATA[!DATA$country %in% c("Switzerland","France","Spain","Austria","Romania","Belarus","United States"),]
cntriesf <- as.character(unique(DATA$country[DATA$SEX=="Female"]))
cntriesm <- as.character(unique(DATA$country[DATA$SEX=="Male"]))

# um, any smoothing necessary?
#plot(NULL,type="n",xlim=c(15,24),ylim=c(0,100))
#for (i in 1:length(cntriesf)){
#	x <- DATA$AGE[DATA$country == cntriesf[i] & DATA$SEX == "Female"  & DATA$AGE > 14 & DATA$SCHOOL == 0]
#	y <- 100*DATA$prop_union2[DATA$country == cntriesf[i] & DATA$SEX == "Female"  & DATA$AGE > 14 & DATA$SCHOOL == 0]
#	lines(x,y,lwd=1,col="#80808050")
#}


pdf(height=8,width=8,"smooth4femunioninschool.pdf")
par(mfrow=c(2,2))
for (i in 1:length(cntriesf)){
	x <- DATA$AGE[DATA$country == cntriesf[i] & DATA$SEX == "Female"  & DATA$AGE > 14 & DATA$SCHOOL == 1]
	y <- 100*DATA$prop_union2[DATA$country == cntriesf[i] & DATA$SEX == "Female"  & DATA$AGE > 14 & DATA$SCHOOL == 1]
	if (sum(sign(diff(y)) == -1,na.rm=TRUE) > 0 & min(diff(y),na.rm=TRUE) < -.5){
		plot(x,y,xlim=c(15,24),ylim=c(0,100),xlab="Age",ylab="% in union",main=cntriesf[i],col="blue",pch=19)
		x <- x[!is.na(y)]
		y <- smooth.spline(x,y[!is.na(y)],spar=.4)$y
		lines(x,y,lwd=2,col="#0000FF70")
	}
}
dev.off()

pdf(height=8,width=8,"smooth4femunionnotinschool.pdf")
par(mfrow=c(2,2))
for (i in 1:length(cntriesf)){
	x <- DATA$AGE[DATA$country == cntriesf[i] & DATA$SEX == "Female"  & DATA$AGE > 14 & DATA$SCHOOL == 0]
	y <- 100*DATA$prop_union2[DATA$country == cntriesf[i] & DATA$SEX == "Female"  & DATA$AGE > 14 & DATA$SCHOOL == 0]
	if (sum(sign(diff(y)) == -1,na.rm=TRUE) > 0 & min(diff(y),na.rm=TRUE) < -.5){
		plot(x,y,xlim=c(15,24),ylim=c(0,100),xlab="Age",ylab="% in union",main=cntriesf[i],col="blue",pch=19)
		x <- x[!is.na(y)]
		y <- smooth.spline(x,y[!is.na(y)],spar=.4)$y
		lines(x,y,lwd=2,col="#0000FF70")
	}
}
dev.off()

pdf(height=8,width=8,"smooth4femchildinschool.pdf")
par(mfrow=c(2,2))
for (i in 1:length(cntriesf)){
	x <- DATA$AGE[DATA$country == cntriesf[i] & DATA$SEX == "Female"  & DATA$AGE > 14 & DATA$SCHOOL == 1]
	y <- 100*(1-DATA$prop_child2[DATA$country == cntriesf[i] & DATA$SEX == "Female"  & DATA$AGE > 14 & DATA$SCHOOL == 1])
	if (sum(sign(diff(y)) == 1,na.rm=TRUE) > 0 & max(diff(y),na.rm=TRUE) > .5){
		plot(x,y,xlim=c(15,24),ylim=c(0,100),xlab="Age",ylab="% in union",main=cntriesf[i],col="blue",pch=19)
		x <- x[!is.na(y)]
		y <- smooth.spline(x,y[!is.na(y)],spar=.4)$y
		lines(x,y,lwd=2,col="#0000FF70")
	}
}
dev.off()

pdf(height=8,width=8,"smooth4femchildnotinschool.pdf")
par(mfrow=c(2,2))
for (i in 1:length(cntriesf)){
	x <- DATA$AGE[DATA$country == cntriesf[i] & DATA$SEX == "Female"  & DATA$AGE > 14 & DATA$SCHOOL == 0]
	y <- 100*(1-DATA$prop_child2[DATA$country == cntriesf[i] & DATA$SEX == "Female"  & DATA$AGE > 14 & DATA$SCHOOL == 0])
	if (sum(sign(diff(y)) == 1,na.rm=TRUE) > 0 & max(diff(y),na.rm=TRUE) > .5){
		plot(x,y,xlim=c(15,24),ylim=c(0,100),xlab="Age",ylab="% in union",main=cntriesf[i],col="blue",pch=19)
		x <- x[!is.na(y)]
		y <- smooth.spline(x,y[!is.na(y)],spar=.4)$y
		lines(x,y,lwd=2,col="#0000FF70")
	}
}
dev.off()

pdf(height=8,width=8,"smooth4maleunioninschool.pdf")
par(mfrow=c(2,2))
for (i in 1:length(cntriesm)){
	x <- DATA$AGE[DATA$country == cntriesm[i] & DATA$SEX == "Male"  & DATA$AGE > 14 & DATA$SCHOOL == 1]
	y <- 100*DATA$prop_union2[DATA$country == cntriesm[i] & DATA$SEX == "Male"  & DATA$AGE > 14 & DATA$SCHOOL == 1]
	if (sum(sign(diff(y)) == -1,na.rm=TRUE) > 0 & min(diff(y),na.rm=TRUE) < -.5){
		plot(x,y,xlim=c(15,24),ylim=c(0,100),xlab="Age",ylab="% in union",main=cntriesm[i],col="blue",pch=19)
		x <- x[!is.na(y)]
		y <- smooth.spline(x,y[!is.na(y)],spar=.4)$y
		lines(x,y,lwd=2,col="#0000FF70")
	}
}
dev.off()

pdf(height=8,width=8,"smooth4maleunionnotinschool.pdf")
par(mfrow=c(2,2))
for (i in 1:length(cntriesm)){
	x <- DATA$AGE[DATA$country == cntriesm[i] & DATA$SEX == "Male"  & DATA$AGE > 14 & DATA$SCHOOL == 0]
	y <- 100*DATA$prop_union2[DATA$country == cntriesm[i] & DATA$SEX == "Male"  & DATA$AGE > 14 & DATA$SCHOOL == 0]
	if (sum(sign(diff(y)) == -1,na.rm=TRUE) > 0 & min(diff(y),na.rm=TRUE) < -.5){
		plot(x,y,xlim=c(15,24),ylim=c(0,100),xlab="Age",ylab="% in union",main=cntriesm[i],col="blue",pch=19)
		x <- x[!is.na(y)]
		y <- smooth.spline(x,y[!is.na(y)],spar=.4)$y
		lines(x,y,lwd=2,col="#0000FF70")
	}
}
dev.off()

### OK, now make DATAadj

# females
DATAadj <- DATA
femadj <- matrix(0,nrow=length(cntriesf),ncol=4)
colnames(femadj) <- c("unioninschool","childlessinschool","unionnotinschool","childlessnotinschool")
rownames(femadj) <- cntriesf
DATAadj$prop_childless[DATAadj$AGE<15] <- NA
DATAadj$prop_union[DATAadj$AGE<15] <- NA

# do adjustments, where necessary, females
for (i in 1:length(cntriesf)){
	# union in school
	ind <- DATA$country == cntriesf[i] & DATA$SEX == "Female"  & DATA$AGE > 14 & DATA$SCHOOL == 1
	x <- DATA$AGE[ind]
	y <- 100*DATA$prop_union2[ind]
	if (sum(sign(diff(y)) == -1,na.rm=TRUE) > 0 & min(diff(y),na.rm=TRUE) < -.5){
		x2 <- x[!is.na(y)]
		y <- smooth.spline(x2,y[!is.na(y)],spar=.4)$y
		DATAadj$prop_union[ind & DATA$AGE %in% x2] <- y/100
		femadj[i,1] <- 1
	}
	# child in school
	y <- 100*(1-DATA$childless2[ind])
	if (sum(sign(diff(y)) == -1,na.rm=TRUE) > 0 & min(diff(y),na.rm=TRUE) > -.5){
		x2 <- x[!is.na(y)]
		y <- smooth.spline(x2,y[!is.na(y)],spar=.4)$y
		DATAadj$prop_childless[ind & DATA$AGE %in% x2] <- 1-(y/100)
		femadj[i,2] <- 1
	}
	# union not in school
	ind <- DATA$country == cntriesf[i] & DATA$SEX == "Female"  & DATA$AGE > 14 & DATA$SCHOOL == 0
	x <- DATA$AGE[ind]
	y <- 100*DATA$prop_union2[ind]
	if (sum(sign(diff(y)) == -1,na.rm=TRUE) > 0 & min(diff(y),na.rm=TRUE) < -.5){
		x2 <- x[!is.na(y)]
		y <- smooth.spline(x2,y[!is.na(y)],spar=.4)$y
		DATAadj$prop_union[ind & DATA$AGE %in% x2] <- y/100
		femadj[i,3] <- 1
	}
	# child not in school
	y <- 100*(1-DATA$childless2[ind])
	if (sum(sign(diff(y)) == -1,na.rm=TRUE) > 0 & min(diff(y),na.rm=TRUE) > -.5){
		x2 <- x[!is.na(y)]
		y <- smooth.spline(x2,y[!is.na(y)],spar=.4)$y
		DATAadj$prop_childless[ind & DATA$AGE %in% x2] <- 1-(y/100)
		femadj[i,4] <- 1
	}
}

# males
maladj <- matrix(0,nrow=length(cntriesm),ncol=2)
colnames(maladj) <- c("unioninschool","unionnotinschool")
rownames(maladj) <- cntriesm
for (i in 1:length(cntriesm)){
	# union in school
	ind <- DATA$country == cntriesm[i] & DATA$SEX == "Male"  & DATA$AGE > 14 & DATA$SCHOOL == 1
	x <- DATA$AGE[ind]
	y <- 100*DATA$prop_union2[ind]
	if (sum(sign(diff(y)) == -1,na.rm=TRUE) > 0 & min(diff(y),na.rm=TRUE) < -.5){
		x2 <- x[!is.na(y)]
		y <- smooth.spline(x2,y[!is.na(y)],spar=.4)$y
		DATAadj$prop_union2[ind & DATA$AGE %in% x2] <- y/100
		maladj[i,1] <- 1
	}
	ind <- DATA$country == cntriesm[i] & DATA$SEX == "Male"  & DATA$AGE > 14 & DATA$SCHOOL == 0
	y <- 100*DATA$prop_union2[ind]
	if (sum(sign(diff(y)) == -1,na.rm=TRUE) > 0 & min(diff(y),na.rm=TRUE) < -.5){
		x2 <- x[!is.na(y)]
		y <- smooth.spline(x2,y[!is.na(y)],spar=.4)$y
		DATAadj$prop_union2[ind & DATA$AGE %in% x2] <- y/100
		maladj[i,2] <- 1
	}
}

# mark countries used

cntriesAll[rownames(cntriesAll) %in% rownames(femadj),6] <- "X"
cntriesAll[rownames(cntriesAll) %in% rownames(maladj),7] <- "X"
### begin boxplot (figure 4a)
ww <- .09
l1 <- -.3
l2 <- -.1
r1 <- .1
r2 <- .3
s.lwd = 1.5
#emf(file="Figure4aBW.emf",width=7,height=7)
pdf(file="Figure4aBW.pdf",width=7,height=7)
plot(NULL,type="n",xlim=c(15,24),ylim=c(0,100),axes=FALSE,xlab="Age",ylab="",family="serif")
par(xpd=TRUE)
for (i in 15:24){
	# in school
	u51 <- fivenum(100*DATAadj$prop_union2[DATAadj$SEX == "Female" & DATAadj$AGE == i & DATA$SCHOOL == 1])
	rect(i+ww+l1,u51[2],i-ww+l1,u51[4],col=gray(.7),border=NA)
	segments(i+l1,u51[1],i+l1,u51[5],col=gray(.7),lwd=s.lwd)
	segments(i+l1+ww,u51[3],i+l1-ww,u51[3],col="white",lwd=s.lwd)
	
	m51 <- fivenum(100*(1-DATAadj$childless2[DATAadj$SEX == "Female" & DATAadj$AGE == i& DATA$SCHOOL == 1]))
	rect(i+ww+l2,m51[2],i-ww+l2,m51[4],col=gray(.5),border=NA)
	segments(i+l2,m51[1],i+l2,m51[5],col=gray(.5),lwd=s.lwd)
	segments(i+ww+l2,m51[3],i-ww+l2,m51[3],col="white",lwd=s.lwd)
	
	# not in school
	u51 <- fivenum(100*DATAadj$prop_union2[DATAadj$SEX == "Female" & DATAadj$AGE == i & DATA$SCHOOL == 0])
	rect(i+ww+r1,u51[2],i-ww+r1,u51[4],col=gray(.3),border=NA)
	segments(i+r1,u51[1],i+r1,u51[5],col=gray(.3),lwd=s.lwd)
	segments(i+r1+ww,u51[3],i+r1-ww,u51[3],col="white",lwd=s.lwd)
	
	m51 <- fivenum(100*(1-DATAadj$childless2[DATAadj$SEX == "Female" & DATAadj$AGE == i& DATA$SCHOOL == 0]))
	rect(i+ww+r2,m51[2],i-ww+r2,m51[4],col=gray(.15),border=NA)
	segments(i+r2,m51[1],i+r2,m51[5],col=gray(.15),lwd=s.lwd)
	segments(i+ww+r2,m51[3],i-ww+r2,m51[3],col="white",lwd=s.lwd)
}
legend(14.4,100,fill=gray(c(.7,.5,.3,.15)),legend=c("student spouse","student mother","spouse not in school","mother not in school"),bty="n",border=NA)

segments(14.4,0,14.4,100,col=gray(.5),lwd=s.lwd) # y axis frame
segments(14.4,0,24.5,0,col=gray(.5),lwd=s.lwd) # x axis frame
# y axis ticks:
segments(14.4,(0:5)*20,14.33,(0:5)*20,col=gray(.5),lwd=s.lwd)
text(14.4,(0:5)*20,paste((0:5)*20,"%",sep=""),col=gray(.2),pos=2,cex=.8,family="serif")
segments(14.5:24.5,0,14.5:24.5,-.8,col=gray(.5),lwd=s.lwd)
text(15:24,0,15:24,pos=1,cex=.8,col=gray(.2),family="serif")
dev.off()

# for males, figure 3b
ww <- .09
l <- -.1
r <- .1
s.lwd <- 1.5
#emf(file="Figure4bBW.emf",width=7,height=7)
pdf(file="Figure4bBW.pdf",width=7,height=7)
plot(NULL,type="n",xlim=c(15,24),ylim=c(0,100),axes=FALSE,xlab="Age",ylab="",family="serif")
par(xpd=TRUE)
for (i in 15:24){
	# union in school
	u51 <- fivenum(100*DATAadj$prop_union2[DATAadj$SEX == "Male" & DATAadj$AGE == i & DATA$SCHOOL == 1])
	rect(i+ww+l,u51[2],i-ww+l,u51[4],col=gray(.7),border=NA)
	segments(i+l,u51[1],i+l,u51[5],col=gray(.7),lwd=s.lwd)
	segments(i+l+ww,u51[3],i+l-ww,u51[3],col="white",lwd=s.lwd)
	# union not in school
	m51 <- fivenum(100*DATAadj$prop_union2[DATAadj$SEX == "Male" & DATAadj$AGE == i& DATA$SCHOOL == 0])
	rect(i+ww+r,m51[2],i-ww+r,m51[4],col=gray(.3),border=NA)
	segments(i+r,m51[1],i+r,m51[5],col=gray(.3),lwd=s.lwd)
	segments(i+ww+r,m51[3],i-ww+r,m51[3],col="white",lwd=s.lwd)
}
legend(14.4,100,fill=gray(c(.7,.3)),legend=c("student spouse","spouse not in school"),bty="n",border=NA)
segments(14.4,0,14.4,100,col=gray(.5),lwd=s.lwd) # y axis frame
segments(14.4,0,24.5,0,col=gray(.5),lwd=s.lwd) # x axis frame
# y axis ticks:
segments(14.4,(0:5)*20,14.33,(0:5)*20,col=gray(.5),lwd=s.lwd)
text(14.4,(0:5)*20,paste((0:5)*20,"%",sep=""),col=gray(.2),pos=2,cex=.8,family="serif")
segments(14.5:24.5,0,14.5:24.5,-.8,col=gray(.5),lwd=s.lwd)
text(15:24,0,15:24,pos=1,cex=.8,col=gray(.2),family="serif")
dev.off()

###################
# on to Figure 5:
# figure 5 was a scatterplot of x=% mother of enrolled; y=%in school total population
# consider not including figure 4, since there is no age pattern in the relationship. Just mention, if necessary
# that there is no apparent relationship, at least when looked at the way we looked at it.

###################
# Figure 5:
# x: % mother of those enrolled
# y: % mother total population (why not % mother of those not enrolled?)

# NOTE: the data are called Figure 6!!!
# in the paper we may push this to Figure 4...
DATA <- read.table("/home/triffe/git/ViennaDiagnostics/ALBERTPAPERS/Figures/DATAfigBW/FIGURE6.tab",header=T,sep="\t",na.strings = ".")
DATA$country <- as.character(DATA$country)
DATA$SEX <- as.character(DATA$SEX)
DATA <- DATA[!DATA$country %in% c("Italy","Palestine","Slovenia",""),]
DATA <- DATA[!DATA$country %in% c("Switzerland","France","Spain","Austria","Romania","Belarus","United States"),]
DATA <- DATA[DATA$AGE > 14,]
cntries <- unique(DATA$country)

#plot(NULL,type="n",xlim=c(15,24),ylim=c(0,100))
#for (i in 1:length(cntries)){
#	x <- DATA$AGE[DATA$country==cntries[i]]
#	y <- 100*(1-DATA$prop_childless_att[DATA$country==cntries[i]])
#	lines(x,y,col="#80808050")
#}
#
#plot(NULL,type="n",xlim=c(15,24),ylim=c(0,100))
#for (i in 1:length(cntries)){
#	x <- DATA$AGE[DATA$country==cntries[i]]
#	y <- 100*(1-DATA$prop_childless[DATA$country==cntries[i]])
#	lines(x,y,col="#80808050")
#}

pdf(height=8,width=8,"smooth5schoolmother.pdf")
par(mfrow=c(2,2))
for (i in 1:length(cntries)){
	x <- DATA$AGE[DATA$country == cntries[i] & DATA$SEX == "Female"]
	y <- 100*(1-DATA$prop_childless_att[DATA$country == cntries[i] & DATA$SEX == "Female"])
	x <- x[!is.na(y)]
	y <- y[!is.na(y)]
	if (length(x) > 3){
		if (sum(sign(diff(y)) == -1,na.rm=TRUE) > 0 & min(diff(y),na.rm=TRUE) < -.5){
			plot(x,y,xlim=c(15,24),ylim=c(0,100),xlab="Age",ylab="% mother enrolled",main=cntries[i],col="blue",pch=19)
			x <- x[!is.na(y)]
			y <- smooth.spline(x,y[!is.na(y)],spar=.4)$y
			lines(x,y,lwd=2,col="#0000FF70")
		}
	}
}
dev.off()

# this was already done for the figure 1s
pdf(height=8,width=8,"smooth5mother.pdf")
par(mfrow=c(2,2))
for (i in 1:length(cntries)){
	x <- DATA$AGE[DATA$country == cntries[i] & DATA$SEX == "Female"]
	y <- 100*(1-DATA$prop_childless[DATA$country == cntries[i] & DATA$SEX == "Female"])
	x <- x[!is.na(y)]
	y <- y[!is.na(y)]
	if (length(x) > 3){
		if (sum(sign(diff(y)) == -1,na.rm=TRUE) > 0 & min(diff(y),na.rm=TRUE) < -.5){
			plot(x,y,xlim=c(15,24),ylim=c(0,100),xlab="Age",ylab="% mother",main=cntries[i],col="blue",pch=19)
			x <- x[!is.na(y)]
			y <- smooth.spline(x,y[!is.na(y)],spar=.4)$y
			lines(x,y,lwd=2,col="#0000FF70")
		}
	}
}
dev.off()

# now make DATAadj for plotting
DATAadj <- DATA
femadj <- matrix(0,nrow=length(cntries),ncol=2)
colnames(femadj) <- c("motherinschool","mother")
rownames(femadj) <- cntries
for (i in 1:length(cntries)){
	# union in school
	ind <- DATA$country == cntries[i]
	x <- DATA$AGE[ind]
	y <- 100*(1-DATA$prop_childless_att[ind])
	if (sum(sign(diff(y)) == -1,na.rm=TRUE) > 0 & min(diff(y),na.rm=TRUE) < -.5){
		x2 <- x[!is.na(y)]
		y <- smooth.spline(x2,y[!is.na(y)],spar=.4)$y
		DATAadj$prop_childless_att[ind & DATA$AGE %in% x2] <- 1-(y/100)
		femadj[i,1] <- 1
	}
	y <- 100*(1-DATA$prop_childless[ind])
	if (sum(sign(diff(y)) == -1,na.rm=TRUE) > 0 & min(diff(y),na.rm=TRUE) < -.5){
		x2 <- x[!is.na(y)]
		y <- smooth.spline(x2,y[!is.na(y)],spar=.4)$y
		DATAadj$prop_childless[ind & DATA$AGE %in% x2] <- 1-(y/100)
		femadj[i,2] <- 1
	}
}

# mark countries used:
cntriesAll[rownames(cntriesAll) %in% rownames(femadj),8] <- "X"
### now make figure:
# switch axes from original color plot: x axis for % mother in general pop
# y axis for % mother of students
#png(file="Figure5BW.png",height=7,width=7,units="in",res=300,antialias="gray")
pdf(file="Figure5BW.pdf",height=7,width=7)
par(xaxs="i",yaxs="i")
plot(NULL,type="n",xlim=c(0,100),ylim=c(0,100),axes=FALSE,xlab="",ylab="",asp=1,col=paste(gray(.5),50,sep=""),family="serif")
for (i in c(16,18,20,22,24)){
	y <- 100 *  (1-DATAadj$prop_childless[DATAadj$AGE == i])
	x <- 100 * (1-DATAadj$prop_childless_att[DATAadj$AGE == i])
	minx <- min(x,na.rm=T) ; maxx <- max(x,na.rm=T)
	coefs <- Deming(x,y)[1:2]
	xnew <- seq(min(x,na.rm=T),max(x,na.rm=T),length.out=200)
	CIs <- DemingCI(x,y,xnew)
	polygon(c(xnew,rev(xnew)),c(CIs[,1],rev(CIs[,2])),col=paste(gray(.8),50,sep=""),border=FALSE)
	segments(xnew[1],coefs[1]+xnew[1]*coefs[2],xnew[200],coefs[1]+xnew[200]*coefs[2],col=gray(.4))
	text(xnew[1],coefs[1]+xnew[1]*coefs[2],i,cex=1.1,col=gray(.2),xpd=TRUE,pos=2,family="serif")
}
segments(-5,(0:5)*20,-6,(0:5)*20,col=gray(.2),xpd=TRUE)
text(-5,(0:5)*20,paste((0:5)*20,"%",sep=""),pos=2,cex=.8,col=gray(.2),xpd=TRUE,family="serif")
segments((0:5)*20,-5,(0:5)*20,-6,col=gray(.2),xpd=TRUE)
text((0:5)*20,-5,paste((0:5)*20,"%",sep=""),pos=1,cex=.8,col=gray(.2),xpd=TRUE,family="serif")
text(50,-12,"% Mother of female students",pos=1,col=gray(.2),xpd=TRUE,family="serif")
text(-12,50,"% Mother of\nall females",col=gray(.2),xpd=TRUE,family="serif")
slopes <- c()
for (i in 15:24){
	y <- 100 * DATAadj$prop_childless[DATAadj$AGE == i]
	x	 <- 100 * DATAadj$prop_childless_att[DATAadj$AGE == i]
	coefs <- Deming(x,y)[1:2]
	slopes[i-14] <- coefs[2]
}
dev.off()
slopesFigs[,4] <- slopes
###############
# Figure 6 (using data called Figure 7)

DATA <- read.table("/home/triffe/git/ViennaDiagnostics/ALBERTPAPERS/Figures/DATAfigBW/FIGURE7.tab",header=T,sep="\t",na.strings = ".")
DATA$country <- as.character(DATA$country)
DATA$SEX <- as.character(DATA$SEX)
DATA <- DATA[!DATA$country %in% c("Italy","Palestine","Slovenia",""),]
DATA <- DATA[!DATA$country %in% c("Switzerland","France","Spain","Austria","Romania","Belarus","United States"),]
DATA <- DATA[DATA$AGE > 14,]
cntries <- unique(DATA$country)

#plot(NULL,type="n",xlim=c(15,24),ylim=c(0,100))
#for (i in 1:length(cntries)){
#	x <- DATA$AGE[DATA$country==cntries[i] & DATA$SEX == "Female"]
#	y <- 100*DATA$prop_union_att[DATA$country==cntries[i] & DATA$SEX == "Female"]
#	lines(x,y,col="#80808050")
#}
#
## head(DATA)
#plot(NULL,type="n",xlim=c(15,24),ylim=c(0,100))
#for (i in 1:length(cntries)){
#	x <- DATA$AGE[DATA$country==cntries[i] & DATA$SEX == "Female"]
#	y <- 100*DATA$prop_union[DATA$country==cntries[i] & DATA$SEX == "Female"]
#	lines(x,y,col="#80808050")
#}

pdf(height=8,width=8,"smooth6schoolunion.pdf")
par(mfrow=c(2,2))
for (i in 1:length(cntries)){
	x <- DATA$AGE[DATA$country == cntries[i] & DATA$SEX == "Female"]
	y <- 100*DATA$prop_union_att[DATA$country == cntries[i] & DATA$SEX == "Female"]
	x <- x[!is.na(y)]
	y <- y[!is.na(y)]
	if (length(x) > 3){
		if (sum(sign(diff(y)) == -1,na.rm=TRUE) > 0 & min(diff(y),na.rm=TRUE) < -.5){
			plot(x,y,xlim=c(15,24),ylim=c(0,100),xlab="Age",ylab="% spouse of enrolled",main=cntries[i],col="blue",pch=19)
			x <- x[!is.na(y)]
			y <- smooth.spline(x,y[!is.na(y)],spar=.4)$y
			lines(x,y,lwd=2,col="#0000FF70")
		}
	}
}
dev.off()

# this was already done for the figure 1s
pdf(height=8,width=8,"smooth6union.pdf")
par(mfrow=c(2,2))
for (i in 1:length(cntries)){
	x <- DATA$AGE[DATA$country == cntries[i] & DATA$SEX == "Female"]
	y <- 100*DATA$prop_union[DATA$country == cntries[i] & DATA$SEX == "Female"]
	x <- x[!is.na(y)]
	y <- y[!is.na(y)]
	if (length(x) > 3){
		if (sum(sign(diff(y)) == -1,na.rm=TRUE) > 0 & min(diff(y),na.rm=TRUE) < -.5){
			plot(x,y,xlim=c(15,24),ylim=c(0,100),xlab="Age",ylab="% union",main=cntries[i],col="blue",pch=19)
			x <- x[!is.na(y)]
			y <- smooth.spline(x,y[!is.na(y)],spar=.4)$y
			lines(x,y,lwd=2,col="#0000FF70")
		}
	}
}
dev.off()

# now make DATAadj for plotting
DATAadj <- DATA
femadj <- matrix(0,nrow=length(cntries),ncol=2)
colnames(femadj) <- c("spouseinschool","spouse")
rownames(femadj) <- cntries
for (i in 1:length(cntries)){
	# union in school
	ind <- DATA$country == cntries[i] & DATA$SEX == "Female"
	x <- DATA$AGE[ind]
	y <- 100*DATA$prop_union_att[ind]
	if (sum(sign(diff(y)) == -1,na.rm=TRUE) > 0 & min(diff(y),na.rm=TRUE) < -.5){
		x2 <- x[!is.na(y)]
		y <- smooth.spline(x2,y[!is.na(y)],spar=.4)$y
		DATAadj$prop_union_att[ind & DATA$AGE %in% x2] <- y/100
		femadj[i,1] <- 1
	}
	y <- 100*DATA$prop_union[ind]
	if (sum(sign(diff(y)) == -1,na.rm=TRUE) > 0 & min(diff(y),na.rm=TRUE) < -.5){
		x2 <- x[!is.na(y)]
		y <- smooth.spline(x2,y[!is.na(y)],spar=.4)$y
		DATAadj$prop_union[ind & DATA$AGE %in% x2] <- y/100
		femadj[i,2] <- 1
	}
}

# mark countries used:

cntriesAll[rownames(cntriesAll) %in% rownames(femadj),9] <- "X"
### now make figure:
# switch axes from original color plot: x axis for % mother in general pop
# y axis for % mother of students
#png(file="Figure6BW.png",height=7,width=7,units="in",res=300,antialias="gray")
pdf(file="Figure6BW.pdf",height=7,width=7)
plot(NULL,type="n",xlim=c(0,100),ylim=c(0,100),axes=FALSE,xlab="",ylab="",asp=1,col=paste(gray(.5),50,sep=""),family="serif")
for (i in c(16,18,20,22,24)){
	x <- 100 *  DATAadj$prop_union[DATAadj$AGE == i & DATA$SEX == "Female"]
	y <- 100 * DATAadj$prop_union_att[DATAadj$AGE == i & DATA$SEX == "Female"]
	minx <- min(x,na.rm=T) ; maxx <- max(x,na.rm=T)
	coefs <- Deming(x,y)[1:2]
	xnew <- seq(min(x,na.rm=T),max(x,na.rm=T),length.out=200)
	CIs <- DemingCI(x,y,xnew)
	polygon(c(xnew,rev(xnew)),c(CIs[,1],rev(CIs[,2])),col=paste(gray(.8),50,sep=""),border=FALSE)
	segments(xnew[1],coefs[1]+xnew[1]*coefs[2],xnew[200],coefs[1]+xnew[200]*coefs[2],col=gray(.4))
	text(xnew[200],coefs[1]+xnew[200]*coefs[2],i,cex=1.1,col=gray(.2),xpd=TRUE,pos=4,family="serif")
}
segments(-5,(0:5)*20,-6,(0:5)*20,col=gray(.2),xpd=TRUE)
text(-5,(0:5)*20,paste((0:5)*20,"%",sep=""),pos=2,cex=.8,col=gray(.2),xpd=TRUE,family="serif")
segments((0:5)*20,-5,(0:5)*20,-6,col=gray(.2),xpd=TRUE)
text((0:5)*20,-5,paste((0:5)*20,"%",sep=""),pos=1,cex=.8,col=gray(.2),xpd=TRUE,family="serif")
text(-10,55,"% Spouse of\nfemale students",pos=1,col=gray(.2),xpd=TRUE,family="serif")
text(50,-12,"% Spouse of all females",col=gray(.2),xpd=TRUE,family="serif")
slopes <- c()
for (i in 15:24){
	y <- 100 * DATAadj$prop_union[DATAadj$AGE == i & DATA$SEX == "Female"]
	x	 <- 100 * DATAadj$prop_union_att[DATAadj$AGE == i & DATA$SEX == "Female"]
	coefs <- Deming(x,y)[1:2]
	slopes[i-14] <- coefs[2]
}
dev.off()
slopesFigs[,5] <- slopes
####################################
# finally, looking at former figure 7, which disaggregates by 2 edu levels
# % mother total pop, age 20
# vs % mother of enrolled, age 20
# now called figure 6, using data called Figure 8

DATA <- read.table("/home/triffe/git/ViennaDiagnostics/ALBERTPAPERS/Figures/DATAfigBW/FIGURE8.tab",header=T,sep="\t",na.strings = ".")
DATA$country <- as.character(DATA$country)
DATA <- DATA[!DATA$country %in% c("Italy","Palestine","Slovenia","","Austria","United States","Belarus","Kyrgyz Republic","Kazakhstan","Madagascar",
				"Malaysia","Niger","Zimbabwe","Jordan","Ghana","Cote Ivoire","Belarus","Armenia"),]
DATA <- DATA[!DATA$country %in% c("Switzerland","France","Spain","Austria","Romania","Belarus","United States"),]
cntries <- unique(DATA$country)
DATA <- DATA[!DATA$AGE<15,]
#unique(DATA$country)
#plot(NULL,type="n",xlim=c(15,24),ylim=c(0,100))
#for (i in 1:length(cntries)){
#	x <- DATA$AGE[DATA$country==cntries[i]]
#	y <- 100*DATA$prop_child[DATA$country==cntries[i]]
#	lines(x,y,col="#80808050")
#}
#
## head(DATA)
#plot(NULL,type="n",xlim=c(15,24),ylim=c(0,100))
#for (i in 1:length(cntries)){
#	x <- DATA$AGE[DATA$country==cntries[i]]
#	y <- 100*DATA$withchild1[DATA$country==cntries[i]]
#	lines(x,y,col="#80808050")
#}
#plot(NULL,type="n",xlim=c(15,24),ylim=c(0,100))
#for (i in 1:length(cntries)){
#	x <- DATA$AGE[DATA$country==cntries[i]]
#	y <- 100*DATA$withchild2[DATA$country==cntries[i]]
#	lines(x,y,col="#80808050")
#}
# not as bad as other measures
pdf(height=8,width=8,"smooth7child.pdf")
par(mfrow=c(2,2))
for (i in 1:length(cntries)){
	ind <- DATA$country == cntries[i]
	x <- DATA$AGE[ind]
	y <- 100*DATA$prop_child[ind]
	x <- x[!is.na(y)]
	y <- y[!is.na(y)]
	if (length(x) > 3){
		if (sum(sign(diff(y)) == -1,na.rm=TRUE) > 0 & min(diff(y),na.rm=TRUE) < -.5){
			plot(x,y,xlim=c(15,24),ylim=c(0,100),xlab="Age",ylab="% females with child",main=cntries[i],col="blue",pch=19)
			x <- x[!is.na(y)]
			y <- smooth.spline(x,y[!is.na(y)],spar=.4)$y
			lines(x,y,lwd=2,col="#0000FF70")
			Sys.sleep(1)
		}
	}
}
dev.off()


pdf(height=8,width=8,file="smooth7primarychild.pdf")
par(mfrow=c(2,2))
for (i in 1:length(cntries)){
	ind <- DATA$country == cntries[i]
	x <- DATA$AGE[ind]
	y <- 100*DATA$withchild1[ind]
	x <- x[!is.na(y)]
	y <- y[!is.na(y)]
	if (length(x) > 3){
		if (sum(sign(diff(y)) == -1,na.rm=TRUE) > 0 & min(diff(y),na.rm=TRUE) < -.5){
			plot(x,y,xlim=c(15,24),ylim=c(0,100),xlab="Age",ylab="% females in primary school with child",main=cntries[i],col="blue",pch=19)
			x <- x[!is.na(y)]
			y <- smooth.spline(x,y[!is.na(y)],spar=.4)$y
			y[y<0] <- 0
			lines(x,y,lwd=2,col="#0000FF70")
		}
	}
}
dev.off()

pdf(height=8,width=8,file="smooth7secondarychild.pdf")
par(mfrow=c(2,2))
for (i in 1:length(cntries)){
	ind <- DATA$country == cntries[i]
	x <- DATA$AGE[ind]
	y <- 100*DATA$withchild2[ind]
	x <- x[!is.na(y)]
	y <- y[!is.na(y)]
	if (length(x) > 3){
		if (sum(sign(diff(y)) == -1,na.rm=TRUE) > 0 & min(diff(y),na.rm=TRUE) < -.5){
			plot(x,y,xlim=c(15,24),ylim=c(0,100),xlab="Age",ylab="% females in secondary school+ with child",main=cntries[i],col="blue",pch=19)
			x <- x[!is.na(y)]
			y <- smooth.spline(x,y[!is.na(y)],spar=.4)$y
			y[y<0] <- 0
			lines(x,y,lwd=2,col="#0000FF70")
		}
	}
}
dev.off()

## DATAadj:
DATAadj <- DATA
femadj <- matrix(0,nrow=length(cntries),ncol=3)
colnames(femadj) <- c("prop_child","primarychild","secondarychild")
rownames(femadj) <- cntries
for (i in 1:length(cntries)){
	# union in school
	ind <- DATA$country == cntries[i] 
	x <- DATA$AGE[ind]
	y <- 100*DATA$prop_child[ind]
	
	if (sum(sign(diff(y)) == -1,na.rm=TRUE) > 0 & min(diff(y),na.rm=TRUE) < -.5){
		x2 <- x[!is.na(y)]
		y <- smooth.spline(x2,y[!is.na(y)],spar=.4)$y
		y[y < 0] <- 0
		DATAadj$prop_child[ind & DATA$AGE %in% x2] <- y/100
		femadj[i,1] <- 1
	}
	y <- 100*DATA$withchild1[ind]
	if (sum(sign(diff(y)) == -1,na.rm=TRUE) > 0 & min(diff(y),na.rm=TRUE) < -.5){
		x2 <- x[!is.na(y)]
		y <- smooth.spline(x2,y[!is.na(y)],spar=.4)$y
		y[y < 0] <- 0
		DATAadj$withchild1[ind & DATA$AGE %in% x2] <- y/100
		femadj[i,2] <- 1
	}
	y <- 100*DATA$withchild2[ind]
	if (sum(sign(diff(y)) == -1,na.rm=TRUE) > 0 & min(diff(y),na.rm=TRUE) < -.5){
		x2 <- x[!is.na(y)]
		y <- smooth.spline(x2,y[!is.na(y)],spar=.4)$y
		y[y < 0] <- 0
		DATAadj$withchild2[ind & DATA$AGE %in% x2] <- y/100
		femadj[i,3] <- 1
	}
}
DATAadj <- DATAadj[DATAadj$AGE==20,]
# figure 7
cntriesAll[rownames(cntriesAll) %in% rownames(femadj),10] <- "X"
#png(file="Figure7BW.png",height=7,width=7,units="in",res=300,antialias="gray")
pdf(file="Figure7BW.pdf",height=7,width=7)
par(xaxs="i",yaxs="i")
plot(NULL,type="n",xlim=c(0,100),ylim=c(0,100),axes=FALSE,xlab="",ylab="",asp=1,col=paste(gray(.5),50,sep=""),family="serif")
x 	<- 100 * DATAadj$prop_child
y1 	<- 100 * DATAadj$withchild1
y2 	<- 100 * DATAadj$withchild2
minx <- min(x,na.rm=T) ; maxx <- max(x,na.rm=T)
# for primary school:
coefs1 <- Deming(x,y1)[1:2]
xnew <- seq(min(x,na.rm=T),max(x,na.rm=T),length.out=200)
CIs <- DemingCI(x,y1,xnew)
polygon(c(xnew,rev(xnew)),c(CIs[,1],rev(CIs[,2])),col=paste(gray(.8),50,sep=""),border=FALSE)
segments(xnew[1],coefs1[1]+xnew[1]*coefs1[2],xnew[200],coefs1[1]+xnew[200]*coefs1[2],col=gray(.4))
text(xnew[200],coefs1[1]+xnew[200]*coefs1[2],paste("primary (   )\nslope =",round(coefs1[2],3)),cex=1.1,col=gray(.2),xpd=TRUE,pos=4,family="serif")
points(93.2,74.4,pch=19,col=gray(.4)) # circle
# for secondary school:
coefs2 <- Deming(x,y2)[1:2]
xnew <- seq(min(x,na.rm=T),max(x,na.rm=T),length.out=200)
CIs <- DemingCI(x,y2,xnew)
polygon(c(xnew,rev(xnew)),c(CIs[,1],rev(CIs[,2])),col=paste(gray(.8),50,sep=""),border=FALSE)
segments(xnew[1],coefs2[1]+xnew[1]*coefs2[2],xnew[200],coefs2[1]+xnew[200]*coefs2[2],col=gray(.4))
text(xnew[200],coefs2[1]+xnew[200]*coefs2[2],paste("secondary+  (   )\nslope =",round(coefs2[2],3)),cex=1.1,col=gray(.2),xpd=TRUE,pos=4,family="serif")
points(99.2,31,pch=2,col=gray(.5)) # triangle
segments(-5,(0:5)*20,-6,(0:5)*20,col=gray(.2),xpd=TRUE)
text(-5,(0:5)*20,paste((0:5)*20,"%",sep=""),pos=2,cex=.8,col=gray(.2),xpd=TRUE,family="serif")
segments((0:5)*20,-5,(0:5)*20,-6,col=gray(.2),xpd=TRUE)

# plot points on top of confidence regions:
points(x,y1,pch=19,col=gray(.4),xpd=TRUE)
points(x,y2,pch=2,col=gray(.5),xpd=TRUE)

text((0:5)*20,-5,paste((0:5)*20,"%",sep=""),pos=1,cex=.8,col=gray(.2),xpd=TRUE,family="serif")
text(-10,55,"% Mother of\nfemale students",pos=1,col=gray(.2),xpd=TRUE,family="serif")
text(50,-15,"% Mother of all females",col=gray(.2),xpd=TRUE,family="serif")
dev.off()

slopesFigs
write.table(slopesFigs,file = "Slopes.csv", sep = ",", row.names = rownames(slopesFigs), col.names = colnames(slopesFigs))
cntriesAll
write.table(cntriesAll,file = "CountriesFigs.csv", sep = ",", row.names = rownames(cntriesAll), col.names = colnames(cntriesAll))