# TODO: Add comment
# 
# Author: Tim Riffe
###############################################################################
setwd("C:/Users/triffe/git/ViennaPaper/ALBERTPAPERS/Figures/figsW")
setwd("/home/triffe/git/ViennaDiagnostics/ALBERTPAPERS/Figures/figsW")
#install.packages("devEMF")
library(devEMF)
#install.packages("Cairo")
library(Cairo)

#

# Figure 1a
DATA <- read.table("http://www.ced.uab.es/worldfam/figures/figure1_2.tab",header=T,sep="\t",na.strings = ".")
DATA$country <- as.character(DATA$country)
DATA$sex <- as.character(DATA$sex)


emf(file="Figure1a.emf",width=7,height=7)
omar <- par("mar")
par("xaxs"="i","yaxs"="i",mar=c(10,4,1,2))
plot(NULL,type="n",xlim=c(11.5,24.5),ylim=c(0,100),ylab="% population",xlab="Age",cex.lab=1.5)
extr <- par("usr")
rect(extr[1],extr[3],extr[2],extr[4],col="#EBEBEB")
abline(v=seq(12,24,by=2),col="white")
abline(h=seq(20,80,by=20),col="white")

for (i in 12:24){
	# in school
	y <- 100*DATA[DATA$age==i & DATA$sex=="Female" & DATA$round==2000,"prop_school"]
	x <- rep(i-.3,length(y))
	# IQR box:
	FN <- quantile(y,probs=c(.25,.5,.75),na.rm=TRUE)
	rect(i-.4,FN[1],i-.2,FN[3],col="#EEC900") #IQR box
	segments(i-.4,FN[2],i-.2,FN[2]) #median line
	maxcut <- ifelse(max(y,na.rm=T) > FN[3]+1.5*abs(diff(range(FN))),FN[3]+1.5*abs(diff(range(FN))),max(y,na.rm=T))
	mincut <- ifelse(min(y,na.rm=T) < FN[1]-1.5*abs(diff(range(FN))),FN[1]-1.5*abs(diff(range(FN))),min(y,na.rm=T))
	segments(i-.3,FN[1],i-.3,mincut,lty=2,col="#EEC900") # lower whisker
	segments(i-.3,FN[3],i-.3,maxcut,lty=2,col="#EEC900") # upper whisker
	points(c(i-.3,i-.3),c(mincut,maxcut),pch=19,col="#EEC900",cex=.5)
	if (i > 14){
		# in union
		y <- 100*DATA[DATA$age==i & DATA$sex=="Female" & DATA$round==2000,"prop_union"]
		x <- rep(i,length(y))
		#points(jitter(x,amount=.05),y,col="#7D26CD30",pch=19)
		FN <- quantile(y,probs=c(.25,.5,.75),na.rm=TRUE)
		rect(i-.1,FN[1],i+.1,FN[3],col="#7D26CD")
		segments(i-.1,FN[2],i+.1,FN[2])
		maxcut <- ifelse(max(y,na.rm=T) > FN[3]+1.5*abs(diff(range(FN))),FN[3]+1.5*abs(diff(range(FN))),max(y,na.rm=T))
		mincut <- ifelse(min(y,na.rm=T) < FN[1]-1.5*abs(diff(range(FN))),FN[1]-1.5*abs(diff(range(FN))),min(y,na.rm=T))
		segments(i,FN[1],i,mincut,lty=2,col="#7D26CD")
		segments(i,FN[3],i,maxcut,lty=2,col="#7D26CD")
		points(c(i,i),c(mincut,maxcut),pch=19,col="#7D26CD",cex=.5)
		# mother
		y <- 100*(1-DATA[DATA$age==i & DATA$sex=="Female" & DATA$round==2000,"prop_childless"])
		x <- rep(i+.3,length(y))
		FN <- quantile(y,probs=c(.25,.5,.75),na.rm=TRUE)
		rect(i+.2,FN[1],i+.4,FN[3],col="#FF69B4")
		segments(i+.2,FN[2],i+.4,FN[2]) 
		maxcut <- ifelse(max(y,na.rm=T) > FN[3]+1.5*abs(diff(range(FN))),FN[3]+1.5*abs(diff(range(FN))),max(y,na.rm=T))
		mincut <- ifelse(min(y,na.rm=T) < FN[1]-1.5*abs(diff(range(FN))),FN[1]-1.5*abs(diff(range(FN))),min(y,na.rm=T))
		segments(i+.3,FN[1],i+.3,mincut,lty=2,col="#FF69B4")
		segments(i+.3,FN[3],i+.3,maxcut,lty=2,col="#FF69B4")
		points(c(i+.3,i+.3),c(mincut,maxcut),pch=19,col="#FF69B4",cex=.5)
	}
}
legend(10.5,-13.5,fill=c("#EEC900","#7D26CD","#FF69B4"),legend=c("% enrolled","% in union","% mother"),xpd=T,cex=1.5)
par(mar=omar)
dev.off()

##################################

# Figure 1b (males)
DATA <- read.table("http://www.ced.uab.es/worldfam/figures/figure1_2.tab",header=T,sep="\t",na.strings = ".")
DATA$country <- as.character(DATA$country)
DATA$sex <- as.character(DATA$sex)

emf(file="Figure1b.emf",width=7,height=7)
omar <- par("mar")
par("xaxs"="i","yaxs"="i",mar=c(10,4,1,2))
plot(NULL,type="n",xlim=c(11.5,24.5),ylim=c(0,100),ylab="% population",xlab="Age",cex.lab=1.5)
extr <- par("usr")
rect(extr[1],extr[3],extr[2],extr[4],col="#EBEBEB")
abline(v=seq(12,24,by=2),col="white")
abline(h=seq(20,80,by=20),col="white")
for (i in 12:24){
	# in school
	y <- 100*DATA[DATA$age==i & DATA$sex=="Male" & DATA$round==2000,"prop_school"]
	# IQR box:
	FN <- quantile(y,probs=c(.25,.5,.75),na.rm=TRUE)
	rect(i-.3,FN[1],i-.1,FN[3],col="#EEC900") #IQR box
	segments(i-.3,FN[2],i-.1,FN[2]) #median line
	maxcut <- ifelse(max(y,na.rm=T) > FN[3]+1.5*abs(diff(range(FN))),FN[3]+1.5*abs(diff(range(FN))),max(y,na.rm=T))
	mincut <- ifelse(min(y,na.rm=T) < FN[1]-1.5*abs(diff(range(FN))),FN[1]-1.5*abs(diff(range(FN))),min(y,na.rm=T))
	segments(i-.2,FN[1],i-.2,mincut,lty=2,col="#EEC900") # lower whisker
	segments(i-.2,FN[3],i-.2,maxcut,lty=2,col="#EEC900") # upper whisker
	points(c(i-.2,i-.2),c(mincut,maxcut),pch=19,col="#EEC900",cex=.5)
	
	if (i >14){
		# in union
		y <- 100*DATA[DATA$age==i & DATA$sex=="Male" & DATA$round==2000,"prop_union"]
		FN <- quantile(y,probs=c(.25,.5,.75),na.rm=TRUE)
		rect(i+.1,FN[1],i+.3,FN[3],col="#7D26CD")
		segments(i+.1,FN[2],i+.3,FN[2])
		maxcut <- ifelse(max(y,na.rm=T) > FN[3]+1.5*abs(diff(range(FN))),FN[3]+1.5*abs(diff(range(FN))),max(y,na.rm=T))
		mincut <- ifelse(min(y,na.rm=T) < FN[1]-1.5*abs(diff(range(FN))),FN[1]-1.5*abs(diff(range(FN))),min(y,na.rm=T))
		segments(i+.2,FN[1],i+.2,mincut,lty=2,col="#7D26CD")
		segments(i+.2,FN[3],i+.2,maxcut,lty=2,col="#7D26CD")
		points(c(i+.2,i+.2),c(mincut,maxcut),pch=19,col="#7D26CD",cex=.5)
	}
}
legend(10.5,-13,fill=c("#EEC900","#7D26CD"),legend=c("% enrolled","% in union"),xpd=T,cex=1.5)
par(mar=omar)
dev.off()
#######################
# Figure 2a

DATA <- read.table("http://www.ced.uab.es/worldfam/figures/figure1_2.tab",header=T,sep="\t",na.strings = ".")
DATA$country <- as.character(DATA$country)
DATA$sex <- as.character(DATA$sex)

CairoPNG("Figure2a.png",width=1000,height=1000,pointsize=25)

# figure 2adots

png("Figure2adots.png",width=1000,height=1000,pointsize=25)
ages <- 15:24
library(grDevices)
colsR <- colorRampPalette(c("green","yellow","magenta","blue"))
cols <- colsR(length(ages))
sdev <- spsprint <- sps <- cty <- c()
par("xaxs"="i");par("yaxs"="i");par(mar=c(4,4,1,2))
plot(NULL,type="n",xlim=c(0,100),ylim=c(0,100),ylab="% enrolled",xlab="% in union",cex.lab=1.5,axes=F,asp=1)
extr <- par("usr")
rect(0,0,100,100,col="#EBEBEB")
abline(v=seq(20,80,by=20),col="white")
abline(h=seq(20,80,by=20),col="white")
axis(1,cex=2,pos=0);axis(2,cex=2,pos=0)
for (i in 1:length(ages)){
	x <- 100*DATA[DATA$age==ages[i] & DATA$sex=="Female" & DATA$round==2000,"prop_union"]
	y <- 100*DATA[DATA$age==ages[i] & DATA$sex=="Female" & DATA$round==2000,"prop_school"]
	ctyi <- DATA[DATA$age==ages[i] & DATA$sex=="Female" & DATA$round==2000,"country"]
	yri <- DATA[DATA$age==ages[i] & DATA$sex=="Female" & DATA$round==2000,"year"]
	ctyi <- paste(ctyi,yri,sep="")
	nax <- which(is.na(x)) ; nay <-  which(is.na(y))
	nas <- unique(c(nax,nay))
	if (length(nas)>0){	ctyi <- ctyi[-nas]}
	cty <- c(cty,ctyi)
	minx <- min(x,na.rm=T) ; maxx <- max(x,na.rm=T)
	LM <- lm(y~x)
	xref <- data.frame(x=seq(from=minx, to=maxx, length.out=25))
	#segments(minx,LM$coef[1]+LM$coef[2]*minx,maxx,LM$coef[1]+LM$coef[2]*maxx,col=cols[i],lwd=2)
	points(x,y,col=paste(cols[i],45,sep=""),pch=19)
	pv <- summary(LM)$coefficients[2,4] # p val
	pv <- ifelse(pv<.0001,"***",ifelse(pv<.001,"**",ifelse(pv<.01,"*",ifelse(pv<.05,"'",""))))
	sps[i] <- summary(LM)$coefficients[2,1]
	spsprint[i] <- paste(round(sps[i],3),pv)
	sdev[i] <- summary(LM)$coefficients[2,2]
}
#rect(57,60,100,100,col="white")
#legend("topright",col=cols,lwd=2,legend=paste(ages,", slope = ",spsprint,sep=""),box.col="transparent")
rect(0,0,100,100)
dev.off()




png("Figure2aline.png",width=1000,height=1000,pointsize=25)
ages <- 15:24
library(grDevices)
colsR <- colorRampPalette(c("green","yellow","magenta","blue"))
cols <- colsR(length(ages))
sdev <- spsprint <- sps <- cty <- c()
par("xaxs"="i");par("yaxs"="i");par(mar=c(4,4,1,2))
plot(NULL,type="n",xlim=c(0,100),ylim=c(0,100),ylab="% enrolled",xlab="% in union",cex.lab=1.5,axes=F,asp=1)
extr <- par("usr")
rect(0,0,100,100,col="#EBEBEB")
abline(v=seq(20,80,by=20),col="white")
abline(h=seq(20,80,by=20),col="white")
axis(1,cex=2,pos=0);axis(2,cex=2,pos=0)
for (i in 1:length(ages)){
	x <- 100*DATA[DATA$age==ages[i] & DATA$sex=="Female" & DATA$round==2000,"prop_union"]
	y <- 100*DATA[DATA$age==ages[i] & DATA$sex=="Female" & DATA$round==2000,"prop_school"]
	ctyi <- DATA[DATA$age==ages[i] & DATA$sex=="Female" & DATA$round==2000,"country"]
	yri <- DATA[DATA$age==ages[i] & DATA$sex=="Female" & DATA$round==2000,"year"]
	ctyi <- paste(ctyi,yri,sep="")
	nax <- which(is.na(x)) ; nay <-  which(is.na(y))
	nas <- unique(c(nax,nay))
	if (length(nas)>0){	ctyi <- ctyi[-nas]}
	cty <- c(cty,ctyi)
	minx <- min(x,na.rm=T) ; maxx <- max(x,na.rm=T)
	LM <- lm(y~x)
	xref <- data.frame(x=seq(from=minx, to=maxx, length.out=25))
	segments(minx,LM$coef[1]+LM$coef[2]*minx,maxx,LM$coef[1]+LM$coef[2]*maxx,col=cols[i],lwd=2)
	points(x,y,col=paste(cols[i],45,sep=""),pch=19)
	pv <- summary(LM)$coefficients[2,4] # p val
	pv <- ifelse(pv<.0001,"***",ifelse(pv<.001,"**",ifelse(pv<.01,"*",ifelse(pv<.05,"'",""))))
	sps[i] <- summary(LM)$coefficients[2,1]
	spsprint[i] <- paste(round(sps[i],3),pv)
	sdev[i] <- summary(LM)$coefficients[2,2]
}
rect(57,60,100,100,col="white")
legend("topright",col=cols,lwd=2,legend=paste(ages,", slope = ",spsprint,sep=""),box.col="transparent")
rect(0,0,100,100)
dev.off()



#############################################################
# Figure 2c

# need to remove thailand, iran, nepal, palestine, sudan, re: email from Jeroen, 28 Nov, 2011:
#      decision based on low response rates: probable bias leads to high leverage of particular points in plot
#      that then overly determine the slope. 
#      I argued for weighting based on a combo of response rate and proportion significance
DATA <- read.table("http://www.ced.uab.es/worldfam/figures/figure1_2.tab",header=T,sep="\t",na.strings = ".")
DATA$country <- as.character(DATA$country)
DATA$sex <- as.character(DATA$sex)

# removing some countries (see above comment)
indrm <- DATA$country %in% c("Thailand", "Iran", "Nepal", "Palestine", "Sudan")
DATA <- DATA[!indrm,]

#CairoPNG("Figure2cdots.png",width=1000,height=1000,pointsize=25)

png("Figure2cline.png",width=1000,height=1000,pointsize=25)
ages <- 15:24
sps <- spsprint <- c()
library(grDevices)
colsR <- colorRampPalette(c("green","yellow","magenta","blue"))
cols <- colsR(length(ages))
par("xaxs"="i");par("yaxs"="i");par(mar=c(4,4,1,2))
plot(NULL,type="n",xlim=c(0,100),ylim=c(0,100),ylab="% enrolled",xlab="% mother",axes=F,cex.lab=1.5,asp=1)
extr <- par("usr")
rect(0,0,100,100,col="#EBEBEB")
abline(v=seq(20,80,by=20),col="white")
abline(h=seq(20,80,by=20),col="white")
axis(1,cex=2,pos=0);axis(2,cex=2,pos=0)
for (i in 1:length(ages)){
	x <- 100*(1-DATA[DATA$age==ages[i] & DATA$sex=="Female" & DATA$round==2000,"prop_childless"])
	y <- 100*DATA[DATA$age==ages[i] & DATA$sex=="Female" & DATA$round==2000,"prop_school"]
	ctyi <- DATA[DATA$age==ages[i] & DATA$sex=="Female" & DATA$round==2000,"country"]
	yri <- DATA[DATA$age==ages[i] & DATA$sex=="Female" & DATA$round==2000,"year"]
	nax <- which(is.na(x)) ; nay <-  which(is.na(y))
	nas <- unique(c(nax,nay))
	minx <- min(x,na.rm=T) ; maxx <- max(x,na.rm=T)
	LM <- lm(y~x)
	xref <- data.frame(x=seq(from=minx, to=maxx, length.out=25))
	segments(minx,LM$coef[1]+LM$coef[2]*minx,maxx,LM$coef[1]+LM$coef[2]*maxx,col=cols[i],lwd=2)
	points(x,y,col=paste(cols[i],45,sep=""),pch=19)
	pv <- summary(LM)$coefficients[2,4] # p val
	pv <- ifelse(pv<.0001,"***",ifelse(pv<.001,"**",ifelse(pv<.01,"*",ifelse(pv<.05,"'",""))))
	sps[i] <- summary(LM)$coefficients[2,1]
	spsprint[i] <- paste(round(sps[i],3),pv)
}
rect(57,60,100,100,col="white")
legend("topright",col=cols,lwd=2,legend=paste(ages,", slope = ",spsprint,sep=""),box.col="transparent")
rect(0,0,100,100)
dev.off()

#############################
# Figure 2b
#------------------------------------------------------
# Male Scatter, all ages, Enrollment vs In Union
#------------------------------------------------------
DATA <- read.table("http://www.ced.uab.es/worldfam/figures/figure1_2.tab",header=T,sep="\t",na.strings = ".")
DATA$country <- as.character(DATA$country)
DATA$sex <- as.character(DATA$sex)

CairoPNG("Figure2b.png",width=1000,height=1000,pointsize=25)

png("Figure2bdots.png",width=1000,height=1000,pointsize=25)
ages <- 15:24
library(grDevices)
colsR <- colorRampPalette(c("green","yellow","magenta","blue"))
cols <- colsR(length(ages))
sdev <- spsprint <- sps <- cty <- c()
par("xaxs"="i");par("yaxs"="i");par(mar=c(4,4,1,2))
plot(NULL,type="n",xlim=c(0,100),ylim=c(0,100),ylab="% enrolled",xlab="% in union",axes=F,cex.lab=1.5,asp=1)
rect(0,0,100,100,col="#EBEBEB")
abline(v=seq(20,80,by=20),col="white")
abline(h=seq(20,80,by=20),col="white")
axis(1,cex=2,pos=0);axis(2,cex=2,pos=0)
for (i in 1:length(ages)){
	x <- 100*DATA[DATA$age==ages[i] & DATA$sex=="Male" & DATA$round==2000,"prop_union"]
	y <- 100*DATA[DATA$age==ages[i] & DATA$sex=="Male" & DATA$round==2000,"prop_school"]
	ctyi <- DATA[DATA$age==ages[i] & DATA$sex=="Male" & DATA$round==2000,"country"]
	yri <- DATA[DATA$age==ages[i] & DATA$sex=="Male" & DATA$round==2000,"year"]
	ctyi <- paste(ctyi,yri,sep="")
	nax <- which(is.na(x)) ; nay <-  which(is.na(y))
	nas <- unique(c(nax,nay))
	if (length(nas)>0){	ctyi <- ctyi[-nas]}
	cty <- c(cty,ctyi)
	minx <- min(x,na.rm=T) ; maxx <- max(x,na.rm=T)
	LM <- lm(y~x)
	xref <- data.frame(x=seq(from=minx, to=maxx, length.out=25))
	#segments(minx,LM$coef[1]+LM$coef[2]*minx,maxx,LM$coef[1]+LM$coef[2]*maxx,col=cols[i],lwd=2)
	points(x,y,col=paste(cols[i],45,sep=""),pch=19)
	pv <- summary(LM)$coefficients[2,4] # p val
	pv <- ifelse(pv<.0001,"***",ifelse(pv<.001,"**",ifelse(pv<.01,"*",ifelse(pv<.05,"'",""))))
	sps[i] <- summary(LM)$coefficients[2,1]
	spsprint[i] <- paste(round(sps[i],3),pv)

}
#rect(60,60,100,100,col="white")
#legend("topright",col=cols,lwd=2,legend=paste(ages,", slope = ",spsprint,sep=""),box.col="transparent")
rect(0,0,100,100)
dev.off()


png("Figure2bdots.png",width=1000,height=1000,pointsize=25)
ages <- 15:24
library(grDevices)
colsR <- colorRampPalette(c("green","yellow","magenta","blue"))
cols <- colsR(length(ages))
sdev <- spsprint <- sps <- cty <- c()
par("xaxs"="i");par("yaxs"="i");par(mar=c(4,4,1,2))
plot(NULL,type="n",xlim=c(0,100),ylim=c(0,100),ylab="% enrolled",xlab="% in union",axes=F,cex.lab=1.5,asp=1)
rect(0,0,100,100,col="#EBEBEB")
abline(v=seq(20,80,by=20),col="white")
abline(h=seq(20,80,by=20),col="white")
axis(1,cex=2,pos=0);axis(2,cex=2,pos=0)
for (i in 1:length(ages)){
	x <- 100*DATA[DATA$age==ages[i] & DATA$sex=="Male" & DATA$round==2000,"prop_union"]
	y <- 100*DATA[DATA$age==ages[i] & DATA$sex=="Male" & DATA$round==2000,"prop_school"]
	ctyi <- DATA[DATA$age==ages[i] & DATA$sex=="Male" & DATA$round==2000,"country"]
	yri <- DATA[DATA$age==ages[i] & DATA$sex=="Male" & DATA$round==2000,"year"]
	ctyi <- paste(ctyi,yri,sep="")
	nax <- which(is.na(x)) ; nay <-  which(is.na(y))
	nas <- unique(c(nax,nay))
	if (length(nas)>0){	ctyi <- ctyi[-nas]}
	cty <- c(cty,ctyi)
	minx <- min(x,na.rm=T) ; maxx <- max(x,na.rm=T)
	LM <- lm(y~x)
	xref <- data.frame(x=seq(from=minx, to=maxx, length.out=25))
	segments(minx,LM$coef[1]+LM$coef[2]*minx,maxx,LM$coef[1]+LM$coef[2]*maxx,col=cols[i],lwd=2)
	points(x,y,col=paste(cols[i],45,sep=""),pch=19)
	pv <- summary(LM)$coefficients[2,4] # p val
	pv <- ifelse(pv<.0001,"***",ifelse(pv<.001,"**",ifelse(pv<.01,"*",ifelse(pv<.05,"'",""))))
	sps[i] <- summary(LM)$coefficients[2,1]
	spsprint[i] <- paste(round(sps[i],3),pv)
	
}
#rect(60,60,100,100,col="white")
#legend("topright",col=cols,lwd=2,legend=paste(ages,", slope = ",spsprint,sep=""),box.col="transparent")
rect(0,0,100,100)
dev.off()


################################################
# Figure 3a
#------------------------------------------------------
# Female boxplots split on school attendance
#------------------------------------------------------


DATA <- read.table("http://www.ced.uab.es/worldfam/figures/figure3.tab",header=T,sep="\t",na.strings = ".")
DATA$country <- as.character(DATA$country)
DATA$sex <- as.character(DATA$sex)

emf(file="Figure3a.emf")
opar <- par()
cols <- c("#EEC900","#FF69B4","#CD5B45","#8B008B")
par("xaxs"="i","yaxs"="i",mar=c(11,4,1,2))
plot(NULL,type="n",xlim=c(14.5,24.5),ylim=c(0,100),ylab="% population",xlab="Age",cex.lab=1.5)
extr <- par("usr")
rect(extr[1],extr[3],extr[2],extr[4],col="#EBEBEB")
abline(v=seq(12,24,by=2),col="white")
abline(h=seq(20,80,by=20),col="white")

# iterate over ages
for (i in 15:24){
	###################
	# in school, in union
	###########
	y <- 100*DATA[DATA$age==i & DATA$sex=="Female" & DATA$round==2000 & DATA$school==1,"prop_union2"]
	x <- rep(i-.3,length(y))
	xmid <- -.37
	# IQR box:
	FN <- quantile(y,probs=c(.25,.5,.75),na.rm=TRUE)
	rect(i+xmid-.1,FN[1],i+xmid+.1,FN[3],col=cols[1]) #IQR box
	segments(i+xmid-.1,FN[2],i+xmid+.1,FN[2]) #median line
	maxcut <- ifelse(max(y,na.rm=T) > FN[3]+1.5*abs(diff(range(FN))),FN[3]+1.5*abs(diff(range(FN))),max(y,na.rm=T))
	mincut <- ifelse(min(y,na.rm=T) < FN[1]-1.5*abs(diff(range(FN))),FN[1]-1.5*abs(diff(range(FN))),min(y,na.rm=T))
	segments(i+xmid,FN[1],i+xmid,mincut,lty=2,col=cols[1]) # lower whisker
	segments(i+xmid,FN[3],i+xmid,maxcut,lty=2,col=cols[1]) # upper whisker
	points(c(i+xmid,i+xmid),c(mincut,maxcut),pch=19,col=cols[1],cex=.5)
	###################
	# in school, has child
	###########
	y <- 100*DATA[DATA$age==i & DATA$sex=="Female" & DATA$round==2000 & DATA$school==1,"prop_child2"]
	x <- rep(i-.3,length(y))
	xmid <- -.12
	#points(jitter(x,amount=.05),y,col="#FF450050",pch=19)
	# IQR box:"#FF69B4"
	FN <- quantile(y,probs=c(.25,.5,.75),na.rm=TRUE)
	rect(i+xmid-.1,FN[1],i+xmid+.1,FN[3],col=cols[2]) #IQR box
	segments(i+xmid-.1,FN[2],i+xmid+.1,FN[2]) #median line
	maxcut <- ifelse(max(y,na.rm=T) > FN[3]+1.5*abs(diff(range(FN))),FN[3]+1.5*abs(diff(range(FN))),max(y,na.rm=T))
	mincut <- ifelse(min(y,na.rm=T) < FN[1]-1.5*abs(diff(range(FN))),FN[1]-1.5*abs(diff(range(FN))),min(y,na.rm=T))
	segments(i+xmid,FN[1],i+xmid,mincut,lty=2,col=cols[2]) # lower whisker
	segments(i+xmid,FN[3],i+xmid,maxcut,lty=2,col=cols[2]) # upper whisker
	points(c(i+xmid,i+xmid),c(mincut,maxcut),pch=19,col=cols[2],cex=.5)
	###################
	# not in school, in union
	###########
	y <- 100*DATA[DATA$age==i & DATA$sex=="Female" & DATA$round==2000 & DATA$school==0,"prop_union2"]
	x <- rep(i-.3,length(y))
	xmid <- .12
	# IQR box:
	FN <- quantile(y,probs=c(.25,.5,.75),na.rm=TRUE)
	rect(i+xmid-.1,FN[1],i+xmid+.1,FN[3],col=cols[3]) #IQR box
	segments(i+xmid-.1,FN[2],i+xmid+.1,FN[2]) #median line
	maxcut <- ifelse(max(y,na.rm=T) > FN[3]+1.5*abs(diff(range(FN))),FN[3]+1.5*abs(diff(range(FN))),max(y,na.rm=T))
	mincut <- ifelse(min(y,na.rm=T) < FN[1]-1.5*abs(diff(range(FN))),FN[1]-1.5*abs(diff(range(FN))),min(y,na.rm=T))
	segments(i+xmid,FN[1],i+xmid,mincut,lty=2,col=cols[3]) # lower whisker
	segments(i+xmid,FN[3],i+xmid,maxcut,lty=2,col=cols[3]) # upper whisker
	points(c(i+xmid,i+xmid),c(mincut,maxcut),pch=19,col=cols[3],cex=.5)
	###################
	# not in school, has child
	###########
	y <- 100*DATA[DATA$age==i & DATA$sex=="Female" & DATA$round==2000 & DATA$school==0,"prop_child2"]
	x <- rep(i-.3,length(y))
	xmid <- .37
	# IQR box:
	FN <- quantile(y,probs=c(.25,.5,.75),na.rm=TRUE)
	rect(i+xmid-.1,FN[1],i+xmid+.1,FN[3],col=cols[4]) #IQR box
	segments(i+xmid-.1,FN[2],i+xmid+.1,FN[2]) #median line
	maxcut <- ifelse(max(y,na.rm=T) > FN[3]+1.5*abs(diff(range(FN))),FN[3]+1.5*abs(diff(range(FN))),max(y,na.rm=T))
	mincut <- ifelse(min(y,na.rm=T) < FN[1]-1.5*abs(diff(range(FN))),FN[1]-1.5*abs(diff(range(FN))),min(y,na.rm=T))
	segments(i+xmid,FN[1],i+xmid,mincut,lty=2,col=cols[4]) # lower whisker
	segments(i+xmid,FN[3],i+xmid,maxcut,lty=2,col=cols[4]) # upper whisker
	points(c(i+xmid,i+xmid),c(mincut,maxcut),pch=19,col=cols[4],cex=.5)
}
legend(13.5,-13,fill=cols,legend=c("in school, in union","in school, mother",
				"not in school, in union","not in school, mother"),xpd=T,cex=1.5)
par(opar)
dev.off()


##########################
# Figure 3b
DATA <- read.table("http://www.ced.uab.es/worldfam/figures/figure3.tab",header=T,sep="\t",na.strings = ".")
DATA$country <- as.character(DATA$country)
DATA$sex <- as.character(DATA$sex)

emf(file="Figure3b.emf")
omar <- par("mar")
cols <- c("#EEC900","#CD5B45")
QuantilesMat <- matrix(ncol=4,nrow=13)
par("xaxs"="i","yaxs"="i",mar=c(11,4,1,2))
plot(NULL,type="n",xlim=c(14.5,24.5),ylim=c(0,100),ylab="% in union",xlab="Age",cex.lab=1.5)
extr <- par("usr")
rect(extr[1],extr[3],extr[2],extr[4],col="#EBEBEB")
abline(v=seq(12,24,by=2),col="white")
abline(h=seq(20,80,by=20),col="white")
for (i in 15:24){
	###################
	# in school, in union
	###########
	y <- 100*DATA[DATA$age==i & DATA$sex=="Male" & DATA$round==2000 & DATA$school==1,"prop_union2"]
	# IQR box:
	FN <- quantile(y,probs=c(.25,.5,.75),na.rm=TRUE)
	rect(i-.3,FN[1],i-.1,FN[3],col=cols[1]) #IQR box
	segments(i-.3,FN[2],i-.1,FN[2]) #median line
	maxcut <- ifelse(max(y,na.rm=T) > FN[3]+1.5*abs(diff(range(FN))),FN[3]+1.5*abs(diff(range(FN))),max(y,na.rm=T))
	mincut <- ifelse(min(y,na.rm=T) < FN[1]-1.5*abs(diff(range(FN))),FN[1]-1.5*abs(diff(range(FN))),min(y,na.rm=T))
	segments(i-.2,FN[1],i-.2,mincut,lty=2,col=cols[1]) # lower whisker
	segments(i-.2,FN[3],i-.2,maxcut,lty=2,col=cols[1]) # upper whisker
	points(c(i-.2,i-.2),c(mincut,maxcut),pch=19,col=cols[1],cex=.5)
	###################
	# not in school, in union
	###########
	y <- 100*DATA[DATA$age==i & DATA$sex=="Male" & DATA$round==2000 & DATA$school==0,"prop_union2"]
	FN <- quantile(y,probs=c(.25,.5,.75),na.rm=TRUE)
	rect(i+.1,FN[1],i+.3,FN[3],col=cols[2])
	segments(i+.1,FN[2],i+.3,FN[2])
	maxcut <- ifelse(max(y,na.rm=T) > FN[3]+1.5*abs(diff(range(FN))),FN[3]+1.5*abs(diff(range(FN))),max(y,na.rm=T))
	mincut <- ifelse(min(y,na.rm=T) < FN[1]-1.5*abs(diff(range(FN))),FN[1]-1.5*abs(diff(range(FN))),min(y,na.rm=T))
	segments(i+.2,FN[1],i+.2,mincut,lty=2,col=cols[2])
	segments(i+.2,FN[3],i+.2,maxcut,lty=2,col=cols[2])
	points(c(i+.2,i+.2),c(mincut,maxcut),pch=19,col=cols[2],cex=.5)
}
legend(13.5,-13,fill=cols,legend=c("in school, in union","not in school, in union"),xpd=T,cex=1.5)
par(mar=omar)
dev.off()


########################
# Figure 4

DATA <- read.table("http://www.ced.uab.es/worldfam/figures/figure5.tab",header=T,sep="\t",na.strings = ".")
DATA$country <- as.character(DATA$country)
DATA$sex <- as.character(DATA$sex)

CairoPNG("Figure4.png",width=1000,height=1000,pointsize=25)
ages <- 15:24
library(grDevices)
colsR <- colorRampPalette(c("green","yellow","magenta","blue"))
cols <- colsR(length(ages))
sdev <- spsprint <- sps <- cty <- c()
par("xaxs"="i");par("yaxs"="i");par(mar=c(4,4,1,2))
plot(NULL,type="n",xlim=c(0,100),ylim=c(0,100),ylab="% in school total pop",
		xlab="% mother of those enrolled",cex.lab=1.5,asp=1,axes=F)
extr <- par("usr")
rect(0,0,100,100,col="#EBEBEB")
abline(v=seq(20,80,by=20),col="white")
abline(h=seq(20,80,by=20),col="white")
axis(1,cex=2,pos=0);axis(2,cex=2,pos=0)
for (i in 1:length(ages)){
	x <- 100*(1-DATA[DATA$age==ages[i] & DATA$sex=="Female" & DATA$year>=1998,"prop_childless_att"])
	y <- 100*DATA[DATA$age==ages[i] & DATA$sex=="Female" & DATA$year>=1998,"prop_school"]
	ctyi <- DATA[DATA$age==ages[i] & DATA$sex=="Female" & DATA$year>=1998,"country"]
	yri <- DATA[DATA$age==ages[i] & DATA$sex=="Female" & DATA$year>=1998,"year"]
	ctyi <- paste(ctyi,yri,sep="")
	nax <- which(is.na(x)) ; nay <-  which(is.na(y))
	nas <- unique(c(nax,nay))
	if (length(nas)>0){	ctyi <- ctyi[-nas]}
	cty <- c(cty,ctyi)
	minx <- min(x,na.rm=T) ; maxx <- max(x,na.rm=T)
	LM <- lm(y~x)
	xref <- data.frame(x=seq(from=minx, to=maxx, length.out=25))
	segments(minx,LM$coef[1]+LM$coef[2]*minx,maxx,LM$coef[1]+LM$coef[2]*maxx,col=cols[i],lwd=2)
	points(x,y,col=paste(cols[i],45,sep=""),pch=19)
	pv <- summary(LM)$coefficients[2,4] # p val
	pv <- ifelse(pv<.0001,"***",ifelse(pv<.001,"**",ifelse(pv<.01,"*",ifelse(pv<.05,"'",""))))
	sps[i] <- summary(LM)$coefficients[2,1]
	spsprint[i] <- paste(round(sps[i],3),pv)
	sdev[i] <- summary(LM)$coefficients[2,2]
}
rect(60,60,100,100,col="white")
legend("topright",col=cols,lwd=2,legend=paste(ages,", slope = ",spsprint,sep=""),box.col="transparent")
rect(0,0,100,100)
dev.off()

####################
# Figure 5

DATA <- read.table("http://www.ced.uab.es/worldfam/figures/figure6.tab",header=T,sep="\t",na.strings = ".")
DATA$country <- as.character(DATA$country)
DATA$sex <- as.character(DATA$sex)

CairoPNG("Figure5.png",width=1000,height=1000,pointsize=25)
ages <- 15:24
library(grDevices)
colsR <- colorRampPalette(c("green","yellow","magenta","blue"))
cols <- colsR(length(ages))
sdev <- spsprint <- sps <- cty <- c()
par("xaxs"="i","yaxs"="i")
plot(NULL,type="n",xlim=c(0,100),ylim=c(0,100),ylab="% mother total pop",
		xlab="% mother of those enrolled",cex.lab=1.5,asp=1,axes=FALSE)
extr <- par("usr")
rect(0,0,100,100,col="#EBEBEB")
abline(v=seq(20,80,by=20),col="white")
abline(h=seq(20,80,by=20),col="white")
axis(1,cex=2,pos=0);axis(2,cex=2,pos=0)
for (i in 1:length(ages)){
	x <- 100*(1-DATA[DATA$age==ages[i] & DATA$sex=="Female" & DATA$year>=1998,"prop_childless_att"])
	y <- 100*(1-DATA[DATA$age==ages[i] & DATA$sex=="Female" & DATA$year>=1998,"prop_childless"])
	ctyi <- DATA[DATA$age==ages[i] & DATA$sex=="Female" & DATA$year>=1998,"country"]
	yri <- DATA[DATA$age==ages[i] & DATA$sex=="Female" & DATA$year>=1998,"year"]
	ctyi <- paste(ctyi,yri,sep="")
	nax <- which(is.na(x)) ; nay <-  which(is.na(y))
	nas <- unique(c(nax,nay))
	if (length(nas)>0){	ctyi <- ctyi[-nas]}
	cty <- c(cty,ctyi)
	minx <- min(x,na.rm=T) ; maxx <- max(x,na.rm=T)
	LM <- lm(y~x)
	xref <- data.frame(x=seq(from=minx, to=maxx, length.out=25))
	segments(minx,LM$coef[1]+LM$coef[2]*minx,maxx,LM$coef[1]+LM$coef[2]*maxx,col=cols[i],lwd=2)
	points(x,y,col=paste(cols[i],45,sep=""),pch=19)
	pv <- summary(LM)$coefficients[2,4] # p val
	pv <- ifelse(pv<.0001,"***",ifelse(pv<.001,"**",ifelse(pv<.01,"*",ifelse(pv<.05,"'",""))))
	sps[i] <- summary(LM)$coefficients[2,1]
	spsprint[i] <- paste(round(sps[i],3),pv)
	sdev[i] <- summary(LM)$coefficients[2,2]
}
rect(55,0,100,46,col="white",border="black")
legend(x=55,y=46,col=cols,lwd=2,legend=paste(ages,", slope = ",spsprint,sep=""),bty="o",box.col="transparent")
rect(55,0,100,46)
rect(0,0,100,100)
dev.off()

####################
# Figure 6
DATA <- read.table("http://www.ced.uab.es/worldfam/figures/figure7.tab",header=T,sep="\t",na.strings = ".")
DATA$country <- as.character(DATA$country)
DATA$sex <- as.character(DATA$sex)
# Females, bivariate relationship, percentage in school and in union versus in union in the overall population
CairoPNG("Figure6.png",width=1000,height=1000,pointsize=25)
ages <- 15:24
library(grDevices)
colsR <- colorRampPalette(c("green","yellow","magenta","blue"))
cols <- colsR(length(ages))
sdev <- spsprint <- sps <- cty <- c()
par("xaxs"="i","yaxs"="i")
plot(NULL,type="n",xlim=c(0,100),ylim=c(0,100),ylab="% in union total pop",
		xlab="% in union of those enrolled",cex.lab=1.5,axes=FALSE,asp=1)
extr <- par("usr")
rect(0,0,100,100,col="#EBEBEB")
abline(v=seq(20,80,by=20),col="white")
abline(h=seq(20,80,by=20),col="white")
axis(1,cex=2,pos=0);axis(2,cex=2,pos=0)
for (i in 1:length(ages)){
	x <- 100*DATA[DATA$age==ages[i] & DATA$sex=="Female" & DATA$year>=1998,"prop_union_att"]
	y <- 100*DATA[DATA$age==ages[i] & DATA$sex=="Female" & DATA$year>=1998,"prop_union"]
	ctyi <- DATA[DATA$age==ages[i] & DATA$sex=="Female" & DATA$year>=1998,"country"]
	yri <- DATA[DATA$age==ages[i] & DATA$sex=="Female" & DATA$year>=1998,"year"]
	ctyi <- paste(ctyi,yri,sep="")
	nax <- which(is.na(x)) ; nay <-  which(is.na(y))
	nas <- unique(c(nax,nay))
	if (length(nas)>0){	ctyi <- ctyi[-nas]}
	cty <- c(cty,ctyi)
	minx <- min(x,na.rm=T) ; maxx <- max(x,na.rm=T)
	LM <- lm(y~x)
	xref <- data.frame(x=seq(from=minx, to=maxx, length.out=25))
	segments(minx,LM$coef[1]+LM$coef[2]*minx,maxx,LM$coef[1]+LM$coef[2]*maxx,col=cols[i],lwd=2)
	points(x,y,col=paste(cols[i],45,sep=""),pch=19)
	pv <- summary(LM)$coefficients[2,4] # p val
	pv <- ifelse(pv<.0001,"***",ifelse(pv<.001,"**",ifelse(pv<.01,"*",ifelse(pv<.05,"'",""))))
	sps[i] <- summary(LM)$coefficients[2,1]
	spsprint[i] <- paste(round(sps[i],3),pv)
	sdev[i] <- summary(LM)$coefficients[2,2]
}
rect(55,0,100,46,col="white",border="black")
legend(x=55,y=46,col=cols,lwd=2,legend=paste(ages,", slope = ",spsprint,sep=""),bty="o",box.col="transparent")
rect(55,0,100,46)
rect(0,0,100,100)
dev.off()
#######################
# Figure 8 (old figure 7 deprecated)

DATA <- read.table("C:\\Users\\triffe\\git\\ViennaPaper\\ALBERTPAPERS\\Figures\\figsW\\data\\Figure8.txt",header=T,sep="\t",na.strings = ".")
DATA$country <- as.character(DATA$country)
colalpha <- function(color,alpha){
	colalphai <- function(color,alpha){
		paste(rgb(t(col2rgb(color)/255)),alpha,sep="")
	}
	sapply(color,colalphai,alpha=alpha)
}

CairoPNG("Figure7.png",width=1000,height=1000,pointsize=25)
plot(NULL,type="n",xlim=c(0,100),ylim=c(0,100),
		ylab="% mother total pop (20)",xlab="% mother of those in school (20)",
		axes=FALSE,cex.lab=1.5,asp=1)
extr <- par("usr")
rect(0,0,100,100,col="#EBEBEB")
abline(v=seq(20,80,by=20),col="white")
abline(h=seq(20,80,by=20),col="white")
colsi <- c("purple","orange")
axis(1,cex=2,pos=0);axis(2,cex=2,pos=0)
for (i in 1:2){
	x1 <- 100*DATA[,i+2]
	y1 <- 100*DATA$prop_child
	x <- x1[!(x1==0 | x1 == 100 | y1==0 | y1 == 100)]
	y <- y1[!(x1==0 | x1 == 100 | y1==0 | y1 == 100)]
	points(x,y,col=colalpha(colsi[i],65),pch=19)
	minx <- min(x,na.rm=T) ; maxx <- max(x,na.rm=T)
	LM <- lm(y~x) # OLS
	xref <- data.frame(x=seq(from=minx, to=maxx, length.out=25))
	clim <- as.data.frame(predict(LM, xref, level=0.95, interval="confidence")) # confidence limits
	#paste(cols[i],15,sep="")
	polygon(c(xref$x,rev(xref$x)),c(clim$lwr,rev(clim$upr)),col="#30303010",border="transparent")
	lines(cbind(xref,clim$lwr), col=colsi[i], lty="dashed")
	lines(cbind(xref,clim$upr), col=colsi[i], lty="dashed")
	segments(minx,LM$coef[1]+LM$coef[2]*minx,maxx,LM$coef[1]+LM$coef[2]*maxx,col=colsi[i],lwd=2)
}
rect(65,0,100,13,col="white",border="black")
legend(65,13,col=colsi,lty=1,lwd=2,legend=c("primary","secondary +"),bty="o",box.col="transparent")
rect(65,0,100,13)
rect(0,0,100,100)
dev.off()




