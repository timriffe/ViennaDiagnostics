# TODO: Add comment
# 
# Author: triffe
###############################################################################
#file.choose()
DATA <- read.table("E:\\WRITING\\ViennaDec2011\\proportions.txt",header=T,sep="\t",na.strings = ".")
DATA$country <- as.character(DATA$country)
DATA$sex <- as.character(DATA$sex)
head(DATA)
lowedu <- list()
for (i in 12:24){
	lowedu[[i]] <- DATA$country[DATA$prop_school < .1 & DATA$age == i & DATA$sex =="Female" & DATA$round == 2000]
}

# DATA[DATA$country=="Slovenia",]
# Italy only has 6 lines, particular ages 12,17,22 	(5-year age groups, leave out)
# Albania has no school info
# Palestine only has ages 12, 17 and 22 			(5-year age groups, leave out)
# Slovenia: only ages 12,17,22 						(5-year age groups, leave out)

# Figure 1: (females, 2000 round, in union)
ages <- c(16,20,24)
cols <- c("#CD2626","#66CD00","#6495ED")
par("xaxs"="i","yaxs"="i")
plot(NULL,type="n",xlim=c(0,100),ylim=c(0,100),ylab="% enrolled",xlab="% in union")
extr <- par("usr")
rect(extr[1],extr[3],extr[2],extr[4],col="#EBEBEB")
abline(v=seq(20,80,by=20),col="white")
abline(h=seq(20,80,by=20),col="white")
# iterates over ages,cols: fit line, calc conf, plot band, then line, then points
cty <- c()
for (i in 1:3){
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
	LM <- lm(y~x) # OLS
	xref <- data.frame(x=seq(from=minx, to=maxx, length.out=25))
	clim <- as.data.frame(predict(LM, xref, level=0.95, interval="confidence")) # confidence limits
	#paste(cols[i],15,sep="")
	polygon(c(xref$x,rev(xref$x)),c(clim$lwr,rev(clim$upr)),col="#30303010",border="transparent")
	lines(cbind(xref,clim$lwr), col=cols[i], lty="dashed")
	lines(cbind(xref,clim$upr), col=cols[i], lty="dashed")
	segments(minx,LM$coef[1]+LM$coef[2]*minx,maxx,LM$coef[1]+LM$coef[2]*maxx,col=cols[i],lwd=2)
	points(x,y,col=cols[i],pch=19)
}
legend("topright",lwd=2,col=cols,legend=paste("age",ages))
cty<-unique(cty)

# Figure 1: (females, 2000 round, childless)
ages <- c(16,20,24)
cols <- c("#CD2626","#66CD00","#6495ED")
par("xaxs"="i","yaxs"="i")
plot(NULL,type="n",xlim=c(0,100),ylim=c(0,100),ylab="% enrolled",xlab="% mother")
extr <- par("usr")
rect(extr[1],extr[3],extr[2],extr[4],col="#EBEBEB")
abline(v=seq(20,80,by=20),col="white")
abline(h=seq(20,80,by=20),col="white")
# iterates over ages,cols: fit line, calc conf, plot band, then line, then points
cty <- c()
for (i in 1:3){
	x <- 100*(1-DATA[DATA$age==ages[i] & DATA$sex=="Female" & DATA$round==2000,"prop_childless"])
	y <- 100*DATA[DATA$age==ages[i] & DATA$sex=="Female" & DATA$round==2000,"prop_school"]
	ctyi <- DATA[DATA$age==ages[i] & DATA$sex=="Female" & DATA$round==2000,"country"]
	nax <- which(is.na(x)) ; nay <-  which(is.na(y))
	nas <- unique(c(nax,nay))
	ctyi <- ctyi[-nas]
	cty <- c(cty,ctyi)
	minx <- min(x,na.rm=T) ; maxx <- max(x,na.rm=T)
	LM <- lm(y~x)
	xref <- data.frame(x=seq(from=minx, to=maxx, length.out=25))
	clim <- as.data.frame(predict(LM, xref, level=0.95, interval="confidence"))
	#paste(cols[i],15,sep="")
	polygon(c(xref$x,rev(xref$x)),c(clim$lwr,rev(clim$upr)),col="#30303010",border="transparent")
	lines(cbind(xref,clim$lwr), col=cols[i], lty="dashed")
	lines(cbind(xref,clim$upr), col=cols[i], lty="dashed")
	segments(minx,LM$coef[1]+LM$coef[2]*minx,maxx,LM$coef[1]+LM$coef[2]*maxx,col=cols[i],lwd=2)
	points(x,y,col=cols[i],pch=19)
}
legend("topright",lwd=2,col=cols,legend=paste("age",ages))
cty <- unique(cty)
ind <- unique(DATA$country[DATA$round==2000])%in%cty
unique(DATA$country[DATA$round==2000])[ind==F]
# missing countries:
fix(cty)

# Figure 1: (males, 2000 round, in union)
ages <- c(16,20,24)
cols <- c("#CD2626","#66CD00","#6495ED")
par("xaxs"="i","yaxs"="i")
plot(NULL,type="n",xlim=c(0,100),ylim=c(0,100),ylab="% enrolled",xlab="% in union")
extr <- par("usr")
rect(extr[1],extr[3],extr[2],extr[4],col="#EBEBEB")
abline(v=seq(20,80,by=20),col="white")
abline(h=seq(20,80,by=20),col="white")
# iterates over ages,cols: fit line, calc conf, plot band, then line, then points
cty <- cty <-  c()
for (i in 1:3){
	x <- 100*DATA[DATA$age==ages[i] & DATA$sex=="Male" & DATA$round==2000,"prop_union"]
	y <- 100*DATA[DATA$age==ages[i] & DATA$sex=="Male" & DATA$round==2000,"prop_school"]
	ctyi <- DATA[DATA$age==ages[i] & DATA$sex=="Male" & DATA$round==2000,"country"]
	nax <- which(is.na(x)) ; nay <-  which(is.na(y))
	nas <- unique(c(nax,nay))
	ctyi <- ctyi[-nas]
	cty <- c(cty,ctyi)
	minx <- min(x,na.rm=T) ; maxx <- max(x,na.rm=T)
	LM <- lm(y~x)
	xref <- data.frame(x=seq(from=minx, to=maxx, length.out=25))
	clim <- as.data.frame(predict(LM, xref, level=0.95, interval="confidence"))
	#paste(cols[i],15,sep="")
	polygon(c(xref$x,rev(xref$x)),c(clim$lwr,rev(clim$upr)),col="#30303010",border="transparent")
	lines(cbind(xref,clim$lwr), col=cols[i], lty="dashed")
	lines(cbind(xref,clim$upr), col=cols[i], lty="dashed")
	segments(minx,LM$coef[1]+LM$coef[2]*minx,maxx,LM$coef[1]+LM$coef[2]*maxx,col=cols[i],lwd=2)
	points(x,y,col=cols[i],pch=19)
}
legend("topright",lwd=2,col=cols,legend=paste("age",ages))
cty<-unique(cty)
ind <- unique(DATA$country[DATA$round==2000])%in%cty
unique(DATA$country[DATA$round==2000])[ind==F]
# missing countries:
fix(cty)

# Figure 1, all ages, only OLS lines, females, 2000 round, in union
ages <- 12:24
library(grDevices)
colsR <- colorRampPalette(c("green","yellow","magenta","blue"))
cols <- colsR(length(ages))
sps <- c()
par("xaxs"="i","yaxs"="i")
plot(NULL,type="n",xlim=c(0,100),ylim=c(0,100),ylab="% enrolled",xlab="% in union")
extr <- par("usr")
rect(extr[1],extr[3],extr[2],extr[4],col="#EBEBEB")
abline(v=seq(20,80,by=20),col="white")
abline(h=seq(20,80,by=20),col="white")
for (i in 1:length(ages)){
	x <- 100*DATA[DATA$age==ages[i] & DATA$sex=="Female" & DATA$round==2000,"prop_union"]
	y <- 100*DATA[DATA$age==ages[i] & DATA$sex=="Female" & DATA$round==2000,"prop_school"]
	minx <- min(x,na.rm=T) ; maxx <- max(x,na.rm=T)
	LM <- lm(y~x)
	xref <- data.frame(x=seq(from=minx, to=maxx, length.out=25))
	segments(minx,LM$coef[1]+LM$coef[2]*minx,maxx,LM$coef[1]+LM$coef[2]*maxx,col=cols[i],lwd=2)
	points(x,y,col=paste(cols[i],35,sep=""),pch=19)
	pv <- summary(LM)$coefficients[2,4] # p val
	pv <- ifelse(pv<.0001,"***",ifelse(pv<.001,"**",ifelse(pv<.01,"*",ifelse(pv<.05,"'",""))))
	sps[i] <- paste(round(summary(LM)$coefficients[2,1],3),pv)
}
legend("topright",col=cols,lwd=2,legend=paste(ages,", slope = ",sps,sep=""))


# Figure 2: proposal 1, rather cluttery
par("xaxs"="i","yaxs"="i",mar=c(8,4,3,2))
plot(NULL,type="n",xlim=c(11.5,24.5),ylim=c(0,100),ylab="% population",xlab="Age")
extr <- par("usr")
rect(extr[1],extr[3],extr[2],extr[4],col="#EBEBEB")
abline(v=seq(12,24,by=2),col="white")
abline(h=seq(20,80,by=20),col="white")
for (i in 12:24){
	# in school
	y <- 100*DATA[DATA$age==i & DATA$sex=="Female" & DATA$round==2000,"prop_school"]
	x <- rep(i-.3,length(y))
	FN <- quantile(y,probs=c(.25,.5,.75),na.rm=TRUE)
	maxcut <- ifelse(max(y,na.rm=T) > FN[3]+1.5*abs(diff(range(FN))),FN[3]+1.5*abs(diff(range(FN))),max(y,na.rm=T))
	mincut <- ifelse(min(y,na.rm=T) < FN[1]-1.5*abs(diff(range(FN))),FN[1]-1.5*abs(diff(range(FN))),min(y,na.rm=T))
	segments(i-.3,mincut,i-.3,maxcut)
	points(jitter(x,amount=.05),y,col="#FF450050",pch=19)
	# IQR box:
	rect(i-.4,FN[1],i-.2,FN[3])
	# in union
	y <- 100*DATA[DATA$age==i & DATA$sex=="Female" & DATA$round==2000,"prop_union"]
	x <- rep(i,length(y))
	FN <- quantile(y,probs=c(.25,.5,.75),na.rm=TRUE)
	maxcut <- ifelse(max(y,na.rm=T) > FN[3]+1.5*abs(diff(range(FN))),FN[3]+1.5*abs(diff(range(FN))),max(y,na.rm=T))
	mincut <- ifelse(min(y,na.rm=T) < FN[1]-1.5*abs(diff(range(FN))),FN[1]-1.5*abs(diff(range(FN))),min(y,na.rm=T))
	segments(i,mincut,i,maxcut)
	points(jitter(x,amount=.05),y,col="#7D26CD30",pch=19)
	rect(i-.1,FN[1],i+.1,FN[3])
	# mother
	y <- 100*(1-DATA[DATA$age==i & DATA$sex=="Female" & DATA$round==2000,"prop_childless"])
	x <- rep(i+.3,length(y))
	FN <- quantile(y,probs=c(.25,.5,.75),na.rm=TRUE)
	maxcut <- ifelse(max(y,na.rm=T) > FN[3]+1.5*abs(diff(range(FN))),FN[3]+1.5*abs(diff(range(FN))),max(y,na.rm=T))
	mincut <- ifelse(min(y,na.rm=T) < FN[1]-1.5*abs(diff(range(FN))),FN[1]-1.5*abs(diff(range(FN))),min(y,na.rm=T))
	segments(i+.3,mincut,i+.3,maxcut)
	points(jitter(x,amount=.05),y,col="#FF69B460",pch=19)
	rect(i+.2,FN[1],i+.4,FN[3])
}
legend(12,-12,fill=c("#FF4500","#7D26CD","#FF69B4"),legend=c("% enrolled","% union","% parent"),xpd=T)



plot(1:40,1:40,pch=1:40)
text(1:40,2:42,1:40)
# 2, closer to final: hand-drawn boxplots:,mar=c(9,5,3,1)
omar <- par("mar")
par("xaxs"="i","yaxs"="i",mar=c(8,4,3,2))
plot(NULL,type="n",xlim=c(11.5,24.5),ylim=c(0,100),ylab="% population",xlab="Age")
QuantilesMat <- matrix(ncol=8,nrow=13)
extr <- par("usr")
rect(extr[1],extr[3],extr[2],extr[4],col="#EBEBEB")
abline(v=seq(12,24,by=2),col="white")
abline(h=seq(20,80,by=20),col="white")
for (i in 12:24){
	# in school
	y <- 100*DATA[DATA$age==i & DATA$sex=="Female" & DATA$round==2000,"prop_school"]
	x <- rep(i-.3,length(y))
	#points(jitter(x,amount=.05),y,col="#FF450050",pch=19)
	# IQR box:
	FN <- quantile(y,probs=c(.25,.5,.75),na.rm=TRUE)
	rect(i-.4,FN[1],i-.2,FN[3],col="#EEC900") #IQR box
	segments(i-.4,FN[2],i-.2,FN[2]) #median line
	maxcut <- ifelse(max(y,na.rm=T) > FN[3]+1.5*abs(diff(range(FN))),FN[3]+1.5*abs(diff(range(FN))),max(y,na.rm=T))
	mincut <- ifelse(min(y,na.rm=T) < FN[1]-1.5*abs(diff(range(FN))),FN[1]-1.5*abs(diff(range(FN))),min(y,na.rm=T))
	
	segments(i-.3,FN[1],i-.3,mincut,lty=2,col="#EEC900") # lower whisker
	segments(i-.3,FN[3],i-.3,maxcut,lty=2,col="#EEC900") # upper whisker
	points(c(i-.3,i-.3),c(mincut,maxcut),pch=19,col="#EEC900",cex=.5)
	# in union
	y <- 100*DATA[DATA$age==i & DATA$sex=="Female" & DATA$round==2000,"prop_union"]
	x <- rep(i,length(y))
	#points(jitter(x,amount=.05),y,col="#7D26CD30",pch=19)
	FN <- quantile(y,probs=c(.25,.5,.75),na.rm=TRUE)
	QuantilesMat[i-11,1:3] <- c(FN)
	QuantilesMat[i-11,4] <- length(y[!is.na(y)])
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
	#points(jitter(x,amount=.05),y,col="#FF69B460",pch=19)
	FN <- quantile(y,probs=c(.25,.5,.75),na.rm=TRUE)
	QuantilesMat[i-11,5:7] <- c(FN)
	QuantilesMat[i-11,8] <- length(y[!is.na(y)])
	rect(i+.2,FN[1],i+.4,FN[3],col="#FF69B4")
	segments(i+.2,FN[2],i+.4,FN[2]) 
	maxcut <- ifelse(max(y,na.rm=T) > FN[3]+1.5*abs(diff(range(FN))),FN[3]+1.5*abs(diff(range(FN))),max(y,na.rm=T))
	mincut <- ifelse(min(y,na.rm=T) < FN[1]-1.5*abs(diff(range(FN))),FN[1]-1.5*abs(diff(range(FN))),min(y,na.rm=T))
	segments(i+.3,FN[1],i+.3,mincut,lty=2,col="#FF69B4")
	segments(i+.3,FN[3],i+.3,maxcut,lty=2,col="#FF69B4")
	points(c(i+.3,i+.3),c(mincut,maxcut),pch=19,col="#FF69B4",cex=.5)
}
legend(12,-12,fill=c("#EEC900","#7D26CD","#FF69B4"),legend=c("% enrolled","% union","% parent"),xpd=T)
par(mar=omar)
colnames(QuantilesMat) <- c("union .25","union .5","union .75","obs","parent .25","parent .5","parent .75","obs")
rownames(QuantilesMat)<- 12:24
##########################
plot(12:24,QuantilesMat[,4],type="s",xlab="age",ylab="observations",col="#7D26CD",ylim=c(0,110))
lines(12:24,QuantilesMat[,8],type="s",col="#FF69B4",lty=2)
legend("bottomright",col=c("#7D26CD","#FF69B4"),lty=c(1,2),legend=c("obs in union","obs parent"))


# 2, male boxplots, just in union- no prop child
omar <- par("mar")
QuantilesMat <- matrix(ncol=4,nrow=13)
par("xaxs"="i","yaxs"="i",mar=c(8,4,3,2))
plot(NULL,type="n",xlim=c(11.5,24.5),ylim=c(0,100),ylab="% population",xlab="Age")
extr <- par("usr")
rect(extr[1],extr[3],extr[2],extr[4],col="#EBEBEB")
abline(v=seq(12,24,by=2),col="white")
abline(h=seq(20,80,by=20),col="white")
for (i in 12:24){
	# in school
	y <- 100*DATA[DATA$age==i & DATA$sex=="Male" & DATA$round==2000,"prop_school"]
	#points(jitter(x,amount=.05),y,col="#FF450050",pch=19)
	# IQR box:
	FN <- quantile(y,probs=c(.25,.5,.75),na.rm=TRUE)
	rect(i-.3,FN[1],i-.1,FN[3],col="#EEC900") #IQR box
	segments(i-.3,FN[2],i-.1,FN[2]) #median line
	maxcut <- ifelse(max(y,na.rm=T) > FN[3]+1.5*abs(diff(range(FN))),FN[3]+1.5*abs(diff(range(FN))),max(y,na.rm=T))
	mincut <- ifelse(min(y,na.rm=T) < FN[1]-1.5*abs(diff(range(FN))),FN[1]-1.5*abs(diff(range(FN))),min(y,na.rm=T))
	segments(i-.2,FN[1],i-.2,mincut,lty=2,col="#EEC900") # lower whisker
	segments(i-.2,FN[3],i-.2,maxcut,lty=2,col="#EEC900") # upper whisker
	points(c(i-.2,i-.2),c(mincut,maxcut),pch=19,col="#EEC900",cex=.5)
	# in union
	y <- 100*DATA[DATA$age==i & DATA$sex=="Male" & DATA$round==2000,"prop_union"]
	#points(jitter(x,amount=.05),y,col="#7D26CD30",pch=19)
	FN <- quantile(y,probs=c(.25,.5,.75),na.rm=TRUE)
	QuantilesMat[i-11,1:3] <- c(FN)
	QuantilesMat[i-11,4] <- length(y[!is.na(y)])
	rect(i+.1,FN[1],i+.3,FN[3],col="#7D26CD")
	segments(i+.1,FN[2],i+.3,FN[2])
	maxcut <- ifelse(max(y,na.rm=T) > FN[3]+1.5*abs(diff(range(FN))),FN[3]+1.5*abs(diff(range(FN))),max(y,na.rm=T))
	mincut <- ifelse(min(y,na.rm=T) < FN[1]-1.5*abs(diff(range(FN))),FN[1]-1.5*abs(diff(range(FN))),min(y,na.rm=T))
	segments(i+.2,FN[1],i+.2,mincut,lty=2,col="#7D26CD")
	segments(i+.2,FN[3],i+.2,maxcut,lty=2,col="#7D26CD")
	points(c(i+.2,i+.2),c(mincut,maxcut),pch=19,col="#7D26CD",cex=.5)
}
legend(12,-12,fill=c("#EEC900","#7D26CD"),legend=c("% enrolled","% union"),xpd=T)
par(mar=omar)
colnames(QuantilesMat) <- c("union .25","union .5","union .75","obs")
rownames(QuantilesMat)<- 12:24
##########################

##########################

##########################

##########################
# now for substantivelt different graphics: separate boxes for in school and not in school
# in union, not in union for each age.

DATA <- read.table("E:\\WRITING\\ViennaDec2011\\proportions_att.txt",header=T,sep="\t",na.strings = ".")
DATA$country <- as.character(DATA$country)
DATA$sex <- as.character(DATA$sex)
head(DATA)

# colors: in school in union, in school not in union, not in school in union, not in school not in union
cols <- c("#EEC900","#FF69B4","#FFB90F","#8B008B")
windows(width=8,height=7)
par("xaxs"="i","yaxs"="i",mar=c(8,4,3,2))
plot(NULL,type="n",xlim=c(11.5,24.5),ylim=c(0,100),ylab="% population",xlab="Age")
extr <- par("usr")
rect(extr[1],extr[3],extr[2],extr[4],col="#EBEBEB")
abline(v=seq(12,24,by=2),col="white")
abline(h=seq(20,80,by=20),col="white")
for (i in 12:24){
	# in school prop in union
	y <- 100*DATA[DATA$age==i & DATA$sex=="Female" & DATA$round==2000 & DATA$school==1,"prop_union2"]
	x <- rep(i-.3,length(y))
	xmid <- -.37
	#points(jitter(x,amount=.05),y,col="#FF450050",pch=19)
	# IQR box:
	FN <- fivenum(y)[2:4]
	rect(i+xmid-.1,FN[1],i+xmid+.1,FN[3],col=cols[1]) #IQR box
	segments(i+xmid-.1,FN[2],i+xmid+.1,FN[2]) #median line
	maxcut <- ifelse(max(y,na.rm=T) > FN[3]+1.5*abs(diff(range(FN))),FN[3]+1.5*abs(diff(range(FN))),max(y,na.rm=T))
	mincut <- ifelse(min(y,na.rm=T) < FN[1]-1.5*abs(diff(range(FN))),FN[1]-1.5*abs(diff(range(FN))),min(y,na.rm=T))
	segments(i+xmid,FN[1],i+xmid,mincut,lty=2,col=cols[1]) # lower whisker
	segments(i+xmid,FN[3],i+xmid,maxcut,lty=2,col=cols[1]) # upper whisker
	points(c(i+xmid,i+xmid),c(mincut,maxcut),pch=19,col=cols[1],cex=.5)
	# in union
	
	# in school prop with child
	y <- 100*DATA[DATA$age==i & DATA$sex=="Female" & DATA$round==2000 & DATA$school==1,"prop_child2"]
	x <- rep(i-.3,length(y))
	xmid <- -.12
	#points(jitter(x,amount=.05),y,col="#FF450050",pch=19)
	# IQR box:"#FF69B4"
	FN <- fivenum(y)[2:4]
	rect(i+xmid-.1,FN[1],i+xmid+.1,FN[3],col=cols[2]) #IQR box
	segments(i+xmid-.1,FN[2],i+xmid+.1,FN[2]) #median line
	maxcut <- ifelse(max(y,na.rm=T) > FN[3]+1.5*abs(diff(range(FN))),FN[3]+1.5*abs(diff(range(FN))),max(y,na.rm=T))
	mincut <- ifelse(min(y,na.rm=T) < FN[1]-1.5*abs(diff(range(FN))),FN[1]-1.5*abs(diff(range(FN))),min(y,na.rm=T))
	segments(i+xmid,FN[1],i+xmid,mincut,lty=2,col=cols[2]) # lower whisker
	segments(i+xmid,FN[3],i+xmid,maxcut,lty=2,col=cols[2]) # upper whisker
	points(c(i+xmid,i+xmid),c(mincut,maxcut),pch=19,col=cols[2],cex=.5)
	
	# not in school in union
	y <- 100*DATA[DATA$age==i & DATA$sex=="Female" & DATA$round==2000 & DATA$school==0,"prop_union2"]
	x <- rep(i-.3,length(y))
	xmid <- .12
	#points(jitter(x,amount=.05),y,col="#FF450050",pch=19)
	# IQR box:
	FN <- fivenum(y)[2:4]
	rect(i+xmid-.1,FN[1],i+xmid+.1,FN[3],col=cols[3]) #IQR box
	segments(i+xmid-.1,FN[2],i+xmid+.1,FN[2]) #median line
	maxcut <- ifelse(max(y,na.rm=T) > FN[3]+1.5*abs(diff(range(FN))),FN[3]+1.5*abs(diff(range(FN))),max(y,na.rm=T))
	mincut <- ifelse(min(y,na.rm=T) < FN[1]-1.5*abs(diff(range(FN))),FN[1]-1.5*abs(diff(range(FN))),min(y,na.rm=T))
	segments(i+xmid,FN[1],i+xmid,mincut,lty=2,col=cols[3]) # lower whisker
	segments(i+xmid,FN[3],i+xmid,maxcut,lty=2,col=cols[3]) # upper whisker
	points(c(i+xmid,i+xmid),c(mincut,maxcut),pch=19,col=cols[3],cex=.5)
	
	# not in school has child
	y <- 100*DATA[DATA$age==i & DATA$sex=="Female" & DATA$round==2000 & DATA$school==0,"prop_child2"]
	x <- rep(i-.3,length(y))
	xmid <- .37
	#points(jitter(x,amount=.05),y,col="#FF450050",pch=19)
	# IQR box:
	FN <- fivenum(y)[2:4]
	rect(i+xmid-.1,FN[1],i+xmid+.1,FN[3],col=cols[4]) #IQR box
	segments(i+xmid-.1,FN[2],i+xmid+.1,FN[2]) #median line
	maxcut <- ifelse(max(y,na.rm=T) > FN[3]+1.5*abs(diff(range(FN))),FN[3]+1.5*abs(diff(range(FN))),max(y,na.rm=T))
	mincut <- ifelse(min(y,na.rm=T) < FN[1]-1.5*abs(diff(range(FN))),FN[1]-1.5*abs(diff(range(FN))),min(y,na.rm=T))
	segments(i+xmid,FN[1],i+xmid,mincut,lty=2,col=cols[4]) # lower whisker
	segments(i+xmid,FN[3],i+xmid,maxcut,lty=2,col=cols[4]) # upper whisker
	points(c(i+xmid,i+xmid),c(mincut,maxcut),pch=19,col=cols[4],cex=.5)
}
legend(12,-12,fill=cols,legend=c("in school & union","in school has child","not in school in union","not in school has child"),xpd=T)



graphics.off()

# males attending, not attending
cols <- c("#EEC900","#FFB90F")
windows(width=8,height=7)
par("xaxs"="i","yaxs"="i",mar=c(8,4,3,2))
plot(NULL,type="n",xlim=c(11.5,24.5),ylim=c(0,100),ylab="% in union",xlab="Age")
extr <- par("usr")
rect(extr[1],extr[3],extr[2],extr[4],col="#EBEBEB")
abline(v=seq(12,24,by=2),col="white")
abline(h=seq(20,80,by=20),col="white")
for (i in 12:24){
	# in school prop in union
	y <- 100*DATA[DATA$age==i & DATA$sex=="Female" & DATA$round==2000 & DATA$school==1,"prop_union2"]
	x <- rep(i-.3,length(y))
	xmid <- -.12
	#points(jitter(x,amount=.05),y,col="#FF450050",pch=19)
	# IQR box:
	FN <- fivenum(y)[2:4]
	rect(i+xmid-.1,FN[1],i+xmid+.1,FN[3],col=cols[1]) #IQR box
	segments(i+xmid-.1,FN[2],i+xmid+.1,FN[2]) #median line
	maxcut <- ifelse(max(y,na.rm=T) > FN[3]+1.5*abs(diff(range(FN))),FN[3]+1.5*abs(diff(range(FN))),max(y,na.rm=T))
	mincut <- ifelse(min(y,na.rm=T) < FN[1]-1.5*abs(diff(range(FN))),FN[1]-1.5*abs(diff(range(FN))),min(y,na.rm=T))
	segments(i+xmid,FN[1],i+xmid,mincut,lty=2,col=cols[1]) # lower whisker
	segments(i+xmid,FN[3],i+xmid,maxcut,lty=2,col=cols[1]) # upper whisker
	points(c(i+xmid,i+xmid),c(mincut,maxcut),pch=19,col=cols[1],cex=.5)
	# in union
	
	# not in school in union
	y <- 100*DATA[DATA$age==i & DATA$sex=="Female" & DATA$round==2000 & DATA$school==0,"prop_union2"]
	x <- rep(i-.3,length(y))
	xmid <- .12
	#points(jitter(x,amount=.05),y,col="#FF450050",pch=19)
	# IQR box:
	FN <- fivenum(y)[2:4]
	rect(i+xmid-.1,FN[1],i+xmid+.1,FN[3],col=cols[2]) #IQR box
	segments(i+xmid-.1,FN[2],i+xmid+.1,FN[2]) #median line
	maxcut <- ifelse(max(y,na.rm=T) > FN[3]+1.5*abs(diff(range(FN))),FN[3]+1.5*abs(diff(range(FN))),max(y,na.rm=T))
	mincut <- ifelse(min(y,na.rm=T) < FN[1]-1.5*abs(diff(range(FN))),FN[1]-1.5*abs(diff(range(FN))),min(y,na.rm=T))
	segments(i+xmid,FN[1],i+xmid,mincut,lty=2,col=cols[2]) # lower whisker
	segments(i+xmid,FN[3],i+xmid,maxcut,lty=2,col=cols[2]) # upper whisker
	points(c(i+xmid,i+xmid),c(mincut,maxcut),pch=19,col=cols[2],cex=.5)
	
}
legend(12,-12,fill=cols,legend=c("in school & union","not in school in union"),xpd=T)



