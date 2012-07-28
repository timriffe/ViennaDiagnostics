 
# Author: triffe
###############################################################################

# Data Prep is done separately.
# Data to be found in folder DATAfigBW in Rdata binaries, already adjusted
# (smoothed for age heaping in some cases), already in format rquired for figure
# syntax

# this syntax will aim to create all plots 9x9 cm in bmp format
setwd("/home/triffe/git/ViennaDiagnostics/ALBERTPAPERS/Figures")

# load some functions, packages:
library(MethComp)
# custom function for returning CIs
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

# globals:
#
dims       <- c(8,8)
units      <- "cm"
res        <- 300
pointsize  <- 5

# --------------------------------------------------------------------------------
# Figure 1a

load("DATAfigBW/Fig1_DATAadj.Rdata")

# females boxplot, Tufte-style
ww <- .09
lfs <- -.25
rgs <- .25
s.lwd <- 1.5
t.cex <- 1.4
#emf(file="Figure1aBW.emf",width=7,height=7)
#pdf(file="Figure1aBW.pdf",width=7,height=7)
bmp(file="FiguresPaperBW/bmp/Figure1aBW.bmp",
        width=dims[1],
        height=dims[2],
        units = units, 
        res = res, 
        pointsize = pointsize)
plot(NULL,type="n",xlim=c(12,24),ylim=c(0,100),axes=FALSE,xlab="",ylab="",family="serif")
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
        segments(i+ww,u5[3],i-ww,u5[3],col="white", lwd = 1)
        
        m5 <- fivenum(100*(1-DATAadj$prop_child[DATAadj$SEX == "Female" & DATAadj$AGE == i]))
        rect(i+ww+rgs,m5[2],i-ww+rgs,m5[4],col=gray(.2),border=NA)
        segments(i+rgs,m5[1],i+rgs,m5[5],col=gray(.2), lwd = s.lwd)
        segments(i+ww+rgs,m5[3],i-ww+rgs,m5[3],col="white", lwd = 1)
    }
}

legend(11.4,21,fill=c(gray(.6),gray(.4),gray(.2)),legend=c("student","spouse","mother"),bty="n",border=NA, cex = t.cex)
segments(11.4,0,11.4,100,col=gray(.2), lwd = .8) # y axis frame
segments(11.4,0,24.5,0,col=gray(.2), lwd = .8) # x axis frame
# y axis ticks:
segments(11.4,(0:5)*20,11.3,(0:5)*20,col=gray(.2), lwd = s.lwd, lend = 1)
text(11.4,(0:5)*20,paste((0:5)*20,"%",sep=""),col=gray(.2),pos=2,cex=t.cex,family="serif")
# x axis ticks
segments(12.5:24.5,0,12.5:24.5,-.8,col=gray(.2), lwd = s.lwd, lend = 1)
text(12:24,-2,12:24,pos=1,cex=t.cex,col=gray(.2),family="serif")
# x axis label
text(17.5,-12,"Age", cex = t.cex + .1,col=gray(.2),family="serif")
dev.off()

# --------------------------------------------------------------------------------
# Figure 1b

# similar thing, but for males:
ww <- .09
lfs <- -.23
rgs <- .23
s.lwd <- 1.5
t.cex <- 1.4
#emf(file="Figure1bBW.emf",width=7,height=7)
#pdf(file="Figure1bBW.pdf",width=7,height=7)
bmp(file="FiguresPaperBW/bmp/Figure1bBW.bmp",
        width=dims[1],
        height=dims[2],
        units = units, 
        res = res, 
        pointsize = pointsize)
plot(NULL,type="n",xlim=c(12,24),ylim=c(0,100),axes=FALSE,xlab="",ylab="",family="serif")
par(xpd=TRUE)
for (i in 12:24){
    s5 <- fivenum(100*DATAadj$prop_school[DATAadj$SEX == "Male"  & DATAadj$AGE == i])
    rect(i+lfs+ww,s5[2],i+lfs-ww,s5[4],col=gray(.6),border=NA)
    segments(i+lfs,s5[1],i+lfs,s5[5],col=gray(.6), lwd = s.lwd)
    segments(i+lfs+ww,s5[3],i+lfs-ww,s5[3],col="white", lwd = 1)
    if (i > 14){
        u5 <- fivenum(100*DATAadj$prop_union[DATAadj$SEX == "Male" & DATAadj$AGE == i])
        rect(i+ww,u5[2],i-ww,u5[4],col=gray(.4),border=NA)
        segments(i,u5[1],i,u5[5],col=gray(.4), lwd = s.lwd)
        segments(i+ww,u5[3],i-ww,u5[3],col="white", lwd = 1)
    }
}
legend(11.4,21,fill=c(gray(.6),gray(.4)),legend=c("student","spouse"),bty="n",border=NA, cex = t.cex)
segments(11.4,0,11.4,100,col=gray(.2), lwd = .8) # y axis frame
segments(11.4,0,24.5,0,col=gray(.2), lwd = .8) # x axis frame
# y axis ticks:
segments(11.4,(0:5)*20,11.3,(0:5)*20,col=gray(.2), lwd = s.lwd, lend = 1)
text(11.4,(0:5)*20,paste((0:5)*20,"%",sep=""),col=gray(.2),pos=2,cex=t.cex,family="serif")
# x axis ticks
segments(12.5:24.5,0,12.5:24.5,-.8,col=gray(.2), lwd = s.lwd, lend = 1)
text(12:24,-2,12:24,pos=1,cex=t.cex,col=gray(.2),family="serif")
text(17.5,-12,"Age", cex = t.cex + .1,col=gray(.2),family="serif")
dev.off()

# --------------------------------------------------------------------------------
# Figure 2a (student, spouse)

s.lwd <- 1.5
t.cex <- 1.4
#pdf(file="Figure2aBW.pdf",height=7,width=7)
bmp(file="FiguresPaperBW/bmp/Figure2aBW.bmp",
        width=dims[1],
        height=dims[2],
        units = units, 
        res = res, 
        pointsize = pointsize)
par(xaxs="i",yaxs="i")
plot(NULL,type="n",xlim=c(0,100),ylim=c(0,100),axes=FALSE,xlab="",ylab="",asp=1,col=paste(gray(.5),50,sep=""),family="serif")
# separate polygons and text, due to opacity and overlapping
for (i in c(16,18,20,22,24)){
    x <- 100 * DATAadj$prop_school[DATAadj$SEX=="Female" & DATAadj$AGE == i]
    y <- 100 * DATAadj$prop_union[DATAadj$SEX=="Female"  & DATAadj$AGE == i]
    xnew <- seq(min(x,na.rm=T),max(x,na.rm=T),length.out=200)
    coefs <- Deming(x,y)[1:2]
    CIs <- DemingCI(x,y,xnew)
    polygon(c(xnew,rev(xnew)),c(CIs[,1],rev(CIs[,2])),col=paste(gray(.8),50,sep=""),border=FALSE)
}
# now overplot text to be clear
for (i in c(16,18,20,22,24)){
    x <- 100 * DATAadj$prop_school[DATAadj$SEX=="Female" & DATAadj$AGE == i]
    y <- 100 * DATAadj$prop_union[DATAadj$SEX=="Female"  & DATAadj$AGE == i]
    xnew <- seq(min(x,na.rm=T),max(x,na.rm=T),length.out=200)
    coefs <- Deming(x,y)[1:2]
    CIs <- DemingCI(x,y,xnew)
    segments(xnew[1],coefs[1]+xnew[1]*coefs[2],xnew[200],coefs[1]+xnew[200]*coefs[2],col=gray(.4), lwd = s.lwd)
    text(xnew[1],coefs[1]+xnew[1]*coefs[2],i,cex=t.cex,col=gray(.2),xpd=TRUE,pos=2,family="serif")
}
# y axis
segments(-6,0,-6,100,col = gray(.2), xpd = TRUE, lwd = .8) # frame...
segments(-6,(0:5)*20,-7,(0:5)*20,col=gray(.2),xpd=TRUE, lwd = s.lwd,lend=1)
text(-6,(0:5)*20,paste((0:5)*20,"%",sep=""),pos=2,cex=t.cex,col=gray(.2),xpd=TRUE,family="serif")
# x axis
segments(0,-6,100,-6,col = gray(.2), xpd = TRUE, lwd = .8) # frame...
segments((0:5)*20,-6,(0:5)*20,-7,col=gray(.2),xpd=TRUE, lwd = s.lwd,lend=1)
text((0:5)*20,-8,paste((0:5)*20,"%",sep=""),pos=1,cex=t.cex,col=gray(.2),xpd=TRUE,family="serif")
# axis labels, y at top
text(50,-13,"Student",pos=1,col=gray(.2),xpd=TRUE,family="serif",cex=t.cex)
text(-12,107,"Spouse",col=gray(.2),xpd=TRUE,family="serif",cex=t.cex)

dev.off()

# --------------------------------------------------------------------------------
# Figure 2b (student, spouse) (probably exclude; it doesn't show anything)
s.lwd <- 1.5
t.cex <- 1.4

bmp(file="FiguresPaperBW/bmp/Figure2bBW.bmp",
        width=dims[1],
        height=dims[2],
        units = units, 
        res = res, 
        pointsize = pointsize)
plot(NULL,type="n",xlim=c(0,100),ylim=c(0,100),axes=FALSE,xlab="",ylab="",asp=1,col=paste(gray(.5),50,sep=""),family="serif")
for (i in c(16,18,20,22,24)){
    x <- 100 * DATAadj$prop_school[DATAadj$SEX=="Male" & DATAadj$AGE == i]
    y    <- 100 * DATAadj$prop_union[DATAadj$SEX=="Male"  & DATAadj$AGE == i]
    minx <- min(x,na.rm=T) ; maxx <- max(x,na.rm=T)
    coefs <- Deming(x,y)[1:2]
    xnew <- seq(min(x,na.rm=T),max(x,na.rm=T),length.out=200)
    CIs <- DemingCI(x,y,xnew)
    polygon(c(xnew,rev(xnew)),c(CIs[,1],rev(CIs[,2])),col=paste(gray(.8),50,sep=""),border=FALSE)
}
for (i in c(16,18,20,22,24)){
    x <- 100 * DATAadj$prop_school[DATAadj$SEX=="Male" & DATAadj$AGE == i]
    y    <- 100 * DATAadj$prop_union[DATAadj$SEX=="Male"  & DATAadj$AGE == i]
    minx <- min(x,na.rm=T) ; maxx <- max(x,na.rm=T)
    coefs <- Deming(x,y)[1:2]
    xnew <- seq(min(x,na.rm=T),max(x,na.rm=T),length.out=200)
    CIs <- DemingCI(x,y,xnew)
    segments(xnew[1],coefs[1]+xnew[1]*coefs[2],xnew[200],coefs[1]+xnew[200]*coefs[2],col=gray(.4), lwd = s.lwd)
    text(xnew[1],coefs[1]+xnew[1]*coefs[2],i,cex=t.cex,col=gray(.2),xpd=TRUE,pos=2,family="serif")
}
# y axis
segments(-6,0,-6,100,col = gray(.2), xpd = TRUE, lwd = .8) # frame...
segments(-6,(0:5)*20,-7,(0:5)*20,col=gray(.2),xpd=TRUE, lwd = s.lwd,lend=1)
text(-6,(0:5)*20,paste((0:5)*20,"%",sep=""),pos=2,cex=t.cex,col=gray(.2),xpd=TRUE,family="serif")
# x axis
segments(0,-6,100,-6,col = gray(.2), xpd = TRUE, lwd = .8) # frame...
segments((0:5)*20,-6,(0:5)*20,-7,col=gray(.2),xpd=TRUE, lwd = s.lwd,lend=1)
text((0:5)*20,-8,paste((0:5)*20,"%",sep=""),pos=1,cex=t.cex,col=gray(.2),xpd=TRUE,family="serif")
# axis labels, y at top
text(50,-13,"Student",pos=1,col=gray(.2),xpd=TRUE,family="serif",cex=t.cex)
text(-12,107,"Spouse",col=gray(.2),xpd=TRUE,family="serif",cex=t.cex)

dev.off()

# --------------------------------------------------------------------------------
# Figure 3 (student, mother)
s.lwd <- 1.5
t.cex <- 1.4

bmp(file="FiguresPaperBW/bmp/Figure3BW.bmp",
        width=dims[1],
        height=dims[2],
        units = units, 
        res = res, 
        pointsize = pointsize)
plot(NULL,type="n",xlim=c(0,100),ylim=c(0,100),axes=FALSE,xlab="",ylab="",asp=1,col=paste(gray(.5),50,sep=""),family="serif")
for (i in c(8:12)*2){
    x <- 100 * DATAadj$prop_school[DATAadj$SEX=="Female" & DATAadj$AGE == i]
    y <- 100 * (1-DATAadj$prop_child[DATAadj$SEX=="Female"  & DATAadj$AGE == i])
    minx <- min(x,na.rm=T) ; maxx <- max(x,na.rm=T)
    coefs <- Deming(x,y)[1:2]
    xnew <- seq(min(x,na.rm=T),max(x,na.rm=T),length.out=200)
    CIs <- DemingCI(x,y,xnew)
    polygon(c(xnew,rev(xnew)),c(CIs[,1],rev(CIs[,2])),col=paste(gray(.8),50,sep=""),border=FALSE)
}
for (i in c(8:12)*2){
    x <- 100 * DATAadj$prop_school[DATAadj$SEX=="Female" & DATAadj$AGE == i]
    y <- 100 * (1-DATAadj$prop_child[DATAadj$SEX=="Female"  & DATAadj$AGE == i])
    minx <- min(x,na.rm=T) ; maxx <- max(x,na.rm=T)
    coefs <- Deming(x,y)[1:2]
    xnew <- seq(min(x,na.rm=T),max(x,na.rm=T),length.out=200)
    CIs <- DemingCI(x,y,xnew)
    segments(xnew[1],coefs[1]+xnew[1]*coefs[2],xnew[200],coefs[1]+xnew[200]*coefs[2],col=gray(.4), lwd = s.lwd)
    text(xnew[1] + ifelse(i == 24,5,0),coefs[1]+xnew[1]*coefs[2] + ifelse(i == 24, -2, 0),i,cex=t.cex,col=gray(.2),xpd=TRUE,pos=2,family="serif")
}
# y axis
segments(-6,0,-6,100,col = gray(.2), xpd = TRUE, lwd = .8) # frame...
segments(-6,(0:5)*20,-7,(0:5)*20,col=gray(.2),xpd=TRUE, lwd = s.lwd,lend=1)
text(-6,(0:5)*20,paste((0:5)*20,"%",sep=""),pos=2,cex=t.cex,col=gray(.2),xpd=TRUE,family="serif")
# x axis
segments(0,-6,100,-6,col = gray(.2), xpd = TRUE, lwd = .8) # frame...
segments((0:5)*20,-6,(0:5)*20,-7,col=gray(.2),xpd=TRUE, lwd = s.lwd,lend=1)
text((0:5)*20,-8,paste((0:5)*20,"%",sep=""),pos=1,cex=t.cex,col=gray(.2),xpd=TRUE,family="serif")
# axis labels, y at top
text(50,-13,"Student",pos=1,col=gray(.2),xpd=TRUE,family="serif",cex=t.cex)
text(-12,107,"Mother",col=gray(.2),xpd=TRUE,family="serif",cex=t.cex)
dev.off()

# remove data:
rm(DATAadj)

# --------------------------------------------------------------------------------
# Figure 4 data
load("DATAfigBW/Fig4_DATAadj.Rdata")

# 4a, females
ww <- .09
l1 <- -.3
l2 <- -.1
r1 <- .1
r2 <- .3
s.lwd <- 1.5
t.cex <- 1.4
bmp(file="FiguresPaperBW/bmp/Figure4aBW.bmp",
        width=dims[1],
        height=dims[2],
        units = units, 
        res = res, 
        pointsize = pointsize)
plot(NULL,type="n",xlim=c(15,24),ylim=c(0,100),axes=FALSE,xlab="Age",ylab="",family="serif")
par(xpd=TRUE)
for (i in 15:24){
    # in school
    u51 <- fivenum(100*DATAadj$prop_union2[DATAadj$SEX == "Female" & DATAadj$AGE == i & DATAadj$SCHOOL == 1])
    rect(i+ww+l1,u51[2],i-ww+l1,u51[4],col=gray(.7),border=NA)
    segments(i+l1,u51[1],i+l1,u51[5],col=gray(.7),lwd=s.lwd)
    segments(i+l1+ww,u51[3],i+l1-ww,u51[3],col="white",lwd=1)
    
    m51 <- fivenum(100*(1-DATAadj$childless2[DATAadj$SEX == "Female" & DATAadj$AGE == i& DATAadj$SCHOOL == 1]))
    rect(i+ww+l2,m51[2],i-ww+l2,m51[4],col=gray(.5),border=NA)
    segments(i+l2,m51[1],i+l2,m51[5],col=gray(.5),lwd=s.lwd)
    segments(i+ww+l2,m51[3],i-ww+l2,m51[3],col="white",lwd=1)
    
    # not in school
    u51 <- fivenum(100*DATAadj$prop_union2[DATAadj$SEX == "Female" & DATAadj$AGE == i & DATAadj$SCHOOL == 0])
    rect(i+ww+r1,u51[2],i-ww+r1,u51[4],col=gray(.3),border=NA)
    segments(i+r1,u51[1],i+r1,u51[5],col=gray(.3),lwd=s.lwd)
    segments(i+r1+ww,u51[3],i+r1-ww,u51[3],col="white",lwd=1)
    
    m51 <- fivenum(100*(1-DATAadj$childless2[DATAadj$SEX == "Female" & DATAadj$AGE == i& DATAadj$SCHOOL == 0]))
    rect(i+ww+r2,m51[2],i-ww+r2,m51[4],col=gray(.15),border=NA)
    segments(i+r2,m51[1],i+r2,m51[5],col=gray(.15),lwd=s.lwd)
    segments(i+ww+r2,m51[3],i-ww+r2,m51[3],col="white",lwd=1)
}
legend(14.4,110,fill=gray(c(.7,.5,.3,.15)),
        legend=c("student spouse","student mother","spouse not in school","mother not in school"),
        bty="n",border=NA, cex = t.cex, xpd = TRUE)

segments(14.4,0,14.4,100,col=gray(.2),lwd=.8) # y axis frame
segments(14.4,0,24.5,0,col=gray(.2),lwd=.8) # x axis frame
# y axis ticks:
segments(14.4,(0:5)*20,14.33,(0:5)*20,col=gray(.2),lwd=s.lwd, lend = 1)
text(14.4,(0:5)*20,paste((0:5)*20,"%",sep=""),col=gray(.2),pos=2,cex=t.cex,family="serif")
segments(14.5:24.5,0,14.5:24.5,-.8,col=gray(.2),lwd=s.lwd, lend = 1)
text(15:24,-2,15:24,pos=1,cex=t.cex,col=gray(.2),family="serif")
dev.off()

# 4b, males
ww <- .09
l <- -.1
r <- .1
s.lwd = 1.5
t.cex <- 1.4
bmp(file="FiguresPaperBW/bmp/Figure4bBW.bmp",
        width=dims[1],
        height=dims[2],
        units = units, 
        res = res, 
        pointsize = pointsize)
plot(NULL,type="n",xlim=c(15,24),ylim=c(0,100),axes=FALSE,xlab="Age",ylab="",family="serif")
par(xpd=TRUE)
for (i in 15:24){
    # union in school
    u51 <- fivenum(100*DATAadj$prop_union2[DATAadj$SEX == "Male" & DATAadj$AGE == i & DATAadj$SCHOOL == 1])
    rect(i+ww+l,u51[2],i-ww+l,u51[4],col=gray(.7),border=NA)
    segments(i+l,u51[1],i+l,u51[5],col=gray(.7),lwd=s.lwd)
    segments(i+l+ww,u51[3],i+l-ww,u51[3],col="white",lwd=1)
    # union not in school
    m51 <- fivenum(100*DATAadj$prop_union2[DATAadj$SEX == "Male" & DATAadj$AGE == i& DATAadj$SCHOOL == 0])
    rect(i+ww+r,m51[2],i-ww+r,m51[4],col=gray(.3),border=NA)
    segments(i+r,m51[1],i+r,m51[5],col=gray(.3),lwd=s.lwd)
    segments(i+ww+r,m51[3],i-ww+r,m51[3],col="white",lwd=1)
}
legend(14.4,110,fill=gray(c(.7,.3)),legend=c("student spouse","spouse not in school"),bty="n",border=NA, cex = t.cex)

segments(14.4,0,14.4,100,col=gray(.2),lwd=.8) # y axis frame
segments(14.4,0,24.5,0,col=gray(.2),lwd=.8) # x axis frame
# y axis ticks:
segments(14.4,(0:5)*20,14.33,(0:5)*20,col=gray(.2),lwd=s.lwd, lend = 1)
text(14.4,(0:5)*20,paste((0:5)*20,"%",sep=""),col=gray(.2),pos=2,cex=t.cex,family="serif")
segments(14.5:24.5,0,14.5:24.5,-.8,col=gray(.2),lwd=s.lwd, lend = 1)
text(15:24,-2,15:24,pos=1,cex=t.cex,col=gray(.2),family="serif")
dev.off()

rm(DATAadj)
# --------------------------------------------------------------------------------
# Figure 5 data
load("DATAfigBW/Fig5_DATAadj.Rdata")

s.lwd <- 1.5
t.cex <- 1.4

bmp(file="FiguresPaperBW/bmp/Figure5BW.bmp",
        width=dims[1],
        height=dims[2],
        units = units, 
        res = res, 
        pointsize = pointsize)

par(xaxs="i",yaxs="i")
plot(NULL,type="n",xlim=c(0,100),ylim=c(0,100),axes=FALSE,xlab="",ylab="",asp=1)
for (i in c(16,18,20,22,24)){
    y <- 100 *  (1-DATAadj$prop_childless[DATAadj$AGE == i])
    x <- 100 * (1-DATAadj$prop_childless_att[DATAadj$AGE == i])
    minx <- min(x,na.rm=T) ; maxx <- max(x,na.rm=T)
    coefs <- Deming(x,y)[1:2]
    xnew <- seq(min(x,na.rm=T),max(x,na.rm=T),length.out=200)
    CIs <- DemingCI(x,y,xnew)
    polygon(c(xnew,rev(xnew)),c(CIs[,1],rev(CIs[,2])),col=paste(gray(.8),50,sep=""),border=FALSE)
}
for (i in c(16,18,20,22,24)){
    y <- 100 *  (1-DATAadj$prop_childless[DATAadj$AGE == i])
    x <- 100 * (1-DATAadj$prop_childless_att[DATAadj$AGE == i])
    minx <- min(x,na.rm=T) ; maxx <- max(x,na.rm=T)
    coefs <- Deming(x,y)[1:2]
    xnew <- seq(min(x,na.rm=T),max(x,na.rm=T),length.out=200)
    CIs <- DemingCI(x,y,xnew)
    segments(xnew[1],coefs[1]+xnew[1]*coefs[2],xnew[200],coefs[1]+xnew[200]*coefs[2],col=gray(.4), lwd = s.lwd)
    text(xnew[1]+.4,coefs[1]+xnew[1]*coefs[2],i,cex=t.cex,col=gray(.2),xpd=TRUE,pos=2,family="serif")
}
# y axis
segments(-6,0,-6,100,col = gray(.2), xpd = TRUE, lwd = .8) # frame...
segments(-6,(0:5)*20,-7,(0:5)*20,col=gray(.2),xpd=TRUE, lwd = s.lwd,lend=1)
text(-6,(0:5)*20,paste((0:5)*20,"%",sep=""),pos=2,cex=t.cex,col=gray(.2),xpd=TRUE,family="serif")
# x axis
segments(0,-6,100,-6,col = gray(.2), xpd = TRUE, lwd = .8) # frame...
segments((0:5)*20,-6,(0:5)*20,-7,col=gray(.2),xpd=TRUE, lwd = s.lwd,lend=1)
text((0:5)*20,-8,paste((0:5)*20,"%",sep=""),pos=1,cex=t.cex,col=gray(.2),xpd=TRUE,family="serif")


text(50,-13,"% Mother of female students",pos=1,col=gray(.2),xpd=TRUE,family="serif", cex = t.cex)
text(-8,109.5,"% Mother of\nall females",col=gray(.2),xpd=TRUE,family="serif", cex = t.cex)

dev.off()

rm(DATAadj)
# --------------------------------------------------------------------------------
# Figure 6 data
load("DATAfigBW/Fig6_DATAadj.Rdata")

s.lwd <- 1.5
t.cex <- 1.4

bmp(file="FiguresPaperBW/bmp/Figure6BW.bmp",
        width=dims[1],
        height=dims[2],
        units = units, 
        res = res, 
        pointsize = pointsize)

plot(NULL,type="n",xlim=c(0,100),ylim=c(0,100),axes=FALSE,xlab="",ylab="",asp=1)
for (i in c(16,18,20,22)){
    x <- 100 *  DATAadj$prop_union[DATAadj$AGE == i & DATAadj$SEX == "Female"]
    y <- 100 * DATAadj$prop_union_att[DATAadj$AGE == i & DATAadj$SEX == "Female"]
    minx <- min(x,na.rm=T) ; maxx <- max(x,na.rm=T)
    coefs <- Deming(x,y)[1:2]
    xnew <- seq(min(x,na.rm=T),max(x,na.rm=T),length.out=200)
    CIs <- DemingCI(x,y,xnew)
    polygon(c(xnew,rev(xnew)),c(CIs[,1],rev(CIs[,2])),col=paste(gray(.8),50,sep=""),border=FALSE)
}
for (i in c(16,18,20,22)){
    x <- 100 *  DATAadj$prop_union[DATAadj$AGE == i & DATAadj$SEX == "Female"]
    y <- 100 * DATAadj$prop_union_att[DATAadj$AGE == i & DATAadj$SEX == "Female"]
    minx <- min(x,na.rm=T) ; maxx <- max(x,na.rm=T)
    coefs <- Deming(x,y)[1:2]
    xnew <- seq(min(x,na.rm=T),max(x,na.rm=T),length.out=200)
    CIs <- DemingCI(x,y,xnew)
    segments(xnew[1],coefs[1]+xnew[1]*coefs[2],xnew[200],coefs[1]+xnew[200]*coefs[2],col=gray(.4), lwd = s.lwd)
    text(xnew[200]+ ifelse(i == 16, -1, 0),coefs[1]+xnew[200]*coefs[2] + ifelse(i == 16, -4, 0),i,cex=t.cex,col=gray(.2),xpd=TRUE,pos=4,family="serif")
}
# y axis
segments(-6,0,-6,100,col = gray(.2), xpd = TRUE, lwd = .8) # frame...
segments(-6,(0:5)*20,-7,(0:5)*20,col=gray(.2),xpd=TRUE, lwd = s.lwd,lend=1)
text(-6,(0:5)*20,paste((0:5)*20,"%",sep=""),pos=2,cex=t.cex,col=gray(.2),xpd=TRUE,family="serif")
# x axis
segments(0,-6,100,-6,col = gray(.2), xpd = TRUE, lwd = .8) # frame...
segments((0:5)*20,-6,(0:5)*20,-7,col=gray(.2),xpd=TRUE, lwd = s.lwd,lend=1)
text((0:5)*20,-8,paste((0:5)*20,"%",sep=""),pos=1,cex=t.cex,col=gray(.2),xpd=TRUE,family="serif")

text(-8,109.5,"% Spouse of\nfemale students",col=gray(.2),xpd=TRUE,family="serif", cex = t.cex)
text(50,-13,"% Spouse of all females",col=gray(.2),xpd=TRUE,family="serif", cex = t.cex, pos = 1)

dev.off()

rm(DATAadj)
# --------------------------------------------------------------------------------
# Figure 7 data
load("DATAfigBW/Fig7_DATAadj.Rdata")

s.lwd <- 1.5
t.cex <- 1.4

bmp(file="FiguresPaperBW/bmp/Figure7BW.bmp",
        width=dims[1],
        height=dims[2],
        units = units, 
        res = res, 
        pointsize = pointsize)

par(xaxs="i",yaxs="i")
plot(NULL,type="n",xlim=c(0,100),ylim=c(0,100),axes=FALSE,xlab="",ylab="",asp=1)
x   <- 100 * DATAadj$prop_child
y1  <- 100 * DATAadj$withchild1
y2  <- 100 * DATAadj$withchild2
minx <- min(x,na.rm=T) ; maxx <- max(x,na.rm=T)
# ---------------------------------------------
# CIs first
# for primary school:
coefs1 <- Deming(x,y1)[1:2]
xnew <- seq(min(x,na.rm=T),max(x,na.rm=T),length.out=200)
CIs <- DemingCI(x,y1,xnew)
polygon(c(xnew,rev(xnew)),c(CIs[,1],rev(CIs[,2])),col=paste(gray(.8),50,sep=""),border=FALSE)
# for secondary school:
coefs2 <- Deming(x,y2)[1:2]
xnew <- seq(min(x,na.rm=T),max(x,na.rm=T),length.out=200)
CIs <- DemingCI(x,y2,xnew)
polygon(c(xnew,rev(xnew)),c(CIs[,1],rev(CIs[,2])),col=paste(gray(.8),50,sep=""),border=FALSE)
# ---------------------------------------------
# now points, segments, text
coefs1 <- Deming(x,y1)[1:2]
xnew <- seq(min(x,na.rm=T),max(x,na.rm=T),length.out=200)
CIs <- DemingCI(x,y1,xnew)
segments(xnew[1],coefs1[1]+xnew[1]*coefs1[2],xnew[200],coefs1[1]+xnew[200]*coefs1[2],col=gray(.4))
text(xnew[200],coefs1[1]+xnew[200]*coefs1[2],paste("primary (   )\nslope =",round(coefs1[2],3)),
        cex=t.cex,col=gray(.2),xpd=TRUE,pos=4,family="serif")
points(95.8,74.4,pch=19,col=gray(.4)) # circle
# for secondary school:
coefs2 <- Deming(x,y2)[1:2]
xnew <- seq(min(x,na.rm=T),max(x,na.rm=T),length.out=200)
CIs <- DemingCI(x,y2,xnew)
segments(xnew[1],coefs2[1]+xnew[1]*coefs2[2],xnew[200],coefs2[1]+xnew[200]*coefs2[2],col=gray(.4))

text(xnew[200],coefs2[1]+xnew[200]*coefs2[2],paste("secondary+  (   )\nslope =",round(coefs2[2],3)),
        cex=t.cex,col=gray(.2),xpd=TRUE,pos=4,family="serif")
points(102.7,31,pch=2,col=gray(.5)) # triangle
# ---------------------------------------------
# y axis
segments(-6,0,-6,100,col = gray(.2), xpd = TRUE, lwd = .8) # frame...
segments(-6,(0:5)*20,-7,(0:5)*20,col=gray(.2),xpd=TRUE, lwd = s.lwd,lend=1)
text(-6,(0:5)*20,paste((0:5)*20,"%",sep=""),pos=2,cex=t.cex,col=gray(.2),xpd=TRUE,family="serif")
# x axis
segments(0,-6,100,-6,col = gray(.2), xpd = TRUE, lwd = .8) # frame...
segments((0:5)*20,-6,(0:5)*20,-7,col=gray(.2),xpd=TRUE, lwd = s.lwd,lend=1)
text((0:5)*20,-8,paste((0:5)*20,"%",sep=""),pos=1,cex=t.cex,col=gray(.2),xpd=TRUE,family="serif")

# plot points on top of confidence regions:
points(x,y1,pch=19,col=gray(.4),xpd=TRUE)
points(x,y2,pch=2,col=gray(.5),xpd=TRUE)

text(-6.5,109,"% Mother of\nfemale students",col=gray(.2),xpd=TRUE,family="serif", cex = t.cex)
text(50,-13,"% Mother of all females",col=gray(.2),xpd=TRUE,family="serif", cex = t.cex, pos = 1)
dev.off()
