 
# Author: triffe
###############################################################################
#install.packages("Cairo")
# Data Prep is done separately.
# Data to be found in folder DATAfigBW in Rdata binaries, already adjusted
# (smoothed for age heaping in some cases), already in format rquired for figure
# syntax

# this syntax will aim to create all plots 9x9 cm in bmp format
setwd("/home/triffe/git/ViennaDiagnostics/ALBERTPAPERS/Figures")
Nboot <- 1000
# load some functions, packages:
library(MethComp)
# custom function for returning CIs
DemingCI <- function(x,y,xnew,level=95,Nboot){
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
# --------------------------------------------------------------------------------
# Figure 1a
{
load("DATAfigBW/Fig1_DATAadj.Rdata")

# females boxplot, Tufte-style
ww <- .1
lfs <- -.25
rgs <- .25
s.lwd <- .5

# 12 point font by default. for 9, use 3/4 , for 8 use 2/3  

res <- 600
png(file="FiguresPaperBW/final/Figure1aBW.png",
        width=2.67,
        height=2.67,
        units = "in", 
        res = res,
        pointsize = 6)

plot(NULL,type="n",xlim=c(12,24),ylim=c(0,100),axes=FALSE,xlab="",ylab="",family="sans")
par(xpd=TRUE, mar = c(.5,.1,05,.05), ps = 12)
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
# legend by hand:
a <- par("usr")
xd <- a[2]-a[1]
yd <- a[4]-a[3]
ratio <- yd/xd
rect(c(10,14,18),-25,c(10,14,18)+3/ratio,-28,col=c(gray(.6),gray(.4),gray(.2)), xpd =TRUE, border = NA)
text(c(10,14,18)+3/ratio ,-26.5,c("Student","Spouse","Mother"),family = "sans",pos=4,cex=(3/4))
segments(11.4,0,11.4,100,col=gray(.2), lwd = .8) # y axis frame
segments(11.4,0,24.5,0,col=gray(.2), lwd = .8) # x axis frame
# y axis ticks:
segments(11.4,(0:5)*20,11.3,(0:5)*20,col=gray(.2), lwd = s.lwd, lend = 1)
text(11.4,(0:5)*20,paste((0:5)*20,"%",sep=""),col=gray(.2),pos=2,cex=(2/3),family="sans")
# x axis ticks
segments(12.5:24.5,0,12.5:24.5,-.8,col=gray(.2), lwd = s.lwd, lend = 1)
text(12:24,-2,12:24,pos=1,cex=(2/3),col=gray(.2),family="sans")
# x axis label
text(17.5,-17,"Age", cex = (3/4),col=gray(.2),family="sans")
dev.off()
}
# --------------------------------------------------------------------------------
# Figure 1b
{
# similar thing, but for males:
ww <- .1
lfs <- -.25
rgs <- .25
s.lwd <- .5

png(file="FiguresPaperBW/final/Figure1bBW.png",
        width=2.67,
        height=2.67,
        units = "in", 
        res = res, 
        pointsize = 6)
plot(NULL,type="n",xlim=c(12,24),ylim=c(0,100),axes=FALSE,xlab="",ylab="",family="sans")
par(xpd=TRUE, mar = c(.5,.1,05,.05), ps = 12)
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
#legend(11.4,21,fill=c(gray(.6),gray(.4)),legend=c("Student","Spouse"),bty="n",border=NA, cex = t.cex)
a <- par("usr")
xd <- a[2]-a[1]
yd <- a[4]-a[3]
ratio <- yd/xd
rect(c(10,14),-25,c(10,14)+3/ratio,-28,col=c(gray(.6),gray(.4)), xpd =TRUE, border = NA)
text(c(10,14)+3/ratio ,-26.5,c("Student","Spouse"),family = "sans",pos=4,cex=(3/4))
segments(11.4,0,11.4,100,col=gray(.2), lwd = .8) # y axis frame

segments(11.4,0,24.5,0,col=gray(.2), lwd = .8) # x axis frame
# y axis ticks:
segments(11.4,(0:5)*20,11.3,(0:5)*20,col=gray(.2), lwd = s.lwd, lend = 1)
text(11.4,(0:5)*20,paste((0:5)*20,"%",sep=""),col=gray(.2),pos=2,cex=(2/3),family="sans")
# x axis ticks
segments(12.5:24.5,0,12.5:24.5,-.8,col=gray(.2), lwd = s.lwd, lend = 1)
text(12:24,-2,12:24,pos=1,cex=(2/3),col=gray(.2),family="sans")
text(17.5,-17,"Age", cex = (3/4),col=gray(.2),family="sans")
dev.off()
}
# --------------------------------------------------------------------------------
# Figure 2 (student, spouse)
{
s.lwd <- 1.5

#pdf(file="Figure2aBW.pdf",height=7,width=7)
png(file="FiguresPaperBW/final/Figure2BW.png",
        width=2.7,
        height=2.7,
        units = "in", 
        res = res, 
        pointsize = 6)
par(xaxs="i",yaxs="i",ps=12, mar = c(5,4,3,.5))
plot(NULL,type="n",xlim=c(0,100),ylim=c(0,100),axes=FALSE,xlab="",ylab="",
        asp=1,col=paste(gray(.5),50,sep=""),family="sans")
# separate polygons and text, due to opacity and overlapping
for (i in c(16,18,20,22,24)){
    x <- 100 * DATAadj$prop_school[DATAadj$SEX=="Female" & DATAadj$AGE == i]
    y <- 100 * DATAadj$prop_union[DATAadj$SEX=="Female"  & DATAadj$AGE == i]
    xnew <- seq(min(x,na.rm=T),max(x,na.rm=T),length.out=200)
    coefs <- Deming(x,y)[1:2]
    CIs <- DemingCI(x,y,xnew,Nboot = Nboot)
    polygon(c(xnew,rev(xnew)),c(CIs[,1],rev(CIs[,2])),col=paste(gray(.8),50,sep=""),border=FALSE)
}
# now overplot text to be clear
for (i in c(16,18,20,22,24)){
    x <- 100 * DATAadj$prop_school[DATAadj$SEX=="Female" & DATAadj$AGE == i]
    y <- 100 * DATAadj$prop_union[DATAadj$SEX=="Female"  & DATAadj$AGE == i]
    xnew <- seq(min(x,na.rm=T),max(x,na.rm=T),length.out=200)
    coefs <- Deming(x,y)[1:2]
    CIs <- DemingCI(x,y,xnew,Nboot = Nboot)
    segments(xnew[1],coefs[1]+xnew[1]*coefs[2],xnew[200],coefs[1]+xnew[200]*coefs[2],col=gray(.4), lwd = s.lwd)
    text(xnew[1]+.5,coefs[1]+xnew[1]*coefs[2],i,cex=(3/4),col=gray(.2),xpd=TRUE,pos=2,family="sans")
}
# y axis
segments(-10,0,-10,100,col = gray(.2), xpd = TRUE, lwd = .8) # frame...
segments(-10,(0:5)*20,-11,(0:5)*20,col=gray(.2),xpd=TRUE, lwd = s.lwd,lend=1)
text(-10,(0:5)*20,paste((0:5)*20,"%",sep=""),pos=2,cex=(2/3),col=gray(.2),xpd=TRUE,family="sans")
# x axis
segments(0,-6,100,-6,col = gray(.2), xpd = TRUE, lwd = .8) # frame...
segments((0:5)*20,-6,(0:5)*20,-7,col=gray(.2),xpd=TRUE, lwd = s.lwd,lend=1)
text((0:5)*20,-8,paste((0:5)*20,"%",sep=""),pos=1,cex=(2/3),col=gray(.2),xpd=TRUE,family="sans")
# axis labels, y at top
text(50,-16,"Student",pos=1,col=gray(.2),xpd=TRUE,family="sans",cex=(3/4))
text(-12,109,"Spouse",col=gray(.2),xpd=TRUE,family="sans",cex=(3/4))

dev.off()
}
# --------------------------------------------------------------------------------
# Figure 3 (student, mother)
{
s.lwd <- 1.5

png(file="FiguresPaperBW/final/Figure3BW.png",
        width=2.7,
        height=2.7,
        units = "in", 
        res = res, 
        pointsize = 6)
par(xaxs="i",yaxs="i",ps=12, mar = c(5,4,3,.5))
plot(NULL,type="n",xlim=c(0,100),ylim=c(0,100),axes=FALSE,xlab="",ylab="",asp=1,
        col=paste(gray(.5),50,sep=""),family="sans")

for (i in c(8:12)*2){
    x <- 100 * DATAadj$prop_school[DATAadj$SEX=="Female" & DATAadj$AGE == i]
    y <- 100 * (1-DATAadj$prop_child[DATAadj$SEX=="Female"  & DATAadj$AGE == i])
    minx <- min(x,na.rm=T) ; maxx <- max(x,na.rm=T)
    coefs <- Deming(x,y)[1:2]
    xnew <- seq(min(x,na.rm=T),max(x,na.rm=T),length.out=200)
    CIs <- DemingCI(x,y,xnew, Nboot = Nboot)
    polygon(c(xnew,rev(xnew)),c(CIs[,1],rev(CIs[,2])),col=paste(gray(.8),50,sep=""),border=FALSE)
}
for (i in c(8:12)*2){
    x <- 100 * DATAadj$prop_school[DATAadj$SEX=="Female" & DATAadj$AGE == i]
    y <- 100 * (1-DATAadj$prop_child[DATAadj$SEX=="Female"  & DATAadj$AGE == i])
    minx <- min(x,na.rm=T) ; maxx <- max(x,na.rm=T)
    coefs <- Deming(x,y)[1:2]
    xnew <- seq(min(x,na.rm=T),max(x,na.rm=T),length.out=200)
    CIs <- DemingCI(x,y,xnew, Nboot = Nboot)
    segments(xnew[1],coefs[1]+xnew[1]*coefs[2],xnew[200],coefs[1]+xnew[200]*coefs[2],col=gray(.4), lwd = s.lwd)
    text(xnew[1]+.5 + ifelse(i == 24,5,0),coefs[1]+xnew[1]*coefs[2] + ifelse(i == 24, -2, 0),
            i,cex=(3/4),col=gray(.2),xpd=TRUE,pos=2,family="sans")
}
# y axis
segments(-10,0,-10,100,col = gray(.2), xpd = TRUE, lwd = .8) # frame...
segments(-10,(0:5)*20,-11,(0:5)*20,col=gray(.2),xpd=TRUE, lwd = s.lwd,lend=1)
text(-10,(0:5)*20,paste((0:5)*20,"%",sep=""),pos=2,cex=(2/3),col=gray(.2),xpd=TRUE,family="sans")
# x axis
segments(0,-6,100,-6,col = gray(.2), xpd = TRUE, lwd = .8) # frame...
segments((0:5)*20,-6,(0:5)*20,-7,col=gray(.2),xpd=TRUE, lwd = s.lwd,lend=1)
text((0:5)*20,-8,paste((0:5)*20,"%",sep=""),pos=1,cex=(2/3),col=gray(.2),xpd=TRUE,family="sans")
# axis labels, y at top
text(50,-16,"Student",pos=1,col=gray(.2),xpd=TRUE,family="sans",cex=(3/4))
text(-16,109,"Mother",col=gray(.2),xpd=TRUE,family="sans",cex=(3/4))
dev.off()

# remove data:
rm(DATAadj)
}
# --------------------------------------------------------------------------------
# Figure 4 data
{
load("DATAfigBW/Fig4_DATAadj.Rdata")

# 4a, females
ww <- .09
l1 <- -.3
l2 <- -.1
r1 <- .1
r2 <- .3
s.lwd <- 1.5

# bmp for paper
png(file="FiguresPaperBW/final/Figure4aBW.png",
        width=2.7,
        height=2.7,
        units = "in", 
        res = res, 
        pointsize = 6)
plot(NULL,type="n",xlim=c(15,24),ylim=c(-15,85),axes=FALSE,xlab="",ylab="",family="sans")
par(xpd=TRUE, mar = c(3,2,0,.05), ps = 12)
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
#legend(14.4,110,fill=gray(c(.7,.5,.3,.15)),
#        legend=c("Student spouse","Student mother","Spouse not in school","Mother not in school"),
#        bty="n",border=NA, cex = t.cex, xpd = TRUE)
a <- par("usr")
xd <- a[2]-a[1]
yd <- a[4]-a[3]
ratio <- yd/xd
yoff <- -22-seq(0,by=7,length.out = 4)
rect(13, yoff,13+3/ratio,yoff-3, col = gray(c(.7,.5,.3,.15)), border = NA)
text(13+3/ratio,yoff-1.5,
        c("in union; in school","has a child; in school","in union; not in school","has a child; not in school"),
        pos = 4, family = "sans", cex = (3/4))
segments(14.4,0,14.4,100,col=gray(.2),lwd=.8) # y axis frame
segments(14.4,0,24.5,0,col=gray(.2),lwd=.8) # x axis frame
# y axis ticks:
segments(14.4,(0:5)*20,14.33,(0:5)*20,col=gray(.2),lwd=s.lwd, lend = 1)
text(14.4,(0:5)*20,paste((0:5)*20,"%",sep=""),col=gray(.2),pos=2,cex=(2/3),family="sans")
segments(14.5:24.5,0,14.5:24.5,-.8,col=gray(.2),lwd=s.lwd, lend = 1)
text(15:24,-2,15:24,pos=1,cex=(2/3),col=gray(.2),family="sans")
text(19.5,-17,"Age", cex = (3/4) + .1,col=gray(.2),family="sans")
dev.off()

# pdf for cover
pdf(file="FiguresPaperBW/final/Figure4aBW.pdf",
        width=5,
        height=5,  
        pointsize = 12)
plot(NULL,type="n",xlim=c(15,24),ylim=c(0,100),axes=FALSE,xlab="",ylab="",family="sans")
par(xpd=TRUE, mar = c(5,2,.5,.5), ps = 12)
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
#legend(14.4,110,fill=gray(c(.7,.5,.3,.15)),
#        legend=c("Student spouse","Student mother","Spouse not in school","Mother not in school"),
#        bty="n",border=NA, cex = t.cex, xpd = TRUE)
a <- par("usr")
xd <- a[2]-a[1]
yd <- a[4]-a[3]
ratio <- yd/xd
rect(c(13,19,13,19), c(-25,-25,-32,-32),c(13,19,13,19)+3/ratio,c(-28,-28,-35,-35), col = gray(c(.7,.5,.3,.15)), border = NA)
text(c(13,19,13,19)+3/ratio,c(-26.5,-26.5,-33.5,-33.5),
        c("in union; in school","has a child; in school","in union; not in school","has a child; not in school"),
        pos = 4, family = "sans", cex = (3/4))
segments(14.4,0,14.4,100,col=gray(.2),lwd=.8) # y axis frame
segments(14.4,0,24.5,0,col=gray(.2),lwd=.8) # x axis frame
# y axis ticks:
segments(14.4,(0:5)*20,14.33,(0:5)*20,col=gray(.2),lwd=s.lwd, lend = 1)
text(14.4,(0:5)*20,paste((0:5)*20,"%",sep=""),col=gray(.2),pos=2,cex=(2/3),family="sans")
segments(14.5:24.5,0,14.5:24.5,-.8,col=gray(.2),lwd=s.lwd, lend = 1)
text(15:24,-2,15:24,pos=1,cex=(2/3),col=gray(.2),family="sans")
text(19.5,-17,"Age", cex = (3/4) + .1,col=gray(.2),family="sans")
dev.off()

rm(DATAadj)
}

# --------------------------------------------------------------------------------
# Figure 5 data
{
load("DATAfigBW/Fig5_DATAadj.Rdata")

s.lwd <- 1.5

png(file="FiguresPaperBW/final/Figure5BW.png",
        width=2.7,
        height=2.7,
        units = "in", 
        res = res, 
        pointsize = 6)

par(xaxs="i",yaxs="i",ps=12, mar = c(5,4,4,.5))
plot(NULL,type="n",xlim=c(0,100),ylim=c(0,100),axes=FALSE,xlab="",ylab="",asp=1)
for (i in c(16,18,20,22,24)){
    y <- 100 *  (1-DATAadj$prop_childless[DATAadj$AGE == i])
    x <- 100 * (1-DATAadj$prop_childless_att[DATAadj$AGE == i])
    minx <- min(x,na.rm=T) ; maxx <- max(x,na.rm=T)
    coefs <- Deming(x,y)[1:2]
    xnew <- seq(min(x,na.rm=T),max(x,na.rm=T),length.out=200)
    CIs <- DemingCI(x,y,xnew, Nboot=Nboot)
    polygon(c(xnew,rev(xnew)),c(CIs[,1],rev(CIs[,2])),col=paste(gray(.8),50,sep=""),border=FALSE)
}
for (i in c(16,18,20,22,24)){
    y <- 100 *  (1-DATAadj$prop_childless[DATAadj$AGE == i])
    x <- 100 * (1-DATAadj$prop_childless_att[DATAadj$AGE == i])
    minx <- min(x,na.rm=T) ; maxx <- max(x,na.rm=T)
    coefs <- Deming(x,y)[1:2]
    xnew <- seq(min(x,na.rm=T),max(x,na.rm=T),length.out=200)
    CIs <- DemingCI(x,y,xnew, Nboot = Nboot)
    segments(xnew[1],coefs[1]+xnew[1]*coefs[2],xnew[200],coefs[1]+xnew[200]*coefs[2],col=gray(.4), lwd = s.lwd)
    text(xnew[1]+1,coefs[1]+xnew[1]*coefs[2],i,cex=(3/4),col=gray(.2),xpd=TRUE,pos=2,family="sans")
}
# y axis
segments(-11,0,-11,100,col = gray(.2), xpd = TRUE, lwd = .8) # frame...
segments(-11,(0:5)*20,-12,(0:5)*20,col=gray(.2),xpd=TRUE, lwd = s.lwd,lend=1)
text(-11,(0:5)*20,paste((0:5)*20,"%",sep=""),pos=2,cex=(2/3),col=gray(.2),xpd=TRUE,family="sans")
# x axis
segments(0,-6,100,-6,col = gray(.2), xpd = TRUE, lwd = .8) # frame...
segments((0:5)*20,-6,(0:5)*20,-7,col=gray(.2),xpd=TRUE, lwd = s.lwd,lend=1)
text((0:5)*20,-8,paste((0:5)*20,"%",sep=""),pos=1,cex=(2/3),col=gray(.2),xpd=TRUE,family="sans")

text(50,-18,"% mothers among female students",pos=1,col=gray(.2),xpd=TRUE,family="sans", cex = (3/4))
text(-8,113,"% mother of\nall females",col=gray(.2),xpd=TRUE,family="sans", cex = (3/4))

dev.off()

rm(DATAadj)
}
# --------------------------------------------------------------------------------
# Figure 6 data
{
load("DATAfigBW/Fig6_DATAadj.Rdata")

s.lwd <- 1.5

png(file="FiguresPaperBW/final/Figure6BW.png",
        width=2.7,
        height=2.7,
        units = "in", 
        res = res, 
        pointsize = 6)
par(xaxs="i",yaxs="i",ps=12, mar = c(5,4,4,.5))
plot(NULL,type="n",xlim=c(0,100),ylim=c(0,100),axes=FALSE,xlab="",ylab="",asp=1)
for (i in c(16,18,20,22)){
    x <- 100 * DATAadj$prop_union[DATAadj$AGE == i & DATAadj$SEX == "Female"]
    y <- 100 * DATAadj$prop_union_att[DATAadj$AGE == i & DATAadj$SEX == "Female"]
    minx <- min(x,na.rm=T) ; maxx <- max(x,na.rm=T)
    coefs <- Deming(x,y)[1:2]
    xnew <- seq(min(x,na.rm=T),max(x,na.rm=T),length.out=200)
    CIs <- DemingCI(x,y,xnew,Nboot=Nboot)
    polygon(c(xnew,rev(xnew)),c(CIs[,1],rev(CIs[,2])),col=paste(gray(.8),50,sep=""),border=FALSE)
}
for (i in c(16,18,20,22)){100
    x <- 100 *  DATAadj$prop_union[DATAadj$AGE == i & DATAadj$SEX == "Female"]
    y <- 100 * DATAadj$prop_union_att[DATAadj$AGE == i & DATAadj$SEX == "Female"]
    minx <- min(x,na.rm=T) ; maxx <- max(x,na.rm=T)
    coefs <- Deming(x,y)[1:2]
    xnew <- seq(min(x,na.rm=T),max(x,na.rm=T),length.out=200)
    CIs <- DemingCI(x,y,xnew,Nboot=Nboot)
    segments(xnew[1],coefs[1]+xnew[1]*coefs[2],xnew[200],coefs[1]+xnew[200]*coefs[2],col=gray(.4), lwd = s.lwd)
    text(xnew[200]+ ifelse(i == 16, -1, 0),coefs[1]+xnew[200]*coefs[2] + ifelse(i == 16, -4, 0),
            i,cex=(3/4),col=gray(.2),xpd=TRUE,pos=4,family="sans")
}
# y axis
segments(-6,0,-6,100,col = gray(.2), xpd = TRUE, lwd = .8) # frame...
segments(-6,(0:5)*20,-7,(0:5)*20,col=gray(.2),xpd=TRUE, lwd = s.lwd,lend=1)
text(-6,(0:5)*20,paste((0:5)*20,"%",sep=""),pos=2,cex=(2/3),col=gray(.2),xpd=TRUE,family="sans")
# x axis
segments(0,-6,100,-6,col = gray(.2), xpd = TRUE, lwd = .8) # frame...
segments((0:5)*20,-6,(0:5)*20,-7,col=gray(.2),xpd=TRUE, lwd = s.lwd,lend=1)
text((0:5)*20,-8,paste((0:5)*20,"%",sep=""),pos=1,cex=2/3,col=gray(.2),xpd=TRUE,family="sans")

text(-8,113,"% spouse of\nfemale students",col=gray(.2),xpd=TRUE,family="sans", cex = (3/4))
text(50,-18,"% spouse among all females",col=gray(.2),xpd=TRUE,family="sans", cex = (3/4), pos = 1)

dev.off()

rm(DATAadj)
}
# --------------------------------------------------------------------------------
# Figure 7 data
{
load("DATAfigBW/Fig7_DATAadj.Rdata")

s.lwd <- 1.5

png(file="FiguresPaperBW/final/Figure7BW.png",
        width=3.1,
        height=2.7,
        units = "in", 
        res = res, 
        pointsize = 6)

par(xaxs="i",yaxs="i",ps=12, mar = c(5.5,2,4.5,4))
plot(NULL,type="n",xlim=c(-20,120),ylim=c(0,100),axes=FALSE,xlab="",ylab="",asp=1)
x   <- 100 * DATAadj$prop_child
y1  <- 100 * DATAadj$withchild1
y2  <- 100 * DATAadj$withchild2
minx <- min(x,na.rm=T) ; maxx <- max(x,na.rm=T)
# ---------------------------------------------
# CIs first
# for primary school:
coefs1 <- Deming(x,y1)[1:2]
xnew <- seq(min(x,na.rm=T),max(x,na.rm=T),length.out=200)
CIs <- DemingCI(x,y1,xnew, Nboot = Nboot)
polygon(c(xnew,rev(xnew)),c(CIs[,1],rev(CIs[,2])),col=paste(gray(.8),50,sep=""),border=FALSE,xpd=FALSE)
# for secondary school:
coefs2 <- Deming(x,y2)[1:2]
xnew <- seq(min(x,na.rm=T),max(x,na.rm=T),length.out=200)
CIs <- DemingCI(x,y2,xnew, Nboot = Nboot)
polygon(c(xnew,rev(xnew)),c(CIs[,1],rev(CIs[,2])),col=paste(gray(.8),50,sep=""),border=FALSE,xpd=FALSE)
# ---------------------------------------------
# now points, segments, text
coefs1 <- Deming(x,y1)[1:2]
xnew <- seq(min(x,na.rm=T),max(x,na.rm=T),length.out=200)
CIs <- DemingCI(x,y1,xnew, Nboot = Nboot)
segments(xnew[1],coefs1[1]+xnew[1]*coefs1[2],xnew[200],coefs1[1]+xnew[200]*coefs1[2],col=gray(.4))
text(90,coefs1[1]+xnew[200]*coefs1[2],paste("Primary (p)\nslope =",round(coefs1[2],3)),
        cex=(3/4),col=gray(.2),xpd=TRUE,pos=4,family="sans")
#points(91.47,67.56,pch="p",col=gray(.4)) # circle
# for secondary school:
coefs2 <- Deming(x,y2)[1:2]
xnew <- seq(min(x,na.rm=T),max(x,na.rm=T),length.out=200)
CIs <- DemingCI(x,y2,xnew, Nboot = Nboot)
segments(xnew[1],coefs2[1]+xnew[1]*coefs2[2],xnew[200],coefs2[1]+xnew[200]*coefs2[2],col=gray(.4))

text(90,coefs2[1]+xnew[200]*coefs2[2],paste("Secondary+ (s)\nslope =",round(coefs2[2],3)),
        cex=(3/4),col=gray(.2),xpd=TRUE,pos=4,family="sans")
#points(97.66,28.86,pch="s",col=gray(.5)) # triangle
# ---------------------------------------------
# y axis
segments(-6,0,-6,100,col = gray(.2), xpd = TRUE, lwd = .8) # frame...
segments(-6,(0:5)*20,-7,(0:5)*20,col=gray(.2),xpd=TRUE, lwd = s.lwd,lend=1)
text(-6,(0:5)*20,paste((0:5)*20,"%",sep=""),pos=2,cex=(2/3),col=gray(.2),xpd=TRUE,family="sans")
# x axis
segments(0,-6,100,-6,col = gray(.2), xpd = TRUE, lwd = .8) # frame...
segments((0:5)*20,-6,(0:5)*20,-7,col=gray(.2),xpd=TRUE, lwd = s.lwd,lend=1)
text((0:5)*20,-8,paste((0:5)*20,"%",sep=""),pos=1,cex=(2/3),col=gray(.2),xpd=TRUE,family="sans")

# plot points on top of confidence regions:
points(x,y1,pch="p",col=gray(.4),xpd=TRUE,cex = .5)
points(x,y2,pch="s",col=gray(.2),xpd=TRUE, cex = .5)

text(-5,117,"% mother of\nfemale students",col=gray(.2),xpd=TRUE,family="sans", cex = (3/4))
text(50,-20,"% mother among all females",col=gray(.2),xpd=TRUE,family="sans", cex = (3/4), pos = 1)
dev.off()

rm(DATAadj)

}




#write.table(DATAadj, file="Fig7data4Jeroen.csv",sep=",",col.names = colnames(DATAadj))
#getwd()

