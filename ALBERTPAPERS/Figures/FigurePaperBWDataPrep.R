# Author: triffe
###############################################################################

# similar to previous figures script, except all data stuff is done first, saved as Rdata files,
# the smoothing diagnostics are not included here.
setwd("/home/triffe/git/ViennaDiagnostics/ALBERTPAPERS/Figures/DATAfigBW")

# countries to remove either beause reviewer thinks we're mixing rich and poor too mch or because they had 5-year age groups:
rm.countries <- c("Italy","Palestine","Slovenia","Switzerland","France","Spain","Austria","Romania","Belarus","United States", "")

# -----------------------------------------------------------------------------------------------------------------------------
# Figure 1, 2, 3 
# read in data from Joan:
DATA <- read.table("FIGURE1_2.tab",header=T,sep="\t",na.strings = ".",stringsAsFactors =FALSE)
DATA <- DATA[!DATA$country %in% rm.countries,]

cntriesf <- unique(DATA$country[DATA$SEX=="Female"])
cntriesm <- unique(DATA$country[DATA$SEX=="Male"])
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

# save out
save(DATAadj,file = "Fig1_DATAadj.Rdata")
rm(DATAadj, DATA)
# -----------------------------------------------------------------------------------------------------------------------------
# Figure 4

# tab data called fig 3 for legacy- actually used for fig 4
DATA <- read.table("FIGURE3.tab",
        header=TRUE,
        sep="\t",
        na.strings = ".",
        stringsAsFactors = FALSE)

DATA <- DATA[!DATA$country %in% rm.countries,]
cntriesf <-unique(DATA$country[DATA$SEX=="Female"])
cntriesm <- unique(DATA$country[DATA$SEX=="Male"])
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

# save out
save(DATAadj,file = "Fig4_DATAadj.Rdata")
rm(DATAadj, DATA)
# -----------------------------------------------------------------------------------------------------------------------------
# Figure 5

# NOTE: the data are called Figure 6!!!
# in the paper we may push this to Figure 4...
DATA <- read.table("FIGURE6.tab",
        header=TRUE,
        sep="\t",
        na.strings = ".",
        stringsAsFactors = FALSE)
DATA <- DATA[!DATA$country %in% rm.countries,]
DATA <- DATA[DATA$AGE > 14,]
cntries <- unique(DATA$country)

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

# save out
save(DATAadj,file = "Fig5_DATAadj.Rdata")
rm(DATAadj)

# -----------------------------------------------------------------------------------------------------------------------------
# Figure 6
# Figure 6 (using data called Figure 7)
DATA <- read.table("FIGURE7.tab",
        header=TRUE,
        sep="\t",
        na.strings = ".", 
        stringsAsFactors = FALSE)
DATA <- DATA[!DATA$country %in% rm.countries,]
DATA <- DATA[DATA$AGE > 14,]
cntries <- unique(DATA$country)

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

# save out
save(DATAadj,file = "Fig6_DATAadj.Rdata")
rm(DATAadj)

# -----------------------------------------------------------------------------------------------------------------------------
# Figure 7
# Figure 7 (using data called Figure 8)
DATA <- read.table("FIGURE8.tab",
        header=TRUE,
        sep="\t",
        na.strings = ".", 
        stringsAsFactors = FALSE)

# many more removed
DATA <- DATA[!DATA$country %in% c("Kyrgyz Republic","Kazakhstan","Madagascar","Malaysia","Niger","Zimbabwe",
                "Jordan","Ghana","Cote Ivoire","Belarus","Armenia", rm.countries),]
cntries <- unique(DATA$country)
DATA <- DATA[!DATA$AGE<15,]


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

save(DATAadj,file = "Fig7_DATAadj.Rdata")

# clean up
rm(list = ls())
