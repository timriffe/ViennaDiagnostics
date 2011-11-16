# Author: Tim Riffe
###############################################################################

# age-specific curves of enrolled, in union and has child for each country.

DATAc <- read.table("http://www.ced.uab.es/worldfam/figures/FIGURE1_2CURRENT.tab",
		header=T,
		sep="\t",
		na.strings = ".")
ind <- DATAc$country=="Uganda" & DATAc$year==2002 # remove extra Uganda sample
DATAc <- DATAc[!ind,]
DATAc$country <- as.character(DATAc$country)
DATAc$sex <- as.character(DATAc$sex)
countries <- unique(DATAc$country)

pdf(file="C:\\Users\\triffe\\git\\ViennaPaper\\ALBERTPAPERS\\JeroenDiagnosticmales.pdf",width=7,height=7)
for (i in countries){
	ind <-  DATAc$country==i & DATAc$sex=="Male"
	age <- DATAc$age[ind]
	edu <- DATAc$prop_school[ind]
	unio <- DATAc$current_union[ind]
	child <- DATAc$with_child[ind]
	par("xaxs"="i","yaxs"="i")
	plot(NULL,type='n',main=paste(i,", ",unique(DATAc$year[ind]),sep=""),xlab="Age",ylab="proportion",ylim=c(0,1),xlim=c(12,24))
	extr <- par("usr")
	rect(extr[1],extr[3],extr[2],extr[4],col="#EBEBEB")
	abline(v=12:24,col="white")
	abline(h=seq(0,1,by=.1),col="white")
	lines(age,edu,lwd=2,col="gold")
	lines(age,unio,lwd=2,col="purple")
	lines(age,child,lwd=2,col="pink")
}
dev.off()


