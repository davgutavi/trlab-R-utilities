require(lattice)

filePath<-"/Users/davgutavi/Desktop/soluciones_paper/elu/elu-msl-02/coordinates/elu-msl-02_co_1.csv"
samplePath<-"/Users/davgutavi/Desktop/TrLab_go_actualizado/resources/0002/elutriation_samples.txt"
timePath<-"/Users/davgutavi/Desktop/TrLab_go_actualizado/resources/0002/elutriation_times.txt"
  
w         <-5
h         <-4
fsizeXaxis<-0.7
fsizeYaxis<-0.7
fsizeBoxes<-1.0
lib       <-FALSE
color     <-TRUE

sfile <- read.table(samplePath, sep="\n")
tfile <- read.table(timePath, sep="\n")
allConditions = sfile$V1
allTimes = tfile$V1

axis <-c(fsizeYaxis,fsizeXaxis)
boxes<-list (cex = fsizeBoxes)
aux  <-unlist(strsplit(filePath, "\\."))
out_gct  <-paste(aux[1], "_gct.eps", sep = "")
out_gtc  <-paste(aux[1], "_gtc.eps", sep = "")
out_tgc  <-paste(aux[1], "_tgc.eps", sep = "")
tricluster<-read.csv2(filePath)
attach(tricluster,warn.conflicts = FALSE)
fs<-factor(s)
ft<-factor(t)
fg<-factor(g)
nel   <-as.numeric(levels(tricluster$el)[as.double(tricluster$el)]) 
left  <-5
div   <- ceiling(length(levels(fg))/2)
right <-length(levels(fg))-5
at    <-c(left,div,right)
labels<-c(g[left],g[div],g[right]) 

conditions <- c()
for (si in levels(fs)){
  conditions<-append(conditions, as.character(allConditions[as.numeric(si)+1]))
}
times <- c()
for (ti in levels(ft)){
  times<-append(times, as.character(allTimes[as.numeric(ti)+1]))
}

# Genes, Conditions, Times 
xyplot(nel ~ fg | ft,
       xlab="genes",
       ylab="", 
       groups = s,
       type = "a",
       font = "arial",
       scales = list(x = list(at=at,labels=labels),cex=axis),
       layout = c(1, nlevels(ft)),
       strip=strip.custom(factor.levels=times,par.strip.text = boxes))
dev.copy2eps(file = out_gct, width=w, height=h)
#dev.off()

# Genes, Times , Conditions 
xyplot(nel ~ fg | fs, tricluster,
       xlab="genes",
       ylab="", 
       groups = t,
       type = "a",
       font = "arial",
       scales = list(x = list(at=at,labels=labels),cex=axis),
       layout = c(1, nlevels(fs)),
       strip=strip.custom(factor.levels=conditions,par.strip.text = boxes)
) 
dev.copy2eps(file = out_gtc, width=w, height=h)
#dev.off()   

#Times, Genes, Conditions 
xyplot(nel ~ ft | fs, tricluster,
       xlab="time",
       ylab="", 
       groups = g, 
       type = "a",
       font = "arial",
       layout = c(1, nlevels(fs)),
       scales = list(x = list(labels=times),cex=axis),
       strip=strip.custom(factor.levels=conditions,par.strip.text = boxes)
) 
dev.copy2eps(file = out_tgc, width=w, height=h)