source("strigen_utils.R")

rootFolder         <- "/Users/davgutavi/Desktop/strigen_ijcis_plots"
syntheticFolder    <- paste0(rootFolder,"/synthetic")
sensoresFolder     <- paste0(rootFolder,"/sensores")
syntheticFolder.s1 <- paste0(syntheticFolder,"/synthetic1")
syntheticFolder.s2 <- paste0(syntheticFolder,"/synthetic2")
syntheticFolder.s3 <- paste0(syntheticFolder,"/synthetic3")
grqFolder          <- paste0(sensoresFolder,"/grq")
compFolder         <- paste0(sensoresFolder,"/comp")
s1.runFolderPaths  <- dir(syntheticFolder.s1, full.names = T)
s2.runFolderPaths  <- dir(syntheticFolder.s2, full.names = T)
s3.runFolderPaths  <- dir(syntheticFolder.s3, full.names = T)
grq.paths          <- dir(grqFolder, full.names = T)
comp.paths         <- dir(compFolder, full.names = T)
general.comp.path  <- paste0(compFolder,"/comp.csv")

# ---- Synthetic
sinthetic1Dfs <- loadDfsFromVectorOfFilePaths(s1.runFolderPaths)
sinthetic2Dfs <- loadDfsFromVectorOfFilePaths(s2.runFolderPaths)
sinthetic3Dfs <- loadDfsFromVectorOfFilePaths(s3.runFolderPaths)

s1df <- buidGlobalSyntheticDf(sinthetic1Dfs)
s2df <- buidGlobalSyntheticDf(sinthetic2Dfs)
s3df <- buidGlobalSyntheticDf(sinthetic3Dfs)

# View(s1df)
# View(s2df)
# View(s3df)

s1.acc <- accPlot(s1df,ylimits = c(0.95,1))#
s1.f1 <- f1Plot(s1df,ylimits = c(0,1))#OK
s1.f1.bp <- f1Box(s1df,ylimits = c(0,1))#OK

s2.acc <- accPlot(s2df,ylimits = c(0.95,1))#
s2.f1 <- f1Plot(s2df,ylimits = c(0,1))#OK
s2.f1.bp <- f1Box(s2df,ylimits = c(0,1))#OK

s3.acc <- accPlot(s3df,ylimits = c(0.85,1))#
s3.f1 <- f1Plot(s3df,ylimits = c(0,1))#OK
s3.f1.bp <- f1Box(s3df,ylimits = c(0,1))#OK

# ---- GRQ, General

grqDf  <- loadDfsFromPath(grq.paths,type="grq")
compDf <- loadDfsFromPath(comp.paths,type="comp")
gcompDf <- read.csv(general.comp.path, sep = ";")
  
# View(grqDf)
# View(compDf)

grq <- grqPlot(grqDf,ylimits = c(0.75,1))#OK
accF1Plot <- accF1Plot (gcompDf,ylimits = c(0,1))#OK

# ---- Prints
require(Cairo)
folder <- "/Users/davgutavi/Desktop/TriGen Streaming IJCIS/img"
h <- 12
w  <- 10
ggsave("acc_aditive.eps",plot=s1.acc,path=folder,width =w,height=h,units="cm",device=cairo_ps)
ggsave("acc_multiplicative.eps",plot=s2.acc,path=folder,width =w,height=h,units="cm",device=cairo_ps)
ggsave("acc_abrupt.eps",plot=s3.acc,path=folder,width =w,height=h,units="cm",device=cairo_ps)
ggsave("f1_aditive.eps",plot=s1.f1,path=folder,width =w,height=h,units="cm",device=cairo_ps)
ggsave("f1_multiplicative.eps",plot=s2.f1,path=folder,width=w,height=h,units="cm",device=cairo_ps)
ggsave("f1_abrupt.eps",plot=s3.f1,path=folder,width =w,height=h,units="cm",device=cairo_ps)
ggsave("comp_sensors.eps",plot=accF1Plot,path=folder,width =w,height=h,units="cm",device=cairo_ps)
h <- 10
w  <- 10
ggsave("grq_sensors.eps",plot=grq,path=folder,width =w,height=h,units="cm",device=cairo_ps)
ggsave("f1_box_aditive.eps",plot=s1.f1.bp,path=folder,width =w,height=h,units="cm",device=cairo_ps)
ggsave("f1_box_multiplicative.eps",plot=s2.f1.bp,path=folder,width =w,height=h,units="cm",device=cairo_ps)
ggsave("f1_box_abrupt.eps",plot=s3.f1.bp,path=folder,width =w,height=h,units="cm",device=cairo_ps)


# combi <- do.call(grid.arrange,list(gr.f1,gr.acc,nrow=2,ncol=1))