library(lattice)
source("old/genes-utilities.R")

singleGraphV2("/home/david/TrLab/test/coordinates")
singleGraphV2("/home/david/experiment_01/coordinates")

expGraph("/Users/davgutavi/Desktop/evobio/elutriation/resources/msr3d-elu-10/erre",w=5,h=7)
# ----
plat <- getGEOplatform("GDS4262")
table<-Table(plat)
s = c("IFREP01","GFREP01","IMREP01","GMREP01","IFREP02","GFREP02","IMREP02","GMREP02",
      "IFREP03","GFREP03","IMREP03","GMREP03")
t = c("0d","3d","7d","14d")
paperGraph("/Users/davgutavi/Desktop/evobio/gse4324/multi/gse4324-multi-02/erre",s,t)
s = c("IFREP01","GFREP01","IMREP01","GMREP01","IFREP02","GFREP02","IMREP02","GMREP02",
      "IFREP03","GFREP03","IMREP03","GMREP03")
t = c("0d","3d","7d","14d")
day0 = df[,c("GSM98652","GSM98656","GSM98660","GSM98664","GSM98668","GSM98672",
             "GSM98676","GSM98680","GSM98684","GSM98688","GSM98692","GSM98696")] 
day3 = df[,c("GSM98653","GSM98657","GSM98661","GSM98665","GSM98669","GSM98673",
             "GSM98677","GSM98681","GSM98685","GSM98689","GSM98693","GSM98697")]
day7 = df[,c("GSM98654","GSM98658","GSM98662","GSM98666","GSM98670","GSM98674",
             "GSM98678","GSM98682","GSM98686","GSM98690","GSM98694","GSM98698")]
day14 = df[,c("GSM98655","GSM98659","GSM98663","GSM98667","GSM98671","GSM98675",
              "GSM98679","GSM98683","GSM98687","GSM98691","GSM98695","GSM98699")]
write.table(day14,file="/Users/davgutavi/Desktop/0017/day14.csv",sep=";",row.names=F,col.names=F)