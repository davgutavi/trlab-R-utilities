source("LoadEnvironment.R")
source("utils/general_graphs.R")
require(gtable)

# Configuración general  ----
Xsize <- 30
Ysize <- 20
root29 <- "/Users/davgutavi/NAS_DAVGUTAVI/INVESTIGACION/zonificacion_pirineos/experimentos/pirineos/pirineos29"
root30 <- "/Users/davgutavi/NAS_DAVGUTAVI/INVESTIGACION/zonificacion_pirineos/experimentos/pirineos/pirineos30"
root29a <- "/Users/davgutavi/NAS_DAVGUTAVI/INVESTIGACION/zonificacion_pirineos/experimentos/pirineos/pirineos29alt"
root30a <- "/Users/davgutavi/NAS_DAVGUTAVI/INVESTIGACION/zonificacion_pirineos/experimentos/pirineos/pirineos30alt"
tot29Path <- "/Users/davgutavi/NAS_DAVGUTAVI/INVESTIGACION/zonificacion_pirineos/dataset/pirineos/totales/pirineos29_tot.csv"
tot30Path <- "/Users/davgutavi/NAS_DAVGUTAVI/INVESTIGACION/zonificacion_pirineos/dataset/pirineos/totales/pirineos30_tot.csv"
outPutPath <- "/Users/davgutavi/NAS_DAVGUTAVI/INVESTIGACION/zonificacion_pirineos/experimentos/pirineos/mapas"
# test<-zonification_plot_list(paste0("/Users/davgutavi/Desktop","/test/test.sol"),tot29Path,Xsize,Ysize,"Test")
# testmap<-marrangeGrob(test,nrow=4,ncol=2,top="Test",layout_matrix=matrix(seq_len(8),byrow=T,nrow=4,ncol=2))
# ggsave("test.pdf",testmap,"pdf","/Users/davgutavi/Desktop",width=40,height=40,units="cm")

# Tests ----
current<-"05"

sol<-paste0(root29,"/p29_",current,"/p29_",current,".sol")
pl<-zonification_plot_list(sol,tot29Path,Xsize,Ysize,paste0("p29_",current))
maps<-marrangeGrob(pl,nrow=4,ncol=2,top=current,layout_matrix=matrix(seq_len(8),byrow=T,nrow=4,ncol=2))
ggsave(paste0("p29_",current,".pdf"),maps,"pdf",outPutPath,width=40,height=40,units="cm")
  
sol<-paste0(root30,"/p30_",current,"/p30_",current,".sol")
pl<-zonification_plot_list(sol,tot30Path,Xsize,Ysize,paste0("p30_",current))
maps<-marrangeGrob(pl,nrow=4,ncol=2,top=current,layout_matrix=matrix(seq_len(8),byrow=T,nrow=4,ncol=2))
ggsave(paste0("p30_",current,".pdf"),maps,"pdf",outPutPath,width=40,height=40,units="cm")
  
sol<-paste0(root29a,"/p29a_",current,"/p29a_",current,".sol")
pl<-zonification_plot_list(sol,tot29Path,Xsize,Ysize,paste0("p29a_",current))
maps<-marrangeGrob(pl,nrow=4,ncol=2,top=current,layout_matrix=matrix(seq_len(8),byrow=T,nrow=4,ncol=2))
ggsave(paste0("p29a_",current,".pdf"),maps,"pdf",outPutPath,width=40,height=40,units="cm")
  
sol<-paste0(root30a,"/p30a_",current,"/p30a_",current,".sol")
pl<-zonification_plot_list(sol,tot30Path,Xsize,Ysize,paste0("p30a_",current))
maps<-marrangeGrob(pl,nrow=4,ncol=2,top=current,layout_matrix=matrix(seq_len(8),byrow=T,nrow=4,ncol=2))
ggsave(paste0("p30a_",current,".pdf"),maps,"pdf",outPutPath,width=40,height=40,units="cm")



