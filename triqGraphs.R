require(xlsx)
require(ggplot2)
require(gridExtra)
require(grid)

##****CARGA DE FICHEROS*****************************************************************************************************
dataset = "Elutriatrion"
dataset_multiexperiment_df <- read.xlsx("/Users/davgutavi/Desktop/soluciones_paper/elu/elu_exp_level.xlsx", 2)
msr3d_df <- read.xlsx("/Users/davgutavi/Desktop/soluciones_paper/elu/elu-msr3d-08/elu-msr3d-08_triq.xlsx", 2)
lsl_df <- read.xlsx("/Users/davgutavi/Desktop/soluciones_paper/elu/elu-lsl-03/elu-lsl-03_triq.xlsx", 2)
msl_df <- read.xlsx("/Users/davgutavi/Desktop/soluciones_paper/elu/elu-msl-02/elu-msl-02_triq.xlsx", 2)

##****CONSTRUCCIÓN DE DATAFRAMES*********************************************************************************************

##****Medias y desviación típica de todos los experimentos de casda dataset
meanstdev_df <- dataset_multiexperiment_df[5:6]

##****Eqtiquetas de solución y valores de TRIQ
msr3d_triqdf <- msr3d_df[1:2]
lsl_triqdf   <- lsl_df[1:2]
msl_triqdf   <- msl_df[1:2]

##****Valores de GRQ y BIOQ
msr3d_grqbioqdf <- msr3d_df[3:4]
lsl_grqbioqdf <- lsl_df[3:4]
msl_grqbioqdf <- msl_df[3:4]

##****Valores de PEQ y SPQ
msr3d_peqspqdf <- msr3d_df[5:6]
lsl_peqspqdf <- lsl_df[5:6]
msl_peqspqdf <- msl_df[5:6]

##****CONSTRUCCIÓN DE GRÁFICOS*********************************************************************************************

##****Para poder poner subíndices hay que recoger todas las etiquetas TRI_i en un vector de bquote
c <-1
l<-c()
for (i in msr3d_triqdf$SOLUTION){
l<-c(l,bquote(TRI[.(c)]))
c<-c+1
}

##****Gráficos de barras TRIQ
msr3d_triq_barplot <- ggplot(data=msr3d_triqdf,aes(SOLUTION,TRIQ))+geom_col()+scale_x_discrete(labels=l)+ggtitle("TRIQ",subtitle = dataset)
lsl_triq_barplot <- ggplot(data=lsl_triqdf,aes(SOLUTION,TRIQ))+geom_col()+scale_x_discrete(labels=l)+ggtitle("TRIQ",subtitle = dataset)
msl_triq_barplot <- ggplot(data=msl_triqdf,aes(SOLUTION,TRIQ))+geom_col()+scale_x_discrete(labels=l)+ggtitle("TRIQ",subtitle = dataset)

##****Gráficos scatter de GRQ vs BIOQ
msr3d_grqbioq_scatter <- ggplot(data = msr3d_grqbioqdf, aes(BIOQ, GRQ)) + geom_jitter() + xlim(0.0, 1.0) + ylim(0.0, 1.0)+ggtitle(bquote(MSR[3*D]))
lsl_grqbioq_scatter <- ggplot(data = lsl_grqbioqdf, aes(BIOQ, GRQ)) + geom_jitter() + xlim(0.0, 1.0) + ylim(0.0, 1.0)+ggtitle("LSL")
msl_grqbioq_scatter <- ggplot(data = msl_grqbioqdf, aes(BIOQ, GRQ)) + geom_jitter() + xlim(0.0, 1.0) + ylim(0.0, 1.0)+ggtitle("MSL")

##****Gráficos scatter de PEQ vs SPQ
msr3d_peqspq_scatter <- ggplot(data = msr3d_peqspqdf, aes(PEQ, SPQ)) + geom_jitter() + xlim(0.0, 1.0) + ylim(0.0, 1.0)+ggtitle(bquote(MSR[3*D]))
lsl_peqspq_scatter <- ggplot(data = lsl_peqspqdf, aes(PEQ, SPQ)) + geom_jitter() + xlim(0.0, 1.0) + ylim(0.0, 1.0)+ggtitle("LSL")
msl_peqspq_scatter <- ggplot(data = msl_peqspqdf, aes(PEQ, SPQ)) + geom_jitter() + xlim(0.0, 1.0) + ylim(0.0, 1.0)+ggtitle("MSL")

##****Combo: barras TRIQ + scatter de GRQ vs BIOQ + scatter de PEQ vs SPQ
msr3d_combo_barplot_scatter_scatter <- grid.arrange(msr3d_triq_barplot, msr3d_grqbioq_scatter, msr3d_peqspq_scatter, ncol=3)
lsl_combo_barplot_scatter_scatter <- grid.arrange(lsl_triq_barplot, lsl_grqbioq_scatter, lsl_peqspq_scatter, ncol=3)
msl_combo_barplot_scatter_scatter <- grid.arrange(msl_triq_barplot, msl_grqbioq_scatter, msl_peqspq_scatter, ncol=3)

##****Combo: scatter de PEQ vs SPQ de MSR3D + scatter de PEQ vs SPQ de LSL + scatter de PEQ vs SPQ de MSL
all_combo_peqspq_scatter <- grid.arrange(msr3d_peqspq_scatter, lsl_peqspq_scatter,msl_peqspq_scatter, ncol=3,top=paste0("SPQ vs PEQ ",dataset," experiment"))

##****Combo: scatter de GRQ vs BIOQ de MSR3D + scatter de GRQ vs BIOQ de LSL + scatter de GRQ vs BIOQ de MSL
all_combo_grqbioq_scatter <- grid.arrange(msr3d_grqbioq_scatter, lsl_grqbioq_scatter,msl_grqbioq_scatter, ncol=3,top=paste0("GRQ vs BIOQ ",dataset," experiment"))

##****Gráfico scatter de MEAN vs STDEV

##****Leyendas de los puntos en una lista para el mathexpression
l2<-c(paste("MSR[3*D]"),"LSL","MSL")
meanstdev_scatter <- ggplot(data = meanstdev_df, aes(MEAN, STDEV)) + geom_jitter() + 
  geom_text(check_overlap = FALSE, size = 4, hjust = 0, vjust = 0,angle=45, parse = TRUE, aes(label =l2))+
  xlim(0.0, 1.0)+  ylim(0.0, 1.0)+ggtitle(dataset,subtitle = "Experiment Summary")

##****PINTAR GRÁFICOS PARA EXPORTAR****************************************************************************************
print(meanstdev_scatter)

print(msr3d_triq_barplot)
print(lsl_triq_barplot)
print(msl_triq_barplot)
print(msr3d_grqbioq_scatter)
print(lsl_grqbioq_scatter)
print(msl_grqbioq_scatter)
print(msr3d_peqspq_scatter)
print(lsl_peqspq_scatter)
print(msl_peqspq_scatter)

print(msr3d_combo_barplot_scatter_scatter)
print(lsl_combo_barplot_scatter_scatter)
print(msl_combo_barplot_scatter_scatter)
