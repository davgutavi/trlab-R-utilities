require(ggplot2)
require(gridExtra)
require(ggrepel)


################################################ Nube de puntos----
rt<- "/Users/davgutavi/NAS_DAVGUTAVI/biodata_mining/fuentes/biodatamining_revisions/paper_solutions"
output<-"/Users/davgutavi/.dropbox-alt/Dropbox/biodatamining_sources/img"

# Carga de ficheros----
dataset<-"GDS4472"
short<-"4472"
summary <- read.csv(paste0(rt,"/",short,"-sum.csv"), sep = ";",header = T)
rank<- read.csv(paste0(rt,"/",short,"-rank.csv"), sep = ";",header = T)

# Cambio a notación matemática----
rank$EXPERIMENT <- as.character(rank$EXPERIMENT)
rank$EXPERIMENT[rank$EXPERIMENT == "MSR_{3D}"] <- "MSR[3*D]"
rank$EXPERIMENT <- as.factor(rank$EXPERIMENT)
rank$EXPERIMENT_f <- factor(rank$EXPERIMENT, levels=c("MSR[3*D]","LSL","MSL","OPT"))

# Bioq vs Grq----
bioqGrq<-ggplot(rank, aes(BIOQ,GRQ,group = EXPERIMENT_f)) +
  geom_jitter() + 
  ggtitle(paste0(dataset," experiment: BIOQ v GRQ"))+
  # xlim(0.0013, 0.00072)+ 
  # ylim(0.0, 1.0)+
  facet_wrap(~EXPERIMENT_f,labeller = label_parsed,nrow = 1,ncol = 4)+
  theme( panel.border = element_rect(color="black", fill=NA),
        strip.background = element_rect(fill=NA, color="black"),
        axis.title = element_text(size = 10, face = "bold"),
        axis.text = element_text(size = 5),
        strip.text = element_text(size = 10)
  )

# Peq vs Spq----
peqSpq<-ggplot(rank, aes(PEQ,SPQ,group = EXPERIMENT_f)) +
  geom_jitter() + 
  ggtitle(paste0(dataset," experiment: PEQ v SPQ"))+
  # xlim(0.0, 1.0)+ 
  # ylim(0.0, 1.0)+
  facet_wrap(~EXPERIMENT_f,labeller = label_parsed,nrow = 1,ncol = 4)+
  theme( panel.border = element_rect(color="black", fill=NA),
         strip.background = element_rect(fill=NA, color="black"),
         axis.title = element_text(size = 10, face = "bold"),
         axis.text = element_text(size = 5),
         strip.text = element_text(size = 10)
  )


# Summary----
sumLabels<-c("MSR[3*D]","LSL","MSL","OPT")
sm <- ggplot(summary, aes(MEAN, STDEV)) + 
      geom_jitter() +
      ggtitle(paste0(dataset," experiment: TRIQ summary"))+
      geom_text_repel(aes(label=sumLabels),parse = TRUE, size=3,segment.color = NA)+
      # geom_text(aes(label=sumLabels),parse = TRUE, size=3)+
      # geom_text_repel(aes(label=sumLabels),parse = TRUE, size=3)+
      # xlim(0.0, 1.0)+  
      # ylim(0.0, 1.0)+
      theme(panel.border = element_rect(color="black", fill=NA),
         strip.background = element_rect(fill=NA, color="black"),
         axis.title = element_text(size = 10, face = "bold"),
         axis.text = element_text(size = 5)
    )

# Impresión por pantalla y guardar en eps----
cw <- 8
ch <- 4
sw <- 5
sh <- 5
print(bioqGrq)
ggsave(paste0(short,"_bioqVgrq.eps"),plot=bioqGrq,path=output,device = "eps",width = cw, height = ch)
print(peqSpq)
ggsave(paste0(short,"_peqVspq.eps"),plot=peqSpq,path=output,device = "eps",width = cw, height = ch)
print(sm)
ggsave(paste0(short,"_summary.eps"),plot=sm,path=output,device = "eps",width = sw, height = sh)

################################################ Variación de pesos----

df <- read.csv("/Users/davgutavi/Desktop/triq_weights.csv", sep = ";",header = T)


# levels(df$weight) <- c("Capped~brood~cells", expression(sqrt("Colony weight (g)")))

gr<-ggplot(df, aes(x = x,y = triq,color = weight)) +
  geom_line() +
  labs(y = "TRIQ", x="WEIGHT VALUE")+
  scale_color_manual(labels = c(expression(W[bio]),expression(W[gr]),expression(W[pe]),expression(W[sp])),
                     values = c("red", "blue", "green","orange"))+
  theme( panel.border = element_rect(color="black", fill=NA),
       strip.background = element_rect(fill=NA, color="black"),
       axis.title = element_text(size = 10, face = "bold"),
       axis.text = element_text(size = 5),
       strip.text = element_text(size = 10),
       legend.text.align = 0
  )

print(gr)




gr<-ggplot(df, aes(x = x,y = triq,color = weight)) +
  geom_line() +
  # geom_point(aes(x=0.5, y=0.4935392438), colour="red") +
  # # geom_label(aes(x=0.5, y=0.4935392438, label="W[bio]"),parse = T) +
  # geom_point(aes(x=0.4, y=0.4935392438), colour="blue") +
  # geom_point(aes(x=0.05, y=0.4935392438), colour="green") +
  # geom_point(aes(x=0.05, y=0.4935392438), colour="orange") +
  labs(y = "TRIQ", x="WEIGHT VALUE")+
  scale_color_manual(labels = c(expression(W[bio]), expression(W[gr]),expression(W[pe]),expression(W[sp])),
                     values = c("red", "blue", "green","orange"))+
  theme( panel.border = element_rect(color="black", fill=NA),
         strip.background = element_rect(fill=NA, color="black"),
         axis.title = element_text(size = 10, face = "bold"),
         axis.text = element_text(size = 5),
         strip.text = element_text(size = 10)
  )

print(gr)






























