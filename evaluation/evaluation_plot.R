library(psych)
library(ggplot2)
library(ggpubr)
library(ggcorrplot)
library(corrplot)
library(RColorBrewer)
library(png)
library(lessR)
library(psych)
library(readxl)
library(openxlsx)
library(car)
library(simplevis)
library(dplyr)
library(plotrix)
library(patchwork)
library(ggsignif)
library(ggpubr)


Bewertung<- read_excel("./data/Bewertung.xlsx")
Bewertung_new<-Bewertung



IndiCCT_order_old<-c("Indi6500","Indi7500","Indi9000","Indi11000","Indi14000","Indi18000",
                 "Indi24000","Indi30000")
IndiCCT_order<-c("6,500 K","7,500 K","9,000 K","11,000 K","14,000 K","18,000 K",
                 "24,000 K","30,000 K")
Testperson_order<-c("TP1","TP2","TP3","TP4","TP5","TP6","TP7","TP8","TP9","TP10",
                    "TP11","TP12","TP13","TP14","TP15","TP16","TP17","TP18","TP19","TP20",
                    "TP21","TP22","TP23","TP24","TP25","TP26","TP27","TP28","TP29")
L_id_oder<-c("L_1","L_2","L_3","L_4","L_5","L_6","L_7","L_8","L_9","L_10",
             "L_11","L_12","L_13","L_14","L_15","L_16","L_17","L_18","L_19","L_20",
             "L_21","L_22","L_23","L_24","L_25","L_26","L_27","L_28","L_29","L_30",
             "L_31","L_32")
Kombi_indi_oder<-c("70%Ee+6500K","70%Ee+7500K","70%Ee+9000K","70%Ee+11000K",
                   "70%Ee+14000K",	"70%Ee+18000K",	"70%Ee+24000K","70%Ee+30000K",
                   "50%Ee+6500K",	"50%Ee+7500K",	"50%Ee+9000K", "50%Ee+11000K",
                   "50%Ee+14000K",	"50%Ee+18000K","50%Ee+24000K",	"50%Ee+30000K")




# Paper naming
Bewertung_new$IndiEe <- gsub("50Prozent","D50/U50",Bewertung_new$IndiEe)
Bewertung_new$IndiEe <- gsub("70Prozent","D30/U70",Bewertung_new$IndiEe)

Bewertung_new$DiEe <- gsub("50Prozent","D50/U50",Bewertung_new$DiEe)
Bewertung_new$DiEe <- gsub("30Prozent","D30/U70",Bewertung_new$DiEe)

Bewertung_new$Anteil <- gsub("30%/70%","D30/U70",Bewertung_new$Anteil)
Bewertung_new$Anteil <- gsub("50%/50%","D50/U50",Bewertung_new$Anteil)

Bewertung_new$IndiCCT <- gsub("Indi6500","6,500 K",Bewertung_new$IndiCCT)
Bewertung_new$IndiCCT <- gsub("Indi7500","7,500 K",Bewertung_new$IndiCCT)
Bewertung_new$IndiCCT <- gsub("Indi9000","9,000 K",Bewertung_new$IndiCCT)
Bewertung_new$IndiCCT <- gsub("Indi11000","11,000 K",Bewertung_new$IndiCCT)
Bewertung_new$IndiCCT <- gsub("Indi14000","14,000 K",Bewertung_new$IndiCCT)
Bewertung_new$IndiCCT <- gsub("Indi18000","18,000 K",Bewertung_new$IndiCCT)
Bewertung_new$IndiCCT <- gsub("Indi24000","24,000 K",Bewertung_new$IndiCCT)
Bewertung_new$IndiCCT <- gsub("Indi30000","30,000 K",Bewertung_new$IndiCCT)

Bewertung_new$IndiCCT<-factor(Bewertung_new$IndiCCT,IndiCCT_order)
Bewertung_new$Testperson<-factor(Bewertung_new$Testperson,Testperson_order)
Bewertung_new$L_id<-factor(Bewertung_new$L_id,L_id_oder)
Bewertung_new$Kombi_indi<-factor(Bewertung_new$Kombi_indi,Kombi_indi_oder)



#####################Q1
# Figure 5
ggplot(Bewertung_new,aes(y=F1,x=IndiEe,fill=IndiCCT))+
  geom_violin(trim = FALSE, alpha = 0.5,position=position_dodge(width=0.8))+
  stat_boxplot(geom = "errorbar",position=position_dodge(width=0.8),width=0.1)+
  geom_boxplot(width=0.2, position=position_dodge(width=0.8))+
  stat_summary(fun= mean, geom = "point",size=2,col="darkred",
               position=position_dodge(width=0.8))+
  stat_summary(fun = mean, geom = "text", col = "darkred",  size=7,
               position=position_dodge(width=0.8),
               vjust = 2, aes(label = paste( round(..y.., digits = 2))))+
  theme(text = element_text(size = 26),
        legend.text = element_text(size=26))+
  scale_y_continuous(breaks = c(-3,-2,-1,0,1,2,3))+
  scale_fill_brewer(palette = "Set2")+
  labs(x="direct/indirect ratio",y = "Q1: brightness - subjective ratings")+
  guides(fill=guide_legend(title="indirect CCT")) 


#####################Q2
# Figure 6
ggplot(Bewertung_new,aes(y=F2,x=IndiEe,fill=IndiCCT))+
  geom_violin(trim = FALSE, alpha = 0.5,position=position_dodge(width=0.8))+
  stat_boxplot(geom = "errorbar",position=position_dodge(width=0.8),width=0.1)+
  geom_boxplot(width=0.2, position=position_dodge(width=0.8))+
  stat_summary(fun= mean, geom = "point",size=2,col="darkred",
               position=position_dodge(width=0.8))+
  stat_summary(fun = mean, geom = "text", col = "darkred",  size=7,
               position=position_dodge(width=0.8),
               vjust = 2, aes(label = paste( round(..y.., digits = 2))))+
  theme(text = element_text(size = 26),
        legend.text = element_text(size=26))+
  scale_y_continuous(breaks = c(-3,-2,-1,0,1,2,3))+
  scale_fill_brewer(palette = "Set2")+
  labs(x="direct/indirect ratio",y = "Q2: sky-likeness - subjective ratings")+
  guides(fill=guide_legend(title="indirect CCT")) 


# additional Plot
ggplot(Bewertung_new,aes(y=F2,x=IndiCCT,fill=IndiCCT))+
  geom_violin(trim = FALSE, alpha = 0.5,position=position_dodge(width=0.8))+
  stat_boxplot(geom = "errorbar",position=position_dodge(width=0.8),width=0.1)+
  geom_boxplot(width=0.3, position=position_dodge(width=0.8))+
  stat_summary(fun= mean, geom = "point",size=3,col="darkred",
               position=position_dodge(width=0.8))+
  stat_summary(fun = mean, geom = "text", col = "darkred",  size=7,
               position=position_dodge(width=0.8),
               vjust = 2, aes(label = paste(round(..y.., digits = 2))))+
  theme(text = element_text(size = 20),
        legend.text = element_text(size=20))+
  scale_y_continuous(breaks = c(-3,-2,-1,0,1,2,3))+
  scale_fill_brewer(palette = "Set2")+
  labs(x="indirect CCT in K",y = "Q2: sky-likeness - subjective ratings")+
  guides(fill=guide_legend(title="indirect CCT")) +
  stat_compare_means(paired = FALSE,method="kruskal.test",
                     label.y =5, size=6)


#####################Q3
# Figure 8
ggplot(Bewertung_new,aes(y=F3,x=IndiEe,fill=IndiCCT))+
  geom_violin(trim = FALSE, alpha = 0.5,position=position_dodge(width=0.8))+
  stat_boxplot(geom = "errorbar",position=position_dodge(width=0.8),width=0.1)+
  geom_boxplot(width=0.2, position=position_dodge(width=0.8))+
  stat_summary(fun= mean, geom = "point",size=2,col="darkred",
               position=position_dodge(width=0.8))+
  stat_summary(fun = mean, geom = "text", col = "darkred",  size=7,
               position=position_dodge(width=0.8),
               vjust = 2, aes(label = paste( round(..y.., digits = 2))))+
  theme(text = element_text(size = 26),
        legend.text = element_text(size=26))+
  scale_y_continuous(breaks = c(-3,-2,-1,0,1,2,3))+
  scale_fill_brewer(palette = "Set2")+
  labs(x="direct/indirect ratio",y = "Q3: satisfaction - subjective ratings")+
  guides(fill=guide_legend(title="indirect CCT")) 


#####################Q4
# additional Figure to Figure 8
ggplot(Bewertung_new,aes(y=F4,x=IndiEe,fill=IndiCCT))+
  geom_violin(trim = FALSE, alpha = 0.5,position=position_dodge(width=0.8))+
  stat_boxplot(geom = "errorbar",position=position_dodge(width=0.8),width=0.1)+
  geom_boxplot(width=0.2, position=position_dodge(width=0.8))+
  stat_summary(fun= mean, geom = "point",size=2,col="darkred",
               position=position_dodge(width=0.8))+
  stat_summary(fun = mean, geom = "text", col = "darkred",  size=7,
               position=position_dodge(width=0.8),
               vjust = 2, aes(label = paste( round(..y.., digits = 2))))+
  theme(text = element_text(size = 26),
        legend.text = element_text(size=26))+
  scale_y_continuous(breaks = c(-3,-2,-1,0,1,2,3))+
  scale_fill_brewer(palette = "Set2")+
  labs(x="Indirektanteil",y = "Bewertung der visuellen Annehmlichkeit")


###################Q5
rm(mean)
# additional Figure to Figure 8
ggplot(Bewertung_new,aes(y=F5,x=IndiEe,fill=IndiCCT))+
  geom_violin(trim = FALSE, alpha = 0.5,position=position_dodge(width=0.8))+
  stat_boxplot(geom = "errorbar",position=position_dodge(width=0.8),width=0.1)+
  geom_boxplot(width=0.2, position=position_dodge(width=0.8))+
  stat_summary(fun= mean, geom = "point",size=2,col="darkred",
               position=position_dodge(width=0.8))+
  stat_summary(fun = mean, geom = "text", col = "darkred",  size=6,
               position=position_dodge(width=0.8),
               vjust = 2, aes(label = paste( round(..y.., digits = 2))))+
  theme(text = element_text(size = 26),
        legend.text = element_text(size=26))+
  scale_fill_brewer(palette = "Set2")+
  labs(x="Indirektanteil",y = "Bewertung des allgemeinen Gefallen")+
  scale_y_continuous(breaks = seq(0,100,10))


########################################## Q3, Q4, Q5
# Figure 7

library(reshape2)
Bewertung_new_long_f3_f4_f5 <- melt(Bewertung_new, id=c("Testperson", "Anteil"), measure=c("F3","F4","F5"))

ggplot(Bewertung_new_long_f3_f4_f5,aes(y=value,x=Anteil,fill=Anteil))+ # 
  geom_violin(trim = FALSE, alpha = 0.5,position=position_dodge(width=0.8))+
  stat_boxplot(geom = "errorbar",position=position_dodge(width=0.8),width=0.1)+
  geom_boxplot(width=0.3, position=position_dodge(width=0.8))+
  stat_summary(fun= mean, geom = "point",size=3,col="darkred",
               position=position_dodge(width=0.8))+
  stat_summary(fun = mean, geom = "text", col = "darkred",  size=7,
               position=position_dodge(width=0.8),
               vjust = 2, aes(label = paste(round(..y.., digits = 2))))+
  theme(text = element_text(size = 30),
        legend.text = element_text(size=30))+
  scale_fill_manual(values = c("#938EB8","#CDCBDB"))+
  labs(x="direct/indirect ratio",y = "rating of Q3, Q4 or Q5")+
  scale_y_continuous(breaks = seq(-3,3,1)) + #  part 1
  #scale_y_continuous(breaks = seq(0,100,10)) + # part 2
  facet_wrap(. ~ variable, scales="free_y", labeller=labeller(variable = 
                                                                c("F3" = "Q3: satisfaction",
                                                                  "F4" = "Q4: pleasantness",
                                                                  "F5" = "Q5: general appeal")
  )) +
  theme(legend.position = "none")
