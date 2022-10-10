library(psych)
library(correlation)
library(corrplot)


######## correlation matrix

sub_Fragen<-subset(Bewertung_new,select = c(F1,F2,F3,F4,F5))

correlation(sub_Fragen, include_factors=TRUE,methos="spearman")
correlation(sub_Fragen, include_factors=TRUE,methos="pearson")

# Correlation between preference Q3,Q4,Q5 and indirect CCT
sub_Fragen_CCT<-subset(group30di,select = c(F3,F4,F5,indiCCT))
correlation(sub_Fragen_CCT, include_factors=TRUE,methos="spearman")
correlation(sub_Fragen_CCT, include_factors=TRUE,methos="pearson")

sub_Fragen_CCT<-subset(group50di,select = c(F3,F4,F5,indiCCT))
correlation(sub_Fragen_CCT, include_factors=TRUE,methos="spearman")
correlation(sub_Fragen_CCT, include_factors=TRUE,methos="pearson")

# Spearman
Kor<-cor(sub_Fragen,method = "spearman")
ggcorrplot(Kor, 
           colors = c("lightsteelblue","white", "royalblue4"),
           lab =TRUE,lab_size=10,
           legend.title = "rho", type="lower")+
  theme(text = element_text(size = 20),
        legend.text = element_text(size=20), axis.text.x=element_text(size=20), 
        axis.text.y=element_text(size=20), panel.grid = element_blank())

# Pearson
Kor_pear<-cor(sub_Fragen,method = "pearson")
ggcorrplot(Kor_pear, 
           colors = c("lightsteelblue","white", "royalblue4"),
           lab =TRUE,lab_size=10,
           legend.title = "rho", type="lower")+
  theme(text = element_text(size = 20),
        legend.text = element_text(size=20), axis.text.x=element_text(size=20), 
        axis.text.y=element_text(size=20), panel.grid = element_blank())


# correlation between Q1 and illuminance Ev
# Ev
cor.test(Bewertung_new$F1,Bewertung_new$Ev,method = "spearman")# 0.3050732, p-value < 2.2e-16
cor.test(Bewertung_new$F1,Bewertung_new$Ev,method = "pearson")# 0.3252055, p-value < 2.2e-16
# Ee
cor.test(Bewertung_new$F1,Bewertung_new$Ee,method = "spearman")# 0.2633756, p-value = 3.438e-16
cor.test(Bewertung_new$F1,Bewertung_new$Ee,method = "pearson")# 0.2696824, p-value < 2.2e-16


