library(psych)
library(ggplot2)
library(ggpubr)
library(ggcorrplot)
library(corrplot)
library(RColorBrewer)
library(readxl)
library(dplyr)
library(data.table)
library(FSA)


Bewertung <- read_excel("./data/Bewertung.xlsx")
Bewertung_new <-data.frame(Bewertung, stringsAsFactors = TRUE)
Messung <- read_excel("./data/gesamtKombi.xlsx")
Messung_dt<-as.data.table(Messung)
Ev <- Messung_dt[1,2:33]
Ee <- Messung_dt[4,2:33]
Rf <- Messung_dt[26,2:33]
Ra <- Messung_dt[29,2:33]


group50indi=filter(Bewertung_new,indiEe==50)
group70indi=filter(Bewertung_new,indiEe==70)
group6500=filter(Bewertung_new,indiCCT==6500)
group7500=filter(Bewertung_new,indiCCT==7500)
group9000=filter(Bewertung_new,indiCCT==9000)
group11000=filter(Bewertung_new,indiCCT==11000)
group14000=filter(Bewertung_new,indiCCT==14000)
group18000=filter(Bewertung_new,indiCCT==18000)
group24000=filter(Bewertung_new,indiCCT==24000)
group30000=filter(Bewertung_new,indiCCT==30000)

group30di=filter(Bewertung_new,diEe==30)
group50di=filter(Bewertung_new,diEe==50)
group4000=filter(Bewertung_new,diCCT==4000)
group5500=filter(Bewertung_new,diCCT==5500)

L_6k5_18k=filter(Bewertung_new,indiCCT!=c(24000,30000))
L_24k_30k=filter(Bewertung_new,indiCCT==c(24000,30000))


describeBy(Bewertung_new$F1,Bewertung_new$IndiEe)
describeBy(Bewertung_new$F1,Bewertung_new$IndiCCT)


### Normal distribution
# H0 normal distributed
# Q1
shapiro.test(Bewertung$F1) # p-value < 0.00000000000000022
ggplot(Bewertung, aes(x = F1)) + 
  geom_histogram(aes(y =..density..),
                 breaks = seq(-3, 3, by = 1), 
                 colour = "black", 
                 fill = "white") +
  stat_function(fun = dnorm, args = list(mean = mean(Bewertung$F1), sd = sd(Bewertung$F1)))

# Q2
shapiro.test(Bewertung$F2) # p-value < 0.00000000000000022
ggplot(Bewertung, aes(x = F2)) + 
  geom_histogram(aes(y =..density..),
                 breaks = seq(-3, 3, by = 1), 
                 colour = "black", 
                 fill = "white") +
  stat_function(fun = dnorm, args = list(mean = mean(Bewertung$F2), sd = sd(Bewertung$F2)))

# Q3
shapiro.test(Bewertung$F3) # p-value < 0.00000000000000022
ggplot(Bewertung, aes(x = F3)) + 
  geom_histogram(aes(y =..density..),
                 breaks = seq(-3, 3, by = 1), 
                 colour = "black", 
                 fill = "white") +
  stat_function(fun = dnorm, args = list(mean = mean(Bewertung$F3), sd = sd(Bewertung$F3)))

# Q4
shapiro.test(Bewertung$F4) # p-value < 0.00000000000000022 
ggplot(Bewertung, aes(x = F4)) + 
  geom_histogram(aes(y =..density..),
                 breaks = seq(-3, 3, by = 1), 
                 colour = "black", 
                 fill = "white") +
  stat_function(fun = dnorm, args = list(mean = mean(Bewertung$F4), sd = sd(Bewertung$F4)))

# Q5
shapiro.test(Bewertung$F5) # p-value = 0.000000000002257
ggplot(Bewertung, aes(x = F5)) + 
  geom_histogram(aes(y =..density..),
                 breaks = seq(0, 100, by = 1), 
                 colour = "black", 
                 fill = "white") +
  stat_function(fun = dnorm, args = list(mean = mean(Bewertung$F5), sd = sd(Bewertung$F5)))



### mixed linear models

library(ez)
library(pastecs)
library(lme4)
library(nlme)
# install.packages("WRS", repos="http://R-Forge.R-project.org")
library(WRS)




# brightness table: Table 4
baseline_F1_Ev <- lme(F1~1,random=~1|Testperson,data=Bewertung_new_f, na.action=na.omit,method="ML")
EvM <- update(baseline_F1_Ev,.~.+Ev, na.action=na.omit)
Anova_F1_Ev=anova(baseline_F1_Ev,EvM)
Anova_F1_Ev
baseline_F2_Ev <- lme(F2~1,random=~1|Testperson,data=Bewertung_new_f, na.action=na.omit,method="ML")
EvM <- update(baseline_F2_Ev,.~.+Ev, na.action=na.omit)
Anova_F2_Ev=anova(baseline_F2_Ev,EvM)
Anova_F2_Ev
baseline_F3_Ev <- lme(F3~1,random=~1|Testperson,data=Bewertung_new_f, na.action=na.omit,method="ML")
EvM <- update(baseline_F3_Ev,.~.+Ev, na.action=na.omit)
Anova_F3_Ev=anova(baseline_F3_Ev,EvM)
Anova_F3_Ev
baseline_F4_Ev <- lme(F4~1,random=~1|Testperson,data=Bewertung_new_f, na.action=na.omit,method="ML")
EvM <- update(baseline_F4_Ev,.~.+Ev, na.action=na.omit)
Anova_F4_Ev=anova(baseline_F4_Ev,EvM)
Anova_F4_Ev
baseline_F5_Ev <- lme(F5~1,random=~1|Testperson,data=Bewertung_new_f, na.action=na.omit,method="ML")
EvM <- update(baseline_F5_Ev,.~.+Ev, na.action=na.omit)
Anova_F5_Ev=anova(baseline_F5_Ev,EvM)
Anova_F5_Ev

# D50/U50 column: Table 4
# factorization
group50di_f <- group50di
group50di_f$indiCCT <- as.factor(group50di_f$indiCCT)
group50di_f$diCCT <- as.factor(group50di_f$diCCT)
group50di_f$Anteil <- as.factor(group50di_f$Anteil)
group50di_f$Testperson <- as.factor(group50di_f$Testperson)
group50di_f$Ev <- as.factor(group50di_f$Ev)


baseline_F1_Ev <- lme(F1~1,random=~1|Testperson,data=group50di_f, na.action=na.omit,method="ML")
EvM <- update(baseline_F1_Ev,.~.+Ev, na.action=na.omit)
Anova_F1_Ev=anova(baseline_F1_Ev,EvM)
Anova_F1_Ev
baseline_F2_Ev <- lme(F2~1,random=~1|Testperson,data=group50di_f, na.action=na.omit,method="ML")
EvM <- update(baseline_F2_Ev,.~.+Ev, na.action=na.omit)
Anova_F2_Ev=anova(baseline_F2_Ev,EvM)
Anova_F2_Ev
baseline_F3_Ev <- lme(F3~1,random=~1|Testperson,data=group50di_f, na.action=na.omit,method="ML")
EvM <- update(baseline_F3_Ev,.~.+Ev, na.action=na.omit)
Anova_F3_Ev=anova(baseline_F3_Ev,EvM)
Anova_F3_Ev
baseline_F4_Ev <- lme(F4~1,random=~1|Testperson,data=group50di_f, na.action=na.omit,method="ML")
EvM <- update(baseline_F4_Ev,.~.+Ev, na.action=na.omit)
Anova_F4_Ev=anova(baseline_F4_Ev,EvM)
Anova_F4_Ev
baseline_F5_Ev <- lme(F5~1,random=~1|Testperson,data=group50di_f, na.action=na.omit,method="ML")
EvM <- update(baseline_F5_Ev,.~.+Ev, na.action=na.omit)
Anova_F5_Ev=anova(baseline_F5_Ev,EvM)
Anova_F5_Ev

# D30/U70 column: Table 4
group30di_f <- group30di
group30di_f$indiCCT <- as.factor(group30di_f$indiCCT)
group30di_f$diCCT <- as.factor(group30di_f$diCCT)
group30di_f$Anteil <- as.factor(group30di_f$Anteil)
group30di_f$Testperson <- as.factor(group30di_f$Testperson)
group30di_f$Ev <- as.factor(group30di_f$Ev)

baseline_F1_Ev <- lme(F1~1,random=~1|Testperson,data=group30di_f, na.action=na.omit,method="ML")
EvM <- update(baseline_F1_Ev,.~.+Ev, na.action=na.omit)
Anova_F1_Ev=anova(baseline_F1_Ev,EvM)
Anova_F1_Ev
baseline_F2_Ev <- lme(F2~1,random=~1|Testperson,data=group30di_f, na.action=na.omit,method="ML")
EvM <- update(baseline_F2_Ev,.~.+Ev, na.action=na.omit)
Anova_F2_Ev=anova(baseline_F2_Ev,EvM)
Anova_F2_Ev
baseline_F3_Ev <- lme(F3~1,random=~1|Testperson,data=group30di_f, na.action=na.omit,method="ML")
EvM <- update(baseline_F3_Ev,.~.+Ev, na.action=na.omit)
Anova_F3_Ev=anova(baseline_F3_Ev,EvM)
Anova_F3_Ev
baseline_F4_Ev <- lme(F4~1,random=~1|Testperson,data=group30di_f, na.action=na.omit,method="ML")
EvM <- update(baseline_F4_Ev,.~.+Ev, na.action=na.omit)
Anova_F4_Ev=anova(baseline_F4_Ev,EvM)
Anova_F4_Ev
baseline_F5_Ev <- lme(F5~1,random=~1|Testperson,data=group30di_f, na.action=na.omit,method="ML")
EvM <- update(baseline_F5_Ev,.~.+Ev, na.action=na.omit)
Anova_F5_Ev=anova(baseline_F5_Ev,EvM)
Anova_F5_Ev


# factorization
Bewertung_new_f <- Bewertung_new
Bewertung_new_f$indiCCT <- as.factor(Bewertung_new_f$indiCCT)
Bewertung_new_f$diCCT <- as.factor(Bewertung_new_f$diCCT)
Bewertung_new_f$Anteil <- as.factor(Bewertung_new_f$Anteil)
Bewertung_new_f$Testperson <- as.factor(Bewertung_new_f$Testperson)
Bewertung_new_f$Ev <- as.factor(Bewertung_new_f$Ev)


#str(Bewertung_new_f)
library(multcomp)


# Q1
baseline_F1 <- lme(F1~1,random=~1|Testperson,data=Bewertung_new_f, na.action=na.omit,method="ML")
anteilM <- update(baseline_F1,.~.+Anteil, na.action=na.omit)
indiM <- update(anteilM,.~.+indiCCT, na.action=na.omit)
diM <- update(indiM,.~.+diCCT, na.action=na.omit)

anteil_di <- update(diM,.~.+Anteil:diCCT, na.action=na.omit)
indi_di <- update(anteil_di,.~.+indiCCT:diCCT, na.action=na.omit)
Anteil_indi <- update(indi_di,.~.+Anteil:indiCCT, na.action=na.omit)

Anteil_indi_di <- update(Anteil_indi,.~.+Anteil:indiCCT:diCCT, na.action=na.omit)
Anova_F1=anova(baseline_F1,anteilM,indiM,diM,anteil_di, indi_di, Anteil_indi, Anteil_indi_di)
Anova_F1


summary(glht(diM, linfct = mcp(diCCT = "Tukey")), test = adjusted("holm"))
summary(glht(anteilM, linfct = mcp(Anteil = "Tukey")), test = adjusted("holm"))
summary(glht(indiM, linfct = mcp(indiCCT = "Tukey")), test = adjusted("holm"))
summary(glht(indi_di, linfct = mcp(indiCCT:diCCT = "Tukey")), test = adjusted("holm"))


baseline_F1 <- lme(F1~1,random=~1|Testperson,data=group50di_f, na.action=na.omit,method="ML")
indiM <- update(baseline_F1,.~.+indiCCT, na.action=na.omit)
diM <- update(indiM,.~.+diCCT, na.action=na.omit)

indi_di <- update(diM,.~.+indiCCT:diCCT, na.action=na.omit)

Anova_F1=anova(baseline_F1,indiM,diM, indi_di)
Anova_F1
summary(glht(indi_di, linfct = mcp(diCCT = "Tukey")), test = adjusted("holm"))


# Q2
baseline_F2 <- lme(F2~1,random=~1|Testperson,data=Bewertung_new_f, na.action=na.omit,method="ML")
anteilM <- update(baseline_F2,.~.+Anteil, na.action=na.omit)
indiM <- update(anteilM,.~.+indiCCT, na.action=na.omit)
diM <- update(indiM,.~.+diCCT, na.action=na.omit)

anteil_di <- update(diM,.~.+Anteil:diCCT, na.action=na.omit)
indi_di <- update(anteil_di,.~.+indiCCT:diCCT, na.action=na.omit)
Anteil_indi <- update(indi_di,.~.+Anteil:indiCCT, na.action=na.omit)

Anteil_indi_di <- update(Anteil_indi,.~.+Anteil:indiCCT:diCCT, na.action=na.omit)
Anova_F2=anova(baseline_F2,anteilM,indiM,diM,anteil_di, indi_di, Anteil_indi, Anteil_indi_di)
Anova_F2

summary(glht(indiM, linfct = mcp(indiCCT = "Tukey")), test = adjusted("holm"))
describeBy(Bewertung_new_f$F2, Bewertung_new_f$DiCCT)

# check which indirect CCT has a significant influence on each other
library(multcomp)
summary(glht(indiM, linfct = mcp(indiCCT = "Tukey")), test = adjusted("holm"))
summary(indiM)


# Q3
baseline_F3 <- lme(F3~1,random=~1|Testperson,data=Bewertung_new_f, na.action=na.omit,method="ML")
anteilM <- update(baseline_F3,.~.+Anteil, na.action=na.omit)
indiM <- update(anteilM,.~.+indiCCT, na.action=na.omit)
diM <- update(indiM,.~.+diCCT, na.action=na.omit)

anteil_di <- update(diM,.~.+Anteil:diCCT, na.action=na.omit)
indi_di <- update(anteil_di,.~.+indiCCT:diCCT, na.action=na.omit)
Anteil_indi <- update(indi_di,.~.+Anteil:indiCCT, na.action=na.omit)

Anteil_indi_di <- update(Anteil_indi,.~.+Anteil:indiCCT:diCCT, na.action=na.omit)
Anova_F3=anova(baseline_F3,anteilM,indiM,diM,anteil_di, indi_di, Anteil_indi, Anteil_indi_di)
Anova_F3

# Q4
baseline_F4 <- lme(F4~1,random=~1|Testperson,data=Bewertung_new_f, na.action=na.omit,method="ML")
anteilM <- update(baseline_F4,.~.+Anteil, na.action=na.omit)
indiM <- update(anteilM,.~.+indiCCT, na.action=na.omit)
diM <- update(indiM,.~.+diCCT, na.action=na.omit)

anteil_di <- update(diM,.~.+Anteil:diCCT, na.action=na.omit)
indi_di <- update(anteil_di,.~.+indiCCT:diCCT, na.action=na.omit)
Anteil_indi <- update(indi_di,.~.+Anteil:indiCCT, na.action=na.omit)

Anteil_indi_di <- update(Anteil_indi,.~.+Anteil:indiCCT:diCCT, na.action=na.omit)
Anova_F4=anova(baseline_F4,anteilM,indiM,diM,anteil_di, indi_di, Anteil_indi, Anteil_indi_di)
Anova_F4


# Q5
baseline_F5 <- lme(F5~1,random=~1|Testperson,data=Bewertung_new_f, na.action=na.omit,method="ML")
anteilM <- update(baseline_F5,.~.+Anteil, na.action=na.omit)
indiM <- update(anteilM,.~.+indiCCT, na.action=na.omit)
diM <- update(indiM,.~.+diCCT, na.action=na.omit)

anteil_di <- update(diM,.~.+Anteil:diCCT, na.action=na.omit)
indi_di <- update(anteil_di,.~.+indiCCT:diCCT, na.action=na.omit)
Anteil_indi <- update(indi_di,.~.+Anteil:indiCCT, na.action=na.omit)

Anteil_indi_di <- update(Anteil_indi,.~.+Anteil:indiCCT:diCCT, na.action=na.omit)
Anova_F5=anova(baseline_F5,anteilM,indiM,diM,anteil_di, indi_di, Anteil_indi, Anteil_indi_di)
Anova_F5

# check which indirect CCT has a significant influence on each other
summary(glht(indiM, linfct = mcp(indiCCT = "Tukey")), test = adjusted("holm"))

Anteil_indi1 <- update(diM,.~.+Anteil:indiCCT, na.action=na.omit)
summary(Anteil_indi1)

# statistic for Q3, Q4, Q5
describeBy(Bewertung_new_f$F3, Bewertung_new_f$Anteil)
describeBy(Bewertung_new_f$F4, Bewertung_new_f$Anteil)
describeBy(Bewertung_new_f$F5, Bewertung_new_f$Anteil)

describeBy(Bewertung_new_f$F3, Bewertung_new_f$DiCCT)
describeBy(Bewertung_new_f$F4, Bewertung_new_f$DiCCT)
describeBy(Bewertung_new_f$F5, Bewertung_new_f$DiCCT)



