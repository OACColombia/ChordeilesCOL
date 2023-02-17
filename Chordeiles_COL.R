#Chordeiles
rm(list=ls())

#########
#Data from Oberholser (1914), Selander & Alvarez-Toro (1955), Eisenmann (1962),
#and measurements by FGS in ICN and OAC in IAvH-A
#########
data<-read.csv("Chordeiles_EffectSize.csv")
head(data)
#Cohen's d
attach(data)

data$s <- sqrt(var) #SD
data$d1 <- ((MeanE - MeanC1)/data$s)*((n-3)/(n-2.25))*(sqrt((n-2)/n)) #corrected by small sample size
data$d2 <- ((MeanE - MeanC2)/data$s)*((n-3)/(n-2.25))*(sqrt((n-2)/n)) #corrected by small sample size
#margin of error (w) = critical value for T * SE
#df = n-1
data$df<-n-1
#estimate tritical value for T
data$t99<-qt(p=0.01, df=data$df, lower.tail = F)
data$t95<-qt(p=0.05, df=data$df, lower.tail = F)
data$t80<-qt(p=0.2, df=data$df, lower.tail = F)

#CI lower and upper
data$CI99upp1 <- data$d1 + data$t99 * SE
data$CI99low1 <- data$d1 - data$t99 * SE

data$CI99upp2 <- data$d2 + data$t99 * SE
data$CI99low2 <- data$d2 - data$t99 * SE

data$CI95upp1 <- data$d1 + data$t95 * SE
data$CI95low1 <- data$d1 - data$t95 * SE
data$CI95upp2 <- data$d2 + data$t95 * SE
data$CI95low2 <- data$d2 - data$t95 * SE

data$CI80upp1 <- data$d1 + data$t80 * SE
data$CI80low1 <- data$d1 - data$t80 * SE

data$CI80upp2 <- data$d2 + data$t80 * SE
data$CI80low2 <- data$d2 - data$t80 * SE

library(ggplot2)

mCminorWingd1<-mean(data$d1[1:6])
mCgunlaWingd1<-mean(data$d1[7:8])

mCminorWingd2<-mean(data$d2[1:6])
mCgunlaWingd2<-mean(data$d2[7:8])


mCminorTaild1<-mean(data$d1[9:14])
mCgunlaTaild1<-mean(data$d1[15:16])

mCminorTaild2<-mean(data$d2[9:14])
mCgunlaTaild2<-mean(data$d2[15:16])


data$Measurement <- factor(data$Measurement, levels=c("Wing",
                                                      "Tail"))

data$Group <- factor(data$Group, levels=c("C. g. gundlachii",
                                          "C. g. vicinus",
                                          "C. m. panamensis",
                                          "C. m. chapmani",
                                          "C. minor **",
                                          "C. m. aserriensis",
                                          "C. minor *",
                                          "C. m. neotropicalis"))


ICN<-ggplot(data,aes(x=d1,y=Group))+
  facet_wrap(~Measurement, ncol=1)+
  geom_vline(xintercept = 0)+
  geom_vline(data=subset(data, Measurement=="Wing"),aes(xintercept = mCminorWingd1),linetype="dotted")+
  geom_vline(data=subset(data, Measurement=="Tail"),aes(xintercept = mCminorTaild1),linetype="dotted")+
  geom_vline(data=subset(data, Measurement=="Wing"),aes(xintercept = mCgunlaWingd1),linetype="dashed")+
  geom_vline(data=subset(data, Measurement=="Tail"),aes(xintercept = mCgunlaTaild1),linetype="dashed")+
  geom_linerange(aes(xmin=CI99low1,xmax=CI99upp1), color="gray", size=0.3125)+
  geom_linerange(aes(xmin=CI95low1,xmax=CI95upp1), color="gray", size=1)+
  geom_linerange(aes(xmin=CI65low1,xmax=CI65upp1), color="gray30", size=1.75)+
  geom_point(shape=15, size=2)+
  labs(x="ICN-8501",
       y="")+
  theme_classic()+
  theme(axis.text.y = element_text(face="italic"))

IAvH<-ggplot(data,aes(x=d2,y=Group))+
  geom_vline(xintercept = 0)+
  geom_vline(data=subset(data, Measurement=="Wing"),aes(xintercept = mCminorWingd2),linetype="dotted")+
  geom_vline(data=subset(data, Measurement=="Tail"),aes(xintercept = mCminorTaild2),linetype="dotted")+
  geom_vline(data=subset(data, Measurement=="Wing"),aes(xintercept = mCgunlaWingd2),linetype="dashed")+
  geom_vline(data=subset(data, Measurement=="Tail"),aes(xintercept = mCgunlaTaild2),linetype="dashed")+
  geom_linerange(aes(xmin=CI99low2,xmax=CI99upp2), color="gray", size=0.3125)+
  geom_linerange(aes(xmin=CI95low2,xmax=CI95upp2), color="gray", size=1)+
  geom_linerange(aes(xmin=CI65low2,xmax=CI65upp2), color="gray30", size=1.75)+
  geom_point(shape=15, size=2)+
  facet_wrap(~Measurement, ncol=1)+
  labs(x="IAvH-A-96",
       y="")+
  theme_classic()+
  theme(axis.text.y = element_blank())

# combine
library(gridExtra)
library(grid)
grid.arrange(arrangeGrob(ICN,IAvH,
                         ncol=2, 
                         widths = c(2,1.4),
                         left=textGrob("Group measured", rot = 90, vjust = 1.5)),
                         bottom=textGrob(expression("Cohen's"~italic(d)*" Standardized effect size"),
                                         vjust = 0.5, hjust = 0.225))
#Export pdf 6.0 * 5.5
write.csv(data, "Anexo.csv")
