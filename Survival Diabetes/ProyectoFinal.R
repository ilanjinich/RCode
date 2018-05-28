library(foreign)
library(magrittr)
library(dplyr)
library(survminer)
library(survival)
#library(icenReg)
#library(coxinterval)
#library(interval)
#library(readstata13)

ruta<-"C:/Users/ilan/Documents"
setwd(ruta)
file<-'adultos_cronicas.sav'

dats <- read.spss(file, to.data.frame=TRUE)

#datos<- read.dta13(paste0(getwd(),"/9ITAM/ASUPERVIVENCIA/Adultoscronicas.dta"))

 dats %<>%
  select(a301,a3025,a101m, a101h, a101bh, a101bm,a301, a302b, a3025, a701a, a701b, a1301, region_h, rural, ponde_f, edad, sexo,diabetes) %>%
  filter(!is.na(a301))  %>% 
  filter(a301!='Diabetes Gestacional') %>% 
  filter((a301 == 'si' & a3025>=20) | a301=='No' ) 



 Censurasizq<-ifelse( is.na(dats$a3025) | dats$edad>dats$a3025,1,0)
 CensuraDer<-ifelse(dats$diabetes=='Positivo',1,0)
 Tiempos<-NULL
 Tiempos<-dats$a3025*Censurasizq+dats$edad*(1-Censurasizq)*0
 Tiempos[which(is.na(Tiempos))]<-dats[which(is.na(Tiempos)),]$edad
 
 
 Tiempos2<-Tiempos*Censurasizq*CensuraDer+dats$edad*(1-Censurasizq)+100*(1-CensuraDer)
 for (i in 1:length(Tiempos2)){if(Tiempos2[i]==100){Tiempos2[i]<-NA}}
 
 Censura<-ifelse(Censurasizq==0,2,1)
 Censura<-ifelse(CensuraDer==1,1,0)*Censura
 
 
t<-Surv(Tiempos,Tiempos2,type='interval2')
 
Tiempoini<-NULL
Tiempofin<- NULL
Censura<- ifelse(Censurasizq==0,2, ifelse(CensuraDer==0,0,1))
for(i in 1:length(Censura)){
  Tiempoini[i]<- ifelse(Censura[i]==0,dats$edad[i],ifelse(Censura[i]==1,dats$a3025[i],NA))
}

for(i in 1:length(Censura)){
  Tiempofin[i]<- ifelse(Censura[i]==0,NA,ifelse(Censura[i]==1,dats$a3025[i],dats$edad[i]))
}

t<-Surv(Tiempos,Tiempos2,type='interval2')
t<- Surv(Tiempoini, Tiempofin, type="interval2")
xfit<-survfit(t~1)
xfit<-survfit(t~1,data=dats,weights = dats$ponde_f)
xfit
plot(xfit)
summary(xfit)


#Limpiar NA en atributos


length(which(dats$a701a == 'no sabe' | is.na(dats$a701a)))
conj1<- which(dats$a701a == 'no sabe' | is.na(dats$a701a))
Tiempoini2<- Tiempoini[-conj1]
Tiempofin2<- Tiempofin[-conj1]
dats<- dats[-conj1,]
Censura<-Censura[-conj1]


conj2<- which(dats$a701b == 'no sabe' | is.na(dats$a701b))
Tiempoini2<- Tiempoini2[-conj2]
Tiempofin2<- Tiempofin2[-conj2]
dats<- dats[-conj2,]
Censura<-Censura[-conj2]


conj3<- which(dats$a1301 == 'ns/nr' | is.na(dats$a1301))
Tiempoini2<- Tiempoini2[-conj3]
Tiempofin2<- Tiempofin2[-conj3]
dats<- dats[-conj3,]
Censura<-Censura[-conj3]

#dats<- dats[-which(is.na(dats$rural)),]
#dats<- dats[-which(is.na(dats$sexo)),] 

#RegresiÃ³n ParamÃ©trica
t2<- Surv(Tiempoini2, Tiempofin2, type="interval2")

regweib<- survreg(t2~
                    #a701a #+
                    #a701b +
                    #a1301 #+
                    rural #+
                    #sexo,
                  ,data=dats,
                  dist = "t",
                  weights = ponde_f,
                  control = list(maxiter=900))

summary(regweib)



ggsurvplot(xfit,data=dats,
           #risk.table = TRUE,
           title = "Kaplan-Meier",
           xlab = "Edad", ylab = "Supervivencia",palette='blue')
ggsave('KM1.png')

ggsurvplot(xfit,data=dats,
           #risk.table = TRUE,
           title = "Riesgo Acumulado",
           xlab = "Edad", ylab = "Riesgo acumulado",palette='blue',
           fun = "cumhaz")
ggsave('NA1.png')

ggsurvplot(survfit(t2~sexo,data=dats,weights = dats$ponde_f),
           data=dats,
           #risk.table = TRUE,
           #pval = TRUE,
           title = "Kaplan-Meier por sexo",
           xlab = "Edad", ylab = "Supervivencia")
ggsave('KM2.png')

ggsurvplot(survfit(t2~sexo,data=dats,weights = dats$ponde_f),
           data=dats,
           #risk.table = TRUE,
           #pval = TRUE,
           title = "Riesgo Acumulado por sexo",
           xlab = "Edad", ylab = "Riesgo acumulado",
           fun = "cumhaz")
ggsave('NA2.png')


ggsurvplot(survfit(t2~rural,data=dats,weights = dats$ponde_f),
           data=dats,
           #risk.table = TRUE,
           main = "Kaplan-Meier por categoría rural",
           xlab = "Edad", ylab = "Supervivencia")
ggsave('KM3.png')

ggsurvplot(survfit(t2~rural,data=dats,weights = dats$ponde_f),
           data=dats,
           #risk.table = TRUE,
           main = "Riesgo Acumulado por categoría rural",
           xlab = "Edad", ylab = "Riesgo acumulado",
           fun = "cumhaz")
ggsave('NA3.png')

x<-ifelse(dats$a1301=='si','si','no')
ggsurvplot(survfit(t2~x,data=dats,weights = dats$ponde_f),
           data=dats,
           #risk.table = TRUE,
           title = "Kaplan-Meier separando por fumadores o no fumadores",
           xlab = "Edad", ylab = "Supervivencia")
ggsave('KM4.png')

x<-ifelse(dats$a1301=='si','si','no')
ggsurvplot(survfit(t2~x,data=dats,weights = dats$ponde_f),
           data=dats,
           #risk.table = TRUE,
           title = "Riesgo Acumulado separando por fumadores o no fumadores",
           xlab = "Edad", ylab = "Riesgo acumulado",
           fun = "cumhaz")
ggsave('NA4.png')


ggsurvplot(survfit(t2~a701a,data=dats,weights = dats$ponde_f),
           data=dats,
           #risk.table = TRUE,
           title = "Kaplan-Meier separando entre los que su padre tuvo diabetes y los que no",
           xlab = "Edad", ylab = "Supervivencia")
ggsave('KM5.png')

ggsurvplot(survfit(t2~a701a,data=dats,weights = dats$ponde_f),
           data=dats,
           #risk.table = TRUE,
           title = "Riesgo Acumulado separando entre los que su padre tuvo diabetes y los que no",
           xlab = "Edad", ylab = "Riesgo acumulado",
           fun = "cumhaz")
ggsave('NA5.png')

ggsurvplot(survfit(t2~a701b,data=dats,weights = dats$ponde_f),
           data=dats,
           #risk.table = TRUE,
           title = "Kaplan-Meier separando entre los que su madre tuvo diabetes y los que no",
           xlab = "Edad", ylab = "Supervivencia")
ggsave('KM6.png')

ggsurvplot(survfit(t2~a701b,data=dats,weights = dats$ponde_f),
           data=dats,
           #risk.table = TRUE,
           title = "Riesgo Acumulado separando entre los que su madre tuvo diabetes y los que no",
           xlab = "Edad", ylab = "Riesgo acumulado",
           fun = "cumhaz")
ggsave('NA6.png')

y<-ifelse(dats$a701a=='si' & dats$a701a=='si','si',
          ifelse(dats$a701a=='si','solo un padre',
                 ifelse(dats$a701b=='si','solo un padre','no')))

ggsurvplot(survfit(t2~y,data=dats,weights = dats$ponde_f),
           data=dats,
           #risk.table = TRUE,
           title = "Kaplan-Meier considerando si ambos padres tuvieron diabetes",
           xlab = "Edad", ylab = "Supervivencia")
ggsave('KM7.png')

ggsurvplot(survfit(t2~y,data=dats,weights = dats$ponde_f),
           data=dats,
           #risk.table = TRUE,
           title = "Riesgo Acumulado considerando si ambos padres tuvieron diabetes",
           xlab = "Edad", ylab = "Riesgo acumulado",
           fun = "cumhaz")
ggsave('NA7.png')

dats<-cbind(dats,y)
dats<-cbind(dats, Censura)
dats<-cbind(dats,Tiempoini2,Tiempofin2)


dats %<>%
  filter(Censura!=2)

colnames(dats)[19]<-'Tiempo1'
colnames(dats)[20]<-'Tiempo2'
colnames(dats)[18]<-'censura'
colnames(dats)[17]<-'gen'
attach(dats)
t3<- Surv(Tiempo1, censura)
detach(dats)
xfit<-survfit(t3~sexo,data=dats,weights = dats$ponde_f)
xfit
plot(xfit)
summary(xfit)


fit <- coxph(t3 ~  sexo+gen+rural , data = dats,weights = dats$ponde_f,robust=T)
summary(fit)
ftest<-cox.zph(fit)
ggcoxzph(ftest)

ggcoxdiagnostics(fit,
                 type = "deviance",
                 ox.scale = "linear.predictions")

ggcoxdiagnostics(fit,
                 type = "schoenfeld",
                 ox.scale = "time")

ggforest(fit)

ggcoxadjustedcurves(fit)

coxsnellres=dats$censura-resid(fit,type="martingale")
fitres=survfit(coxph(Surv(coxsnellres,dats$censura)~1,method='breslow'),type='aalen')
png(filename="exp.png")
plot(fitres$time,-log(fitres$surv),type='s',xlab='Cox-Snell Residuals', 
     ylab='Estimated Cumulative Hazard Function',
     main='Análisis de supuestos')
abline(0,1,col='red',lty=2)
dev.off()

