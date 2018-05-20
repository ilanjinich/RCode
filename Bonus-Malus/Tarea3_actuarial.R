Clases<-c(0,.30,.50)
Asegurados<-c(.05,.15)
NumPol<-10000
Proporciones<-c(.6,.4)

#Parte 1
#cantidades máximas a absorver
Prima<-550
CMA_0<-0
for (i in 1:(length(Clases)-1)){
  print(i)
  CMA_0<-Prima*(1-Clases[i])-Prima*(1-Clases[i+1])+CMA_0
  
}
CMA_0

CMA_.3<-0
for (i in 1:(length(Clases)-1)){
  print(i)
  CMA_.3<-Prima*(1-Clases[i])-Prima*(1-Clases[3])+CMA_.3
  
}
CMA_.3


CMA_.5<-0
for (i in 1:(length(Clases)-1)){
  print(i)
  CMA_.5<-Prima*(1-Clases[i+1])-Prima*(1-Clases[3])+CMA_.5
  
}
CMA_.5

CMA<-CMA_0+CMA_.3+CMA_.5
CMA

#Parte 2
#Probabilidad de reclamación

mu<-6
sigma2<-4.013
sigma<-sqrt(sigma2)

A=matrix(nrow=3,ncol=2)
dimnames(A)=list(c('0%','30%','50%'),c('Buenos','Malos'))

A[1,1]<-(1-pnorm(log(CMA_0),mu,sigma))*Asegurados[1]
A[1,2]<-(1-pnorm(log(CMA_0),mu,sigma))*Asegurados[2]
A[2,1]<-(1-pnorm(log(CMA_.3),mu,sigma))*Asegurados[1]
A[2,2]<-(1-pnorm(log(CMA_.3),mu,sigma))*Asegurados[2]
A[3,1]<-(1-pnorm(log(CMA_.5),mu,sigma))*Asegurados[1]
A[3,2]<-(1-pnorm(log(CMA_.5),mu,sigma))*Asegurados[2]


#Parte 3
#Matrices de transición

Buenos=matrix(nrow=3,ncol=3)
dimnames(Buenos)=list(c('0%','30%','50%'),c('0%','30%','50%'))
Buenos[1,1]<-A[1,1]
Buenos[1,2]<-1-A[1,1]
Buenos[1,3]<-Buenos[2,2]<-Buenos[3,1]<-0
Buenos[2,1]<-A[2,1]
Buenos[2,3]<-1-A[2,1]
Buenos[3,2]<-A[3,1]
Buenos[3,3]<-1-A[3,1]


Malos=matrix(nrow=3,ncol=3)
dimnames(Malos)=list(c('0%','30%','50%'),c('0%','30%','50%'))
Malos[1,1]<-A[1,2]
Malos[1,2]<-1-A[1,2]
Malos[1,3]<-Malos[2,2]<-Malos[3,1]<-0
Malos[2,1]<-A[2,2]
Malos[2,3]<-1-A[2,2]
Malos[3,2]<-A[3,2]
Malos[3,3]<-1-A[3,2]

#Verificamos que la matriz sea estocastica
rowSums(Buenos)
rowSums(Malos)

#Parte 4
#Determina el número de asegurados en cada categoría una vez que se ha estabilizado
#el sistema
library("markovchain")


MCBuenos <- new("markovchain", states = c("0%", "30%", "50%"),
                  transitionMatrix =Buenos,
                  name = "Asegurados Buenos")
plot(MCBuenos)

MCMalos <- new("markovchain", states = c("0%", "30%", "50%"),
                transitionMatrix =Malos,
                name = "Asegurados Buenos")

plot(MCMalos)

NEAseguradosBuenos<-steadyStates(MCBuenos)*NumPol*Proporciones[1]
NEAseguradosMalos<-steadyStates(MCMalos)*NumPol*Proporciones[2]

NEXCategoria<-NEAseguradosBuenos+NEAseguradosMalos
rowSums(NEXCategoria)

#Parte 5
#Monto Esperado de Siniestros
MontoEsperado<-exp(mu+sigma2/2)*(Asegurados[1]*Proporciones[1]+
                                   Asegurados[2]*Proporciones[2])*NumPol
#Parte 6
#Prima total
PrimaTotal<-0
for(i in 1:length(Clases)){
PrimaTotal<- Prima*((NEXCategoria[i]*(1-Clases[i])))+PrimaTotal
}


#Parte 7
#Como se mueven en el tiempo
initialState <- c(1, 0, 0)

#Buenos
for(i in 1:8){
  print(i)
print((t(MCBuenos) ^ (i)) * initialState*Proporciones[1]*NumPol)
}

#Malos
for(i in 1:8){
  print(i)
  print((t(MCMalos) ^ (i)) * initialState*Proporciones[2]*NumPol)
}


#Pregunta 2
#Parte 1
#cantidades máximas a absorver
Prima<-550
CMA_0<-0
for (i in 1:(length(Clases)-1)){
  print(i)
  CMA_0<-Prima*(1-Clases[i])-Prima*(1-Clases[i+1])+CMA_0
  
}
CMA_0

CMA_.3<-0
for (i in 1:(length(Clases)-1)){
  print(i)
  CMA_.3<-Prima*(1-Clases[i])-Prima*(1-Clases[3])+CMA_.3
  
}
CMA_.3


CMA_.5<-0
for (i in 1:(length(Clases)-1)){
  print(i)
  CMA_.5<-Prima*(1-Clases[i])-Prima*(1-Clases[3])+CMA_.5
  
}
CMA_.5

CMA<-CMA_0+CMA_.3+CMA_.5
CMA

#Parte 2
#Probabilidad de reclamación

mu<-6
sigma2<-4.013
sigma<-sqrt(sigma2)

A=matrix(nrow=3,ncol=2)
dimnames(A)=list(c('0%','30%','50%'),c('Buenos','Malos'))

A[1,1]<-(1-pnorm(log(CMA_0),mu,sigma))*Asegurados[1]
A[1,2]<-(1-pnorm(log(CMA_0),mu,sigma))*Asegurados[2]
A[2,1]<-(1-pnorm(log(CMA_.3),mu,sigma))*Asegurados[1]
A[2,2]<-(1-pnorm(log(CMA_.3),mu,sigma))*Asegurados[2]
A[3,1]<-(1-pnorm(log(CMA_.5),mu,sigma))*Asegurados[1]
A[3,2]<-(1-pnorm(log(CMA_.5),mu,sigma))*Asegurados[2]


#Parte 3
#Matrices de transición

Buenos=matrix(nrow=3,ncol=3)
dimnames(Buenos)=list(c('0%','30%','50%'),c('0%','30%','50%'))
Buenos[1,1]<-A[1,1]
Buenos[1,2]<-1-A[1,1]
Buenos[1,3]<-Buenos[2,2]<-Buenos[3,2]<-0
Buenos[2,1]<-A[2,1]
Buenos[2,3]<-1-A[2,1]
Buenos[3,1]<-A[3,1]
Buenos[3,3]<-1-A[3,1]


Malos=matrix(nrow=3,ncol=3)
dimnames(Malos)=list(c('0%','30%','50%'),c('0%','30%','50%'))
Malos[1,1]<-A[1,2]
Malos[1,2]<-1-A[1,2]
Malos[1,3]<-Malos[2,2]<-Malos[3,2]<-0
Malos[2,1]<-A[2,2]
Malos[2,3]<-1-A[2,2]
Malos[3,1]<-A[3,2]
Malos[3,3]<-1-A[3,2]

#Verificamos que la matriz sea estocastica
rowSums(Buenos)
rowSums(Malos)

#Parte 4
#Determina el número de asegurados en cada categoría una vez que se ha estabilizado
#el sistema
library("markovchain")


MCBuenos <- new("markovchain", states = c("0%", "30%", "50%"),
                transitionMatrix =Buenos,
                name = "Asegurados Buenos")
plot(MCBuenos)

MCMalos <- new("markovchain", states = c("0%", "30%", "50%"),
               transitionMatrix =Malos,
               name = "Asegurados Buenos")

plot(MCMalos)

NEAseguradosBuenos<-steadyStates(MCBuenos)*NumPol*Proporciones[1]
NEAseguradosMalos<-steadyStates(MCMalos)*NumPol*Proporciones[2]

NEXCategoria<-NEAseguradosBuenos+NEAseguradosMalos
rowSums(NEXCategoria)

#Parte 5
#Monto Esperado de Siniestros
MontoEsperado<-exp(mu+sigma2/2)*(Asegurados[1]*Proporciones[1]+
                                   Asegurados[2]*Proporciones[2])*NumPol
#Parte 6
#Prima total
PrimaTotal<-0
for(i in 1:length(Clases)){
  PrimaTotal<- Prima*((NEXCategoria[i]*(1-Clases[i])))+PrimaTotal
}


#Parte 7
#Como se mueven en el tiempo
initialState <- c(1, 0, 0)

#Buenos
for(i in 1:8){
  print(i)
  print((t(MCBuenos) ^ (i)) * initialState*Proporciones[1]*NumPol)
}

#Malos
for(i in 1:8){
  print(i)
  print((t(MCMalos) ^ (i)) * initialState*Proporciones[2]*NumPol)
}

