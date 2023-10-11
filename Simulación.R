#### Codigo de flujos de fondos. Seguro de Retiro vs S&P 500 (Cedears) ----
set.seed(123)
Flujos_FinalesPromedio_SP = matrix(NA,nrow=35,ncol=10)
Flujos_FinalesPromedio_SR = matrix(NA,nrow=35,ncol=10)
SR_SP = matrix(rep(0,10*13),nrow=10,ncol=13)
colnames(SR_SP) = c("Aporte","Pr(SP>SR)","Min SR","Min SP","Max SR","Max SP","Saldo Final Promedio - SR","Saldo Final Promedio - SP",
                    "Pr(mu*0.8 < x < mu*1.2) - SR","Pr(mu*0.8 < x < mu*1.2) - SP)",
                    "Tasa Neta Promedio - SR", "Tasa Neta Promedio - SP", "Pr(Min(SR[35])>SP)")
Rendimiento = matrix(NA,nrow=10,ncol=4) #para calcular media y varianza distr normal tasa neta de imp SP y SR
for(q in 1:10){
anios = 35
Flujos_Finales_SP = matrix(NA,nrow=anios,ncol = 10000)
Flujos_Finales_SR = matrix(NA,nrow=anios,ncol = 10000)
Aporte = 250*q
for(j in 1:10000){
  Flujos_SP = matrix(rep(NA,anios*5), ncol=5)
  colnames(Flujos_SP) = c("t", "Saldo", "Intereses", "Impuestos", "Fondo Neto")
  Flujos_SR = matrix(rep(NA,anios*4), ncol=4)
  colnames(Flujos_SR) = c("t", "Saldo", "Intereses", "Saldo Final")
  for(i in 0:(anios-1)){
    Flujos_SR[i+1,1]=i
    Flujos_SP[i+1,1]=i
  }
for(i in 1:35){
  #CEDEAR S&P500
  Rend_SP = qnorm(runif(1),0.085662379,0.050322057,lower.tail = TRUE)
  if(i == 1){
    Flujos_SP[i,2] = Aporte
    Flujos_SP[i,3] = Flujos_SP[i,2]*Rend_SP
    Flujos_SP[i,4] = (Flujos_SP[i,2]+Flujos_SP[i,3])*0.0225
    Flujos_SP[i,5] = Flujos_SP[i,2]+Flujos_SP[i,3]-Flujos_SP[i,4]
  }
  else{
    Flujos_SP[i,2] = Flujos_SP[(i-1),5]+Aporte
    Flujos_SP[i,3] = Flujos_SP[i,2]*Rend_SP
    Flujos_SP[i,4] = (Flujos_SP[i,2]+Flujos_SP[i,3])*0.0225
    Flujos_SP[i,5] = Flujos_SP[i,2]+Flujos_SP[i,3]-Flujos_SP[i,4]
  }
  #SEGURO DE RETIRO
  Rend_SR = max(qlnorm(runif(1),-2.929173789,0.2397986470,lower.tail = TRUE),0.02)
  if(i == 1){
    Flujos_SR[i,2] = Aporte
    Flujos_SR[i,3] = Flujos_SR[i,2]*Rend_SR+43.75
    Flujos_SR[i,4] = Flujos_SR[i,2]+Flujos_SR[i,3]
  }
  else{
    Flujos_SR[i,2] = Flujos_SR[(i-1),4]+Aporte
    Flujos_SR[i,3] = Flujos_SR[i,2]*Rend_SR+43.75
    Flujos_SR[i,4] = Flujos_SR[i,2]+Flujos_SR[i,3]
  }
}
Flujos_Finales_SP[,j] = t(Flujos_SP[,5])
Flujos_Finales_SR[,j] = t(Flujos_SR[,4])
if(Flujos_SP[35,5]>Flujos_SR[35,4]){
  SR_SP[q,2] = SR_SP[q,2]+1
}
}
for(i in 1:35){
  Flujos_FinalesPromedio_SP[i,q] = mean(Flujos_Finales_SP[i,])
  Flujos_FinalesPromedio_SR[i,q] = mean(Flujos_Finales_SR[i,])
}
SR_SP[q,1] = 250*q
SR_SP[q,3] = min(Flujos_Finales_SR[35,])
SR_SP[q,4] = min(Flujos_Finales_SP[35,])
SR_SP[q,5] = max(Flujos_Finales_SR[35,])
SR_SP[q,6] = max(Flujos_Finales_SP[35,])
SR_SP[q,7] = mean(Flujos_Finales_SR[35,])
SR_SP[q,8] = mean(Flujos_Finales_SP[35,])
for(i in 1:10000){
  if((Flujos_Finales_SR[35,i]>Flujos_FinalesPromedio_SR[35,q]*0.8)*(Flujos_Finales_SR[35,i]<Flujos_FinalesPromedio_SR[35,q]*1.2)){
    SR_SP[q,9]=SR_SP[q,9]+1
  }
  if((Flujos_Finales_SP[35,i]>Flujos_FinalesPromedio_SP[35,q]*0.8)*(Flujos_Finales_SP[35,i]<Flujos_FinalesPromedio_SP[35,q]*1.2)){
    SR_SP[q,10]=SR_SP[q,10]+1
  }
}
for(i in 1:10000){
  if(Flujos_Finales_SP[35,i]<SR_SP[q,3]){
    SR_SP[q,13]=SR_SP[q,13]+1
  }
}
Rendimiento_SP_aux=NA
Rendimiento_SR_aux=NA
for (i in 1:34){
  for (s in 1:10000){
    n=(i-1)*10000
    Rendimiento_SP_aux[s+n]=(Flujos_Finales_SP[i+1,s]/(Flujos_Finales_SP[i,s]+Aporte))-1
    Rendimiento_SR_aux[s+n]=(Flujos_Finales_SR[i+1,s]/(Flujos_Finales_SR[i,s]+Aporte))-1 
    }
}
Rendimiento[q,1]=mean(Rendimiento_SP_aux)
Rendimiento[q,2]=sd(Rendimiento_SP_aux)
Rendimiento[q,3]=mean(Rendimiento_SR_aux)
Rendimiento[q,4]=sd(Rendimiento_SR_aux)
}
View(Flujos_FinalesPromedio_SR)
View(Flujos_FinalesPromedio_SP)
SR_SP[,2]=SR_SP[,2]/10000
SR_SP[,9]=SR_SP[,9]/10000
SR_SP[,10]=SR_SP[,10]/10000
SR_SP[,13]=SR_SP[,13]/10000
View(SR_SP)



Raiz_Biseccion = function(a,b,TOL,N){
  #Error si no se cumplen los criterios previos ----
  if(Polinomio(a)*Polinomio(b)>= 0){
    return(paste("f(a) y f(b) deben tener signos opuestos para, mediante Teorema de Bolzano, poder asumir la existencia de una raiz"))
  }
  if(b<a){
    return(paste("b tiene que ser mayor que a"))
  }
  #Comenzar a iterar ----
  for(i in 1:N){
    p = (a+b)/2
    FP = Polinomio(p)
    FA = Polinomio(a)
    if(FP == 0 | (b-a)/2 < TOL){ 
      #Por el planteo mismo del problema, nunca b va a ser menor que a,
      #por lo que no hace falta agregar la funcion abs en la comparacion con la tolerancia
      e = abs((p-p0)/p)
      resultado = data.frame(p,i,e)
      names(resultado) = c('Raiz Aproximada', 'Iteraciones', 'Error Relativo')
      return(resultado)
    }
    if(sign(FA)*sign(FP)>0){
      a = p
    } else{
      b = p
    }
    p0=p
  }
  return(paste("El metodo fracaso despues de ", N, " iteraciones. El ultimo p fue p=",p,".", sep = ""))
}
j=1
  for(i in 1:10){
    C = SR_SP[i,1]
    VF = SR_SP[i,8-j]
  Polinomio = function(x){
    f = C*((((1+x)^35)-1)/x)*(1+x)-VF
    return(f)
  }
  SR_SP[i,12-j]=Raiz_Biseccion(0.01,0.15,0.00000001,40)[1,1]
  }
j=0
  for(i in 1:10){
  C = SR_SP[i,1]
  VF = SR_SP[i,8-j]
  Polinomio = function(x){
    f = C*((((1+x)^35)-1)/x)*(1+x)-VF
    return(f)
  }
  SR_SP[i,12-j]=Raiz_Biseccion(0.01,0.15,0.00000001,40)[1,1]
}

#GRAFICO FLUJOS PROMEDIOS



t=seq(1:35)
plot(t,Flujos_FinalesPromedio_SP[,4], type="l", col = "blue", lwd=3, ylim=c(0,max(max(Flujos_FinalesPromedio_SP),max(Flujos_FinalesPromedio_SR))))
lines(t,Flujos_FinalesPromedio_SR[,4], col = "light blue", lwd=3)