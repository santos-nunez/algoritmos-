#-------------------------------------------------------------------------------
#                           
#integrantes:         Juan Sebastian Prado Valero
#                     Santos David Nuñez Villamil
#                     Gustavo Antonio Rivera Delgado
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
#                           Ejercicios raiz con metodos
#-------------------------------------------------------------------------------

#----------------------------------------
#             Posicion Falsa
#----------------------------------------

Fx <- function(x) ((exp(1)^x) - (pi*x))
F1x <- function(x) ((exp(1)^x)-pi)

falsaPosicion <- function(a, b, err) {
  x = seq(a,b,0.1)
  plot(x,Fx(x),type="l",col="blue")
  abline(h=0,col="blue")
  
  #x = b
  #d = (Fx(b)*a-Fx(a)*b)/(Fx(b)-Fx(a))
  it = 0
  error = 1
  while (error > err) {
    it = it + 1
    x = (Fx(b)*a-Fx(a)*b)/(Fx(b)-Fx(a))
    
    if (Fx(x) == 0) {
      break
    }
    
    if (Fx(x)*Fx(a) < 0) {
      b = x
    }
    else {
      a = x
    }
    
    error = abs(Fx(x)/F1x(x))
    
    points(rbind(c(x,0)),pch=19,cex=0.7,col="red")
    
    cat("X=",x,"\t","E=",error,"\t","Iteracion=",it,"\n")
  }
}
falsaPosicion(0, 1, 10e-8)
falsaPosicion(1, 2, 10e-8)