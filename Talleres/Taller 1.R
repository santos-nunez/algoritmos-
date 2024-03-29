#-------------------------------------------------------------------------------
#                                   Taller 1
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
#                                  Problema 1
#-------------------------------------------------------------------------------

Almacenar <- function(x){
  f<-x
  y=0
  while(f >= 1)
  {
    f<-f/10
    y<-y+1
  }
  
  z<-f*10000
  m<-z%%1
  z<-z-m
  z<-z/10000
  f<-z*10^y
  E<-abs(f-x)
  
  e<-E/f
  
  cat("El numero almacenado es", f, "con un error de redondeo absoluto de", E, "y un error de redondeo relativo de", e)
  
}

Almacenar(536.78)

#-------------------------------------------------------------------------------
#                                  Problema 2
#-------------------------------------------------------------------------------


RaizCuadrada <- function (n,E,x){
  
  y<-(x+(n/x))/2
  c<-0
  while((abs(x-y))>E)
  {
    x<-y
    y<-(x+(n/x))/2
    c<-c+1
    cat("Iteracion:", c, ", Valor de y:", y, "\n")
  }
  
  cat("\nLa formula converge en la iteracion", c, "con el valor de", y, "para la raiz de", n, "y el valor de y*y es", y*y)
  
  f<-function(x) x^(1/2)
  
  plot(f, xlim = c(0, n+5), ylim=c(0, n/2), col = "blue", ylab = "y", main="RaizCuadrada")
  abline(v= n, h = y, col= "red")
  points(n,y, col = "green")
  
}

RaizCuadrada(7, 1e-8, 2)

#-------------------------------------------------------------------------------
#                                  Problema 3
#-------------------------------------------------------------------------------


AproximacionEuler<- function(x)
{
  m<-0
  p<-0
  while (m<=5)
  {
    p<-p+((x^m)/factorial(m))
    m<-m+1
  }
  p<- p+(((x^m)/factorial(m))*exp(x))
  signif(p, 5)
}


AproximacionEuler(0.5)

#-------------------------------------------------------------------------------
#                                  Problema 4
#-------------------------------------------------------------------------------


CalculoError_AyR<- function(v, Ev, t, Et)
{
  
  d<-v*t
  
  Ed<-v*Ev+t*Et
  
  ed<-Ev/v+Et/t
  
  cat("La distancia recorrida esta entre", d-Ed, "y", d+Ed, "debido a su error Absoluto de:", Ed, "y el error relativo es de:", ed*100, "%")
  
}

CalculoError_AyR(4, 0.1, 5, 0.1)

#-------------------------------------------------------------------------------
#                                  Problema 5
#-------------------------------------------------------------------------------


ResolverPolinomio <- function(x0)
{
  x2<-x0*x0
  x4<-x2*x2
  resultado<-2*x4+3*(x0-x2)-4
  cat("El resultado del polinomio evaluado en x =", x0, "es:", resultado)
}

ResolverPolinomio(-2)

