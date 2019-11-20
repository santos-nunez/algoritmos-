#-------------------------------------------------------------------------------
#                           Ejercicios raiz con metodos
#-------------------------------------------------------------------------------

fe<-function(x)
{
  signif(exp(1), 5)^x
}

fpi<-function(x)
{
  signif(pi,5)*x
}

f1<-function(x)
{
  signif(exp(1), 5)^x-signif(pi,5)*x
}

f1p<-function(x)
{
  signif(exp(1), 5)^x-signif(pi,5)
}

#----------------------------------------
#             Posicion Falsa
#----------------------------------------

falsaPosicion <- function(Fx, F1x, a, b, err) 
{
  
  plot(fe, xlim = c(-2,2), ylim = c(0,6), col = "blue", main = "Grafica de las Funciones", sub = "Posicion Falsa", xlab = "x", ylab = "y")
  par(new=TRUE)
  curve(fpi, type = "l", col="green", axes=FALSE, ylab = "y")
  par(new=FALSE)
  
  plot(Fx, xlim = c(-0.5,5), ylim = c(-2,5), col = "blue", main = "Grafica de la Función (resta)", sub = "Posicion Falsa", xlab = "x", ylab = "y")
  abline(h=0, v=0, col="red")
  
  m<-c()
  y<-c()
  iteraciones<-c()
  
  it = 0
  error = 1
  while (error > err) {
    
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
    
    e2<-error
    error = abs(Fx(x)/F1x(x))
    
    if(it>=1)
    {
      m<-c(m,e2)
      y<-c(y,error)
    }
    
    cat("X=",x,"\t","E=",error,"\t","Iteracion=",it,"\n")
    
    it = it + 1
  }
  
  points(x,0)
  
  plot(Fx, xlim = c(0,max(m)), ylim = c(0,max(y)), col = "white", main = "Errores(i) vs Errores(i+1)", sub = "Posicion Falsa", xlab = "Errores(i)", ylab = "Errores(i+1)")
  lines(m, y, type = "l")
  
  m<-c(m,y[it-1])
  iteraciones<-c(1:it)
  plot(Fx, xlim = c(0,iteraciones[it]), ylim = c(0,m[1]), col = "white", main = "Iteraciones vs Errores", sub = "Posicion Falsa", xlab = "Iteraciones", ylab = "Errores")
  lines(iteraciones, m, type = "l")
  
}

falsaPosicion(f1,f1p,0, 1, 10e-8)
falsaPosicion(f1,f1p,1, 2, 10e-8)
