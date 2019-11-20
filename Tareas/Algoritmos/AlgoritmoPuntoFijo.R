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

g1 <- function(x)
{
  (exp(x)/pi)
}


#-------------------------------------
#              Punto Fijo
#-------------------------------------

PuntoFijo<-function(g, x0, tol)
{
  
  plot(fe, xlim = c(-2,2), ylim = c(0,6), col = "blue", main = "Grafica de las Funciones", sub = "PuntoFijo", xlab = "x", ylab = "y")
  par(new=TRUE)
  curve(fpi, type = "l", col="green", axes=FALSE, ylab = "y")
  par(new=FALSE)
  
  plot(f1, xlim = c(-0.5,5), ylim = c(-2,5), col = "blue", main = "Grafica de la Función (resta)", sub = "PuntoFijo", xlab = "x", ylab = "y")
  abline(h=0, v=0, col="red")
  
  x<-c()
  y<-c()
  iteraciones<-c()
  
  m<-0
  
  maxI<-100
  l<-0
  repeat
  {
    x1<-g(x0)
    m2<-m
    m<-abs(x1-x0)
    
    if(l>=1)
    {
      x<-c(x, m2/x1)
      y<-c(y, m/x1)
    }
    
    cat("Iteracion:", l, "\b, valor actual:", x1, "Error actual:", m, "\n")
    
    l<-l+1
    
    if( m<tol || l>maxI )
    {
      break;
    }
    
    x0<-x1
    
  }
  
  if(m>tol)
  {
    cat("No hubo convergencia")
  }
  else
  {
    cat("x* es aproximadamente", x1, "con error menor que", tol)
  }
  
  points(x1,0)
  
  plot(g, xlim = c(0,x[1]), ylim = c(0,y[1]), col = "white", main = "Errores(i) vs Errores(i+1)", sub = "PuntoFijo", xlab = "Errores(i)", ylab = "Errores(i+1)")
  lines(x, y, type = "l")
  
  x<-c(x,y[l-1])
  iteraciones<-c(1:l)
  plot(g, xlim = c(0,iteraciones[l]), ylim = c(0,x[1]), col = "white", main = "Iteraciones vs Errores", sub = "PuntoFijo", xlab = "Iteraciones", ylab = "Errores")
  lines(iteraciones, x, type = "l")  
  
}

PuntoFijo(g1, 0, 1e-8)
