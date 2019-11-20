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

#----------------------------------------------
#                 Steffensen
#----------------------------------------------
Steffensen<- function(tol,m,x0,f,fg)
{
  
  plot(fe, xlim = c(-2,2), ylim = c(0,6), col = "blue", main = "Grafica de las Funciones", sub = "Steffensen", xlab = "x", ylab = "y")
  par(new=TRUE)
  curve(fpi, type = "l", col="green", axes=FALSE, ylab = "y")
  par(new=FALSE)
  
  iteraciones<-c()
  
  x<-0
  x1<-0
  x2<-1
  
  Er1<-c()
  Er2<-c()
  
  k<-1
  E1<-0
  
  
  
  plot(fg, xlim = c(-0.5,5), ylim = c(-2,5), col = "blue", main = "Grafica funcion", sub = "Steffensen", xlab = "x", ylab = "y")
  abline(h = 0, v=0, col= "red")
  
  
  while(k<=m )
  {
    x1 = f(x0)
    x2 = f(x1)
    x = (x0 - ((x1-x0)^2)/(x2-2*x1+x0))
    
    E2<-E1
    E1<-abs(x-x0)/x
    
    
    if(k > 1)
    {
      Er1<-c(Er1, E2)
      Er2<-c(Er2, E1)
    }
    
    
    cat("\nX1 es:",x1, "con error relativo:", E1, "Iteracion:", k)
    if(abs(x - x0) < tol)
    {
      break
    }
    k<-k+1
    x0 = x
    
  }
  
  points(x,0)
  
  plot(fg, xlim = c(0,max(Er1)), ylim = c(0,max(Er2)), col = "white", main = "Errores(i) vs Errores(i+1)", sub = "Steffensen", xlab = "Errores(i)", ylab = "Errores(i+1)")
  lines(Er1, Er2, type = "l")
  
  Er1<-c(Er1,Er2[k-1])
  cat(Er1)
  iteraciones<-c(1:k-1)
  plot(fg, xlim = c(0,max(iteraciones)), ylim = c(0,max(Er1)), col = "white", main = "Iteraciones vs Errores", sub = "Steffensen", xlab = "Iteraciones", ylab = "Errores")
  lines(iteraciones, Er1, type = "l")
  
}

Steffensen(1e-08, 100, 2, g1,f1)
Steffensen(1e-08, 100, 1, g1,f1)
