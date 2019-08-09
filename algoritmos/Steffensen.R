#-------------------------------------------------------------------------------
#                           
#integrantes:         Juan Sebastian Prado Valero
#                     Santos David Nuñez Villamil
#                     Gustavo Antonio Rivera Delgado
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
#                           Ejercicios raiz con metodos
#-------------------------------------------------------------------------------
#----------------------------------------------
#                 Steffensen
#----------------------------------------------
Steffensen<- function(tol,m,x0,f,fg){
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
  plot(fg, xlim = c(0,1), ylim = c(0,1), col = "white", main = "Errores(i) vs Errores(i+1)", sub = "Steffensen", xlab = "Errores(i)", ylab = "Errores(i+1)")
  lines(Er1, Er2, type = "l")
  return(x)
  
}

g1 <- function(x){
  (exp(x)/pi)
}
fg <- function(x){
  (exp(x)-pi*x)
}

Steffensen(1e-08, 100, 2, g1,fg)
Steffensen(1e-08, 100, 1, g1,fg)