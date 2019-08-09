#-------------------------------------------------------------------------------
#                           
#integrantes:         Juan Sebastian Prado Valero
#                     Santos David Nuñez Villamil
#                     Gustavo Antonio Rivera Delgado
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
#                           Ejercicios raiz con metodos
#-------------------------------------------------------------------------------


f1<-function(x)
{
  signif(exp(1), 5)^x-signif(pi,5)*x
}

f1p<-function(x)
{
  signif(exp(1), 5)^x-signif(pi,5)
}

f2<-function(x)
{
  (x-1.8974)^3
}

f2p<-function(x)
{
  2*(x-1.8974)^2
}

#-------------------------------------
#       Formula de la secante
#-------------------------------------

secante = function(f, x0, x1, tol, maxiter){
  f0 = f(x0)
  f1 = f(x1)
  k = 0
  dx = 0
  
  x<-c()
  y<-c()
  
  while (abs(x1 - x0) > tol && k <= maxiter ) {
    
    pendiente = (f1 - f0)/(x1 - x0)
    if (pendiente == 0) return( cero = NA, f.cero = NA, iter = k, ErrorEst = NA)
    x2 = x1 - f1/pendiente
    f2 = f(x2)
    x0 = x1; f0 = f1
    x1 = x2; f1 = f2
    
    dy=dx
    dx=abs(x1-x0)
    
    if(k>=1)
    {
      x<-c(x,dy)
      y<-c(y, dx)
    }
    k = k+1
    
    # Imprimir iteraciones
    cat(x1, x2, dx, "\n")
  }
  if (k > maxiter) {
    warning("No se alcanz? el n?mero de iteraciones")
  }
  
  
  lines(x,y, type = "l")
  
  
  #return(list(cero=x2, f.cero=f2, iter=k, ErrorEst =abs(x2-x1)))
}


plot(f2, xlim = c(0,0.04), ylim = c(0,0.04), col = "white", main = "Errores vs Errores", sub = "Secante", xlab = "Errores(i)", ylab = "Errores(i+1)")

secante(f2, 0, 2, 1e-8, 100)



