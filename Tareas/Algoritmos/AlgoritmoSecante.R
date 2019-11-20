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

#-------------------------------------
#       Formula de la secante
#-------------------------------------

secante = function(f, x0, x1, tol, maxiter)
{
  plot(fe, xlim = c(-2,2), ylim = c(0,6), col = "blue", main = "Grafica de las Funciones", sub = "Secante", xlab = "x", ylab = "y")
  par(new=TRUE)
  curve(fpi, type = "l", col="green", axes=FALSE, ylab = "y")
  par(new=FALSE)
  
  plot(f, xlim = c(-0.5,5), ylim = c(-2,5), col = "blue", main = "Grafica de la Función (resta)", sub = "Secante", xlab = "x", ylab = "y")
  abline(h=0, v=0, col="red")
  
  f0 = f(x0)
  f1 = f(x1)
  k = 0
  dx = 0
  
  x<-c()
  y<-c()
  iteraciones<-c()
  
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
    cat("Iteracion:",k,", valor de x1:",x1, ", valor de x2:", x2, ", error:",dx, "\n")
  }
  if (k > maxiter) {
    warning("No se alcanz? el n?mero de iteraciones")
  }
  
  points(x1,0)
  
  plot(f, xlim = c(0,max(x)), ylim = c(0,max(y)), col = "white", main = "Errores(i) vs Errores(i+1)", sub = "Secante", xlab = "Errores(i)", ylab = "Errores(i+1)")
  lines(x, y, type = "l")
  
  x<-c(x,y[k-1])
  iteraciones<-c(1:k)
  plot(f, xlim = c(0,iteraciones[k]), ylim = c(0,x[1]), col = "white", main = "Iteraciones vs Errores", sub = "Secante", xlab = "Iteraciones", ylab = "Errores")
  lines(iteraciones, x, type = "l")
  
}

secante(f1, 0, 1, 1e-8, 100)

secante(f1, 1, 2, 1e-8, 100)
