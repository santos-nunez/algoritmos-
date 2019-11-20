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

#-------------------------------------
#               Newton
#-------------------------------------

newton = function(f, fp, x0, tol, maxiter)
{
  plot(fe, xlim = c(-2,2), ylim = c(0,6), col = "blue", main = "Grafica de las Funciones", sub = "Newton", xlab = "x", ylab = "y")
  par(new=TRUE)
  curve(fpi, type = "l", col="green", axes=FALSE, ylab = "y")
  par(new=FALSE)
  
  plot(f, xlim = c(-0.5,5), ylim = c(-2,5), col = "blue", main = "Grafica de la Función (resta)", sub = "Newton", xlab = "x", ylab = "y")
  abline(h=0, v=0, col="red")
  
  x<-c()
  y<-c()
  
  k = 0
  dx = 0
  # Imprimir estado
  cat("---------------------------------------------------------------------------\n")
  cat(formatC( c("x_k"," f(x_k)","Error est."), width = -20, format = "f", flag = " "), "\n")
  cat("---------------------------------------------------------------------------\n")
  repeat{
    correccion = f(x0)/fp(x0)
    x1 = x0 - correccion
    dy = dx
    dx = abs(x1-x0)
    
    if(k>=1)
    {
      x<-c(x,dy/x0)
      y<-c(y, dx/x0)
      
    }
    
    
    
    # Imprimir iteraciones
    cat(formatC( c(x1 ,f(x1), dx/x1), digits=15, width = -15, format = "f", flag = " "), "\n")
    k = k+1
    # until
    if(dx <= tol || k > maxiter )
    {
      break;
    } 
    x0 = x1
  }
  cat("---------------------------------------------------------------------------\n")
  if(k > maxiter){
    cat("Se alcanz? el m?ximo n?mero de iteraciones.\n")
    cat("k = ", k, "Estado: x = ", x1, "Error estimado <= ", y[k-1])
  } else {
    cat("k = ", k, " x = ", x1, " f(x) = ", f(x1), " Error estimado <= ", y[k-1]) }
  
  points(x1,0)
  
  plot(f, xlim = c(0,x[1]), ylim = c(0,y[1]), col = "white", main = "Errores(i) vs Errores(i+1)", sub = "Newton", xlab = "Errores(i)", ylab = "Errores(i+1)")
  lines(x, y, type = "l")
  
  x<-c(x,y[k-1])
  iteraciones<-c(1:k)
  plot(f, xlim = c(0,iteraciones[k]), ylim = c(0,x[1]), col = "white", main = "Iteraciones vs Errores", sub = "Newton", xlab = "Iteraciones", ylab = "Errores")
  lines(iteraciones, x, type = "l")
}

newton(f1, f1p, 2, 1e-5, 100)

newton(f1, f1p, 0.9, 1e-5, 100)
