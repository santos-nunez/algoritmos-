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
#               Biseccion
#-------------------------------------

biseccion = function(f, xa, xb, tol)
{
  plot(fe, xlim = c(-2,2), ylim = c(0,6), col = "blue", main = "Grafica de las Funciones", sub = "Biseccion", xlab = "x", ylab = "y")
  par(new=TRUE)
  curve(fpi, type = "l", col="green", axes=FALSE, ylab = "y")
  par(new=FALSE)
  
  plot(f, xlim = c(-0.5,5), ylim = c(-2,5), col = "blue", main = "Grafica de la Función (resta)", sub = "Biseccion", xlab = "x", ylab = "y")
  abline(h=0, v=0, col="red")
  
  if( sign(f(xa)) == sign(f(xb)) )
  { 
    stop("f(xa) y f(xb) tienen el mismo signo") 
  }
  
  a = xa
  b = xb
  k = 0
  dx=0
  
  x<-c()
  y<-c()
  iteraciones<-c()
  
  cat("----------------------------------------------------------\n")
  cat(formatC( c("a","b","m","Error est."), width = -15, format = "f", flag = " "), "\n")
  cat("----------------------------------------------------------\n")
  repeat
  {
    m = a + 0.5*(b-a)
    if( f(m)==0 )
    { 
      cat("Cero de f en [",xa,",",xb,"] es: ", m ) 
    }
    if( sign(f(a)) != sign(f(m)) )
    {
      b = m
    } 
    else 
    { 
      a = m 
    }
    dy = dx
    dx = (b-a)/2
    # imprimir estado
    
    if(k>= 1)
    {
      x<-c(x,dy/b)
      y<-c(y,dx/b)
    }
    
    cat(formatC( c(a,b,m,dx), digits=7, width = -15, format = "f", flag = " "), "\n")
    k = k+1
    
    
    
    #until
    if( dx < tol )
    {
      cat("----------------------------------------------------------\n\n")
      cat("Cero de f en [",xa,",",xb,"] es approx: ", m, "con error <=", dx, "Iteraciones:" ,k)
      break;
    }
  } #repeat
  
  points(m,0)
  
  plot(f, xlim = c(0,x[1]), ylim = c(0,y[1]), col = "white", main = "Errores(i) vs Errores(i+1)", sub = "Biseccion", xlab = "Errores(i)", ylab = "Errores(i+1)")
  lines(x, y, type = "l")
  
  x<-c(x,y[k-1])
  iteraciones<-c(1:k)
  plot(f, xlim = c(0,iteraciones[k]), ylim = c(0,x[1]), col = "white", main = "Iteraciones vs Errores", sub = "Biseccion", xlab = "Iteraciones", ylab = "Errores")
  lines(iteraciones, x, type = "l") 
  
}

biseccion(f1, 0, 1, 1e-8)
biseccion(f1, 1, 2, 1e-8)
