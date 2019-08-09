#-------------------------------------------------------------------------------
#                           
#integrantes:         Juan Sebastian Prado Valero
#                     Santos David Nuñez Villamil
#                     Gustavo Antonio Rivera Delgado
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
#                           Ejercicios raiz con metodos
#-------------------------------------------------------------------------------

g0<- function(x)
{
  x^3+5*x-1
}

g1<- function(x)
{
  (1-x^3)/5
}

g2 <- function(x)
{
  (1-5*x)^(1/3)
}

y<-function(x)
{
  x
}

#-------------------------------------
#               Biseccion
#-------------------------------------

biseccion = function(f, xa, xb, tol)
{
  if( sign(f(xa)) == sign(f(xb)) )
  { 
    stop("f(xa) y f(xb) tienen el mismo signo") 
  }
  # a = min(xa,xb)
  # b = max(xa,xb)
  a = xa
  b = xb
  k = 0
  dx=0
  
  x<-c()
  y<-c()
  
  #Par imprimir estado
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
      x<-c(x,dy)
      y<-c(y,dx)
    }
    
    cat(formatC( c(a,b,m,dx), digits=7, width = -15, format = "f", flag = " "), "\n")
    k = k+1
    
    
    
    #points(k,dx, col = "red")
    
    #until
    if( dx < tol )
    {
      cat("----------------------------------------------------------\n\n")
      cat("Cero de f en [",xa,",",xb,"] es approx: ", m, "con error <=", dx, "Iteraciones:" ,k)
      break;
    }
  } #repeat
  lines (x,y, type = "l")
}

plot(g0, xlim = c(0,0.5), ylim = c(0,0.5), col = "white", main = "Errores vs Errores", sub = "Newton", xlab = "Errores(i)", ylab = "Errores(i+1)")

abline(h = 0, col= "black")

biseccion(g0, 0, 1, 1e-8)


