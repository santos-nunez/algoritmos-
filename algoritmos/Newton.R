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
#               Newton
#-------------------------------------

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

newton1 = function(f, fp, x0, tol, maxiter)
{
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
    cat(formatC( c(x1 ,f(x1), dx), digits=15, width = -15, format = "f", flag = " "), "\n")
    x0 = x1
    k = k+1
    # until
    if(dx <= tol || k > maxiter )
    {
      break;
    } 
  }
  cat("---------------------------------------------------------------------------\n")
  if(k > maxiter){
    cat("Se alcanz? el m?ximo n?mero de iteraciones.\n")
    cat("k = ", k, "Estado: x = ", x1, "Error estimado <= ", correccion)
  } else {
    cat("k = ", k, " x = ", x1, " f(x) = ", f(x1), " Error estimado <= ", correccion) }
  
  lines(x, y, type = "l")
  
}

plot(f1, xlim = c(0,1), ylim = c(0,0.5), col = "white", main = "Errores vs Errores", sub = "Newton", xlab = "Errores(i)", ylab = "Errores(i+1)")

abline(h = 0, col= "black")

newton1(f2, f2p, 2, 1e-4, 100)

Fx <- function(x) ((exp(1)^x) - (pi*x))
F1x <- function(x) ((exp(1)^x)-pi)

newton <- function(a,b,err) {
  
  # Grafico
  x = seq(a,b,0.1)
  plot(1*x,type="l",col="orange")
  plot(x,Fx(x),type="l",col="blue")
  abline(h=0,v=0,col="red")
  
  x0 = (a+b)/2;
  it = 0
  
  repeat{
    correccion = Fx(x0)/F1x(x0)
    x1 = x0 - correccion
    dx = abs(correccion)
    x0 = x1
    it = it+1
    # until
    if(dx <= err ) break;
  }
  cat("Iteraciones = ", it, " Resultado = ", x1, "\n")
}

newton(0,1,10e-8)
newton(1,2,10e-8)



