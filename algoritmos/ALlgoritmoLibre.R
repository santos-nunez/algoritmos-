#-------------------------------------------------------------------------------
#                           
#integrantes:         Juan Sebastian Prado Valero
#                     Santos David Nuñez Villamil
#                     Gustavo Antonio Rivera Delgado
#--------------------------------------------------------------------

f<-function(x)
{
  ((exp(1)^x) - (pi*x))
}
f1<-function(x)
{
  ((exp(1)^x) - (pi))
}
# NDHfzero(f,f1,2)


libre <- function (f, f1, x0, num, eps, eps1) 
{
  a = x0
  b = a - f(a)/f1(a)
  i = 0
  
  Er1<-c()
  Er2<-c()
  
  E1 <- 0
  
  plot(f, xlim = c(-0.5,5), ylim = c(-2,5), col = "blue", main = "Grafica funcion", sub = "Libre", xlab = "x", ylab = "y")
  abline(h = 0, v=0, col= "red")
  while ((abs(b - a) > eps)) {
    c = 1
    j = 0
    while (abs(f(b)) >= abs(f(a))) {
      b = a - c * f(a)/f1(a)
      j = j + 1
      c = 1/(2^j)
    }
    a = b
    b = a - f(a)/f1(a)
    c = 1
    j = 0
    while (abs(f(b)) >= abs(f(a))) {
      b = a - c * f(a)/f1(a)
      j = j + 1
      c = 1/(2^j)
    }
    i = i + 1
    E2 = E1
    E1 = abs(b-a)/b
    if(i > 1)
    {
      Er1<-c(Er1, E2)
      Er2<-c(Er2, E1)
    }
  }
  print(b)
  print(f(b))
  if (abs(f(b)) < eps1) 
  {
    print("finding root is successful")
    plot(f, xlim = c(0,0.5), ylim = c(0,0.5), col = "white", main = "Errores(i) vs Errores(i+1)", sub = "Steffensen", xlab = "Errores(i)", ylab = "Errores(i+1)")
    lines(Er1, Er2, type = "l")
    
  }
  else print("finding root is fail")
}

libre(f, f1, 1,100, 1e-05,1e-05)
libre(f, f1, 2,100, 1e-05,1e-05)
