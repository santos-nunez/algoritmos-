#--------------------------------------------------------------------
#                     Taylor
#--------------------------------------------------------------------

DD <- function(expr, name, order = 1) {
  if(order < 1) stop("'order' must be >= 1")
  if(order == 1) D(expr, name)
  else DD(D(expr, name), name, order - 1)
}

taylorT = function(f, x0, a, n)
{ 
  # f es tira
  # parse devuelve una expresión
  g = parse(text=f)
  
  # convertir en función
  
  fx = function(x){eval(g[[1]])}
  # almacenar los sumandos
  
  plot(fx, xlim = c(-0.5,5), ylim = c(-2,5), col = "blue", main = "Grafica funcion", sub = "Steffensen", xlab = "x", ylab = "y")
  abline(h = 0, v=0, col= "red")
  
  suma = rep(NA, length=n+1)
  for(k in 1:n)
  {
    g. = DD(g,"x", k)
    fp = function(x) eval(g.)
    suma[k]=1/factorial(k)*(x0-a)^k *fp(a)
  }
  
  suma[n+1] = fx(a)
  sum(suma)
  
}

taylorT("sin(x)", 1.1, 1, 2)
