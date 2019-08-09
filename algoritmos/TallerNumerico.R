#-------------------------------------------------------------------------------
#                           Taller 1
#integrantes:         Juan Sebastian Prado Valero
#                     Santos David Nuñez Villamil
#                     Gustavo Antonio Rivera Delgado
#-------------------------------------------------------------------------------

#------------------------------
#       Problema 1
#------------------------------

Almacenar <- function(x){
  f<-x
  y=0
  while(f >= 1)
  {
    f<-f/10
    y<-y+1
  }
  
  z<-f*10000
  m<-z%%1
  z<-z-m
  z<-z/10000
  f<-z*10^y
  E<-abs(x-f)
  
  e<-E/f
  
  cat("El n?mero almacenado es", f, "con un error de redondeo absoluto de", E, "y un error de redondeo relativo de", e)
  
}

Almacenar(536.78)

#------------------------------
#       Problema 2
#------------------------------

RaizCuadrada <- function (n,E,x){
  
  y<-(x+(n/x))/2
  c<-0
  while((abs(x-y))>E)
  {
    x<-y
    y<-(x+(n/x))/2
    c<-c+1
  }
  
  cat("La formula converge en la iteraci?n", c, "con el valor de", y, "para la raiz de", n, "y el valor de y*y es", y*y)
  
}

RaizCuadrada(7, 1e-8, 2.3)

#------------------------------
#       Problema 3
#------------------------------

AproximacionEuler<- function(x)
{
  m<-0
  p<-0
  while (m<=5) 
  {
    p<-p+((x^m)/factorial(m))
    m<-m+1
  }
  p<- p+(((x^m)/factorial(m))*exp(x))
  signif(p, 5)
}

AproximacionEuler(0.5)

#------------------------------
#       Problema 4
#------------------------------

CalculoError_AyR<- function(v, Ev, t, Et)
{
  
  d<-v*t
  
  Ed<-v*Ev+t*Et
  
  ed<-Ev/v+Et/t
  
  cat("La distancia recorrida esta entre", d-Ed, "y", d+Ed, "debido a su error Absoluto de", Ed, "y el error relativo es de", ed*100, "%")
  
}

CalculoError_AyR(4, 0.1, 5, 0.1)

#------------------------------
#       Problema 5
#------------------------------

ResolverPolinomio <- function(x0)
{
  x2<-x0*x0
  x4<-x2*x2
  resultado<-2*x4+3*(x0-x2)-4
  cat("El resultado del polinomio evaluado en x =", x0, "es:", resultado)
}

ResolverPolinomio(-2)

#------------------------------
#       Problema 6
#------------------------------



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
#              Punto Fijo
#-------------------------------------

PuntoFijo<-function(g, x0, tol){
  
  maxI<-100
  l<-0
  repeat
  {
    x1<-g(x0)
    m<-abs(x1-x0)
    x0<-x1
    l<-l+1
    cat("Iteracion:", l, "\b, valor actual:", x1, "Error actual:", m, "\n")
    
    points(l, m, col = "Red")
    
    if( m<tol || l>maxI )
    {
      break;
    }
    
  }
  
  if(m>tol)
  {
    cat("No hubo convergencia")
  }
  else
  {
    cat("x* es aproximadamente", x1, "con error menor que", tol)
  }
}

plot(g0, xlim = c(0,7), ylim = c(0,0.200001), col = "white", main = "Iteraciones vs Errores", sub = "Punto Fijo", xlab = "Iteraciones", ylab = "Errores")

abline(h = 0, col= "black")

PuntoFijo(g1, 0, 1e-8)

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

#plot(f1, xlim = c(0,0.5), ylim = c(0,0.5), col = "white", main = "Errores vs Errores", sub = "Newton", xlab = "Errores(i)", ylab = "Errores(i+1)")

#biseccion(f2, 0, 2, 1e-8)

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

#-------------------------------------
#             taylor
#-------------------------------------

DD <- function(expr, name, order = 1) {
  if(order < 1) stop("'order' must be >= 1")
  if(order == 1) D(expr, name)
  else DD(D(expr, name), name, order - 1)
}

taylorT = function(f, x0, a, n)
{ 
  g = parse(text=f)
  
  fx = function(x){eval(g[[1]])}
  
  smds = rep(NA, length=n+1)
  for(k in 1:n){
    g. = DD(g,"x", k)
    fp = function(x) eval(g.)
    smds[k]=1/factorial(k)*(x0-a)^k *fp(a)
  }
  smds[n+1] = fx(a)
  sum(smds)
}

taylorT("exp(x)", 0.50001, 0.5, 5)

#----------------------------------------
#             Posicion Falsa
#----------------------------------------

Fx <- function(x) ((exp(1)^x) - (pi*x))
F1x <- function(x) ((exp(1)^x)-pi)

falsaPosicion <- function(a, b, err) {
  x = seq(a,b,0.1)
  plot(x,Fx(x),type="l",col="blue")
  abline(h=0,col="blue")
  
  #x = b
  #d = (Fx(b)*a-Fx(a)*b)/(Fx(b)-Fx(a))
  it = 0
  error = 1
  while (error > err) {
    it = it + 1
    x = (Fx(b)*a-Fx(a)*b)/(Fx(b)-Fx(a))
    
    if (Fx(x) == 0) {
      break
    }
    
    if (Fx(x)*Fx(a) < 0) {
      b = x
    }
    else {
      a = x
    }
    
    error = abs(Fx(x)/F1x(x))
    
    points(rbind(c(x,0)),pch=19,cex=0.7,col="red")
    
    cat("X=",x,"\t","E=",error,"\t","Iteracion=",it,"\n")
  }
}
falsaPosicion(0, 1, 10e-8)
falsaPosicion(1, 2, 10e-8)

#--------------------------------------------
#               Aitken
#--------------------------------------------

aitken<- function(f, m, x0)
{
  Er1<-c()
  Er2<-c()
  
  k<-0
  E1<-0
  
  g<-parse(text=f)
  fx = function(x){eval(g[[1]])}
  
  plot(fx, xlim = c(-0.5,5), ylim = c(-2,5), col = "blue", main = "Grafica funcion", sub = "Aitken", xlab = "x", ylab = "y")
  abline(h = 0, v=0, col= "red")
  
  d.<-D(parse(text=f ), "x")
  df<-function(x) eval(d.)
  
  while(k<=m)
  {
    x1 = x0 - m*(fx(x0)/df(x0))
    
    E2<-E1
    E1<-abs(x1-x0)/x1
    
    if(k >= 1)
    {
      Er1<-c(Er1, E2)
      Er2<-c(Er2, E1)
    }
    
    cat("\nX1 es:",x1, "con error relativo:", E1, "Iteracion:", k)
    k<-k+1
    x0 = x1
    
  }
  
  plot(fx, xlim = c(0,1), ylim = c(0,1), col = "white", main = "Errores(i) vs Errores(i+1)", sub = "Aitken", xlab = "Errores(i)", ylab = "Errores(i+1)")
  lines(Er1, Er2, type = "l")
  
}

f4<- function(x) 2.7183^x-3.1416*x

aitken("2.7183^x-3.1416*x", 2, 0.8)









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
  
  plot(fx, xlim = c(-0.5,5), ylim = c(-2,5), col = "blue", main = "Grafica funcion", sub = "Taylor", xlab = "x", ylab = "y")
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


#--------------------------------------------------------------------
#                     Ejercicio número 8
#--------------------------------------------------------------------

g1 <- function(x){
  exp(sin(x))
}
fg <- function(x){
  (sin(x)-log(x))
}
Steffensen(1e-08,100,2,g1,fg)

#--------------------------------------------------------------------
#                     Ejercicio número 13
#--------------------------------------------------------------------

newtonraphson = function(fun, der, x0, tol = 0.000000005, maxiter = 100)
{ 
  eje_x = c()
  eje_y = c()
  errores = c()
  cont = 0
  # f = string
  numiter = 0 
  g = parse(text=fun) # parse devuelve tipo "expression"
  g. = parse(text=der) # parse devuelve tipo "expression"
  fx = function(x){eval(g)} # convertir f a función
  fp = function(x){eval(g.)} # convertir f' a función
  
  correccion = -fx(x0)/fp(x0)
  cat( "\n", formatC ( c( "Iteracion", "Cero", "f(cero)", "error"), width = 10, format = "d", flag = " "  ), "\n")
  
  plot(fx, xlim = c(-0.5,5), ylim = c(-2,5), col = "blue", main = "Solución Newton", sub = "Punto 13", xlab = "x", ylab = "y")
  abline(h = 0, v=0, col= "red")
  
  while (abs(correccion) >= tol && numiter <= maxiter)
  {
    cont = cont + 1
    numiter = numiter + 1 
    if (fp(x0) == 0) stop("División por cero")
    x1 = x0 + correccion 
    errores[cont] = abs((x1-x0))/x1
    correccion = -fx(x1)/fp(x1)
    cat( formatC( c(numiter, x0, fx(x0), errores[cont] ), digits = 15, width = -15, format = "f", flag = "  "  ), "\n" )
    x0 = x1 
  }
  
  if (numiter > maxiter)
  {
    warning("Se alcanzó el máximo número de iteraciones.")
    cat("Estado:\n")
    cat("k = ", k, "x = ", x1, " f(x) = ", f(x1), "Error estimado <= ", correccion)
  }
  else 
  {
    iter = c(1:cont)
    cont_n = 0;
    cont_e = 0;
    
    repeat
    {
      eje_x[cont_n] = errores[cont_e]
      eje_y[cont_n] = errores[cont_e+1]
      cont_n = cont_n + 1
      cont_e = cont_e + 1;
      
      if (cont_n == cont)
      {
        break;
      }
    }
    
    plot(eje_x, eje_y, col = "red")
    lines(eje_x,eje_y, type="l",  col = "green")
    plot(iter, errores, col ="red")
    lines(iter,errores, type="l" , col = "green")
    
    
    return(list(cero = x0, f.cero = fx(x0), numeroiter=numiter, error.est = correccion)) 
    
  }
}


newtonraphson("x^3-4","3*x^2", 2, 0.00000005, 10)

