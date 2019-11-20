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

g1 <- function(x)
{
  (exp(x)/pi)
}

g2 <- function(x)
{
  log(exp(x)/pi)
}

f1p<-function(x)
{
  signif(exp(1), 5)^x-signif(pi,5)
}

#--------------------------------------------
#               Aitken
#--------------------------------------------

aitken = function(f, m, x0, tol)
{
  
  plot(fe, xlim = c(-2,2), ylim = c(0,6), col = "blue", main = "Grafica de las Funciones", sub = "Aitken", xlab = "x", ylab = "y")
  par(new=TRUE)
  curve(fpi, type = "l", col="green", axes=FALSE, ylab = "y")
  par(new=FALSE)
  
  iteraciones<-c()
  
  Er1<-c()
  Er2<-c()
  
  k<-0
  E1<-0
  
  g<-parse(text=f)
  fx = function(x){eval(g[[1]])}
  d.<-D(parse(text=f ), "x")
  df<-function(x) eval(d.)
  
  plot(fx, xlim = c(-0.5,5), ylim = c(-2,5), col = "blue", main = "Grafica funcion", sub = "Aitken", xlab = "x", ylab = "y")
  abline(h = 0, v=0, col= "red")
  
  repeat
  {
    
    x1 = x0 - m*(fx(x0)/df(x0))
    dx = abs(x1-x0)
    E2 = E1
    E1 = dx/x1
    cat("X=", x1, "\t", "E=", dx, "\t e=", E1,"\t Iteracion", k+1,"\n")
    
    if(k >= 1)
    {
      Er1<-c(Er1, E2)
      Er2<-c(Er2, E1)
    }
    
    k = k + 1
    
    if (dx < tol) break;
    
    x0 = x1
    
    
  }
  
  points(x1,0)
  
  plot(fx, xlim = c(0,max(Er1)), ylim = c(0,max(Er2)), col = "white", main = "Errores(i) vs Errores(i+1)", sub = "Aitken", xlab = "Errores(i)", ylab = "Errores(i+1)")
  lines(Er1, Er2, type = "l")
  
  Er1<-c(Er1,Er2[k])
  iteraciones<-c(1:k)
  plot(fx, xlim = c(0,iteraciones[k]), ylim = c(0,Er1[1]), col = "white", main = "Iteraciones vs Errores", sub = "Aitken", xlab = "Iteraciones", ylab = "Errores")
  lines(iteraciones, Er1, type = "l")
}

aitken("2.7182^x-3.1415*x", 1, 2, 10^-8)

aitken("2.7182^x-3.1415*x", 1, 0, 10^-8)

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

#-------------------------------------
#              Libre
#-------------------------------------

#------------------
#   Original
#------------------

library(NLRoot)
NDHfzero(f1,f1p,2)

#------------------
#       Codigo
#------------------

libre <- function (f, f1, x0 = 0, num = 1000, eps = 1e-05, eps1 = 1e-05) 
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

libre(f1, f1p, 1)
libre(f1, f1p, 2)

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

#----------------------------------------
#             Posicion Falsa
#----------------------------------------

falsaPosicion <- function(Fx, F1x, a, b, err) 
{
  
  plot(fe, xlim = c(-2,2), ylim = c(0,6), col = "blue", main = "Grafica de las Funciones", sub = "Posicion Falsa", xlab = "x", ylab = "y")
  par(new=TRUE)
  curve(fpi, type = "l", col="green", axes=FALSE, ylab = "y")
  par(new=FALSE)
  
  plot(Fx, xlim = c(-0.5,5), ylim = c(-2,5), col = "blue", main = "Grafica de la Función (resta)", sub = "Posicion Falsa", xlab = "x", ylab = "y")
  abline(h=0, v=0, col="red")
  
  m<-c()
  y<-c()
  iteraciones<-c()
  
  it = 0
  error = 1
  while (error > err) {
    
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
    
    e2<-error
    error = abs(Fx(x)/F1x(x))
    
    if(it>=1)
    {
      m<-c(m,e2)
      y<-c(y,error)
    }
    
    cat("X=",x,"\t","E=",error,"\t","Iteracion=",it,"\n")
    
    it = it + 1
  }
  
  points(x,0)
  
  plot(Fx, xlim = c(0,max(m)), ylim = c(0,max(y)), col = "white", main = "Errores(i) vs Errores(i+1)", sub = "Posicion Falsa", xlab = "Errores(i)", ylab = "Errores(i+1)")
  lines(m, y, type = "l")
  
  m<-c(m,y[it-1])
  iteraciones<-c(1:it)
  plot(Fx, xlim = c(0,iteraciones[it]), ylim = c(0,m[1]), col = "white", main = "Iteraciones vs Errores", sub = "Posicion Falsa", xlab = "Iteraciones", ylab = "Errores")
  lines(iteraciones, m, type = "l")
  
}

falsaPosicion(f1,f1p,0, 1, 10e-8)
falsaPosicion(f1,f1p,1, 2, 10e-8)

#-------------------------------------
#              Punto Fijo
#-------------------------------------

PuntoFijo<-function(g, x0, tol)
{
  
  plot(fe, xlim = c(-2,2), ylim = c(0,6), col = "blue", main = "Grafica de las Funciones", sub = "PuntoFijo", xlab = "x", ylab = "y")
  par(new=TRUE)
  curve(fpi, type = "l", col="green", axes=FALSE, ylab = "y")
  par(new=FALSE)
  
  plot(f1, xlim = c(-0.5,5), ylim = c(-2,5), col = "blue", main = "Grafica de la Función (resta)", sub = "PuntoFijo", xlab = "x", ylab = "y")
  abline(h=0, v=0, col="red")
  
  x<-c()
  y<-c()
  iteraciones<-c()
  
  m<-0
  
  maxI<-100
  l<-0
  repeat
  {
    x1<-g(x0)
    m2<-m
    m<-abs(x1-x0)
    
    if(l>=1)
    {
      x<-c(x, m2/x1)
      y<-c(y, m/x1)
    }
    
    cat("Iteracion:", l, "\b, valor actual:", x1, "Error actual:", m, "\n")
    
    l<-l+1
    
    if( m<tol || l>maxI )
    {
      break;
    }
    
    x0<-x1
    
  }
  
  if(m>tol)
  {
    cat("No hubo convergencia")
  }
  else
  {
    cat("x* es aproximadamente", x1, "con error menor que", tol)
  }
  
  points(x1,0)
  
  plot(g, xlim = c(0,x[1]), ylim = c(0,y[1]), col = "white", main = "Errores(i) vs Errores(i+1)", sub = "PuntoFijo", xlab = "Errores(i)", ylab = "Errores(i+1)")
  lines(x, y, type = "l")
  
  x<-c(x,y[l-1])
  iteraciones<-c(1:l)
  plot(g, xlim = c(0,iteraciones[l]), ylim = c(0,x[1]), col = "white", main = "Iteraciones vs Errores", sub = "PuntoFijo", xlab = "Iteraciones", ylab = "Errores")
  lines(iteraciones, x, type = "l")  
  
}

PuntoFijo(g1, 0, 1e-8)

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

#----------------------------------------------
#                 Steffensen
#----------------------------------------------
Steffensen<- function(tol,m,x0,f,fg)
{
  
  plot(fe, xlim = c(-2,2), ylim = c(0,6), col = "blue", main = "Grafica de las Funciones", sub = "Steffensen", xlab = "x", ylab = "y")
  par(new=TRUE)
  curve(fpi, type = "l", col="green", axes=FALSE, ylab = "y")
  par(new=FALSE)
  
  iteraciones<-c()
  
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
  
  plot(fg, xlim = c(0,max(Er1)), ylim = c(0,max(Er2)), col = "white", main = "Errores(i) vs Errores(i+1)", sub = "Steffensen", xlab = "Errores(i)", ylab = "Errores(i+1)")
  lines(Er1, Er2, type = "l")
  
  Er1<-c(Er1,Er2[k-1])
  cat(Er1)
  iteraciones<-c(1:k-1)
  plot(fg, xlim = c(0,max(iteraciones)), ylim = c(0,max(Er1)), col = "white", main = "Iteraciones vs Errores", sub = "Steffensen", xlab = "Iteraciones", ylab = "Errores")
  lines(iteraciones, Er1, type = "l")
  
}

Steffensen(1e-08, 100, 2, g1,f1)
Steffensen(1e-08, 100, 1, g1,f1)

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

