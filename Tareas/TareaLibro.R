#------------------------------------------------------------------------------------------
#                                       Tarea Libro Phyton
#------------------------------------------------------------------------------------------

#----------------------------------------------
#               Punto Libre = 8
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

#-----------------------------------------------
#           Tarea Integral = Punto 15
#-----------------------------------------------

riemman = function(f, a, b, n)
{
  i = 1
  delta = (b-a)/n
  x = a + i*delta
  area = 0
  
  repeat
  {
    area = area + f(x)*delta
    i = i + 1
    if (i == n) break;
    x = a + i*delta
  }
  
  #cat ("area" , area, "\n")
  return (area)
}


ejercicio = function(fun, i, f, tol, avance, n)
{
  sumatoria = 0
  x0 = riemman(fun, i, f, n)
  sumatoria =  sumatoria + x0
  cat ("area" , sumatoria, "\n")
  i = f
  f =  f + avance
  
  repeat
  {
    
    x1 = riemman(fun, i, f, n)
    sumatoria = sumatoria + x1
    cat ("area" , sumatoria, "\n")
    error = abs(x1-x0)
    i = f
    f = f + avance
    x0 = x1
    
    if (sumatoria >  2 | error < tol)
    {
      return (sumatoria)
    }
    
  }
}

f = function(x) 5-(exp(1)^x)
ejercicio(f, 0, 0.1, 10^-8, 0.1, 10000)

#----------------------------------
#           Ejercicio 27
#----------------------------------

polar <- function (theta, r, color=4){
  y <- 0
  x <- 0
  ejex <- 1
  
  for (i in 1:length(r)){
    if(is.nan(r[i])== T){
      r[i] <- 0
    }
  }
  
  dim <- seq(0, 2*pi, by=pi/300) 
  angulo <- seq(-max(dim),max(dim),by=dim[2]-dim[1])
  y <- r*sin(theta)
  x <- r*cos(theta)
  plot.new()
  plot.window(xlim = c(-max(r), max(r)), ylim = c(-max(r), max(r)), asp = 1)
  
  aux <- max(r)
  # Dibuja los ejes.
  while (aux > 0){
    fi <- aux*sin(angulo)
    cir <- aux*cos(angulo)
    points(cir,fi,pch="-",col="gray",cex=0.3)
    text(ejex+0.2,-0.2,ejex,col="gray")
    ejex <- ejex + 1
    aux <- aux - 1
  }
  
  abline(v=((max(cir)+min(cir))/2),col="gray")
  abline(h=((max(cir)+min(cir))/2),col="gray")
  segments(-max(r)+0.5,-max(r)+0.5,max(r)-0.5,max(r)-0.5,col="gray")
  segments(-max(r)+0.5,max(r)-0.5,max(r)-0.5,-max(r)+0.5,col="gray")
  
  points(x,y,pch=20,col=color,cex=1)
  
}

f<-function(x) 3*sin(x)^3-1-4*sin(x)*cos(x)
fp<-function(x) 9*cos(x)*sin(x)^2+4*sin(x)^2-4*cos(x)^2

newtonP = function(f, fp, x0, tol, maxiter)
{
  dim <- seq(0, 2*pi, by=pi/300) 
  r=3*sin(dim)^3-1
  r2=4*sin(dim)*cos(dim)
  polar(dim,r,"blue")
  par(new=TRUE)
  polar(dim,r2,"red")
  title(main="Gráficas de las Funciones Polares (Originales)")
  
  points(0.5,1.29, col = "green", pch = 20)
  
  dim <- seq(0, pi/2, by=pi/300) 
  r=3*sin(dim)^3-1
  r2=4*sin(dim)*cos(dim)
  polar(dim,r,"blue")
  par(new=TRUE)
  polar(dim,r2,"red")
  title(main="Gráficas de las Funciones Polares en intervalo theta = [0, pi/2]")
  
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
  
  points(0.5,1.29, col = "green", pch = 20)
  
  plot(f, xlim = c(0,x[1]), ylim = c(0,y[1]), col = "white", main = "Errores(i) vs Errores(i+1)", sub = "Newton", xlab = "Errores(i)", ylab = "Errores(i+1)")
  lines(x, y, type = "l")
  
  x<-c(x,y[k-1])
  iteraciones<-c(1:k)
  plot(f, xlim = c(0,iteraciones[k]), ylim = c(0,x[1]), col = "white", main = "Iteraciones vs Errores", sub = "Newton", xlab = "Iteraciones", ylab = "Errores")
  lines(iteraciones, x, type = "l")
}

newtonP(f, fp, pi/2, 1e-5, 100)

