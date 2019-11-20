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
