#-------------------------------------------------------------------------------
#                           
#integrantes:         Juan Sebastian Prado Valero
#                     Santos David Nuñez Villamil
#                     Gustavo Antonio Rivera Delgado
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
#                           Ejercicios raiz con metodos
#-------------------------------------------------------------------------------
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






