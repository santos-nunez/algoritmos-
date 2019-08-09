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


