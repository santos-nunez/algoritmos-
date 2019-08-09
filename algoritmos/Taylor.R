#-------------------------------------------------------------------------------
#                           
#integrantes:         Juan Sebastian Prado Valero
#                     Santos David Nuñez Villamil
#                     Gustavo Antonio Rivera Delgado
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
#                           Ejercicios raiz con metodos
#-------------------------------------------------------------------------------


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