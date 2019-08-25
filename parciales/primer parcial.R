#-------------------------------------------------------------------------------------------------------
#                                                   Primer Parcial Analisis numerico
#-------------------------------------------------------------------------------------------------------

# 1. Sea f(n) la eficiencia del algoritmo, medida como el n´umero m´inimo de operaciones requeridas para
#resolver el problema

# a) Implemente en R o Python un algoritmo que le permita sumar ´unicamente los elementos de la
# sub matriz triangular superior o triangular inferior, dada la matriz cuadrada An. Imprima varias
# pruebas, para diferentes valores de n y exprese f(n) en notaci´on O() con una gr´afica que muestre
# su orden de convergencia.

crearMatrix = function(n)
{
  
  datos = sample(1:20, n*n, replace=T) ## Datos de la matrix aleatorios
  
  A = matrix(datos, nrow = n)
  
  return(A)
  
}

sumarElementosMatriz = function(tamMatrices)
{
  eje_x = c()
  eje_y = c()
  
  
  
  for (m in 1:length(tamMatrices))
  {
    
    n = tamMatrices[m]
    
    eje_y[m] = n*n
    eje_x[m] = n
    
    sum = 0
    
    A = crearMatrix(n)
    
    
    for (i in 1:n)
    {
      for (j in 1:n)
      {
        sum = sum + A[i,j]
      } 
    }
    
    cat("\nLa suma para la matriz A: \n" )
    print(A)
    cat("\nes = ", sum)
    
  }
  
  plot(eje_x, eje_y, main = "funcion O(n)", col="blue", xlab = "Tamaño matriz", ylab = "Iteraciones", type = "o", ylim = c(0,n*n) )
  
}

tamMatrices = c(1:80)

sumarElementosMatriz(tamMatrices)


# 2. Sean f(x) = ln(x + 2) y g(x) = sin(x) dos funciones de valor real.

f <-function(x){
  return(log(x + 2))
}
g <-function(x){
  return(sin(x))
}

#intersección
h <-function(x){
  return (f(x)-g(x))  
}

# a)  Utilice la siguiente formula recursiva con E = 1e???8 para el punto de intersección.

# El primer y segundo parámetro son aproximaciones de la solución
# Respectivamente corresponden a los valores iniciales de x(n-1) y x(n-2) 

formula <- function(x0, x1, tol)
{
  
  plot(f, xlim = c(-3,0), ylim = c(-2,2), col = "orange", main = "Grafica de las Funciones", xlab = "x", ylab = "y")
  par(new=TRUE)
  curve(g, type = "l", col="purple", axes=FALSE, ylab = "y", xlim = c(-3,0), ylim = c(-2,2))
  points(-1.631, -0.998, col="black")
  abline(h=0, v=0, col="black")
  par(new=FALSE)
  
  plot(h, xlim = c(-3,0), ylim = c(-2,2), col = "red", main = "Grafica de la Función (resta)", xlab = "x", ylab = "y")
  abline(h=0, v=0, col="black")
  
  err = abs(x1-x0)
  contador <-0
  
  cat("---------------------------------------------------------------------------\n")
  cat(formatC( c("x_k"," f(x_k)","Error est."), width = -20, format = "f", flag = " "), "\n")
  cat("---------------------------------------------------------------------------\n")
  
  x = 0
  
  iteraciones<-c()
  Erx<-c()
  Ery<-c()
  
  while(tol < err)
  {
    #numerador de la expresion iterativa
    numerador = h(x1)*(x1-x0)
    
    #denominador de la expresion iterativa
    denominador = h(x1) - h(x0)
    
    
    #calcular xn
    x = x1 - numerador/denominador
    
    if(contador>0)
    {
      err2<-err
      err = abs(x - x1)
      Erx<-c(Erx, err2/abs(x))
      Ery<-c(Ery, err/abs(x))
    }
    else
    {
      err = abs(x - x1)  
    }
    contador <-contador+1
    
    #se preparan los valores para la siguiente iteracion
    x0 = x1
    x1 = x
    cat(formatC( c(x ,h(x), err), digits=15, width = -15, format = "f", flag = " "), "\n")
  }
  
  points(x,0, col="blue")
  
  plot(h, xlim = c(min(Erx),max(Erx)), ylim = c(min(Ery),max(Ery)), col = "white", main = "Errores(i) vs Errores(i+1)", xlab = "Errores(i)", ylab = "Errores(i+1)")
  lines(Erx, Ery, type = "l")
  
  Erx<-c(Erx,Ery[contador-1])
  iteraciones<-c(1:contador)
  plot(f, xlim = c(0,iteraciones[contador]), ylim = c(min(Erx),max(Erx)), col = "white", main = "Iteraciones vs Errores", sub = "Newton", xlab = "Iteraciones", ylab = "Errores")
  lines(iteraciones, Erx, type = "l")
  
  cat("\n\nLa intersección se encuentra en el punto x=", x, ", E=",err ," iteraciones = ", contador ,"\n")
}

#Advertencia: Los valores iniciales deben ser cercanos a la solucion para que haya convergencia
formula(-1.8,-1.7,1e-8)

