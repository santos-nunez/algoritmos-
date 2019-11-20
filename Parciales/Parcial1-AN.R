#-------------------------------------------------------------------------------------------------------
#                                                   Parcial
#-------------------------------------------------------------------------------------------------------
#               Juan Sebastían Prado Valero
#----------------------------------------------------------------

# 1) Sea f(n) la eficiencia del algoritmo, medida como el n´umero m´ınimo de operaciones requeridas para resolver el problema 

  # b)  Implemente en R o Python un algoritmo que le permita sumar los elementos de una matriz cuadrada
  # An. Imprima varias pruebas, para diferentes valores de n y exprese f(n) en notación O() con una
  # gráfica que muestre su orden de convergencia.

GenerarMatrix = function(n)
{
  datos <- sample(1:20, n*n, replace=T) ## Datos de la matrix aleatorios
  A <- matrix(datos, nrow = n)
  return(A)
}

sumarElementosMatriz = function(tamaño)
{
  x = c()
  y = c()
  for (m in 1:length(tamaño))
  {
    n = tamaño[m]
    y[m] = n*n
    x[m] = n
    suma = 0
    A = GenerarMatrix(n)
    for (i in 1:n)
    {
      for (j in 1:n)
      {
        suma = suma + A[i,j]
      } 
    }
    cat("\nMatriz", m,"\n" )
    print(A)
    cat("\n\t\tLa suma de los elementos de la matriz es: ", suma)
    
  }
  
  plot(x, y, main = "funcion O(n)", xlab = "Tamaño matriz", ylab = "Iteraciones", type = "o", ylim = c(0,n*n) )
  
}

tamaño = c(1:20)

sumarElementosMatriz(tamaño)

cat("\n\tViendo la gráfica, podemos deducir que el orden de convergenica es cuadrática O(n^2)")

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

# a)  Utilice la siguiente formula recursiva con E = 1e−8 para el punto de intersección.

  # El primer y segundo parámetro son aproximaciones de la solución
  # Respectivamente corresponden a los valores iniciales de x(n-1) y x(n-2) 

formula <- function(x0, x1, tol)
{
  
  plot(f, xlim = c(-3,0), ylim = c(-2,2), col = "blue", main = "Grafica de las Funciones", xlab = "x", ylab = "y")
  par(new=TRUE)
  curve(g, type = "l", col="green", axes=FALSE, ylab = "y", xlim = c(-3,0), ylim = c(-2,2))
  points(-1.631, -0.998, col="red")
  abline(h=0, v=0, col="red")
  par(new=FALSE)
  
  plot(h, xlim = c(-3,0), ylim = c(-2,2), col = "blue", main = "Grafica de la Función (resta)", xlab = "x", ylab = "y")
  abline(h=0, v=0, col="red")
  
  err = abs(x1-x0)
  cont <-0
  
  cat("---------------------------------------------------------------------------\n")
  cat(formatC( c("x_k"," f(x_k)","Error est."), width = -20, format = "f", flag = " "), "\n")
  cat("---------------------------------------------------------------------------\n")
  
  x = 0
  
  iteraciones<-c()
  Ex<-c()
  Ey<-c()
  
  while(tol < err)
  {
    #numerador de la expresion iterativa
    numerador = h(x1)*(x1-x0)
    
    #denominador de la expresion iterativa
    denominador = h(x1) - h(x0)
    
    
    #calcular xn
    x = x1 - numerador/denominador
    
    if(cont>0)
    {
      err2<-err
      err = abs(x - x1)
      Ex<-c(Ex, err2/abs(x))
      Ey<-c(Ey, err/abs(x))
    }
    else
    {
      err = abs(x - x1)  
    }
    cont <-cont+1
    
    #se preparan los valores para la siguiente iteracion
    x0 = x1
    x1 = x
    cat(formatC( c(x ,h(x), err), digits=15, width = -15, format = "f", flag = " "), "\n")
  }

  points(x,0, col="green")
  
  plot(h, xlim = c(min(Ex),max(Ex)), ylim = c(min(Ey),max(Ey)), col = "white", main = "Errores(i) vs Errores(i+1)", xlab = "Errores(i)", ylab = "Errores(i+1)")
  lines(Ex, Ey, type = "l")
  
  Ex<-c(Ex,Ey[cont-1])
  iteraciones<-c(1:cont)
  plot(f, xlim = c(0,iteraciones[cont]), ylim = c(min(Ex),max(Ex)), col = "white", main = "Iteraciones vs Errores", sub = "Newton", xlab = "Iteraciones", ylab = "Errores")
  lines(iteraciones, Ex, type = "l")
  
  cat("\n\nLa intersección se encuentra en el punto x=", x, ", E=",err ," iteraciones = ", cont ,"\n")
}

  #Advertencia: Los valores iniciales deben ser cercanos a la solucion para que haya convergencia
formula(-1.8,-1.7,1e-8)
