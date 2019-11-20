require(PolynomF)
require(pracma)
datx = c(0, 9.7,11,12.4,14.6,17,20,24); daty = c(1780,7140,32140,207150,292850,328570, 357000,360000)

pchip(datx, daty, seq(1, 24, by = 1))
fp <- pchipfun(datx, daty)
fp(seq(1, 24, by = 0.5))


plot(fp,add = T,  xlim=c(1, 24))
curve(fp,from=datx[1],to=datx[8],add=T, lwd=1,col="blue")


#estado inicial de los compartimentos

valor =  1780
beta = 0.8
Total = 359000
init <- c(S = 359000,
          I = valor)

N = init[1]+init[2]

#parámetros del modelo (coeficientes de las variables)
param <- c(beta = 0.8)

init = init/N

N = 1



graficar = function(init, param, N, beta2)
{
  #crear la funciÃ³n con las ODE
  
  
  N = init[1]+init[2]
  
  #parámetros del modelo (coeficientes de las variables)
  param <- c(beta = beta2)
  
  init = init/N
  
  N = 1
  
  #crear la función con las ODE
  si <- function(times, init, param)
  {
    with(as.list(c(init, param)),
         {
           #ecuaciones diferenciales
           dS <- -beta * S * I
           dI <-  beta * S * I
           #resultados de las tasas de cambio
           return(list(c(dS, dI)))
         })
  }
  #intervalo de tiempo y resolución
  times <- seq(0, 24, by = 1)
  #resolver el sistema de ecuaciones con función 'ode'
  out <- ode(y = init, times = times, func = si, parms = param, method = "rk4")
  #cambiar out a un data.frame
  out <- as.data.frame(out*N) #aqui puede multiplicar 'out' por N
  #eliminar la variable 'time' en out
  out$time <- NULL
  #mostrar 10 primeros datos
  head(out, 10)
  
  #gráfica
  matplot(x = times, y = out$I, type = "l",
          xlab = "Tiempo (horas)", ylab = "S, I", main = "Modelo SI básico",
          lwd = 1, lty = 1, bty = "l", col = 2:3)
  #añadir leyenda de líneas
  # legend(40, 0.7, c("Susceptibles", "Infectados"),
  #        pch = 1, col = 2:3, bty = "n", cex = 1)
  return(out$I)
}


determinarError = function(init, param, beta, Total,fp)
{
  
  dx = graficar(init, param, total, beta)
  j = 1
  fun = c()
  funo = c()
  while(j <= 24)
  {
    fun[j]= fp(j-1)
    funo[j] = dx[j]*Total
    cat(fun[j]," ", funo[j],"\n")
    j = j+1
  }
  i = 1
  sumI = 0
  error = c()
  m = 0
  while( i <= 24)
  {
    m = abs(funo[i]-fun[i])/fun[i]
    sumI = m + sumI
    error[i] = m 
    i = i + 1
  }
  x = (1:24)
  plot(x,error)
  cat("El promedio del error es de ", sumI/24, "\n")
}
determinarError(init, param, 0.8, Total, fp)
determinarError(init, param, 1.2, Total, fp)