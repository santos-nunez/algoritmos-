library(shiny)
library(shinydashboard)
library(deSolve)
require(PolynomF)
require(pracma)
library(phaseR)

dif = function(out, param, init, time2)
{
  #S
  beta = param[1]
  infectados = out$I
  A = matrix(rep(0, times = time2^2), nrow = time2, ncol = time2)
  i = 1
  t = 1
  
  A[i,1] = t*beta*infectados[t+1]
  A[i,2] = 1
  
  i = i +1
  t = t + 1
  
  repeat
  {
    A[i,t] = t*beta*infectados[t+1]
    A[i,t-1] = -1
    A[i,t+1] = 1
    i = i +1
    t = t + 1
    
    if(i==time2)
    {
      break;
    }
  }
  
  A[time2,time2] = 1
  b = c(rep(0,time2))
  b[1] = init[1]
  b[time2] = out$S[time2+1]
  
  ysus = solve(A, b)
  # num1 = ysus[64]
  # num2 = 4.207208e-05
  # num1
  # num2
  # abs(num1-num2)/num1
  #plot(c(1:time2),ysus)
  
  #I
  beta = param[1]
  alfa = param[2]
  susceptile = out$S
  A = matrix(rep(0, times = time2^2), nrow = time2, ncol = time2)
  i = 1
  t = 1
  
  A[i,1] = -t*beta*susceptile[t+1]+t*alfa
  A[i,2] = 1
  
  i = i +1
  t = t + 1
  
  repeat
  {
    A[i,t] = -t*beta*susceptile[t+1]+t*alfa
    A[i,t-1] = -1
    A[i,t+1] = 1
    i = i +1
    t = t + 1
    
    if(i==time2)
    {
      break;
    }
  }
  
  A[time2,time2] = 1
  b = c(rep(0,time2))
  b[1] = init[2]
  b[time2] = out$I[time2+1]
  
  yinfec = solve(A, b)
  # num1 = yinfec[66]
  # num2 = 1.067680e-04
  # num1
  # num2
  # abs(num1-num2)/num1
  #plot(c(1:70),yinfec)
  #yinfec
  
  #R
  beta = param[1]
  alfa = param[2]
  infectados = out$I
  A = matrix(rep(0, times = time2^2), nrow = time2, ncol = time2)
  i = 1
  t = 1
  
  A[i,1] = -t*alfa*infectados[t+1]
  A[i,2] = 1
  
  i = i +1
  t = t + 1
  
  repeat
  {
    A[i,t] = -t*alfa*infectados[t+1]
    A[i,t-1] = -1
    A[i,t+1] = 1
    i = i +1
    t = t + 1
    
    if(i==time2)
    {
      break;
    }
  }
  
  A[time2,time2] = 1
  b = c(rep(0,time2))
  b[1] = init[3]
  b[time2] = out$R[time2+1]
  
  yrecup = solve(A, b)
  # num1 = yrecup[60]
  # num2 = 0.99970638
  # num1
  # num2
  # abs(num1-num2)/num1
  #plot(c(1:70),yrecup)
  
  #-------------------------------
  
  out = matrix(rep(0, times = time2*3), nrow = time2, ncol = 3)
  out[,1] = ysus
  out[,2] = yinfec
  out[,3] = yrecup
  return(out)
}

dif2 = function(out, param, init, time2)
{
  #S
  beta = param[1]
  infectados = out$I
  A = matrix(rep(0, times = time2^2), nrow = time2, ncol = time2)
  i = 1
  t = 1
  
  A[i,1] = t*beta*infectados[t+1]
  A[i,2] = 1
  
  i = i +1
  t = t + 1
  
  repeat
  {
    A[i,t] = t*beta*infectados[t+1]
    A[i,t-1] = -1
    A[i,t+1] = 1
    i = i +1
    t = t + 1
    
    if(i==time2)
    {
      break;
    }
  }
  
  A[time2,time2] = 1
  b = c(rep(0,time2))
  b[1] = init[1]
  b[time2] = out$S[time2+1]
  
  ysus = solve(A, b)
  # num1 = ysus[64]
  # num2 = 4.207208e-05
  # num1
  # num2
  # abs(num1-num2)/num1
  #plot(c(1:time2),ysus)
  
  #I
  beta = param[1]
  susceptile = out$S
  A = matrix(rep(0, times = time2^2), nrow = time2, ncol = time2)
  i = 1
  t = 1
  
  A[i,1] = -t*beta*susceptile[t+1]
  A[i,2] = 1
  
  i = i +1
  t = t + 1
  
  repeat
  {
    A[i,t] = -t*beta*susceptile[t+1]
    A[i,t-1] = -1
    A[i,t+1] = 1
    i = i +1
    t = t + 1
    
    if(i==time2)
    {
      break;
    }
  }
  
  A[time2,time2] = 1
  b = c(rep(0,time2))
  b[1] = init[2]
  b[time2] = out$I[time2+1]
  
  yinfec = solve(A, b)
  # num1 = yinfec[66]
  # num2 = 1.067680e-04
  # num1
  # num2
  # abs(num1-num2)/num1
  #plot(c(1:70),yinfec)
  #yinfec
  
  out = matrix(rep(0, times = time2*2), nrow = time2, ncol = 2)
  out[,1] = ysus
  out[,2] = yinfec
  return(out)
}

ui = dashboardPage(
  
  dashboardHeader(title = "Simulación de Infección"),
  dashboardSidebar(
    
    sidebarMenu(
      
      menuItem("Información", tabName = "Info", icon = icon("info-circle")),
      menuItem("SI", tabName = "SI", icon = icon("chart-area")),
      menuItem("SIR", tabName = "SIR ", icon = icon("chart-area"))
    )
    
  ),
  dashboardBody(
    
    tabItems(
      tabItem(tabName = "Info",
              
              fluidRow(
                
                box(
                  
                  textOutput(outputId = "texto")
                  
                )
              ),
              fluidRow(
                box(
                  
                  textOutput(outputId = "texto2")
                  
                )
              ),
              fluidRow(
                box(
                  
                  textOutput(outputId = "texto3")
                  
                )
                
              )
              
      ),
      
      tabItem(tabName = "SIR",
              
              fluidRow(
                
                box( radioButtons(inputId = "Selected", label = "Seleccione el metodo de solución", choices = c("ODE", "EDF"), selected = "ODE"),
                     
                     numericInput(inputId = "time", label = "Tiempo Máximo de la Simulación", value = 70),   
                     
                     numericInput(inputId = "numS", label = "Numero de Suceptibles iniciales", value = 0.9), #1-1e-6
                     
                     numericInput(inputId = "numI", label = "Numero de Infectados iniciales", value = 0.1), #1e-6
                     
                     numericInput(inputId = "numR", label = "Numero de Recuperados iniciales", value = 0),
                     
                     sliderInput(inputId = "Betha", label = "Tasa de transmición de infeccion", value = 1.4247, min = 0.8, max= 1.2, step = 0.0001),
                     
                     sliderInput(inputId = "Gamma", label = "Tasa de recuperacion de la infeccion", value = 0.14286, min = 0.135, max= 1.1, step = 0.00001)
                ),
                
                box(
                  
                  plotOutput("grafica")
                  
                
                ),
                box(
                  
                    plotOutput("grafica2")
                  
                )
                
              )
      ),
      
      tabItem(tabName = "SI",
              
              fluidRow(
                
                box( radioButtons(inputId = "Selected1", label = "Seleccione el metodo de solución", choices = c("ODE", "EDF"), selected = "ODE"),
                     
                     numericInput(inputId = "time1", label = "Tiempo Máximo de la Simulación", value = 70), 
                     
                     numericInput(inputId = "numS1", label = "Numero de Suceptibles iniciales", value = 0.9), #1-1e-6
                     
                     numericInput(inputId = "numI1", label = "Numero de Infectados iniciales", value = 0.1), #1e-6
                     
                     sliderInput(inputId = "Betha1", label = "Tasa de transmición de infeccion", value = 1.4247, min = 0.8, max= 1.2, step = 0.0001),
                     
                ),
                
                box(
                  
                  plotOutput("grafica1")
                  
                ),
                box(
                  
                  plotOutput("grafica3")
                  
                )
              )
      )
    )
  )
)

server = function(input, output)
{
  output$texto = renderText(
    {
      "Red Code"
    }
  )
  output$texto2 = renderText(
    {
      
      "Fue descubierto el 13 de julio de 2001 y su mayor afección fue el 19 de julio del mismo año donde el número de computadoras infectadas fueron cerca de 359000 en menos
    de 14 horas en gran parte en Estados Unidos con un 49%[1]. Explota una conocida vulnerabilidad en el archivo IDQ.DLL, de los servidores Microsoft IIS 5.0, empleando una 
    gran cadena de caracteres que repetía hasta desbordar el buffer y así permitiendo al gusano ejecutar su código capaz de tomar el control  del servidor web a través de 
    las extensiones ISAPI[1]. Solo infecta sistemas que se estén ejecutando en idioma inglés (Windows NT y 2000). Si se cumple esta condición, el gusano permanece latente 
    por unas dos horas antes de dar sus siguientes pasos. Este lapso de tiempo, permite que el gusano se pueda propagar con mayor libertad antes que el usuario o el administrador
      Una característica que cabe destacar es que este gusano no modifica las páginas HTML ni el código de algún archivo  en la maquina infectada, e interpreta funciones del servidor,
        para mostrar un propio HTML basada en el código residente en memoria. Es por esta característica puede ser suprimido fácilmente con el reinicia de la computadora que se infectó [2]."
      
    }
  )
  output$texto3 = renderText(
    {
      
      
      "[1]	D. Moore y  C. Shannon.  La propagación del gusano código rojo (CRv2) online. Disponible: http://www.caida.org/research/security/code-red/coderedv2_analysis.xml 
        [2]	Video Soft BBS.  CodeRed. Un gusano en la memoria de los servidores. online. Disponible: http://www.vsantivirus.com/codered.htm"
    }
  )
  
  
  
  output$grafica = renderPlot(
    
    {
      #estado inicial de los compartimentos
      init <- c(S = input$numS,
                I = input$numI,
                R = input$numR)
      
      N = init[1]+init[2]+init[3]
      
      #parámetros del modelo (coeficientes de las variables)
      param <- c(beta = input$Betha,
                 gamma = input$Gamma)
      
      init = init/N
      
      N = 1
      
      #crear la función con las ODE
      sir <- function(times, init, param)
      {
        with(as.list(c(init, param)),
             {
               #ecuaciones diferenciales
               dS <- -beta * S * I
               dI <-  beta * S * I - gamma * I
               dR <-                 gamma * I
               #resultados de las tasas de cambio
               return(list(c(dS, dI, dR)))
             })
      }
      #intervalo de tiempo y resolución
      times <- seq(0, input$time, by = 1)
      out <- ode(y = init, times = times, func = sir, parms = param, method = "rk4")
      #cambiar out a un data.frame
      out <- as.data.frame(out*N) #aqui puede multiplicar 'out' por N
      #eliminar la variable 'time' en out
      out$time <- NULL
      #mostrar 10 primeros datos
      head(out, 10)
      if(input$Selected == "EDF")
      {
        out = dif(out, param, init, input$time)
        times = times[-1]
        #print(out)
      }
      
      #gráfica
      matplot(x = times, y = out, type = "l",
              xlab = "Tiempo (horas)", ylab = "S, I, R", main = "Modelo SIR básico",
              lwd = 1, lty = 1, bty = "l", col = 2:4)
      #añadir leyenda de líneas
      legend(40, 0.7, c("Susceptibles", "Infectados", "Recuperados"),
             pch = 1, col = 2:4, bty = "n", cex = 1)
    
    
    }
    
  )
  
  output$grafica1 = renderPlot(
    
    {
      
      #estado inicial de los compartimentos
      init <- c(S = input$numS1,
                I = input$numI1)
      
      N = init[1]+init[2]
      
      #parámetros del modelo (coeficientes de las variables)
      param <- c(beta = input$Betha1)
      
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
      times <- seq(0, input$time1, by = 1)
      #resolver el sistema de ecuaciones con función 'ode'
      out <- ode(y = init, times = times, func = si, parms = param, method = "rk4")
      #cambiar out a un data.frame
      out <- as.data.frame(out*N) #aqui puede multiplicar 'out' por N
      #eliminar la variable 'time' en out
      out$time <- NULL
      #mostrar 10 primeros datos
      head(out, 10)
      if(input$Selected1 == "EDF")
      {
        out = dif2(out, param, init, input$time1)
        times = times[-1]
        #print(out)
      }
      xmin = 0 
      xmax = 80
      a = out$I
      b = out$S
      h = 0
      k = 0
      bandera = FALSE
      while(k <= 500 && !bandera)
      {
        h = abs(a[k]-b[k])
        k = k +1 
      }
      if(k < 1000)
      {
        xmin = k - 50
        xmax = k + 80
      }
      if(xmin < 0)
      {
        xmin =0
      }
      
      xmin = 0
      xmax = 80
      #gráfica
      matplot(x = times, y = out, type = "l",
              xlab = "Tiempo (horas)", ylab = "S, I", main = "Modelo SI básico",
              lwd = 1, lty = 1, bty = "l", col = 2:3, xlim = c(xmin, xmax))
      #añadir leyenda de líneas
      legend(40, 0.7, c("Susceptibles", "Infectados"),
             pch = 1, col = 2:3, bty = "n", cex = 1)

      
    }
    
  )
  output$grafica2 = renderPlot(
    
    {
      
      #estado inicial de los compartimentos
      init <- c(S = input$numS,
                I = input$numI,
                R = input$numR)
      
      N = init[1]+init[2]+init[3]
      
      #parámetros del modelo (coeficientes de las variables)
      param <- c(beta = input$Betha,
                 gamma = input$Gamma)
      
      init = init/N
      
      N = 1
      
      #crear la función con las ODE
      sir <- function(times, init, param)
      {
        with(as.list(c(init, param)),
             {
               #ecuaciones diferenciales
               dS <- -beta * S * I
               dI <-  beta * S * I - gamma * I
               dR <-                 gamma * I
               #resultados de las tasas de cambio
               return(list(c(dS, dI, dR)))
             })
      }
      #intervalo de tiempo y resolución
      times <- seq(0, input$time, by = 1)
      out <- ode(y = init, times = times, func = sir, parms = param, method = "rk4")
      #cambiar out a un data.frame
      out <- as.data.frame(out*N) #aqui puede multiplicar 'out' por N
      #eliminar la variable 'time' en out
      out$time <- NULL
      #mostrar 10 primeros datos
      head(out, 10)
      if(input$Selected == "EDF")
      {
        out = dif(out, param, init, input$time)
        times = times[-1]
        #print(out)
      }
      
      #gráfica
      # matplot(x = times, y = out, type = "l",
      #         xlab = "Tiempo", ylab = "S, I, R", main = "Modelo SIR básico",
      #         lwd = 1, lty = 1, bty = "l", col = 2:4)
      # #añadir leyenda de líneas
      # legend(40, 0.7, c("Susceptibles", "Infectados", "Recuperados"),
      #        pch = 1, col = 2:4, bty = "n", cex = 1)
      
      param <- c(beta = input$Betha, gamma = input$Gamma)
      y=init[2]
      apma1 <- function(t, y, param)
      {
        beta = param[1]
        dy <- beta*(init[1])*y - param[2]*y
        list(dy)
      } 
      
      apma1.flowField <- flowField(apma1, xlim = c(0, input$time), 
                                   ylim    = c(0, 1), parameters = param,
                                   points = 9, system = "two.dim", 
                                   add = FALSE, xlab = "time", ylab = "I",col = "Black", 
                                   main = "Mice Population")
      grid()
      
    }
    
  )
  output$grafica3 = renderPlot(
    
    {
      
      #estado inicial de los compartimentos
      init <- c(S = input$numS1,
                I = input$numI1)
      
      N = init[1]+init[2]+init[3]
      
      #parámetros del modelo (coeficientes de las variables)
      param <- c(beta = input$Betha)
      
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
      times <- seq(0, input$time1, by = 1)
      out <- ode(y = init, times = times, func = si, parms = param, method = "rk4")
      #cambiar out a un data.frame
      out <- as.data.frame(out*N) #aqui puede multiplicar 'out' por N
      #eliminar la variable 'time' en out
      out$time <- NULL
      #mostrar 10 primeros datos
      head(out, 10)
      if(input$Selected == "EDF")
      {
        out = dif2(out, param, init, input$time1)
        times = times[-1]
        #print(out)
      }
      
      # #gráfica
      # matplot(x = times, y = out, type = "l",
      #         xlab = "Tiempo", ylab = "S, I, R", main = "Modelo SIR básico",
      #         lwd = 1, lty = 1, bty = "l", col = 2:4)
      # #añadir leyenda de líneas
      # legend(40, 0.7, c("Susceptibles", "Infectados", "Recuperados"),
      #        pch = 1, col = 2:4, bty = "n", cex = 1)
      
      param <- c(beta = input$Betha1)
      y=init[2]
      apma1 <- function(t, y, param){
        beta = param[1]
        dy <- beta*(N-y)*y
        list(dy)
      } 
      
      apma1.flowField <- flowField(apma1, xlim = c(0, input$time1), 
                                   ylim    = c(0, 1), parameters = param[1],
                                   points = 9, system = "two.dim", 
                                   add = FALSE, xlab = "time", ylab = "I",col = "Black", 
                                   main = "Mice Population")
      grid()
      
    }
    
  )
  
}

shinyApp(ui = ui, server = server)