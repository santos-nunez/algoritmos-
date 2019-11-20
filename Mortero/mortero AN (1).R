  
  require(bezier)
  require(gridBezier)
  require(PolynomF)
  require(rgl)
  require(plot3D)


#-----------------------------------
# Autores:
#       Juan Sebastián Prado
#       Gustavo Antonio Rivera
#       Santos David Nuñez
#-----------------------------------

#-----------------------------------
#         SPLINES CON BEZIER 
#-----------------------------------
  
  t <- seq(0, 4, length=100)
  p <- matrix(c(0,7,1, 0.33,6.92,1, 0.63, 6.62,1,0.7,6.2,1,#1
                1.44,6.05,1,2.88,5.43,1,4.44,4.38,1,#2 
                5.43,2.88,1,6.05,1.44,1,6.2,0.7,1,#3
                6.62,0.63,1, 6.92,0.33,1, 7,0,1), nrow=13, ncol=3, byrow=TRUE)
  
  puntos <- bezier(t=t, p=p, deg =3)
  
  plot(puntos)

#----------------------------------------------------------
#     PUNTOS (X,Y,Z) PRIMER CUADRANTE XY CON Z = 1
#----------------------------------------------------------

  X= puntos[,1]
  Y= puntos[,2]
  Z= puntos[,3]

#------------------------------------
#     REDUCCIÓN HASTA Z = -0.54
#------------------------------------
  
  contx = length(X) + 1
  conty = 1
  ayuda = 1-0.01
  ayuda2 = 0.02
  ayuda3 = 0.02
  
  repeat
  {
    if(X[conty]-ayuda3 >= 0)
    {
      X[contx] = X[conty] -ayuda3
    }
    
    if(Y[conty] -ayuda2 >= 0)
    {
      Y[contx] = Y[conty] -ayuda2
    }
    else
    {
      Y[contx] = 0
      X[contx]=X[contx-1]
    }
    
    Z[contx] = ayuda
    contx = contx + 1
    conty = conty + 1
    
    if(conty == 101) 
    {
      conty = 1
      ayuda = ayuda - 0.01
      ayuda2 = ayuda2 + 0.02
      ayuda3 = ayuda3 + 0.02
    }
    if(contx == 15501)
    {
      cat("z", Z[contx-1], "\n")
      
      break;
    }
  }
  
  Xn=-1*X
  Yn=-1*Y

#-----------------------------------------------------
#         GRÁFICO EN 3D DE LOS 4 CUADRANTES XY
#-----------------------------------------------------

  plot3d(X, Y, Z, type = "p", col = "blue",
         xlab = "x", ylab="y", zlab="z", xlim = c(-7,7), ylim = c(-7,7) , zlim = c(-2,2))
  
  plot3d(Xn, Y, Z, type = "p", col = "blue",
         xlab = "x", ylab="y", zlab="z", xlim = c(-7,7), ylim = c(-7,7) , zlim = c(-2,2))
  
  plot3d(Xn, Yn, Z, type = "p", col = "blue",
         xlab = "x", ylab="y", zlab="z", xlim = c(-7,7), ylim = c(-7,7) , zlim = c(-2,2))
  
  plot3d(X, Yn, Z, type = "p", col = "blue",
         xlab = "x", ylab="y", zlab="z", xlim = c(-7,7), ylim = c(-7,7) , zlim = c(-2,2))

#-------------------------------------------
#       GRÁFICO EN 3D DE PARED INTERNA
#-------------------------------------------

  z = function(x,y)           # FUNCIÓN DEL CIRCULO CON RADIO 9 Y CENTRADO EN (0,0,8.5)
  {
    -sqrt(81-x^2-y^2)+8.5
  }
  
  plot3d(z, xlim = c(-7,7), ylim = c(-7,7), zlim =c(-1,1), col = "blue")

#---------------------------------------------------
#         GRÁFICO EN 3D DEL PISO DEL MORTERO
#---------------------------------------------------

  ultimoZ = Z[contx-1]
  
  ultimosx = X[(contx-101):(contx-1)]
  ultimosy = Y[(contx-101):(contx-1)]
  ultimosz=c()
  
  plot(ultimosx, ultimosy)
  
  contx = 1
  conty = 1
  ayuda = 1-0.01
  ayuda2 = 0.02
  ayuda3 = 0.02
  
  repeat
  {
    ultimosx[contx] = ultimosx[conty] -ayuda3
    ultimosy[contx] = ultimosy[conty] -ayuda2
    ultimosz[contx] = ultimoZ
    contx = contx + 1
    conty = conty + 1
    
    if(conty == 101)
    {
      conty = 1
      ayuda = ayuda - 0.01
      ayuda2 = ayuda2 + 0.02
      ayuda3 = ayuda3 + 0.02
      cat("hola", ayuda2, "\n")
    }
    if(contx == 10001)
    {
      cat("z", Z[contx-1], "\n")
      
      break;
    }
  }
  
  uXn=-1*ultimosx
  uYn=-1*ultimosy
  
  plot3d(ultimosx, ultimosy, ultimosz, type = "p", col = "blue",
         xlab = "x", ylab="y", zlab="z", xlim = c(-7,7), ylim = c(-7,7) , zlim = c(-2,2))
  
  plot3d(uXn, ultimosy, ultimosz, type = "p", col = "blue",
         xlab = "x", ylab="y", zlab="z", xlim = c(-7,7), ylim = c(-7,7) , zlim = c(-2,2))
  
  plot3d(uXn, uYn, ultimosz, type = "p", col = "blue",
         xlab = "x", ylab="y", zlab="z", xlim = c(-7,7), ylim = c(-7,7) , zlim = c(-2,2))
  
  plot3d(ultimosx, uYn, ultimosz, type = "p", col = "blue",
         xlab = "x", ylab="y", zlab="z", xlim = c(-7,7), ylim = c(-7,7) , zlim = c(-2,2))

#---------------------------------------------------------------------------
#         GRÁFICO 3D DEL NUEVO MORTERO CONSTRUIDO A PARTIR DEL ANTERIOR
#---------------------------------------------------------------------------

  cont = 1
  XE = c()
  YE = c()
  ZE = c()
  zm = 1
  repeat
  {
    if((cont+99) > 15500)
    {
      break
    }
    x2 = X[cont:(cont+99)]
    y2 = Y[cont:(cont+99)]
    
    x3 = c(x2[1])
    y3 = c(y2[1])
    na = sample(4:10, 1)
    na
    actual = na
    repeat
    {
      if(actual >= 100)
      {
        break
      }
      x3 = c(x3, x2[actual])
      y3 = c(y3, y2[actual])
      actual = actual+na
    }
    x3 = c(x3, x2[100])
    y3 = c(y3, y2[100])
    
    g = spline(x3,y3)
    
    XE = c(XE, g$x)
    YE = c(YE, g$y)
    
    cz = 1
    while (cz<=length(g$x))
    {
      ZE = c(ZE, zm)
      cz = cz+1
    }
    zm = zm-0.01
    cont = cont+100
    x3 = c()
    y3 = c()
  }
  
  plot3d(XE, YE, ZE, col = "green", type = "p",
         xlab = "x", ylab="y", zlab="z", xlim = c(-7,7), ylim = c(-7,7) , zlim = c(-2,2) )
  plot3d(-XE, YE, ZE, col = "green", type = "p",
         xlab = "x", ylab="y", zlab="z", xlim = c(-7,7), ylim = c(-7,7) , zlim = c(-2,2) )
  plot3d(XE, -YE, ZE, col = "green", type = "p",
         xlab = "x", ylab="y", zlab="z", xlim = c(-7,7), ylim = c(-7,7) , zlim = c(-2,2) )
  plot3d(-XE, -YE, ZE, col = "green", type = "p",
         xlab = "x", ylab="y", zlab="z", xlim = c(-7,7), ylim = c(-7,7) , zlim = c(-2,2) )

#----------------------------------------
#       TABLA DE ERRORES DE PUNTOS
#----------------------------------------

  cat("\tZ\t X Mortero 1\t Y Mortero 1\t X Mortero 2\t Y Mortero 2\t Error absoluto\t Error Relativo\n")
  
  conte = 1
  
  repeat
  {
    
    aux1x = c()
    aux1y = c()
    
    if(conte == 1)
    {
      aux1x = X[1:100]
      aux1y = Y[1:100]
    }
    else if (conte == 0.23)
    {
      aux1x = X[7734:7800]
      aux1y = Y[7734:7800]
    }
    else if(conte == -0.54)
    {
      aux1x = X[15444:15500]
      aux1y = Y[15444:15500]
    }
    
    aux2x = c()
    aux2y = c()
    
    k = 1
    
    repeat
    {
      
      if(round(ZE[k], 3) == round(conte,3))
      {
        aux2x = c(aux2x, XE[k])
        aux2y = c(aux2y, YE[k])
      }
      k=k+1
      if(k>length(XE))
        break
    }
    
    n=1
    m=1
    
    while (n <= length(aux2x)) 
    {
      m = 1
      while(m <= length(aux1x))
      {
        if(aux1x[m]<= aux2x[n]+0.05 && aux1x[m] >= aux2x[n]-0.05)
        {
          errorabs = abs(aux1y[m]-aux2y[n])
          errorrel = abs(aux1y[m]-aux2y[n])/aux1y[m]*100
          cat("\t",conte, "\t", aux1x[m],"\t", aux1y[m], "\t", aux2x[n], "\t", aux2y[n], "\t", errorabs, "\t", errorrel, "\n")
          
          break
        }
        
        m=m+1
      }
      n=n+1
    }
    if(conte == 1)
    {
      conte = 0.23
    }
    else if (conte == 0.23)
    {
      conte = -0.54
    }
    else if(conte == -0.54)
    {
      break
    }
  }
  
