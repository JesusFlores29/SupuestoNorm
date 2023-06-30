#' SDENORM
#'
#' Realiza una prueba de normalidad.
#'
#' @param x (vector) datos de la muestra.
#' @param alpha (vector) nivel de significancia para el test de Shapiro-Wilk
#' @return Un grafico Q-Q para visualizar los datos y un True o False si se cumple la normalidad.
#' @export
Normalidad<-function(x, alpha){
  #caculos preliminares
  n<-length(x)
  if(!is.numeric(x))
    xbar<-mean(x)
  s<-sd(x)
  z<-(x-xbar)/s
  p<-pnorm(z)
  #grafico q-q
  plot(sort(z),p,main="Gráfico Q-Q",xlab="Valores Q",ylab="Probabilidad esperada")
  abline(0,1,col="red")
  #test shapiro-wilk
  shapiro.test(x)$p.value < alpha
}

