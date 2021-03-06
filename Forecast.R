##Adquirindo os dados
library(tidyverse)
carga <- read_excel("Forecast/Forecast/Carga por mes.xlsx")

##Tratando dados
carga = carga[,c(4,7)]
colnames(carga)[1]="Data"
colnames(carga)[2]="Carga"

##Lidando com datas
library(lubridate)
carga$Data = ymd(carga$Data)

##Transformando em formato Time Series
library(xts)
cargats = xts(x = carga$Carga, order.by = carga$Data)

##Plotando gr�fico interativo para an�lise macro

library(dygraphs)
tsgraph = dygraph(cargats,
                  main="Carga Mensal do SIN",
                  ylab="(MWmed)") %>%
  dySeries("V1", label="Carga") %>%
  dyOptions(labelsUTC = TRUE, fillGraph=TRUE, fillAlpha=0.1, drawGrid = TRUE, colors="#636363") %>%
  dyRangeSelector() %>%
  dyCrosshair(direction = "vertical") %>%
  dyHighlight(highlightCircleSize = 5, highlightSeriesBackgroundAlpha = 0.2, hideOnMouseOut = FALSE)  %>%
  dyRoller(rollPeriod = 1)

##Transformando o arquivo xts em ts

cargats = as.ts(cargats)
cargats = ts(cargats,frequency=12)

##An�lise mais profunda da sazonalidade

monthplot(cargats, ylab="MWmed")

library(ggfortify)
tsdecomposed = cargats %>%
  decompose() %>%
  autoplot()+
  ggtitle("Decomposi��o da S�rie")

##Usando a fun��o de autocorrela��o para an�lise de estacionaridade
cargats %>%
  acf() %>%
  autoplot()+
  ggtitle("Fun��o de Autocorrela��o")+
  ylab("FAC")

##Teste da ra�z unica

library(urca)

dickeyfuller = ur.df(y = cargats)
summary(dickeyfuller)
#Como o valor do test-statistic 0.5946 � maior que o valor cr�tico para o teste estatistico com 95% de confian�a (-1,95), eu FALHO em REJEITAR a hip�tese nula de que a s�rie � n�o estacion�ria, ous seja, a s�rie � n�o estacion�ria.


##Escolhendo o melhor modelo de ARIMA

library(forecast)

#Fazendo um modelo ARIMA (1,1,1)(1,1,1)
cargatsarima= Arima(cargats, order = c(0,1,1), seasonal = c(0,1,1),
                   method = "ML", lambda = 0)
summary(cargatsarima)

#Testando se s�o estatisticamente determinantes as variaveis MA - Moving Average
t.test <- function(modelo_arima){
      coef <- modelo_arima$coef
      se <- sqrt(diag(modelo_arima$var.coef))
      t <- abs(coef/se)
      ok <- t > qt(0.95, length(modelo_arima$x) -
      sum(modelo_arima$arma[c(1,2,3,4,6,7)]))
      resul <- data.frame(Coef = coef, sd = se, t = t, rej_H0 = ok)
      return(resul)
}

t.test(cargatsarima)



##Fazendo o dign�stico dos res�duos

#Autocorrela��o linear nos res�duos?
Box.test(x = cargatsarima$residuals, lag = log(nrow(carga)),
           type = "Ljung-Box", fitdf = 2)

#Multiplicador de Lagrange para heterocedasticidade condicional autorregressiva (ARCH LM)
library(FinTS)
ArchTest(cargatsarima$residuals, lag=log(nrow(carga)))

#Teste de normalidade dos res�duos
library(normtest)
jb.norm.test(cargatsarima$residuals, nrepl =2000)



###Previs�o
prev = forecast(object = cargatsarima, h=12, level = 0.95)
accuracy(prev)
plot(prev)

tsprev = prev %>%
  autoplot() +
  geom_forecast()+
  ggtitle("Previs�o de Carga")+
  ylab("MWmed")+
  xlab("Anos")

library(DT)
tabprev = as_tibble(prev)
rownames(tabprev)=c("Jan20","Fev20", "Mar20", "Abr20", "Mai20", "Jun20","Jul20","Ago20","Set20","Out20","Nov20","Dez20")

round(tabprev,2) %>%
  datatable(options = list(
    searching = FALSE,
    pageLength = 12),
    colnames=c("Previs�o [MWmed]","Menor com 95% de confian�a","Maior com 95% de confian�a"))

