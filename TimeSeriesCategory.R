# Librerías ---------------------------------------------------------------
library(magrittr)
c("data.table", "dplyr", "forecast", "astsa", "xts", "dygraphs",
  "lattice", "ggplot2", "lme4") %>% 
  sapply(require, character.only=T)

### Creado por Fernando Dorantes Nieto
## OBJETIVO
### Crear análisis de series de tiempo tomando en cuenta variables ------
### --- categóricas y numéricas discretas como predictores

## --NOTA-- No hay estacionalidad

# Cargando y modificando datos --------------------------------------------

data <- read.csv("~/Documentos/FernandoDorantes/serieTiempoPractica/datosPractica.csv",
                 header = T)


data <- data %>%
  data.table %>% 
  .[, dato := abs(dato)] %>% 
  .[, dato := ifelse(predictor2 ==0, dato, dato +5)] %>% 
  .[, dato := ifelse(predictor3 =="normal",  dato, dato +3)] %>% 
  .[, tiempo := as.Date(tiempo)] 
  


# Exploratorios -----------------------------------------------------------
par(mfrow= c(3,1))
xts(data[,2], as.Date(data$tiempo)) %>%  plot
plot(data$predictor2, data$dato )
boxplot(dato~predictor3, data = data)

pruebaNormal <- data %>% 
  group_by(predictor3) %>% 
  do(test = shapiro.test(.$dato))
pruebaNormal$test

# Análisis ----------------------------------------------------------------
##Modelos
ANOVA1 <- aov(dato~predictor3, data= data)
residuales1 <- ANOVA1$residuals %>%  unlist

GLM1 <- glm(dato~predictor2, data= data, family = quasipoisson(link=log))
residuales2 <- GLM1$residuals %>%  unlist


plot(residuales1, residuales2)
modeloResidual <- lm(residuales2~residuales1) 
plot(residuales1, residuales2)
abline(modeloResidual)



?Arima





















