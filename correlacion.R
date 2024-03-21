
nuevo_dir<- "C:\correlacion"
---
title: "Correlación Lineal"
author: "Agustín Romero Reina"
output: html_document
date: "2024-03-16"
---
  
```{r, echo = FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

```{r, echo = TRUE}
library(readxl)
data <- as.data.frame(read_excel("C:/correlacion_regresion_lineal/información correlacion/data.xls"))
View(data)
print(data)
```
#1

La correlación lineal es una medida estadística que describe la relación entre dos variables cuantitativas, indicando la dirección y la fuerza de esa relación. Cuando dos variables tienen una correlación lineal positiva, aumentan o disminuyen juntas. Por otro lado, una correlación lineal negativa indica que cuando una variable aumenta, la otra tiende a disminuir. Una correlación lineal de cero sugiere que no hay relación lineal entre las variables. La correlación lineal se mide mediante el coeficiente de correlación de Pearson, que varía entre -1 y 1, donde -1 indica una correlación lineal negativa perfecta, 1 indica una correlación lineal positiva perfecta, y 0 indica ausencia de correlación lineal.

#2
Decimos que la correlación lineal es una prueba de correlación paramétrica porque se basa en ciertas suposiciones sobre la distribución de los datos y los parámetros poblacionales. En el caso del coeficiente de correlación de Pearson, se asume que las variables están distribuidas normalmente y que la relación entre ellas es lineal. Este enfoque paramétrico utiliza la estructura de los parámetros de la población subyacente para realizar inferencias sobre la correlación entre variables.

#3
```{r, echo = TRUE}
correlacion_datos <- cor(data)
print(correlacion_datos)
```
#4
```{r, echo = TRUE}
panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...) {
  usr <- par("usr")
  on.exit(par(usr))
  par(usr = c(0, 1, 0 ,1))
  Cor <- abs(cor(x, y)) 
  txt <- paste0(prefix, format(c(Cor, 0.123456789), digits = digits)[1])
  if(missing(cex.cor)) {
    cex.cor <- 0.4 / strwidth(txt)
  }
  text(0.5, 0.5, txt,
       cex = 1 + cex.cor*Cor)
}
```

```{r, echo = TRUE}
pairs(data,
      upper.panel = panel.cor, # Este es el panel de correlación
      lower.panel = panel.smooth)
```
#5
```{r, echo = TRUE}
library(correlation)
matriz <- correlation(data)
matriz
```
#6

```{r, echo = TRUE}
library(ggpubr)
library(ggplot2)
ggscatter(data, x = "altura", y = "peso",
          add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "altura piezas (mm)", ylab = "peso piezas (mg)")
```
#7
```{r, echo = TRUE}
library(corrplot)
corrplot(cor(data))
```
#8
```{r, echo = TRUE}
distancia <- c( 1.1,100.2,90.3,5.4,57.5,6.6,34.7,65.8,57.9,86.1)
n_piezas <- c(110,2,6,98,40,94,31,5,8,10)
datos_2 <- data.frame(distancia, n_piezas)
print(datos_2)
```
```{r, echo = TRUE}
correlacion_datos_2 <- cor(datos_2)
print(correlacion_datos_2)
```
```{r, echo = TRUE}
significancia_datos_2 <- cor.test(datos_2$distancia, datos_2$n_piezas)$p.value
print(significancia_datos_2)
```

```{r, echo = TRUE}
intervaloconfianza_datos_2 <- cor.test(datos_2$distancia, datos_2$n_piezas)$conf.int
print(intervaloconfianza_datos_2)
```
La correlación entre las variables es fuerte, con un valor de 0.9249824, muy próximo a 1, lo que sugiere una relación inversamente proporcional. Esto significa que cuando una variable aumenta, la otra tiende a disminuir.

La relación es estadísticamente significativa, ya que el valor p o 'nivel de significancia' es menor que 0.05. Esto indica que la correlación encontrada no es producto del azar y tiene relevancia desde el punto de vista estadístico.


Debido al tamaño muestral reducido, no es apropiado afirmar una correlación significativa entre variables. Se recomienda un tamaño de muestra más grande, idealmente entre 30 y 50 observaciones, o incluso hasta 100, para obtener resultados más confiables en el análisis de correlación.

#9
La diferencia clave entre una relación lineal y una relación monótona radica en la constancia del cambio: en una relación lineal, el cambio es constante a lo largo de la gama de valores, reflejándose en una línea recta en un gráfico, mientras que en una relación monótona, el cambio puede ser constante pero no necesariamente, lo que se refleja en una dirección consistente de aumento o disminución sin una tasa de cambio uniforme.

# Cargar la librería necesaria
library(ggplot2)

# Crear datos para relación lineal
x_lineal <- 1:100
y_lineal <- 2*x_lineal + rnorm(100, mean = 0, sd = 10)

# Crear datos para relación monótona
x_monotona <- 1:100
y_monotona <- x_monotona^2 + rnorm(100, mean = 0, sd = 100)

# Crear un data frame para cada tipo de relación
df_lineal <- data.frame(x = x_lineal, y = y_lineal)
df_monotona <- data.frame(x = x_monotona, y = y_monotona)

# Crear gráfico de dispersión para relación lineal
ggplot(data = df_lineal, aes(x = x, y = y)) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Relación Lineal", x = "Variable X", y = "Variable Y")

# Crear gráfico de dispersión para relación monótona
ggplot(data = df_monotona, aes(x = x, y = y)) +
  geom_point(color = "red") +
  geom_smooth(method = "loess", se = FALSE) +
  labs(title = "Relación Monótona", x = "Variable X", y = "Variable Y")

En el caso de la relación lineal, los puntos parecen formar una línea recta, mientras que en la relación monótona, los puntos tienden a aumentar o disminuir en una dirección específica, pero no necesariamente a una tasa constante.

#10
Se utiliza la prueba de correlación de Spearman. Esta prueba evalúa la relación entre las variables utilizando el coeficiente de correlación de Spearman, que es una medida no paramétrica de la fuerza y la dirección de una relación monótona entre dos variables.

# Generar datos de ejemplo
x <- 1:100
y <- x^2 + rnorm(length(x), mean = 0, sd = 100)

# Calcular la correlación de Spearman
correlation <- cor.test(x, y, method = "spearman")

# Imprimir el resultado
print(correlation)  






