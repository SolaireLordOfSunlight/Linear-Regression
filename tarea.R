table_values <- read.csv("./nba_stats.csv")

plot(table_values$MPG, table_values$PPG)

l_model <- summary(lm(formula = PPG ~ MPG, data = table_values))

fx <- function(x) l_model$coefficients[1] + l_model$coefficients[2] * x

eps <- table_values$PPG - fx(table_values$MPG)

summary(eps)

### Validacion de Modelo ###
## Normalidad de los Residuos
library(tseries)
library(nortest)
jarque.bera.test(eps)
shapiro.test(eps)
ad.test(eps)
ks.test(eps, "pnorm")
qqnorm(eps)
## Significancia del Modelo
t.test(eps, mu = 0)
## Prueba de Independencia
acf(eps)
library(lmtest)
dwtest(table_values$PPG ~ table_values$MPG)

### Graph ###
library(ggplot2)

ggplot(table_values, aes(MPG, PPG)) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE) +
    ggtitle("PPG = -1.92 + 0.53MPG")

ggplot(as.data.frame(eps), aes(sample = eps)) +
    stat_qq(distribution = stats::qnorm) +
    stat_qq_line(distribution = stats::qnorm)

library(ggpubr)

theme_set(theme_minimal())

png("plot2.png")
ggqqplot(as.data.frame(eps), x = "eps",
    ggtheme = theme_minimal(),
    title = "QQPlot")
dev.off()
