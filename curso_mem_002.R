# Cargamos la base de datos 
trees <- read.csv("d:/Dropbox/INIFAP/CURSO_MEM/trees.csv")
require(ggplot2)
# plot de dispersión
# h = altura; d = diámetro
ggplot(trees, aes(d, h)) + # en aes poner las variables de diámetro y altura, en este caso son d y h.
  geom_point() +
  theme_classic()
# Pruebla de un modelo lineal tradicional, ajustado con mínimos cuadrados ordinarios
lm01 <- lm(h~d, data = trees)
# Exploramos los resultados
summary(lm01)
# Tiene una r2 decente de más de 70 %
# Sin embargo los residuales muestra una tendencia a la homoscedasticidad ~ desigualdad de varianzas
plot(lm01, which = 1)
# Con base en esto determinamos que se necesita ajustar un modelo no lineal a nuestros datos
#######################
# El modelo no lineal #
#######################
# El primer paso es seleccionar una función de la literatura
# En este caso usaremos la de Gomperz y la escribimos en forma de función
gomperzFunction <- function(b0, b1, b2, d) {
  f <- 1.30 + b0 * exp(-b1 * exp(-b2 * d))
  return(f)
}
gomperzFunction(b0 = 29.6, b1 = 2.01, b2 = 0.06, d = 25 )

# b0,b1 y b2 son los parámetros del modelo
# d es el diámetro
# En este caso vamos utilizar solamente el diámetro como variable independiente

# El segundo paso es ajusta el modelo utilizando los mínimos cuadrados ordinarios no lineales.
# Esto se hace con la función nls
# Se tiene que adivinar el valor inicial de los parámetros, generalmente con base en la literatura
trees_gomperz_onls <- nls(h ~ gomperzFunction(b0, b1, b2, d),
                          data = trees,
                          start = c(b0 = 20, b1 = 2, b2 = 0.01)
)
# ya se ajustó, sin embargo si le damos a la función summary, este no nos dá un valor de r2
summary(trees_gomperz_onls)
# solamente nos dice si los parámetros son significativos. Que en este caso todos los son, porque el valor de p
# es menor a 0.001
# Si quisieramos generar la r2 y la raíz del cuadrado medio del error, que es lo más común en la literatura,
# tenemos que generar las funciones
# eso ya lo tengo, solo tenemos que leer el siguiente archivo
R2(trees_gomperz_onls)
# vemos que la r2 sube un poco, lo cual es bueno
# ahora exploramos los residuales
require(nlme)
plot(trees_gomperz_onls)
# Se ven un poco mejor pero aún se puede mejorar
# ahora como ejercicio te recomiento probar lo mimos pero con otros modelos
# por ejemplo

gomperzFunction <- function(b0, b1, b2, d) {
  f <- 1.30 + b0 * exp(-b1 * exp(-b2 * d))
  return(f)
}

# require(lmfor)
# fithd(trees$d,trees$h,trees$SITIO, modelName = "gomperz")
mix_model <- nlme(h ~ gomperzFunction(b0, b1, b2, d), data = trees,
                  fixed  = b0 + b1 + b2  ~ 1, 
                  weights = varPower(form=~d),
                  random = b0 ~ 1|SITIO, 
                  start = list(fixed = c(b0 = 50, b1 = 2, b2 = 0.07)),
                  method = "ML", control = nlmeControl(tolerance = 1e-2, maxIter = 1000), verbose = T) 	
summary(mix_model)        
R2(mix_model)
RMSE(mix_model)
AIC(mix_model)
anova(mix_model,trees_gomperz_onls)
plot(mix_model)
########################################################
############### Gráfico de artículo ####################
########################################################
require(ggplot2)
require(viridis)
trees$SITIO <- as.factor(trees$SITIO)
ggplot(trees, aes(x = d, y = h))+
  geom_point(col = "gray45", size = 0.8)+
  geom_line(aes(y = predict(mix_model), group = SITIO, colour = SITIO), size = 0.7)+
  theme_classic()+
  # xlim(7, 85)+
  # ylim(2, 45)+
  xlab("d [cm]")+
  ylab("h [m]")+
  annotate("text", x = 90, y = 5, label = 'Local curves', fontface=4, hjust = 1)+
  theme(legend.position="none")+
  scale_color_viridis(discrete = TRUE, option = "inferno")

########################################################
############### Calibración ############################
########################################################
source("d:/Dropbox/INIFAP/CURSO_MEM/calibration_function.R")
sel <- 1
site_out <-
  trees %>% 
  filter(SITIO == sel);site_out
trees_out <- site_out[1:3,]
plot(ranef(mix_model))
b0 <- gompertz_b0(model = mix_model, h = h, d = d, plot = SITIO, trees_out)
b0
site_out$h_pred <- gomperzFunction(b0 = 21.55 + b0, b1 = 2.08, b2 = 0.073, d = site_out$d)
site_out[,c("h","h_pred")]



####################################
### Funciones con sólo dos parámetros
#####################################
# Naslund function
naslundFunction <- function(b0, b1, d) {
  f <- 1.3 + d^2 / (b0 + b1 * d)^2
  return(f)
}

# Curtis
curtisFunction <- function(b0, b1, d) {
  f <- 1.3 + b0 * (d / (1 + d))^b1
  return(f)
}

# Schumacher 
michaFunction <- function(b0, b1, d) {
  f <- 1.3 + b0 * exp(-b1 * d^(-1))
  return(f)
}
#####################################
### Funciones con tres parámetros. Regularmente más difícil de anitar al los valores de inicio.
#####################################

weibullFunction <- function(b0, b1, b2, d) {
  f <- 1.30 + b0 * (1 - exp(-b1 * d^b2))
  return(f)
}

richardsFunction <- function(b0, b1, b2, d) {
  f <- 1.30 + b0 * (1 - exp(-b1 * d))^b2
  return(f)
}



