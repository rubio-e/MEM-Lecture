###################################
### Cargar las bases de datos #####
###################################
trees <- read.csv("d:/Dropbox/INIFAP/CURSO_MEM/trees.csv")
require(ggplot2)
require(dplyr)
require(nlme)
require(lmfor)

###################################
### Explorar la base de datos #####
###################################
ggplot(trees, aes(x = d, y = cw)) +
  geom_point() +
  theme_classic()
###################################
### mínimos cuadrados ordinarios ##
###################################
lm01 <- lm(sqrt(cw) ~ d, data = trees)

summary(lm01)

plot(lm01, which = 1)

###################################
### mínimos cuadrados ordinarios ##
###################################
require(nlme)
names(trees)
str(trees)
trees$SITIO <- as.factor(trees$SITIO)
trees_cw_mix <- lme(sqrt(cw)~1+d, random = ~1|SITIO, data = trees)
plot(random.effects(trees_cw_mix))
anova(trees_cw_mix, lm01)
plot(trees_cw_mix)
plot(residuals(trees_cw_mix), )

###############################################
#### Bondad de ajuste de los modelos ##########
###############################################
RMSE <- function(model) {
  SCR <- sum(residuals(model)^2)
  n <- length(residuals(model))
  p <- length(coef(model))
  RMSE_res <- sqrt(SCR/(n-p))
  return(RMSE_res)
}

R2 <- function(model) {
  n <- length(residuals(model))
  p <- length(coef(model))
  y <- residuals(model) + fitted(model)         
  SCR <- sum(residuals(model)^2)
  SCT <- sum((y - mean(y))^2)           
  R2 <- 1 - SCR / SCT                   
  R2adj <- 1 - SCR / SCT * (n - 1) / (n - p)  #
  R2adj
}

RMSE(trees_cw_mix)
R2(trees_cw_mix)

###############################################
#### Calibración del un efecto aleatorio ######
###############################################
str(trees)
trees <- trees[order(trees$SITIO, decreasing = F),]
trees_test <-
trees %>% 
  filter(SITIO != 1)
head(trees_test)

trees_cw_mix02 <- lme(sqrt(cw)~1+d, random = ~1|SITIO, data = trees_test)
RMSE(trees_cw_mix02)
R2(trees_cw_mix02)
plot(random.effects(trees_cw_mix02))

######################################################
############### Calibración del nuevo sitio ##########
######################################################
sel <- 1
site_out <-
  trees %>% 
  filter(SITIO == sel);site_out
trees_out <- site_out[1:3,]
# fixed.effects(trees_cw_mix02)
# Promedio obsevado de la muestra
obs_mean <- mean(sqrt(trees_out$cw))
# Promedio de los predichos utilizando los efectos fijos
# fixed.effects(trees_cw_mix02)
pred_mean <- mean(1.33096196 + 0.03479375 * trees_out$d)
# Varianza del efecto aleatorio
var_random <- as.numeric(VarCorr(trees_cw_mix02)[1])
# Varianza del modelo
var_resid <- as.numeric(VarCorr(trees_cw_mix02)[2])
n <- length(trees_out$d)
b_local <- (var_random  / ((1/n) * var_resid+var_random)) * (obs_mean-pred_mean)
b_local
# predecir el diámetro de copa con base en el modelo y sitio
site_out$pred_cw <- (1.33096196 + b_local + 0.03479375  * site_out$d)^2
site_out[,c("cw","pred_cw")]

#####################################################################
###################### Test general #################################
#####################################################################
# ss <- 20
# trees_out <-
#   trees %>% 
#   filter(SITIO == ss);trees_out
# # trees_out[order(trees_out$cw, decreasing = T),]
# # fixed.effects(trees_cw_mix02)
# # fixed.effects(trees_cw_mix)
# obs_mean <- mean(sqrt(trees_out$cw))
# pred_mean <- mean(1.32904730 + 0.03487708    * trees_out$d)
# var_random <- as.numeric(VarCorr(trees_cw_mix)[1])
# var_resid <- as.numeric(VarCorr(trees_cw_mix)[2])
# (var_random  / ((1/length(trees_out$d)) * var_resid+var_random)) * (obs_mean-pred_mean)
# random.effects(trees_cw_mix)[ss,]





