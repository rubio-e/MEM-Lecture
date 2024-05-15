require(dplyr)
rats <- read.table("https://bit.ly/2PXvtWs", header = TRUE)
head(rats)
str(rats)
dput(names(rats))
fact<-c("subject", "group")
group_labels<-c("control","low", "high")
rats$group<-factor(rats$group, levels = c(3,2,1), labels = group_labels)
rats[,fact]<-lapply(rats[fact], as.factor)
rats$time<-log(1+(rats$time-45)/10)
rats<-arrange(rats, subject, time)

str(rats)
require(ggplot2)
summary(rats)
ggplot(rats,aes(group,response))+
  geom_boxplot()+
  # facet_wrap(~group)+
  theme_bw()

# Descriptive plot
ggplot(rats,aes(time,response, group=subject, color=group))+geom_line()

fixed_no_dummy<-lm(response~1+time+group:time, data= rats)
summary(fixed_no_dummy)
plot(resid(fixed_no_dummy))

# fixed with dummy
head(rats)
fixed_dummy<-lm(response~1+subject+group:time, data=rats)
summary(fixed_dummy)
plot(resid(fixed_dummy))

# random effects, intercept
require(lme4)
mixed_intercept<-lmer(response~1+(1| subject)+time+group:time,data=rats)
rats$response
predict(mixed_intercept)
summary(mixed_intercept)
?pvalues
# is my random effect relevant?
# is the difference betwwen deviance significant?
anova(mixed_intercept,fixed_no_dummy)
# Better to do this 
RLRsim::exactRLRT(mixed_intercept)
# lmerTest:::summary.lmerModLmerTest(mixed_intercept)

boot<-bootMer(mixed_intercept, function(x) getME(x,"fixef"), nsim = 1000)
hist(boot$t[,"time"])
hist(boot$t[,"time:grouplow"])
hist(boot$t[,"time:grouphigh"])

mean(boot$t[,"time"]<=0)
mean(boot$t[,"time:grouplow"]<=0)

# exercise 2
# mixed effects, correlated slope
# the variance is really small, warning, not a big difference between the slope
# singular fit is for the perfect correlation

mixed_cslope<-lmer(response~1+time+group:time+(1+time|subject),
                  data=rats)
str(rats)
# mixed_cslope@resp$mu

summary(mixed_cslope)

# there is no evidence that the mixed slope improved the model
anova(mixed_cslope,mixed_intercept)
RLRsim::exactRLRT(mixed_cslope)
# lmerTest:::summary.lmerModLmerTest(mixed_cslope)

# mixed effects models, uncorrelated slope


mixed_uslope<-lmer(response~1+time+group:time+(1+time||subject),
                   data=rats)

summary(mixed_uslope)

# there is no evidence that the mixed slope improved the model
anova(mixed_uslope,mixed_intercept)
RLRsim::exactRLRT(mixed_uslope)






