### Multilevel linear regression models

library(lme4)
library(lmerTest)
library(lmtest)

load("~/GitHub/Extended-Independent-Project/Gulf_Of_Alaska.RData")

model.lr <- lmer(log.recruits ~ log.ssb + (1 + log.ssb | species), data = GoA.data)
# warning - model failed to converge - small warning, could indicate random intercept is better

summary(model.lr)

randomeffects <- ranef(model.lr)

u0 <- randomeffects$species$"(Intercept)"
u1 <- randomeffects$species$"log.ssb"

beta0 <- model.lr@beta[1]
beta1 <- model.lr@beta[2]

lines <- data.frame(species = unique(GoA.data$species), slope = beta1 + u1, intercept = beta0 + u0)

ggplot(GoA.data, aes(x = log.ssb, y = log.recruits, col = species, shape = species, group = species)) + 
  geom_point(size = 2.75) +  
  scale_color_okabe_ito(labels = GoA.species$commonname, name = "Species") + 
  scale_shape_manual(values = c(49:54), labels = GoA.species$commonname, name = "Species") + 
  theme_bw() +
  geom_abline(data = lines, aes(slope = slope, intercept = intercept, color = species), linewidth = 0.5) +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("log(SSB)") +
  ylab("log(R)") +
  ggtitle("Multilevel linear regression for stock-recruitment of 6 species in the Gulf of Alaska 1980-2017")

# check responses are approx normally distributed
qqnorm(GoA.data$log.recruits)
qqline(GoA.data$log.recruits)

# check normality of residuals
epsilon <- residuals(model.lr)
qqnorm(epsilon)
qqline(epsilon)

# check for homo/heteroscedasticity
plot(fitted(model.lr), epsilon)
abline(0,0)

# likelihood ratio test - random slope vs random intercept
intercept.model <- lmer(log.recruits ~ log.ssb + (1 | species), data = GoA.data)

anova(model.lr, intercept.model)

# likelihood ratio test - random intercept vs no random effects
no.effects.model <- lm(log.recruits ~ log.ssb, data = GoA.data)

anova(intercept.model, no.effects.model)
