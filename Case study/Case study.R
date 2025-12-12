
# Editing - many sections can be found more clearly in other scripts


library(lme4)
library(brms)
library(lmerTest)
library(lmtest)

load("~/GitHub/Extended-Independent-Project/RAM Legacy R Data/DBdata[asmt][v4.66].RData")

##### Finding case study #####

# reduce stock table to variables I care about
cs <- as.data.frame(cbind(stock$stockid, stock$scientificname, stock$commonname, stock$areaid, stock$region))
colnames(cs) <- c('stockid', 'scientificname', 'commonname', 'areaid', 'region')

# reduce areas to only ones that are repeated more than twice
count <- table(cs$areaid)
repeated <- names(count[count > 2])
cs <- cs[cs$areaid %in% repeated,]

# add area names
areas <- as.data.frame(cbind(area$areaid, area$areaname))
colnames(areas) <- c('areaid', 'areaname')
cs$areaname <- areas$areaname[match(cs$areaid, areas$areaid)]

# reorder and remove area id
cs <- cs[,c('region','areaname', 'commonname', 'scientificname', 'stockid')]

# how many different areas are there
n_distinct(cs$areaname) # 132 different areas

##### Overview of data #####

# SSB data

ssb.long <- melt(GoA.ssb, id.vars = "year") # converts data to long form for use in plot
colnames(ssb.long) <- c("year", "species", "ssb")
ggplot(ssb.long, aes(x = year, y = ssb, col = species, group = species)) + 
  geom_line() + 
  scale_y_log10() + 
  scale_color_okabe_ito(labels = GoA$commonname, name = "Species") + 
  scale_x_continuous(breaks = scales::pretty_breaks(n = 7)) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Year") +
  ylab("log(SSB)") +
  ggtitle("Spawning stock of 6 species in the Gulf of Alaska 1980-2017")

# Recruit data

r.long <- melt(GoA.r, id.vars = "year") # converts data to long form
colnames(r.long) <- c("year", "species", "recruits")
ggplot(r.long, aes(x = year, y = recruits, col = species, group = species)) + 
  geom_line() + 
  scale_y_log10() +  
  scale_color_okabe_ito(labels = GoA$commonname, name = "Species") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 7)) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Year") +
  ylab("log(R)") +
  ggtitle("Recruitment of 6 species in the Gulf of Alaska 1980-2017")

# Plotting SSB vs recruits

GoA.data <- data.frame(ssb.long, r.long$recruits)
colnames(GoA.data) <- c("year", "species", "ssb", "recruits")
ggplot(GoA.data, aes(x = ssb, y = recruits, col = species, shape = species, group = species)) + 
  geom_point(size = 2.75) + 
  scale_x_log10() + scale_y_log10() +  
  scale_color_okabe_ito(labels = GoA$commonname, name = "Species") + 
  scale_shape_manual(values = c(49:54), labels = GoA$commonname, name = "Species") + 
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("log(SSB)") +
  ylab("log(R)") +
  ggtitle("Stock-recruitment of 6 species in the Gulf of Alaska 1980-2017")

##### Multilevel linear regression model #####

# add log(SSB) and log(R) to data frame
GoA.long$log.ssb <- log(GoA.long$ssb)
GoA.long$log.recruits <- log(GoA.long$recruits)

model.lr <- lmer(log.recruits ~ log.ssb + (1 + log.ssb | species), data = GoA.long)
# warning - model failed to converge - small warning, could indicate random intercept is better

summary(model.lr)

randomeffects <- ranef(model.lr)

u0 <- randomeffects$species$"(Intercept)"
u1 <- randomeffects$species$"log.ssb"

beta0 <- model.lr@beta[1]
beta1 <- model.lr@beta[2]

lines <- data.frame(species = unique(GoA.long$species), slope = beta1 + u1, intercept = beta0 + u0)

ggplot(GoA.long, aes(x = log.ssb, y = log.recruits, col = species, shape = species, group = species)) + 
  geom_point(size = 2.75) +  
  scale_color_okabe_ito(labels = GoA$commonname, name = "Species") + 
  scale_shape_manual(values = c(49:54), labels = GoA$commonname, name = "Species") + 
  theme_bw() +
  geom_abline(data = lines, aes(slope = slope, intercept = intercept, color = species), linewidth = 0.5) +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("log(SSB)") +
  ylab("log(R)") +
  ggtitle("Multilevel linear regression for stock-recruitment of 6 species in the Gulf of Alaska 1980-2017")

# check responses are approx normally distributed
qqnorm(GoA.long$log.recruits)
qqline(GoA.long$log.recruits)

# check normality of residuals
epsilon <- residuals(model.lr)
qqnorm(epsilon)
qqline(epsilon)

# check for homo/heteroscedasticity
plot(fitted(model.lr), epsilon)
abline(0,0)

# likelihood ratio test - random slope vs random intercept
intercept.model <- lmer(log.recruits ~ log.ssb + (1 | species), data = GoA.long)

anova(model.lr, intercept.model)

# likelihood ratio test - random intercept vs no random effects
no.effects.model <- lm(log.recruits ~ log.ssb, data = GoA.long)

anova(intercept.model, no.effects.model)

##### Bayesian hierarchical model #####

model.b <- brm(log.recruits ~ log.ssb + (1 + log.ssb | species), 
               data = GoA.long,
               family = gaussian(),
               prior = c( 
                 prior(normal(20, 10), class = "Intercept"), # beta0
                 prior(normal(20, 10), class = "b"),         # beta1 
                 prior(exponential(1), class = "sd"),       # u0 and u1 standard deviation 
                 prior(exponential(1), class = "sigma")     # residual standard deviation
                 ), 
               chains = 4, 
               iter = 4000)


