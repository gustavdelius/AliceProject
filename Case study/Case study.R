library(dplyr)
library(reshape2)  
library(ggplot2)
library(ggokabeito)
library(lme4)

load("~/GitHub/extended-independent-project/RAM Legacy R Data/DBdata[asmt][v4.66].RData")

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

##### Gulf of Alaska #####

GoA <- cs[cs$areaname == 'Gulf of Alaska',] # contains 19 species
species <- GoA$stockid
GoA.ssb <- select(ssb.data, any_of(species)) # find ssb data

ssb.species <- colnames(GoA.ssb)        # Stock IDs of species that have SSB data available
GoA <- GoA[!(GoA$stockid %in% remove),] # remove from original data frame

GoA.r <- select(r.data, any_of(species)) # find recruitment data

GoA.ssb <- na.omit(GoA.ssb) # remove rows with missing values
GoA.r <- na.omit(GoA.r)

colnames(GoA.ssb) <- GoA$commonname
GoA.ssb <- data.frame(year = as.numeric(row.names(GoA.ssb)), GoA.ssb) # add year as the first column
row.names(GoA.ssb) <- NULL # remove row names

colnames(GoA.r) <- GoA$commonname
GoA.r <- data.frame(year = as.numeric(row.names(GoA.r)), GoA.r)
row.names(GoA.r) <- NULL

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
  ggtitle("Spawning stock of 6 species in the Gulf of Alaska 1978-2017")

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
  ggtitle("Recruitment of 6 species in the Gulf of Alaska 1978-2017")

# Plotting SSB vs recruits

GoA.long <- data.frame(ssb.long, r.long$recruits)
colnames(GoA.long) <- c("year", "species", "ssb", "recruits")
ggplot(GoA.long, aes(x = ssb, y = recruits, col = species, shape = species, group = species)) + 
  geom_point(size = 2.75) + 
  scale_x_log10() + scale_y_log10() +  
  scale_color_okabe_ito(labels = GoA$commonname, name = "Species") + 
  scale_shape_manual(values = c(49:54), labels = GoA$commonname, name = "Species") + 
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("log(SSB)") +
  ylab("log(R)") +
  ggtitle("Stock-recruitment of 6 species in the Gulf of Alaska 1978-2017")

##### Fitting models #####

# add log(SSB) and log(R) to data frame
GoA.long$log.ssb <- log(GoA.long$ssb)
GoA.long$log.recruits <- log(GoA.long$recruits)

# Linear regression

model <- lmer(log.recruits ~ log.ssb + (1 + log.ssb | species), data = GoA.long)
# warning - model is singular - assume random slope and random intercept are independent

summary(model)

randomeffects <- ranef(model)

u0 <- randomeffects$species$"(Intercept)"
u1 <- randomeffects$species$"log.ssb"

beta0 <- model@beta[1]
beta1 <- model@beta[2]

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
  ggtitle("Multilevel linear regression for stock-recruitment of 6 species in the Gulf of Alaska 1978-2017")

