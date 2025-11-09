library(FSA)
library(car)
library(dplyr)
library(magrittr)
library(plotrix)
library(nlstools)
library(lsmeans)
library(FSAsim)

### (first attempt not great results)
# Pacific coast Arrowtooth flounder (ARFLOUNDPCOAST) data from 1916 to 2021
af.r <- r.data$ARFLOUNDPCOAST # retrieve recruitment data
names(af.r) <- rownames(r.data) # add row names (years)
af.r <- na.omit(af.r) # remove missing values to restrict to 1916-2021
plot(af.r, 
     xaxt = 'n', 
     main = 'Arrowtooth flounder recruitment 1916-2021',
     xlab = 'Year',
     ylab = 'Recruitment')
axis(1, at = 1:length(af.r), labels = names(af.r))

# Restricting to 1964-2016 
af.r <- af.r[names(af.r) >= "1964" & names(af.r) <= "2016"]
plot(af.r, 
     xaxt = 'n', 
     main = 'Pacific coast arrowtooth flounder recruitment 1964-2016',
     xlab = 'Year',
     ylab = 'Recruitment')
axis(1, at = 1:length(af.r), labels = names(af.r))

af.ssb <- ssb.data$ARFLOUNDPCOAST # retrieve spawning stock biomass 
names(af.ssb) <- rownames(ssb.data) # add row names (years)
af.ssb <- af.ssb[names(af.ssb) >= "1964" & names(af.ssb) <= "2016"] # restrict to 1964-2016
plot(af.ssb, 
     xaxt = 'n', 
     main = 'Pacific coast arrowtooth flounder SSB 1964-2016',
     xlab = 'Year',
     ylab = 'Spawning stock biomass (SSB)')
axis(1, at = 1:length(af.ssb), labels = names(af.ssb))

plot(af.ssb, af.r) # plot stock against recruitment

# Combine into one matrix
af <- cbind(af.ssb, af.r)
colnames(af) <- c('ssb', 'recruits')
af <- as.data.frame(af) # turn into data frame to use in later functions
af$recruits <- af$recruits/1000000 # scaling down recruits to millions
af$logrec <- log(af$recruits) # create column of natural log of recruits

# Fitting models (13.1.3 of textbook)

# Ricker
afR.sv <- srStarts(recruits~ssb, data = af, type = 'Ricker') # calculates starting values for parameters
srStartsDP(recruits~ssb, data = af, type = 'Ricker') # dynamic plot to vary parameters

ricker <- srFuns('Ricker')
afR.sr <- nls(logrec~log(ricker(ssb, a, b)), data = af, start = afR.sv) # create non-linear least squares model
cbind(estimates = coef(afR.sr), confint(afR.sr)) # extract parameter estimates and confidence intervals

bootR <- nlsBoot(afR.sr) # alternatively can use bootstrap to find confidence intervals
cbind(estimates=coef(afR.sr),confint(bootR))

x <- seq(min(af$ssb), max(af$ssb), length.out = 200) # ssb for prediction
pR <- ricker(x,a=coef(afR.sr)) # predicted mean R
LCI <- UCI <- numeric(length(x))
for(i in 1:length(x)){
  tmp <- apply(bootR$coefboot, MARGIN = 1, FUN = ricker, S = x[i])
  LCI[i] <- quantile(tmp, 0.025)
  UCI[i] <- quantile(tmp, 0.975)
}
ylmts <- range(c(pR, LCI, UCI, af$recruits))
xlmts <- range(c(x, af$ssb))
plot(recruits~ssb, data = af, xlim = xlmts, ylim = ylmts, col = 'white', # plot model
     ylab = 'Recruits (millions)',
     xlab = 'Stock',
     main = 'Ricker model for Pacific coast arrowhead flounder')
polygon(c(x, rev(x)), c(LCI, rev(UCI)), col = 'gray80', border = NA) # plot confidence interval
points(recruits~ssb, data = af, pch = 19, col = rgb(0,0,0,1/2)) # plot points
lines(pR~x, lwd = 2) # plot model line

# Beverton- Holt

afBH.sv <- srStarts(recruits~ssb, data = af, type = 'BevertonHolt') # calculates starting values for parameters
# !!! generated a and b likely poor starting values
# srStartsDP(recruits~ssb, data = af, type = 'BevertonHolt') # dynamic plot

bevholt <- srFuns('BevertonHolt')
afBH.sr <- nls(logrec~log(bevholt(ssb, a, b)), data = af, start = afBH.sv) # create non-linear least squares model
cbind(estimates = coef(afBH.sr), confint(afBH.sr)) # extract parameter estimates and confidence intervals

# !!! this said 50 or more warnings
bootBH <- nlsBoot(afBH.sr) # alternatively can use bootstrap to find confidence intervals
cbind(estimates=coef(afBH.sr),confint(bootBH))

x <- seq(min(af$ssb), max(af$ssb), length.out = 200) # ssb for prediction
pBH <- bevholt(x,a=coef(afBH.sr)) # predicted mean R
LCI <- UCI <- numeric(length(x))
for(i in 1:length(x)){
  tmp <- apply(bootBH$coefboot, MARGIN = 1, FUN = bevholt, S = x[i])
  LCI[i] <- quantile(tmp, 0.025)
  UCI[i] <- quantile(tmp, 0.975)
}
ylmts <- range(c(pBH, LCI, UCI, af$recruits))
xlmts <- range(c(x, af$ssb))
plot(recruits~ssb, data = af, xlim = xlmts, ylim = ylmts, col = 'white', # plot model
     ylab = 'Recruits (millions)',
     xlab = 'Stock',
     main = 'Beverton-Holt model for Pacific coast arrowhead flounder')
polygon(c(x, rev(x)), c(LCI, rev(UCI)), col = 'gray80', border = NA) # plot confidence interval
points(recruits~ssb, data = af, pch = 19, col = rgb(0,0,0,1/2)) # plot points
lines(pBH~x, lwd = 2) # plot model line

