library(FSA)
library(car)
library(dplyr)
library(magrittr)
library(plotrix)
library(nlstools)
library(lsmeans)
library(FSAsim)

# North Sea European plaice (PLAICNS)

ep.r <- r.data$PLAICNS # recruitment data
names(ep.r) <- rownames(r.data) # add row names (years)
ep.r <- na.omit(ep.r) # data from 1956 - 2022

plot(ep.r, # plotting recruitment
     xaxt = 'n', 
     main = 'North Sea European plaice recruitment 1956 - 2022',
     xlab = 'Year',
     ylab = 'Recruitment',
     pch = 16)
axis(1, at = 1:length(ep.r), labels = names(ep.r))

plot(log(ep.r), # plotting log recruitment
     xaxt = 'n', 
     main = 'North Sea European plaice log recruitment 1956 - 2022',
     xlab = 'Year',
     ylab = 'log(Recruitment)',
     pch = 16)
axis(1, at = 1:length(ep.r), labels = names(ep.r))

ep.ssb <- ssb.data$PLAICNS
names(ep.ssb) <- rownames(ssb.data)
ep.ssb <- na.omit(ep.ssb)
plot(ep.ssb, 
     xaxt = 'n',
     main = 'North Sea European plaice SSB 1957 - 2022',
     xlab = 'Year',
     ylab = 'SSB',
     pch = 16)
axis(1, at = 1:length(ep.ssb), labels = names(ep.ssb))

par(mfrow = c(1, 2))

plot(ep.ssb, ep.r,
     main = 'Stock-recruitment for North Sea European plaice 1957 - 2022',
     xlab = 'SSB',
     ylab = 'Recruitment',
     pch = 16)

plot(ep.ssb, log(ep.r),
     main = 'Stock-log(recruitment) for North Sea European plaice 1957 - 2022',
     xlab = 'SSB',
     ylab = 'log(Recruitment)',
     pch = 16)

# residual investigation?

ep <- cbind(ep.ssb, ep.r)
colnames(ep) <- c('ssb', 'recruits')
ep <- as.data.frame(ep)
ep$logrec <- log(ep$recruits)

epR.sv <- srStarts(recruits~ssb, data = ep, type = 'Ricker')
srStartsDP(recruits~ssb, data = ep, type = 'Ricker') 

ricker <- srFuns('Ricker')

epR.sr <- nls(logrec~log(ricker(ssb, a, b)), data = ep, start = epR.sv) # create non-linear least squares model
cbind(estimates = coef(epR.sr), confint(epR.sr)) # extract parameter estimates and confidence intervals

bootR <- nlsBoot(epR.sr) # alternatively can use bootstrap to find confidence intervals
cbind(estimates=coef(epR.sr),confint(bootR))

x <- seq(min(ep$ssb), max(ep$ssb), length.out = 200) # ssb for prediction
epR <- ricker(x,a=coef(epR.sr)) # predicted mean R
LCI <- UCI <- numeric(length(x))
for(i in 1:length(x)){
  tmp <- apply(bootR$coefboot, MARGIN = 1, FUN = ricker, S = x[i])
  LCI[i] <- quantile(tmp, 0.025)
  UCI[i] <- quantile(tmp, 0.975)
}
ylmts <- range(c(epR, LCI, UCI, ep$recruits))
xlmts <- range(c(x, ep$ssb))
par(mfrow = c(1, 1))
plot(recruits~ssb, data = ep, xlim = xlmts, ylim = ylmts, col = 'white', # plot model
     ylab = 'Recruits',
     xlab = 'Stock',
     main = 'Ricker model for North Sea European plaice')
polygon(c(x, rev(x)), c(LCI, rev(UCI)), col = 'gray80', border = NA) # plot confidence interval
points(recruits~ssb, data = ep, pch = 19, col = rgb(0,0,0,1/2)) # plot points
lines(epR~x, lwd = 2) # plot model line
