### Data processing

library(dplyr)
library(reshape2)

load("~/GitHub/Extended-Independent-Project/RAM Legacy R Data/DBdata[asmt][v4.66].RData")

# GoA

stock.areas <- as.data.frame(cbind(stock$stockid, stock$scientificname, stock$commonname, stock$areaid, stock$region))
colnames(stock.areas) <- c('stockid', 'scientificname', 'commonname', 'areaid', 'region')

areas <- as.data.frame(cbind(area$areaid, area$areaname))
colnames(areas) <- c('areaid', 'areaname')
stock.areas$areaname <- areas$areaname[match(stock.areas$areaid, areas$areaid)]

GoA.species <- stock.areas[stock.areas$areaname == 'Gulf of Alaska',]
species <- c("ARFLOUNDGA", "FLSOLEGA", "PCODGA", "POPERCHGA", "RSOLEGA", "WPOLLGA")
GoA.species <- GoA.species[GoA.species$stockid %in% species, 1:3]

# GoA.data

GoA.ssb <- select(ssb.data, any_of(species))
GoA.r <- select(r.data, any_of(species))

GoA.ssb <- na.omit(GoA.ssb) # remove rows with missing values
GoA.r <- na.omit(GoA.r)

colnames(GoA.ssb) <- GoA.species$commonname
GoA.ssb <- data.frame(year = as.numeric(row.names(GoA.ssb)), GoA.ssb) # add year as the first column
row.names(GoA.ssb) <- NULL # remove row names

colnames(GoA.r) <- GoA.species$commonname
GoA.r <- data.frame(year = as.numeric(row.names(GoA.r)), GoA.r)
row.names(GoA.r) <- NULL

GoA.r <- GoA.r %>%
  mutate(
    Arrowtooth.flounder = lag(Arrowtooth.flounder, 1), # recruitment age 1
    Pacific.ocean.perch = lag(Pacific.ocean.perch, 2), # recruitment age 2
    Walleye.pollock = lag(Walleye.pollock, 1)          # recruitment age 1
  )

GoA.r <- na.omit(GoA.r)
GoA.ssb <- GoA.ssb[-c(1,2),] # no longer need the first two rows

ssb.long <- melt(GoA.ssb, id.vars = "year") # converts data to long form for use in plot
colnames(ssb.long) <- c("year", "species", "ssb")

r.long <- melt(GoA.r, id.vars = "year") # converts data to long form
colnames(r.long) <- c("year", "species", "recruits")

GoA.data <- data.frame(ssb.long, r.long$recruits)
colnames(GoA.data) <- c("year", "species", "ssb", "recruits")

GoA.data$log.ssb <- log(GoA.data$ssb)
GoA.data$log.recruits <- log(GoA.data$recruits)

GoA.data$species <- gsub("\\.", " ", GoA.data$species)
