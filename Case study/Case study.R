library(dplyr)
library(reshape2)  
library(ggplot2) 

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

GoA.ssb <- select(ssb.data, ends_with("GA")) # find ssb data
remove1 <- c("HERRRIGA", "PSALMPYBUSGA", "SABLEFEBSAIGA") # species that aren't in GoA
GoA.ssb <- select(GoA.ssb, -remove1)

# remove species that don't interact with other species
remove2 <- c("DSOLEGA", "DUSROCKGA", "NROCKGA", "NRSOLEGA", "REXSOLEGA", "REYEROCKGA")
GoA <- GoA[!(GoA$stockid %in% remove2),]
GoA.ssb <- select(GoA.ssb, -remove2)

# remove species from original list that don't have stock data
remove3 <- c("ATKAGA", "BIGSKAGA", "GPOCTOGA", "LNOSESKAGA", "SRAKEROCKGA", "SSTHORNHGA", "YEYEROCKGA")
GoA <- GoA[!(GoA$stockid %in% remove3),]

GoA.r <- select(r.data, ends_with("GA")) # find recruitment data

GoA.r <- select(GoA.r, -any_of(remove1)) # remove species that aren't in GoA
GoA.r <- select(GoA.r, -any_of(remove2)) # remove species that don't interact 
GoA.r <- select(GoA.r, -any_of(remove3)) # remove species that don't have ssb data

GoA.ssb <- na.omit(GoA.ssb) # remove rows with missing values
GoA.r <- na.omit(GoA.r)

##### Overview of data #####

# SSB data
GoA.ssb <- data.frame(year = row.names(GoA.ssb), GoA.ssb) # add year as the first column
row.names(GoA.ssb) <- NULL # remove row names

ssb.long <- melt(GoA.ssb, id.vars = "year") # converts data to long form for use in plot
colnames(ssb.long) <- c("year", "species", "ssb")
ggplot(ssb.long, aes(x = year, y = ssb, col = species, group = species)) + geom_line()

# Recruit data
GoA.r <- data.frame(year = row.names(GoA.r), GoA.r)
row.names(GoA.r) <- NULL

r.long <- melt(GoA.r, id.vars = "year") # converts data to long form
colnames(r.long) <- c("year", "species", "recruits")
ggplot(r.long, aes(x = year, y = recruits, col = species, group = species)) + geom_line()

# Plotting SSB vs recruits
GoA.long <- data.frame(ssb.long, r.long$recruits)
colnames(GoA.long) <- c("year", "species", "ssb", "recruits")
ggplot(GoA.long, aes(x = ssb, y = recruits, col = species, group = species)) + geom_line()

