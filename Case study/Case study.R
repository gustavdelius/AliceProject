### Finding case study

# reduce stock table to variables i care about
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
library(dplyr)
n_distinct(cs$areaname) # 132 different areas

# gulf of alaska - 19 species
GoA <- cs[cs$areaname == 'Gulf of Alaska',]

