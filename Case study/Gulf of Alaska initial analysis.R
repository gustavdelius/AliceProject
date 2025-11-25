library(dplyr)

##### Gulf of Alaska initial analysis #####

# Read in data
alaska1 <- read.csv("~/GitHub/extended-independent-project/NOAA Fisheries data/Assessment_TimeSeries_Data_Part_1.csv")
alaska2 <- read.csv("~/GitHub/extended-independent-project/NOAA Fisheries data/Assessment_TimeSeries_Data_Part_2.csv")
alaska <- cbind(alaska1, alaska2[,-c(1,2)]) # Combine separate files into 1

# Tidying up
alaska <- as.data.frame(t(as.matrix(alaska)))
colnames(alaska)[1:7] <- alaska[1,1:7]
colnames(alaska)[8:73] <- alaska[2,8:73]
alaska <- alaska[-c(1,2),]
alaska <- cbind(row.names(alaska), alaska)
colnames(alaska)[1] <- "Species"
row.names(alaska) <- NULL
alaska$Species <- sub("\\.\\.\\..*", "", alaska$Species) # keeps the species name
alaska$Species <- gsub("\\.", " ", alaska$Species) # removes dots in name


# Stock data
alaska.ssb <- alaska[alaska$Parameter == "Abundance",]

table(alaska.ssb$Description)
# 61 values are recorded as mature female biomass, 28 as spawning stock biomass

# Recruitment data
alaska.r <- alaska[alaska$Parameter == "Recruitment",]
