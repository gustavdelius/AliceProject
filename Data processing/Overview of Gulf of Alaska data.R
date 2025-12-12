### Overview of data

library(ggplot2)
library(ggokabeito)

load("~/GitHub/Extended-Independent-Project/Gulf_Of_Alaska.RData")

ssb <- GoA.data[,1:3]

ggplot(ssb, aes(x = year, y = ssb, col = species, group = species)) + 
  geom_line() + 
  scale_y_log10() + 
  scale_color_okabe_ito(labels = GoA.species$commonname, name = "Species") + 
  scale_x_continuous(breaks = scales::pretty_breaks(n = 7)) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Year") +
  ylab("log(SSB)") +
  ggtitle("Spawning stock of 6 species in the Gulf of Alaska 1980-2017")

recruits <- GoA.data[,c(1,2,4)]

ggplot(recruits, aes(x = year, y = recruits, col = species, group = species)) + 
  geom_line() + 
  scale_y_log10() +  
  scale_color_okabe_ito(labels = GoA.species$commonname, name = "Species") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 7)) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Year") +
  ylab("log(R)") +
  ggtitle("Recruitment of 6 species in the Gulf of Alaska 1980-2017")

ggplot(GoA.data, aes(x = log.ssb, y = log.recruits, col = species, shape = species, group = species)) + 
  geom_point(size = 2.75) + 
  scale_color_okabe_ito(labels = GoA.species$commonname, name = "Species") + 
  scale_shape_manual(values = c(49:54), labels = GoA.species$commonname, name = "Species") + 
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("log(SSB)") +
  ylab("log(R)") +
  ggtitle("Stock-recruitment of 6 species in the Gulf of Alaska 1980-2017")
