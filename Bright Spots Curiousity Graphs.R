library(dplyr)
library(ggplot2)

#creating a temporal graph looking at CPUE in lakes with a specific habitat type over time
walleye_ecoregion <- as.data.frame(walleye_ecoregion) #don't know if this actually does anything
filtered_data <- filter(walleye_ecoregion, Cross_Habitat_Class == "SmallEutrophic") #manipulating the lake type will give you a graph showing all lakes with that classification over time

ggplot(filtered_data, aes(x = year, y = cpue, color = lake_name.1)) +
  geom_line() + 
  geom_point() +
  geom_smooth() + 
  labs(x = "Year", y = "CPUE", color = "Lake Name" )
