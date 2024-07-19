#Need to load in data each time you run this

library(dplyr)
library(ggplot2)

filtered_ramsey_county_zooplankton <- ramsey_county_zooplankton %>% 
  filter(!is.na(invasion_status))

ggplot(
  data = filtered_ramsey_county_zooplankton,
  mapping = aes(x = invasion_status , y = BOSM.THOUS.M3)
) +
  geom_boxplot()