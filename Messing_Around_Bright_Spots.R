walleye_ecoregion <- read.csv("~/Desktop/Hansen Lab 2023-2024/Bright Spots/walleye_ecoregion.csv", header=TRUE)
View(walleye_ecoregion)
library(tidyverse)
glimpse(walleye_ecoregion)

#creating a graph exploring lake type and walleye CPUE
library(ggplot2)
ggplot(walleye_ecoregion, aes(x=Cross_Habitat_Class, y=cpue)) + geom_bar(stat='identity')

#creating the average of the different CPUEs of each lake type
library(dplyr)
group_mean <- walleye_ecoregion %>%
  group_by(Cross_Habitat_Class) %>%
  summarise_at(vars(cpue),
               list(Mean_cpue = mean))

#creating a bar chart for the CPUE based on lake type; need to expand or figure out shorthand for lake types
attach(group_mean)
barplot(Mean_cpue, names=Cross_Habitat_Class, main="Walleye CPUE in Different Lake Classifications")

#creating a side-by-side box and whisker plot for Lake type and CPUE
library(ggplot2)
ggplot(walleye_ecoregion, aes(x=Cross_Habitat_Class, y=cpue)) +
  geom_boxplot() +
  labs(x = "Lake Type", y = "CPUE", title = "Walleye CPUE by Lake Type in Minnesota Lakes")

#creating a side-by-side box and whisker plot for Specific Lake Classification and CPUE (It's a mess)
#NEED HELP WITH CONVERTING Lake_Class TO FACTOR 
Lake_Class <- as.numeric(Lake_Class)  #this converted Lake_Class from "int" (integer) to "fct" (factor)
glimpse(walleye_ecoregion)
cut(Lake_Class, breaks=41) #huh?

#This gets the right number of boxplots (41), but don't know which ones are which and the x axis is all weird
ggplot(walleye_ecoregion, aes(group=Lake_Class, y=cpue, fill=Lake_Class)) +
  geom_boxplot() +
  labs(x = "MN DNR Lake Classification", y = "CPUE", title = "Walleye CPUE by Lake Type in Minnesota Lakes")

#Figuring out how many "Prairie" lakes there are
walleye_ecoregion %>%
  distinct(lake_id, .keep_all=T) %>%
  group_by(Cross_Habitat_Class) %>%
  count()

#Figuring out how many different lake classes are present
walleye_ecoregion %>%
  distinct(Lake_Class, .keep_all=T) %>%
  group_by(Lake_Class) %>%
  count() %>%
  print(n=41)
#MISSING LAKE CLASSES: 9, 26, 44

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#2/20/24
#creating a temporal graph looking at CPUE in lakes with a specific habitat type over time
walleye_ecoregion <- as.data.frame(walleye_ecoregion) #don't know if this actually does anything
filtered_data <- filter(walleye_ecoregion, Cross_Habitat_Class == "SmallEutrophic") #manipulating the lake type will give you a graph showing all lakes with that classification over time

ggplot(filtered_data, aes(x = year, y = cpue, color = lake_name.1)) +
  geom_line() + 
  geom_point() +
  geom_smooth() + 
  theme(legend.position="none")
labs(x = "Year", y = "CPUE", color = "Lake Name" )

#Turning off legend
ggplot(filtered_data, aes(x = year, y = cpue, color = lake_name.1)) +
  geom_line() + 
  geom_point() +
  geom_smooth() + 
  theme(legend.position="none") +
  labs(x = "Year", y = "CPUE", color = "Lake Name" )

#Not breaking it down by lakes
ggplot(walleye_ecoregion) +
  geom_point(aes(x = year, y = cpue, color = lake_name.1)) +
  geom_smooth(aes(x = year, y = cpue), method = "lm")
#Not working for some reason...

#Recreating the box and whisker plots only using data from last ten years
#First, creating a dataset only containing data from last 10 years
recent_data <- walleye_ecoregion |> filter(year >= 2014)

#making sure it worked
recent_data %>%
  distinct(year, .keep_all=T) %>%
  group_by(year) %>%
  count()

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#2/21/2024
#making the boxplot with all time data
ggplot(walleye_ecoregion, aes(x = Lake_Class, y=cpue)) +
  geom_boxplot(aes(group = Lake_Class)) +
  scale_x_continuous(breaks = seq(min(walleye_ecoregion$Lake_Class), max(walleye_ecoregion$Lake_Class), by = 1)) +
  labs(x = "MN DNR Lake Classification", y = "CPUE")

#making the boxplot with data from last 10 years
ggplot(recent_data, aes(x = Lake_Class, y=cpue)) +
  geom_boxplot(aes(group = Lake_Class)) +
  scale_x_continuous(breaks = seq(min(recent_data$Lake_Class), max(recent_data$Lake_Class), by = 1)) +
  labs(x = "MN DNR Lake Classification", y = "CPUE")

#splitting up the data set into two different categories, based on lake classification
classes_1to19 <- walleye_ecoregion |>
  filter(Lake_Class <= 19) #This dataset is missing class 9
classes_20to44 <- walleye_ecoregion |>
  filter(Lake_Class >19) #This dataset is missing classes 26 and 44

#Re-doing boxplots with this split-up data
ggplot(classes_1to19, aes(x = Lake_Class, y=cpue)) +
  geom_boxplot(aes(group = Lake_Class)) +
  scale_x_continuous(breaks = seq(min(classes_1to19$Lake_Class), max(classes_1to19$Lake_Class), by = 1)) +
  labs(x = "MN DNR Lake Classification", y = "CPUE")

ggplot(classes_20to44, aes(x = Lake_Class, y=cpue)) +
  geom_boxplot(aes(group = Lake_Class)) +
  scale_x_continuous(breaks = seq(min(classes_20to44$Lake_Class), max(classes_20to44$Lake_Class), by = 1)) +
  labs(x = "MN DNR Lake Classification", y = "CPUE")

#Filtering data to include all data from pre-2014
old_data <- walleye_ecoregion |> filter(year < 2014)

#making a graph with this older data
ggplot(old_data, aes(x = Lake_Class, y=cpue)) +
  geom_boxplot(aes(group = Lake_Class)) +
  scale_x_continuous(breaks = seq(min(old_data$Lake_Class), max(old_data$Lake_Class), by = 1)) +
  labs(x = "MN DNR Lake Classification", y = "CPUE")

#Next, figuring out which lakes have the most observations
observations_per_lake <- walleye_ecoregion %>%
  count(lake_name.1)
view(observations_per_lake)

#to view this list in descending order, use this command:
observations_per_lake_sorted <- observations_per_lake %>%
  arrange(desc(n))
view(observations_per_lake_sorted)

#Just screwing around: relating cpue_yep with cpue of walleye using data from <10 years ago
filtered_smallyep_wae <- walleye_ecoregion %>%
  filter(cpue_yep <= 100 & cpue < 90)
ggplot(filtered_smallyep_wae, aes(x = cpue_yep, y = cpue)) +
  geom_point() +
  geom_smooth(method = "auto") +
  labs(x = "CPUE Yellow Perch", y = "CPUE Walleye")
#Basically no relationship

#Relating cpue_np and cpue
filtered_smallwae <- walleye_ecoregion %>%
  filter(cpue < 60)
ggplot(filtered_smallwae, aes(x = cpue_np, y = cpue)) +
  geom_point() +
  geom_smooth(method = "auto") +
  labs(x = "CPUE Northern Pike", y = "CPUE Walleye")
#Basically no relationship

#Figuring out which years we have data for (latest year possible)
walleye_ecoregion %>%
  distinct(year, .keep_all=T) %>%
  group_by(year) %>%
  count() %>%
  print(n=43)

#Figuring out how many lakes are in each of the 44 classes
walleye_ecoregion %>%
  distinct(year, .keep_all=T) %>%
  group_by(Lake_Class) %>%
  count() %>%
  print(n=41)

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#3/18/24
#Figuring out how many observations there are for each year
walleye_ecoregion %>%
  group_by(year) %>%
  count() %>%
  print(n=43)

#Figuring out how many different lakes were observed in each year
walleye_ecoregion %>%
  group_by(lake_name.1) %>%
  group_by(year) %>%
  count() %>%
  print(n=43)

#Creating a graph of max depth vs. cpue
ggplot(walleye_ecoregion, aes(x=max_depth, y=cpue)) +
  geom_point() +
  geom_smooth() +
  labs(x="Maximum Depth", y="Walleye CPUE")

#making the same graph but omitting outliers (anything above 45 ft deep)
shallow_lake_depth <- walleye_ecoregion %>%
  filter(max_depth <= 20) #this creates a subset

ggplot(shallow_wae_cpue, aes(x=max_depth, y=cpue)) +
  geom_point() +
  geom_smooth() +
  labs(x="Maximum Depth", y="Walleye CPUE")

#making the same graph but using moderate depths (20-40 ft)
mod_lake_depth <- walleye_ecoregion %>%
  filter(max_depth > 20 & max_depth <= 40)

ggplot(mod_lake_depth, aes(x=max_depth, y=cpue)) +
  geom_point() +
  geom_smooth() +
  labs(x="Maximum Depth", y="Walleye CPUE")

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#3/19/24
#looking at lake area compared to CPUE 
ggplot(walleye_ecoregion, aes(x=area, y=cpue)) +
  geom_point() +
  geom_smooth() +
  labs(x="Lake Area", y="Walleye CPUE")

#similar process above. Shortening data to exclude lakes greater than 5.0x10^7
small_lakes <- walleye_ecoregion %>%
  filter(area < 5.0e+07)

ggplot(small_lakes, aes(x=area, y=cpue)) +
  geom_point() +
  geom_smooth() +
  labs(x="Lake Area", y="Walleye CPUE")

#quest to make a line graph showing walleye CPUE in a specific class of lakes over time
#first, creating a subset for only Northest lakes
northeast_subset <- walleye_ecoregion %>%
  subset(Cross_Habitat_Class == "Northeast") #use 'subset()' for categorical variables

#next, creating a line graph using that data over time
ggplot(northeast_subset, aes(x=year, y=cpue)) +
  geom_point() +
  geom_smooth(method=lm) +
  labs(x="Year", y="Walleye CPUE")

#repeating the same thing for each of the different Cross_Habitat_Class classifications
prairie_subset <- walleye_ecoregion %>%
  subset(Cross_Habitat_Class == "Prairie")

smallmesotrophic_subset <- walleye_ecoregion %>%
  subset(Cross_Habitat_Class == "SmallMesotrophic")

mediummesotrophic_subset <- walleye_ecoregion %>%
  subset(Cross_Habitat_Class == "MediumMesotrophic")

largemesotrophic_subset <- walleye_ecoregion %>%
  subset(Cross_Habitat_Class == "LargeMesotrophic")

smalleutrophic_subset <- walleye_ecoregion %>%
  subset(Cross_Habitat_Class == "SmallEutrophic")

largeeutrophic_subset <- walleye_ecoregion %>%
  subset(Cross_Habitat_Class == "LargeEutrophic")

#now creating graphs for all of those
#prairie
ggplot(prairie_subset, aes(x=year, y=cpue)) +
  geom_point() +
  geom_smooth(method=lm) +
  labs(x="Year", y="Walleye CPUE")

#small mesotrophic
ggplot(smallmesotrophic_subset, aes(x=year, y=cpue)) +
  geom_point() +
  geom_smooth(method=lm) +
  labs(x="Year", y="Walleye CPUE")

#medium mesotrophic
ggplot(mediummesotrophic_subset, aes(x=year, y=cpue)) +
  geom_point() +
  geom_smooth(method=lm) +
  labs(x="Year", y="Walleye CPUE")

#large mesotrophic
ggplot(largemesotrophic_subset, aes(x=year, y=cpue)) +
  geom_point() +
  geom_smooth(method=lm) +
  labs(x="Year", y="Walleye CPUE")

#small eutrophic
ggplot(smalleutrophic_subset, aes(x=year, y=cpue)) +
  geom_point() +
  geom_smooth(method=lm) +
  labs(x="Year", y="Walleye CPUE")

#large eutrophic
ggplot(largeeutrophic_subset, aes(x=year, y=cpue)) +
  geom_point() +
  geom_smooth(method=lm) +
  labs(x="Year", y="Walleye CPUE")

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# comments from meeting with Chris (3/20)
walleye_ecoregion %>%
  group_by(Cross_Habitat_Class) %>%
  summarize(quantile.75 = quantile(cpue, c(.75)))

hotspot_class <-walleye_ecoregion %>%
  group_by(Cross_Habitat_Class) %>%
  summarise( mean = mean(cpue), sd = sd(cpue), one_sd_greater = mean(cpue) + sd(cpue))

temporal_hotspot <- walleye_ecoregion %>%
  merge(hotspot_class, by = "Cross_Habitat_Class") %>% 
  mutate(hotspot = ifelse(cpue > one_sd_greater, 1, 0))
#this creates an additonal column with 1 indicating lakes which are one sd above the mean for cpue, 0 for ones that are not

view(temporal_hotspot)

temporal_hotspot %>%
  ggplot(aes(x = year, y = cpue)) +
  geom_point() +
  geom_hline(aes(yintercept = one_sd_greater)) +
  facet_wrap(~Cross_Habitat_Class)

#proportion of time these lakes were above 1 sd above mean cpue over time
temporal_hotspot %>%
  group_by(lake_id) %>%
  summarize(prop_hotspot = mean(hotspot)) %>%
  filter(prop_hotspot > 0)
  
temporal_hotspot %>%
  group_by(lake_id, Cross_Habitat_Class) %>%
  summarize(prop_hotspot = mean(hotspot)) %>%
  filter(prop_hotspot > 0) %>%
  group_by(Cross_Habitat_Class) %>%
  summarize(class_hotspot = mean(prop_hotspot)) 


ggplot(largeeutrophic_subset, aes(x=year, y=cpue)) +
  geom_point() +
  geom_hline(yintercept = 11.6) +
  labs(x="Year", y="Walleye CPUE")

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#3/26/23
#creating a subset of the lake classes we care about; the 14 classes, and everything BUT Northeast habitat classes
notable_classes <- walleye_ecoregion %>% 
  filter(Lake_Class %in% c(22, 23, 25, 27, 29, 31, 32, 34, 35, 38, 39, 40, 41, 43)) %>% 
  filter(Cross_Habitat_Class %in% c("LargeEutrophic", "LargeMesotrophic", "MediumMesotrophic", "Prairie", "SmallEutrophic", "SmallMesotrophic")) %>% 
  glimpse()

#seeing if it worked
notable_classes %>%
  distinct(Lake_Class, .keep_all=T) %>%
  group_by(Lake_Class) %>%
  count() %>%
  print(n=14)

notable_classes %>%
  distinct(Cross_Habitat_Class, .keep_all=T) %>%
  group_by(Cross_Habitat_Class) %>%
  count() %>%
  print(n=6)
#it worked!

view(notable_classes)

#combining this new subset with codes provided by Chris; this gives the 75th quantile for CPUE (anything above this is a hotspot)
walleye_ecoregion %>%
  group_by(Cross_Habitat_Class) %>%
  summarize(quantile.75 = quantile(cpue, c(.75)))
#this should probably stay in reference to ALL Cross_Habitat_Classes, not just those in this new subset

#gives the mean, sd, and one sd greater than mean for each Cross_Habitat_Class from the whole dataset
hotspot_class <- walleye_ecoregion %>%
  group_by(Cross_Habitat_Class) %>%
  summarise( mean = mean(cpue), sd = sd(cpue), one_sd_greater = mean(cpue) + sd(cpue))

#creating a column showing a 1 if a lake is 1 sd above the mean for every lake in that cross_habitat_class across the state
hotspot_notable_classes <- notable_classes %>%
  merge(hotspot_class, by = "Cross_Habitat_Class") %>% 
  mutate(hotspot = ifelse(cpue > one_sd_greater, 1, 0))

view(hotspot_notable_classes)

#seeing if it worked
hotspot_notable_classes %>%
  distinct(Lake_Class, .keep_all=T) %>%
  group_by(Lake_Class) %>%
  count() %>%
  print(n=14)

hotspot_notable_classes %>%
  distinct(Cross_Habitat_Class, .keep_all=T) %>%
  group_by(Cross_Habitat_Class) %>%
  count() %>%
  print(n=6)
#holy shit it actually worked


--------------------------------------------------------------------------------
#THIS IS THE CODE FOR CREATING THE hotspot_notable_classes SUBSET IN ONE PLACE
  
notable_classes <- walleye_ecoregion %>% 
  filter(Lake_Class %in% c(22, 23, 25, 27, 29, 31, 32, 34, 35, 38, 39, 40, 41, 43)) %>% 
  filter(Cross_Habitat_Class %in% c("LargeEutrophic", "LargeMesotrophic", "MediumMesotrophic", "Prairie", "SmallEutrophic", "SmallMesotrophic")) 

hotspot_class <- walleye_ecoregion %>%
  group_by(Cross_Habitat_Class) %>%
  summarise( mean = mean(cpue), sd = sd(cpue), one_sd_greater = mean(cpue) + sd(cpue))

hotspot_notable_classes <- notable_classes %>%
  merge(hotspot_class, by = "Cross_Habitat_Class") %>% 
  mutate(hotspot = ifelse(cpue > one_sd_greater, 1, 0))

view(hotspot_notable_classes)
--------------------------------------------------------------------------------
  
#proportion of time these lakes were above 1 sd above mean cpue over time
hotspot_notable_classes %>%
  group_by(lake_id) %>%
  summarize(prop_hotspot = mean(hotspot)) %>%
  filter(prop_hotspot > 0)
#this groups it by specific lake_id

#and this groups it by Cross_Habitat_Class
hotspot_notable_classes %>%
  group_by(lake_id, Cross_Habitat_Class) %>%
  summarize(prop_hotspot = mean(hotspot)) %>%
  filter(prop_hotspot > 0) %>%
  group_by(Cross_Habitat_Class) %>%
  summarize(class_hotspot = mean(prop_hotspot)) 
#not really sure why it includes lake_id, maybe ask Chris? Maybe because it's grouped by that within the different Cross_Habitat_Class categories?

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#3/27/24

#Seeing which lake Cross_Habitat_Class categories have the highest frequencies of cpue greater than 1 sd above the mean
hotspot_notable_classes %>%
  group_by(Cross_Habitat_Class) %>%
  summarize(prop_hotspot = mean(hotspot)) %>%
  filter(prop_hotspot > 0)
#maybe this means ignoring small eutrophic??

#Seeing which Lake_Class categories have the highest frequencies of cpue greater than 1 sd above the mean
hotspot_notable_classes %>%
  group_by(Lake_Class) %>%
  summarize(prop_hotspot = mean(hotspot)) %>%
  filter(prop_hotspot > 0)

#making a boxplot for each Lake_Class
ggplot(hotspot_notable_classes, aes(x = Lake_Class, y=cpue)) +
  geom_boxplot(aes(group = Lake_Class)) +
  scale_x_continuous(breaks = seq(min(hotspot_notable_classes$Lake_Class), max(hotspot_notable_classes$Lake_Class), by = 1)) +
  labs(x = "MN DNR Lake Class", y = "CPUE")
#could maybe do this for indiidual Cross_Habitat_Class variables?

