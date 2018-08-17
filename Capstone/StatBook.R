library(dplyr)
library(tidyr)
library(ggplot2)
library(knitr)
library(kableExtra)
library(stringr)
library(leaflet)

# Set local working directory
setwd("/Users/JARVIS/RStudio/Capstone")

df <- read.csv("properties_2017_clean.csv")
prop.train.join <- tbl_df(df)


############# Map of Properties #############
lat <- range(prop.train.join$latitude / 1e06, na.rm = TRUE)
long <- range(prop.train.join$longitude / 1e06, na.rm = TRUE)

# Create a new dataframe using prop.train.join so that the values in the original are maintained
temp <- prop.train.join %>% 
  
  # Take a random sample of 2000 parcels in the given dataset
  sample_n(2000) %>% 
  
  # modify the existing values for lat and long and store them in the temp dataframe
  mutate(lat = latitude / 1e6, long = longitude / 1e6) %>% 
  select(parcel_id, lat, long)

# Create the map
leaflet(temp) %>% 
  addTiles() %>% 
  fitBounds(long[1], lat[1], long[2], lat[2]) %>% 
  addCircleMarkers(stroke=FALSE) %>% 
  addMiniMap("bottomleft")

########### Map number of bedrooms in a random sample ###########

minBd <- min(prop.train.join$bed_count, na.rm = TRUE)
maxBd <- max(prop.train.join$bed_count, na.rm = TRUE)
bdDist <- hist(prop.train.join$bed_count, 
               main =  "Number of Bedrooms Distribution", 
               xlab = "Number of Bedrooms",
               xlim = range(minBd, maxBd))


tmp <- prop.train.join %>% 
  sample_n(2000) %>% 
  mutate(lat = latitude / 1e6, long = longitude / 1e6) %>% 
  select(parcel_id, lat, long, bed_count) %>%
  
  # looking at the bedroom distribution, most of the values above 6 can be considered outliers  
  filter(bed_count <= 6)

qpal <- colorNumeric("Blues", tmp$bed_count, 1:6)
leaflet(tmp) %>% 
  addTiles() %>% 
  fitBounds(long[1], lat[1], long[2], lat[2]) %>% 
  addCircleMarkers(stroke = FALSE, color = ~qpal(bed_count)) %>% 
  addLegend("bottomright", pal = qpal, values = ~bed_count, title = "num of beds", opacity = 1)

########### Map number of bathrooms in a random sample ###########

minBth <- min(prop.train.join$bath_count, na.rm = TRUE)
maxBth <- max(prop.train.join$bath_count, na.rm = TRUE)
bthDist <- hist(prop.train.join$bath_count, 
               main =  "Number of Bathrooms Distribution", 
               xlab = "Number of Bathrooms",
               xlim = range(minBth, maxBth))

lat <- range(prop.train.join$latitude / 1e06, na.rm = TRUE)
long <- range(prop.train.join$longitude / 1e06, na.rm = TRUE)

tmp <- prop.train.join %>% 
  sample_n(2000) %>% 
  mutate(lat = latitude / 1e6, long = longitude / 1e6) %>% 
  select(parcel_id, lat, long, bath_count) %>%
  
  # looking at the bath distribution, most of the values above 4 can be considered outliers  
  filter(bath_count <= 4)

qpal <- colorNumeric("Reds", tmp$bath_count, 1:4)
leaflet(tmp) %>% 
  addTiles() %>% 
  fitBounds(long[1], lat[1], long[2], lat[2]) %>% 
  addCircleMarkers(stroke = FALSE, color = ~qpal(bath_count)) %>% 
  addLegend("bottomright", pal = qpal, values = ~bath_count, title = "num of baths", opacity = 1)

########### Map property square footage in a random sample ###########

minBth <- min(prop.train.join$calc_finished_sqft, na.rm = TRUE)
maxBth <- max(prop.train.join$calc_finished_sqft, na.rm = TRUE)
bthDist <- hist(prop.train.join$calc_finished_sqft, 
                main =  "Square Footage Distribution", 
                xlab = "Square Feet",
                xlim = range(minBth, maxBth))

lat <- range(prop.train.join$latitude / 1e06, na.rm = TRUE)
long <- range(prop.train.join$longitude / 1e06, na.rm = TRUE)

tmp <- prop.train.join %>% 
  sample_n(2000) %>% 
  mutate(lat = latitude / 1e6, long = longitude / 1e6) %>% 
  select(parcel_id, lat, long, calc_finished_sqft) %>%
  
  # looking at the square footage distribution, most of the values above 5000 can be considered outliers  
  filter(calc_finished_sqft <= 5000)

qpal <- colorNumeric("Oranges", tmp$calc_finished_sqft, 1:5000)
leaflet(tmp) %>% 
  addTiles() %>% 
  fitBounds(long[1], lat[1], long[2], lat[2]) %>% 
  addCircleMarkers(stroke = FALSE, color = ~qpal(calc_finished_sqft)) %>% 
  addLegend("bottomright", pal = qpal, values = ~calc_finished_sqft, title = "square footage", opacity = 1,
            labFormat = labelFormat())


############## CorrPlots ##############

## CorrPlot With ALL Features ##

# Create a temporary dataframe that removes the categorical variables
tmp <- prop.train.join %>% 
  select(-assessment_year, -fireplace_exists, -tax_delinquency_flag, -has_hot_tub)

# Create the correlation plot using this temporary dataframe
corrplot(cor(tmp, use="complete.obs"), type="lower")


## CorrPlot With Selected Features ##

# Create an array of features that are missing than 37.5% of their values
missLimit <- filter(missing_values, missing_pct < 0.375)

# Pipe the desired keywords into a new array
# in this case we're looking for anything with 'bath', 'bed', 'sqft', and 'tax_amount'
keys <- missLimit$feature[str_detect(missLimit$feature, 'bath|bed|sqft|tax_amount')]

# Create a temporary dataframe that takes the prop.train.join keys and then create the corrplot
tmp <- prop.train.join %>% select(one_of(keys, "total_tax_value_dollars"))
corrplot(cor(tmp, use="complete.obs"), type="lower")


# For some of the parcels (properties) there is transaction information, and one of the most revealing values it has
# it the logerror which effectively calculates how far off the Zestimate was from the actual Sale Price.
# 
# Judging from this graph we can see that a large portion of the values are within 0.2 logerror
train %>% 
  ggplot(aes(x = abs(logerror))) + 
  geom_histogram(bins = 1000, fill="blue") +
  theme_bw() + theme(axis.title = element_text(size = 12), axis.text = element_text(size = 10)) +
  ylab("Count") + coord_cartesian(x = c(0, 0.5), y = c(0, 10000))


