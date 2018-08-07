library(dplyr)
library(tidyr)
library(ggplot2)
library(knitr)
library(kableExtra)
library(stringr)
library(mice)
# Set local working directory
setwd("/Users/JARVIS/RStudio/Capstone")

# Read in the data and convert it into a dataframe
df <- read.csv("properties_2017.csv")
properties <- tbl_df(df)

df2 <- read.csv("train_2017.csv")
train <- tbl_df(df2)

# Rename the columns for easier viewing
properties <- properties %>% rename(
    parcel_id = parcelid,
    ac_type_id = airconditioningtypeid,
    arch_style_type_id = architecturalstyletypeid,
    basement_sqft = basementsqft,
    bath_count = bathroomcnt,
    bed_count = bedroomcnt,
    build_quality_type_id = buildingqualitytypeid,
    build_class_type_id = buildingclasstypeid,
    calc_bath_and_bed = calculatedbathnbr,
    deck_type_id = decktypeid,
    finished_floor_1_sqft = finishedfloor1squarefeet,
    calc_finished_sqft = calculatedfinishedsquarefeet,
    finished_living = finishedsquarefeet12,
    finished_perimeter_living = finishedsquarefeet13,
    finished_total_area = finishedsquarefeet15,
    finished_living_size = finishedsquarefeet50,
    base_area = finishedsquarefeet6,
    fips = fips,
    fireplace_count = fireplacecnt,
    full_bath_count = fullbathcnt,
    garage_car_count = garagecarcnt,
    garage_sqft = garagetotalsqft,
    has_hot_tub = hashottuborspa,
    heat_system_type_id = heatingorsystemtypeid,
    latitude = latitude,
    longitude = longitude,
    lot_size = lotsizesquarefeet,
    pool_count = poolcnt,
    pool_size = poolsizesum,
    spa_or_tub = pooltypeid10,
    pool_no_tub = pooltypeid2,
    pool_and_tub = pooltypeid7,
    county_land_use_code = propertycountylandusecode,
    land_type_id = propertylandusetypeid,
    land_zone_desc = propertyzoningdesc,
    raw_census_tract_and_block = rawcensustractandblock,
    region_id_city = regionidcity,
    region_id_county = regionidcounty,
    region_id_neighborhood = regionidneighborhood,
    region_id_zip = regionidzip,
    room_count = roomcnt,
    floor_type_id = storytypeid,
    three_quarter_bath = threequarterbathnbr,
    construction_material_type_id = typeconstructiontypeid,
    unit_count = unitcnt,
    patio_in_yard = yardbuildingsqft17,
    shed_in_yard = yardbuildingsqft26,
    year_built = yearbuilt,
    num_stories = numberofstories,
    fireplace_exists = fireplaceflag,
    struct_tax_value_dollars = structuretaxvaluedollarcnt,
    total_tax_value_dollars = taxvaluedollarcnt,
    assessment_year = assessmentyear,
    land_tax_value_dollars = landtaxvaluedollarcnt,
    tax_amount = taxamount,
    tax_delinquency_flag = taxdelinquencyflag,
    tax_delinquency_year = taxdelinquencyyear,
    census_tract_and_block = censustractandblock
)

train <- train %>% rename(
  parcel_id = parcelid,
  date = transactiondate
)


prop.train.join <- merge(properties, train, by="parcel_id")
nrow(properties)
nrow(train)
nrow(prop.train.join)

# Remove categorical features with more than 50 levels (required for random forest)
prop.train.join <- prop.train.join %>% 
  select(-county_land_use_code, -land_zone_desc, -date)

# Find all the missing values and display the percent of values each column (representing a factor)
# is missing
missing_values <- prop.train.join %>% summarize_all(funs(sum(is.na(.)/n())))
missing_values <- gather(missing_values, key="feature", value="missing_pct")
missing_values %>% 
  ggplot(aes(x = reorder(feature, -missing_pct), y = missing_pct)) +
  geom_bar(stat = "identity", fill = "blue") +
  coord_flip() + 
  theme_bw()


# Remove the columns that are missing more than 37.5% of its values
percentMiss <- sapply(prop.train.join, function(x) {sum(is.na(x)) / length(x)})
removeMiss <- which(percentMiss > 0.333)
prop.train.join <- subset(prop.train.join, select=-removeMiss)

# Omit any remaining rows that have an NA value
prop.train.join <- na.omit(prop.train.join)

prop.train.join %>%
  select(struct_tax_value_dollars, land_tax_value_dollars, total_tax_value_dollars) %>%
  head()

# CorrPlot
tmp <- prop.train.join %>% 
  select(-assessment_year, -fireplace_exists, -tax_delinquency_flag, -has_hot_tub)
corrplot(cor(tmp, use="complete.obs"), type="lower")

#write.csv(prop.train.join, "properties_2017_clean.csv")



#imputeVals <- mice(prop.train.join, m = 1, maxIt = 3, method = "cart")
#prop.train.impute <- tbl_df(imputeVals)
#write.csv(prop.train.impute, "properties_2017_imputed.csv")




