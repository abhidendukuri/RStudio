library(dplyr)
library(tidyr)
library(car)
df <- read.csv("refine_original.csv")
products <- tbl_df(df)

#replace names
products$company <- tolower(products$company)
products$company <- sub(pattern = ".*ps$", replacement = "Philips", x = products$company)
products$company <- sub(pattern = "^a.*", replacement = "Akzo", x = products$company)
products$company <- sub(pattern = "^v.*", replacement = "Van Houten", x = products$company)
products$company <- sub(pattern = "^u.*", replacement = "Unilever", x = products$company)

#separate product code/number
products %>% 
  separate(Product.code...number, c("product_code", "product_number"), "-") %>%
  mutate(product_category = recode(product_code, "'p'='smartphone';'v'='tv';'x'='laptop';'q'='tablet'")) %>%
  
  #merge address, city, country for geocoding
  mutate(full_address = paste(address, city, country, sep = ", ")) %>%
  select(company, product_code, product_category, product_number, full_address, name) %>%
  print(n=20)

write.csv(products, "refine_clean.csv")
