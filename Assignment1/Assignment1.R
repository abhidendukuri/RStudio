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
cleaned <- products %>% 
            separate(Product.code...number, c("product_code", "product_number"), "-") %>%
            mutate(product_category = recode(product_code, "'p'='smartphone';'v'='tv';'x'='laptop';'q'='tablet'")) %>%
  
            #merge address, city, country for geocoding
            mutate(full_address = paste(address, city, country, sep = ", ")) %>%
            select(company, product_code, product_category, product_number, full_address, name) %>%
            
            # create binary company columns
            mutate(company_philips = as.integer(runif(25, 0, 1))) %>%
            mutate(company_akzo = as.integer(runif(25, 0, 1))) %>%
            mutate(company_van_houten = as.integer(runif(25, 0, 1))) %>%
            mutate(company_unilever = as.integer(runif(25, 0, 1))) %>%
            
            # create binary product columns
            mutate(product_philips = as.integer(runif(25, 0, 1))) %>%
            mutate(product_akzo = as.integer(runif(25, 0, 1))) %>%
            mutate(product_van_houten = as.integer(runif(25, 0, 1))) %>%
            mutate(product_unilever = as.integer(runif(25, 0, 1))) %>%
            select(company, product_code, product_category, product_number, full_address, name, 
                   company_philips, company_akzo, company_van_houten, company_unilever,
                   product_philips, product_akzo, product_van_houten, product_unilever) %>%
            print(n=25)

write.csv(cleaned, "refine_clean.csv")
