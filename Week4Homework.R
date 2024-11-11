# Load necessary library
library(tidyverse)
library(dplyr)
library(here)
library(sf)

# Read the files
hdr_data <- read.csv(here('hdr-data.csv'))
WorldCountries <- st_read('World_Countries_(Generalized)_9029012925078512962.geojson')

# Check the structure of the data
head(hdr_data)

# Tidying Data
hdr_data_tidy <- hdr_data %>% select(,c(1,2,8,9))

hdr_data_tidy_again <- hdr_data_tidy %>% 
  pivot_wider(
    names_from = year,
    values_from = value
  )

# Calculate the difference between 2010 and 2019 inequality values and create a new column
class('hdr_data_tidy_again')

hdr_data_final <- hdr_data_tidy_again %>%
  mutate(`2010` = as.numeric(hdr_data_tidy_again$`2010`),
         `2019` = as.numeric(hdr_data_tidy_again$`2019`),
         difference = `2019` - `2010`)

# View the updated data
head(hdr_data_final)

#ISO Transformation
install.packages('countrycode')
library(countrycode)

hdr_data_final$countryIsoCode <- hdr_data_final$countryIsoCode %>% countrycode(., origin = "iso3c", destination = "iso2c")

#Combination DFs
WorldDataMap <- WorldCountries %>%
  merge(.,
        hdr_data_final, 
        by.x="ISO", 
        by.y="countryIsoCode",
        no.dups = TRUE)

#Painting
install.packages("ggplot2")
library(ggplot2)

ggplot(data = WorldDataMap) +
  geom_sf(aes(fill = difference)) +
  scale_fill_viridis_c() +
  theme_minimal() +
  labs(title = "GII Difference in inequality between 2010 and 2019",
       fill = "Difference")