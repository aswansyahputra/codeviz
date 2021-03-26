# Load packages -----------------------------------------------------------
library(httr)
library(tidyverse)
library(lubridate)
library(slider)
library(scales)


# Fetch data from API -----------------------------------------------------

resp_jakarta <- 
  GET("https://data.covid19.go.id/public/api/prov_detail_DKI_JAKARTA.json")

cov_jakarta_raw <- 
  content(resp_jakarta, as = "parsed", simplifyVector = TRUE) %>% 
  pluck("list_perkembangan") %>% 
  as_tibble()

glimpse(cov_jakarta_raw)

# Clean and extract data in 2020 ------------------------------------------

cov_jakarta <- cov_jakarta_raw %>%
  select(-contains("DIRAWAT_OR_ISOLASI")) %>% 
  select(-starts_with("AKUMULASI")) %>% 
  rename(
    date = tanggal,
    newcase = KASUS,
    death = MENINGGAL,
    recovered = SEMBUH
  ) %>% 
  mutate(
    date = as.POSIXct(date / 1000, origin = "1970-01-01"),
    date = as.Date(date)
  ) %>% 
  filter(year(date) == 2020)

glimpse(cov_jakarta)

# Save data into the project ----------------------------------------------

save(cov_jakarta, file = "data/cov_jakarta.rda", compress = "bzip2", compression_level = 9)
