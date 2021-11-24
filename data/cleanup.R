library(tidyverse)
library(readxl)
library(janitor)
library(here)

pubs_ons <- read_excel(
  here("data/publichousesandbarsbylocalauthority20012018.xls"),
  sheet = "Pubs size LA",
  skip = 2
) #Made using GUI as helper

pubs_cleaned <- pubs_ons %>%
  clean_names() %>%
  rename(num_employees = number_of_employees_in_public_house_or_bar) %>%
  filter(
    if_any(everything(), ~ !is.na(.)), #Remove rows that are all NA
    !is.na(area_name), #Remove notes at the end
    str_sub(area_code, start = 1, end = 3) %in%
      c("E06", "E07", "E08", "E09", "W06", "S12", "N09"),
      #Filter for local authorities, using area codes: see
      #https://en.wikipedia.org/wiki/ONS_coding_system
    num_employees == "Any number of employees" #Just get total counts
  ) %>%
  select(-num_employees) %>%
  pivot_longer(
    -c(area_code, area_name),
    names_to = "yr",
    names_prefix = "x",
    values_to = "num_pubs"
  ) %>%
  mutate(
    area_code = case_when(
      area_name == "Fife"              ~ "S12000047",
      area_name == "Perth and Kinross" ~ "S12000048",
      TRUE                             ~ area_code
    ) #Some codes changed in 2018
  )

pop_ons <- read_csv(
  here("data/MYEB1_detailed_population_estimates_series_UK_(2018).csv")
) #Again using the GUI to help here

pop_cleaned <- pop_ons %>%
  clean_names() %>%
  select(-c(lad2018_name, country)) %>%
  pivot_longer(
    -c(lad2018_code, sex, age),
    names_to = "yr",
    names_prefix = "population_",
    values_to = "pop"
  ) %>%
  group_by(lad2018_code, yr) %>%
  summarise(pop = sum(pop), .groups = "drop")

pubs_pc <- pubs_cleaned %>%
  left_join(pop_cleaned, by = c("area_code" = "lad2018_code", "yr")) %>%
  mutate(pubs_per_capita = num_pubs/pop)

saveRDS(pubs_pc, here("data/pubs-pc.rds"))

pubs_2018 <- pubs_pc %>%
  filter(yr == "2018") %>%
  select(-yr)


area_ons <- read_csv(
  here("data/SAM_LAD_DEC_2018_UK.csv"), 
  col_types = cols(...7 = col_skip(), ...8 = col_skip())
) #Again using GUI; we get a message that we can ignore because we
#are renaming but also not importing certain columns

area_cleaned <- area_ons %>%
  clean_names() %>%
  mutate(
    area_sqkm = areachect/100,
    coastal = case_when(
      str_detect(lad18cd, "^N") ~ NA_character_,
      areaehect == areachect    ~ "Inland",
      TRUE                      ~ "Coastal"
    )
  ) %>%
  select(lad18cd, area_sqkm, coastal)

weekly_pay_ons <- read_excel(
  here("data/Home Geography Table 8.1a   Weekly pay - Gross 2017.xls"), 
  sheet = "All",
  col_types = c(
    "text", "text", "numeric", "numeric", "skip", "numeric", "skip", "numeric",
    "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", 
    "numeric", "numeric", "skip", "skip", "skip"
  ),
  skip = 4
)
#Again using GUI: we will get warnings!  It's because we're trying to convert
#some of what it thinks is text to numeric

weekly_pay_cleaned <- weekly_pay_ons %>%
  clean_names() %>%
  rename(num_jobs_thou = thousand, median_pay_2017 = median) %>%
  filter(
    !is.na(num_jobs_thou),
    str_sub(code, start = 1, end = 3) %in%
      c("E06", "E07", "E08", "E09", "W06", "S12", "N09")
  ) %>%
  mutate(
    code = case_when(
      description == "Fife"              ~ "S12000047",
      description == "Perth and Kinross" ~ "S12000048",
      TRUE                               ~ code
    ) #Some codes changed in 2018
  ) %>%
  select(code, median_pay_2017)


life_exp_ons <- read_csv(
  here(
    "data/life-expectancy-by-local-authority-time-series-v1-filtered-2021-11-11T12-11-26Z.csv"
  )
)

life_exp_cleaned <- life_exp_ons %>%
  select(-c(Sex, AgeGroups)) %>%
  clean_names() %>%
  filter(two_year_intervals == "2016-18") %>%
  select(geography, administrative_geography, v4_2, sex) %>%
  arrange(sex) %>% #Force alphabetical order of new columns
  pivot_wider(
    names_from = sex,
    names_prefix = "life_exp_",
    values_from = v4_2
  ) %>%
  mutate(
    administrative_geography = case_when(
      geography == "Glasgow City"      ~ "S12000046",
      geography == "North Lanarkshire" ~ "S12000044",
      TRUE                             ~ administrative_geography
    ) #Reverting some codes from newer data to old ones
  ) %>%
  select(-geography)

#Joining everything

pubs_final <- pubs_2018 %>%
  mutate(
    country = case_when(
      str_detect(area_code, "^E") ~ "England",
      str_detect(area_code, "^S") ~ "Scotland",
      str_detect(area_code, "^W") ~ "Wales",
      str_detect(area_code, "^N") ~ "Northern Ireland",
      TRUE                        ~ NA_character_
    )
  ) %>%
  left_join(weekly_pay_cleaned, by = c("area_code" = "code")) %>%
  left_join(area_cleaned, by = c("area_code" = "lad18cd")) %>%
  mutate(pop_dens = pop/area_sqkm) %>%
  left_join(
    life_exp_cleaned,
    by = c("area_code" = "administrative_geography")
  )

saveRDS(pubs_final, here("data/pubs-final.rds"))
