####Analysis 1 = Ecological####

if (!require("pacman")) install.packages("pacman")

pacman::p_load(devtools,np,lazyeval, hmisc,interp, lmtest,gt, modelsummary, dplyr,pdftools, tidyverse,rattle,glmnet,caret, rpart.plot, RcolorBrewer,rpart, tidyr, mice, stringr,randomForest,  curl, plm, readxl, zoo, stringr, patchwork,  sf, clubSandwich, modelsummary, sjPlot)


outsourced <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/adults_social_care_data/refs/heads/main/activity_keep_all.csv"))%>%
  dplyr::mutate(DH_GEOGRAPHY_NAME = str_trim(gsub("[0-9]", "", DH_GEOGRAPHY_NAME)))%>%
  dplyr::filter(SupportSetting=="Residential",
                DH_GEOGRAPHY_NAME!="LONDON")%>%
  dplyr::select(DH_GEOGRAPHY_NAME, year, ITEMVALUE, ActivityProvision)%>%
  tidyr::pivot_wider(id_cols = c("year","DH_GEOGRAPHY_NAME" ), names_from = "ActivityProvision", values_from = "ITEMVALUE")%>%
  dplyr::rename(inhouse_activity = `In House`,
                outsourced_activity = External)%>%
  dplyr::mutate(total_activity = outsourced_activity+inhouse_activity,
                percent_inhouse_activity = inhouse_activity/total_activity*100)%>%
  dplyr::select(-`99`)%>%
  dplyr::mutate(DH_GEOGRAPHY_NAME = DH_GEOGRAPHY_NAME %>%
                  gsub( "BATH AND SOMERSET", "BATH AND NORTH EAST SOMERSET",.)%>%
                  gsub("ISLE OF WIGHT COUNCIL", "ISLE OF WIGHT", .)%>%
                  gsub("COUNTY DURHAM", "DURHAM", .)%>%
                  gsub("MEDWAY TOWNS", "MEDWAY", .)
  )%>%
  dplyr::filter(DH_GEOGRAPHY_NAME!="EAST",
                DH_GEOGRAPHY_NAME!="EAST MIDLANDS",
                DH_GEOGRAPHY_NAME!="NORTH EAST",
                DH_GEOGRAPHY_NAME!="NORTH WEST",
                DH_GEOGRAPHY_NAME!="SOUTH WEST",
                DH_GEOGRAPHY_NAME!="SOUTH EAST",
                DH_GEOGRAPHY_NAME!="YORKSHIRE AND HUMBER",
                DH_GEOGRAPHY_NAME!="WEST MIDLANDS"
                
  )

# Count number of years per geography
geo_year_counts <- outsourced %>%
  count(DH_GEOGRAPHY_NAME, name = "n_years") %>%
  arrange(n_years)


all_years <- sort(unique(outsourced$year))

la_missing_middle <- outsourced %>%
  complete(DH_GEOGRAPHY_NAME, year = all_years) %>%
  group_by(DH_GEOGRAPHY_NAME) %>%
  arrange(year) %>%
  mutate(missing = is.na(percent_inhouse_activity)) %>%
  # Store first and last non-missing years
  mutate(
    first_year = min(year[!missing], na.rm = TRUE),
    last_year = max(year[!missing], na.rm = TRUE),
    is_gap = missing & year > first_year & year < last_year
  ) %>%
  summarise(
    has_internal_gap = any(is_gap),
    missing_years = list(year[is_gap]),
    n_years = sum(!missing),
    .groups = "drop"
  ) %>%
  filter(has_internal_gap)

print(la_missing_middle)



# working_age <- read.csv("~/Library/CloudStorage/OneDrive-Nexus365/Documents/GitHub/GitHub_new/adults_social_care_data/Final_data/expenditure.csv")%>%
#   dplyr::mutate(DH_GEOGRAPHY_NAME = str_trim(gsub("[0-9]", "", DH_GEOGRAPHY_NAME)))%>%
#   dplyr::filter(Sector == "External"|Sector=="Total",
#                 SupportSetting == "U65 PHYSICAL DISABILITY"|SupportSetting == "U65 LEARNING DISABILITY"|SupportSetting == "U65 MENTAL HEALTH")%>%
#   dplyr::select(DH_GEOGRAPHY_NAME, year,percent_sector, Sector, SupportSetting, Expenditure )%>%
#   dplyr::filter(DH_GEOGRAPHY_NAME!="ALL DATA RELATING TO NUMBERS OF PEOPLE ARE ROUNDED TO NEAREST",
#                 DH_GEOGRAPHY_NAME!="ALL REMAINING UNIT COSTS WHICH ARE REPORTED BY COUNCILS AS A SAMPLE WEEK OR WEEKLY VALUE HAVE BEEN EXPRESSED IN DENOMINATORS AS AN ANNL FIGURE TO AID USER INTERPRETATION",
#                 DH_GEOGRAPHY_NAME!="COPYRIGHT  HEALTH AND SOCIAL CARE INFORMATION CENTRE ALL RIGHTS RESERVED")%>%
#   tidyr::pivot_wider(id_cols = c("DH_GEOGRAPHY_NAME", "year"), names_from = c("SupportSetting", "Sector"), values_from = c("Expenditure", "percent_sector") )
# 


outsourced_spend <- read.csv("~/Library/CloudStorage/OneDrive-Nexus365/Documents/GitHub/GitHub_new/adults_social_care_data/Final_data/expenditure.csv")%>%
  dplyr::mutate(DH_GEOGRAPHY_NAME = str_trim(gsub("[0-9]", "", DH_GEOGRAPHY_NAME)))%>%
  dplyr::filter(Sector == "External",
                SupportSetting == "Home care"|SupportSetting == "home care")%>%
  dplyr::select(DH_GEOGRAPHY_NAME, year,percent_sector )%>%
  dplyr::rename(outsourced_home_care = percent_sector)%>%
  dplyr::full_join(.,
                   read.csv("~/Library/CloudStorage/OneDrive-Nexus365/Documents/GitHub/GitHub_new/adults_social_care_data/Final_data/expenditure.csv")%>%
                     dplyr::mutate(DH_GEOGRAPHY_NAME = str_trim(gsub("[0-9]", "", DH_GEOGRAPHY_NAME)))%>%
                     dplyr::filter(Sector == "External",
                                   SupportSetting == "Residential care home placements")%>%
                     dplyr::select(DH_GEOGRAPHY_NAME, year,percent_sector )%>%
                     dplyr::rename(outsourced_res_care = percent_sector))%>%
  dplyr::full_join(.,
                   read.csv("~/Library/CloudStorage/OneDrive-Nexus365/Documents/GitHub/GitHub_new/adults_social_care_data/Final_data/expenditure.csv")%>%
                     dplyr::mutate(DH_GEOGRAPHY_NAME = str_trim(gsub("[0-9]", "", DH_GEOGRAPHY_NAME)))%>%
                     dplyr::filter(Sector == "External",
                                   SupportSetting == "Total over 65")%>%
                     dplyr::select(DH_GEOGRAPHY_NAME, year,percent_sector )%>%
                     dplyr::rename(outsourced_old_care = percent_sector))%>%
  dplyr::filter(DH_GEOGRAPHY_NAME!="ALL DATA RELATING TO NUMBERS OF PEOPLE ARE ROUNDED TO NEAREST",
                DH_GEOGRAPHY_NAME!="ALL REMAINING UNIT COSTS WHICH ARE REPORTED BY COUNCILS AS A SAMPLE WEEK OR WEEKLY VALUE HAVE BEEN EXPRESSED IN DENOMINATORS AS AN ANNL FIGURE TO AID USER INTERPRETATION",
                DH_GEOGRAPHY_NAME!="COPYRIGHT  HEALTH AND SOCIAL CARE INFORMATION CENTRE ALL RIGHTS RESERVED")

all_spend <- read.csv("~/Library/CloudStorage/OneDrive-Nexus365/Documents/GitHub/GitHub_new/adults_social_care_data/Final_data/expenditure.csv")%>%
  dplyr::mutate(DH_GEOGRAPHY_NAME = str_trim(gsub("[0-9]", "", DH_GEOGRAPHY_NAME)))%>%
  dplyr::filter(Sector == "Total",
                SupportSetting == "Home care"|SupportSetting == "home care")%>%
  dplyr::select(DH_GEOGRAPHY_NAME, year,Expenditure )%>%
  dplyr::rename(spend_home_care = Expenditure)%>%
  dplyr::full_join(.,
                   read.csv("~/Library/CloudStorage/OneDrive-Nexus365/Documents/GitHub/GitHub_new/adults_social_care_data/Final_data/expenditure.csv")%>%
                     dplyr::mutate(DH_GEOGRAPHY_NAME = str_trim(gsub("[0-9]", "", DH_GEOGRAPHY_NAME)))%>%
                     dplyr::filter(Sector == "Total",
                                   SupportSetting == "Residential care home placements")%>%
                     dplyr::select(DH_GEOGRAPHY_NAME, year,Expenditure )%>%
                     dplyr::rename(spend_res_care = Expenditure))%>%
  dplyr::full_join(.,
                   read.csv("~/Library/CloudStorage/OneDrive-Nexus365/Documents/GitHub/GitHub_new/adults_social_care_data/Final_data/expenditure.csv")%>%
                     dplyr::mutate(DH_GEOGRAPHY_NAME = str_trim(gsub("[0-9]", "", DH_GEOGRAPHY_NAME)))%>%
                     dplyr::filter(Sector == "Total",
                                   SupportSetting == "Total over 65")%>%
                     dplyr::select(DH_GEOGRAPHY_NAME, year,Expenditure )%>%
                     dplyr::rename(spend_old_care = Expenditure))%>%
  dplyr::filter(DH_GEOGRAPHY_NAME!="ALL DATA RELATING TO NUMBERS OF PEOPLE ARE ROUNDED TO NEAREST",
                DH_GEOGRAPHY_NAME!="ALL REMAINING UNIT COSTS WHICH ARE REPORTED BY COUNCILS AS A SAMPLE WEEK OR WEEKLY VALUE HAVE BEEN EXPRESSED IN DENOMINATORS AS AN ANNL FIGURE TO AID USER INTERPRETATION",
                DH_GEOGRAPHY_NAME!="COPYRIGHT  HEALTH AND SOCIAL CARE INFORMATION CENTRE ALL RIGHTS RESERVED")





lifeexpectancy <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/care_home_mortality/refs/heads/main/Data/lifeexpectancylocalareas_23.csv"), skip=5)%>%
  dplyr::filter(Area.type == "County"|
                  Area.type == "Local Areas",
                Age.band == 16|Age.band == 17|Age.band == 18|Age.band == 19|Age.band == 20)%>%
  dplyr::mutate(year = str_extract_all(Period, "\\d{4}") %>%
                  lapply(as.numeric) %>%
                  sapply(function(x) floor(mean(x) )),
                DH_GEOGRAPHY_NAME  = Area.name %>%
                  gsub('&', 'and', .) %>%
                  gsub('[[:punct:] ]+', ' ', .) %>%
                  gsub('[0-9]', '', .)%>%
                  toupper() %>%
                  gsub("CITY OF", "",.)%>%
                  gsub("UA", "",.)%>%
                  gsub("COUNTY OF", "",.)%>%
                  gsub("ROYAL BOROUGH OF", "",.)%>%
                  gsub("LEICESTER CITY", "LEICESTER",.)%>%
                  gsub("UA", "",.)%>%
                  gsub("DARWIN", "DARWEN", .)%>%
                  gsub("COUNTY DURHAM", "DURHAM", .)%>%
                  gsub("AND DARWEN", "WITH DARWEN", .)%>%
                  gsub("NE SOM", "NORTH EAST SOM", .)%>%
                  gsub("N E SOM", "NORTH EAST SOM", .)%>%
                  str_trim())%>%
  dplyr::select(DH_GEOGRAPHY_NAME, year,Life.expectancy, Sex, Age.group)%>%
  dplyr::mutate(Age.group = gsub(" ", "_", Age.group))%>%
  tidyr::pivot_wider(names_from = c("Age.group", "Sex"), values_from = "Life.expectancy", names_prefix = "life_expectancy")

deaths <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/care_home_mortality/refs/heads/main/Data/lanonladeaths20141021.csv"), skip=4)%>%
  dplyr::rename(year = Year,
                DH_GEOGRAPHY_NAME = Local.Authority.Name,
                deaths.LA.care.homes = LA.care.homes,
                deaths.non.LA.care.homes = non.LA.care.homes,
                deaths.All.care.homes = All.care.homes)%>%
  dplyr::mutate(DH_GEOGRAPHY_NAME  = DH_GEOGRAPHY_NAME %>%
                  gsub('&', 'and', .) %>%
                  gsub('[[:punct:] ]+', ' ', .) %>%
                  gsub('[0-9]', '', .)%>%
                  toupper() %>%
                  gsub("CITY OF", "",.)%>%
                  gsub("UA", "",.)%>%
                  gsub("COUNTY OF", "",.)%>%
                  gsub("ROYAL BOROUGH OF", "",.)%>%
                  gsub("LEICESTER CITY", "LEICESTER",.)%>%
                  gsub("UA", "",.)%>%
                  gsub("DARWIN", "DARWEN", .)%>%
                  gsub("COUNTY DURHAM", "DURHAM", .)%>%
                  gsub("AND DARWEN", "WITH DARWEN", .)%>%
                  gsub("NE SOM", "NORTH EAST SOM", .)%>%
                  gsub("N E SOM", "NORTH EAST SOM", .)%>%
                  str_trim())


### removed care home deaths for now - needs aggregating to upper tier la###



library(fingertipsR)

# 
# Emergency hospital admissions due to falls in people aged 80 plus
# Hip fractures in people aged 80 and over
# 
# Winter mortality index (age 85 plus)

# # Confirm that UTLA is AreaTypeID 102
# area_types() %>%
#   filter(AreaTypeName == "Upper tier local authority")
# 
# # 1. Pull all available indicators
# all_inds <- indicators()
# 
# # 2. Filter to the exact indicator names you want
# wanted <- all_inds %>%
#   filter(IndicatorName %in% c(
#     "Emergency hospital admissions due to falls in people aged 80 plus",
#     "Hip fractures in people aged 80 and over",
#     "Winter mortality index (age 85 plus)",
#     "Delayed transfers of care per 100,000 population",                # DTOC
#     "Mortality rate from causes considered preventable (all ages)"     # preventable mortality
#   )) %>%
#   select(IndicatorID, IndicatorName)
# 
# print(wanted)

# Build your vector of the three corrected IDs
indicator_ids <- c(22403, 41403, 90361)

# Fetch data
phdata <- fingertips_data(
  IndicatorID = indicator_ids,
  AreaTypeID  = 502   # UTLA
)



phdata_clean <- phdata %>%
  dplyr::filter(Sex=="Persons")%>%
  dplyr::select(IndicatorName, AreaName, Value, TimeperiodSortable)%>%
  dplyr::mutate(year = as.numeric(str_extract(TimeperiodSortable, "\\d{4}"))+1)%>%
  tidyr::pivot_wider(id_cols = c("AreaName","year" ), values_from = "Value", names_from = "IndicatorName")%>%
  dplyr::mutate(DH_GEOGRAPHY_NAME  = AreaName %>%
                  gsub('&', 'and', .) %>%
                  gsub('[[:punct:] ]+', ' ', .) %>%
                  gsub('[0-9]', '', .)%>%
                  toupper() %>%
                  gsub("CITY OF", "",.)%>%
                  gsub("UA", "",.)%>%
                  gsub("COUNTY OF", "",.)%>%
                  gsub("ROYAL BOROUGH OF", "",.)%>%
                  gsub("LEICESTER CITY", "LEICESTER",.)%>%
                  gsub("UA", "",.)%>%
                  gsub("DARWIN", "DARWEN", .)%>%
                  gsub("COUNTY DURHAM", "DURHAM", .)%>%
                  gsub("AND DARWEN", "WITH DARWEN", .)%>%
                  gsub("NE SOM", "NORTH EAST SOM", .)%>%
                  gsub("N E SOM", "NORTH EAST SOM", .)%>%
                  str_trim())%>%
  dplyr::select(-AreaName)

                  
  








#   
# # Define full range of years and all LAs
# all_years <- 2011:2024
# all_LAs <- unique(carehomedata$DH_GEOGRAPHY_NAME)
# 
# # Create a complete grid of all year–LA combinations
# complete_grid <- expand_grid(
#   year_location_end_2025 = all_years,
#   DH_GEOGRAPHY_NAME = all_LAs
# )
# 
# # Join with the original data and fill NAs with 0
# completed_data <- complete_grid %>%
#   left_join(carehomedata, by = c("year_location_end_2025", "DH_GEOGRAPHY_NAME")) %>%
#   mutate(closed_complete = replace_na(closed_complete, 0)) %>%
#   arrange(DH_GEOGRAPHY_NAME, year_location_end_2025)%>%
#   dplyr::rename(year = year_location_end_2025)%>%
#   dplyr::group_by(DH_GEOGRAPHY_NAME)%>%
#   arrange(year) %>%
#   dplyr::mutate(lagged_closed = dplyr::lag(closed_complete, 1))
# 
# 
# 
# data <- data %>%
#   dplyr::full_join(., completed_data)
# 
# 

# 




#nomisd <- read.csv(curl("https://www.nomisweb.co.uk/api/v01/dataset/NM_161_1.data.csv?geography=1774190593...1774190597,1774190637,1774190646,1774190675...1774190678,1774190691,1774190598...1774190601,1774190638,1774190639,1774190652,1774190653,1774190656...1774190670,1774190734,1774190602...1774190606,1774190654,1774190671...1774190674,1774190686...1774190690,1774190607...1774190610,1774190650,1774190651,1774190726,1774190735,1774190736,1774190738,1774190611...1774190613,1774190640,1774190679...1774190685,1774190740,1774190743,1774190745,1774190621...1774190624,1774190644,1774190645,1774190725,1774190729,1774190732,1774190737,1774190741,1774190692...1774190724,1774190625...1774190636,1774190649,1774190728,1774190731,1774190733,1774190739,1774190742,1774190744,1774190614...1774190620,1774190641...1774190643,1774190647,1774190648,1774190655,1774190727,1774190730,1774190746...1774190767&cause_of_death=114390,2420&gender=0&age=17...20&measure=7&measures=20100"))


 
####controls####
 
#Population total and aged over x
 
library(dplyr)
library(tidyr)
library(readr)
library(janitor)

# 1) Read and clean the 2001–2011 file
pop1 <- read_csv(
  curl::curl("https://raw.githubusercontent.com/BenGoodair/care_home_mortality/refs/heads/main/Data/myebtablesewsn20012011.csv"),
  skip = 1,        # drop the first metadata row
  col_types = cols(.default = "c")
) %>%
  clean_names() %>%
  rename(
    lad_code  = ladcode18,
    geography = laname18
  ) %>%
  # pick off only age & the "population_YYYY" cols
  pivot_longer(
    cols = starts_with("population_"),
    names_to  = "year",
    names_prefix = "population_",
    values_to = "pop"
  ) %>%
  mutate(
    year = as.integer(year),
    age  = as.integer(age),
    pop  = as.integer(pop)
  )

# 2) Read and clean the 2011–2023 file
pop2 <- read_csv(
  curl::curl("https://raw.githubusercontent.com/BenGoodair/care_home_mortality/refs/heads/main/Data/myebtablesenglandwales20112023%20(3).csv"),
  skip = 1,
  col_types = cols(.default = "c")
) %>%
  clean_names() %>%
  rename(
    lad_code  = ladcode23,
    geography = laname23
  ) %>%
  pivot_longer(
    cols = starts_with("population_"),
    names_to  = "year",
    names_prefix = "population_",
    values_to = "pop"
  ) %>%
  mutate(
    year = as.integer(year),
    age  = as.integer(age),
    pop  = as.integer(pop)
  )

# 3) Bind, aggregate, and compute totals + 80+ counts
pop <- bind_rows(pop1, pop2) %>%
  group_by(geography, year) %>%
  summarise(
    total_population   = sum(pop, na.rm = TRUE),
    population_over_80 = sum(pop[age >= 80], na.rm = TRUE),
    .groups = "drop"
  )%>%
  mutate(percent_80 = population_over_80/total_population*100,
         DH_GEOGRAPHY_NAME  = geography %>%
           gsub('&', 'and', .) %>%
           gsub('[[:punct:] ]+', ' ', .) %>%
           gsub('[0-9]', '', .)%>%
           toupper() %>%
           gsub("CITY OF", "",.)%>%
           gsub("UA", "",.)%>%
           gsub("COUNTY OF", "",.)%>%
           gsub("ROYAL BOROUGH OF", "",.)%>%
           gsub("LEICESTER CITY", "LEICESTER",.)%>%
           gsub("UA", "",.)%>%
           gsub("DARWIN", "DARWEN", .)%>%
           gsub("COUNTY DURHAM", "DURHAM", .)%>%
           gsub("AND DARWEN", "WITH DARWEN", .)%>%
           gsub("NE SOM", "NORTH EAST SOM", .)%>%
           gsub("N E SOM", "NORTH EAST SOM", .)%>%
           str_trim())%>%
  dplyr::select(-geography)


 
#Pension credit
 
 library(dplyr)
 library(tidyr)
 library(curl)
 
 pen <- rbind(
   read.csv(curl("https://raw.githubusercontent.com/BenGoodair/care_home_mortality/refs/heads/main/Data/pension_post_18.csv"), skip = 6),
   read.csv(curl("https://raw.githubusercontent.com/BenGoodair/care_home_mortality/refs/heads/main/Data/pension_pre_18%203.csv"), skip = 6) %>%
     select(-X)
 ) %>%
   mutate(
     Counting = gsub(" - 2011 Geographies", "", Counting),
     Counting = ifelse(Counting == "", NA, Counting),
     year = paste0("20", substr(Quarter, nchar(Quarter) - 1, nchar(Quarter)))  # Extract 2-digit year and make 4-digit
   ) %>%
   filter(!is.na(Counting)) %>%
   pivot_wider(names_from = "Counting", values_from = "Count") %>%
   group_by(year, National...Regional...LA...OAs) %>%
   summarise(across(where(is.numeric), sum, na.rm = TRUE), .groups = "drop")%>%
   dplyr::ungroup()%>%
   dplyr::mutate(DH_GEOGRAPHY_NAME  = National...Regional...LA...OAs %>%
                   gsub('&', 'and', .) %>%
                   gsub('[[:punct:] ]+', ' ', .) %>%
                   gsub('[0-9]', '', .)%>%
                   toupper() %>%
                   gsub("CITY OF", "",.)%>%
                   gsub("UA", "",.)%>%
                   gsub("COUNTY OF", "",.)%>%
                   gsub("ROYAL BOROUGH OF", "",.)%>%
                   gsub("LEICESTER CITY", "LEICESTER",.)%>%
                   gsub("UA", "",.)%>%
                   gsub("DARWIN", "DARWEN", .)%>%
                   gsub("COUNTY DURHAM", "DURHAM", .)%>%
                   gsub("AND DARWEN", "WITH DARWEN", .)%>%
                   gsub("NE SOM", "NORTH EAST SOM", .)%>%
                   gsub("N E SOM", "NORTH EAST SOM", .)%>%
                   str_trim(),
                 year = as.numeric(year),
                 total_pension_credits = `Pension Credit Caseload`*`Mean of Weekly Award Amount`)%>%
   dplyr::select(-National...Regional...LA...OAs)
 
 

#Unemployment and disability
 
unemp <- read.csv(curl("https://www.nomisweb.co.uk/api/v01/dataset/NM_17_5.data.csv?geography=1774190593...1774190597,1774190637,1774190646,1774190675...1774190678,1774190691,1774190598...1774190601,1774190638,1774190639,1774190652,1774190653,1774190656...1774190670,1774190734,1774190602...1774190606,1774190654,1774190671...1774190674,1774190686...1774190690,1774190607...1774190610,1774190650,1774190651,1774190726,1774190735,1774190736,1774190738,1774190611...1774190613,1774190640,1774190679...1774190685,1774190740,1774190743,1774190745,1774190621...1774190624,1774190644,1774190645,1774190725,1774190729,1774190732,1774190737,1774190741,1774190692...1774190724,1774190625...1774190636,1774190649,1774190728,1774190731,1774190733,1774190739,1774190742,1774190744,1774190614...1774190620,1774190641...1774190643,1774190647,1774190648,1774190655,1774190727,1774190730,1774190746...1774190799&date=latestMINUS80,latestMINUS76,latestMINUS72,latestMINUS68,latestMINUS64,latestMINUS60,latestMINUS56,latestMINUS52,latestMINUS48,latestMINUS44,latestMINUS40,latestMINUS36,latestMINUS32,latestMINUS28,latestMINUS24,latestMINUS20,latestMINUS16,latestMINUS12,latestMINUS8,latestMINUS4,latest&variable=84&measures=20599,21001,21002,21003"))%>%
  dplyr::filter(MEASURES_NAME=="Variable")%>%
  dplyr::select(GEOGRAPHY_NAME, DATE, OBS_VALUE)%>%
  dplyr::mutate(DH_GEOGRAPHY_NAME  = GEOGRAPHY_NAME %>%
                  gsub('&', 'and', .) %>%
                  gsub('[[:punct:] ]+', ' ', .) %>%
                  gsub('[0-9]', '', .)%>%
                  toupper() %>%
                  gsub("CITY OF", "",.)%>%
                  gsub("UA", "",.)%>%
                  gsub("COUNTY OF", "",.)%>%
                  gsub("ROYAL BOROUGH OF", "",.)%>%
                  gsub("LEICESTER CITY", "LEICESTER",.)%>%
                  gsub("UA", "",.)%>%
                  gsub("DARWIN", "DARWEN", .)%>%
                  gsub("COUNTY DURHAM", "DURHAM", .)%>%
                  gsub("AND DARWEN", "WITH DARWEN", .)%>%
                  gsub("NE SOM", "NORTH EAST SOM", .)%>%
                  gsub("N E SOM", "NORTH EAST SOM", .)%>%
                  str_trim(),
                year = as.numeric(substr(DATE, 1, 4)))%>%
  dplyr::select(-GEOGRAPHY_NAME, -DATE)%>%
  dplyr::rename(unemployed = OBS_VALUE)


disab <- read.csv(curl("https://www.nomisweb.co.uk/api/v01/dataset/NM_17_5.data.csv?geography=1774190593...1774190597,1774190637,1774190646,1774190675...1774190678,1774190691,1774190598...1774190601,1774190638,1774190639,1774190652,1774190653,1774190656...1774190670,1774190734,1774190602...1774190606,1774190654,1774190671...1774190674,1774190686...1774190690,1774190607...1774190610,1774190650,1774190651,1774190726,1774190735,1774190736,1774190738,1774190611...1774190613,1774190640,1774190679...1774190685,1774190740,1774190743,1774190745,1774190621...1774190624,1774190644,1774190645,1774190725,1774190729,1774190732,1774190737,1774190741,1774190692...1774190724,1774190625...1774190636,1774190649,1774190728,1774190731,1774190733,1774190739,1774190742,1774190744,1774190614...1774190620,1774190641...1774190643,1774190647,1774190648,1774190655,1774190727,1774190730,1774190746...1774190799&date=latestMINUS80,latestMINUS76,latestMINUS72,latestMINUS68,latestMINUS64,latestMINUS60,latestMINUS56,latestMINUS52,latestMINUS48,latestMINUS44,latestMINUS40,latestMINUS36,latestMINUS32,latestMINUS28,latestMINUS24,latestMINUS20,latestMINUS16,latestMINUS12,latestMINUS8,latestMINUS4,latest&variable=146&measures=20599,21001,21002,21003"))
  
 
#Central LA grants (maybe reserves better...)

grant <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/care_home_mortality/refs/heads/main/Data/FIN_07_50U.csv"))%>%
  dplyr::rename(year=Year)%>%
  dplyr::select(year, UTLA18NM, Central_Gov_Funding_Total, Central_Gov_Funding_Total_PerCap)%>%
  dplyr::mutate(DH_GEOGRAPHY_NAME  = UTLA18NM %>%
                  gsub('&', 'and', .) %>%
                  gsub('[[:punct:] ]+', ' ', .) %>%
                  gsub('[0-9]', '', .)%>%
                  toupper() %>%
                  gsub("CITY OF", "",.)%>%
                  gsub("UA", "",.)%>%
                  gsub("COUNTY OF", "",.)%>%
                  gsub("ROYAL BOROUGH OF", "",.)%>%
                  gsub("LEICESTER CITY", "LEICESTER",.)%>%
                  gsub("UA", "",.)%>%
                  gsub("DARWIN", "DARWEN", .)%>%
                  gsub("COUNTY DURHAM", "DURHAM", .)%>%
                  gsub("AND DARWEN", "WITH DARWEN", .)%>%
                  gsub("NE SOM", "NORTH EAST SOM", .)%>%
                  gsub("N E SOM", "NORTH EAST SOM", .)%>%
                  str_trim(),
                year = as.numeric(year))%>%
  dplyr::select(-UTLA18NM)

#  


data = dplyr::full_join(outsourced , lifeexpectancy, by=c("DH_GEOGRAPHY_NAME", "year"))%>%
  tidyr::drop_na(life_expectancy70_to_74_Female, percent_inhouse_activity)%>%
  # dplyr::full_join(., deaths)%>%
  dplyr::group_by(DH_GEOGRAPHY_NAME)%>%
  arrange(year) %>%
  dplyr::mutate(lagged_sector_1 = dplyr::lag(percent_inhouse_activity, 1),
                lagged_sector_2 = dplyr::lag(percent_inhouse_activity, 2),
                lagged_le_1_female_70 = dplyr::lag(life_expectancy70_to_74_Female, 1),
                lagged_le_1_female_75 = dplyr::lag(life_expectancy75_to_79_Female, 1),
                lagged_le_1_female_80 = dplyr::lag(life_expectancy80_to_84_Female, 1),
                lagged_le_1_female_85 = dplyr::lag(life_expectancy85_to_89_Female, 1),
                lagged_le_1_female_90 = dplyr::lag(`life_expectancy90+_Female`, 1),
                lagged_le_1_male_70 = dplyr::lag(life_expectancy70_to_74_Male, 1),
                lagged_le_1_male_75 = dplyr::lag(life_expectancy75_to_79_Male, 1),
                lagged_le_1_male_80 = dplyr::lag(life_expectancy80_to_84_Male, 1),
                lagged_le_1_male_85 = dplyr::lag(life_expectancy85_to_89_Male, 1),
                lagged_le_1_male_90 = dplyr::lag(`life_expectancy90+_Male`, 1))



duplicated_rows <- data %>% 
  dplyr::group_by(DH_GEOGRAPHY_NAME, year) %>%
  dplyr::filter(n() > 1)


data = dplyr::full_join(data , phdata_clean, by=c("DH_GEOGRAPHY_NAME", "year"))

data <- data %>% 
  dplyr::full_join(., outsourced_spend)

data <- data %>% 
  dplyr::full_join(., all_spend)

data <- data %>% 
  dplyr::full_join(., pen)

data <- data %>% 
  dplyr::full_join(., pop)

data <- data %>% 
  dplyr::full_join(., grant)

data <- data %>% 
  dplyr::full_join(., unemp)


data = data %>%
  dplyr::group_by(DH_GEOGRAPHY_NAME)%>%
  arrange(year) %>%
  dplyr::mutate(lagged_home_out = dplyr::lag(outsourced_home_care, 1),
                lagged_home_spend = dplyr::lag(spend_home_care, 1),
                lagged_res_out = dplyr::lag(outsourced_res_care, 1),
                lagged_res_spend = dplyr::lag(spend_res_care, 1),
                lagged_old_out = dplyr::lag(outsourced_old_care, 1),
                lagged_old_spend = dplyr::lag(spend_old_care, 1),
                lagged_activity_insourced =dplyr::lag(percent_inhouse_activity, 1),
                lagged_activity_insourced_2 =dplyr::lag(percent_inhouse_activity, 1),
                lagged_activity_total =dplyr::lag(total_activity, 1),
                lagged_winter =dplyr::lag(`Winter mortality index (age 85 plus)`, 1),
                lagged_hips =dplyr::lag(`Hip fractures in people aged 80 and over`, 1),
                lagged_falls =dplyr::lag(`Emergency hospital admissions due to falls in people aged 80 plus`, 1),
                lagged_pension_credits =dplyr::lag(total_pension_credits, 1),
                lagged_total_pop =dplyr::lag(total_population, 1),
                lagged_per_80 =dplyr::lag(percent_80, 1),
                lagged_gov_grant =dplyr::lag(Central_Gov_Funding_Total, 1),
                lagged_unemp =dplyr::lag(unemployed, 1),
  )



####Analysis####

library(plm)
library(dplyr)


pdata <- pdata.frame(data%>%dplyr::filter(year>2002, year<2011), index = c("DH_GEOGRAPHY_NAME","year"))
head(pdata)

#summary(plm(log(Emergency.hospital.admissions.due.to.falls.in.people.aged.80.plus)~lagged_activity_insourced+lagged_activity_total+lagged_old_spend+lagged_pension_credits+lagged_total_pop+lagged_per_80, data=pdata, method = "within", effect = "twoways"))
#summary(plm(log(Hip.fractures.in.people.aged.80.and.over)~lagged_activity_insourced+lagged_activity_total+lagged_old_spend+lagged_pension_credits+lagged_total_pop+lagged_per_80, data=pdata, method = "within", effect = "twoways"))
summary(plm(log(life_expectancy_Female)~lagged_activity_insourced+lagged_activity_total+lagged_old_spend+lagged_pension_credits+lagged_total_pop+lagged_per_80+lagged_unemp, data=pdata, method = "within", effect = "twoways"))
summary(plm(log(life_expectancy_Male)~lagged_activity_insourced+lagged_activity_total+lagged_old_spend+lagged_pension_credits+lagged_total_pop+lagged_per_80+lagged_unemp, data=pdata, method = "within", effect = "twoways"))

# #summary(plm(log(Emergency.hospital.admissions.due.to.falls.in.people.aged.80.plus)~lagged_activity_insourced+lagged_activity_total+lagged_old_spend+lagged_pension_credits+lagged_total_pop+lagged_per_80, data=pdata, method = "fd"))
# #summary(plm(log(Hip.fractures.in.people.aged.80.and.over)~lagged_activity_insourced+lagged_activity_total+lagged_old_spend+lagged_pension_credits+lagged_total_pop+lagged_per_80, data=pdata, method = "fd"))
# summary(plm(log(life_expectancy_Female)~percent_inhouse_activity+lagged_activity_insourced+lagged_activity_total+lagged_old_spend+lagged_pension_credits+lagged_total_pop+lagged_per_80+lagged_gov_grant, data=pdata, method = "fd"))
# summary(plm(log(life_expectancy_Male)~percent_inhouse_activity+lagged_activity_insourced+lagged_activity_total+lagged_old_spend+lagged_pension_credits+lagged_total_pop+lagged_per_80+lagged_gov_grant, data=pdata, method = "fd"))


pdata <- pdata.frame(data%>%dplyr::filter(year>2010, year<2020), index = c("DH_GEOGRAPHY_NAME","year"))

summary(plm(log(Emergency.hospital.admissions.due.to.falls.in.people.aged.80.plus)~lagged_activity_insourced+lagged_activity_total+lagged_old_spend+lagged_pension_credits+lagged_total_pop+lagged_per_80+lagged_unemp, data=pdata, method = "within", effect = "twoways"))
summary(plm(log(Hip.fractures.in.people.aged.80.and.over)~lagged_activity_insourced+lagged_activity_total+lagged_old_spend+lagged_pension_credits+lagged_total_pop+lagged_per_80+lagged_unemp, data=pdata, method = "within", effect = "twoways"))
summary(plm(log(life_expectancy_Female)~lagged_activity_insourced+lagged_activity_total+lagged_old_spend+lagged_pension_credits+lagged_total_pop+lagged_per_80+lagged_unemp, data=pdata, method = "within", effect = "twoways"))
summary(plm(log(life_expectancy_Male)~lagged_activity_insourced+lagged_activity_total+lagged_old_spend+lagged_pension_credits+lagged_total_pop+lagged_per_80+lagged_unemp, data=pdata, method = "within", effect = "twoways"))

# summary(plm(log(Emergency.hospital.admissions.due.to.falls.in.people.aged.80.plus)~lagged_activity_insourced+lagged_activity_total+lagged_old_spend+lagged_pension_credits+lagged_total_pop+lagged_per_80+lagged_gov_grant, data=pdata, method = "fd"))
# summary(plm(log(Hip.fractures.in.people.aged.80.and.over)~lagged_activity_insourced+lagged_activity_total+lagged_old_spend+lagged_pension_credits+lagged_total_pop+lagged_per_80+lagged_gov_grant, data=pdata, method = "fd"))
# summary(plm(log(life_expectancy_Female)~percent_inhouse_activity+lagged_activity_insourced+lagged_activity_total+lagged_old_spend+lagged_pension_credits+lagged_total_pop+lagged_per_80+lagged_gov_grant, data=pdata, method = "fd"))
# summary(plm(log(life_expectancy_Male)~percent_inhouse_activity+lagged_activity_insourced+lagged_activity_total+lagged_old_spend+lagged_pension_credits+lagged_total_pop+lagged_per_80+lagged_gov_grant, data=pdata, method = "fd"))



pdata <- pdata.frame(data%>%dplyr::filter( year<2020), index = c("DH_GEOGRAPHY_NAME","year"))

summary(plm(log(Emergency.hospital.admissions.due.to.falls.in.people.aged.80.plus)~lagged_activity_insourced+lagged_activity_total+lagged_old_spend+lagged_pension_credits+lagged_total_pop+lagged_per_80+lagged_unemp, data=pdata, method = "within", effect = "twoways"))
summary(plm(log(Hip.fractures.in.people.aged.80.and.over)~lagged_activity_insourced+lagged_activity_total+lagged_old_spend+lagged_pension_credits+lagged_total_pop+lagged_per_80+lagged_unemp, data=pdata, method = "within", effect = "twoways"))
summary(plm(log(life_expectancy_Female)~lagged_activity_insourced+lagged_activity_total+lagged_old_spend+lagged_pension_credits+lagged_total_pop+lagged_per_80+lagged_unemp, data=pdata, method = "within", effect = "twoways"))
summary(plm(log(life_expectancy_Male)~lagged_activity_insourced+lagged_activity_total+lagged_old_spend+lagged_pension_credits+lagged_total_pop+lagged_per_80+lagged_unemp, data=pdata, method = "within", effect = "twoways"))

# summary(plm(log(Emergency.hospital.admissions.due.to.falls.in.people.aged.80.plus)~lagged_activity_insourced+lagged_activity_total+lagged_old_spend+lagged_pension_credits+lagged_total_pop+lagged_per_80+lagged_gov_grant, data=pdata, method = "fd"))
# summary(plm(log(Hip.fractures.in.people.aged.80.and.over)~lagged_activity_insourced+lagged_activity_total+lagged_old_spend+lagged_pension_credits+lagged_total_pop+lagged_per_80+lagged_gov_grant, data=pdata, method = "fd"))
# summary(plm(log(life_expectancy_Female)~percent_inhouse_activity+lagged_activity_insourced+lagged_activity_total+lagged_old_spend+lagged_pension_credits+lagged_total_pop+lagged_per_80+lagged_gov_grant, data=pdata%>%dplyr::filter(lagged_activity_insourced!=0), method = "fd"))
# summary(plm(log(life_expectancy_Male)~percent_inhouse_activity+lagged_activity_insourced+lagged_activity_total+lagged_old_spend+lagged_pension_credits+lagged_total_pop+lagged_per_80+lagged_gov_grant, data=pdata, method = "fd"))




# Load required libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(ggridges)
library(viridis)

# Assuming your data is called 'outsourced'
# Remove NAs for cleaner visualizations
clean_data <- outsourced %>% 
  filter(!is.na(percent_inhouse_activity))

# Calculate summary statistics for enhanced visualizations
summary_stats <- clean_data %>%
  group_by(year) %>%
  summarise(
    mean_pct = mean(percent_inhouse_activity, na.rm = TRUE),
    median_pct = median(percent_inhouse_activity, na.rm = TRUE),
    q25 = quantile(percent_inhouse_activity, 0.25, na.rm = TRUE),
    q75 = quantile(percent_inhouse_activity, 0.75, na.rm = TRUE),
    .groups = 'drop'
  )

# ==== VISUALIZATION 1: RIDGELINE PLOT ====
# Shows distribution evolution over time - excellent for seeing spread changes
viz1_ridgeline <- ggplot(clean_data, aes(x = percent_inhouse_activity, y = factor(year))) +
  geom_density_ridges(aes(fill = factor(year)), 
                      alpha = 0.7, 
                      scale = 2.5,
                      rel_min_height = 0.01) +
  geom_point(data = summary_stats, 
             aes(x = mean_pct, y = factor(year)), 
             color = "red", size = 3, shape = 18) +
  scale_fill_viridis_d(name = "Year") +
  labs(title = "Distribution Evolution: Percent In-house Activity by Year",
       subtitle = "Red diamonds show yearly averages",
       x = "Percent In-house Activity",
       y = "Year") +
  theme_minimal() +
  theme(legend.position = "none")

# ==== VISUALIZATION 2: HEATMAP WITH TRAJECTORY CLUSTERING ====
# Groups similar LAs and shows patterns as color intensity
# First, create trajectory clusters
la_trajectories <- clean_data %>%
  dplyr::select(DH_GEOGRAPHY_NAME, year, percent_inhouse_activity) %>%
  pivot_wider(names_from = year, values_from = percent_inhouse_activity) %>%
  column_to_rownames("DH_GEOGRAPHY_NAME")

# Simple clustering based on starting and ending values
la_summary <- clean_data %>%
  group_by(DH_GEOGRAPHY_NAME) %>%
  summarise(
    start_val = first(percent_inhouse_activity, order_by = year),
    end_val = last(percent_inhouse_activity, order_by = year),
    change = end_val - start_val,
    .groups = 'drop'
  ) %>%
  mutate(
    trajectory_type = case_when(
      change > 5 ~ "Strong Increase",
      change > 0 ~ "Slight Increase", 
      change > -5 ~ "Slight Decrease",
      TRUE ~ "Strong Decrease"
    )
  )

viz2_heatmap <- clean_data %>%
  left_join(la_summary, by = "DH_GEOGRAPHY_NAME") %>%
  ggplot(aes(x = year, y = reorder(DH_GEOGRAPHY_NAME, start_val))) +
  geom_tile(aes(fill = percent_inhouse_activity), color = "white", size = 0.1) +
  scale_fill_viridis_c(name = "% In-house") +
  labs(title = "LA Heatmap: Individual Trajectories vs Average",
       subtitle = "LAs ordered by starting values, red line shows average",
       x = "Year", y = "Local Authority") +
  theme_minimal() +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank())

# ==== VISUALIZATION 3: SLOPE GRAPH WITH CONFIDENCE BANDS ====
# Shows start-to-end changes with uncertainty bands
start_end_data <- clean_data %>%
  group_by(DH_GEOGRAPHY_NAME) %>%
  filter(year %in% c(min(year), max(year))) %>%
  mutate(time_point = ifelse(year == min(year), "Start", "End")) %>%
  ungroup()%>%
  dplyr::mutate(time_point = factor(time_point, levels = c("Start", "End")))


viz3_slope <- ggplot(start_end_data, aes(x = time_point, y = percent_inhouse_activity)) +
  geom_line(aes(group = DH_GEOGRAPHY_NAME), 
            alpha = 0.3, color = "gray60") +
  scale_x_discrete(breaks=c("Start", "End"))+
  geom_point(alpha = 0.4, color = "gray60", size = 1) +
  # Add confidence ribbon for average
  stat_summary(aes(group = 1), fun = mean, geom = "line", 
               color = "red", linewidth = 2) +
  stat_summary(aes(group = 1), fun.data = mean_se, geom = "ribbon", 
               alpha = 0.3, fill = "red") +
  stat_summary(aes(group = 1), fun = mean, geom = "point", 
               color = "red", size = 4) +
  labs(title = "Slope Graph: Start vs End Values",
       subtitle = "Individual LAs in gray, average with confidence bands in red",
       x = "Time Period", 
       y = "Percent In-house Activity") +
  theme_minimal()

# ==== VISUALIZATION 4: SMALL MULTIPLES WITH QUARTILE BACKGROUNDS ====
# Panel of selected LAs with quartile shading
# Select representative LAs (top/bottom quartiles + some middle)
representative_las <- clean_data %>%
  group_by(DH_GEOGRAPHY_NAME) %>%
  summarise(mean_pct = mean(percent_inhouse_activity), .groups = 'drop') %>%
  mutate(quartile = ntile(mean_pct, 4)) %>%
  group_by(quartile) %>%
  slice_sample(n = 2) %>%  # 2 from each quartile
  pull(DH_GEOGRAPHY_NAME)

viz4_small_multiples <- clean_data %>%
  filter(DH_GEOGRAPHY_NAME %in% representative_las) %>%
  ggplot(aes(x = year, y = percent_inhouse_activity)) +
  # Add quartile background ribbons
  geom_ribbon(data = summary_stats, 
              aes(x = year, ymin = q25, ymax = q75), 
              fill = "lightblue", alpha = 0.3, inherit.aes = FALSE) +
  # Individual LA line
  geom_line(color = "navy", linewidth = 1) +
  geom_point(color = "navy", size = 2) +
  # Average line
  geom_line(data = summary_stats, 
            aes(x = year, y = median_pct), 
            color = "red", linewidth = 1.5, inherit.aes = FALSE) +
  facet_wrap(~DH_GEOGRAPHY_NAME, scales = "free_y", ncol = 4) +
  labs(title = "Representative LAs: Individual vs Average Trends",
       subtitle = "Blue ribbon = interquartile range, Red line = average",
       x = "Year", y = "% In-house") +
  theme_minimal() +
  theme(strip.text = element_text(size = 8))

# Display all visualizations
# print(viz1_ridgeline)
# print(viz2_heatmap)
# print(viz3_slope)
# print(viz4_small_multiples)

# ==== BONUS: Your original approach but enhanced ====
viz_enhanced_original <- ggplot(clean_data, aes(x = year, y = percent_inhouse_activity)) +
  # Individual LA lines - very transparent
  geom_line(aes(group = DH_GEOGRAPHY_NAME), 
            alpha = 0.1, color = "gray70", linewidth = 0.3) +
  # Confidence ribbon for average
  geom_ribbon(data = summary_stats, 
              aes(x = year, ymin = q25, ymax = q75), 
              fill = "blue", alpha = 0.2, inherit.aes = FALSE) +
  # Average line - bold
  geom_line(data = summary_stats, 
            aes(x = year, y = median_pct), 
            color = "red", linewidth = 2, inherit.aes = FALSE) +
  labs(title = "Enhanced Original: All LAs with Average Trend",
       subtitle = "Faint gray = individual LAs, Blue ribbon = IQR, Bold red = average",
       x = "Year", y = "Percent In-house Activity") +
  theme_minimal()

# print(viz_enhanced_original)



cowplot::plot_grid(viz1_ridgeline,
                   viz2_heatmap,
                   viz3_slope,
                   viz4_small_multiples,
                   viz_enhanced_original,
                   ncol=3)






#regression tables#



pdata <- pdata.frame(data%>%dplyr::filter(year<2020), index = c("DH_GEOGRAPHY_NAME","year"))
head(pdata)

one <- plm(log(life_expectancy70_to_74_Female)~lagged_activity_insourced+lagged_activity_total+lagged_old_spend+lagged_pension_credits+lagged_total_pop+lagged_per_80+lagged_unemp, data=pdata, method = "within", effect = "twoways")
two <- plm(log(life_expectancy75_to_79_Female)~lagged_activity_insourced+lagged_activity_total+lagged_old_spend+lagged_pension_credits+lagged_total_pop+lagged_per_80+lagged_unemp, data=pdata, method = "within", effect = "twoways")
three <- plm(log(life_expectancy80_to_84_Female)~lagged_activity_insourced+lagged_activity_total+lagged_old_spend+lagged_pension_credits+lagged_total_pop+lagged_per_80+lagged_unemp, data=pdata, method = "within", effect = "twoways")
four <- plm(log(life_expectancy85_to_89_Female)~lagged_activity_insourced+lagged_activity_total+lagged_old_spend+lagged_pension_credits+lagged_total_pop+lagged_per_80+lagged_unemp, data=pdata, method = "within", effect = "twoways")
five <- plm(log(life_expectancy90._Female)~lagged_activity_insourced+lagged_activity_total+lagged_old_spend+lagged_pension_credits+lagged_total_pop+lagged_per_80+lagged_unemp, data=pdata, method = "within", effect = "twoways")


onesum <- as.list(modelsummary(one, output = "modelsummary_list", statistic = c("conf.int","p={p.value}")))
twosum <- as.list(modelsummary(two, output = "modelsummary_list", statistic = c("conf.int","p={p.value}")))
threesum <- as.list(modelsummary(three, output = "modelsummary_list", statistic = c("conf.int","p={p.value}")))
foursum <- as.list(modelsummary(four, output = "modelsummary_list", statistic = c("conf.int","p={p.value}")))
fivesum <- as.list(modelsummary(five, output = "modelsummary_list", statistic = c("conf.int","p={p.value}")))



onesum$tidy$p.value <- coef_test(one, vcov = "CR2", cluster = pdata$DH_GEOGRAPHY_NAME, test = "Satterthwaite")$p
onesum$tidy$std.error <- coef_test(one, vcov = "CR2", cluster = pdata$DH_GEOGRAPHY_NAME, test = "Satterthwaite")$SE
onesum$tidy$conf.low <- onesum$tidy$estimate-(1.96*coef_test(one, vcov = "CR2", cluster = pdata$DH_GEOGRAPHY_NAME, test = "Satterthwaite")$SE)
onesum$tidy$conf.high <- onesum$tidy$estimate+(1.96*coef_test(one, vcov = "CR2", cluster = pdata$DH_GEOGRAPHY_NAME, test = "Satterthwaite")$SE)
onesum$tidy$estimate <- onesum$tidy$estimate

twosum$tidy$p.value <- coef_test(two, vcov = "CR2", cluster = pdata$DH_GEOGRAPHY_NAME, test = "Satterthwaite")$p
twosum$tidy$std.error <- coef_test(two, vcov = "CR2", cluster = pdata$DH_GEOGRAPHY_NAME, test = "Satterthwaite")$SE
twosum$tidy$conf.low <- twosum$tidy$estimate-(1.96*coef_test(two, vcov = "CR2", cluster = pdata$DH_GEOGRAPHY_NAME, test = "Satterthwaite")$SE)
twosum$tidy$conf.high <- twosum$tidy$estimate+(1.96*coef_test(two, vcov = "CR2", cluster = pdata$DH_GEOGRAPHY_NAME, test = "Satterthwaite")$SE)
twosum$tidy$estimate <- twosum$tidy$estimate

threesum$tidy$p.value <- coef_test(three, vcov = "CR2", cluster = pdata$DH_GEOGRAPHY_NAME, test = "Satterthwaite")$p
threesum$tidy$std.error <- coef_test(three, vcov = "CR2", cluster = pdata$DH_GEOGRAPHY_NAME, test = "Satterthwaite")$SE
threesum$tidy$conf.low <- threesum$tidy$estimate-(1.96*coef_test(three, vcov = "CR2", cluster = pdata$DH_GEOGRAPHY_NAME, test = "Satterthwaite")$SE)
threesum$tidy$conf.high <- threesum$tidy$estimate+(1.96*coef_test(three, vcov = "CR2", cluster = pdata$DH_GEOGRAPHY_NAME, test = "Satterthwaite")$SE)
threesum$tidy$estimate <- threesum$tidy$estimate

foursum$tidy$p.value <- coef_test(four, vcov = "CR2", cluster = pdata$DH_GEOGRAPHY_NAME, test = "Satterthwaite")$p
foursum$tidy$std.error <- coef_test(four, vcov = "CR2", cluster = pdata$DH_GEOGRAPHY_NAME, test = "Satterthwaite")$SE
foursum$tidy$conf.low <- foursum$tidy$estimate-(1.96*coef_test(four, vcov = "CR2", cluster = pdata$DH_GEOGRAPHY_NAME, test = "Satterthwaite")$SE)
foursum$tidy$conf.high <- foursum$tidy$estimate+(1.96*coef_test(four, vcov = "CR2", cluster = pdata$DH_GEOGRAPHY_NAME, test = "Satterthwaite")$SE)
foursum$tidy$estimate <- foursum$tidy$estimate

fivesum$tidy$p.value <- coef_test(five, vcov = "CR2", cluster = pdata$DH_GEOGRAPHY_NAME, test = "Satterthwaite")$p
fivesum$tidy$std.error <- coef_test(five, vcov = "CR2", cluster = pdata$DH_GEOGRAPHY_NAME, test = "Satterthwaite")$SE
fivesum$tidy$conf.low <- fivesum$tidy$estimate-(1.96*coef_test(five, vcov = "CR2", cluster = pdata$DH_GEOGRAPHY_NAME, test = "Satterthwaite")$SE)
fivesum$tidy$conf.high <- fivesum$tidy$estimate+(1.96*coef_test(five, vcov = "CR2", cluster = pdata$DH_GEOGRAPHY_NAME, test = "Satterthwaite")$SE)
fivesum$tidy$estimate <- fivesum$tidy$estimate

cm <- c("lagged_activity_insourced" = "In house residential care (%)",
        "lagged_activity_total" = "Total residential care (hrs)",
        "lagged_old_spend" = "Spend on 65+ social care (£000s)",
        "lagged_pension_credits" = "Pension credit spend (£000s)",
        "lagged_total_pop" = "Total population (n)", 
        "lagged_per_80" = "Population over 80 (%)",
        "lagged_unemp" = "Unemployment rate (%))")

rows <- tribble(~term,          ~`Placements outside LA (%) [.95 ci]`,  ~`p-value`,~`Placements outside LA (%) [.95 ci]`,  ~`p-value`,  ~`Placements outside LA (%) [.95 ci]`,  ~`p-value`, ~`Placements unstable (%) [.95 ci]`,  ~`p-value`, ~`Placements unstable (%) [.95 ci]`,  ~`p-value`,  
                'CCG Fixed Effects', 'Yes',  'Yes', 'Yes',  'Yes','Yes',  'Yes','Yes',  'Yes',  'Yes','Yes',
                'Time Fixed Effects','Yes','Yes','Yes',  'Yes','Yes','Yes','Yes',  'Yes',  'Yes','Yes',
                'Clustered Standard Errors', 'Yes','Yes', 'Yes',  'Yes','Yes','Yes','Yes',  'Yes','Yes','Yes')


table <- modelsummary(list("Aged 70-74 [.95 ci]"=onesum,"p-value"=onesum,"Aged 75-79 [.95 ci]"=twosum,"p-value"=twosum,"Aged 80-85 [.95 ci]"=threesum,"p-value"=threesum, "Aged 85-90 (%) [.95 ci]" = foursum, "p-value" = foursum,"Aged 90+ [.95 ci]" = fivesum, "p-value" = fivesum),
                      coef_omit = "Intercept|dept|year", add_rows = rows, coef_map = cm, title = "Female life expectancy",
                      fmt = 4, estimate = c("{estimate} [{conf.low}, {conf.high}]", "p.value","{estimate} [{conf.low}, {conf.high}]", "p.value","{estimate} [{conf.low}, {conf.high}]", "p.value","{estimate} [{conf.low}, {conf.high}]", "p.value","{estimate} [{conf.low}, {conf.high}]", "p.value"), statistic = NULL,
                      notes = list('Table reports results from multivariate longitudinal regression models.',
                                   'Robust SEs are clustered at CCG level and use a bias-reduced linearization estimator (CR2)'),
                      output = "gt") 

table







pdata <- pdata.frame(data%>%dplyr::filter(year<2020), index = c("DH_GEOGRAPHY_NAME","year"))
head(pdata)

one <- plm(log(life_expectancy70_to_74_Male)~lagged_activity_insourced+lagged_activity_total+lagged_old_spend+lagged_pension_credits+lagged_total_pop+lagged_per_80+lagged_unemp, data=pdata, method = "within", effect = "twoways")
two <- plm(log(life_expectancy75_to_79_Male)~lagged_activity_insourced+lagged_activity_total+lagged_old_spend+lagged_pension_credits+lagged_total_pop+lagged_per_80+lagged_unemp, data=pdata, method = "within", effect = "twoways")
three <- plm(log(life_expectancy80_to_84_Male)~lagged_activity_insourced+lagged_activity_total+lagged_old_spend+lagged_pension_credits+lagged_total_pop+lagged_per_80+lagged_unemp, data=pdata, method = "within", effect = "twoways")
four <- plm(log(life_expectancy85_to_89_Male)~lagged_activity_insourced+lagged_activity_total+lagged_old_spend+lagged_pension_credits+lagged_total_pop+lagged_per_80+lagged_unemp, data=pdata, method = "within", effect = "twoways")
five <- plm(log(life_expectancy90._Male)~lagged_activity_insourced+lagged_activity_total+lagged_old_spend+lagged_pension_credits+lagged_total_pop+lagged_per_80+lagged_unemp, data=pdata, method = "within", effect = "twoways")


onesum <- as.list(modelsummary(one, output = "modelsummary_list", statistic = c("conf.int","p={p.value}")))
twosum <- as.list(modelsummary(two, output = "modelsummary_list", statistic = c("conf.int","p={p.value}")))
threesum <- as.list(modelsummary(three, output = "modelsummary_list", statistic = c("conf.int","p={p.value}")))
foursum <- as.list(modelsummary(four, output = "modelsummary_list", statistic = c("conf.int","p={p.value}")))
fivesum <- as.list(modelsummary(five, output = "modelsummary_list", statistic = c("conf.int","p={p.value}")))



onesum$tidy$p.value <- coef_test(one, vcov = "CR2", cluster = pdata$DH_GEOGRAPHY_NAME, test = "Satterthwaite")$p
onesum$tidy$std.error <- coef_test(one, vcov = "CR2", cluster = pdata$DH_GEOGRAPHY_NAME, test = "Satterthwaite")$SE
onesum$tidy$conf.low <- onesum$tidy$estimate-(1.96*coef_test(one, vcov = "CR2", cluster = pdata$DH_GEOGRAPHY_NAME, test = "Satterthwaite")$SE)
onesum$tidy$conf.high <- onesum$tidy$estimate+(1.96*coef_test(one, vcov = "CR2", cluster = pdata$DH_GEOGRAPHY_NAME, test = "Satterthwaite")$SE)
onesum$tidy$estimate <- onesum$tidy$estimate

twosum$tidy$p.value <- coef_test(two, vcov = "CR2", cluster = pdata$DH_GEOGRAPHY_NAME, test = "Satterthwaite")$p
twosum$tidy$std.error <- coef_test(two, vcov = "CR2", cluster = pdata$DH_GEOGRAPHY_NAME, test = "Satterthwaite")$SE
twosum$tidy$conf.low <- twosum$tidy$estimate-(1.96*coef_test(two, vcov = "CR2", cluster = pdata$DH_GEOGRAPHY_NAME, test = "Satterthwaite")$SE)
twosum$tidy$conf.high <- twosum$tidy$estimate+(1.96*coef_test(two, vcov = "CR2", cluster = pdata$DH_GEOGRAPHY_NAME, test = "Satterthwaite")$SE)
twosum$tidy$estimate <- twosum$tidy$estimate

threesum$tidy$p.value <- coef_test(three, vcov = "CR2", cluster = pdata$DH_GEOGRAPHY_NAME, test = "Satterthwaite")$p
threesum$tidy$std.error <- coef_test(three, vcov = "CR2", cluster = pdata$DH_GEOGRAPHY_NAME, test = "Satterthwaite")$SE
threesum$tidy$conf.low <- threesum$tidy$estimate-(1.96*coef_test(three, vcov = "CR2", cluster = pdata$DH_GEOGRAPHY_NAME, test = "Satterthwaite")$SE)
threesum$tidy$conf.high <- threesum$tidy$estimate+(1.96*coef_test(three, vcov = "CR2", cluster = pdata$DH_GEOGRAPHY_NAME, test = "Satterthwaite")$SE)
threesum$tidy$estimate <- threesum$tidy$estimate

foursum$tidy$p.value <- coef_test(four, vcov = "CR2", cluster = pdata$DH_GEOGRAPHY_NAME, test = "Satterthwaite")$p
foursum$tidy$std.error <- coef_test(four, vcov = "CR2", cluster = pdata$DH_GEOGRAPHY_NAME, test = "Satterthwaite")$SE
foursum$tidy$conf.low <- foursum$tidy$estimate-(1.96*coef_test(four, vcov = "CR2", cluster = pdata$DH_GEOGRAPHY_NAME, test = "Satterthwaite")$SE)
foursum$tidy$conf.high <- foursum$tidy$estimate+(1.96*coef_test(four, vcov = "CR2", cluster = pdata$DH_GEOGRAPHY_NAME, test = "Satterthwaite")$SE)
foursum$tidy$estimate <- foursum$tidy$estimate

fivesum$tidy$p.value <- coef_test(five, vcov = "CR2", cluster = pdata$DH_GEOGRAPHY_NAME, test = "Satterthwaite")$p
fivesum$tidy$std.error <- coef_test(five, vcov = "CR2", cluster = pdata$DH_GEOGRAPHY_NAME, test = "Satterthwaite")$SE
fivesum$tidy$conf.low <- fivesum$tidy$estimate-(1.96*coef_test(five, vcov = "CR2", cluster = pdata$DH_GEOGRAPHY_NAME, test = "Satterthwaite")$SE)
fivesum$tidy$conf.high <- fivesum$tidy$estimate+(1.96*coef_test(five, vcov = "CR2", cluster = pdata$DH_GEOGRAPHY_NAME, test = "Satterthwaite")$SE)
fivesum$tidy$estimate <- fivesum$tidy$estimate

cm <- c("lagged_activity_insourced" = "In house residential care (%)",
        "lagged_activity_total" = "Total residential care (hrs)",
        "lagged_old_spend" = "Spend on 65+ social care (£000s)",
        "lagged_pension_credits" = "Pension credit spend (£000s)",
        "lagged_total_pop" = "Total population (n)", 
        "lagged_per_80" = "Population over 80 (%)",
        "lagged_unemp" = "Unemployment rate (%))")

rows <- tribble(~term,          ~`Placements outside LA (%) [.95 ci]`,  ~`p-value`,~`Placements outside LA (%) [.95 ci]`,  ~`p-value`,  ~`Placements outside LA (%) [.95 ci]`,  ~`p-value`, ~`Placements unstable (%) [.95 ci]`,  ~`p-value`, ~`Placements unstable (%) [.95 ci]`,  ~`p-value`,  
                'CCG Fixed Effects', 'Yes',  'Yes', 'Yes',  'Yes','Yes',  'Yes','Yes',  'Yes',  'Yes','Yes',
                'Time Fixed Effects','Yes','Yes','Yes',  'Yes','Yes','Yes','Yes',  'Yes',  'Yes','Yes',
                'Clustered Standard Errors', 'Yes','Yes', 'Yes',  'Yes','Yes','Yes','Yes',  'Yes','Yes','Yes')


table <- modelsummary(list("Aged 70-74 [.95 ci]"=onesum,"p-value"=onesum,"Aged 75-79 [.95 ci]"=twosum,"p-value"=twosum,"Aged 80-85 [.95 ci]"=threesum,"p-value"=threesum, "Aged 85-90 (%) [.95 ci]" = foursum, "p-value" = foursum,"Aged 90+ [.95 ci]" = fivesum, "p-value" = fivesum),
                      coef_omit = "Intercept|dept|year", add_rows = rows, coef_map = cm, title = "Male life expectancy",
                      fmt = 4, estimate = c("{estimate} [{conf.low}, {conf.high}]", "p.value","{estimate} [{conf.low}, {conf.high}]", "p.value","{estimate} [{conf.low}, {conf.high}]", "p.value","{estimate} [{conf.low}, {conf.high}]", "p.value","{estimate} [{conf.low}, {conf.high}]", "p.value"), statistic = NULL,
                      notes = list('Table reports results from multivariate longitudinal regression models.',
                                   'Robust SEs are clustered at CCG level and use a bias-reduced linearization estimator (CR2)'),
                      output = "gt") 


one <- plm(log(life_expectancy70_to_74_Female)~lagged_old_out+lagged_activity_total+lagged_old_spend+lagged_pension_credits+lagged_total_pop+lagged_per_80+lagged_unemp, data=pdata, method = "within", effect = "twoways")
two <- plm(log(life_expectancy75_to_79_Female)~lagged_old_out+lagged_activity_total+lagged_old_spend+lagged_pension_credits+lagged_total_pop+lagged_per_80+lagged_unemp, data=pdata, method = "within", effect = "twoways")
three <- plm(log(life_expectancy80_to_84_Female)~lagged_old_out+lagged_activity_total+lagged_old_spend+lagged_pension_credits+lagged_total_pop+lagged_per_80+lagged_unemp, data=pdata, method = "within", effect = "twoways")
four <- plm(log(life_expectancy85_to_89_Female)~lagged_old_out+lagged_activity_total+lagged_old_spend+lagged_pension_credits+lagged_total_pop+lagged_per_80+lagged_unemp, data=pdata, method = "within", effect = "twoways")
five <- plm(log(life_expectancy90._Female)~lagged_old_out+lagged_activity_total+lagged_old_spend+lagged_pension_credits+lagged_total_pop+lagged_per_80+lagged_unemp, data=pdata, method = "within", effect = "twoways")


onesum <- as.list(modelsummary(one, output = "modelsummary_list", statistic = c("conf.int","p={p.value}")))
twosum <- as.list(modelsummary(two, output = "modelsummary_list", statistic = c("conf.int","p={p.value}")))
threesum <- as.list(modelsummary(three, output = "modelsummary_list", statistic = c("conf.int","p={p.value}")))
foursum <- as.list(modelsummary(four, output = "modelsummary_list", statistic = c("conf.int","p={p.value}")))
fivesum <- as.list(modelsummary(five, output = "modelsummary_list", statistic = c("conf.int","p={p.value}")))



onesum$tidy$p.value <- coef_test(one, vcov = "CR2", cluster = pdata$DH_GEOGRAPHY_NAME, test = "Satterthwaite")$p
onesum$tidy$std.error <- coef_test(one, vcov = "CR2", cluster = pdata$DH_GEOGRAPHY_NAME, test = "Satterthwaite")$SE
onesum$tidy$conf.low <- onesum$tidy$estimate-(1.96*coef_test(one, vcov = "CR2", cluster = pdata$DH_GEOGRAPHY_NAME, test = "Satterthwaite")$SE)
onesum$tidy$conf.high <- onesum$tidy$estimate+(1.96*coef_test(one, vcov = "CR2", cluster = pdata$DH_GEOGRAPHY_NAME, test = "Satterthwaite")$SE)
onesum$tidy$estimate <- onesum$tidy$estimate

twosum$tidy$p.value <- coef_test(two, vcov = "CR2", cluster = pdata$DH_GEOGRAPHY_NAME, test = "Satterthwaite")$p
twosum$tidy$std.error <- coef_test(two, vcov = "CR2", cluster = pdata$DH_GEOGRAPHY_NAME, test = "Satterthwaite")$SE
twosum$tidy$conf.low <- twosum$tidy$estimate-(1.96*coef_test(two, vcov = "CR2", cluster = pdata$DH_GEOGRAPHY_NAME, test = "Satterthwaite")$SE)
twosum$tidy$conf.high <- twosum$tidy$estimate+(1.96*coef_test(two, vcov = "CR2", cluster = pdata$DH_GEOGRAPHY_NAME, test = "Satterthwaite")$SE)
twosum$tidy$estimate <- twosum$tidy$estimate

threesum$tidy$p.value <- coef_test(three, vcov = "CR2", cluster = pdata$DH_GEOGRAPHY_NAME, test = "Satterthwaite")$p
threesum$tidy$std.error <- coef_test(three, vcov = "CR2", cluster = pdata$DH_GEOGRAPHY_NAME, test = "Satterthwaite")$SE
threesum$tidy$conf.low <- threesum$tidy$estimate-(1.96*coef_test(three, vcov = "CR2", cluster = pdata$DH_GEOGRAPHY_NAME, test = "Satterthwaite")$SE)
threesum$tidy$conf.high <- threesum$tidy$estimate+(1.96*coef_test(three, vcov = "CR2", cluster = pdata$DH_GEOGRAPHY_NAME, test = "Satterthwaite")$SE)
threesum$tidy$estimate <- threesum$tidy$estimate

foursum$tidy$p.value <- coef_test(four, vcov = "CR2", cluster = pdata$DH_GEOGRAPHY_NAME, test = "Satterthwaite")$p
foursum$tidy$std.error <- coef_test(four, vcov = "CR2", cluster = pdata$DH_GEOGRAPHY_NAME, test = "Satterthwaite")$SE
foursum$tidy$conf.low <- foursum$tidy$estimate-(1.96*coef_test(four, vcov = "CR2", cluster = pdata$DH_GEOGRAPHY_NAME, test = "Satterthwaite")$SE)
foursum$tidy$conf.high <- foursum$tidy$estimate+(1.96*coef_test(four, vcov = "CR2", cluster = pdata$DH_GEOGRAPHY_NAME, test = "Satterthwaite")$SE)
foursum$tidy$estimate <- foursum$tidy$estimate

fivesum$tidy$p.value <- coef_test(five, vcov = "CR2", cluster = pdata$DH_GEOGRAPHY_NAME, test = "Satterthwaite")$p
fivesum$tidy$std.error <- coef_test(five, vcov = "CR2", cluster = pdata$DH_GEOGRAPHY_NAME, test = "Satterthwaite")$SE
fivesum$tidy$conf.low <- fivesum$tidy$estimate-(1.96*coef_test(five, vcov = "CR2", cluster = pdata$DH_GEOGRAPHY_NAME, test = "Satterthwaite")$SE)
fivesum$tidy$conf.high <- fivesum$tidy$estimate+(1.96*coef_test(five, vcov = "CR2", cluster = pdata$DH_GEOGRAPHY_NAME, test = "Satterthwaite")$SE)
fivesum$tidy$estimate <- fivesum$tidy$estimate

cm <- c("lagged_old_out" = "Outsourced 65+ social care (%)",
        "lagged_activity_total" = "Total residential care (hrs)",
        "lagged_old_spend" = "Spend on 65+ social care (£000s)",
        "lagged_pension_credits" = "Pension credit spend (£000s)",
        "lagged_total_pop" = "Total population (n)", 
        "lagged_per_80" = "Population over 80 (%)",
        "lagged_unemp" = "Unemployment rate (%))")

rows <- tribble(~term,          ~`Placements outside LA (%) [.95 ci]`,  ~`p-value`,~`Placements outside LA (%) [.95 ci]`,  ~`p-value`,  ~`Placements outside LA (%) [.95 ci]`,  ~`p-value`, ~`Placements unstable (%) [.95 ci]`,  ~`p-value`, ~`Placements unstable (%) [.95 ci]`,  ~`p-value`,  
                'CCG Fixed Effects', 'Yes',  'Yes', 'Yes',  'Yes','Yes',  'Yes','Yes',  'Yes',  'Yes','Yes',
                'Time Fixed Effects','Yes','Yes','Yes',  'Yes','Yes','Yes','Yes',  'Yes',  'Yes','Yes',
                'Clustered Standard Errors', 'Yes','Yes', 'Yes',  'Yes','Yes','Yes','Yes',  'Yes','Yes','Yes')


table <- modelsummary(list("Aged 70-74 [.95 ci]"=onesum,"p-value"=onesum,"Aged 75-79 [.95 ci]"=twosum,"p-value"=twosum,"Aged 80-85 [.95 ci]"=threesum,"p-value"=threesum, "Aged 85-90 (%) [.95 ci]" = foursum, "p-value" = foursum,"Aged 90+ [.95 ci]" = fivesum, "p-value" = fivesum),
                      coef_omit = "Intercept|dept|year", add_rows = rows, coef_map = cm, title = "Female life expectancy",
                      fmt = 4, estimate = c("{estimate} [{conf.low}, {conf.high}]", "p.value","{estimate} [{conf.low}, {conf.high}]", "p.value","{estimate} [{conf.low}, {conf.high}]", "p.value","{estimate} [{conf.low}, {conf.high}]", "p.value","{estimate} [{conf.low}, {conf.high}]", "p.value"), statistic = NULL,
                      notes = list('Table reports results from multivariate longitudinal regression models.',
                                   'Robust SEs are clustered at CCG level and use a bias-reduced linearization estimator (CR2)'),
                      output = "gt") 

table


one <- plm(log(life_expectancy70_to_74_Male)~lagged_old_out+lagged_activity_total+lagged_old_spend+lagged_pension_credits+lagged_total_pop+lagged_per_80+lagged_unemp, data=pdata, method = "within", effect = "twoways")
two <- plm(log(life_expectancy75_to_79_Male)~lagged_old_out+lagged_activity_total+lagged_old_spend+lagged_pension_credits+lagged_total_pop+lagged_per_80+lagged_unemp, data=pdata, method = "within", effect = "twoways")
three <- plm(log(life_expectancy80_to_84_Male)~lagged_old_out+lagged_activity_total+lagged_old_spend+lagged_pension_credits+lagged_total_pop+lagged_per_80+lagged_unemp, data=pdata, method = "within", effect = "twoways")
four <- plm(log(life_expectancy85_to_89_Male)~lagged_old_out+lagged_activity_total+lagged_old_spend+lagged_pension_credits+lagged_total_pop+lagged_per_80+lagged_unemp, data=pdata, method = "within", effect = "twoways")
five <- plm(log(life_expectancy90._Male)~lagged_old_out+lagged_activity_total+lagged_old_spend+lagged_pension_credits+lagged_total_pop+lagged_per_80+lagged_unemp, data=pdata, method = "within", effect = "twoways")


onesum <- as.list(modelsummary(one, output = "modelsummary_list", statistic = c("conf.int","p={p.value}")))
twosum <- as.list(modelsummary(two, output = "modelsummary_list", statistic = c("conf.int","p={p.value}")))
threesum <- as.list(modelsummary(three, output = "modelsummary_list", statistic = c("conf.int","p={p.value}")))
foursum <- as.list(modelsummary(four, output = "modelsummary_list", statistic = c("conf.int","p={p.value}")))
fivesum <- as.list(modelsummary(five, output = "modelsummary_list", statistic = c("conf.int","p={p.value}")))



onesum$tidy$p.value <- coef_test(one, vcov = "CR2", cluster = pdata$DH_GEOGRAPHY_NAME, test = "Satterthwaite")$p
onesum$tidy$std.error <- coef_test(one, vcov = "CR2", cluster = pdata$DH_GEOGRAPHY_NAME, test = "Satterthwaite")$SE
onesum$tidy$conf.low <- onesum$tidy$estimate-(1.96*coef_test(one, vcov = "CR2", cluster = pdata$DH_GEOGRAPHY_NAME, test = "Satterthwaite")$SE)
onesum$tidy$conf.high <- onesum$tidy$estimate+(1.96*coef_test(one, vcov = "CR2", cluster = pdata$DH_GEOGRAPHY_NAME, test = "Satterthwaite")$SE)
onesum$tidy$estimate <- onesum$tidy$estimate

twosum$tidy$p.value <- coef_test(two, vcov = "CR2", cluster = pdata$DH_GEOGRAPHY_NAME, test = "Satterthwaite")$p
twosum$tidy$std.error <- coef_test(two, vcov = "CR2", cluster = pdata$DH_GEOGRAPHY_NAME, test = "Satterthwaite")$SE
twosum$tidy$conf.low <- twosum$tidy$estimate-(1.96*coef_test(two, vcov = "CR2", cluster = pdata$DH_GEOGRAPHY_NAME, test = "Satterthwaite")$SE)
twosum$tidy$conf.high <- twosum$tidy$estimate+(1.96*coef_test(two, vcov = "CR2", cluster = pdata$DH_GEOGRAPHY_NAME, test = "Satterthwaite")$SE)
twosum$tidy$estimate <- twosum$tidy$estimate

threesum$tidy$p.value <- coef_test(three, vcov = "CR2", cluster = pdata$DH_GEOGRAPHY_NAME, test = "Satterthwaite")$p
threesum$tidy$std.error <- coef_test(three, vcov = "CR2", cluster = pdata$DH_GEOGRAPHY_NAME, test = "Satterthwaite")$SE
threesum$tidy$conf.low <- threesum$tidy$estimate-(1.96*coef_test(three, vcov = "CR2", cluster = pdata$DH_GEOGRAPHY_NAME, test = "Satterthwaite")$SE)
threesum$tidy$conf.high <- threesum$tidy$estimate+(1.96*coef_test(three, vcov = "CR2", cluster = pdata$DH_GEOGRAPHY_NAME, test = "Satterthwaite")$SE)
threesum$tidy$estimate <- threesum$tidy$estimate

foursum$tidy$p.value <- coef_test(four, vcov = "CR2", cluster = pdata$DH_GEOGRAPHY_NAME, test = "Satterthwaite")$p
foursum$tidy$std.error <- coef_test(four, vcov = "CR2", cluster = pdata$DH_GEOGRAPHY_NAME, test = "Satterthwaite")$SE
foursum$tidy$conf.low <- foursum$tidy$estimate-(1.96*coef_test(four, vcov = "CR2", cluster = pdata$DH_GEOGRAPHY_NAME, test = "Satterthwaite")$SE)
foursum$tidy$conf.high <- foursum$tidy$estimate+(1.96*coef_test(four, vcov = "CR2", cluster = pdata$DH_GEOGRAPHY_NAME, test = "Satterthwaite")$SE)
foursum$tidy$estimate <- foursum$tidy$estimate

fivesum$tidy$p.value <- coef_test(five, vcov = "CR2", cluster = pdata$DH_GEOGRAPHY_NAME, test = "Satterthwaite")$p
fivesum$tidy$std.error <- coef_test(five, vcov = "CR2", cluster = pdata$DH_GEOGRAPHY_NAME, test = "Satterthwaite")$SE
fivesum$tidy$conf.low <- fivesum$tidy$estimate-(1.96*coef_test(five, vcov = "CR2", cluster = pdata$DH_GEOGRAPHY_NAME, test = "Satterthwaite")$SE)
fivesum$tidy$conf.high <- fivesum$tidy$estimate+(1.96*coef_test(five, vcov = "CR2", cluster = pdata$DH_GEOGRAPHY_NAME, test = "Satterthwaite")$SE)
fivesum$tidy$estimate <- fivesum$tidy$estimate

cm <- c("lagged_old_out" = "Outsourced 65+ social care (%)",
        "lagged_activity_total" = "Total residential care (hrs)",
        "lagged_old_spend" = "Spend on 65+ social care (£000s)",
        "lagged_pension_credits" = "Pension credit spend (£000s)",
        "lagged_total_pop" = "Total population (n)", 
        "lagged_per_80" = "Population over 80 (%)",
        "lagged_unemp" = "Unemployment rate (%))")

rows <- tribble(~term,          ~`Placements outside LA (%) [.95 ci]`,  ~`p-value`,~`Placements outside LA (%) [.95 ci]`,  ~`p-value`,  ~`Placements outside LA (%) [.95 ci]`,  ~`p-value`, ~`Placements unstable (%) [.95 ci]`,  ~`p-value`, ~`Placements unstable (%) [.95 ci]`,  ~`p-value`,  
                'CCG Fixed Effects', 'Yes',  'Yes', 'Yes',  'Yes','Yes',  'Yes','Yes',  'Yes',  'Yes','Yes',
                'Time Fixed Effects','Yes','Yes','Yes',  'Yes','Yes','Yes','Yes',  'Yes',  'Yes','Yes',
                'Clustered Standard Errors', 'Yes','Yes', 'Yes',  'Yes','Yes','Yes','Yes',  'Yes','Yes','Yes')


table <- modelsummary(list("Aged 70-74 [.95 ci]"=onesum,"p-value"=onesum,"Aged 75-79 [.95 ci]"=twosum,"p-value"=twosum,"Aged 80-85 [.95 ci]"=threesum,"p-value"=threesum, "Aged 85-90 (%) [.95 ci]" = foursum, "p-value" = foursum,"Aged 90+ [.95 ci]" = fivesum, "p-value" = fivesum),
                      coef_omit = "Intercept|dept|year", add_rows = rows, coef_map = cm, title = "Male life expectancy",
                      fmt = 4, estimate = c("{estimate} [{conf.low}, {conf.high}]", "p.value","{estimate} [{conf.low}, {conf.high}]", "p.value","{estimate} [{conf.low}, {conf.high}]", "p.value","{estimate} [{conf.low}, {conf.high}]", "p.value","{estimate} [{conf.low}, {conf.high}]", "p.value"), statistic = NULL,
                      notes = list('Table reports results from multivariate longitudinal regression models.',
                                   'Robust SEs are clustered at CCG level and use a bias-reduced linearization estimator (CR2)'),
                      output = "gt") 









# Required packages
library(tidyverse)
library(rlang)
library(cowplot)
library(ggplot2)

# --- Helper to make one ribbon+median plot for a single variable ----
make_le_plot <- function(df, var_name, title_text, y_limits = NULL) {
  var_sym <- sym(var_name)
  monthly_stats <- df %>%
    group_by(year) %>%
    summarise(
      p10    = quantile( !!var_sym, 0.10, na.rm = TRUE),
      median = median(  !!var_sym, na.rm = TRUE),
      p90    = quantile( !!var_sym, 0.90, na.rm = TRUE),
      .groups = "drop"
    )
  
  p <- ggplot(monthly_stats, aes(x = year)) +
    geom_ribbon(aes(ymin = p10, ymax = p90), fill = "grey80", alpha = 0.6) +
    geom_line(aes(y = median), linewidth = 1.1, color = "grey20") +
    labs(
      title = title_text,
      x = NULL,
      y = "Life expectancy",
      caption = "Source: ONS. Band = cross-LA distribution per month (10–90%)."
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold", hjust = 0),
      plot.caption = element_text(size = 8),
      axis.title.x = element_blank()
    )
  
  if (!is.null(y_limits)) {
    p <- p + scale_y_continuous(limits = y_limits)
  }
  
  p
}

# --- Age groups and corresponding column names in your data ---
age_groups <- list(
  list(short = "70-74", female = "life_expectancy70_to_74_Female", male = "life_expectancy70_to_74_Male"),
  list(short = "75-79", female = "life_expectancy75_to_79_Female", male = "life_expectancy75_to_79_Male"),
  list(short = "80-84", female = "life_expectancy80_to_84_Female", male = "life_expectancy80_to_84_Male"),
  list(short = "85-89", female = "life_expectancy85_to_89_Female", male = "life_expectancy85_to_89_Male"),
  list(short = "90+",   female = "life_expectancy90+_Female",    male = "life_expectancy90+_Male")
)

# --- Build plots in pairs (female left, male right) with matched y-limits per row ---
plots <- list()
for (age in age_groups) {
  # compute combined monthly range across female+male for consistent y axis
  f_sym <- sym(age$female)
  m_sym <- sym(age$male)
  
  combined_stats <- data %>%
    group_by(year) %>%
    summarise(
      f_p10 = quantile( !!f_sym, 0.10, na.rm = TRUE),
      f_p90 = quantile( !!f_sym, 0.90, na.rm = TRUE),
      m_p10 = quantile( !!m_sym, 0.10, na.rm = TRUE),
      m_p90 = quantile( !!m_sym, 0.90, na.rm = TRUE),
      .groups = "drop"
    )
  
  # Derive sensible y-limits from the combined quantiles (ignore NA rows)
  ymin <- min(c(combined_stats$f_p10, combined_stats$m_p10), na.rm = TRUE)
  ymax <- max(c(combined_stats$f_p90, combined_stats$m_p90), na.rm = TRUE)
  # small padding so ribbon doesn't clip
  pad <- (ymax - ymin) * 0.03
  y_limits <- if (is.finite(ymin) && is.finite(ymax)) c(ymin - pad, ymax + pad) else NULL
  
  # create the two plots
  p_f <- make_le_plot(data, age$female, paste0(age$short, " — Female"), y_limits)
  p_m <- make_le_plot(data, age$male,   paste0(age$short, " — Male"),   y_limits)
  
  # optional: strip x-axis text from upper rows for neatness (you can customize)
  plots <- c(plots, list(p_f, p_m))
}

# --- Combine into 2-column grid: rows = age groups, left=Female, right=Male ---
final_plot <- cowplot::plot_grid(plotlist = plots, ncol = 2, align = "v", axis = "lr")

# Show the final plot
print(final_plot)

# --- Optional: save to file ---
ggsave("life_expectancy_by_age_sex_grid.png", final_plot, width = 10, height = 12, dpi = 300)



data$total_activity

data$inhouse_activity

# packages
library(tidyverse)
library(scales)    # for nice axis formatting

# --- Defensive detection of inhouse format: fraction / percent / count ---
# (adjust 'year' grouping if you want monthly or date-based aggregation)
# Assumes: data$total_activity and data$inhouse_activity exist

# 1) basic checks
if (!("total_activity" %in% names(data)) || !("inhouse_activity" %in% names(data))) {
  stop("data must contain columns: total_activity and inhouse_activity")
}

# 2) detect numeric range of inhouse_activity
inh_max <- max(data$inhouse_activity, na.rm = TRUE)
inh_min <- min(data$inhouse_activity, na.rm = TRUE)

# 3) create canonical columns: total (count), inhouse_count, inhouse_pct (0-100)
data2 <- data %>%
  mutate(
    total = as.numeric(total_activity),
    # Decide what inhouse_activity likely represents:
    # - if max <= 1  => fraction (0..1)
    # - else if max <= 100 => percent (0..100)
    # - else => assume it's already a count (absolute)
    inhouse_count = case_when(
      is.na(inhouse_activity) ~ NA_real_,
      inh_max <= 1 ~ as.numeric(inhouse_activity) * total,             # fraction -> count
      inh_max <= 100 ~ (as.numeric(inhouse_activity) / 100) * total,  # percent -> count
      TRUE ~ as.numeric(inhouse_activity)                              # already count
    )
  ) %>%
  mutate(
    inhouse_pct = if_else(total > 0, inhouse_count / total * 100, NA_real_)
  )

# --- Aggregate by period (year used here; change to date/month if you prefer) ---
monthly_totals <- data2 %>%
  group_by(year) %>%                          # <-- change to group_by(date) if needed
  summarise(
    total = sum(total, na.rm = TRUE),
    inhouse = sum(inhouse_count, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    outsourced = total - inhouse,
    inhouse_pct = if_else(total > 0, inhouse / total * 100, NA_real_)
  )

# --- Long form for stacked area (in-house + outsourced) ---
plot_long <- monthly_totals %>%
  select(year, inhouse, outsourced) %>%
  pivot_longer(cols = -year, names_to = "component", values_to = "value")

# --- Secondary axis scaling: map 0-100% to 0-max_total so percent line sits correctly ---
max_total <- max(monthly_totals$total, na.rm = TRUE)
if (!is.finite(max_total) || max_total == 0) {
  stop("No finite total activity values found for plotting.")
}
scale_factor <- max_total / 100   # multiply percent (0-100) by this to put on primary scale

# --- Plot ---
p_activity <- ggplot() +
  geom_area(
    data = plot_long,
    aes(x = year, y = value, fill = component),
    position = "stack",
    alpha = 0.9
  ) +
  # overlay % in-house as a line (mapped to primary scale via scale_factor)
  geom_line(
    data = monthly_totals,
    aes(x = year, y = inhouse_pct * scale_factor),
    size = 1,
    colour = "black",
    linetype = "dashed"
  ) +
  # primary and secondary axis: primary = counts, secondary = percent (0-100)
  scale_y_continuous(
    name = "Activity (count)",
    sec.axis = sec_axis(~ . / scale_factor, name = "In-house (%)", labels = function(x) paste0(round(x, 1), "%"))
  ) +
  scale_x_continuous(expand = expansion(mult = c(0.01, 0.01))) +
  scale_fill_manual(
    values = c(inhouse = "#2b8cbe", outsourced = "#bdbdbd"),
    labels = c(inhouse = "In-house", outsourced = "Outsourced")
  ) +
  labs(
    title = "Total residential care activity",
    subtitle = "Stacked area = In-house + outsourced; dashed line = % delivered in-house",
    x = NULL  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.title = element_blank(),
    plot.title = element_text(face = "bold", hjust = 0),
    plot.caption = element_text(size = 8),
    axis.title.x = element_blank()
  )+
  coord_cartesian(xlim=c(2003,2022))

# show
print(p_activity)

# optional: save
ggsave("total_activity_inhouse_stack.png", p_activity, width = 10, height = 6, dpi = 300)



####ANALYSIS TWO: deaths and closures####

lookuppost <- read.csv("~/Library/CloudStorage/OneDrive-Nexus365/Documents/Children's Care Homes Project/Data/PCD_OA21_LSOA21_MSOA21_LAD_AUG23_UK_LU.csv")

# STEP 1: Process care home closure data
closures <- read.csv("~/Library/CloudStorage/OneDrive-Nexus365/Documents/Children's Care Homes Project/CQC_API_Materials/Data/complete inspection and location data_ben_feb2025v2.csv") %>%
  dplyr::filter(serviceuserbandolderpeople=="Y")%>%
  select(closed_complete, locationpostalcode, location_end_2025) %>%
  dplyr::rename(pcds = locationpostalcode)%>%
  left_join(., lookuppost)%>%
  dplyr::select(closed_complete, location_end_2025, ladnm )%>%
  
  
  filter(closed_complete == 1) %>%
  mutate(
    closure_date = as.Date(location_end_2025, origin = "1960-01-01"),
    year = isoyear(closure_date),
    month = month(closure_date)
  ) %>%
  group_by(ladnm, year, month) %>%
  summarise(n_closures = n(), .groups = "drop") %>%
  mutate(
    ladnm = str_to_upper(ladnm)  # Uppercase for match
  )

# Fill missing combinations
all_combos <- expand_grid(
  ladnm = unique(closures$ladnm),
  year = min(closures$year):max(closures$year),
  month = 1:12
)

closures_full <- all_combos %>%
  left_join(closures, by = c("ladnm", "year", "month")) %>%
  mutate(n_closures = replace_na(n_closures, 0))%>%
  dplyr::mutate(DH_GEOGRAPHY_NAME  = ladnm %>%
                  gsub('&', 'and', .) %>%
                  gsub('[[:punct:] ]+', ' ', .) %>%
                  gsub('[0-9]', '', .)%>%
                  toupper() %>%
                  gsub("CITY OF", "",.)%>%
                  gsub("UA", "",.)%>%
                  gsub("COUNTY OF", "",.)%>%
                  gsub("ROYAL BOROUGH OF", "",.)%>%
                  gsub("LEICESTER CITY", "LEICESTER",.)%>%
                  gsub("UA", "",.)%>%
                  gsub("DARWIN", "DARWEN", .)%>%
                  gsub("COUNTY DURHAM", "DURHAM", .)%>%
                  gsub("AND DARWEN", "WITH DARWEN", .)%>%
                  gsub("NE SOM", "NORTH EAST SOM", .)%>%
                  gsub("N E SOM", "NORTH EAST SOM", .)%>%
                  str_trim())%>%
  dplyr::select(-ladnm)


library(dplyr)
library(tidyr)
library(lubridate)
library(curl)

one <- rbind( read.csv(curl("https://raw.githubusercontent.com/BenGoodair/care_home_mortality/refs/heads/main/Data/monthly_deaths_ltla/la2006.csv"), skip = 2) %>%
  mutate(DH_GEOGRAPHY_NAME = trimws(paste(X, X.1, X.2))) %>%
  select(-X, -X.1, -X.2) %>%
  pivot_longer(
    cols = matches("^[A-Za-z]{3}\\.\\d{2}$"), # matches e.g. Jan.06, Feb.06
    names_to = "month_year",
    values_to = "deaths"
  ) %>%
  mutate(
    date = my(month_year),                   # parse month-year like "Jan.06"
    year = year(date),
    month = month(date),
    .keep = "unused"                         # drop month_year after parsing
  ) %>%
  select(year, month, deaths, DH_GEOGRAPHY_NAME)%>%
  dplyr::filter(DH_GEOGRAPHY_NAME!=""),
  
  read.csv(curl("https://raw.githubusercontent.com/BenGoodair/care_home_mortality/refs/heads/main/Data/monthly_deaths_ltla/la2007.csv"), skip = 2) %>%
    mutate(DH_GEOGRAPHY_NAME = trimws(paste(X, X.1, X.2))) %>%
    select(-X, -X.1, -X.2) %>%
    pivot_longer(
      cols = matches("^[A-Za-z]{3}\\.\\d{2}$"), # matches e.g. Jan.06, Feb.06
      names_to = "month_year",
      values_to = "deaths"
    ) %>%
    mutate(
      date = my(month_year),                   # parse month-year like "Jan.06"
      year = year(date),
      month = month(date),
      .keep = "unused"                         # drop month_year after parsing
    ) %>%
    select(year, month, deaths, DH_GEOGRAPHY_NAME)%>%
    dplyr::filter(DH_GEOGRAPHY_NAME!=""),
  
  
  read.csv(curl("https://raw.githubusercontent.com/BenGoodair/care_home_mortality/refs/heads/main/Data/monthly_deaths_ltla/la2008.csv"), skip = 2) %>%
    mutate(DH_GEOGRAPHY_NAME = trimws(paste(X, X.1, X.2))) %>%
    select(-X, -X.1, -X.2) %>%
    pivot_longer(
      cols = matches("^[A-Za-z]{3}\\.\\d{2}$"), # matches e.g. Jan.06, Feb.06
      names_to = "month_year",
      values_to = "deaths"
    ) %>%
    mutate(
      date = my(month_year),                   # parse month-year like "Jan.06"
      year = year(date),
      month = month(date),
      .keep = "unused"                         # drop month_year after parsing
    ) %>%
    select(year, month, deaths, DH_GEOGRAPHY_NAME)%>%
    dplyr::filter(DH_GEOGRAPHY_NAME!=""),
  
  
  read.csv(curl("https://raw.githubusercontent.com/BenGoodair/care_home_mortality/refs/heads/main/Data/monthly_deaths_ltla/la2009.csv"), skip = 2) %>%
    mutate(DH_GEOGRAPHY_NAME = trimws(paste(X, X.1, X.2))) %>%
    select(-X, -X.1, -X.2) %>%
    pivot_longer(
      cols = matches("^[A-Za-z]{3}\\.\\d{2}$"), # matches e.g. Jan.06, Feb.06
      names_to = "month_year",
      values_to = "deaths"
    ) %>%
    mutate(
      date = my(month_year),                   # parse month-year like "Jan.06"
      year = year(date),
      month = month(date),
      .keep = "unused"                         # drop month_year after parsing
    ) %>%
    select(year, month, deaths, DH_GEOGRAPHY_NAME)%>%
    dplyr::filter(DH_GEOGRAPHY_NAME!=""),
  
  
  read.csv(curl("https://raw.githubusercontent.com/BenGoodair/care_home_mortality/refs/heads/main/Data/monthly_deaths_ltla/la2010.csv"), skip = 2) %>%
    mutate(DH_GEOGRAPHY_NAME = trimws(paste(X, X.1, X.2))) %>%
    select(-X, -X.1, -X.2) %>%
    pivot_longer(
      cols = matches("^[A-Za-z]{3}\\.\\d{2}$"), # matches e.g. Jan.06, Feb.06
      names_to = "month_year",
      values_to = "deaths"
    ) %>%
    mutate(
      date = my(month_year),                   # parse month-year like "Jan.06"
      year = year(date),
      month = month(date),
      .keep = "unused"                         # drop month_year after parsing
    ) %>%
    select(year, month, deaths, DH_GEOGRAPHY_NAME)%>%
    dplyr::filter(DH_GEOGRAPHY_NAME!=""),
  
  
  read.csv(curl("https://raw.githubusercontent.com/BenGoodair/care_home_mortality/refs/heads/main/Data/monthly_deaths_ltla/la2011.csv"), skip = 2) %>%
    mutate(DH_GEOGRAPHY_NAME = trimws(paste(X, X.1, X.2))) %>%
    select(-X, -X.1, -X.2) %>%
    pivot_longer(
      cols = matches("^[A-Za-z]{3}\\.\\d{2}$"), # matches e.g. Jan.06, Feb.06
      names_to = "month_year",
      values_to = "deaths"
    ) %>%
    mutate(
      date = my(month_year),                   # parse month-year like "Jan.06"
      year = year(date),
      month = month(date),
      .keep = "unused"                         # drop month_year after parsing
    ) %>%
    select(year, month, deaths, DH_GEOGRAPHY_NAME)%>%
    dplyr::filter(DH_GEOGRAPHY_NAME!="")
  ,
  
  
  read.csv(curl("https://raw.githubusercontent.com/BenGoodair/care_home_mortality/refs/heads/main/Data/monthly_deaths_ltla/la2012.csv"), skip = 3) %>%
    mutate(DH_GEOGRAPHY_NAME = trimws(paste(Area.of.usual.residence, X, X.1))) %>%
    select(-X, -X.1,-Area.of.usual.residence ) %>%
    pivot_longer(
      cols = matches("^[A-Za-z]{3}\\.\\d{2}$"), # matches e.g. Jan.06, Feb.06
      names_to = "month_year",
      values_to = "deaths"
    ) %>%
    mutate(
      date = my(month_year),                   # parse month-year like "Jan.06"
      year = year(date),
      month = month(date),
      .keep = "unused"                         # drop month_year after parsing
    ) %>%
    select(year, month, deaths, DH_GEOGRAPHY_NAME)%>%
    dplyr::filter(DH_GEOGRAPHY_NAME!="")
  ,
  
  
  read.csv(curl("https://raw.githubusercontent.com/BenGoodair/care_home_mortality/refs/heads/main/Data/monthly_deaths_ltla/la2013.csv"), skip = 3) %>%
    mutate(DH_GEOGRAPHY_NAME = trimws(paste(Area.of.usual.residence, X, X.1))) %>%
    select(-X, -X.1,-Area.of.usual.residence ) %>%
    pivot_longer(
      cols = matches("^[A-Za-z]{3}\\.\\d{2}$"), # matches e.g. Jan.06, Feb.06
      names_to = "month_year",
      values_to = "deaths"
    ) %>%
    mutate(
      date = my(month_year),                   # parse month-year like "Jan.06"
      year = year(date),
      month = month(date),
      .keep = "unused"                         # drop month_year after parsing
    ) %>%
    select(year, month, deaths, DH_GEOGRAPHY_NAME)%>%
    dplyr::filter(DH_GEOGRAPHY_NAME!="")  ,
  
  
  read.csv(curl("https://raw.githubusercontent.com/BenGoodair/care_home_mortality/refs/heads/main/Data/monthly_deaths_ltla/la2014.csv"), skip = 3) %>%
    mutate(DH_GEOGRAPHY_NAME = trimws(paste(Area.of.usual.residence, X, X.1))) %>%
    select(-X, -X.1,-Area.of.usual.residence ) %>%
    pivot_longer(
      cols = matches("^[A-Za-z]{3}\\.\\d{2}$"), # matches e.g. Jan.06, Feb.06
      names_to = "month_year",
      values_to = "deaths"
    ) %>%
    mutate(
      date = my(month_year),                   # parse month-year like "Jan.06"
      year = year(date),
      month = month(date),
      .keep = "unused"                         # drop month_year after parsing
    ) %>%
    select(year, month, deaths, DH_GEOGRAPHY_NAME)%>%
    dplyr::filter(DH_GEOGRAPHY_NAME!="") ,
  
  
  read.csv(curl("https://raw.githubusercontent.com/BenGoodair/care_home_mortality/refs/heads/main/Data/monthly_deaths_ltla/la2015.csv"), skip = 3) %>%
    mutate(DH_GEOGRAPHY_NAME = X) %>%
    select(-X, -Area.of.usual.residence ) %>%
    pivot_longer(
      cols = matches("^[A-Za-z]{3}\\.\\d{2}$"), # matches e.g. Jan.06, Feb.06
      names_to = "month_year",
      values_to = "deaths"
    ) %>%
    mutate(
      date = my(month_year),                   # parse month-year like "Jan.06"
      year = year(date),
      month = month(date),
      .keep = "unused"                         # drop month_year after parsing
    ) %>%
    select(year, month, deaths, DH_GEOGRAPHY_NAME)%>%
    dplyr::filter(DH_GEOGRAPHY_NAME!="")
  ,
  
  
  read.csv(curl("https://raw.githubusercontent.com/BenGoodair/care_home_mortality/refs/heads/main/Data/monthly_deaths_ltla/la2016.csv"), skip = 3) %>%
    mutate(DH_GEOGRAPHY_NAME = X) %>%
    select(-X, -Area.of.usual.residence ) %>%
    pivot_longer(
      cols = matches("^[A-Za-z]{3}\\.\\d{2}$"), # matches e.g. Jan.06, Feb.06
      names_to = "month_year",
      values_to = "deaths"
    ) %>%
    mutate(
      date = my(month_year),                   # parse month-year like "Jan.06"
      year = year(date),
      month = month(date),
      .keep = "unused"                         # drop month_year after parsing
    ) %>%
    select(year, month, deaths, DH_GEOGRAPHY_NAME)%>%
    dplyr::filter(DH_GEOGRAPHY_NAME!="")  ,
  
  
  read.csv(curl("https://raw.githubusercontent.com/BenGoodair/care_home_mortality/refs/heads/main/Data/monthly_deaths_ltla/la2017.csv"), skip = 3) %>%
    mutate(DH_GEOGRAPHY_NAME = X) %>%
    select(-X, -Area.of.usual.residence ) %>%
    pivot_longer(
      cols = matches("^[A-Za-z]{3}\\.\\d{2}$"), # matches e.g. Jan.06, Feb.06
      names_to = "month_year",
      values_to = "deaths"
    ) %>%
    mutate(
      date = my(month_year),                   # parse month-year like "Jan.06"
      year = year(date),
      month = month(date),
      .keep = "unused"                         # drop month_year after parsing
    ) %>%
    select(year, month, deaths, DH_GEOGRAPHY_NAME)%>%
    dplyr::filter(DH_GEOGRAPHY_NAME!="")  ,
  
  
  read.csv(curl("https://raw.githubusercontent.com/BenGoodair/care_home_mortality/refs/heads/main/Data/monthly_deaths_ltla/la2018.csv"), skip = 3) %>%
    mutate(DH_GEOGRAPHY_NAME = X) %>%
    select(-X, -Area.of.usual.residence ) %>%
    pivot_longer(
      cols = matches("^[A-Za-z]{3}\\.\\d{2}$"), # matches e.g. Jan.06, Feb.06
      names_to = "month_year",
      values_to = "deaths"
    ) %>%
    mutate(
      date = my(month_year),                   # parse month-year like "Jan.06"
      year = year(date),
      month = month(date),
      .keep = "unused"                         # drop month_year after parsing
    ) %>%
    select(year, month, deaths, DH_GEOGRAPHY_NAME)%>%
    dplyr::filter(DH_GEOGRAPHY_NAME!="")  ,
  
  
  read.csv(curl("https://raw.githubusercontent.com/BenGoodair/care_home_mortality/refs/heads/main/Data/monthly_deaths_ltla/la2019.csv"), skip = 3) %>%
    mutate(DH_GEOGRAPHY_NAME = X) %>%
    select(-X, -Area.of.usual.residence ) %>%
    pivot_longer(
      cols = matches("^[A-Za-z]{3}\\.\\d{2}$"), # matches e.g. Jan.06, Feb.06
      names_to = "month_year",
      values_to = "deaths"
    ) %>%
    mutate(
      date = my(month_year),                   # parse month-year like "Jan.06"
      year = year(date),
      month = month(date),
      .keep = "unused"                         # drop month_year after parsing
    ) %>%
    select(year, month, deaths, DH_GEOGRAPHY_NAME)%>%
    dplyr::filter(DH_GEOGRAPHY_NAME!="")  ,
  
  
  read.csv(curl("https://raw.githubusercontent.com/BenGoodair/care_home_mortality/refs/heads/main/Data/monthly_deaths_ltla/la2020.csv"), skip = 3) %>%
    mutate(DH_GEOGRAPHY_NAME = X) %>%
    select(-X, -Area.of.usual.residence ) %>%
    pivot_longer(
      cols = matches("^[A-Za-z]{3}\\.\\d{2}$"), # matches e.g. Jan.06, Feb.06
      names_to = "month_year",
      values_to = "deaths"
    ) %>%
    mutate(
      date = my(month_year),                   # parse month-year like "Jan.06"
      year = year(date),
      month = month(date),
      .keep = "unused"                         # drop month_year after parsing
    ) %>%
    select(year, month, deaths, DH_GEOGRAPHY_NAME)%>%
    dplyr::filter(DH_GEOGRAPHY_NAME!="")  ,
  
  
  read.csv( curl("https://raw.githubusercontent.com/BenGoodair/care_home_mortality/refs/heads/main/Data/monthly_deaths_ltla/la2021.csv"),skip = 4 ) %>%
    mutate(DH_GEOGRAPHY_NAME = Geography) %>%
    select(-Geography, -Code) %>%   # drop original geography and code
    pivot_longer(
      cols = January:December,      # select all month columns
      names_to = "month_name",
      values_to = "deaths"
    ) %>%
    mutate(
      month = match(month_name, month.name),  # convert month name to number
      year = 2021                             # assign the year manually
    ) %>%
    select(year, month, deaths, DH_GEOGRAPHY_NAME) ,
  
  read.csv( curl("https://raw.githubusercontent.com/BenGoodair/care_home_mortality/refs/heads/main/Data/monthly_deaths_ltla/la2022.csv"),skip = 4 ) %>%
    mutate(DH_GEOGRAPHY_NAME = Geography) %>%
    select(-Geography, -Code) %>%   # drop original geography and code
    pivot_longer(
      cols = January:December,      # select all month columns
      names_to = "month_name",
      values_to = "deaths"
    ) %>%
    mutate(
      month = match(month_name, month.name),  # convert month name to number
      year = 2022                             # assign the year manually
    ) %>%
    select(year, month, deaths, DH_GEOGRAPHY_NAME) ,
  
  read.csv( curl("https://raw.githubusercontent.com/BenGoodair/care_home_mortality/refs/heads/main/Data/monthly_deaths_ltla/la2023.csv"),skip = 4 ) %>%
    mutate(
      year = 2023,
      month = match(Month, month.name),         # convert month name to number
      deaths = Number.of.deaths, # remove commas and convert
      DH_GEOGRAPHY_NAME = Geography
    ) %>%
    select(year, month, deaths, DH_GEOGRAPHY_NAME) ,
  
  read.csv( curl("https://raw.githubusercontent.com/BenGoodair/care_home_mortality/refs/heads/main/Data/monthly_deaths_ltla/la2024.csv"),skip = 4 ) %>%
    mutate(
      year = 2024,
      month = match(Month, month.name),         # convert month name to number
      deaths = Number.of.deaths, # remove commas and convert
      DH_GEOGRAPHY_NAME = Geography
    ) %>%
    select(year, month, deaths, DH_GEOGRAPHY_NAME) ,
  
  read.csv( curl("https://raw.githubusercontent.com/BenGoodair/care_home_mortality/refs/heads/main/Data/monthly_deaths_ltla/la2025.csv"),skip = 4 ) %>%
    mutate(
      year = 2025,
      month = match(Month, month.name),         # convert month name to number
      deaths = Number.of.deaths, # remove commas and convert
      DH_GEOGRAPHY_NAME = Geography
    ) %>%
    select(year, month, deaths, DH_GEOGRAPHY_NAME) 
  
 
  
  

)%>%
  dplyr::mutate(deaths = as.numeric(gsub(",", "", deaths)),
                DH_GEOGRAPHY_NAME  = DH_GEOGRAPHY_NAME %>%
                  gsub('&', 'and', .) %>%
                  gsub('[[:punct:] ]+', ' ', .) %>%
                  gsub('[0-9]', '', .)%>%
                  toupper() %>%
                  gsub("CITY OF", "",.)%>%
                  gsub("UA", "",.)%>%
                  gsub("COUNTY OF", "",.)%>%
                  gsub("ROYAL BOROUGH OF", "",.)%>%
                  gsub("LEICESTER CITY", "LEICESTER",.)%>%
                  gsub("UA", "",.)%>%
                  gsub("DARWIN", "DARWEN", .)%>%
                  gsub("COUNTY DURHAM", "DURHAM", .)%>%
                  gsub("AND DARWEN", "WITH DARWEN", .)%>%
                  gsub("NE SOM", "NORTH EAST SOM", .)%>%
                  gsub("N E SOM", "NORTH EAST SOM", .)%>%
                  str_trim()) 


remove_names <- c(
  "", "TOTAL REGISTRATIONS", "ENGLAND", "ENGLAND AND WALES",
  "NORTH EAST", "NORTH WEST", "YORKSHIRE AND THE HUMBER",
  "EAST MIDLANDS", "WEST MIDLANDS", "EAST", "LONDON",
  "SOUTH EAST", "SOUTH WEST", "WALES",
  "ENGLAND WALES AND ELSEWHERE NOTE",
  "ENGLAND WALES AND NON RESIDENTS",
  # NHS ICBs
  "NHS BATH AND NORTH EAST SOMERSET SWINDON AND WILTSHIRE INTEGRATED CARE BOARD",
  "NHS BEDFORDSHIRE LUTON AND MILTON KEYNES INTEGRATED CARE BOARD",
  "NHS BIRMINGHAM AND SOLIHULL INTEGRATED CARE BOARD",
  "NHS BLACK COUNTRY INTEGRATED CARE BOARD",
  "NHS BRISTOL NORTH SOMERSET AND SOUTH GLOUCESTERSHIRE INTEGRATED CARE BOARD",
  "NHS BUCKINGHAMSHIRE OXFORDSHIRE AND BERKSHIRE WEST INTEGRATED CARE BOARD",
  "NHS CAMBRIDGESHIRE AND PETERBOROUGH INTEGRATED CARE BOARD",
  "NHS CHESHIRE AND MERSEYSIDE INTEGRATED CARE BOARD",
  "NHS CORNWALL AND THE ISLES OF SCILLY INTEGRATED CARE BOARD",
  "NHS COVENTRY AND WARWICKSHIRE INTEGRATED CARE BOARD",
  "NHS DERBY AND DERBYSHIRE INTEGRATED CARE BOARD",
  "NHS DEVON INTEGRATED CARE BOARD",
  "NHS DORSET INTEGRATED CARE BOARD",
  "NHS FRIMLEY INTEGRATED CARE BOARD",
  "NHS GLOUCESTERSHIRE INTEGRATED CARE BOARD",
  "NHS GREATER MANCHESTER INTEGRATED CARE BOARD",
  "NHS HAMPSHIRE AND ISLE OF WIGHT INTEGRATED CARE BOARD",
  "NHS HEREFORDSHIRE AND WORCESTERSHIRE INTEGRATED CARE BOARD",
  "NHS HERTFORDSHIRE AND WEST ESSEX INTEGRATED CARE BOARD",
  "NHS HUMBER AND NORTH YORKSHIRE INTEGRATED CARE BOARD",
  "NHS KENT AND MEDWAY INTEGRATED CARE BOARD",
  "NHS LANCASHIRE AND SOUTH CUMBRIA INTEGRATED CARE BOARD",
  "NHS LEICESTER LEICESTERSHIRE AND RUTLAND INTEGRATED CARE BOARD",
  "NHS LINCOLNSHIRE INTEGRATED CARE BOARD",
  "NHS MID AND SOUTH ESSEX INTEGRATED CARE BOARD",
  "NHS NORFOLK AND WAVENEY INTEGRATED CARE BOARD",
  "NHS NORTH CENTRAL LONDON INTEGRATED CARE BOARD",
  "NHS NORTH EAST LONDON INTEGRATED CARE BOARD",
  "NHS NORTH EAST AND NORTH CUMBRIA INTEGRATED CARE BOARD",
  "NHS NORTH WEST LONDON INTEGRATED CARE BOARD",
  "NHS NORTHAMPTONSHIRE INTEGRATED CARE BOARD",
  "NHS NOTTINGHAM AND NOTTINGHAMSHIRE INTEGRATED CARE BOARD",
  "NHS SHROPSHIRE TELFORD AND WREKIN INTEGRATED CARE BOARD",
  "NHS SOMERSET INTEGRATED CARE BOARD",
  "NHS SOUTH EAST LONDON INTEGRATED CARE BOARD",
  "NHS SOUTH WEST LONDON INTEGRATED CARE BOARD",
  "NHS SOUTH YORKSHIRE INTEGRATED CARE BOARD",
  "NHS STAFFORDSHIRE AND STOKE ON TRENT INTEGRATED CARE BOARD",
  "NHS SUFFOLK AND NORTH EAST ESSEX INTEGRATED CARE BOARD",
  "NHS SURREY HEARTLANDS INTEGRATED CARE BOARD",
  "NHS SUSSEX INTEGRATED CARE BOARD",
  "NHS WEST YORKSHIRE INTEGRATED CARE BOARD",
  # Wales health boards
  "ANEURIN BEVAN UNIVERSITY HEALTH BOARD",
  "BETSI CADWALADR UNIVERSITY HEALTH BOARD",
  "CARDIFF AND VALE UNIVERSITY HEALTH BOARD",
  "CWM TAF MORGANNWG UNIVERSITY HEALTH BOARD",
  "HYWEL DDA UNIVERSITY HEALTH BOARD",
  "POWYS TEACHING HEALTH BOARD",
  "SWANSEA BAY UNIVERSITY HEALTH BOARD",
  "ENGLAND WALES AND ELSEWHERE",
  "OUTER LONDON",
  "INNER LONDON",
  "WEST MIDLANDS MET COUNTY",
  "GREATER MANCHESTER MET COUNTY",
  "WEST YORKSHIRE MET COUNTY",
  "MERSEYSIDE MET COUNTY",
  "TYNE AND WEAR MET COUNTY",
  "SOUTH YORKSHIRE MET COUNTY"
)

one <- one %>%
  filter(!DH_GEOGRAPHY_NAME %in% remove_names)%>%
  dplyr::distinct(.keep_all = T)



one <- one %>%
  mutate(
    year  = as.integer(year),
    month = as.integer(month),
    deaths = as.numeric(deaths),
    date  = make_date(year, month, 1)
  ) %>%
  filter(date >= as.Date("2006-01-01"), date <= as.Date("2025-12-01"))


analysis <- full_join(closures_full, one)

head(analysis)

df <- analysis %>%
  rename(ltla = DH_GEOGRAPHY_NAME) %>%
  arrange(ltla, year, month) %>%
  mutate(
    ym = year * 12 + month,
    month_factor = factor(month),      # month FE (seasonality)
    ltla = factor(ltla),
    n_closures = as.numeric(n_closures)
  )

# optional: restrict to pre-COVID for the main specification
df_pre2020 <- df %>% filter(ym <= (2019*12 + 12))

df_lag <- df %>%
  group_by(ltla) %>%
  arrange(ym) %>%
  mutate(n_closures_lag1 = dplyr::lag(n_closures, 1),
         n_closures_lag2 = dplyr::lag(n_closures, 2),
         n_closures_lag3 = dplyr::lag(n_closures, 3),
         n_closures_lag4 = dplyr::lag(n_closures, 4),
         n_closures_lag5 = dplyr::lag(n_closures, 5),
         n_closures_lead1 = dplyr::lead(n_closures, 1),
         n_closures_lead2 = dplyr::lead(n_closures, 2),
         n_closures_lead3 = dplyr::lead(n_closures, 3),
         n_closures_lead4 = dplyr::lead(n_closures, 4),
         n_closures_lead5 = dplyr::lead(n_closures, 5)) %>%
  ungroup()

df_lag_pre2020 <- df_lag %>% filter(ym <= (2019*12 + 12))

ols_fe_lag <- feols(log(deaths) ~ n_closures_lag1 | ltla + month_factor,
                    data = df_lag_pre2020, cluster = "ltla")
summary(ols_fe_lag)


ols_fe_lag <- feols((deaths) ~ n_closures_lag1 | ltla + month_factor,
                    data = df_lag_pre2020, cluster = "ltla")
summary(ols_fe_lag)


# Linear LTLA-specific time trends
df_lag_pre2020 <- df_lag_pre2020 %>%
  group_by(ltla) %>%
  mutate(time_trend = row_number()) %>%  # or use ym - min(ym) + 1
  ungroup()

ols_fe_lag_trend <- feols(deaths ~ n_closures_lag1 + time_trend | ltla + month_factor,
                          data = df_lag_pre2020, cluster = "ltla")

summary(ols_fe_lag_trend)

ols_year_fe <- feols(deaths ~ n_closures_lag1 | ltla + month_factor + year,
                     data = df_lag_pre2020, cluster = "ltla")

summary(ols_year_fe)



# Check number of actual closure events
table(df_lag_pre2020$n_closures_lag1)

# Try longer lags
feols(deaths ~ n_closures_lag1 + n_closures_lag2 + n_closures_lag3 + n_closures_lag4 + n_closures_lag5| ltla + month_factor + year, data = df_lag_pre2020, cluster = "ltla")

# Check if effect varies by closure intensity
# The error suggests a data type issue with the logical indicator
df_lag_pre2020 <- df_lag_pre2020 %>%
  mutate(multiple_closures = as.numeric(n_closures_lag1 >= 2))

feols(deaths ~ multiple_closures | ltla + month_factor + year, 
      data = df_lag_pre2020, cluster = "ltla")

# Also check the actual distribution
table(df_lag_pre2020$n_closures_lag1, useNA = "ifany")



# Event study around closure events
# Focus on LTLAs that actually experience closures
closure_ltlas <- df_lag_pre2020 %>% 
  group_by(ltla) %>% 
  summarise(ever_closure = any(n_closures_lag1 > 0, na.rm = TRUE)) %>%
  filter(ever_closure) %>% 
  pull(ltla)

# Restrict to areas with closure variation
df_closure_sample <- df_lag_pre2020 %>% 
  filter(ltla %in% closure_ltlas)

# Try Poisson regression for count data
library(fixest)
poisson_model <- fepois(deaths ~ n_closures | ltla + month_factor + year,
                        data = df_lag_pre2020, cluster = "ltla")


summary(poisson_model)






# 1. Simple scatter plot: deaths vs closures
ggplot(df_lag_pre2020, aes(x = n_closures_lag1, y = deaths)) +
  geom_point(alpha = 0.3, size = 0.8) +
  geom_smooth(method = "lm", se = TRUE, color = "red") +
  labs(
    title = "Deaths vs Lagged Care Home Closures",
    subtitle = "Raw correlation across all LTLA-months",
    x = "Number of closures (t-1)", 
    y = "Number of deaths (t)"
  ) +
  theme_minimal()






# 2. Box plot by closure categories
df_lag_pre2020 %>%
  mutate(closure_cat = case_when(
    n_closures_lag1 == 0 ~ "0 closures",
    n_closures_lag1 == 1 ~ "1 closure", 
    n_closures_lag1 >= 2 ~ "2+ closures"
  )) %>%
  filter(!is.na(closure_cat)) %>%
  ggplot(aes(x = closure_cat, y = deaths)) +
  geom_boxplot(alpha = 0.7, fill = "lightblue") +
  stat_summary(fun = mean, geom = "point", color = "red", size = 3) +
  labs(
    title = "Distribution of Deaths by Closure Categories",
    subtitle = "Red dots show means",
    x = "Closure category (t-1)",
    y = "Number of deaths (t)"
  ) +
  theme_minimal()



# 4. Time series plot for a few example LTLAs with closures
# First identify LTLAs with some closure variation
example_ltlas <- df_lag_pre2020 %>%
  group_by(ltla) %>%
  summarise(
    total_closures = sum(n_closures_lag1, na.rm = TRUE),
    max_closures = max(n_closures_lag1, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(total_closures >= 5, max_closures >= 2) %>%
  slice_head(n = 30) %>%  # Take first 6 examples
  pull(ltla)

df_lag_pre2020 %>%
  filter(ltla %in% example_ltlas) %>%
  mutate(date = as.Date(paste(year, month, 1, sep = "-"))) %>%
  ggplot(aes(x = date)) +
  geom_line(aes(y = deaths), color = "black", alpha = 0.7) +
  geom_point(aes(y = deaths, size = n_closures_lag1), 
             color = "red", alpha = 0.8) +
  facet_wrap(~ltla, scales = "free_y", ncol = 2) +
  scale_size_continuous(name = "Closures\n(t-1)", range = c(0, 4)) +
  labs(
    title = "Deaths Over Time with Closure Events Highlighted",
    subtitle = "Red dots sized by number of closures in previous month",
    x = "Date", 
    y = "Deaths"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


















# ---- Compute LA-variation per month (national spread across LAs) ----
monthly_stats <- one %>%
  group_by(date) %>%
  summarise(
    p10    = quantile(deaths, 0.10, na.rm = TRUE),
    median = median(deaths, na.rm = TRUE),
    p90    = quantile(deaths, 0.90, na.rm = TRUE),
    .groups = "drop"
  )

# ---- Pick a small set of LAs to lightly overlay (optional) ----
# Here: the 5 LAs with highest total deaths across the period.
highlight_LAs <- one %>%
  group_by(DH_GEOGRAPHY_NAME) %>%
  summarise(total = sum(deaths, na.rm = TRUE), .groups = "drop") %>%
  slice_max(total, n = 5, with_ties = FALSE) %>%
  pull(DH_GEOGRAPHY_NAME)

overlay <- one %>%
  filter(DH_GEOGRAPHY_NAME %in% highlight_LAs)

# ---- Lancet-style plot ----
# Notes:
# - greys for the national band/line (focus)
# - thin, semi-transparent lines for selected LAs to show variation without clutter
# - no chartjunk, generous margins, readable ticks

p <- ggplot() +
  # LA variation band
  geom_ribbon(
    data = monthly_stats,
    aes(x = date, ymin = p10, ymax = p90),
    fill = "grey80", alpha = 0.6
  ) +
  # National median
  geom_line(
    data = monthly_stats,
    aes(x = date, y = median),
    linewidth = 1.1, color = "grey20"
  ) +
  
  # Labels
  labs(
    title = "Monthly deaths by local authority, England & Wales (2006–2025)",
    subtitle = "National median with 10th–90th percentile band across local authorities",
    x = NULL,
    y = "Deaths per month",
    caption = "Source: ONS. Band = cross-LA distribution per month (10–90%)."
  ) +
  # Axes, scales
  scale_x_date(
    breaks = pretty_breaks(n = 10),
    labels = \(d) format(d, "%Y")
  ) +
  # Theme tuned for print (Lancet-esque)
  theme_minimal(base_size = 12) 



closures_full <- closures_full %>%
  mutate(
    year  = as.integer(year),
    month = as.integer(month),
    n_closures = as.numeric(n_closures),
    date  = make_date(year, month, 1)
  ) %>%
  filter(date >= as.Date("2006-01-01"), date <= as.Date("2025-12-01"))


library(dplyr)
library(ggplot2)
library(scales)

# ---- Compute LA-variation per month (mean ± SE across LAs) ----
monthly_stats <- closures_full %>%
  group_by(date) %>%
  summarise(
    mean = mean(n_closures, na.rm = TRUE),
    se   = sd(n_closures, na.rm = TRUE) / sqrt(sum(!is.na(n_closures))),
    .groups = "drop"
  )

# ---- Pick a small set of LAs to lightly overlay (optional) ----
highlight_LAs <- closures_full %>%
  group_by(DH_GEOGRAPHY_NAME) %>%
  summarise(total = sum(deaths, na.rm = TRUE), .groups = "drop") %>%
  slice_max(total, n = 5, with_ties = FALSE) %>%
  pull(DH_GEOGRAPHY_NAME)

overlay <- closures_full %>%
  filter(DH_GEOGRAPHY_NAME %in% highlight_LAs)

# ---- Lancet-style plot ----
p <- ggplot() +
  # Mean ± SE ribbon
  geom_ribbon(
    data = monthly_stats,
    aes(x = date, ymin = mean - se, ymax = mean + se),
    fill = "grey80", alpha = 0.6
  ) +
  # Mean line
  geom_line(
    data = monthly_stats,
    aes(x = date, y = mean),
    linewidth = 1.1, color = "grey20"
  ) +
  labs(
    title = "Monthly care home closures by local authority",
    subtitle = "National mean with ±1 SE band across local authorities",
    x = NULL,
    y = "closures per month",
    caption = "Source: CQC Band = mean ± standard error per month across LAs."
  ) +
  scale_x_date(
    breaks = pretty_breaks(n = 10),
    labels = \(d) format(d, "%Y")
  ) +
  theme_minimal(base_size = 12)


p



library(dplyr)
library(ggplot2)
library(scales)

monthly_totals <- closures_full %>%
  group_by(date) %>%
  summarise(total_closures = sum(n_closures, na.rm = TRUE), .groups = "drop")

ggplot(monthly_totals, aes(x = date, y = total_closures)) +
  geom_line(linewidth = 1.1, color = "grey20") +
  labs(
    title = "Total monthly closures, England (2011–2025)",
    x = NULL,
    y = "Total closures",
    caption = "Source: ONS."
  ) +
  scale_x_date(
    breaks = pretty_breaks(n = 10),
    labels = \(d) format(d, "%Y")
  ) +
  theme_minimal(base_size = 12)










####ANALYSIS THREE DID ####

head(data%>%
       dplyr::select(year, DH_GEOGRAPHY_NAME, percent_inhouse_activity, life_expectancy_Female, population_over_80, total_population, total_activity))

# ---- Required packages ----
# install.packages(c("tidyverse","lubridate","fixest","did","broom","ggplot2"))
library(tidyverse)
library(fixest)    # feols + sunab
library(did)       # Callaway & Sant'Anna DiD
library(broom)
library(ggplot2)

# ---- 0. assumptions & input ----
# - data: dataframe available as `data` with columns:
#     year, DH_GEOGRAPHY_NAME, percent_inhouse_activity,
#     life_expectancy_Female, population_over_80, total_population, total_activity
# - Treatment defined as first year percent_inhouse_activity == 0, 
#   provided percent_inhouse_activity stays == 0 for all subsequent years.
# - We keep only LAs with complete non-missing panel for years 2002:2024
yrs_needed <- 2002:2022
n_years_needed <- length(yrs_needed)

# ---- 1. Filter to years 2002-2024 and check basic types ----
df <- data %>%
  dplyr::select(year, DH_GEOGRAPHY_NAME, percent_inhouse_activity, life_expectancy_Female, population_over_80, total_population, total_activity)%>%
  filter(year %in% yrs_needed) %>%
  mutate(
    year = as.integer(year),
    DH_GEOGRAPHY_NAME = as.character(DH_GEOGRAPHY_NAME),
    percent_inhouse_activity = as.numeric(percent_inhouse_activity),
    life_expectancy_Female = as.numeric(life_expectancy_Female),
    population_over_80 = as.numeric(population_over_80),
    total_population = as.numeric(total_population)
  )

# ---- 2. Keep only LAs with a complete panel & no missing key vars ----
panel_summary <- df %>%
  group_by(DH_GEOGRAPHY_NAME) %>%
  summarise(
    n_years = n_distinct(year),
    missing_pct_inhouse = sum(is.na(percent_inhouse_activity)),
    missing_le_f = sum(is.na(life_expectancy_Female)),
    .groups = "drop"
  )

complete_LAs <- panel_summary %>%
  filter(n_years == n_years_needed,
         missing_pct_inhouse == 0,
         missing_le_f == 0) %>%
  pull(DH_GEOGRAPHY_NAME)

df_clean <- df %>% filter(DH_GEOGRAPHY_NAME %in% complete_LAs)

message("Kept ", length(complete_LAs), " LAs with complete panels (2002-2024).")

# ---- 3. Identify treatment year (first persistent 0 of percent_inhouse_activity) ----
treatment_df <- df_clean %>%
  arrange(DH_GEOGRAPHY_NAME, year) %>%
  group_by(DH_GEOGRAPHY_NAME) %>%
  summarise(
    first_zero = ifelse(any(percent_inhouse_activity == 0), min(year[percent_inhouse_activity == 0]), NA_real_),
    persistent_zero = ifelse(is.na(first_zero), FALSE,
                             all(percent_inhouse_activity[year >= first_zero] == 0)),
    treatment_year = ifelse(persistent_zero, first_zero, NA_real_),
    .groups = "drop"
  )

# Quick table
table(treatment_df %>% mutate(treated_flag = !is.na(treatment_year)) %>% pull(treated_flag))

# Merge back
df_panel <- df_clean %>%
  left_join(treatment_df %>% select(DH_GEOGRAPHY_NAME, treatment_year), by = "DH_GEOGRAPHY_NAME") %>%
  mutate(
    # g for did package: 0 = never-treated, otherwise first treat year
    g = ifelse(is.na(treatment_year), 0L, as.integer(treatment_year)),
    # individual treated indicator per observation (post)
    treated = ifelse(g > 0 & year >= g, 1L, 0L),
    post = treated,
    # relative time (for event study); NA for never-treated to avoid confusion
    rel_year = ifelse(g > 0, year - g, NA_real_)
  )

# How many ever treated vs never treated
df_panel %>% distinct(DH_GEOGRAPHY_NAME, g) %>% count(g == 0)



# ---- 5a. Simple TWFE DiD (not robust to staggered-adoption bias) ----
# Create model data: include covariates you want to control for
model_data <- df_panel %>%
  mutate(
    population_over_80 = as.numeric(population_over_80),
    total_population = as.numeric(total_population)
  )

twfe <- feols(
  life_expectancy_Female ~ treated + population_over_80 + total_population
  | DH_GEOGRAPHY_NAME + year,
  data = model_data,
  cluster = "DH_GEOGRAPHY_NAME"
)

summary(twfe)

# ---- 5b. Staggered-adoption DiD: Callaway & Sant'Anna (att_gt + aggte) ----
# NOTE: did::att_gt expects a column (gname) that is 0 for never treated and >0 for first treat year.
# We already have `g` prepared.
# Keep only observations up to 2024 (we already filtered)
# Control formula for covariates:
xform <- ~ population_over_80 + total_population

model_data$DH_GEOGRAPHY_ID <- as.numeric(factor(model_data$DH_GEOGRAPHY_NAME))

model_data$DH_GEOGRAPHY_ID

att <- att_gt(
  yname = "life_expectancy_Female",
  tname = "year",
  idname = "DH_GEOGRAPHY_ID",
  gname = "g",
  data = model_data,
  xformla = xform,
  est_method = "dr",           # double-robust estimation
  control_group = "nevertreated"  # use never-treated as control
)

# Aggregate to event-study (dynamic effects)
agg_dyn <- aggte(att, type = "dynamic", na.rm=T)

#summary(agg_dyn)
# plot dynamic effects (event study)
library(ggplot2)

dfpl <- data.frame(
  event_time = agg_dyn$egt,
  att = agg_dyn$att.egt,
  se = agg_dyn$se.egt
) |>
  mutate(
    ci_low = att - 1.96 * se,
    ci_high = att + 1.96 * se
  )

ggplot(dfpl, aes(x = event_time, y = att)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  geom_ribbon(aes(ymin = ci_low, ymax = ci_high), alpha = 0.2, fill = "steelblue") +
  geom_line(color = "steelblue") +
  geom_point(color = "steelblue") +
  theme_minimal() +
  labs(
    title = "Event study: ATT by years since treatment",
    x = "Years since treatment",
    y = "ATT"
  )

# Aggregate to overall ATT (simple average)
agg_overall <- aggte(att, type = "simple", na.rm=T)
summary(agg_overall)

# ---- 5c. Event study using fixest::sunab (alternative) ----
# Note: sunab expects the first-treated variable (we pass g), and time var year.
# This produces coefficients for each relative period (omitting an omitted base).
es_fe <- feols(
  life_expectancy_Female ~ sunab(g, year) + population_over_80 + total_population
  | DH_GEOGRAPHY_NAME + year,
  data = model_data,
  cluster = "DH_GEOGRAPHY_NAME"
)

summary(es_fe)
iplot(es_fe, xlab = "Years since treatment", main = "Event study (fixest::sunab)")

# ---- 6. Pre-trend check (simple) ----
# Using the Callaway & Sant'Anna dynamic results:
# Pre-period coefficients are for negative rel_years; we can check joint significance of pre-trends.
# agg_dyn$att.egt is the vector of event-time ATT; agg_dyn$egt are the event-time lags.
evt_df <- data.frame(event_time = agg_dyn$egt, att = agg_dyn$att.egt, se = agg_dyn$se.egt)
evt_df %>% filter(event_time < 0) %>%
  arrange(event_time) %>%
  print(n = 20)

# You could run a Wald test for pre-trend = 0 using the fixest event-study coefficients,
# but the exact code depends on which model you prefer. Example (approx):
coefs <- coef(es_fe)
pre_coefs <- coefs[grepl("^sunab::", names(coefs)) & grepl("^-", names(coefs))]
# (Above lines are helper; you can compute joint test with linearHypothesis from car package)


summary(lm(life_expectancy_Female,data=model_data))







####Claude effort####

## 1. Robustness Checks

# Check reverse causality - does life expectancy Granger-cause outsourcing?
rev_male <- pgrangertest(percent_sector ~ Life.expectancy..years._Male,
                         order = 1, data = pdata_bal)
print(rev_male)

rev_female <- pgrangertest(percent_sector ~ Life.expectancy..years._Female,
                           order = 1, data = pdata_bal)
print(rev_female)

# Try different lag structures (order = 2, 3)
fwd_male_lag2 <- pgrangertest(Life.expectancy..years._Male ~ percent_sector,
                              order = 2, data = pdata_bal)
print(fwd_male_lag2)

fwd_female_lag2 <- pgrangertest(Life.expectancy..years._Female ~ percent_sector,
                                order = 2, data = pdata_bal)
print(fwd_female_lag2)

## 2. Effect Size Estimation

# Calculate elasticity manually for interpretation
data <- data %>%
  dplyr::group_by(DH_GEOGRAPHY_NAME) %>%
  dplyr::mutate(
    pct_change_sector = (percent_sector - dplyr::lag(percent_sector))/dplyr::lag(percent_sector) * 100,
    pct_change_le_male = (Life.expectancy..years._Male - dplyr::lag(Life.expectancy..years._Male))/dplyr::lag(Life.expectancy..years._Male) * 100,
    pct_change_le_female = (Life.expectancy..years._Female - dplyr::lag(Life.expectancy..years._Female))/dplyr::lag(Life.expectancy..years._Female) * 100
  ) %>%
  ungroup()

# Simple elasticity models
elasticity_model_male <- lm(pct_change_le_male ~ pct_change_sector, 
                            data = data %>% filter(!is.infinite(pct_change_sector), 
                                                   !is.na(pct_change_sector),
                                                   !is.infinite(pct_change_le_male),
                                                   !is.na(pct_change_le_male)))
summary(elasticity_model_male)

elasticity_model_female <- lm(pct_change_le_female ~ pct_change_sector, 
                              data = data %>% filter(!is.infinite(pct_change_sector), 
                                                     !is.na(pct_change_sector),
                                                     !is.infinite(pct_change_le_female),
                                                     !is.na(pct_change_le_female)))
summary(elasticity_model_female)

## 3. Alternative Model Specifications

# Dynamic panel model using GMM (if 'plm' package doesn't include GMM, you might need to install 'panelvar')
if (!requireNamespace("panelvar", quietly = TRUE)) {
  install.packages("panelvar")
}
library(panelvar)

# Arellano-Bond dynamic panel model
ab_model_male <- pgmm(Life.expectancy..years._Male ~ dplyr::lag(Life.expectancy..years._Male, 1) + 
                        percent_sector + dplyr::lag(percent_sector, 1) | 
                        dplyr::lag(Life.expectancy..years._Male, 2:3),
                      data = pdata_bal, effect = "twoways", model = "onestep")
summary(ab_model_male)

ab_model_female <- pgmm(Life.expectancy..years._Female ~ dplyr::lag(Life.expectancy..years._Female, 1) + 
                          percent_sector + dplyr::lag(percent_sector, 1) | 
                          dplyr::lag(Life.expectancy..years._Female, 2:3),
                        data = pdata_bal, effect = "twoways", model = "onestep")
summary(ab_model_female)

## 4. Heterogeneity Analysis

# Check if effects differ across regions with different baseline outsourcing levels
data <- data %>%
  dplyr::group_by(DH_GEOGRAPHY_NAME) %>%
  dplyr::mutate(
    avg_outsourcing = mean(percent_sector, na.rm = TRUE),
    outsourcing_tercile = ntile(avg_outsourcing, 3)
  ) %>%
  ungroup()

# Separate models by tercile
for(i in 1:3) {
  tercile_data <- dplyr::filter(data, outsourcing_tercile == i)
  
  cat("\n\n====== TERCILE", i, "======\n")
  cat("Mean outsourcing percentage:", mean(tercile_data$percent_sector, na.rm=TRUE), "\n")
  cat("Number of regions:", length(unique(tercile_data$DH_GEOGRAPHY_NAME)), "\n\n")
  
  model_male <- plm(log(Life.expectancy..years._Male) ~ percent_sector + lagged_le_1_Male,
                    data = tercile_data, 
                    index = c("DH_GEOGRAPHY_NAME", "year"), 
                    effect = "twoways", 
                    model = "within")
  
  model_female <- plm(log(Life.expectancy..years._Female) ~ percent_sector + lagged_le_1_Female, 
                      data = tercile_data, 
                      index = c("DH_GEOGRAPHY_NAME", "year"), 
                      effect = "twoways", 
                      model = "within")
  
  cat("Male model:\n")
  print(summary(model_male))
  
  cat("Female model:\n")
  print(summary(model_female))
}

## 5. Visualize Relationships

library(ggplot2)

# Create scatterplot with regression line
ggplot(data, aes(x = percent_sector, y = Life.expectancy..years._Male)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", color = "blue") +
  facet_wrap(~ year) +
  labs(x = "Percentage Outsourced", y = "Male Life Expectancy", 
       title = "Relationship between Outsourcing and Male Life Expectancy by Year") +
  theme_minimal()

# Create scatterplot with regression line for females
ggplot(data, aes(x = percent_sector, y = Life.expectancy..years._Female)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", color = "red") +
  facet_wrap(~ year) +
  labs(x = "Percentage Outsourced", y = "Female Life Expectancy", 
       title = "Relationship between Outsourcing and Female Life Expectancy by Year") +
  theme_minimal()

# Visualize changes over time
region_summary <- data %>%
  group_by(year) %>%
  summarize(
    mean_outsourcing = mean(percent_sector, na.rm = TRUE),
    mean_le_male = mean(Life.expectancy..years._Male, na.rm = TRUE),
    mean_le_female = mean(Life.expectancy..years._Female, na.rm = TRUE)
  )

# Plot trends over time
ggplot(region_summary, aes(x = year)) +
  geom_line(aes(y = mean_outsourcing), color = "purple") +
  geom_point(aes(y = mean_outsourcing), color = "purple") +
  scale_y_continuous(name = "Mean in-house percentage") +
  theme_minimal() +
  labs(title = "Trend in in-house provision over time")

# Create parallel trends visualization
ggplot(region_summary, aes(x = year)) +
  geom_line(aes(y = scale(mean_outsourcing), color = "Outsourcing %")) +
  geom_line(aes(y = scale(mean_le_male), color = "Male LE")) +
  geom_line(aes(y = scale(mean_le_female), color = "Female LE")) +
  scale_color_manual(values = c("Outsourcing %" = "purple", 
                                "Male LE" = "blue", 
                                "Female LE" = "red")) +
  labs(y = "Standardized Values", x = "Year", 
       title = "Parallel Trends: Outsourcing vs. Life Expectancy", 
       color = "Measure") +
  theme_minimal()

## 6. Check for Non-Linear Relationships

# Add squared term to test for non-linear relationship
nl_model_male <- plm(log(Life.expectancy..years._Male) ~ 
                       percent_sector + I(percent_sector^2) + 
                       lagged_le_1_Male, 
                     data = data, 
                     index = c("DH_GEOGRAPHY_NAME", "year"), 
                     effect = "twoways", 
                     model = "within")
summary(nl_model_male)

nl_model_female <- plm(log(Life.expectancy..years._Female) ~ 
                         percent_sector + I(percent_sector^2) + 
                         lagged_le_1_Female, 
                       data = data, 
                       index = c("DH_GEOGRAPHY_NAME", "year"), 
                       effect = "twoways", 
                       model = "within")
summary(nl_model_female)

# Visualize potential non-linear relationships
ggplot(data, aes(x = percent_sector, y = Life.expectancy..years._Male)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "loess", color = "blue") +
  labs(x = "Percentage Outsourced", y = "Male Life Expectancy", 
       title = "Non-Linear Relationship: Outsourcing vs Male Life Expectancy") +
  theme_minimal()

ggplot(data, aes(x = percent_sector, y = Life.expectancy..years._Female)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "loess", color = "red") +
  labs(x = "Percentage Outsourced", y = "Female Life Expectancy", 
       title = "Non-Linear Relationship: Outsourcing vs Female Life Expectancy") +
  theme_minimal()





## 7. Mediation Analysis

# If you have access to mediating variables, you could test mediation
# For example, if you have data on care quality or staffing levels
# library(mediation)

# med_model <- lm(mediator ~ percent_sector + covariates, data = data)
# outcome_model <- lm(Life.expectancy..years._Male ~ percent_sector + mediator + covariates, data = data)
# med_result <- mediate(med_model, outcome_model, 
#                     treat = "percent_sector", mediator = "mediator", 
#                     boot = TRUE, sims = 1000)
# summary(med_result)































library(dplyr)
library(tidyr)
library(ggplot2)
library(fixest)
library(lubridate)
library(bcp)
library(rdrobust)
library(DIDmultiplegt)
library(modelsummary)
library(kableExtra)

# =============================================================================
# 1. DATA PREPARATION AND ENHANCEMENT
# =============================================================================

# Enhanced data preparation with proper controls
prepare_enhanced_data <- function(chdeaths, closures_full) {
  
  # Process deaths data
  deaths_clean <- chdeaths %>%
    mutate(
      week = as.integer(str_remove(week.number, "week-")),
      year = Time,
      la_code = str_to_upper(DH_GEOGRAPHY_NAME),
      date = case_when(
        week <= 52 ~ as.Date(paste0(year, "-01-01")) + weeks(week - 1),
        TRUE ~ as.Date(paste0(year + 1, "-01-01")) + weeks(week - 53)
      )
    ) %>%
    filter(year %in% 2022:2023) # Focus on post-COVID period
  
  # Process closures data with enhanced features
  closures_enhanced <- closures_full %>%
    filter(year %in% 2022:2023) %>%
    # Add lagged variables for anticipation effects
    arrange(DH_GEOGRAPHY_NAME, year, week) %>%
    group_by(DH_GEOGRAPHY_NAME) %>%
    mutate(
      closure_lead_4 = dplyr::lead(n_closures, 4),
      closure_lead_2 = dplyr::lead(n_closures, 2),
      closure_lag_2 = dplyr::lag(n_closures, 2),
      closure_lag_4 = dplyr::lag(n_closures, 4),
      # Cumulative closures in past 12 weeks
      cum_closures_12w = slider::slide_dbl(n_closures, sum, .before = 11, .complete = TRUE),
      # Moving average baseline
      ma_closures_26w = slider::slide_dbl(n_closures, mean, .before = 25, .after = 1, .complete = TRUE)
    ) %>%
    ungroup() %>%
    replace_na(list(
      closure_lead_4 = 0, closure_lead_2 = 0,
      closure_lag_2 = 0, closure_lag_4 = 0,
      cum_closures_12w = 0, ma_closures_26w = 0
    ))
  
  # Merge datasets
  merged_data <- deaths_clean %>%
    left_join(closures_enhanced, 
              by = c("la_code" = "DH_GEOGRAPHY_NAME", "year", "week")) %>%
    # Add time controls
    mutate(
      month = month(date),
      quarter = quarter(date),
      week_of_year = week,
      year_week = paste(year, week, sep = "_"),
      # Seasonal controls
      sin_week = sin(2 * pi * week / 52),
      cos_week = cos(2 * pi * week / 52),
      # Holiday indicators (approximate)
      christmas_period = ifelse(week %in% c(51, 52, 1, 2), 1, 0),
      easter_period = ifelse(week %in% c(13, 14, 15, 16), 1, 0),
      summer_period = ifelse(week %in% c(26:35), 1, 0)
    ) %>%
    # Create treatment indicators
    mutate(
      any_closure = ifelse(n_closures > 0, 1, 0),
      high_closure = ifelse(n_closures >= quantile(n_closures, 0.9, na.rm = TRUE), 1, 0)
    ) %>%
    # Calculate baseline death rates (pre-treatment periods)
    group_by(la_code) %>%
    mutate(
      baseline_deaths = mean(v4_0[n_closures == 0], na.rm = TRUE),
      deaths_deviation = v4_0 - baseline_deaths,
      log_deaths = log(v4_0 + 1) # Add 1 to handle zeros
    ) %>%
    ungroup()
  
  return(merged_data)
}

# =============================================================================
# 2. IDENTIFICATION STRATEGIES
# =============================================================================

# Strategy 1: Event Study with High-Dimensional Fixed Effects
event_study_analysis <- function(data) {
  
  # Identify isolated closure events (no other closures within 8 weeks)
  isolated_events <- data %>%
    filter(any_closure == 1) %>%
    group_by(la_code) %>%
    arrange(year, week) %>%
    mutate(
      weeks_since_last = c(Inf, diff(as.numeric(date))/7),
      weeks_to_next = c(diff(as.numeric(date))/7, Inf)
    ) %>%
    filter(weeks_since_last >= 8 & weeks_to_next >= 8) %>%
    ungroup() %>%
    select(la_code, year, week) %>%
    mutate(event_id = row_number())
  
  # Create event study dataset
  event_data <- isolated_events %>%
    crossing(weeks_from_event = -8:8) %>%
    mutate(
      target_year = year,
      target_week = week + weeks_from_event
    ) %>%
    # Handle year boundaries
    mutate(
      target_year = case_when(
        target_week < 1 ~ target_year - 1,
        target_week > 52 ~ target_year + 1,
        TRUE ~ target_year
      ),
      target_week = case_when(
        target_week < 1 ~ 52 + target_week,
        target_week > 52 ~ target_week - 52,
        TRUE ~ target_week
      )
    ) %>%
    left_join(data, by = c("la_code", "target_year" = "year", "target_week" = "week")) %>%
    mutate(
      post_closure = ifelse(weeks_from_event >= 0, 1, 0),
      weeks_from_event_factor = factor(weeks_from_event)
    )
  
  # Event study regression with multiple fixed effects
  event_model <- feols(
    v4_0 ~ i(weeks_from_event, ref = -2) |
      la_code + year_week + month,
    data = event_data,
    cluster = ~ la_code
  )
  
  return(list(model = event_model, data = event_data))
}

# Strategy 2: Difference-in-Differences with Staggered Treatment
did_staggered_analysis <- function(data) {
  
  # Create treatment timing variable
  treatment_timing <- data %>%
    filter(any_closure == 1) %>%
    group_by(la_code) %>%
    summarise(first_treatment = min(year * 100 + week), .groups = "drop")
  
  # Prepare DID data
  did_data <- data %>%
    left_join(treatment_timing, by = "la_code") %>%
    mutate(
      period = year * 100 + week,
      treated = !is.na(first_treatment),
      post = case_when(
        is.na(first_treatment) ~ 0,
        period >= first_treatment ~ 1,
        TRUE ~ 0
      ),
      time_to_treatment = case_when(
        is.na(first_treatment) ~ -1000,
        TRUE ~ period - first_treatment
      )
    ) %>%
    filter(time_to_treatment >= -20 & time_to_treatment <= 20) # Balanced window
  
  # Callaway-Sant'Anna DID (requires DIDmultiplegt package)
  did_model <- feols(
    v4_0 ~ i(time_to_treatment, treated, ref = -2) |
      la_code + year_week,
    data = did_data,
    cluster = ~ la_code
  )
  
  return(list(model = did_model, data = did_data))
}


# =============================================================================
# 3. ROBUSTNESS CHECKS
# =============================================================================

# Robustness Check 1: Different outcome specifications
robustness_outcomes <- function(data) {
  
  models <- list(
    "levels" = feols(v4_0 ~ n_closures + baseline_deaths | la_code + year_week, 
                     data = data, cluster = ~ la_code),
    "logs" = feols(log_deaths ~ n_closures + baseline_deaths | la_code + year_week, 
                   data = data, cluster = ~ la_code),
    "rates" = feols(I(v4_0/baseline_deaths) ~ n_closures | la_code + year_week, 
                    data = data, cluster = ~ la_code),
    "deviations" = feols(deaths_deviation ~ n_closures | la_code + year_week, 
                         data = data, cluster = ~ la_code)
  )
  
  return(models)
}

# Robustness Check 2: Placebo tests
placebo_tests <- function(data) {
  
  # Temporal placebo: shift treatment timing
  placebo_data <- data %>%
    group_by(la_code) %>%
    mutate(
      n_closures_lead8 = dplyr::lead(n_closures, 8),
      n_closures_lag8 = dplyr::lag(n_closures, 8)
    ) %>%
    ungroup()
  
  placebo_models <- list(
    "future_treatment" = feols(v4_0 ~ n_closures_lead8 | la_code + year_week, 
                               data = placebo_data, cluster = ~ la_code),
    "past_treatment" = feols(v4_0 ~ n_closures_lag8 | la_code + year_week, 
                             data = placebo_data, cluster = ~ la_code)
  )
  
  return(placebo_models)
}

# Robustness Check 3: Sensitivity to controls
sensitivity_controls <- function(data) {
  
  controls_list <- list(
    "minimal" = ~ n_closures | la_code + year,
    "seasonal" = ~ n_closures + sin_week + cos_week | la_code + year_week,
    "full" = ~ n_closures + baseline_deaths + cum_closures_12w + 
      sin_week + cos_week + christmas_period + easter_period | 
      la_code + year_week + month,
    "max" = ~ n_closures + baseline_deaths + cum_closures_12w + 
      closure_lag_2 + closure_lag_4 + 
      sin_week + cos_week + christmas_period + easter_period + summer_period | 
      la_code^year + year_week + month
  )
  
  models <- map(controls_list, ~ feols(.x, data = data, cluster = ~ la_code))
  names(models) <- names(controls_list)
  
  return(models)
}

# =============================================================================
# 4. HETEROGENEITY ANALYSIS
# =============================================================================

heterogeneity_analysis <- function(data, closure_details) {
  
  # By baseline death rate
  data_het <- data %>%
    mutate(
      high_baseline = ifelse(baseline_deaths > median(baseline_deaths, na.rm = TRUE), 1, 0),
      urban = ifelse(str_detect(la_code, "LONDON|MANCHESTER|BIRMINGHAM|LEEDS"), 1, 0)
    )
  
  het_models <- list(
    "by_baseline" = feols(
      v4_0 ~ n_closures*high_baseline + baseline_deaths | la_code + year_week,
      data = data_het, cluster = ~ la_code
    ),
    "by_urban" = feols(
      v4_0 ~ n_closures*urban + baseline_deaths | la_code + year_week,
      data = data_het, cluster = ~ la_code
    )
  )
  
  return(het_models)
}

# =============================================================================
# 5. VISUALIZATION FUNCTIONS
# =============================================================================

plot_event_study <- function(event_results) {
  
  # Extract coefficients and confidence intervals
  coef_data <- broom::tidy(event_results$model, conf.int = TRUE) %>%
    filter(str_detect(term, "weeks_from_event")) %>%
    mutate(
      weeks_from_event = as.numeric(str_extract(term, "-?\\d+")),
      weeks_from_event = ifelse(is.na(weeks_from_event), -1, weeks_from_event)
    ) %>%
    add_row(weeks_from_event = -1, estimate = 0, conf.low = 0, conf.high = 0) %>%
    arrange(weeks_from_event)
  
  ggplot(coef_data, aes(x = weeks_from_event, y = estimate)) +
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.3, fill = "blue") +
    geom_line(color = "blue", size = 1) +
    geom_point(color = "blue", size = 2) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
    geom_vline(xintercept = -0.5, linetype = "dashed", color = "red", alpha = 0.5) +
    labs(
      title = "Event Study: Effect of Care Home Closures on Deaths",
      subtitle = "95% Confidence Intervals",
      x = "Weeks Relative to Closure",
      y = "Effect on Number of Deaths"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      axis.title = element_text(size = 12),
      axis.text = element_text(size = 10)
    )
}

# =============================================================================
# 6. MAIN ANALYSIS FUNCTION
# =============================================================================

run_complete_analysis <- function(chdeaths, closures_full, closure_details = NULL) {
  
  cat("Preparing enhanced dataset...\n")
  data <- prepare_enhanced_data(chdeaths, closures_full)
  
  cat("Running event study analysis...\n")
  event_results <- event_study_analysis(data)
  
  cat("Running DID analysis...\n")
  did_results <- did_staggered_analysis(data)
  
  
  cat("Running robustness checks...\n")
  robust_outcomes <- robustness_outcomes(data)
  placebo_results <- placebo_tests(data)

  cat("Running heterogeneity analysis...\n")
  het_results <- heterogeneity_analysis(data, closure_details)
  
  # Compile results
  results <- list(
    data = data,
    event_study = event_results,
    did = did_results,
    robustness = list(
      outcomes = robust_outcomes,
      placebo = placebo_results    ),
    heterogeneity = het_results
  )
  
  cat("Analysis complete!\n")
  return(results)
}



# Example usage:
 results <- run_complete_analysis(chdeaths, closures_full)
 plot_event_study(results$event_study)
 
 
 
 summary(results$event_study$model)
 
 
 
 # =============================================================================
 # ENHANCED CARE HOME CLOSURE ANALYSIS
 # =============================================================================
 
 library(tidyverse)
 library(fixest)
 library(broom)
 library(ggplot2)
 library(patchwork)
 library(modelsummary)
 library(kableExtra)
 
 # =============================================================================
 # 1. IMPROVED DATA PREPARATION WITH BETTER MISSING DATA HANDLING
 # =============================================================================
 
 prepare_enhanced_data_v2 <- function(chdeaths, closures_full) {
   
   # More robust data cleaning with explicit missing data handling
   deaths_clean <- chdeaths %>%
     mutate(
       week = as.integer(str_remove(week.number, "week-")),
       year = Time,
       la_code = str_to_upper(DH_GEOGRAPHY_NAME),
       date = case_when(
         week <= 52 ~ as.Date(paste0(year, "-01-01")) + weeks(week - 1),
         TRUE ~ as.Date(paste0(year + 1, "-01-01")) + weeks(week - 53)
       )
     ) %>%
     filter(year %in% 2022:2023, !is.na(v4_0), v4_0 >= 0) %>%  # Remove negative deaths
     # Flag potential data quality issues
     mutate(outlier_flag = ifelse(v4_0 > quantile(v4_0, 0.99, na.rm = TRUE), 1, 0))
   
   # Enhanced closures processing
   closures_enhanced <- closures_full %>%
     filter(year %in% 2022:2023) %>%
     # Replace missing closures with 0 (assumption: missing = no closures)
     mutate(n_closures = replace_na(n_closures, 0)) %>%
     arrange(DH_GEOGRAPHY_NAME, year, week) %>%
     group_by(DH_GEOGRAPHY_NAME) %>%
     mutate(
       # Lead/lag variables with better handling
       closure_lead_4 = dplyr::lead(n_closures, 4, default = 0),
       closure_lead_2 = dplyr::lead(n_closures, 2, default = 0),
       closure_lag_2 = dplyr::lag(n_closures, 2, default = 0),
       closure_lag_4 = dplyr::lag(n_closures, 4, default = 0),
       
       # Rolling sums and averages (more robust)
       cum_closures_12w = slider::slide_dbl(n_closures, sum, .before = 11, .complete = FALSE),
       cum_closures_26w = slider::slide_dbl(n_closures, sum, .before = 25, .complete = FALSE),
       ma_closures_26w = slider::slide_dbl(n_closures, mean, .before = 25, .after = 1, .complete = FALSE),
       
       # Closure intensity measures
       max_closures_12w = slider::slide_dbl(n_closures, max, .before = 11, .complete = FALSE),
       closure_episodes = cumsum(ifelse(n_closures > 0 & lag(n_closures, default = 0) == 0, 1, 0))
     ) %>%
     ungroup() %>%
     # Replace remaining NAs with 0
     mutate(across(starts_with(c("closure_", "cum_", "ma_", "max_")), ~replace_na(.x, 0)))
   
   # Merge with comprehensive join diagnostics
   merged_data <- deaths_clean %>%
     left_join(closures_enhanced, 
               by = c("la_code" = "DH_GEOGRAPHY_NAME", "year", "week"),
               suffix = c("", "_closures")) %>%
     # Better time controls
     mutate(
       month = month(date),
       quarter = quarter(date),
       week_of_year = week,
       year_week = paste(year, week, sep = "_"),
       
       # Enhanced seasonal controls
       sin_week = sin(2 * pi * week / 52),
       cos_week = cos(2 * pi * week / 52),
       sin_month = sin(2 * pi * month / 12),
       cos_month = cos(2 * pi * month / 12),
       
       # Better holiday indicators
       christmas_period = ifelse(week %in% c(51, 52, 1, 2), 1, 0),
       easter_period = case_when(
         year == 2022 & week %in% c(14, 15, 16) ~ 1,  # Easter 2022: April 17
         year == 2023 & week %in% c(13, 14, 15) ~ 1,  # Easter 2023: April 9
         TRUE ~ 0
       ),
       summer_period = ifelse(week %in% c(26:35), 1, 0),
       winter_period = ifelse(week %in% c(1:8, 48:52), 1, 0)
     ) %>%
     # Enhanced treatment variables
     mutate(
       n_closures = replace_na(n_closures, 0),  # Ensure no missing closures
       any_closure = ifelse(n_closures > 0, 1, 0),
       high_closure = ifelse(n_closures >= quantile(n_closures[n_closures > 0], 0.75, na.rm = TRUE), 1, 0),
       multiple_closures = ifelse(n_closures >= 2, 1, 0)
     ) %>%
     # Improved baseline calculations
     group_by(la_code) %>%
     mutate(
       # Use pre-treatment or no-treatment periods for baseline
       baseline_deaths = case_when(
         sum(n_closures) == 0 ~ mean(v4_0, na.rm = TRUE),  # Never treated areas
         TRUE ~ mean(v4_0[n_closures == 0], na.rm = TRUE)  # Pre-treatment periods
       ),
       baseline_deaths = ifelse(is.na(baseline_deaths), mean(v4_0, na.rm = TRUE), baseline_deaths),
       
       # Additional outcome measures
       deaths_deviation = v4_0 - baseline_deaths,
       deaths_rate = v4_0 / baseline_deaths,
       log_deaths = log(v4_0 + 0.5),  # Add 0.5 instead of 1 for better log properties
       
       # LA-level characteristics
       la_total_deaths = sum(v4_0, na.rm = TRUE),
       la_mean_deaths = mean(v4_0, na.rm = TRUE),
       la_total_closures = sum(n_closures, na.rm = TRUE)
     ) %>%
     ungroup() %>%
     # Create balanced panel indicator
     mutate(
       complete_data = ifelse(!is.na(v4_0) & !is.na(n_closures), 1, 0)
     )
   
   # Data quality report
   cat("Data Quality Summary:\n")
   cat("- Original observations:", nrow(deaths_clean), "\n")
   cat("- After merge:", nrow(merged_data), "\n")
   cat("- Complete cases:", sum(merged_data$complete_data), "\n")
   cat("- LAs with closures:", length(unique(merged_data$la_code[merged_data$la_total_closures > 0])), "\n")
   cat("- LAs never treated:", length(unique(merged_data$la_code[merged_data$la_total_closures == 0])), "\n")
   
   return(merged_data)
 }
 
 # =============================================================================
 # 2. ENHANCED EFFECT SIZE INTERPRETATION
 # =============================================================================
 
 calculate_effect_sizes <- function(model, data) {
   
   # Extract coefficient
   coef_closure <- coef(model)["n_closures"]
   
   # Calculate contextual effect sizes
   baseline_mean <- mean(data$baseline_deaths, na.rm = TRUE)
   closure_mean <- mean(data$n_closures[data$n_closures > 0], na.rm = TRUE)
   
   effect_sizes <- list(
     raw_coefficient = coef_closure,
     percent_of_baseline = (coef_closure / baseline_mean) * 100,
     effect_per_typical_closure = coef_closure * closure_mean,
     deaths_per_100_closures = coef_closure * 100,
     baseline_mean = baseline_mean,
     typical_closure_size = closure_mean
   )
   
   return(effect_sizes)
 }
 
 # =============================================================================
 # 3. ENHANCED VISUALIZATION SUITE
 # =============================================================================
 
 create_analysis_plots <- function(results) {
   
   # Event study plot with improvements
   plot_event_study_enhanced <- function(event_results) {
     
     coef_data <- broom::tidy(event_results$model, conf.int = TRUE) %>%
       filter(str_detect(term, "weeks_from_event")) %>%
       mutate(
         weeks_from_event = as.numeric(str_extract(term, "-?\\d+")),
         weeks_from_event = ifelse(is.na(weeks_from_event), -1, weeks_from_event)
       ) %>%
       add_row(weeks_from_event = -1, estimate = 0, conf.low = 0, conf.high = 0) %>%
       arrange(weeks_from_event) %>%
       mutate(
         significant = ifelse(p.value < 0.05, "Significant", "Not Significant"),
         pre_post = ifelse(weeks_from_event < 0, "Pre-Treatment", "Post-Treatment")
       )
     
     ggplot(coef_data, aes(x = weeks_from_event, y = estimate)) +
       geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.3, fill = "steelblue") +
       geom_line(color = "steelblue", size = 1.2) +
       geom_point(aes(color = significant), size = 3) +
       geom_hline(yintercept = 0, linetype = "dashed", color = "red", alpha = 0.7) +
       geom_vline(xintercept = -0.5, linetype = "dashed", color = "red", alpha = 0.5) +
       scale_color_manual(values = c("Significant" = "red", "Not Significant" = "steelblue")) +
       labs(
         title = "Event Study: Dynamic Effects of Care Home Closures",
         subtitle = "Effect on weekly deaths (95% confidence intervals)",
         x = "Weeks Relative to Closure Event",
         y = "Additional Deaths per Week",
         color = "Statistical Significance"
       ) +
       theme_minimal() +
       theme(
         plot.title = element_text(size = 14, face = "bold"),
         plot.subtitle = element_text(size = 12, color = "gray50"),
         legend.position = "bottom"
       )
   }
   
   # Distribution plots
   plot_distributions <- function(data) {
     
     p1 <- ggplot(data, aes(x = v4_0)) +
       geom_histogram(bins = 50, fill = "steelblue", alpha = 0.7) +
       labs(title = "Distribution of Weekly Deaths", x = "Deaths", y = "Frequency") +
       theme_minimal()
     
     p2 <- ggplot(filter(data, n_closures > 0), aes(x = n_closures)) +
       geom_histogram(bins = 20, fill = "coral", alpha = 0.7) +
       labs(title = "Distribution of Closures (When > 0)", x = "Number of Closures", y = "Frequency") +
       theme_minimal()
     
     p3 <- ggplot(data, aes(x = baseline_deaths, y = v4_0)) +
       geom_point(alpha = 0.3) +
       geom_smooth(method = "lm", color = "red") +
       labs(title = "Deaths vs Baseline", x = "Baseline Deaths", y = "Observed Deaths") +
       theme_minimal()
     
     p4 <- data %>%
       group_by(la_code) %>%
       summarise(total_closures = sum(n_closures), mean_deaths = mean(v4_0)) %>%
       ggplot(aes(x = total_closures, y = mean_deaths)) +
       geom_point(alpha = 0.6) +
       geom_smooth(method = "lm", color = "red") +
       labs(title = "LA-Level: Closures vs Deaths", x = "Total Closures", y = "Mean Deaths") +
       theme_minimal()
     
     return((p1 + p2) / (p3 + p4))
   }
   
   # Time series by treatment status
   plot_time_trends <- function(data) {
     
     trend_data <- data %>%
       mutate(treated = ifelse(la_total_closures > 0, "Ever Treated", "Never Treated")) %>%
       group_by(year, week, treated) %>%
       summarise(mean_deaths = mean(v4_0, na.rm = TRUE), .groups = "drop") %>%
       mutate(date = as.Date(paste0(year, "-01-01")) + weeks(week - 1))
     
     ggplot(trend_data, aes(x = date, y = mean_deaths, color = treated)) +
       geom_line(size = 1) +
       scale_color_manual(values = c("Ever Treated" = "red", "Never Treated" = "blue")) +
       labs(
         title = "Time Trends by Treatment Status",
         subtitle = "Mean weekly deaths across local authorities",
         x = "Date", y = "Mean Deaths", color = "Treatment Status"
       ) +
       theme_minimal() +
       theme(legend.position = "bottom")
   }
   
   # Create all plots
   plots <- list(
     event_study = plot_event_study_enhanced(results$event_study),
     distributions = plot_distributions(results$data),
     time_trends = plot_time_trends(results$data)
   )
   
   return(plots)
 }
 
 # =============================================================================
 # 4. IMPROVED ROBUSTNESS AND SPECIFICATION TESTING
 # =============================================================================
 
 comprehensive_robustness <- function(data) {
   
   # Sample restrictions
   sample_tests <- list(
     "full_sample" = data,
     "no_outliers" = filter(data, outlier_flag == 0),
     "balanced_panel" = filter(data, complete_data == 1),
     "high_baseline_only" = filter(data, baseline_deaths > median(baseline_deaths, na.rm = TRUE))
   )
   
   # Run main specification on each sample
   sample_models <- map(sample_tests, ~ {
     feols(v4_0 ~ n_closures | la_code + year_week, data = .x, cluster = ~ la_code)
   })
   
   # Alternative specifications
   spec_models <- list(
     "main" = feols(v4_0 ~ n_closures | la_code + year_week, 
                    data = data, cluster = ~ la_code),
     "with_trends" = feols(v4_0 ~ n_closures + sin_week + cos_week | la_code + year_week, 
                           data = data, cluster = ~ la_code),
     "with_weather" = feols(v4_0 ~ n_closures + winter_period + summer_period | la_code + year_week, 
                            data = data, cluster = ~ la_code),
     "nonlinear" = feols(v4_0 ~ n_closures + I(n_closures^2) | la_code + year_week, 
                         data = data, cluster = ~ la_code),
     "cumulative" = feols(v4_0 ~ cum_closures_12w | la_code + year_week, 
                          data = data, cluster = ~ la_code)
   )
   
   # Multiple hypothesis testing correction
   p_values <- map_dbl(c(sample_models, spec_models), ~ {
     summary(.x)$coeftable[1, "Pr(>|t|)"]
   })
   
   # Bonferroni correction
   p_adjusted <- p.adjust(p_values, method = "bonferroni")
   
   robustness_results <- list(
     sample_models = sample_models,
     spec_models = spec_models,
     p_values_raw = p_values,
     p_values_adjusted = p_adjusted
   )
   
   return(robustness_results)
 }
 
 # =============================================================================
 # 5. ENHANCED RESULTS REPORTING
 # =============================================================================
 
 create_results_table <- function(models_list, effect_sizes_list = NULL) {
   
   # Create publication-ready table
   modelsummary(
     models_list,
     stars = c('*' = .1, '**' = .05, '***' = .01),
     gof_map = c("nobs", "r.squared", "adj.r.squared", "rmse"),
     coef_map = c(
       "n_closures" = "Care Home Closures",
       "cum_closures_12w" = "Cumulative Closures (12 weeks)",
       "I(n_closures^2)" = "Closures Squared"
     ),
     title = "Effect of Care Home Closures on Weekly Deaths",
     notes = c("Standard errors clustered at local authority level.",
               "All models include LA and year-week fixed effects.")
   )
 }
 
 # =============================================================================
 # 6. MAIN ENHANCED ANALYSIS FUNCTION
 # =============================================================================
 
 run_enhanced_analysis <- function(chdeaths, closures_full) {
   
   cat("=== ENHANCED CARE HOME CLOSURE ANALYSIS ===\n\n")
   
   # Data preparation
   cat("1. Enhanced data preparation...\n")
   data <- prepare_enhanced_data_v2(chdeaths, closures_full)
   
   # Main models
   cat("\n2. Running main specifications...\n")
   main_model <- feols(v4_0 ~ n_closures | la_code + year_week, 
                       data = data, cluster = ~ la_code)
   
   effect_sizes <- calculate_effect_sizes(main_model, data)
   
   # Event study
   cat("3. Event study analysis...\n")
   event_results <- event_study_analysis(data)
   
   # Comprehensive robustness
   cat("4. Comprehensive robustness checks...\n")
   robustness_results <- comprehensive_robustness(data)
   
   # Visualizations
   cat("5. Creating enhanced visualizations...\n")
   plots <- create_analysis_plots(list(data = data, event_study = event_results))
   
   # Compile results
   results <- list(
     data = data,
     main_model = main_model,
     effect_sizes = effect_sizes,
     event_study = event_results,
     robustness = robustness_results,
     plots = plots
   )
   
   # Print key results
   cat("\n=== KEY FINDINGS ===\n")
   cat("Main Effect:", round(effect_sizes$raw_coefficient, 4), "\n")
   cat("As % of baseline:", round(effect_sizes$percent_of_baseline, 2), "%\n")
   cat("Deaths per 100 closures:", round(effect_sizes$deaths_per_100_closures, 2), "\n")
   cat("P-value:", round(summary(main_model)$coeftable[1, "Pr(>|t|)"], 4), "\n")
   
   return(results)
 }
 
 # =============================================================================
 # 7. ADDITIONAL DIAGNOSTICS
 # =============================================================================
 
 model_diagnostics <- function(model, data) {
   
   # Residual analysis
   residuals <- residuals(model)
   fitted_vals <- fitted(model)
   
   diagnostic_plots <- list(
     residual_fitted = qplot(fitted_vals, residuals) + 
       geom_hline(yintercept = 0, color = "red") +
       labs(title = "Residuals vs Fitted", x = "Fitted Values", y = "Residuals"),
     
     qq_plot = qplot(sample = residuals) + 
       stat_qq_line(color = "red") +
       labs(title = "Q-Q Plot of Residuals"),
     
     scale_location = qplot(fitted_vals, sqrt(abs(residuals))) +
       labs(title = "Scale-Location Plot", x = "Fitted Values", y = "√|Residuals|")
   )
   
   return(diagnostic_plots)
 }
 
 # Run the enhanced analysis
 enhanced_results <- run_enhanced_analysis(chdeaths, closures_full)
 
 # View effect sizes in context
 enhanced_results$effect_sizes
 
 # Create comprehensive robustness table
 create_results_table(enhanced_results$robustness$spec_models)
 
 # View all plots
 enhanced_results$plots$event_study
 enhanced_results$plots$distributions  
 enhanced_results$plots$time_trends
 
 # Check model assumptions
 diagnostics <- model_diagnostics(enhanced_results$main_model, enhanced_results$data)

 
 
 
 ####OLDDDDD.  ANALYSIS TWO: deaths and closures####
 
 lookuppost <- read.csv("~/Library/CloudStorage/OneDrive-Nexus365/Documents/Children's Care Homes Project/Data/PCD_OA21_LSOA21_MSOA21_LAD_AUG23_UK_LU.csv")
 
 # STEP 1: Process care home closure data
 closures <- read.csv("~/Library/CloudStorage/OneDrive-Nexus365/Documents/Children's Care Homes Project/CQC_API_Materials/Data/complete inspection and location data_ben_feb2025v2.csv") %>%
   dplyr::filter(serviceuserbandolderpeople=="Y")%>%
   select(closed_complete, locationpostalcode, location_end_2025) %>%
   dplyr::rename(pcds = locationpostalcode)%>%
   left_join(., lookuppost)%>%
   dplyr::select(closed_complete, location_end_2025, ladnm )%>%
   
   
   filter(closed_complete == 1) %>%
   mutate(
     closure_date = as.Date(location_end_2025, origin = "1960-01-01"),
     year = isoyear(closure_date),
     week = isoweek(closure_date)
   ) %>%
   group_by(ladnm, year, week) %>%
   summarise(n_closures = n(), .groups = "drop") %>%
   mutate(
     ladnm = str_to_upper(ladnm)  # Uppercase for match
   )
 
 # Fill missing combinations
 all_combos <- expand_grid(
   ladnm = unique(closures$ladnm),
   year = min(closures$year):max(closures$year),
   week = 1:53
 )
 
 closures_full <- all_combos %>%
   left_join(closures, by = c("ladnm", "year", "week")) %>%
   mutate(n_closures = replace_na(n_closures, 0))%>%
   dplyr::mutate(DH_GEOGRAPHY_NAME  = ladnm %>%
                   gsub('&', 'and', .) %>%
                   gsub('[[:punct:] ]+', ' ', .) %>%
                   gsub('[0-9]', '', .)%>%
                   toupper() %>%
                   gsub("CITY OF", "",.)%>%
                   gsub("UA", "",.)%>%
                   gsub("COUNTY OF", "",.)%>%
                   gsub("ROYAL BOROUGH OF", "",.)%>%
                   gsub("LEICESTER CITY", "LEICESTER",.)%>%
                   gsub("UA", "",.)%>%
                   gsub("DARWIN", "DARWEN", .)%>%
                   gsub("COUNTY DURHAM", "DURHAM", .)%>%
                   gsub("AND DARWEN", "WITH DARWEN", .)%>%
                   gsub("NE SOM", "NORTH EAST SOM", .)%>%
                   gsub("N E SOM", "NORTH EAST SOM", .)%>%
                   str_trim())%>%
   dplyr::select(-ladnm)
 
 
 chdeaths <- rbind(read.csv(curl("https://raw.githubusercontent.com/BenGoodair/care_home_mortality/refs/heads/main/Data/weekly-deaths-local-authority-2020-v21.csv"))%>%
                     dplyr::filter(registration.or.occurrence == "occurrences",
                                   cause.of.death == "all-causes",
                                   place.of.death=="care-home")%>%
                     dplyr::select(v4_0, Geography, Time, week.number)%>%
                     dplyr::mutate(DH_GEOGRAPHY_NAME  = Geography %>%
                                     gsub('&', 'and', .) %>%
                                     gsub('[[:punct:] ]+', ' ', .) %>%
                                     gsub('[0-9]', '', .)%>%
                                     toupper() %>%
                                     gsub("CITY OF", "",.)%>%
                                     gsub("UA", "",.)%>%
                                     gsub("COUNTY OF", "",.)%>%
                                     gsub("ROYAL BOROUGH OF", "",.)%>%
                                     gsub("LEICESTER CITY", "LEICESTER",.)%>%
                                     gsub("UA", "",.)%>%
                                     gsub("DARWIN", "DARWEN", .)%>%
                                     gsub("COUNTY DURHAM", "DURHAM", .)%>%
                                     gsub("AND DARWEN", "WITH DARWEN", .)%>%
                                     gsub("NE SOM", "NORTH EAST SOM", .)%>%
                                     gsub("N E SOM", "NORTH EAST SOM", .)%>%
                                     str_trim())%>%
                     dplyr::select(-Geography),
                   read.csv(curl("https://raw.githubusercontent.com/BenGoodair/care_home_mortality/refs/heads/main/Data/weekly-deaths-local-authority-2021-v79.csv"))%>%
                     dplyr::filter(registration.or.occurrence == "occurrences",
                                   cause.of.death == "all-causes",
                                   place.of.death=="care-home")%>%
                     dplyr::select(v4_0, Geography, Time, week.number)%>%
                     dplyr::mutate(DH_GEOGRAPHY_NAME  = Geography %>%
                                     gsub('&', 'and', .) %>%
                                     gsub('[[:punct:] ]+', ' ', .) %>%
                                     gsub('[0-9]', '', .)%>%
                                     toupper() %>%
                                     gsub("CITY OF", "",.)%>%
                                     gsub("UA", "",.)%>%
                                     gsub("COUNTY OF", "",.)%>%
                                     gsub("ROYAL BOROUGH OF", "",.)%>%
                                     gsub("LEICESTER CITY", "LEICESTER",.)%>%
                                     gsub("UA", "",.)%>%
                                     gsub("DARWIN", "DARWEN", .)%>%
                                     gsub("COUNTY DURHAM", "DURHAM", .)%>%
                                     gsub("AND DARWEN", "WITH DARWEN", .)%>%
                                     gsub("NE SOM", "NORTH EAST SOM", .)%>%
                                     gsub("N E SOM", "NORTH EAST SOM", .)%>%
                                     str_trim())%>%
                     dplyr::select(-Geography),
                   read.csv(curl("https://raw.githubusercontent.com/BenGoodair/care_home_mortality/refs/heads/main/Data/weekly-deaths-local-authority-2022-v86.csv"))%>%
                     dplyr::filter(registration.or.occurrence == "occurrences",
                                   cause.of.death == "all-causes",
                                   place.of.death=="care-home")%>%
                     dplyr::select(v4_0, Geography, Time, week.number)%>%
                     dplyr::mutate(DH_GEOGRAPHY_NAME  = Geography %>%
                                     gsub('&', 'and', .) %>%
                                     gsub('[[:punct:] ]+', ' ', .) %>%
                                     gsub('[0-9]', '', .)%>%
                                     toupper() %>%
                                     gsub("CITY OF", "",.)%>%
                                     gsub("UA", "",.)%>%
                                     gsub("COUNTY OF", "",.)%>%
                                     gsub("ROYAL BOROUGH OF", "",.)%>%
                                     gsub("LEICESTER CITY", "LEICESTER",.)%>%
                                     gsub("UA", "",.)%>%
                                     gsub("DARWIN", "DARWEN", .)%>%
                                     gsub("COUNTY DURHAM", "DURHAM", .)%>%
                                     gsub("AND DARWEN", "WITH DARWEN", .)%>%
                                     gsub("NE SOM", "NORTH EAST SOM", .)%>%
                                     gsub("N E SOM", "NORTH EAST SOM", .)%>%
                                     str_trim())%>%
                     dplyr::select(-Geography),
                   read.csv(curl("https://raw.githubusercontent.com/BenGoodair/care_home_mortality/refs/heads/main/Data/weekly-deaths-local-authority-2023-v50%20(1).csv"))%>%
                     dplyr::filter(registration.or.occurrence == "occurrences",
                                   cause.of.death == "all-causes",
                                   place.of.death=="care-home")%>%
                     dplyr::select(v4_0, Geography, Time, week.number)%>%
                     dplyr::mutate(DH_GEOGRAPHY_NAME  = Geography %>%
                                     gsub('&', 'and', .) %>%
                                     gsub('[[:punct:] ]+', ' ', .) %>%
                                     gsub('[0-9]', '', .)%>%
                                     toupper() %>%
                                     gsub("CITY OF", "",.)%>%
                                     gsub("UA", "",.)%>%
                                     gsub("COUNTY OF", "",.)%>%
                                     gsub("ROYAL BOROUGH OF", "",.)%>%
                                     gsub("LEICESTER CITY", "LEICESTER",.)%>%
                                     gsub("UA", "",.)%>%
                                     gsub("DARWIN", "DARWEN", .)%>%
                                     gsub("COUNTY DURHAM", "DURHAM", .)%>%
                                     gsub("AND DARWEN", "WITH DARWEN", .)%>%
                                     gsub("NE SOM", "NORTH EAST SOM", .)%>%
                                     gsub("N E SOM", "NORTH EAST SOM", .)%>%
                                     str_trim())%>%
                     dplyr::select(-Geography)
                   
                   
 )#%>%
 # dplyr::rename(LTLA16NM = DH_GEOGRAPHY_NAME)
 
 
 covid <- rbind(read.csv(curl("https://raw.githubusercontent.com/BenGoodair/care_home_mortality/refs/heads/main/Data/weekly-deaths-local-authority-2020-v21.csv"))%>%
                  dplyr::filter(registration.or.occurrence == "occurrences",
                                cause.of.death == "covid-19",
                                place.of.death=="care-home")%>%
                  dplyr::select(v4_0, Geography, Time, week.number)%>%
                  dplyr::mutate(DH_GEOGRAPHY_NAME  = Geography %>%
                                  gsub('&', 'and', .) %>%
                                  gsub('[[:punct:] ]+', ' ', .) %>%
                                  gsub('[0-9]', '', .)%>%
                                  toupper() %>%
                                  gsub("CITY OF", "",.)%>%
                                  gsub("UA", "",.)%>%
                                  gsub("COUNTY OF", "",.)%>%
                                  gsub("ROYAL BOROUGH OF", "",.)%>%
                                  gsub("LEICESTER CITY", "LEICESTER",.)%>%
                                  gsub("UA", "",.)%>%
                                  gsub("DARWIN", "DARWEN", .)%>%
                                  gsub("COUNTY DURHAM", "DURHAM", .)%>%
                                  gsub("AND DARWEN", "WITH DARWEN", .)%>%
                                  gsub("NE SOM", "NORTH EAST SOM", .)%>%
                                  gsub("N E SOM", "NORTH EAST SOM", .)%>%
                                  str_trim())%>%
                  dplyr::select(-Geography),
                read.csv(curl("https://raw.githubusercontent.com/BenGoodair/care_home_mortality/refs/heads/main/Data/weekly-deaths-local-authority-2021-v79.csv"))%>%
                  dplyr::filter(registration.or.occurrence == "occurrences",
                                cause.of.death == "covid-19",
                                place.of.death=="care-home")%>%
                  dplyr::select(v4_0, Geography, Time, week.number)%>%
                  dplyr::mutate(DH_GEOGRAPHY_NAME  = Geography %>%
                                  gsub('&', 'and', .) %>%
                                  gsub('[[:punct:] ]+', ' ', .) %>%
                                  gsub('[0-9]', '', .)%>%
                                  toupper() %>%
                                  gsub("CITY OF", "",.)%>%
                                  gsub("UA", "",.)%>%
                                  gsub("COUNTY OF", "",.)%>%
                                  gsub("ROYAL BOROUGH OF", "",.)%>%
                                  gsub("LEICESTER CITY", "LEICESTER",.)%>%
                                  gsub("UA", "",.)%>%
                                  gsub("DARWIN", "DARWEN", .)%>%
                                  gsub("COUNTY DURHAM", "DURHAM", .)%>%
                                  gsub("AND DARWEN", "WITH DARWEN", .)%>%
                                  gsub("NE SOM", "NORTH EAST SOM", .)%>%
                                  gsub("N E SOM", "NORTH EAST SOM", .)%>%
                                  str_trim())%>%
                  dplyr::select(-Geography),
                read.csv(curl("https://raw.githubusercontent.com/BenGoodair/care_home_mortality/refs/heads/main/Data/weekly-deaths-local-authority-2022-v86.csv"))%>%
                  dplyr::filter(registration.or.occurrence == "occurrences",
                                cause.of.death == "covid-19",
                                place.of.death=="care-home")%>%
                  dplyr::select(v4_0, Geography, Time, week.number)%>%
                  dplyr::mutate(DH_GEOGRAPHY_NAME  = Geography %>%
                                  gsub('&', 'and', .) %>%
                                  gsub('[[:punct:] ]+', ' ', .) %>%
                                  gsub('[0-9]', '', .)%>%
                                  toupper() %>%
                                  gsub("CITY OF", "",.)%>%
                                  gsub("UA", "",.)%>%
                                  gsub("COUNTY OF", "",.)%>%
                                  gsub("ROYAL BOROUGH OF", "",.)%>%
                                  gsub("LEICESTER CITY", "LEICESTER",.)%>%
                                  gsub("UA", "",.)%>%
                                  gsub("DARWIN", "DARWEN", .)%>%
                                  gsub("COUNTY DURHAM", "DURHAM", .)%>%
                                  gsub("AND DARWEN", "WITH DARWEN", .)%>%
                                  gsub("NE SOM", "NORTH EAST SOM", .)%>%
                                  gsub("N E SOM", "NORTH EAST SOM", .)%>%
                                  str_trim())%>%
                  dplyr::select(-Geography),
                read.csv(curl("https://raw.githubusercontent.com/BenGoodair/care_home_mortality/refs/heads/main/Data/weekly-deaths-local-authority-2023-v50%20(1).csv"))%>%
                  dplyr::filter(registration.or.occurrence == "occurrences",
                                cause.of.death == "covid-19",
                                place.of.death=="care-home")%>%
                  dplyr::select(v4_0, Geography, Time, week.number)%>%
                  dplyr::mutate(DH_GEOGRAPHY_NAME  = Geography %>%
                                  gsub('&', 'and', .) %>%
                                  gsub('[[:punct:] ]+', ' ', .) %>%
                                  gsub('[0-9]', '', .)%>%
                                  toupper() %>%
                                  gsub("CITY OF", "",.)%>%
                                  gsub("UA", "",.)%>%
                                  gsub("COUNTY OF", "",.)%>%
                                  gsub("ROYAL BOROUGH OF", "",.)%>%
                                  gsub("LEICESTER CITY", "LEICESTER",.)%>%
                                  gsub("UA", "",.)%>%
                                  gsub("DARWIN", "DARWEN", .)%>%
                                  gsub("COUNTY DURHAM", "DURHAM", .)%>%
                                  gsub("AND DARWEN", "WITH DARWEN", .)%>%
                                  gsub("NE SOM", "NORTH EAST SOM", .)%>%
                                  gsub("N E SOM", "NORTH EAST SOM", .)%>%
                                  str_trim())%>%
                  dplyr::select(-Geography))%>%
   dplyr::rename(covid = v4_0)
 
 chdeaths <- merge(chdeaths, covid, by=c("DH_GEOGRAPHY_NAME", "Time", "week.number"), all=T)%>%
   dplyr::mutate(v4_0 = v4_0-covid)%>%
   dplyr::select(-covid)
 
 
 
 
 # STEP 2: Process chdeaths data
 chdeaths <- chdeaths %>%
   mutate(
     week = as.integer(str_remove(week.number, "week-")),
     year = Time,
     DH_GEOGRAPHY_NAME = str_to_upper(DH_GEOGRAPHY_NAME)
   )
 
 # STEP 3: Join the datasets
 matched_data <- chdeaths %>%
   full_join(.,closures_full, by = c("DH_GEOGRAPHY_NAME", "year", "week"))%>%
   tidyr::drop_na(n_closures)%>%
   dplyr::filter(year==2022|
                   year==2023)%>%
   dplyr::mutate(locationlocalauthority = DH_GEOGRAPHY_NAME)
 
 
 
 
 
 # Step 1: Identify closure weeks
 closures_only <- matched_data %>%
   filter(n_closures > 0) %>%
   select(locationlocalauthority, year, week) %>%
   distinct()
 
 # Step 2: For each closure, check if it's isolated
 isolated_closures <- closures_only %>%
   rowwise() %>%
   filter({
     # Define time window
     this_la <- locationlocalauthority
     this_year <- year
     this_week <- week
     
     # Build a data.frame of all LA-year-week in +/- 4 window
     weeks_window <- matched_data %>%
       filter(locationlocalauthority == this_la) %>%
       filter((year == this_year & abs(week - this_week) <= 10) |
                (year == this_year - 1 & week > (52 - 10)) |
                (year == this_year + 1 & week <= 10))
     
     # Must have exactly 1 closure (the focal week) in this window
     sum(weeks_window$n_closures > 0) == 1
   }) %>%
   ungroup()
 
 
 
 # assume `matched_data` and `isolated_closures` already exist
 
 # Step 3: Build the ±4-week windows and join
 death_windows <- isolated_closures %>%
   mutate(week_offset = list(-10:10)) %>%
   unnest(week_offset) %>%
   mutate(
     raw_target = week + week_offset,
     # Roll underflow/overflow into adjacent years
     target_year = case_when(
       raw_target < 1   ~ year - 1,
       raw_target > 52  ~ year + 1,
       TRUE             ~ year
     ),
     target_week = ((raw_target - 1) %% 52) + 1
   ) %>%
   left_join(
     matched_data,
     by = c(
       "locationlocalauthority",
       "target_year" = "year",
       "target_week"  = "week"
     )
   ) 
 
 # Step 4: Compute mean, standard error, and 95% CI
 avg_deaths_by_offset <- death_windows %>%
   group_by(week_offset) %>%
   summarise(
     avg_deaths = mean(v4_0, na.rm = TRUE),
     sd_deaths  = sd(v4_0,   na.rm = TRUE),
     n_obs      = n(),
     se         = sd_deaths / sqrt(n_obs),
     # 95% t-based CI
     ci_lower   = avg_deaths - qt(0.975, df = n_obs - 1) * se,
     ci_upper   = avg_deaths + qt(0.975, df = n_obs - 1) * se,
     .groups     = "drop"
   )
 
 # Step 5: Plot with ribbon for 95% CI
 ggplot(avg_deaths_by_offset, aes(x = week_offset, y = avg_deaths)) +
   geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper), alpha = 0.2) +
   geom_line(size = 1.2) +
   geom_point(size = 2) +
   geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
   labs(
     title    = "Average care home deaths around isolated closures",
     x        = "Weeks relative to closure",
     y        = "Average number of deaths"
   ) +
   theme_minimal()
 
 
 