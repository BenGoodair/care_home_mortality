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
  dplyr::select(-`99`)


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
                     dplyr::rename(outsourced_old_care = percent_sector))

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
                     dplyr::rename(spend_old_care = Expenditure))





lifeexpectancy <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/care_home_mortality/refs/heads/main/Data/lifeexpectancylocalareas.csv"), skip=5)%>%
  dplyr::filter(Area.type == "County"|
                  Area.type == "Local Areas",
                Age.band == 19)%>%
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
  dplyr::select(DH_GEOGRAPHY_NAME, year,Life.expectancy..years., Sex)

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

data = dplyr::full_join(outsourced , lifeexpectancy, by=c("DH_GEOGRAPHY_NAME", "year"))%>%
  tidyr::drop_na(Life.expectancy..years., percent_inhouse_activity)%>%
  tidyr::pivot_wider(names_from = "Sex", values_from = c("Life.expectancy..years."), names_prefix = "life_expectancy_")%>%
 # dplyr::full_join(., deaths)%>%
  dplyr::group_by(DH_GEOGRAPHY_NAME)%>%
  arrange(year) %>%
  dplyr::mutate(lagged_sector_1 = dplyr::lag(percent_inhouse_activity, 1),
                lagged_sector_2 = dplyr::lag(percent_inhouse_activity, 2),
                lagged_le_1_Male = dplyr::lag(life_expectancy_Male, 1),
                lagged_le_1_Female = dplyr::lag(life_expectancy_Female, 1))
  


duplicated_rows <- data %>% 
  dplyr::group_by(DH_GEOGRAPHY_NAME, year) %>%
  dplyr::filter(n() > 1)


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

                  
  

data = dplyr::full_join(data , phdata_clean, by=c("DH_GEOGRAPHY_NAME", "year"))
  
data <- data %>% 
  dplyr::full_join(., outsourced_spend)

data <- data %>% 
  dplyr::full_join(., all_spend)






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
# data = data %>%
#   dplyr::group_by(DH_GEOGRAPHY_NAME)%>%
#   arrange(year) %>%
#   dplyr::mutate(lagged_home_out = dplyr::lag(outsourced_home_care, 1),
#                 lagged_home_spend = dplyr::lag(spend_home_care, 1),
#                 lagged_res_out = dplyr::lag(outsourced_res_care, 1),
#                 lagged_res_spend = dplyr::lag(spend_res_care, 1),
#                 lagged_old_out = dplyr::lag(outsourced_old_care, 1),
#                 lagged_old_spend = dplyr::lag(spend_old_care, 1))




nomisd <- read.csv(curl("https://www.nomisweb.co.uk/api/v01/dataset/NM_161_1.data.csv?geography=1774190593...1774190597,1774190637,1774190646,1774190675...1774190678,1774190691,1774190598...1774190601,1774190638,1774190639,1774190652,1774190653,1774190656...1774190670,1774190734,1774190602...1774190606,1774190654,1774190671...1774190674,1774190686...1774190690,1774190607...1774190610,1774190650,1774190651,1774190726,1774190735,1774190736,1774190738,1774190611...1774190613,1774190640,1774190679...1774190685,1774190740,1774190743,1774190745,1774190621...1774190624,1774190644,1774190645,1774190725,1774190729,1774190732,1774190737,1774190741,1774190692...1774190724,1774190625...1774190636,1774190649,1774190728,1774190731,1774190733,1774190739,1774190742,1774190744,1774190614...1774190620,1774190641...1774190643,1774190647,1774190648,1774190655,1774190727,1774190730,1774190746...1774190767&cause_of_death=114390,2420&gender=0&age=17...20&measure=7&measures=20100"))



library(plm)
library(dplyr)



pdata <- pdata.frame(data, index = c("DH_GEOGRAPHY_NAME","year"))




summary(plm(log(Emergency.hospital.admissions.due.to.falls.in.people.aged.80.plus)~percent_inhouse_activity+total_activity, data=pdata, method = "within", effect = "twoways"))
summary(plm(log(Hip.fractures.in.people.aged.80.and.over)~percent_inhouse_activity+total_activity, data=pdata, method = "within", effect = "twoways"))
summary(plm(log(Winter.mortality.index..age.85.plus.+1)~percent_inhouse_activity+total_activity, data=pdata, method = "within", effect = "twoways"))
summary(plm(log(life_expectancy_Female)~percent_inhouse_activity+total_activity, data=pdata, method = "within", effect = "twoways"))
summary(plm(log(life_expectancy_Male)~percent_inhouse_activity+total_activity, data=pdata, method = "within", effect = "twoways"))


summary(plm(log(Emergency.hospital.admissions.due.to.falls.in.people.aged.80.plus)~outsourced_res_care+spend_res_care, data=pdata, method = "within", effect = "twoways"))
summary(plm(log(Hip.fractures.in.people.aged.80.and.over)~outsourced_res_care+spend_res_care, data=pdata, method = "within", effect = "twoways"))
summary(plm(log(Winter.mortality.index..age.85.plus.+1)~outsourced_res_care+spend_res_care, data=pdata, method = "within", effect = "twoways"))
summary(plm(log(life_expectancy_Female)~outsourced_res_care+spend_res_care, data=pdata, method = "within", effect = "twoways"))
summary(plm(log(life_expectancy_Male)~outsourced_res_care+spend_res_care, data=pdata, method = "within", effect = "twoways"))


summary(plm(Emergency.hospital.admissions.due.to.falls.in.people.aged.80.plus~outsourced_home_care+spend_home_care, data=pdata, method = "within", effect = "twoways"))
summary(plm(Hip.fractures.in.people.aged.80.and.over~outsourced_home_care+spend_home_care, data=pdata, method = "within", effect = "twoways"))
summary(plm(Winter.mortality.index..age.85.plus.~outsourced_home_care+spend_home_care, data=pdata, method = "within", effect = "twoways"))
summary(plm(life_expectancy_Female~outsourced_home_care+spend_home_care, data=pdata, method = "within", effect = "twoways"))


summary(plm(Emergency.hospital.admissions.due.to.falls.in.people.aged.80.plus~outsourced_old_care+spend_old_care, data=pdata, method = "within", effect = "twoways"))
summary(plm(Hip.fractures.in.people.aged.80.and.over~outsourced_old_care+spend_old_care, data=pdata, method = "within", effect = "twoways"))
summary(plm(Winter.mortality.index..age.85.plus.~outsourced_old_care+spend_old_care, data=pdata, method = "within", effect = "twoways"))
summary(plm(life_expectancy_Female~outsourced_old_care+spend_old_care, data=pdata, method = "within", effect = "twoways"))




















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






library(dplyr)
library(ggplot2)
library(tidyr)

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
      filter((year == this_year & abs(week - this_week) <= 24) |
               (year == this_year - 1 & week > (52 - 24)) |
               (year == this_year + 1 & week <= 24))
    
    # Must have exactly 1 closure (the focal week) in this window
    sum(weeks_window$n_closures > 0) == 1
  }) %>%
  ungroup()

library(dplyr)
library(tidyr)
library(ggplot2)

# assume `matched_data` and `isolated_closures` already exist

# Step 3: Build the ±4-week windows and join
death_windows <- isolated_closures %>%
  mutate(week_offset = list(-24:24)) %>%
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
  ) %>%
  replace_na(list(v4_0 = 0, n_closures = 0))

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







# Method 4b: Population-adjusted version (if you have population data)
calculate_excess_rate_adjusted <- function(data, population_data = NULL) {
  if (!is.null(population_data)) {
    # Join with population data and calculate rates first
    data <- data %>%
      left_join(population_data, by = "DH_GEOGRAPHY_NAME") %>%
      mutate(death_rate = v4_0 / population * 100000)  # Deaths per 100k
    
    rate_var <- "death_rate"
  } else {
    # Use raw counts but flag the limitation
    message("Using raw counts - consider getting population data for rate adjustment")
    rate_var <- "v4_0"
  }
  
  # Calculate percentiles on the rate/count variable
  percentiles <- data %>%
    group_by(DH_GEOGRAPHY_NAME, week) %>%
    summarise(
      p25 = quantile(.data[[rate_var]], 0.25, na.rm = TRUE),
      p50 = quantile(.data[[rate_var]], 0.50, na.rm = TRUE),
      p75 = quantile(.data[[rate_var]], 0.75, na.rm = TRUE),
      .groups = "drop"
    )
  
  excess_data <- data %>%
    left_join(percentiles, by = c("DH_GEOGRAPHY_NAME", "week")) %>%
    mutate(
      baseline = p50,
      excess_absolute = .data[[rate_var]] - p50,
      excess_category = case_when(
        .data[[rate_var]] > p75 ~ "High",
        .data[[rate_var]] < p25 ~ "Low",
        TRUE ~ "Normal"
      )
    )
  
  return(excess_data)
}




try <- calculate_excess_rate_adjusted(matched_data)


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
      filter((year == this_year & abs(week - this_week) <= 12) |
               (year == this_year - 1 & week > (52 - 12)) |
               (year == this_year + 1 & week <= 12))
    
    # Must have exactly 1 closure (the focal week) in this window
    sum(weeks_window$n_closures > 0) == 1
  }) %>%
  ungroup()

library(dplyr)
library(tidyr)
library(ggplot2)

# assume `matched_data` and `isolated_closures` already exist

# Step 3: Build the ±4-week windows and join
death_windows <- isolated_closures %>%
  mutate(week_offset = list(-12:12)) %>%
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
    try,
    by = c(
      "locationlocalauthority",
      "target_year" = "year",
      "target_week"  = "week"
    )
  ) %>%
  replace_na(list(excess_absolute = 0, n_closures = 0))

# Step 4: Compute mean, standard error, and 95% CI
avg_deaths_by_offset <- death_windows %>%
  group_by(week_offset) %>%
  summarise(
    avg_deaths = mean(excess_absolute, na.rm = TRUE),
    sd_deaths  = sd(excess_absolute,   na.rm = TRUE),
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




















# Create event time variables for ALL closures
event_data <- panel_data %>%
  # Identify all closure events
  dplyr::filter(n_closures > 0) %>%
  dplyr::select(locationlocalauthority, year, week) %>%
  dplyr::rename(closure_year = year, closure_week = week, la_closure = locationlocalauthority) %>%
  # Cross join with full panel to create event time
  crossing(panel_data %>% dplyr::select(locationlocalauthority, year, week, v4_0)) %>%
  dplyr::filter(la_closure == locationlocalauthority) %>%
  mutate(
    event_time = (year - closure_year) * 52 + (week - closure_week),
    # Keep reasonable window
    event_time = ifelse(abs(event_time) > 26, NA, event_time)
  ) %>%
  filter(!is.na(event_time)) %>%
  # Average if multiple closures affect same LA-week
  group_by(locationlocalauthority, year, week) %>%
  summarise(
    v4_0 = first(v4_0),
    event_time = round(mean(event_time)),
    .groups = "drop"
  )


# Event study regression
summary(feols(v4_0 ~ i(event_time, ref = -1) + 
                       factor(week) + factor(year) |
                       locationlocalauthority,
                     data = event_data,
                     cluster = ~locationlocalauthority))













library(dplyr)
library(lubridate)

# Create a date column from year and week
matched_data <- matched_data %>%
  mutate(date = as.Date(paste0(year, "-W", sprintf("%02d", week), "-1"), format="%Y-W%U-%u"))

# Extract year-month
matched_data <- matched_data %>%
  mutate(month = floor_date(date, unit = "month"),
         week = floor_date(date, unit = "week")
  )

# Group by LA and month, then summarize
monthly_summary <- matched_data %>%
  group_by(DH_GEOGRAPHY_NAME, month) %>%
  summarise(
    total_closures = sum(n_closures, na.rm = TRUE),
    total_deaths = sum(v4_0, na.rm = TRUE),
    .groups = "drop"
  )

# View the result
print(monthly_summary)

summary(lm(total_deaths~total_closures+DH_GEOGRAPHY_NAME+factor(month), data=monthly_summary))


weekly <- matched_data %>%
  dplyr::select(week, DH_GEOGRAPHY_NAME, v4_0, n_closures, year)%>%
  dplyr::group_by(DH_GEOGRAPHY_NAME, year)%>%
  dplyr::arrange(week)%>%
  dplyr::mutate(lagged_closure = dplyr::lag(n_closures, 1),
                lead_closure = dplyr::lead(n_closures, 1),
                closure_3week = lagged_closure + n_closures + lead_closure
  )

summary(lm(v4_0~closure_3week+ DH_GEOGRAPHY_NAME+ as.factor(week), data=weekly%>%dplyr::filter(year==2022|year==2023)))








# Flag your treated windows
death_windows <- death_windows %>%
  mutate(treated = n_closures > 0)

# Then fit a simple DiD
library(fixest)
feols(v4_0 ~ treated*factor(week_offset) | locationlocalauthority + target_year, data=death_windows)

death_windows <- death_windows %>%
  mutate(week_offset_f = factor(week_offset, levels = -4:4)) %>%
  mutate(week_offset_f = relevel(week_offset_f, ref = "-1"))

feols(v4_0 ~ i(week_offset_f, treated, ref = -1) 
      | locationlocalauthority + target_year,
      data = death_windows, cluster = "locationlocalauthority")


#––– Libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(fixest)   # for feols if you want to check the regression later

#––– ASSUMPTION:
# matched_data: a data.frame/tibble with columns
#   locationlocalauthority, year, week, v4_0 (deaths), n_closures

#––– 1. Identify isolated closures (±4 weeks with no other closures)
closures_only <- matched_data %>%
  filter(n_closures > 0) %>%
  select(locationlocalauthority, year, week) %>%
  distinct()

isolated_closures <- closures_only %>%
  rowwise() %>%
  filter({
    la   <- locationlocalauthority
    yr   <- year
    wk   <- week
    window <- matched_data %>%
      filter(locationlocalauthority == la) %>%
      filter(
        (year == yr   & abs(week - wk) <= 4) |
          (year == yr-1 & week > 48)       |
          (year == yr+1 & week <= 4)
      )
    sum(window$n_closures > 0) == 1
  }) %>%
  ungroup()

#––– 2. Build ±4-week windows and join raw deaths & closures
death_windows <- isolated_closures %>%
  mutate(week_offset = list(-4:4)) %>%
  unnest(week_offset) %>%
  mutate(
    raw_target   = week + week_offset,
    target_year  = case_when(
      raw_target < 1   ~ year - 1,
      raw_target > 52  ~ year + 1,
      TRUE             ~ year
    ),
    target_week  = ((raw_target - 1) %% 52) + 1
  ) %>%
  left_join(
    matched_data,
    by = c(
      "locationlocalauthority",
      "target_year" = "year",
      "target_week"  = "week"
    )
  ) %>%
  replace_na(list(v4_0 = 0, n_closures = 0))

#––– 3. Compute calendar-week baseline of average deaths
baseline <- matched_data %>%
  group_by(week) %>%
  summarise(bg = mean(v4_0, na.rm = TRUE), .groups = "drop")

#––– 4. Merge baseline and compute adjusted deaths
death_windows <- death_windows %>%
  left_join(baseline, by = c("target_week" = "week")) %>%
  mutate(
    v4_adj = v4_0 - bg
  )

#––– 5. Aggregate adjusted deaths by week_offset, compute 95% CIs
avg_adj_by_offset <- death_windows %>%
  group_by(week_offset) %>%
  summarise(
    mean_adj = mean(v4_adj, na.rm = TRUE),
    sd_adj   = sd(v4_adj,   na.rm = TRUE),
    n_obs    = n(),
    se       = sd_adj / sqrt(n_obs),
    ci_lo    = mean_adj - qt(0.975, df = n_obs - 1) * se,
    ci_hi    = mean_adj + qt(0.975, df = n_obs - 1) * se,
    .groups  = "drop"
  )

#––– 6. Plot the adjusted event‐study with ribbon
ggplot(avg_adj_by_offset, aes(x = week_offset, y = mean_adj)) +
  geom_ribbon(aes(ymin = ci_lo, ymax = ci_hi), fill = "steelblue", alpha = 0.2) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
  labs(
    title    = "Anomalies in children home deaths around isolated carehome closures",
    x        = "Weeks relative to closure (0 = closure week)",
    y        = "Avg. adjusted deaths (v4_adj)"
  ) +
  theme_minimal()







library(dplyr)
library(tidyr)
library(zoo)
library(purrr)
library(MatchIt)
library(fixest)
library(ggplot2)
library(ISOweek)

library(dplyr)
library(tidyr)
library(zoo)
library(purrr)
library(MatchIt)
library(fixest)
library(ggplot2)
library(ISOweek)

# 0) Prepare your full panel with a 4-week lagged rolling mean
matched_data_dates <- matched_data %>%
  mutate(
    iso_w = paste0(year, "-W", sprintf("%02d", week), "-1"),
    date  = ISOweek2date(iso_w)
  ) %>%
  arrange(locationlocalauthority, date) %>%
  group_by(locationlocalauthority) %>%
  mutate(
    pre_mean = rollmean(lag(v4_0), k = 4, align = "right", fill = NA)
  ) %>%
  ungroup()

# 1) Build treated observations (week_offset == 0)
treated <- death_windows %>%
  filter(week_offset == 0) %>%
  transmute(
    event_id,
    locationlocalauthority,
    year    = target_year,
    week    = target_week,
    v4_0,
    treated = 1
  ) %>%
  left_join(
    matched_data_dates %>% select(locationlocalauthority, year, week, pre_mean),
    by = c("locationlocalauthority","year","week")
  ) %>%
  filter(!is.na(pre_mean))  # drop any treated rows missing pre_mean

# 2) Build forbidden LA-weeks (±4w of any closure)
forbidden <- isolated_closures %>%
  select(locationlocalauthority, year, base_week = week) %>%
  mutate(
    window = map(base_week, ~ ((.x + (-4:4) - 1) %% 52) + 1)
  ) %>%
  unnest(window) %>%
  transmute(
    locationlocalauthority,
    year,
    week = window
  )

# 3) Build controls: same calendar weeks, zero closures, outside forbidden, non-NA pre_mean
controls <- matched_data_dates %>%
  filter(
    n_closures == 0,
    week %in% treated$week   # same calendar weeks as treated
  ) %>%
  anti_join(forbidden, by = c("locationlocalauthority","year","week")) %>%
  filter(!is.na(pre_mean)) %>%  # drop any with missing pre_mean
  transmute(
    event_id    = NA_integer_,
    locationlocalauthority,
    year, week,
    v4_0,
    treated     = 0,
    pre_mean
  )

# 4) Combine and match 1:1 on pre_mean + calendar week
match_data <- bind_rows(treated, controls) %>%
  filter(!is.na(pre_mean))  # just in case

m.out <- matchit(
  treated ~ pre_mean + factor(week),
  data   = match_data,
  method = "nearest",
  ratio  = 1
)
matched_data_did <- match.data(m.out)

# 5) Estimate the DiD/event‐study
#    (If you want to plot leads/lags, you’ll need to add week_offset for controls.)
did_fit <- feols(
  v4_0 ~ treated | locationlocalauthority + year,
  data    = matched_data_did,
  cluster = "locationlocalauthority"
)
summary(did_fit)

# 6) (Optional) Plot simple ATT
ggplot(matched_data_did, aes(x = factor(treated), y = v4_0)) +
  stat_summary(fun = mean, geom = "bar") +
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = .2) +
  labs(x = "Treated (1=closure week)", y = "Average child deaths") +
  theme_minimal()







library(dplyr)
library(fixest)
library(ggplot2)

# ── Assume `matched_data_did` is the output of match.data(m.out),
#     and contains at least: locationlocalauthority, year, week, v4_0, treated, subclass

matched_es <- matched_data_did %>%
  group_by(subclass) %>%
  mutate(
    event_year = if (any(treated == 1)) year[treated == 1][1] else NA_integer_,
    event_week = if (any(treated == 1)) week[treated == 1][1] else NA_integer_
  ) %>%
  ungroup() %>%
  mutate(
    panel_week      = year * 52 + week,
    panel_eventweek = event_year * 52 + event_week,
    week_offset     = panel_week - panel_eventweek
  ) %>%
  filter(!is.na(week_offset)) %>%
  filter(between(week_offset, -8, 8))

# 4) Estimate the TWFE event‐study
es_fit <- feols(
  v4_0 ~ i(week_offset, treated, ref = -1) 
  | locationlocalauthority + year,
  data    = matched_es,
  cluster = "locationlocalauthority"
)

# 5) Extract the lead/lag coefficients
es_coefs <- etable(es_fit, keep = "treated::week_offset") %>% as.data.frame()

# Or with broom:
library(broom)
es_tidy <- tidy(es_fit, conf.int = TRUE) %>%
  filter(grepl("^treated::week_offset::", term)) %>%
  mutate(
    offset = as.integer(gsub(".*week_offset::(-?\\d+).*", "\\1", term))
  )

# 6) Plot the event‐study curve
ggplot(es_tidy, aes(x = offset, y = estimate)) +
  geom_point() +
  geom_line() +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
  labs(
    title = "Event‐Study: Child Deaths Around Care‐Home Closure",
    subtitle = "Matched DiD, weeks relative to closure",
    x = "Weeks from closure (0 = closure week)",
    y = "Difference in deaths"
  ) +
  theme_minimal()











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
