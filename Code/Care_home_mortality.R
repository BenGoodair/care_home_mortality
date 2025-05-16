####Analysis 1 = Ecological####

if (!require("pacman")) install.packages("pacman")

pacman::p_load(devtools,np,lazyeval, hmisc,interp, lmtest,gt, modelsummary, dplyr,pdftools, tidyverse,rattle,glmnet,caret, rpart.plot, RcolorBrewer,rpart, tidyr, mice, stringr,randomForest,  curl, plm, readxl, zoo, stringr, patchwork,  sf, clubSandwich, modelsummary, sjPlot)


outsourced <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/adults_social_care_data/refs/heads/main/activity.csv"))

lifeexpectancy <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/care_home_mortality/refs/heads/main/Data/lifeexpectancylocalareas.csv"), skip=5)%>%
  dplyr::filter(Area.type == "County"|
                  Area.type == "Local Areas",
                Age.band == 17)%>%
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
                  str_trim())


data = dplyr::full_join(outsourced %>%
                          dplyr::mutate(DH_GEOGRAPHY_NAME = str_trim(gsub("[0-9]", "", DH_GEOGRAPHY_NAME))), lifeexpectancy, by=c("DH_GEOGRAPHY_NAME", "year"))%>%
  tidyr::drop_na(Life.expectancy..years., percent_sector)%>%
  dplyr::filter(DH_GEOGRAPHY_NAME!= "BOURNEMOUTH CHRISTCHURCH AND POOLE",
                DH_GEOGRAPHY_NAME!= "BEDFORD",
                DH_GEOGRAPHY_NAME!= "CENTRAL BEDFORDSHIRE",
                DH_GEOGRAPHY_NAME!= "CHESHIRE EAST",
                DH_GEOGRAPHY_NAME!= "CHESHIRE WEST AND CHESTER",
                DH_GEOGRAPHY_NAME!= "MEDWAY",
                DH_GEOGRAPHY_NAME!= "BEXLEY",
                DH_GEOGRAPHY_NAME!= "CORNWALL",
                DH_GEOGRAPHY_NAME!= "KNOWSLEY",
                DH_GEOGRAPHY_NAME!= "HERTFORDSHIRE",
                DH_GEOGRAPHY_NAME!= "SOUTHWARK")%>%
  dplyr::select(-Sex.code)%>%
  tidyr::pivot_wider(names_from = "Sex", values_from = c("Life.expectancy..years.", "Lower.confidence.interval","Upper.confidence.interval"))%>%
  dplyr::group_by(DH_GEOGRAPHY_NAME)%>%
  dplyr::mutate(lagged_sector_1 = dplyr::lag(percent_sector, 1),
                lagged_sector_2 = dplyr::lag(percent_sector, 2),
                lagged_le_1_Male = dplyr::lag(Life.expectancy..years._Male, 1),
                lagged_le_1_Female = dplyr::lag(Life.expectancy..years._Female, 1))


duplicated_rows <- data %>% 
  dplyr::group_by(DH_GEOGRAPHY_NAME, year) %>%
  dplyr::filter(n() > 1)

summary(plm(log(Life.expectancy..years._Male)~ lagged_sector_1+lagged_le_1_Male, data= data%>%dplyr::filter(year<2020), index = c("DH_GEOGRAPHY_NAME", "year"), effect = "twoways", model = "within"))
summary(plm(log(Life.expectancy..years._Female)~ lagged_sector_1+lagged_le_1_Female, data= data%>%dplyr::filter(year<2020), index = c("DH_GEOGRAPHY_NAME", "year"), effect = "twoways", model = "within"))







data <- pdata.frame(data, index = c("DH_GEOGRAPHY_NAME","year"))

# set your desired lag order
lag_order <- 1

# compute minimum length threshold
min_length <- 5 + 3 * lag_order




# (b) Define your two variables
data$logLE75         <- log(data$Life.expectancy..years.)
data$outsourced_pct  <- data$percent_sector

bad_variation <- data %>%
  group_by(DH_GEOGRAPHY_NAME) %>%
  summarise(
    var_le75 = var(logLE75, na.rm = TRUE),
    var_out  = var(outsourced_pct, na.rm = TRUE)
  ) %>%
  filter(var_le75 == 0 | var_out == 0) 




# (c) Panel Granger test for lags = 1…and see if past outsourcing “Granger‐causes” logLE75
for(l in 1:3) {
    pgrangertest(logLE75 ~ outsourced_pct, order = l, data = data)

}






