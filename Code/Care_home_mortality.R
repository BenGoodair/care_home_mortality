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
  dplyr::full_join(., deaths)%>%
  dplyr::group_by(DH_GEOGRAPHY_NAME)%>%
  arrange(year) %>%
  dplyr::mutate(lagged_sector_1 = dplyr::lag(percent_sector, 1),
                lagged_sector_2 = dplyr::lag(percent_sector, 2),
                lagged_le_1_Male = dplyr::lag(Life.expectancy..years._Male, 1),
                lagged_le_1_Female = dplyr::lag(Life.expectancy..years._Female, 1),
                lagged_deaths.All.care.homes = dplyr::lag(deaths.All.care.homes, 1))
  
  


duplicated_rows <- data %>% 
  dplyr::group_by(DH_GEOGRAPHY_NAME, year) %>%
  dplyr::filter(n() > 1)



summary(plm(log(Life.expectancy..years._Male)~ percent_sector+lagged_sector_1+lagged_sector_2+lagged_le_1_Male, data= data, index = c("DH_GEOGRAPHY_NAME", "year"), effect = "twoways", model = "within"))
summary(plm(log(Life.expectancy..years._Female)~  percent_sector+lagged_sector_1+lagged_sector_2+lagged_le_1_Female, data= data, index = c("DH_GEOGRAPHY_NAME", "year"), effect = "twoways", model = "within"))
summary(plm(log(deaths.All.care.homes)~ percent_sector+lagged_sector_1+lagged_sector_2+lagged_deaths.All.care.homes, data= data, index = c("DH_GEOGRAPHY_NAME", "year"), effect = "twoways", model = "within"))

library(plm)
library(dplyr)



pdata <- pdata.frame(data%>%
                      dplyr::select(Life.expectancy..years._Female, Life.expectancy..years._Male, percent_sector,DH_GEOGRAPHY_NAME, year)%>%
                      tidyr::drop_na(), index = c("DH_GEOGRAPHY_NAME","year"))


# List panels with incomplete time-series
incomplete <- pdata %>% 
  pdata.frame(index = c("DH_GEOGRAPHY_NAME","year")) %>% 
  index() %>% 
  as.data.frame() %>%
  group_by(DH_GEOGRAPHY_NAME) %>%
  dplyr::mutate(year = as.numeric(as.character(year)))%>%
  summarize(n_years = n(),
            span = max(year) - min(year) + 1) %>%
  filter(n_years != span)


good_regions <- setdiff(unique(pdata$DH_GEOGRAPHY_NAME), incomplete$DH_GEOGRAPHY_NAME)
pdata_bal <- subset(pdata, DH_GEOGRAPHY_NAME %in% good_regions)

fwd <- pgrangertest(Life.expectancy..years._Female ~ percent_sector,
                    order = 1, data = pdata_bal)
print(fwd)


fwd <- pgrangertest(Life.expectancy..years._Male ~ percent_sector,
                    order = 1, data = pdata_bal)
print(fwd)



library(dplyr)

# Suppose pdata_rev is your current “almost‐clean” data
# Loop over the lags you’ll test:
for(L in 1:3) {
  # Count observations per region
  obs_summary <- pdata_rev %>%
    group_by(DH_GEOGRAPHY_NAME) %>%
    summarize(n_obs = sum(!is.na(percent_sector) & !is.na(Life.expectancy..years._Female))) 
  
  # Find units with too few obs for lag L
  bad_units <- obs_summary %>%
    filter(n_obs <= L) %>%
    pull(DH_GEOGRAPHY_NAME)
  
  cat("Lag =", L, " – dropping", length(bad_units), "units:\n")
  print(bad_units)
  
  # Drop them
  pdata_rev <- filter(pdata_rev, !DH_GEOGRAPHY_NAME %in% bad_units)
}





rev <- pgrangertest(percent_sector ~ Life.expectancy..years._Female,
                    order = 1, data = pdata_rev)
print(rev)

for(l in 1:3) {
  cat("Lag =", l, "\n")
  print(pgrangertest(Life.expectancy..years._Female ~ percent_sector,
                     order = l, data = pdata_rev))
}



pgrangertest(Life.expectancy..years._Female ~ percent_sector, order = 1, data = pdata)
pgrangertest(percent_sector ~ Life.expectancy..years._Female, order = 1, data = pdata)
pgrangertest(Life.expectancy..years._Female ~ percent_sector, order = 2, data = pdata)








library(dplyr)
library(plm)
library(broom)

unit_coefs <- pdata_bal %>%
  dplyr::rename(region = DH_GEOGRAPHY_NAME)%>%
  # make sure pdata_bal is a data.frame with columns region, year, Life.expectancy..years._Female, percent_sector
  group_by(region) %>%
  arrange(year) %>%
  nest() %>%
  dplyr::mutate(
    fit = map(data, ~ {
      df <- .x
      # at least 2 lags + minimal obs
      if(nrow(df) >= 3) {
        tryCatch(
          plm(Life.expectancy..years._Female ~ lag(percent_sector,1),
              data = df, index = c("region","year"),
              model = "within", effect = "twoways"),
          error = function(e) NULL
        )
      }
    }),
    tidy = map(fit, ~ if(!is.null(.x)) tidy(.x) else tibble(term=NA, estimate=NA, std.error=NA, p.value=NA))
  ) %>%
  unnest(tidy) %>%
  filter(term == "lag(percent_sector, 1)") %>%
  select(region, estimate, std.error, p.value)

# View the region-specific βᵢ
print(unit_coefs)




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
