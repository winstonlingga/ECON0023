install.packages("dplyr")
install.packages("readr")
install.packages("ggplot2")
install.packages("modelsummary")
install.packages("tidyverse")
install.packages("sf")
install.packages("eurostat")
install.packages("giscoR")
install.packages("viridis")
install.packages("kableExtra")
library(kableExtra)
library(viridis)
library(giscoR)
library(tidyverse)
library(sf)
library(eurostat)
library(modelsummary)
library(dplyr)
library(readr)
library(ggplot2)

'''
nuts2_map <- data.frame(
  RegionName = c(
    "Piemonte",
    "Valle d’Aosta/Vallée d’Aoste",
    "Liguria",
    "Lombardia",
    "Provincia Autonoma di Bolzano/Bozen",
    "Provincia Autonoma di Trento",
    "Veneto",
    "Friuli-Venezia Giulia",
    "Emilia-Romagna",
    "Toscana",
    "Umbria",
    "Marche",
    "Lazio",
    "Abruzzo",
    "Molise",
    "Campania",
    "Puglia",
    "Basilicata",
    "Calabria",
    "Sicilia",
    "Sardegna"
  ),
  territory_code = c(
    "ITC1",     # Piemonte
    "ITC2",     # Valle d'Aosta
    "ITC3",     # Liguria
    "ITC4",     # Lombardia
    "ITH1",     # Bolzano-Bozen  
    "ITH2",     # Trento          
    "ITH3",     # Veneto          
    "ITH4",     # Friuli-VG       
    "ITH5",     # Emilia-Romagna  
    "ITI1",     # Toscana         
    "ITI2",     # Umbria          
    "ITI3",     # Marche         
    "ITI4",     # Lazio           
    "ITF1",     # Abruzzo
    "ITF2",     # Molise
    "ITF3",     # Campania
    "ITF4",     # Puglia
    "ITF5",     # Basilicata
    "ITF6",     # Calabria
    "ITG1",     # Sicilia
    "ITG2"      # Sardegna
  )
)
nuts21_to_16 <- tibble::tribble(
  ~old,  ~new,
  "ITD1","ITH1",
  "ITD2","ITH2",
  "ITD3","ITH3",
  "ITD4","ITH4",
  "ITD5","ITH5",
  "ITE1","ITI1",
  "ITE2","ITI2",
  "ITE3","ITI3",
  "ITE4","ITI4"
)
'''


#LOAD final dataset
final <- read_csv("~/Desktop/ECON0023/final_dataset_with_HPI_AND_clean_logs.csv")
final$territory_code <- as.character(final$territory_code)
final$Region <- as.character(final$Region)
final$Year <- as.numeric(final$Year)


# Tourism: correct mapping using NUTS 2016

nuts2_2016 <- get_eurostat_geospatial(nuts_level = 2, year = 2016)

nuts_lookup <- nuts2_2016 %>%
  filter(CNTR_CODE == "IT") %>%
  st_drop_geometry() %>%
  select(territory_code = NUTS_ID, RegionName = NAME_LATN)

nuts_lookup
unique(nuts_lookup$RegionName)

tour_clean <- tour %>%
  rename(
    RegionName     = geo,
    Year           = TIME_PERIOD,
    foreign_nights = OBS_VALUE
  ) %>%
  mutate(Year = as.numeric(Year)) %>%
  left_join(nuts2_map, by = "RegionName") %>%
  filter(!is.na(territory_code))

# Add population (ensure it matches territory_code + Year)
tour_clean <- tour_clean %>%
  left_join(
    final %>% select(territory_code, Year, population),
    by = c("territory_code", "Year")
  ) %>%
  mutate(foreign_pc = foreign_nights / population)

# Pre-COVID average
tour_avg <- tour_clean %>%
  group_by(territory_code) %>%
  summarise(foreign_pc_avg = mean(foreign_pc, na.rm = TRUE))


#Clean crime data
crime <- read_csv("~/Desktop/ECON0023/Crime rate2.csv")

crime_clean <- crime_clean %>%
  left_join(nuts21_to_16, by = c("territory_code" = "old")) %>%
  mutate(territory_code = ifelse(is.na(new), territory_code, new)) %>%
  select(-new)


# Average crime per region
crime_avg <- crime_clean %>%
  group_by(territory_code) %>%
  summarise(crime_avg = mean(crime_rate, na.rm = TRUE))



#Coastal dummy
coastal_regions <- c(
  "ITC3",        # Liguria
  "ITH3", "ITH4", "ITH5",   # Veneto, FVG, Emilia-Romagna
  "ITI1", "ITI3", "ITI4",   # Toscana, Marche, Lazio
  "ITF1","ITF2","ITF3","ITF4","ITF5","ITF6",  # Entire South except inland Basilicata? (Still coastal)
  "ITG1", "ITG2"            # Sicilia, Sardegna
)


coast <- data.frame(
  territory_code = unique(final$territory_code)
) %>%
  mutate(
    coastal_dummy = ifelse(territory_code %in% coastal_regions, 1, 0)
  )
'''


'''
#merge all amenity components
amenity <- tour_avg %>%
  left_join(crime_avg, by = "territory_code") %>%
  left_join(coast, by = "territory_code") %>%
  mutate(
    z_tourism = as.numeric(scale(foreign_pc_avg)),
    z_crime   = as.numeric(scale(crime_avg)),
    AmenityIndex = z_tourism - z_crime + coastal_dummy
  )

amenity %>% 
  summarise(
    n_regions = n_distinct(territory_code),
    n_missing_index = sum(is.na(AmenityIndex))
  )
#diagnostics
amenity_diag <- amenity %>%
  mutate(
    miss_tour  = is.na(foreign_pc_avg),
    miss_crime = is.na(crime_avg),
    miss_coast = is.na(coastal_dummy)
  ) %>%
  filter(is.na(AmenityIndex)) %>%
  select(territory_code, miss_tour, miss_crime, miss_coast,
         foreign_pc_avg, crime_avg, coastal_dummy)

amenity_diag

# Check population coverage for problematic regions
problem_codes <- c("ITC2","ITH1","ITH2","ITH3","ITH4","ITH5","ITI1","ITI2","ITI3","ITI4")
final <- final %>%
  left_join(nuts21_to_16, by = c("territory_code" = "old")) %>%
  mutate(territory_code = ifelse(is.na(new), territory_code, new)) %>%
  select(-new)

unique(final$territory_code)
unique(crime_clean$territory_code)


amenity %>% filter(is.na(AmenityIndex)) %>% select(territory_code)
unique(tour$geo)
tour_clean %>% filter(is.na(territory_code)) %>% distinct(RegionName)
amenity %>% 
  summarise(
    n_regions = n_distinct(territory_code),
    n_missing_index = sum(is.na(AmenityIndex))
  )


final2 <- final %>%
  left_join(amenity, by = "territory_code") %>%
  mutate(Post = ifelse(Year >= 2017, 1, 0))

#For chloropleth map
italy_sf <- gisco_get_nuts(
  year = "2016",
  nuts_level = 2,
  country = "ITA"
)
map_df <- italy_sf %>%
  left_join(final2, by = c("NUTS_ID" = "territory_code"))

# Save final dataset
write.csv(final2, "final_dataset_with_AmenityIndex.csv", row.names = FALSE)



#define Post indicator (treatment period)
final2 <- final2 %>%
  mutate(Post = ifelse(Year >= 2017, 1, 0))
'''

# 1. Load your new, fully-cleaned dataset
final2 <- read_csv("~/Desktop/ECON0023/final_dataset_with_AmenityIndex.csv")

# 2. Time-invariant amenity index (z-scores from 2015-2019 averages)
final2 <- final2 %>%
  mutate(
    z_tourism    = as.numeric(scale(foreign_pc_avg)),
    z_crime      = as.numeric(scale(crime_avg)),
    AmenityIndex = z_tourism - z_crime + coastal_dummy,
    Post = ifelse(Year >= 2017, 1, 0)
  )

#Sanity Checks!!!
final2 %>% distinct(territory_code) %>% nrow()
range(final2$Year)
table(final2$Year)
colSums(is.na(final2[, c("AmenityIndex", "log_HPI_average",
                         "Observation_Income_Percentage", "Post")]))
summary(final2$AmenityIndex)
hist(final2$AmenityIndex)

#Save final dataset
write_csv(final2, "final_dataset_for_analysis.csv")

'''
Pre-Treatment (2011-2016) Checks
'''
final2 <- final2 %>%
  mutate(high_amenity = AmenityIndex > median(AmenityIndex))

pretrend <- final2 %>%
  filter(Year < 2017) %>%
  group_by(Year, high_amenity) %>%
  summarise(mean_hpi = mean(log_HPI_average), .groups = "drop")
#Line Plots
ggplot(pretrend, aes(x=Year, y=mean_hpi, color=high_amenity)) +
  geom_line(size=1.1) +
  labs(title="Pre-Treatment HPI Trends by Amenity Level")

pretreat <- final2 %>% filter(Year < 2017)

pretrend_reg <- feols(
  log_HPI_average ~ AmenityIndex:Year | Region + Year,
  cluster = ~Region,
  data = pretreat
)

summary(pretrend_reg)

#Summary Statistics
datasummary_skim(final2) %>%
  kableExtra::save_kable("summary_stats.tex")
#for google docs:
datasummary_skim(final2) %>%
  kableExtra::save_kable("summary_stats.html")

#Save table


#HPI Spaghetti Plot
ggplot(final2, aes(Year, log_HPI_average, group=territory_code)) +
  geom_line(alpha = 0.4) +
  geom_vline(xintercept=2017, linetype="dashed") +
  labs(title="Regional Housing Price Index Over Time")

#Amenity vs Pre-Period Scatter
pre2017 <- final2 %>% filter(Year < 2017)

ggplot(pre2017, aes(AmenityIndex, log_HPI_average)) +
  geom_point() +
  geom_smooth(method="lm") +
  labs(title="Amenity vs Pre-Treatment Housing Prices")

'''
Difference-in-Differences (continuous and inequality)
'''

#continuous DiD
install.packages("fixest")
library(fixest)

did_hpi <- feols(
  log_HPI_average ~ AmenityIndex * Post | Region + Year,
  data = final2,
  cluster = ~Region
)
summary(did_hpi)

etable(did_hpi,
       dict = c("AmenityIndex:Post" = "AmenityIndex × Post"),
       tex = TRUE,
       file = "table2_hpi.tex")


#Inequality regression
did_ineq <- feols(
  Observation_Income_Percentage ~ AmenityIndex * Post | Region + Year,
  data = final2,
  cluster = ~Region
)
summary(did_ineq)


etable(did_ineq,
       dict = c("AmenityIndex:Post" = "AmenityIndex × Post"),
       tex = TRUE,
       file = "table3_ineq.tex")
getwd()
list.files()

#Save Regression Results
modelsummary(
  list(
    "HPI Regression" = did_hpi,
    "Inequality Regression" = did_ineq
  ),
  output = "did_results.tex",
  stars = TRUE,
  fmt = 3,
  gof_omit = "IC|Log|Within|Pseudo"
)

#Again for Docs
modelsummary(
  list(
    "HPI Regression" = did_hpi,
    "Inequality Regression" = did_ineq
  ),
  output = "did_results.html",
  stars = TRUE,
  fmt = 3,
  gof_omit = "IC|Log|Within|Pseudo"
)


# Event Study (Continuous Specification)
final2 <- final2 %>%
  mutate(rel_year = Year - 2017)

event_hpi <- feols(
  log_HPI_average ~ 
    i(rel_year, AmenityIndex, ref = -1) | Region + Year,
  data = final2,
  cluster = "Region"
)

iplot(event_hpi)

#Pre-Trend Check 1 - High vs Low Amenity HPI Trends (2015-2016)

#define high amenity and low amenity groups
final2 <- final2 %>%
  mutate(high_amenity = AmenityIndex > median(AmenityIndex, na.rm = TRUE))
pretrend <- final2 %>%
  group_by(Year, high_amenity) %>%
  summarise(mean_hpi = mean(log_HPI_average, na.rm = TRUE))

#plot
ggplot(pretrend, aes(x = Year, y = mean_hpi, color = high_amenity)) +
  geom_line(size = 1.1) +
  geom_vline(xintercept = 2017, linetype = "dashed") +
  labs(title = "Average Housing Prices in High vs Low-Amenity Regions",
       subtitle = "Pre-Trends (2015–2016) and Post-Policy (2017–2024)")

#Pre-Trend Check 2 - Regression Test for Pre-Treatment Differential Trends
#Restrict sample
pretreat <- final2 %>% filter(Year < 2017)
pretrend_reg <- feols(
  log_HPI_average ~ AmenityIndex:Year | Region + Year,
  data = pretreat,
  cluster = ~Region
)
summary(pretrend_reg)

'''
Descriptive Statistics:
Mandatory:
- Choropleth map of Amenity Index
- Pre-trend plot (above)
- Distribution of Amenity Index
- Housing price spaghetti plot
- Non-dom uptake plot (if available)
Highly recommended:
- Scatterplot of amenity vs pre-trend HPI
'''

# Figure 1 Choropleth map of Amenity Index

nuts2 <- get_eurostat_geospatial(nuts_level = "2", year = 2013)

map_df <- nuts2 %>%
  filter(CNTR_CODE == "IT") %>%
  left_join(
    final2 %>% select(territory_code, AmenityIndex) %>% distinct(),
    by = c("NUTS_ID" = "territory_code")
  )

ggplot(map_df) +
  geom_sf(aes(fill = AmenityIndex)) +
  scale_fill_viridis_c(option="magma") +
  theme_minimal() +
  labs(title = "Amenity Index Across Italian Regions (NUTS2)")

amenity %>% arrange(desc(AmenityIndex)) %>% select(territory_code, AmenityIndex)

#Figure 2 Distribution of Amenity Index
ggplot(final2, aes(x = AmenityIndex)) +
  geom_histogram(bins = 15, fill = "steelblue", color = "white") +
  labs(title = "Distribution of Amenity Index",
       x = "Amenity Index", y = "Count") +
  theme_minimal()

#Figure 4 Housing price spaghetti plot (still need to add legends)
ggplot(final2, aes(x = Year, y = log_HPI_average, group = Region, color = Region)) +
  geom_line(alpha = 0.6) +
  geom_vline(xintercept = 2017, linetype = "dashed") +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(title = "Housing Price Trends Across Italian Regions",
       subtitle = "Visual inspection of dynamics surrounding the 2017 reform",
       x = "Year", y = "log(HPI)")

#Figure 5 Scatterplot of amenity vs pre-trend HPI (incomplete)
pre_growth <- final2 %>%
  filter(Year < 2017) %>%
  group_by(Region) %>%
  summarise(pre_growth = mean(log_HPI_average)) %>%
  left_join(final2 %>% distinct(Region, AmenityIndex))

ggplot(pre_growth, aes(x = AmenityIndex, y = pre_growth)) +
  geom_point(size = 3, color = "darkblue") +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Relationship Between Amenities and Pre-Reform Housing Prices",
       x = "Amenity Index", y = "Average log(HPI), 2015–2016") +
  theme_minimal()
