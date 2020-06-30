#' ---
#' title: "organize_data.R"
#' author: "Nakiessa M. Abbassi"
#' ---

# This script will read in raw data from the input directory, clean it up to produce 
# the analytical dataset, and then write the analytical data to the output directory. 

#source in any useful functions
source("check_packages.R")
source("useful_functions.R")

#load CES data
ces_data <- read_excel("input/ces3results.xlsx", na="NA")

#load CES demogrqphic data
dem_data <- read_excel("input/ces_demographics.xlsx", na="NA")
colnames(dem_data)
new_dem <- dem_data %>%
  select(`Census Tract`, `Hispanic (%)`, `White (%)`, `African American (%)`, `Native American (%)`,
         `Asian American (%)`, `Other (%)`)             

new_dem <- new_dem %>%
  rename(`census_tract` = `Census Tract`,
         `hispanic` = `Hispanic (%)`,
         `white` = `White (%)`, 
         `african_american` = `African American (%)`,
         `native_american` = `Native American (%)`,
         `asian_american` = `Asian American (%)`,
         `other` = `Other (%)`)

#load Social Explorer data
  se_data <- read_csv("input/R12557583_SL140 (2).csv", na="NA", skip=1)
  se_data$Geo_FIPS <- as.numeric(se_data$Geo_FIPS)

#selecting and renaming variables in ces data
colnames(ces_data)
new_ces <- ces_data %>%
  select(`Census Tract`, `Total Population`, `California County`, `CES 3.0 Score`, `Ozone`, `PM2.5`, `Diesel PM`,
         `Drinking Water`, `Pesticides`, `Tox. Release`, `Traffic`, `Pollution Burden`, `Pollution Burden Score`, 
         `Imp. Water Bodies`, `Asthma`, `Low Birth Weight`, `Cardiovascular Disease`, `Education`,
         `Linguistic Isolation`, `Poverty`, `Unemployment`, `Housing Burden`, `Pop. Char.`, `Pop. Char. Score`, 
         `Nearby City`)

new_ces <- new_ces %>% 
  rename(`census_tract` = `Census Tract`,
         `county` = `California County`,
         `total_pop` = `Total Population`, 
         `ces_score` = `CES 3.0 Score`, 
         `ozone` = `Ozone`,
         `pm_2.5` = `PM2.5`, 
         `diesel_pm` = `Diesel PM`,
         `drinking_water` = `Drinking Water`, 
         `pesticides` = `Pesticides`, 
         `tox_release` = `Tox. Release`, 
         `traffic` = `Traffic`,
         `pol_burden` = `Pollution Burden`, 
         `pol_burden_score` = `Pollution Burden Score`,
         `imp_water` = `Imp. Water Bodies`, 
         `asthma` = `Asthma`, 
         `low_birth` = `Low Birth Weight`, 
         `card_disease` = `Cardiovascular Disease`, 
         `education` = `Education`,
         `ling_isolation` = `Linguistic Isolation`, 
         `poverty` = `Poverty`, 
         `unemploy` = `Unemployment`, 
         `housing_burden` = `Housing Burden`, 
         `pop_char` = `Pop. Char.`, 
         `pop_char_score` = `Pop. Char. Score`,
         `city` = `Nearby City`)

colnames(new_ces)
colnames(se_data)

#selecting and renaming variables in se data 
new_se <- se_data %>%
  select("Geo_FIPS", "SE_A17004_001", "SE_A17004_002") 

new_se <- new_se %>% 
  rename(`census_tract` = `Geo_FIPS`,
         `tot_employ` = `SE_A17004_001`,
         `ag_employ` = `SE_A17004_002`)
colnames(new_se)

#merge ces and dem data and then add se data
merge_ces <- merge(new_ces, new_dem, by="census_tract", all.x=TRUE, all.y=FALSE)
colnames(merge_ces)

merge_data <- merge(merge_ces, new_se, by="census_tract", all.x=TRUE, all.y=FALSE)
merge_data$ag_pct <- (merge_data$ag_employ/merge_data$tot_employ)*100
colnames(merge_data)

#selecting only cities within ventura
merge_data <- filter(merge_data, county %in%
                          c("Ventura"))
merge_data <- subset(merge_data, merge_data$total_pop>70)


#save full merge data in output directory
save(merge_data, file="output/merge_data.RData")

#gotta test
ggplot(merge_data, aes(x=reorder(city, ces_score, median, na.rm=TRUE),
                        y=ces_score))+
  geom_boxplot(fill="cornflowerblue", outlier.color = "tan2")+
  labs(x="Farmworker Community in California", 
       y="CES 3.0 Score", 
       title="CES 3.0 Score by Farmworker Community in California",
       fill="test")+
  theme_bw()+
  coord_flip()

