# 1. Libraries
#================================================================================================================================================================
library(tidyverse)
library(lubridate)

# Print session info for debugging
cat("R Session Info:\n")
sessionInfo()
#================================================================================================================================================================
# 2. Create directories if they don't exist
#================================================================================================================================================================
dir.create("data/processed", recursive = TRUE, showWarnings = FALSE)
#================================================================================================================================================================
# 3. Geographic Mapping Data with hierarchy levels
#================================================================================================================================================================
geo_mapping <- tribble(
  ~GeoUID, ~Province_City, ~Geographic_Level, ~Region, ~latitude, ~longitude, ~hierarchy_level,
  # National
  "CAN", "Canada", "National", "National", 56.1304, -106.3468, 1,
  
  # Provinces and Territories (hierarchy_level = 2)
  "10", "Newfoundland and Labrador", "Province", "Atlantic", 53.1355, -57.6604, 2,
  "11", "Prince Edward Island", "Province", "Atlantic", 46.5107, -63.4168, 2,
  "12", "Nova Scotia", "Province", "Atlantic", 44.6820, -63.7443, 2,
  "13", "New Brunswick", "Province", "Atlantic", 46.5653, -66.4619, 2,
  "24", "Quebec", "Province", "Central", 46.8139, -71.2080, 2,
  "35", "Ontario", "Province", "Central", 51.2538, -85.3232, 2,
  "46", "Manitoba", "Province", "Prairie", 53.7609, -98.8139, 2,
  "47", "Saskatchewan", "Province", "Prairie", 52.9399, -106.4509, 2,
  "48", "Alberta", "Province", "Prairie", 53.9333, -116.5765, 2,
  "59", "British Columbia", "Province", "West Coast", 53.7267, -127.6476, 2,
  "60", "Yukon", "Territory", "Northern", 64.2823, -135.0000, 2,
  "61", "Northwest Territories", "Territory", "Northern", 64.8255, -124.8457, 2,
  "62", "Nunavut", "Territory", "Northern", 70.2998, -83.1076, 2,
  
  # Ottawa-Gatineau Components (hierarchy_level = 3)
  "35505", "Ottawa", "City", "Central", 45.4215, -75.6972, 3,
  "24505", "Gatineau", "City", "Central", 45.4765, -75.7013, 3,
  
  # Major Cities (hierarchy_level = 3)
  "10001", "St. John's", "City", "Atlantic", 47.5615, -52.7126, 3,
  "12205", "Halifax", "City", "Atlantic", 44.6488, -63.5752, 3,
  "13305", "Moncton", "City", "Atlantic", 46.0878, -64.7782, 3,
  "13310", "Saint John", "City", "Atlantic", 45.2733, -66.0633, 3,
  "13320", "Fredericton", "City", "Atlantic", 45.9636, -66.6431, 3,
  "24408", "Saguenay", "City", "Central", 48.4279, -71.0485, 3,
  "24421", "Quebec City", "City", "Central", 46.8139, -71.2080, 3,
  "24433", "Sherbrooke", "City", "Central", 45.4040, -71.8929, 3,
  "24442", "Trois-Rivières", "City", "Central", 46.3432, -72.5429, 3,
  "24447", "Drummondville", "City", "Central", 45.8833, -72.4824, 3,
  "24462", "Montreal", "City", "Central", 45.5017, -73.5673, 3,
  "35521", "Kingston", "City", "Central", 44.2312, -76.4860, 3,
  "35522", "Belleville", "City", "Central", 44.1628, -77.3832, 3,
  "35529", "Peterborough", "City", "Central", 44.3091, -78.3197, 3,
  "35535", "Toronto", "City", "Central", 43.6532, -79.3832, 3,
  "35537", "Hamilton", "City", "Central", 43.2557, -79.8711, 3,
  "35539", "St. Catharines-Niagara", "City", "Central", 43.1594, -79.2469, 3,
  "35541", "Kitchener-Cambridge-Waterloo", "City", "Central", 43.4516, -80.4925, 3,
  "35543", "Brantford", "City", "Central", 43.1394, -80.2644, 3,
  "35550", "Guelph", "City", "Central", 43.5449, -80.2482, 3,
  "35555", "London", "City", "Central", 42.9849, -81.2453, 3,
  "35559", "Windsor", "City", "Central", 42.3149, -83.0364, 3,
  "35568", "Barrie", "City", "Central", 44.3894, -79.6903, 3,
  "35580", "Greater Sudbury", "City", "Central", 46.4917, -80.9930, 3,
  "35595", "Thunder Bay", "City", "Central", 48.3809, -89.2477, 3,
  "46602", "Winnipeg", "City", "Prairie", 49.8951, -97.1384, 3,
  "47705", "Regina", "City", "Prairie", 50.4452, -104.6189, 3,
  "47725", "Saskatoon", "City", "Prairie", 52.1332, -106.6700, 3,
  "48810", "Lethbridge", "City", "Prairie", 49.6956, -112.8451, 3,
  "48825", "Calgary", "City", "Prairie", 51.0447, -114.0719, 3,
  "48830", "Red Deer", "City", "Prairie", 52.2690, -113.8116, 3,
  "48835", "Edmonton", "City", "Prairie", 53.5461, -113.4938, 3,
  "59915", "Kelowna", "City", "West Coast", 49.8880, -119.4960, 3,
  "59925", "Kamloops", "City", "West Coast", 50.6745, -120.3273, 3,
  "59930", "Chilliwack", "City", "West Coast", 49.1579, -121.9514, 3,
  "59932", "Abbotsford-Mission", "City", "West Coast", 49.0504, -122.3045, 3,
  "59933", "Vancouver", "City", "West Coast", 49.2827, -123.1207, 3,
  "59935", "Victoria", "City", "West Coast", 48.4284, -123.3656, 3,
  "59938", "Nanaimo", "City", "West Coast", 49.1659, -123.9401, 3,
  
  # Special Administrative (hierarchy_level = 4)
  "99001", "Canadian Forces Military Police", "Special", "National", 45.4215, -75.6972, 4
)

# Basic utility functions
is_empty_column <- function(x) {
  all(is.na(x)) || all(x == "") || all(is.null(x))
}

null_percentage <- function(x) {
  sum(is.na(x)) / length(x) * 100
}

# Location processing functions
extract_geoid <- function(location) {
  case_when(
    location == "Canada" ~ "CAN",
    str_detect(location, "Canadian Forces Military Police") ~ "99001",
    str_detect(location, "Ottawa-Gatineau, Ontario/Quebec") ~ "24505/35505",
    str_detect(location, "Ottawa-Gatineau, Ontario part") ~ "35505",
    str_detect(location, "Ottawa-Gatineau, Quebec part") ~ "24505",
    TRUE ~ str_extract(location, "(?<=\\[)\\d+(?=\\]$)")
  )
}

clean_location_name <- function(geo_string) {
  case_when(
    is.na(geo_string) ~ "Unknown",
    str_detect(geo_string, "Canadian Forces") ~ "Canadian Forces Military Police",
    str_detect(geo_string, "Ottawa-Gatineau, Ontario part") ~ "Ottawa",
    str_detect(geo_string, "Ottawa-Gatineau, Quebec part") ~ "Gatineau",
    str_detect(geo_string, "Ottawa-Gatineau, Ontario/Quebec") ~ NA_character_,
    TRUE ~ str_replace(str_replace(geo_string, "\\s*\\[.*\\]$", ""), "\\[DGUID:.*\\]", "")
  )
}

get_geographic_level <- function(geo_string) {
  case_when(
    is.na(geo_string) ~ "Unknown",
    geo_string == "Canada" ~ "National",
    str_detect(geo_string, "Ottawa-Gatineau") ~ "City",
    str_detect(geo_string, "Canadian Forces") ~ "Special",
    str_detect(geo_string, ",") ~ "City",
    geo_string %in% c("Yukon", "Northwest Territories", "Nunavut") ~ "Territory",  # Explicitly identify territories
    TRUE ~ "Province"
  )
}

extract_province <- function(geo_string) {
  case_when(
    is.na(geo_string) ~ "Unknown",
    str_detect(geo_string, "Ottawa-Gatineau, Ontario part") ~ "Ontario",
    str_detect(geo_string, "Ottawa-Gatineau, Quebec part") ~ "Quebec",
    str_detect(geo_string, "Canadian Forces") ~ "Special",
    str_detect(geo_string, ",") ~ clean_location_name(str_extract(geo_string, "[^,]+$")),
    TRUE ~ clean_location_name(geo_string)
  )
}

# Helper function to identify summary statistics
is_summary_violation <- function(violation_name) {
  str_detect(violation_name, "^Total") | 
    str_detect(violation_name, "^Total,") |
    violation_name %in% c(
      "0", "25", "50", "100", "180", "190",  # Summary codes
      "All other Criminal Code violations",
      "All property crime violations",
      "All Criminal Code traffic violations",
      "All federal statute violations",
      "All drug violations"
    )
}

# Violation categorization function
get_violation_category <- function(violation_codes, violation_names) {
  categories <- list(
    "Homicide" = c(
      "110", "1110", "1120", "1130", "1140", "1150", "1160", "1210", "1220"
    ),
    "Sexual Assault" = c(
      "130", "1300", "1310", "1320", "1330", "1340", "1390"
    ),
    "Sexual Violations Against Children" = c(
      "1345", "1350", "1355", "1356", "1360", "1365", "1367", "1368", "1369", "1370", "1371", "1375", "1380", "1381", "1385"
    ),
    "Assault" = c(
      "140", "1410", "1420", "1430", "1440", "1470", "1475", "1480"
    ),
    "Robbery" = c(
      "160", "1610", "1611", "1620"
    ),
    "Criminal Harassment" = c(
      "1510", "1515", "1516"
    ),
    "Trafficking in Persons" = c(
      "1520", "1525", "1530"
    ),
    "Kidnapping" = c(
      "1540", "1545", "1550", "1560"
    ),
    "Extortion" = c(
      "1625", "1626", "1627", "1628"
    ),
    "Theft & Fraud" = c(
      "2130", "2132", "2133","2120", "2121", "2125",
      "2140", "2142", "2143","2160", "2165", "2166",
      "2170", "2175", "2176", "2177", "2178",
      "2150", "2152", "2153"
    ),
    "Weapons Violations" = c(
      "3310", "3320", "3330", "3340", "3350", "3365", "3370", "3375", "3380", "3390", "3395"
    ),
    "Drug Trafficking" = c(
      "420", "4210", "4220", "4230", "4240", "4250", "4260", "4270", "4310", "4320", "4330", "4340", "4350", "4360", "4370", "4410", "4420", "4430", "4440", "4450", "4460", "4470", "4590"
    ),
    "Drug Possession" = c(
      "400", "401", "410", "4110", "4120", "4130", "4140", "4150", "4160", "4170"
    ),
    "Impaired Driving" = c(
      "910", "9205", "9210", "9213", "9215", "9217", "9220", "9223", "9225", "9227", "9230", "9233", "9235", "9237", "9240", "9245", "9250", "9255", "9260", "9263", "9265", "9267", "9270", "9273", "9275", "9277", "9280", "9283", "9285", "9287"
    ),
    "Other Criminal Code Violations" = c(
      "300", "310", "320", "330", "335", "340", "3430", "3440", "3450", "3455", "3456", "3460", "3470", "3480", "3490", "3510", "3520", "3540", "3550", "3560"
    )
  )
  
  mapply(function(code, name) {
    if (is_summary_violation(name)) return("Summary Statistics")
    
    if (is.na(code)) return("Uncategorized")
    
    code <- str_extract(code, "(?<=\\[)\\d+(?=\\])")
    if (is.na(code)) return("Uncategorized")
    
    for (category in names(categories)) {
      if (code %in% categories[[category]]) return(category)
    }
    return("Other")
  }, violation_codes, violation_names)
}

#================================================================================================================================================================
# 4. Main Data Processing #<---------paste here the lik to local raw table: 35-10-0177-01
#================================================================================================================================================================
cat("Loading data from local file...\n")
crime_data_raw <- read_csv("Canada_Crime_Watch/data/raw/crime_data_raw.csv", show_col_types = FALSE)   #<---------paste here the lik to local raw table: 35-10-0177-01

# Analyze year range in raw data
cat("\nAnalyzing year range in raw data:\n")
year_range_raw <- crime_data_raw %>%
  summarise(
    min_year = min(REF_DATE, na.rm = TRUE),
    max_year = max(REF_DATE, na.rm = TRUE)
  )
print("Raw data year range:")
print(year_range_raw)

# Step 1: Initial filtering
cat("\nStep 1: Initial filtering...\n")
crime_data_filtered <- crime_data_raw %>%
  select(REF_DATE, GEO, DGUID, Violations, Statistics, VALUE) %>%
  filter(!is.na(VALUE)) %>%
  rename(
    year = REF_DATE,
    location = GEO,
    value = VALUE,
    violation = Violations,
    statistic = Statistics
  ) %>%
  mutate(year = as.numeric(as.character(year)))

# Step 2: Filter for specific year range (2014-2023)
cat("\nStep 2: Filtering for years 2014-2023...\n")
recent_years <- crime_data_filtered %>%
  filter(year >= 2014, year <= 2023)

cat("\nFiltered year range:\n")
year_range_filtered <- recent_years %>%
  summarise(
    min_year = min(year, na.rm = TRUE),
    max_year = max(year, na.rm = TRUE)
  )
print(year_range_filtered)

# Step 3: Process locations
cat("\nStep 3: Processing locations...\n")
locations_processed <- recent_years %>%
  mutate(
    geoid = extract_geoid(location),
    original_location = location,
    location = clean_location_name(location)
  ) %>%
  # Remove the combined Ottawa-Gatineau records
  filter(!is.na(location)) %>%
  left_join(geo_mapping, by = c("geoid" = "GeoUID")) %>%
  mutate(
    level = coalesce(
      Geographic_Level,
      case_when(
        location == "Canada" ~ "National",
        str_detect(original_location, "Canadian Forces") ~ "Special",
        str_detect(original_location, "Ottawa-Gatineau, Ontario part") ~ "City",
        str_detect(original_location, "Ottawa-Gatineau, Quebec part") ~ "City",
        TRUE ~ get_geographic_level(location)
      )
    ),
    region = coalesce(
      Region,
      case_when(
        location == "Canada" ~ "National",
        str_detect(location, "Canadian Forces") ~ "Special",
        TRUE ~ "Unknown"
      )
    ),
    province = case_when(
      level == "Province" | level == "Territory" ~ location,  # Keep territory names
      level == "City" & str_detect(original_location, "Ottawa-Gatineau, Ontario part") ~ "Ontario",
      level == "City" & str_detect(original_location, "Ottawa-Gatineau, Quebec part") ~ "Quebec",
      level == "City" ~ extract_province(original_location),
      level == "National" ~ "Canada",
      level == "Special" ~ "Special",
      TRUE ~ "Unknown"
    ),
    hierarchy_level = case_when(
      level == "National" ~ 1,
      level %in% c("Province", "Territory") ~ 2,  # Both provinces and territories at level 2
      level == "City" ~ 3,
      level == "Special" ~ 4,
      TRUE ~ 99
    )
  )

# Validation checks for location processing
cat("\nValidation: Checking Ottawa-Gatineau processing:\n")
ottawa_gatineau_check <- locations_processed %>%
  filter(str_detect(original_location, "Ottawa-Gatineau")) %>%
  select(original_location, location, geoid, level, province) %>%
  distinct()
print(ottawa_gatineau_check)

cat("\nValidation: Checking Canadian Forces processing:\n")
cf_check <- locations_processed %>%
  filter(str_detect(original_location, "Canadian Forces")) %>%
  select(original_location, location, geoid, level, province) %>%
  distinct()
print(cf_check)

#================================================================================================================================================================
# 5. Process violations
#================================================================================================================================================================
cat("\nStep 4: Processing violations...\n")
violations_processed <- locations_processed %>%
  mutate(
    violation_name = clean_location_name(violation),
    violation_code = str_extract(violation, "\\[.*?\\]$"),
    violation_type = get_violation_category(violation_code, violation_name),
    is_summary = is_summary_violation(violation_name)
  )

# Check for uncategorized violations
cat("\nChecking for uncategorized violations:\n")
uncategorized_violations <- violations_processed %>%
  filter(violation_type %in% c("Other", "Uncategorized")) %>%
  select(violation, violation_code, violation_type) %>%
  distinct()
print(uncategorized_violations)

#================================================================================================================================================================
# 6. Final data preparation
#================================================================================================================================================================
cat("\nStep 5: Finalizing data...\n")
crime_data <- violations_processed %>%
  filter(!is_summary) %>%  # Remove summary statistics
  filter(statistic %in% c(
    "Rate per 100,000 population",
    "Percentage change in rate",
    "Actual incidents",
    "Total cleared",
    "Cleared by charge",
    "Unfounded incidents",
    "Total, adult charged",
    "Total, persons charged",
    "Total, youth charged",
    "Total, youth not charged"
  )) %>%
  select(
    year,
    location,
    original_location,
    latitude,
    longitude,
    level,
    hierarchy_level,
    region,
    province,
    violation_name,
    violation_type,
    violation_code,
    statistic,
    value
  ) %>%
  # Add aggregation level column
  mutate(
    aggregation_level = case_when(
      level == "National" ~ "canada_total",
      level == "Province" ~ "province_total",
      level == "City" ~ "city_level",
      level == "Special" ~ "special_admin",
      TRUE ~ "unknown"
    ),
    # Add a flag to indicate if this record should be included in higher-level aggregations
    include_in_aggregation = case_when(
      level == "National" ~ TRUE,  # Canada totals are pre-aggregated
      level == "Province" ~ TRUE,  # Province totals are pre-aggregated
      level == "City" ~ TRUE,      # City data is base level
      level == "Special" ~ TRUE,   # Special jurisdictions are separate
      TRUE ~ FALSE
    )
  ) %>%
  pivot_wider(
    names_from = statistic,
    values_from = value
  ) %>%
  rename(
    Actual_Incidents = `Actual incidents`,
    Rate_per_100k_People = `Rate per 100,000 population`,
    Percent_Change_in_Rate = `Percentage change in rate`,
    Total_Cleared = `Total cleared`,
    Cleared_by_Charge = `Cleared by charge`,
    Unfounded_incidents = `Unfounded incidents`,
    Total_persons_charged = `Total, persons charged`,
    Total_youth_not_charged = `Total, youth not charged`,
    Total_youth_charged = `Total, youth charged`,
    Total_adult_charged = `Total, adult charged`
  ) %>%
  mutate(
    Clearance_Rate = ifelse(Total_Cleared > 0, (Cleared_by_Charge / Total_Cleared) * 100, 0),
    Clearance_Rate = round(Clearance_Rate, 1),
    is_aggregated = hierarchy_level < 3,
    aggregation_note = case_when(
      hierarchy_level == 1 ~ "National aggregate",
      hierarchy_level == 2 ~ "Provincial/Territorial aggregate",
      hierarchy_level == 3 ~ "City-level data",
      hierarchy_level == 4 ~ "Special jurisdiction",
      TRUE ~ "Unknown"
    ),
    # Add source level indication
    source_level = case_when(
      level == "National" ~ "Canada",
      level == "Province" ~ province,
      level == "City" ~ location,
      level == "Special" ~ "Special",
      TRUE ~ "Unknown"
    )
  )

# Add a check for aggregation consistency
cat("\nChecking aggregation consistency...\n")
agg_check <- crime_data %>%
  group_by(year) %>%
  summarise(
    national_total = sum(Actual_Incidents[level == "National"], na.rm = TRUE),
    province_sum = sum(Actual_Incidents[level == "Province"], na.rm = TRUE),
    city_sum = sum(Actual_Incidents[level == "City"], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    difference = national_total - province_sum
  )

print("Aggregation check results:")
print(agg_check)

# Save complete dataset
write_csv(crime_data, "Canada_Crime_Watch/data/processed/crime_data.csv")

#================================================================================================================================================================
# Final summary
#================================================================================================================================================================
cat("\nFinal Data Summary:\n")
cat("Years covered:", min(crime_data$year), "to", max(crime_data$year), "\n")
cat("Number of locations:", n_distinct(crime_data$location), "\n")
cat("Number of violation types:", n_distinct(crime_data$violation_type), "\n")
cat("Geographic levels:", paste(unique(crime_data$level), collapse = ", "), "\n")

# Print data structure
cat("\nStructure of final data:\n")
str(crime_data)

# Print summary statistics
cat("\nSummary of numeric columns:\n")
summary(select(crime_data, where(is.numeric)))