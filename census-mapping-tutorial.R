# Source of Tutorial: https://rstudio-pubs-static.s3.amazonaws.com/377633_f3b82e1624db4ae49dc735a7820bf17a.html

library(acs)
library(tigris)
library(leaflet)
library(mapview)
library(stringr)
library(sf)
library(tidyverse)
library(tidycensus)

# Install API key to get access to census data
# Source: https://api.census.gov/data/key_signup.html
api.key.install(key="0b5de1b0030f5b45d191c313a872de3a1ba1c362")



### Fetch spatial shapefile for giving area ###
# Find state and county codes from ACS package with geo.lookup
geo.lookup(state="UT", place="Sandy") # To find which county a city is a part of
geo.lookup(state="UT", county="Salt Lake County") # To find FIPS codes for state and county

# From tigris package; finds FIPS code for state and county
lookup_code(state="UT",county="Salt Lake County")

# Find spatial shapefile data using tracts from tigris
shapefile <- tracts(state='49', county='035')
plot(shapefile)

washington=geo.make(state=53)
washington


census_api_key("37ac73a1c944172445bf6a7366224526db63ac38", overwrite=TRUE)

# Viewing census data variables:
v17 <- load_variables(2017, "acs5", cache = TRUE)


utah_pop <- get_acs(geography = "county", 
                    variables = "B01003_001", 
                    state = "UT",
                    geometry = TRUE)
utah_pop$geometry
utah_pop$donations <- rep('NA', 29)
utah_pop
utah_pop$NAME
df_filtered_5
utah_gis_df <- data.frame('NAME'=utah_pop$NAME)
utah_gis_df
utah_pop
pal <- colorNumeric(palette = "plasma", 
                    domain = utah_pop$estimate)

# Add state and county fips ids to columns in utah_pop 
len_df <- dim(utah_pop)[[1]]
state_id <- sapply(c(1:len_df), function(i) { substr(utah_pop$GEOID[i],1,2) } )
county_id <- sapply(c(1:len_df), function(i) { substr(utah_pop$GEOID[i],3,5) } )
utah_pop$state_id <- state_id
utah_pop$county_id <- county_id

utah_pop
head(df_filtered_5)
# Show on map
utah_pop %>%
  st_transform(crs = "+init=epsg:4326") %>%
  leaflet(width = "100%") %>%
  addProviderTiles(provider = "CartoDB.Positron") %>%
  addPolygons(popup = ~ str_extract(NAME, "^([^,]*)"),
              stroke = FALSE,
              smoothFactor = 0,
              fillOpacity = 0.7,
              color = ~ pal(estimate)) %>%
  addLegend("bottomright",
  pal = pal,
  values = ~ estimate,
  title = "County Populations",
  opacity = 1)

utah_pop$state_id <- state_id
utah_pop
df_csv <- data.frame(read.csv('utah-counties.csv'))
df_csv
utah_pop

# Pull only data of interest
state_fips <- df_csv$STATEFIPS
state_abbrv <- df_csv$STATE
county_fips <- df_csv$COUNTYFIPS
county_name <- df_csv$COUNTYNAME
adj_gross_income_id <- df_csv$agi_stub
num_returns <- df_csv$N1
num_dependents <- df_csv$NUMDEP
adj_gross_income <- df_csv$A00100
num_returns_w_total_income <- df_csv$N02650
total_income_amt <- df_csv$A02650
num_returns_donations <- df_csv$N19700
total_amt_donations <- df_csv$A19700

# Convert county_fips ids to 3 digits
library(stringr)
str_pad(county_fips[[70]],pad = 7,"0")
formatC(as.numeric(county_fips[[20]]),width=3,flag='0')


county_fips <- sapply(county_fips,function(i) { formatC(as.numeric(county_fips[i]),width=3,flag="0" ) })
library(dplyr)

utah_df <- data.frame(state_fips,
                      state_abbrv,
                      county_fips,
                      county_name,
                      adj_gross_income_id,
                      num_returns,
                      num_dependents,
                      adj_gross_income,
                      num_returns_w_total_income,
                      total_income_amt,
                      num_returns_donations,
                      total_amt_donations)
utah_df
df_filtered_5 <- filter(utah_df, adj_gross_income_id==5)

df_filtered_5$county_fips
df2_utah_pop <- data.frame(utah_pop)

df_filtered_5
df2_utah_pop

df$Var1 <- mapply(function(day, col) df2[df2$date==day, as.character(col)], day=df$date, col=df$X1)

data.frame(utah_pop)
df2_utah_pop$county_id
for(row in df_filtered_5){
  for(subrow in df2_utah_pop){
    print(subrow)
    print(df_filtered_5$county_fips[row])
    print(df2_utah_pop$county_id[subrow])
    # if(subrow == row){
    #   print('found_match!')
    #   print(row)
    #   print(subrow)
    # }
  }
}

map(utah_df,"state_fips")
ifelse(sapply(county_fips, function(i) { county_fips  } ))

for(id in county_fips){
  print(id)
  if(length(as.numeric(id)) == 2){}
}

