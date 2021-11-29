library(acs)
library(tigris)
library(leaflet)
library(mapview)
library(stringr)
library(sf)
library(tidyverse)
library(tidycensus)

## TO DO:
# - update to in the thousands and add dollar sign to labels
# - clean up code

initialize_data <- function(){
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
  
  
  census_api_key("37ac73a1c944172445bf6a7366224526db63ac38", overwrite=TRUE)
  
  # Viewing census data variables:
  v17 <- load_variables(2017, "acs5", cache = TRUE)
}
initialize_data()
get_county_geom_data <- function(){
  
  utah_pop <- get_acs(geography = "county", 
                      variables = "B01003_001", 
                      state = "UT",
                      geometry = TRUE)
  utah_pop %>%
    mutate(NAME = gsub(" County, Utah", "", NAME))
  return(utah_pop)
}

import_donation_data <- function(){
  df_csv <- data.frame(read.csv('utah-counties.csv'))
  
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
  num_returns_w_donations <- df_csv$N19700
  total_amt_donations <- df_csv$A19700
  
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
                        num_returns_w_donations,
                        total_amt_donations)
  utah_df$perct_amt_income_donated <- as.integer(
    (utah_df$total_amt_donations / utah_df$total_income_amt) * 100)
  utah_df$perct_returns_w_donations <- as.integer(
    (utah_df$num_returns_w_donations / utah_df$num_returns_w_total_income) * 100)
  
  return(utah_df)
}

filter_on_income_level <- function(df, income_val){

  df_income_lvl <- filter(df, adj_gross_income_id==income_val)
  df_income_lvl$COUNTYNAME
  df <- df_income_lvl[df_income_lvl$county_name != "Utah",]
  return(df)
}

add_donation_data_to_geom <- function(gis_df, irs_df){
  gis_df$total_amt_donations <- irs_df$total_amt_donations
  gis_df$perct_amt_income_donated <- irs_df$perct_amt_income_donated
  gis_df$perct_returns_w_donations <- irs_df$perct_returns_w_donations
  gis_df$num_returns <- irs_df$num_returns
  gis_df$num_returns_w_total_income <- irs_df$num_returns_w_total_income
  gis_df$total_income_amt <- irs_df$total_income_amt
  gis_df$adj_gross_income <- irs_df$adj_gross_income
  return(gis_df)
}


mapPerctAmtDonated <- function(df){
  
  pal <- colorNumeric(palette = "plasma", 
                      domain = df$perct_amt_income_donated)

  # Show on map
  df %>%
    st_transform(crs = "+init=epsg:4326") %>%
    leaflet(width = "100%") %>%
    addProviderTiles(provider = "CartoDB.Positron") %>%
    addPolygons(popup = ~ str_extract(NAME, "^([^,]*)"),
                stroke = FALSE,
                smoothFactor = 0,
                fillOpacity = 0.7,
                color = ~ pal(perct_amt_income_donated)) %>%
    addLegend("bottomright",
              pal = pal,
              values = ~ perct_amt_income_donated,
              title = "% of Income Donated by County",
              opacity = 1)

  
}
mapPerctAmtDonated(df)

get_data <- function(income_lvl){
  # Get API key, data, etc.
  # initialize_data()
  
  # Get IRS data
  irs_df <- filter_on_income_level(import_donation_data(), income_lvl)
  irs_df <- irs_df[order(irs_df$county_name),]
  
  # Get GIS data
  gis_data <- get_county_geom_data() %>%
    mutate(NAME = gsub(", Utah", "", NAME))
  gis_data <- gis_data[order(gis_data$NAME),]
  
  df <- add_donation_data_to_geom(gis_data, irs_df)
  # print(df)
  return(df)
}

income_lvl <- 8
top_perct_donated <- c()
top_perct_w_donations <- c()
for(i in c(1:8)){
  df <- get_data(income_lvl=i)
  top_6_perct_donated <- head(df[order(df$perct_amt_income_donated),])$NAME
  top_6_perct_w_donations <- head(df[order(df$perct_returns_w_donations),])$NAME
  summary_df <- data.frame("Top % That Donated"=append(top_6_perct_donated,i))
  top_6_perct_donated
  top_6_perct_w_donations
  # show_on_map(df)
}
df <- get_data(income_lvl)
df
top_6_perct_donated_names <- df[order(df$perct_amt_income_donated),]$NAME
top_6_perct_donated_names
top_6_perct_donated_vals <- df[order(df$perct_amt_income_donated),]$perct_amt_income_donated
top_6_perct_donated_vals

df_perct_donated <- data.frame()
top_6_perct_w_donations_names <- head(df[order(df$perct_returns_w_donations),])$NAME
top_6_perct_donated_vals <- head(df[order(df$perct_returns_w_donations),])$perct_returns_w_donations
top_6_perct_w_donations
show_on_map(df)

total_returns <- sum(import_donation_data()$num_returns[import_donation_data()$county_name == "Utah"] )
total_pop <- sum(gis_data$estimate)
perct_pop_w_returns <- (total_returns / total_pop) * 100

####### What income levels spend the highest % of their income on donations (perct_amt_income_donated)? ########

df <- get_data(8)
mapPerctAmtDonated(df)

top_6_perct_donated_names <- df[order(df$perct_amt_income_donated, decreasing=TRUE),]$NAME
top_6_perct_donated_names
top_6_perct_donated_vals <- df[order(df$perct_amt_income_donated, decreasing=TRUE),]$perct_amt_income_donated
top_6_perct_donated_vals
df$NAME

income_lvl = 8
tb <-
  tibble(
    "Income Level" = NA,
    "County Name" = NA,
    "% of Income Donated" = NA,
    "Total Amt Donated" =NA,
    "Population" = NA,
    "Total # of Donations" = NA,
    "% of Returns with Donations" = NA,
    "Total Income Amt" = NA,
    "Adj. Gross Income" = NA,
    "Num. of Returns" = NA
  ) 
tb
for(income_lvl in c(2:8)){
  
  df <- get_data(income_lvl)
  tb <- tb %>%
    add_row(
      "Income Level" = rep(income_lvl, nrow(df)),
      "County Name" = df$NAME,
      "% of Income Donated" = df$perct_amt_income_donated,
      "Total Amt Donated" = df$total_amt_donations,
      "Population" = df$estimate,
      "Total # of Donations" = df$total_amt_donations,
      "% of Returns with Donations" = df$perct_returns_w_donations,
      "Total Income Amt" = df$total_income_amt,
      "Adj. Gross Income" = df$adj_gross_income,
      "Num. of Returns" = df$num_returns
    ) %>%
    arrange(`Income Level`, desc(`% of Income Donated`))
  
    tb <- tb[!is.na(tb$`Income Level`),]
  
  print(tb)
}
?corrplot
library(GGally)
library(ggplot2)

ggpairs(tb_nocounty, title="correlogram with ggpairs()") 

tb_nocounty <- tb %>% select(-c(`County Name`))

tb_nocounty
tb[tb$`County Name` == "Cache County",]
summary(tb)
tb <- tb %>%
  group_by(`Income Level`) %>%
  mutate(Percentile_Rank=rank(`% of Income Donated`)/length(`% of Income Donated`))

ggplot(data=tb, aes(x=`County Name`, y=`% of Income Donated`, fill=Percentile_Rank)) +
  geom_bar(stat="identity") +
  scale_fill_gradient(low="blue", high="yellow") +
  facet_grid(rows=vars(`Income Level`)) +
  theme(axis.text.x = element_text(size = 9, angle = 45, hjust=1))

income_lvl = 3
ggplot(data=tb[tb$`Income Level` == income_lvl,], 
       aes(x=reorder(`County Name`, -`% of Income Donated`), y=`% of Income Donated`,  fill=Percentile_Rank)) +
  geom_bar(stat="identity") +
  scale_fill_gradient(low="blue", high="yellow") +
  theme(axis.text.x = element_text(size = 9, angle = 45, hjust=1))

# What I'd like to know:

# - Which income levels donate the highest % of their income on donations? (perct_amt_income_donated)
# - Which income levels donate the most? (perct_returns_w_donations)
# - Is there a relationship between other factors and high levels of donations? 
      #(religious communities, political party, etc.)
# - Are the counties that donate the highest % of their income also donating the most across the county population?
# - Percentage of population in this income level in this county** - must account for age and who does tax returns
# - Which counties are the richest and poorest?

# regression model to see what factors most heavily influence how much $ someone donatest (y value would be % of income donated)
# income level
# political party
# pop size
# religious affiliation
# num of dependents
# gross income
# num of returns
# % of pop in each income lvl category

# Objective: Create a model to predict what factors influence how much a county donates 
# Scope: state of Utah
# Dependent (predictor) variable: Total Amt Donated
# Independent (features) variables: Pop. size, income lvl, adj. gross income, # of dependents, political affiliation, religious affiliation
# Num of returns
tb[tb$`Income Level` == income_lvl,]
tb
x <- tb$`Total Income Amt`
y <- tb$`Total Amt Donated`
lm(y ~ x)
library(ggpubr)

ggplot(tb, aes(x=`Total Income Amt`, y=`Total Amt Donated`)) +
  geom_point() +
  geom_smooth(method = "lm", se=FALSE) +
  stat_regline_equation(label.y.npc = "top", aes(label = ..eq.label..)) +
  stat_regline_equation(label.y.npc = "center", aes(label = ..rr.label..))
  # xlim(0, 3000000)
  # ylim(0, 50000)
