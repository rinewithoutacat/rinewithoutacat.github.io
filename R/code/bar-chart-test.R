# Chart Test for Github

# Combined Options for TF

# Library Set Up ----
library(airtabler)
library(extrafont)
library(httr)
library(htmlwidgets)
library(jsonlite)
library(lubridate)
library(manipulateWidget)
library(plotly)
library(rsconnect)
library(scales)
library(sf)
library(shiny)
library(showtext)
library(systemfonts)
library(tidyverse)
library(usmap)

# Setting Up Environment ----
# Need to set API key in environment (need to take out before deployment)
Sys.setenv("AIRTABLE_API_KEY" = "pathxIAPFOAUfygsz.e1bd319fb9b2ab9380f1a4a49efe9f2242dc7dd594c6fca45482fcf7a1b076d3") 
Sys.setenv("plotly_username" = "rinewithoutacat")
Sys.setenv("plotly_api_key" = "5MgqkzkfxrUYDA3Zo2KA")

# Setting up fonts ----
#loadfonts(device = "win", quiet = TRUE) 

# Alexandria is the Arabic version of Montserrat
sysfonts::font_add_google(name = "Montserrat", family = "montserrat", db_cache = FALSE)
# Using this because Figtree is not in the package yet
sysfonts::font_add_google(name = "Figtree", family = "figtree", db_cache = FALSE)

# Come back and fix font in plotly https://www.youtube.com/watch?v=tiIdoXxNAxI&t=425s
# Because Plotly is annoying, setting up some extra formatting
alexandria <- list(
  family = "alexandria")


montserrat <- list(
  family = "montserrat"
)

# 

showtext::showtext_auto()

# Custom CSS:
#get the source url to the font file from google
alexandria_file <- showtextdb::google_fonts("Alexandria")$regular_url 

#create custom CSS 
alexandria_css <- paste0(
  "<style type = 'text/css'>",
  "@font-face { ",
  "font-family: 'gochi'; ", #however you want to refer to it
  "src: url('", alexandria_file, "'); ", #file from google
  "}",
  "</style>")


# Shiny tutorial: Using custom CSS in your app
#https://shiny.posit.co/r/articles/build/css/


# Data Cleaning ----
## Pulling "Negative Bills" sheet from "Editing" database ----
tf_airtable<- airtabler::airtable("appVXvrTCBaSvn2Id", "Negative Bills")

# Getting all data
tf_airtable_all <- tf_airtable$`Negative Bills`$select_all()

# Cleaning dates--making sure after 2020, also that it is only one of the three statuses we want
tf_airtable_all$`Date Introduced` <- as.Date(tf_airtable_all$`Date Introduced`)

# Filtering from Jan 01, 2021 onwards (doing this by calling anything after Dec 31, 2020):
tf_airtable_all <- tf_airtable_all %>%
  filter(`Date Introduced` > "2020-12-31") %>%
  filter(Status != "Not Anti-Trans in current form")

## By Date ----
# Making a new spreadsheet
tf_airtable_count <- tf_airtable_all

# Setting the date as a m/y date
tf_airtable_count$`Date Introduced` <- format(tf_airtable_count$`Date Introduced`, format = "%m/%y")

# Counting bills by current status, by month/year date
tf_airtable_count <- tf_airtable_count %>%
  group_by(`Date Introduced`) %>%
  count(`Status`)

# Formatting Date column that is a date parsable by R, and also will have a separate m-y column
tf_airtable_count$Date <- lubridate::my(tf_airtable_count$`Date Introduced`)

# Arranging in ascending order (starting at 01/21 to present)
tf_airtable_count <- tf_airtable_count %>% 
  arrange(ymd(tf_airtable_count$Date))

## By State (for map): ----
# Total over all time
tf_airtable_state_total <- tf_airtable_all %>% 
  group_by(`State`) %>%
  count()

# Total per year per state: Creates a data frame with state name, year, and total
tf_airtable_state_by_year <- tf_airtable_all %>% 
  group_by(`State`) %>%
  count(year = lubridate::year(`Date Introduced`))

tf_airtable_state_by_year <- tf_airtable_state_by_year %>%
  pivot_wider(
    names_from=year,
    values_from=n
  )

# Making NA values 0
tf_airtable_state_by_year[is.na(tf_airtable_state_by_year)] <- 0

# Plotly is annoying and wants abbreviations, so:
# https://stackoverflow.com/questions/5411979/state-name-to-abbreviation
tf_airtable_state_by_year$state_abb <- state.abb[match(tf_airtable_state_by_year$State,state.name)]

# Issue: R doesn't recognize "Hawai'i"
tf_airtable_state_by_year <- tf_airtable_state_by_year %>%
  # replacing NA in state list with HI--HI should be the only one NA
  dplyr::mutate(state_abb = replace_na(state_abb, "HI"))

## For Bar Chart: ----
# Pivoting wider--necessary for setting up bar chart stacked as we want it  
tf_airtable_long <- tf_airtable_count %>%
  pivot_wider(names_from = "Status", values_from = "n")

# Replacing NAs with zeros
tf_airtable_long <- tf_airtable_long %>% 
  replace(is.na(.), 0)

# Calculating totals: Passed, Failed, Active
passed_updated_total <- tf_airtable_count %>%
  filter(Status == 'Passed')
passed_updated_total <-sum(passed_updated_total$n)

failed_updated_total <- tf_airtable_count %>%
  filter(Status == 'Failed')
failed_updated_total <- sum(failed_updated_total$n)

active_updated_total <- tf_airtable_count %>%
  filter(Status == 'Active')
active_updated_total <- sum(active_updated_total$n)

## For Map: ----
# Pulling out US map with all 50 states
tf_usa_map <- us_map(regions = "states")

# Joining so can be used 
tf_current_map <- left_join(tf_usa_map, tf_airtable_state_by_year, by=c("abbr" = "state_abb"))

# Removing DC:
tf_current_map <- tf_current_map %>%
  filter(abbr != "DC")

# Getting total count of bills for 2024
updated_total_bills <- sum(tf_current_map$`2024`)


## For Table: ----
tf_count_year <- tf_airtable_count %>%
  group_by(Date = lubridate::year(Date), Status) %>%
  summarise(Bills = sum(n))

# This will go away when don't need to set as factor
tf_count_year$Status <- as.character(tf_count_year$Status)

# Reverse date order (descending) so 2024 is first
tf_count_year <- tf_count_year %>% 
  arrange(desc(Date))
# Bar Chart: ----
bar_chart_title <- paste0("Since 2021, we've tracked: ", "<br>", 
                          passed_updated_total, " passed bills", "<br>",
                          failed_updated_total, " failed bills", "<br>",
                          active_updated_total, " currently active bills")

chart1 <- plot_ly(
  data = tf_airtable_long,
  x = ~`Date`,
  y = ~Failed,
  name = "Failed",
  #color = ~Status,
  #name = ~Status,
  marker = list(color = "#ed7fa1"),
  type = "bar",
  hovertext = ~paste0(`Failed`, "<br>", 
                      "Bills: ", `Failed`, "<br>",
                      "Status: Failed"),
  hoverinfo = "text") %>%
  add_trace(y = ~Active,
            name = "Active",
            marker = list(color = "#DB0044"),
            hovertext = ~paste0(`Date Introduced`, "<br>", 
                                "Bills: ", `Active`, "<br>",
                                "Status: Active")) %>%
  add_trace(y = ~Passed,
            name = "Passed",
            marker = list(color = "#410014"),
            hovertext = ~paste0(`Date Introduced`, "<br>", 
                                "Bills: ", `Passed`, "<br>",
                                "Status: Passed")) %>%
  layout(barmode = "stack") %>%
  layout(yaxis = list(title = 'Bills Introduced'),
         xaxis = list(title = 'Date Introduced')
         #title = list(title = bar_chart_title)
  ) %>%
  layout(font = list(family = alexandria)) %>%
  layout(title =  ~paste0("Since 2021, we've tracked: ", passed_updated_total, " passed bills", "<br>",
                          failed_updated_total, " failed bills, and ", active_updated_total, " currently active bills")) 


saveWidget(chart1, 
           file.path('chart1.html'))
