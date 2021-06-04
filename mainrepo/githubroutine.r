library(tidyverse)
library(lubridate)
library(writexl)
library(openxlsx)
library(here)
library(ggplot2)
library(scales)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggthemes)
library(zoo)
library(rgeos)
library(janitor)

# Import source data --------------------------------------------------------------
oxcgrtdata <- read_csv("https://raw.githubusercontent.com/OxCGRT/covid-policy-tracker/master/data/OxCGRT_latest.csv", 
                       col_types = cols(RegionName = col_character(), 
                                        RegionCode = col_character()))

oxcgrtUS <- oxcgrtdata[oxcgrtdata$CountryCode == "USA" & !is.na(oxcgrtdata$RegionName),]

#oxcgrtdata <- oxcgrtdata %>% filter(is.na(RegionName))

oxcgrtdata <- 
  oxcgrtdata %>% 
  filter(is.na(RegionName)) %>%
  mutate(Date = ymd(Date)) %>%
  filter(Date != Sys.Date()) %>%
  group_by(CountryCode) %>%
  arrange(CountryCode, Date) %>%
  fill(ConfirmedCases, GovernmentResponseIndex, .direction = "down")

oxcgrtUS <- 
  oxcgrtUS %>% 
  mutate(Date = ymd(Date)) %>%
  filter(Date != Sys.Date()) %>%
  group_by(RegionCode) %>%
  arrange(RegionCode, Date) %>%
  fill(ConfirmedCases, GovernmentResponseIndex, .direction = "down")

# get a time series for each index and indicator ------------------------------------- 

## extract list of indices
indices <- names(clean_names(oxcgrtdata)) %>% str_subset(pattern = "index$")
indices <- indices[indices != "stringency_legacy_index"]

## extract list of indicators 
indicators <- names(clean_names(oxcgrtdata)) %>% str_subset(pattern = "^(c|h|e)[0-9]_") %>% str_subset(pattern = "notes", negate = T)
indicators <- indicators[!indicators %in% c("h4_emergency_investment_in_healthcare", 
                             "h5_investment_in_vaccines", 
                             "e4_international_support",
                             "e3_fiscal_measures")]

## extract cases and deaths
casedeaths <- names(clean_names(oxcgrtdata)) %>% str_subset(pattern = "cases|deaths")

## create empty worksheet and write timeseries in each tab
tslist <- c(indices, indicators, casedeaths)
ts_sheet <- createWorkbook()

timeseries <- function(i){
  temp <- 
    oxcgrtdata %>% 
    clean_names() %>%
    select(all_of(i), country_code, country_name, date)
  temp <- 
    temp %>% 
    mutate(date = format(date, "%d%b%Y")) %>%
    pivot_wider(names_from = date, values_from = all_of(i))
  if(i == "c7_restrictions_on_internal_movement"){
    i <- "c7_movementrestrictions"
  } else if(i == "c8_international_travel_controls"){
    i <- "c8_internationaltravel"
  } else if(i == "e2_debt_contract_relief"){
    i <- "e2_debtrelief"
  }
  print(i)
  addWorksheet(ts_sheet, i)
  writeData(ts_sheet, i, temp)
  datevars <- str_subset(names(temp), pattern = "2020|2021")
  for(j in datevars){
    temp <- 
      temp %>%
      ungroup() %>%
      mutate(!!j := ifelse(is.na(!!sym(j)), "", !!sym(j)))
  }
  #temp <- 
  #  temp %>%
  #  mutate()
  write.csv(temp, file = paste0("./data/timeseries/", i, ".csv"))
}

lapply(tslist, timeseries)

## publish timeseries worksheet
saveWorkbook(ts_sheet, file = "./data/timeseries/OxCGRT_timeseries_all.xlsx", overwrite = T)

# GITHUB README graphs ---------------------------------------------------

## G1 - Government Response Index v/s Cases for 6 countries ================

message("Start G1")

key_countries <- c("CHN", "KOR", "USA", "FRA","GBR", "ITA")

temp_tibble <- 
  oxcgrtdata %>%
  ungroup() %>%
  filter(CountryCode %in% key_countries) %>%
  select(GovernmentResponseIndexForDisplay, ConfirmedCases, CountryName, CountryCode, Date) %>%
  mutate(log10_cases = log10(ConfirmedCases + 1)) %>%
  ungroup()

plot <- 
  ggplot(data = temp_tibble) + 
  geom_line(aes(x = log10_cases, y = GovernmentResponseIndexForDisplay, group = CountryName, colour = CountryName), size = 1) + 
  scale_colour_viridis_d() + 
  labs(y = "Government Response Index", 
       x = "Reported numbers of COVID-19 cases",
       caption = "Source: Oxford COVID-19 Government Response Tracker. More at https://github.com/OxCGRT/covid-policy-tracker 
       or bsg.ox.ac.uk/covidtracker") +
  scale_y_continuous(breaks = c(0, 20, 40, 60, 80, 100)) +
  scale_x_continuous(breaks = c(0, 1, 2, 3, 4, 5, 6, 7), 
                     labels = c("1", "10", "100", "1,000", "10,000", "100,000", "1,000,000", "10,000,000")) +
  expand_limits(y = c(0, 100)) +
  theme(
    # Remove panel border
    panel.border = element_blank(),  
    # Remove panel grid lines
    panel.grid.major = element_line(size = 0.5, linetype = "dashed", colour = "grey"),
    #panel.grid.minor = element_blank(),
    # Remove panel background
    panel.background = element_blank(),
    # Add axis line
    axis.line = element_line(colour = "grey"),
    plot.caption = element_text(hjust = 0.5, face = "italic"),
    legend.position = "bottom", 
    legend.title = element_blank(), 
    legend.key = element_blank()) 

ggsave(plot, 
      filename = "./images/OxCGRT_sixin1_bycases.png",
      height = 8, 
      width = 12)

## 3x2 graph - get 100th case date and then transform to get days since 100th case --------------------

message("Start G2")

temp_tibble <- 
  oxcgrtdata %>%
  ungroup() %>%
  filter(CountryCode %in% key_countries) %>%
  select(ConfirmedCases, GovernmentResponseIndexForDisplay, Date, CountryCode, CountryName) %>%
  group_by(CountryCode) %>%
  arrange(CountryCode, Date) %>%
  mutate(date_100thcase = ifelse(ConfirmedCases >= 100 & (lag(ConfirmedCases) < 100 | is.na(lag(ConfirmedCases))), Date, NA),
         date_100thcase = ymd(as.Date(date_100thcase)),
         newcases = ConfirmedCases - lag(ConfirmedCases),
         newcases = ifelse(newcases <= 0, NA, newcases)) %>%
  fill(date_100thcase, .direction = "updown") %>%
  fill(newcases, .direction = "down") %>%
  mutate(days_100thcase = Date - date_100thcase,
         ave7day_newcases = zoo::rollmean(newcases, k = 7, fill = NA),
         log10_newcases = log10(ave7day_newcases + 1))


coeff <- 20

plot <- 
  ggplot(data = temp_tibble, aes(x = days_100thcase)) + 
  geom_line(aes(y = log10_newcases), colour = "purple") + 
  geom_line(aes(y = GovernmentResponseIndexForDisplay/coeff), colour = "red") + 
  scale_y_continuous(name = "Reported number of new cases",
                     breaks = c(0, 1, 2, 3, 4, 5),
                     labels = c("1", "10", "100", "1,000", "10,000", "100,000"),
                     sec.axis = sec_axis(~.*coeff, 
                                         name = "Government Response Index",
                                         breaks = c(0, 20, 40, 60, 80, 100),
                                         labels = c("0", "20", "40", "60", "80", "100"))) + 
  scale_x_continuous(breaks = c(-100, 0, 100, 200, 300),
                     labels = c(-100, 0, 100, 200, 300),
                     name = "Days after 100th recorded case") + 
  labs(caption = "Source: Oxford COVID-19 Government Response Tracker. More at https://github.com/OxCGRT/covid-policy-tracker 
       or bsg.ox.ac.uk/covidtracker") +
  theme(
    axis.text.x = element_text(size = 6.5), 
    axis.text.y.right = element_text(colour = "red"), 
    axis.title.y.right = element_text(colour = "red"), 
    axis.text.y.left = element_text(colour = "purple"), 
    axis.title.y.left = element_text(colour = "purple"),
    strip.text = element_text(size = 12.5),
    # Remove panel border
    panel.border = element_blank(),  
    # Remove panel grid lines
    panel.grid.major = element_line(size = 0.5, linetype = "dashed", colour = "grey"),
    #panel.grid.minor = element_blank(),
    # Remove panel background
    panel.background = element_blank(),
    # Add axis line
    axis.line = element_line(colour = "grey"),
    plot.caption = element_text(hjust = 0.5, face = "italic")) + 
  facet_wrap(~CountryName)
  
ggsave(plot, 
       filename = "./images/OxCGRT_six_countries.png",
       height = 8, 
       width = 12)  

# Max Government Response level v/s n(covid cases) -------------------------

message("Start G3")

temp_tibble <- 
  oxcgrtdata %>%
  ungroup() %>%
  select(CountryCode, Date, ConfirmedCases, GovernmentResponseIndexForDisplay) %>%
  group_by(CountryCode) %>%
  arrange(CountryCode, Date) %>%
  fill(ConfirmedCases, .direction = "down") %>%
  slice_tail(n = 1) %>%
  #summarise(cases = max(ConfirmedCases), 
  #          maxgri = max(GovernmentResponseIndexForDisplay)) %>%
  mutate(log10_cases = log10(ConfirmedCases+1)) %>%
  ungroup()

key_countries <- c("CHN", "KOR", "USA", "SWE", "GBR", "ITA", "SGP", "HKG", "NIC", "TWN", 
                   "PNG", "KEN")

plot <-
  ggplot(data = temp_tibble, aes(x = log10_cases, y = GovernmentResponseIndexForDisplay, label = CountryCode)) + 
  geom_point() + 
  geom_smooth(method = lm, se = F) +
  geom_text(data = temp_tibble %>% subset(CountryCode %in% key_countries)) +
  expand_limits(y = c(0,100),
                x = c(0, 7)) +
  scale_x_continuous(name = "Reported number of COVID-19 cases",
                     breaks = c(0, 1, 2, 3, 4, 5, 6, 7),
                     labels = c("1", "10", "100", "1,000", "10,000", "100,000", "1,000,000", "10,000,000")) + 
  scale_y_continuous(name = "Government Response Index level",
                     breaks = c(0, 20, 40, 60, 80, 100)) + 
  labs(caption = "Source: Oxford COVID-19 Government Response Tracker. More at https://github.com/OxCGRT/covid-policy-tracker 
       or bsg.ox.ac.uk/covidtracker") +
  theme(
    # Remove panel border
    panel.border = element_blank(),  
    # Remove panel grid lines
    panel.grid.major = element_line(size = 0.5, linetype = "dashed", colour = "grey"),
    #panel.grid.minor = element_blank(),
    # Remove panel background
    panel.background = element_blank(),
    # Add axis line
    axis.line = element_line(colour = "grey"),
    plot.caption = element_text(hjust = 0.5, face = "italic"))
  
ggsave(plot, 
       filename = "./images/OxCGRT_govresponse_vs_cases.png",
       height = 6, 
       width = 10)

# Global means charts of 180 countries for all indices ----------------------

message("Start G4")

temp_tibble <- 
  oxcgrtdata %>%
  ungroup() %>%
  group_by(Date) %>% 
  summarise(gri_mean = mean(GovernmentResponseIndexForDisplay, na.rm = T),
            si_mean = mean(StringencyIndexForDisplay, na.rm = T), 
            chi_mean = mean(ContainmentHealthIndexForDisplay, na.rm = T), 
            esi_mean = mean(EconomicSupportIndexForDisplay, na.rm = T)) %>%
  mutate(Date = lubridate::ymd(Date)) %>%
  pivot_longer(-c(Date), names_to = "index_name", values_to = "index_value") %>%
  ungroup()

maxDate <- max(temp_tibble$Date, na.rm = T)
okabe <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
withr::with_options(
  list(ggplot2.discrete.fill = okabe),
  plot <-
    ggplot(data = temp_tibble, aes(x = Date, y = index_value, colour = index_name, group = index_name)) + 
    geom_line(aes(colour = index_name), size = 0.8) + 
    labs(x = "Date", 
         y = "",
         title = "Relationship between number of COVID-19 cases and government response",
         caption = "Source: Oxford COVID-19 Government Response Tracker. More at https://github.com/OxCGRT/covid-policy-tracker or bsg.ox.ac.uk/covidtracker") + 
    expand_limits(y = c(0, 100)) +
    scale_y_continuous(breaks = c(0, 20, 40, 60, 80, 100)) +
    scale_x_date(breaks = seq.Date(from = ymd(as.Date("2020-01-01")), to = maxDate, by = "1 month"),
                 date_labels = "%d-%b") +
    scale_color_discrete(name = "", labels = c("Containment and Health Index", 
                                               "Economic Support Index",
                                               "Government Response Index", 
                                               "Stringency Index")) +
    theme(
      # Remove panel border
      panel.border = element_blank(),  
      # Remove panel grid lines
      panel.grid.major = element_line(size = 0.5, linetype = "dashed", colour = "grey"),
      #panel.grid.minor = element_blank(),
      # Remove panel background
      panel.background = element_blank(),
      # Add axis line
      axis.line = element_line(colour = "grey"),
      plot.caption = element_text(hjust = 0.5, face = "italic"),
      legend.position = "bottom", 
      legend.title = element_blank(), 
      legend.key = element_blank()) 
  #guides(fill = guide_legend(keywidth = 0.5, 
  #                           keyheight = 0.5,
  #                           default.unit = 'cm'
  #))
  #legend.spacing.x = unit(0.5, 'cm'))
)

ggsave(plot = plot, 
       filename = "./images/OxCGRT_indices_vs_time.png", 
       width = 10, 
       height = 5)


# temp_tibble <- 
#   temp_tibble %>%
#   pivot_longer(-c(Date), names_to = "index_name", values_to = "index_value") %>%
#   mutate(index_name = case_when(index_name == "gri_mean" ~ "Government Response Index", 
#                                 index_name == "si_mean" ~ "Stringency Index", 
#                                 index_name == "esi_mean" ~ "Economic Support Index", 
#                                 index_name == "chi_mean" ~ "Containment and Health Index"))
# 
# ggplot(data = temp_tibble, aes(x = Date, group = index_name)) + 
#   geom_line(aes(y = index_value, colour = index_name)) + 
#   theme(legend.title = element_blank(),
#         legend.position = "bottom",
#         panel.border = element_blank(),  
#         # Remove panel grid lines
#         panel.grid.major = element_line(size = 0.01, linetype = "dashed", colour = "grey"),
#         #panel.grid.minor = element_blank(),
#         # Remove panel background
#         panel.background = element_blank(),
#         # Add axis line
#         axis.line = element_line(colour = "grey")) + 
#   scale_y_continuous(name = "", breaks = c(0, 20, 40, 60, 80, 100)) + 
#   expand_limits(y = 100) + 
#   scale_color_brewer(palette = "RdYlBu")

# Chloropleth Maps for GRI ----------------------------------------

message("Start G5")

## import base sf
world <- ne_countries(scale = "medium",returnclass = "sf")

world <- left_join(world %>% 
                     filter(iso_a3 %in% unique(oxcgrtdata$CountryCode)), 
                   oxcgrtdata %>% 
                     mutate(Date = ymd(Date)) %>%
                     filter(Date == Sys.Date()-2) %>% 
                     select(CountryCode, GovernmentResponseIndexForDisplay), 
                   by = c("iso_a3" = "CountryCode"))

world <- 
  world %>%
  mutate(GRI_cat = case_when(GovernmentResponseIndexForDisplay <= 20 ~ "1",
                         GovernmentResponseIndexForDisplay > 20 & GovernmentResponseIndexForDisplay <= 40 ~ "2",
                         GovernmentResponseIndexForDisplay > 40 & GovernmentResponseIndexForDisplay <= 60 ~ "3",
                         GovernmentResponseIndexForDisplay > 60 & GovernmentResponseIndexForDisplay <= 80 ~ "4",
                         GovernmentResponseIndexForDisplay > 80 & GovernmentResponseIndexForDisplay <= 100 ~ "5",
                         is.na(GovernmentResponseIndexForDisplay) ~ "0"))

cols <- c("1" = "#FFFFFF", "2" = "#eff3ff", "3" = "#bdd7e7", "4" = "#6baed6", "5" = "#03396c", "0" = "#aaaaaa")
plot <- 
  ggplot(data = world) + 
  geom_sf(aes(fill = GRI_cat)) + 
  scale_fill_manual(values = cols,
                    name = "Current Government Response Index level",
                    breaks = c("1", "2", "3", "4", "5", "0"),
                    labels = c("0-20", "20-40", "40-60", "60-80", "80-100", "No data")) +
#  scale_fill_fermenter(breaks = c(0, 20, 40, 60, 80, 100), palette = "Blues", direction = 1, 
#                       labels = c("0-20", "20-40", "40-60", "60-80", "80-100", "100"),
#                       name = "Current Government Response Index level") +
  labs(caption = "Source: Oxford COVID-19 Government Response Tracker. More at https://github.com/OxCGRT/covid-policy-tracker 
       or bsg.ox.ac.uk/covidtracker",
       title = "Map of government responses to COVID-19") +
  theme_map() + 
  theme(
    legend.key = element_blank(), 
    plot.caption = element_text(hjust = 0.5, face = "italic"),
    legend.title = element_text()
    #legend.position = "bottom"
  )

ggsave(plot = plot, 
       filename = "./images/OxCGRT_worldmap_govresponse.png", 
       width = 12, 
       height = 8)

# Chloropleth Maps for School Responses -----------------------------------

message("Start G6")

## generating tibble for first cases
firstcase_tibble <- 
  oxcgrtdata %>%
  group_by(CountryCode) %>%
  arrange(CountryCode, Date) %>%
#  fill(`C1_School closing`, .direction = "down") %>%
  filter(round(ConfirmedCases) > 0 & (is.na(lag(ConfirmedCases)) | lag(ConfirmedCases == 0))) %>%
  slice(n = 1) %>%
  select(CountryCode, Date, `C1_School closing`) %>%
  rename(firstcaseDate = Date) %>%
  ungroup()

## 
schoolclosure_tibble <- 
  oxcgrtdata %>%
  group_by(CountryCode) %>%
  arrange(CountryCode, Date) %>%
  fill(`C1_School closing`, .direction = "down") %>%
  filter(`C1_School closing` > 1 & lag(`C1_School closing` <= 1)) %>%
  slice(n = 1) %>%
  select(CountryCode, Date) %>%
  rename(schoolclosureDate = Date) %>%
  ungroup()
  
temp_tibble <- left_join(firstcase_tibble, schoolclosure_tibble, by = ("CountryCode"))

temp_tibble <- 
  temp_tibble %>%
  filter(!is.na(firstcaseDate)) %>%
  mutate(schoolclosureDate = ifelse(is.na(schoolclosureDate), (Sys.Date() - 1), schoolclosureDate),
         schoolclosureDate = ymd(as.Date(schoolclosureDate)),
         days = schoolclosureDate - firstcaseDate,
         days = ifelse((days < 0 & `C1_School closing` <= 1) | (days < 0 & is.na(`C1_School closing`)), ymd(Sys.Date() - 1) - firstcaseDate, days),
         days_cat = case_when(days >= 0 & days <= 14 ~ "1",
                              days > 14 & days <= 28 ~ "2",
                              days > 28 & days <= 42 ~ "3",
                              days > 42 & days <= 76 ~ "4",
                              days > 76 ~ "5",
                              days < 0 & days >= -28 ~ "-1",
                              days < -28 & days >= -56 ~ "-2",
                              days < -56 ~ "-3"))

#world <- ne_countries(scale = "medium",returnclass = "sf")

world <- left_join(world %>% 
                     filter(iso_a3 %in% unique(temp_tibble$CountryCode)), 
                   temp_tibble, 
                   by = c("iso_a3" = "CountryCode"))

cols <- c("5" = "#40004b", "4" = "#762a83","3" = "#9970ab",  "2" = "#c2a5cf", "1" = "#e7d4e8", "-1" = "#d9f0d3", "-2" = "#5aae61", "-3" = "#00441b")
plot <- 
  ggplot(data = world) + 
  geom_sf(aes(fill = days_cat)) + 
  theme_map() +
  labs(caption = "Source: Oxford COVID-19 Government Response Tracker. More at https://github.com/OxCGRT/covid-policy-tracker 
       or bsg.ox.ac.uk/covidtracker",
       title = "Map of school closures in responses to COVID-19") +
  scale_fill_manual(values = cols,
                    breaks = c("5", "4", "3", "2", "1", "-1", "-2", "-3"),
                    labels = c(">76", "42 to 76", "28 to 42","14 to 28", "0 to 14", "-28 to 0", "-56 to -28", "<-56"),
                    name = "How many days after the first case did schools close") + 
  theme(
    plot.caption = element_text(hjust = 0.5, face = "italic"),
    legend.title = element_text(),
    legend.background = element_blank(),
    plot.margin=grid::unit(c(0,0,0,0), "mm")
    #legend.position = "bottom"
  )

ggsave(plot = plot, 
       filename = "./images/OxCGRT_worldmap_schools.png",
       width = 12, 
       height = 8)

## country charts of daily deaths v.s. GRI -------------------------------

message("Start G7")

temp_tibble <- 
  oxcgrtdata %>%
  select(CountryCode, CountryName, Date, ConfirmedDeaths, GovernmentResponseIndexForDisplay) %>%
  group_by(CountryCode) %>%
  arrange(CountryCode, Date) %>%
  mutate(daily_deaths = ConfirmedDeaths - lag(ConfirmedDeaths),
         daily_deaths = ifelse(daily_deaths < 0, NA, daily_deaths)) %>%
  fill(daily_deaths, .direction = "down") %>%
  mutate(ave_7day_deaths = zoo::rollmean(daily_deaths, k = 7, fill = NA),
         log10_dailydeaths = log10(ave_7day_deaths + 1)) %>%
  ungroup()

for(i in unique(temp_tibble$CountryName)){
  #max_logdeaths <- ceiling(max(unique(temp_tibble %>% filter(CountryName == as.symbol(i)) %>% pull(log10_dailydeaths)), na.rm = T))
  max_logdeaths <- 4
  coeff <- 100/max_logdeaths
  maxDate <- max(temp_tibble$Date, na.rm = T)
  plot <- 
    ggplot(data = temp_tibble %>% filter(CountryName == as.symbol(i)), aes(x = Date)) + 
    geom_line(aes(y = log10_dailydeaths), colour = "purple", size = 0.8) + 
    geom_line(aes(y = GovernmentResponseIndexForDisplay/coeff), colour = "red", size = 0.8) +
    scale_y_continuous(name = "Daily deaths (7 day rolling average)",
                       breaks = seq(0, max_logdeaths),
                       labels = comma(do.call(rbind,lapply(seq(0, max_logdeaths), function(x){return(10^x)}))),
                       sec.axis = sec_axis(~.*coeff,
                                           name = "Government Response Index",
                                           breaks = c(0, 20, 40, 60, 80, 100), 
                                           labels = c(0, 20, 40, 60, 80, 100))) + 
    expand_limits(y = c(0, max_logdeaths)) + 
    scale_x_date(breaks = seq.Date(from = ymd(as.Date("2020-01-01")), to = maxDate, by = "1 month"),
                 date_labels = "%d-%b") +
    labs(title = paste0(i, "'s Covid-19 Trajectory"),
         caption = "Source: Oxford COVID-19 Government Response Tracker. More at https://github.com/OxCGRT/covid-policy-tracker 
       or bsg.ox.ac.uk/covidtracker") +
    theme(
      # Remove panel border
      axis.text.y.right = element_text(colour = "red"),
      axis.title.y.right = element_text(colour = "red"),
      axis.text.y.left = element_text(colour = "purple"),
      axis.title.y.left = element_text(colour = "purple"),
      panel.border = element_blank(),  
      # Remove panel grid lines
      panel.grid.major = element_line(size = 0.5, linetype = "dashed", colour = "grey"),
      #panel.grid.minor = element_blank(),
      # Remove panel background
      panel.background = element_blank(),
      # Add axis line
      axis.line = element_line(colour = "grey"),
      plot.caption = element_text(hjust = 0.5, face = "italic"),
      plot.title = element_text(hjust = 0.5),
      plot.margin=grid::unit(c(0,0,0,0), "mm"))
  ggsave(plot ,
         filename = paste0("./images/country_charts/",i, ".png"),
         width = 12,
         height = 8)
    
}

## US state charts of daily deaths v.s. GRI -------------------------------

message("Start G8")

temp_tibble <- 
  oxcgrtUS %>%
  select(RegionCode, RegionName, Date, ConfirmedDeaths, GovernmentResponseIndexForDisplay) %>%
  group_by(RegionCode) %>%
  arrange(RegionCode, Date) %>%
  mutate(daily_deaths = ConfirmedDeaths - lag(ConfirmedDeaths),
         daily_deaths = ifelse(daily_deaths < 0, NA, daily_deaths)) %>%
  fill(daily_deaths, .direction = "down") %>%
  mutate(ave_7day_deaths = zoo::rollmean(daily_deaths, k = 7, fill = NA),
         log10_dailydeaths = log10(ave_7day_deaths + 1)) %>%
  ungroup()

for(i in unique(temp_tibble$RegionName)){
  #max_logdeaths <- ceiling(max(unique(temp_tibble %>% filter(CountryName == as.symbol(i)) %>% pull(log10_dailydeaths)), na.rm = T))
  max_logdeaths <- 4
  coeff <- 100/max_logdeaths
  maxDate <- max(temp_tibble$Date, na.rm = T)
  plot <- 
    ggplot(data = temp_tibble %>% filter(RegionName == as.symbol(i)), aes(x = Date)) + 
    geom_line(aes(y = log10_dailydeaths), colour = "purple", size = 0.8) + 
    geom_line(aes(y = GovernmentResponseIndexForDisplay/coeff), colour = "red", size = 0.8) +
    scale_y_continuous(name = "Daily deaths (7 day rolling average)",
                       breaks = seq(0, max_logdeaths),
                       labels = comma(do.call(rbind,lapply(seq(0, max_logdeaths), function(x){return(10^x)}))),
                       sec.axis = sec_axis(~.*coeff,
                                           name = "Government Response Index",
                                           breaks = c(0, 20, 40, 60, 80, 100), 
                                           labels = c(0, 20, 40, 60, 80, 100))) + 
    expand_limits(y = c(0, max_logdeaths)) + 
    scale_x_date(breaks = seq.Date(from = ymd(as.Date("2020-01-01")), to = maxDate, by = "1 month"),
                 date_labels = "%d-%b") +
    labs(title = paste0(i, "'s Covid-19 Trajectory"),
         caption = "Source: Oxford COVID-19 Government Response Tracker. More at https://github.com/OxCGRT/covid-policy-tracker 
       or bsg.ox.ac.uk/covidtracker") +
    theme(
      # Remove panel border
      axis.text.y.right = element_text(colour = "red"),
      axis.title.y.right = element_text(colour = "red"),
      axis.text.y.left = element_text(colour = "purple"),
      axis.title.y.left = element_text(colour = "purple"),
      panel.border = element_blank(),  
      # Remove panel grid lines
      panel.grid.major = element_line(size = 0.5, linetype = "dashed", colour = "grey"),
      #panel.grid.minor = element_blank(),
      # Remove panel background
      panel.background = element_blank(),
      # Add axis line
      axis.line = element_line(colour = "grey"),
      plot.caption = element_text(hjust = 0.5, face = "italic"),
      plot.title = element_text(hjust = 0.5),
      plot.margin=grid::unit(c(0,0,0,0), "mm"))
  ggsave(plot ,
         filename = paste0("./images/US_states/",i, ".png"),
         width = 12,
         height = 8)
  
}







# 
# 
# 
# 
# oxcgrtdata <-
#   oxcgrtdata %>%
#   group_by(CountryCode) %>%
#   arrange(CountryCode, Date) %>%
#   mutate(ConfirmedCases = na.approx(ConfirmedCases, na.rm = F), 
#          ConfirmedDeaths = na.approx(ConfirmedDeaths, na.rm = F)) %>%
#   fill(ConfirmedCases, ConfirmedDeaths, .direction = "down")
# 
# oxcgrtdata <-
#   oxcgrtdata %>%
#   group_by(CountryCode) %>%
#   arrange(CountryCode, Date) %>%
#   mutate(moveave_confirmeddeaths = zoo::rollmean(ConfirmedDeaths, k = 7, fill = NA, na.rm = T, align = 'right'), 
#         moveave_confirmeddeaths = ifelse(is.nan(moveave_confirmeddeaths), NA, moveave_confirmeddeaths),
#         lag_moveave_deaths = lag(moveave_confirmeddeaths, order_by = Date), 
#         dailydeaths = ifelse(moveave_confirmeddeaths - lag_moveave_deaths > 0, moveave_confirmeddeaths - lag_moveave_deaths, 0)) %>%
#   fill(GovernmentResponseIndexForDisplay, .direction = "down")
# 
# maxdeaths <- max((oxcgrtdata %>% filter(CountryCode == "USA") %>% filter(!is.na(dailydeaths)) %>% pull(dailydeaths)))
# coeff = floor(maxdeaths/100)
# ggplot(oxcgrtdata %>% filter(CountryCode == "USA") %>% arrange(Date), aes(x = Date, group = 1)) + 
#   geom_line(aes(y = dailydeaths), colour = "red") +
#   geom_line(aes(y = GovernmentResponseIndexForDisplay*coeff), colour = "blue") +
#   scale_y_continuous(
#     name = "Daily Deaths", 
#     #trans = log10_trans(), 
#     sec.axis = sec_axis(~./coeff, name = "GRI", breaks = c(0, 20, 40, 60, 80, 100)), 
#     limits = c(1, maxdeaths)) + 
#   theme_ipsum()
#   
# 
# 
# oxcgrtdata <- 
#   oxcgrtdata %>% 
#   mutate(Date = lubridate::ymd(Date)) %>%
#   group_by(CountryCode) %>%
#   arrange(CountryCode,Date) %>%
#   mutate(hundredth_case_date = ifelse((lag(ConfirmedCases) < 100) & ConfirmedCases >= 100, Date, NA),
#          hundredth_case_date = as.Date(hundredth_case_date)) %>%
#   #         thousandth_case_date = ifelse((lag(ConfirmedCases) < 1000) & ConfirmedCases >= 1000, Date, NA),
#   #         thousandth_case_date = as.Date(thousandth_case_date),
#   #         tenthousandth_case_date = ifelse((lag(ConfirmedCases) < 10000) & ConfirmedCases >= 10000, Date, NA),
#   #         tenthousandth_case_date = as.Date(tenthousandth_case_date))%>% 
#   fill(c(hundredth_case_date),  .direction = "updown") %>%
#   mutate(days_since_hundredth_case = Date - lubridate::ymd(hundredth_case_date)) %>%
#   #         days_since_thousandth_case = Date - lubridate::ymd(thousandth_case_date),
#   #         days_since_tenthousandth_case = Date - lubridate::ymd(tenthousandth_case_date)) %>%
#   select(-ends_with("case_date"))
# 
# key_countries <- c("CHN", "KOR", "USA", "FRA","GBR", "ITA")
# 
# maxcases <- max((oxcgrtdata %>% filter(CountryCode == "USA") %>% filter(!is.na(ConfirmedCases)) %>% pull(ConfirmedCases)))
# coeff = floor(maxcases/100)
# ggplot(data = oxcgrtdata %>% filter(CountryCode == "USA"), 
#        aes(x = days_since_hundredth_case))  + 
#   geom_line(aes(y = ConfirmedCases), colour = "red") +
#   geom_line(aes(y = GovernmentResponseIndexForDisplay*coeff), colour = "blue") + 
#   scale_y_continuous(
#     name = "Confirmed Cases", 
#     trans = log10_trans(),
#     sec.axis = sec_axis(~./coeff, name = "GRI")
#   ) + 
#   theme_ipsum() + 
#   facet_wrap(~CountryCode)
# 
# key = "50622c88625a72597fe88a322b1b0dd31885baa0"
# 
# library(httr)
# a <- GET("https://raw.githubusercontent.com/OxCGRT/Primary-Analysis-Files/main/oxcgrt_primaryanalysis_national.csv?", 
#          authenticate(Sys.getenv(key), ""))
# 
# gtoken <- config(token = key)
# a <- GET("https://raw.githubusercontent.com/OxCGRT/Primary-Analysis-Files/main/oxcgrt_primaryanalysis_national.csv",
#      add_headers(Authorization='Token 50622c88625a72597fe88a322b1b0dd31885baa0'))
# 
# content(a, 'text')
# a <- read_csv(url("https://raw.githubusercontent.com/OxCGRT/Primary-Analysis-Files/main/oxcgrt_primaryanalysis_national.csv?token=ACVSZD5MHHPAA6QHVNEG2M27VHMNW"))
#   
# 
# 
# https://raw.githubusercontent.com/OxCGRT/Primary-Analysis-Files/main/oxcgrt_primaryanalysis_subnational.csv?token=ACVSZD4BXBHJ2ATKD6PAH6K7VMVWC
