#### Lockdown Rollback Checklist

# import packages
library(here)
library(dplyr)
library(tidyr)
library(RcppRoll)
library(feather)
library(zoo)

here()
# define global macros
data_date <- lubridate::today()

# read in base csv file
oxcgrtdata <- read.csv(file = paste("./data/input/OxCGRT_", data_date, ".csv", sep = ""), stringsAsFactors = FALSE)

# Filling in gaps in indicators
## Code Optimisation Notes - can use lapply here
# BUG WARNING - if not converted to csv, some issues where "H3_Contact.Tracing" doesn't exist 
# -> it's stored as "H3_Contact Tracing"
oxcgrtdata <- oxcgrtdata %>% arrange(CountryCode, Date) %>% group_by(CountryCode) %>%
  mutate(H3_Contact.tracing_1 = H3_Contact.tracing, 
         H2_Testing.policy_1 = H2_Testing.policy, 
         C8_International_1 = C8_International.travel.controls,
         H1_Public.info_1 = H1_Public.information.campaigns) %>% 
  fill(H3_Contact.tracing_1, H2_Testing.policy_1, C8_International_1, H1_Public.info_1) %>% 
  mutate(ConfirmedCases = na.approx(ConfirmedCases, na.rm = F), 
         ConfirmedDeaths = na.approx(ConfirmedDeaths, na.rm = F))


### Define cases_controlled metric
## Compute 7-day rolling avg of cases
oxcgrtdata <- oxcgrtdata %>% arrange(CountryCode, Date) %>% group_by(CountryCode) %>%
  mutate(moveave_confirmedcases = zoo::rollmean(ConfirmedCases, k = 7, fill = NA, na.rm = T, align = 'right'), 
         moveave_confirmedcases = ifelse(is.nan(moveave_confirmedcases), NA, moveave_confirmedcases)) %>%
  mutate(lag_moveave_cases = lag(moveave_confirmedcases, order_by = Date), 
         newcases = ifelse(moveave_confirmedcases - lag_moveave_cases > 0, moveave_confirmedcases - lag_moveave_cases, 0), 
         cases_controlled = ifelse((newcases)/50 < 1, (newcases)/50, 1)) 
##FROM TOBY: I can't quite tell if you have handled this elsewhere, but you may need to account for cases that will end up >1. This can occur in uncommon instance where countries revise down their count, giving "negative" newcases.
#' FROM SAPTA: proposed fix -> Since -ve newcases can occur at multiple points along the time series, I could think of only 1 option here - 
#' => set newcases = 0 wherever newcases<0 => we assume a flat moveave_cases wherever the curve drops

### define test and trace indicators

#' define test numbers for each country by date 
#' -> by(date): if missing test_data, test_data = latest test data
#' define max_tests -> max(test_data) by Date
oxcgrtdata <- oxcgrtdata %>% arrange(CountryCode, Date) %>% group_by(CountryCode) %>% fill(test_percase)

# CODE BUG ALERT - Inf handling in test_percase, min_tests and max_tests
oxcgrtdata <- oxcgrtdata %>% arrange(Date, CountryCode) %>% group_by(Date) %>%
  mutate(min_tests = ifelse(is.finite(min(test_percase, na.rm = T)),min(test_percase, na.rm = T), NA), 
         max_tests = ifelse(is.finite(max(test_percase, na.rm = T)),max(test_percase, na.rm = T), NA))  %>% 
  mutate(test_score = ifelse(!is.na(max_tests) & !is.na(min_tests) & (max_tests > 0) & (min_tests > 0) & (max_tests != min_tests), 
                             (log(test_percase) - log(min_tests))/(log(max_tests) - log(min_tests)), NA))

oxcgrtdata <- oxcgrtdata %>% group_by(Date) %>% mutate(global_mean_test_score = ifelse(is.finite(mean(test_score, na.rm = T)), mean(test_score, na.rm = T), NA)) %>% 
  mutate(test_score = ifelse(is.na(test_score), global_mean_test_score, test_score)) %>%
  ungroup() %>%
  mutate(test_score = ifelse(is.na(test_nodata), test_score, 0))

oxcgrtdata <- oxcgrtdata %>% mutate(test_and_trace = 1 - (0.25*H3_Contact.tracing_1/2 + 0.25*H2_Testing.policy_1/3 + 0.5*test_score)) 



#####################
## New openness risk calculations 
#' 1. Change in manage_imported_cases indicator
#' 2. Change in cases_controlled
#' 3. Invert score since this is openness risk, not rollback readiness
#####################

# Updating definition of manage_imported_cases
oxcgrtdata <- oxcgrtdata %>% mutate(manage_imported_cases = case_when(C8_International_1 == 0 ~ 1, 
                                                                      C8_International_1 == 1 ~ 0.5, 
                                                                      C8_International_1 == 2 ~ 0.25, 
                                                                      C8_International_1 > 2 ~ 0))


### Behaviour change and community engagement

# ISSUE(FIXED) - no apple_ave or google_ave variable from previous code
# Code correction notes - creating them now for completion - to be removed later
#oxcgrtdata <- oxcgrtdata %>% ungroup() %>% 
#  mutate(apple_ave = rowMeans(oxcgrtdata[,c("week_apple_transit", "week_apple_driving", "week_apple_walking")]), 
#         google_ave = rowMeans(oxcgrtdata[,c("week_goog_retail", "week_goog_transitstations", "week_goog_workplaces")]))

#' CODE CORRECTION NOTE (FIXED) - Taking min at each date for time series purposes. Stata Code takes 
#' global(within country until date) min of google_ave and apple_ave, before taking
#'  min between these 
##FROM TOBY: this is also worth noting of the min/max range for tests above.
oxcgrtdata <- oxcgrtdata %>% arrange(CountryCode, Date) %>% group_by(CountryCode) %>% 
  mutate(min_google = roll_min(google_ave, n = 28L, align = "right",fill = NA, na.rm = T), 
         min_apple = roll_min(apple_ave, n = 28L, align = "right", fill = NA, na.rm = T),
         min_google = ifelse(is.infinite(min_google), NA, min_google),
         min_apple = ifelse(is.infinite(min_apple), NA, min_apple)) 

oxcgrtdata <- oxcgrtdata %>% mutate(mob = pmin(min_apple, min_google, na.rm = T), 
                                    mob = case_when(mob < 20 ~ 20,
                                                    mob > 20 & mob < 120 ~ mob,
                                                    mob > 120 & (is.na(mob) == F) ~ 120))

#NA handling of mob - what happens when mob is 0? codebook says metric is left blank. Which metric? - confirm
##FROM TOBY:  are there cases of mob=0?? If so, then I guess it is <20 and would be treated as 20 for this equation. But 0 seems implausibly low... the absolute zero of mobility! 
##            More likely is a null value, where google_ave and apple_ave are empty for a particular country. In which case we do not report a community_understanding metric, we leave it blank.

#oxcgrtdata <- oxcgrtdata %>% mutate(community_understanding = 0.5*cases_controlled + (1-0.5*cases_controlled)*(120-mob)/100) %>%
#  mutate(community_understanding = ifelse(H1_Public.info_1!=2, 0, community_understanding)) 

oxcgrtdata <- oxcgrtdata %>% mutate(community_understanding = 0.5*(1 + cases_controlled)*(mob-20)/100) %>%
    mutate(community_understanding = ifelse(H1_Public.info_1!=2, 1, community_understanding)) 

##------------------FINAL INDEX--------------------

# Updating old score to reflect the change in manage_imported cases
oxcgrtdata$openness_risk <- rowMeans(oxcgrtdata[c("community_understanding", "test_and_trace",
                                                   "manage_imported_cases", "cases_controlled")], na.rm = T)

#oxcgrtdata <- oxcgrtdata %>% mutate(openness_risk = 1 - rollback_score)
#write.csv(oxcgrtdata, file = paste("../data/output/OxCGRT_", data_date, ".csv", sep = ""))

## Adding in an endemic factor calculation 
#' Endemic factor defined as metric between [0-1] for cases/mill between 50-200

## creating a cases per million variable
oxcgrtdata <- oxcgrtdata %>% mutate(newcases_permillion = newcases/(popWB/1000000), 
                                    endemic_factor = case_when(newcases_permillion < 50 ~ 0, 
                                                               newcases_permillion > 200 ~ 1, 
                                                               newcases_permillion < 200 & !is.na(newcases_permillion) ~ (newcases_permillion - 50)/150)) 

## recalculating rollback score
oxcgrtdata <- oxcgrtdata %>% mutate(openness_risk = ifelse(!is.na(endemic_factor), endemic_factor + (1 - endemic_factor)*openness_risk, openness_risk))

## recalibrating openness index for the start dates 
oxcgrtdata <- oxcgrtdata %>% 
  mutate(openness_risk = ifelse(is.na(cases_controlled), NA, openness_risk))

write.csv(oxcgrtdata, file = paste("./data/input/OxCGRT_", data_date, ".csv", sep = ""))
write_feather(oxcgrtdata, path = "./data/input/OxCGRT_latest.feather")

## Creating a custom csv file for timeseries 
ORI_output <- 
  oxcgrtdata %>%
  select(CountryCode, CountryName, Date, community_understanding, manage_imported_cases, cases_controlled, test_and_trace,
         endemic_factor, openness_risk)

write.csv(ORI_output, file = paste("./data/riskindex_timeseries_latest", ".csv", sep = ""))

###---------------------End of current code - clean up anything after this-------------------###












