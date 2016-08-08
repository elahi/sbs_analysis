################################################################################
##' @title Functions for returning desired hms sst data from complete hms sst dataframe
##'
##' @author Robin Elahi
##' @contact elahi.robin@gmail.com
##' @contributor 
##' 
##' @date 2016-08-03
##' 
##' @log Add a log here
################################################################################

##' temp_df = sst_hms
##' 
##' size_data: wide data frame of site-level summarized species size data with study, species, year, year2
##' 
##' duration = number of year prior to the year of sampling for which I want temperature data
##' 

##### GET HMS SST FOR TWO PERIODS #####

glimpse(sst_hms)

temp_df = sst_hms
size_data = dat4
duration = 10

get_hms_two_periods <- function(temp_df = sst_hms, size_data, duration = 10) {
  
  nYrsPrior <- duration - 1
  
  # is there a study column in the size_data?
  if(!"study" %in% colnames(size_data)) {
    size_data$study <- "Elahi2015"
  }
  
  spYrs <- size_data %>% select(study, species, year) %>% distinct() %>% 
    arrange(species, year) %>% 
    mutate(era = rep(c("past", "present"), 3)) %>% 
    spread(key = era, value = year) %>% 
    mutate(hadPres1 = floor(present - nYrsPrior),
           hadPres2 = floor(present), 
           hadPast1 = floor(past - nYrsPrior), 
           hadPast2 = floor(past))
  
  dat.i <- spYrs 
  temp_df <- temp_df %>% mutate(study = "Elahi2015")
  
  tempDat.i <- inner_join(temp_df, dat.i, by = "study") %>% 
    mutate(past_data = ifelse(year >= hadPast1 & year <= hadPast2, 
                              "past", NA), 
           pres_data = ifelse(year >= hadPres1 & year <= hadPres2, 
                              "pres", NA)) %>% 
    filter(!is.na(past_data) | !is.na(pres_data)) %>% 
    mutate(era = ifelse(is.na(past_data), "present", past_data)) %>% 
    select(-c(hadPres1:pres_data))
  
  return(tempDat.i)
  
}

##### GET HADISST TIME SERIES #####

get_had_series <- function(had_df = dfHad, size_data) {
  
  spYrs <- size_data %>% select(study, species, year, year2) %>%
    distinct() %>%
    mutate(hadYr1 = floor(year), 
           hadYr2 = floor(year2)) %>% 
    group_by(study, species) %>% slice(1) %>%
    ungroup() %>% filter(!is.na(hadYr2))
  
  dat.i <- spYrs
  
  datHad.i <- inner_join(had_df, dat.i, by = "study") %>% 
    mutate(species1 = ifelse(Year >= hadYr1 & Year <= hadYr2, 
                             "keep", "remove")) %>% 
    filter(species1 == "keep") %>% select(-c(X, species1))
  
  return(datHad.i)
  
}



