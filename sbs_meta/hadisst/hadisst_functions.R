################################################################################
##' @title Functions for returning desired hadisst data from complete had dataframe
##'
##' @author Robin Elahi
##' @contact elahi.robin@gmail.com
##' @contributor 
##' 
##' @date 2016-08-03
##' 
##' @log Add a log here
################################################################################

##' had_df = dfHad : (complete hadisst for each site in meta-analysis)
##' 
##' size_data: wide data frame of site-level summarized species size data with study, species, year, year2
##' 
##' duration = number of year prior to the year of sampling for which I want temperature data
##' 

##### GET HADISST FOR TWO PERIODS #####

get_had_two_periods <- function(had_df = dfHad, size_data, duration = 10) {
  
  nYrsPrior <- duration - 1
  
  spYrs <- size_data %>% select(study, species, year, year2) %>%
    distinct() %>%
    mutate(hadPres1 = floor(year2 - nYrsPrior),
           hadPres2 = floor(year2), 
           hadPast1 = floor(year - nYrsPrior), 
           hadPast2 = floor(year)) %>% 
    group_by(study, species) %>% slice(1) %>%
    ungroup() %>% filter(!is.na(year))
  
  dat.i <- spYrs # %>% filter(species == species.i)
  
  datHad.i <- inner_join(had_df, dat.i, by = "study") %>% 
    mutate(past_data = ifelse(Year >= hadPast1 & Year <= hadPast2, 
                              "past", NA), 
           pres_data = ifelse(Year >= hadPres1 & Year <= hadPres2, 
                              "pres", NA)) %>% 
    filter(!is.na(past_data) | !is.na(pres_data)) %>% 
    mutate(era = ifelse(is.na(past_data), "present", past_data)) %>% 
    select(-c(X, hadPres1:pres_data))
  
  return(datHad.i)
  
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



