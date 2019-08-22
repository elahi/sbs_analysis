#################################################
# Author: Robin Elahi
# Date: 190820

# Correct tidal heights
#################################################

# Get tidal correction
source("R/buttonPositions.R")

# Get snail size data with original tidal heights
source("snail_body_size_hms_prep/01_snail_body_size_hms_prep.R")

# Apply tidal correction, using mod1 (all three species included)
mod <- mod %>% 
  mutate(tideHTm_orig = tideHTm/3.28084, 
         tideHTm = tideHTm_orig * coef(mod1)[2] + coef(mod1)[1])

hexPast <- hexPast %>% 
  mutate(tideHTm_orig = tideHTm/3.28084, 
         tideHTm = tideHTm_orig * coef(mod1)[2] + coef(mod1)[1])

summary(mod)
summary(hexPast)

##### WRITE THE MODERN AND LOTTIA DATA TO OUTPUT #####

hexPast <- hexPast %>% select(-tideHTm_orig)
write.csv(hexPast, "./data/hexter_raw.csv")

mod <- mod %>% select(-tideHTm_orig)
write.csv(mod, './output/snail_body_size_hms_modern.csv')
