################################################################################
##' @title Assembling scraped data, Roy 2003
##'
##' @author Robin Elahi
##' @contact elahi.robin@gmail.com
##' @contributor 
##' 
##' @date 2016-06-11
##' 
##' @log Add a log here
################################################################################

##### LOAD PACKAGES #####

library(dplyr)
library(tidyr)
library(ggplot2)

# Function to take the scraped data and assemble into a single dataframe

path <- "sbs_meta/scraped/Roy2003/"

# Get list of files
fileNames <- dir(path = path, recursive = TRUE, 
                 pattern = ".csv")

fileNames

# Set up the first dataframe

paste(path, fileNames[1], sep = "")

i = 1

df <- read.csv(file = paste(path, fileNames[i], sep = ""), 
               header = FALSE)

df <- df %>% rename(length_ln = V2) %>%
  mutate(temporalBin = c(rep("Pre-1960", 3), rep("1961-1980", 3), 
                         rep("Field", 3), rep("CNM", 3)), 
         csvFile = fileNames[i])

df

# Get the remaining species
for(i in 2:length(fileNames)) { 
    
    df.i <- read.csv(file = paste(path, fileNames[i], sep = ""), 
                   header = FALSE)
    
    df.i <- df.i %>% rename(length_ln = V2) %>%
      mutate(temporalBin = c(rep("Pre-1960", 3), rep("1961-1980", 3), 
                             rep("Field", 3), rep("CNM", 3)), 
             csvFile = fileNames[i])
    
    # Add to existing dataframe
    df <- rbind(df, df.i)
  }

df

##### RENAME SITES AND TIMES #####

# Get sites
df <- df %>% 
  mutate(site = ifelse(test = (temporalBin == "Pre-1960" |
                         temporalBin == "1961-1980"), yes = "Museum", 
         no = temporalBin), 
         time_rep = ifelse(test = (temporalBin == "Pre-1960" |
                                 temporalBin == "1961-1980"), yes = temporalBin, 
                       no = "2001"), 
         time_rep = gsub(pattern = "Pre-1960", time_rep, 
                         replacement = "1885-1960"))

head(df)

##### SPECIES NAMES #####
# Get species names using string split
csvList <- strsplit(df$csvFile, split = "_")

csv_df <- data.frame(matrix(unlist(csvList), nrow = 48, byrow = TRUE), 
                     stringsAsFactors = FALSE) 
csv_df

sppList <- strsplit(csv_df$X3, split = "[.]")

spp_df <- data.frame(matrix(unlist(sppList), nrow = 48, byrow = TRUE), 
                     stringsAsFactors = FALSE) 

df$species <- spp_df$X1

# Get species names by hand
df$species <- c(rep("Acanthinucella_spirata", 12), rep("Fissurella_volcano", 12), 
             rep("Lottia_gigantea", 12), rep("Tegula_aureotincta", 12))

##### CALCULATE MEAN SE #####

# Assign the upper, mean, and lower error bars
df$value <- rep(c("upperSE", "length_mean", "lowerSE"), 16)

# Get wide format for error values
df2 <- df %>% select(- V1) %>%
  spread(., key = value, value = length_ln)
head(df2)

# Get the mean SE
df2 <- df2 %>% 
  mutate(diffLower = length_mean - lowerSE, 
         diffUpper = upperSE - length_mean, 
         diffMean = (diffLower + diffUpper)/2)

head(df2)

##### ASSIGN TIMES #####

### From Roy 2003:
# These data provide a historical time series extending 
# from 1869 to 1981 for F. volcano, 
# 1896–1975 for T. aureotincta,
# 1869–1960 for L. gigantea 
# and 1903–1985 for A. spirata. 

# Pre-1960 times = earliest date, 1959
Fvolcano1 <- mean(c(1869, 1959))
Taureotincta1 <- mean(c(1896, 1959))
Lgigantea1 <- mean(c(1869, 1959))
Aspirata1 <- mean(c(1903, 1959))

# 1961-1980 times = earliest date, 1959
Fvolcano2 <- mean(c(1961, 1980))
Taureotincta2 <- mean(c(1961, 1975))
Lgigantea2 <- NA
Aspirata2 <- mean(c(1961, 1980))

with(df2, cbind(temporalBin, species))
year <- c(Aspirata2, Fvolcano2, Lgigantea2, Taureotincta2, 
          rep(2001, 8), 
          Aspirata1, Fvolcano1, Lgigantea1, Taureotincta1)
year

df2$year <- year

df3 <- df2 %>%
  mutate(size_rep = exp(length_mean),
         size_error = exp(diffMean))

df3 %>%
  ggplot(aes(year, size_rep, color = site)) + geom_point() + 
  facet_wrap(~ species, scales = "free_y") + 
  # facet_wrap(~ species) + 
  geom_errorbar(aes(ymin = size_rep - size_error, 
                    ymax = size_rep + size_error))

##### FORMAT TABLE FOR META-ANALYSIS #####

df_final <- df3 %>% arrange(species, year, site)

dfMeta <- data.frame(
  study = "Roy2003", 
  studySub = NA, 
  fig_table = "Figure_2", 
  species = df_final$species, 
  site = df_final$site, 
  size_rep = df_final$size_rep, 
  size_units = "mm", 
  size_error = df_final$size_error, 
  size_error_type = "CI", 
  time_rep = df_final$time_rep, 
  time_error = NA, 
  year = df_final$year, 
  year_error = NA, 
  year_error_type = NA, 
  sample_size = NA, 
  sample_size_units = NA
)

unique(dfMeta$species)

write.csv(dfMeta, "sbs_meta/output/dfMeta_Roy2003.csv")
