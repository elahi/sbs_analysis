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

csvList <- strsplit(df$csvFile, split = "_")

csv_df <- data.frame(matrix(unlist(csvList), nrow = 48, byrow = TRUE), 
                     stringsAsFactors = FALSE) 
csv_df

sppList <- strsplit(csv_df$X3, split = "[.]")

spp_df <- data.frame(matrix(unlist(sppList), nrow = 48, byrow = TRUE), 
                     stringsAsFactors = FALSE) 

df$species <- spp_df$X1

fig2_df <- df %>% select(species, temporalBin, length_ln, csvFile )
head(fig2_df)

# write to file
write.csv(fig2_df, file = paste(path, "Roy2003_fig2_scraped.csv"))

