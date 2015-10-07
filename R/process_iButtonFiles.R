#################################################
# Author: Robin Elahi
# Date: 151005

# Function to compile iButton temperature files
# into one csv file
#################################################

# The following function assumes the following directory structure
# Main project folder
#   - data - iButtonFiles (csv files to be collated)
#   - R - this file (process_iButtonFiles.R)
#   - script to further massage the master temperature data

# Raw iButton files are placed in a directory called "iButtonFiles", 
# within 'data' directory

# This processing file is in the "R" directory: where is this?
getwd()

# This call gets you the files within the present working directory
dir() # OR
dir(path = ".")

# What if I want the directory above?
dir(path = "..")

# Now choose the appropriate folder, in this case "iButtonFiles"
dir(path = "./data/iButtonFiles")

######################################################
# Need a list of file names from the iButtonFiles
fileNames <- dir(path = "./data/iButtonFiles", recursive = TRUE, 
               pattern = ".csv") 

######################################################
# Goal is to create a single data file with 4 columns:
# date/time
# temperature (Value)
# csv file name
# iButton registration number 
# (in this case, the latter two should be the same)

iButtonTempF <- function(fls) {
	# get names of all files
	fls <- fileNames
	emptyMat <- as.data.frame(matrix(nrow = 0, ncol = 4))
	names(emptyMat) <- c("iButtonID", "spreadsheetID", "dateR", 
	                     "tempC")
	
	for (f in 1:length(fls)) {
		
	  ##########################
		# extract iButtonID
	  paste("./data/iButtonFiles/", fls[f], sep = "")
	 
		ibutton <- read.csv(file = paste("./data/iButtonFiles/", fls[f], sep = ""), 
		                    skip = 1, nrow = 1, header = FALSE, stringsAsFactors = FALSE)
		ibutton
		
		# at which value does the registration number start?
		startZeros <- regexpr(": ", ibutton)[1] # position 35
		
		# position where iButtonID starts 
		iButtonIDstart <- startZeros + 2 
		
		# extract iButtonID	
		iButtonID <- substr(ibutton, start = iButtonIDstart, stop = nchar(ibutton)) 
		
		##########################
		# Extract spreadsheet ID
		# All the names are 20 characters
		nchar(fileNames)
		csvID <- substr(fileNames[f], start = 1, stop = 16)

		##########################
		# Extract temperature Data
		tempDat <- read.csv(file = paste("./data/iButtonFiles/", fls[f], sep = ""), 
		                    skip = 14)
		# convert time stamps to POSIX object
		dateTimeR <- as.POSIXct(strptime(substr(tempDat$Date.Time, 1, 20), 
			format = '%m/%d/%y %I:%M:%S %p'))
		tempDat$dateTimeR <- dateTimeR

		# output data
		tempDat2 <- data.frame(iButtonID,csvID, 
			tempDat$dateTimeR, tempDat$Value)
		names(tempDat2)[3:4] <- c("dateR", "tempC")

		# return data
		emptyMat <- rbind(emptyMat, tempDat2)
	}
	return(emptyMat)
}
