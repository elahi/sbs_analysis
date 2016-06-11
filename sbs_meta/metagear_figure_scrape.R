################################################################################
##' @title Scrape data from figures using metagear
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
library(metagear)
library(EBImage)

##### VIGNETTE: Automated extraction of data from scatter plots  #####

### Example 1
data("Kam_et_al_2003_Fig2")
figure_display(Kam_et_al_2003_Fig2)

# Convert back to jpg
figure_write(Kam_et_al_2003_Fig2, 
             file = "sbs_meta/extract_data/Kam_et_al_2003_Fig2.jpg")

# Detect and display scatterplot objects
rawData <- figure_scatterPlot("sbs_meta/extract_data/Kam_et_al_2003_Fig2.jpg")
head(rawData)

### Example 2
figureSource <- "http://lajeunesse.myweb.usf.edu/metagear/example_2_scatterPlot.jpg"
download.file(figureSource, "sbs_meta/extract_data/example_2_scatterPlot.jpg", 
              quiet = TRUE, mode = "wb")
aFig <- figure_read("sbs_meta/extract_data/example_2_scatterPlot.jpg", 
                    display = TRUE)

# because of the small size of the image the axis parameter needed adjustment from 5 to 3
rawData2 <- figure_scatterPlot("example_2_scatterPlot.jpg",  
                               axis_thickness = 3, # adjusted from 5 to 3 to help detect the thin axis
                               X_min = 0, # minimum X-value reported in the plot
                               X_max = 50, # maximum X-value reported in the plot
                               Y_min = 0,
                               Y_max = 70)

### Example 3
# download the figure image from my website
figureSource <- "http://lajeunesse.myweb.usf.edu/metagear/example_3_scatterPlot.jpg"
download.file(figureSource, "sbs_meta/extract_data/example_3_scatterPlot.jpg", quiet = TRUE, mode = "wb")
aFig <- figure_read("sbs_meta/extract_data/example_3_scatterPlot.jpg", 
                    display = TRUE)

# tweaking the figure_scatterPlot() function to improve object detection
rawData3 <- figure_scatterPlot("sbs_meta/extract_data/example_3_scatterPlot.jpg",
                               binary_point_fill = TRUE, # set to TRUE to fill empty points
                               point_size = 9, # increase from 5 to 9 since points are large
                               binary_threshold = 0.8, # increase from 0.6 to 0.8 to include the grey objects
                               axis_thickness = 3, # decrease from 5 to 3 since axes are thin
                               X_min = 0,
                               X_max = 850,
                               Y_min = 0,
                               Y_max = 35)

# remove false detected points from the regression summary presented within the plot
cleaned_rawData3 <- rawData3[ which(!(rawData3$X < 350 & rawData3$Y > 25)), ]
# estimate the regression coefficients
lm(Y ~ X, data = cleaned_rawData3)

##### VIGNETTE: Automated extraction of data from bar plots  #####
# load the scatterplot image, source: Kortum & Acymyan (2013) J. of Usability Studies 9:14-24).
data(Kortum_and_Acymyan_2013_Fig4)
# display the image
figure_display(Kortum_and_Acymyan_2013_Fig4)

# convert metagear image object back to .jpg and then extract objects from this .jpg
figure_write(Kortum_and_Acymyan_2013_Fig4, 
             file = "sbs_meta/extract_data/Kortum_and_Acymyan_2013_Fig4.jpg")
rawData <- figure_barPlot("sbs_meta/extract_data/Kortum_and_Acymyan_2013_Fig4.jpg")

# display extracted points
as.vector(round(rawData, 2))

# extractions are in triplicates with an upper, mean, and lower values, so let's
# stack by three and sort within triplicates from lowest to highest
organizedData <- t(apply(matrix(rawData, ncol = 3, byrow = TRUE), 1, sort))
# rename rows and columns of these triplicates as presented in Kortum_and_Acymyan_2013_Fig4.jpg
theExtraction_names <- c("lower 95%CI", "mean SUS score", "upper 95%CI")
theBar_names <- toupper(letters[1:14])
dimnames(organizedData) <- list(theBar_names, theExtraction_names)
organizedData

### Example 2
# download the figure image from my website
figureSource <- "http://lajeunesse.myweb.usf.edu/metagear/example_2_barPlot.jpg"
download.file(figureSource, "sbs_meta/extract_data/example_2_barPlot.jpg", 
              quiet = TRUE, mode = "wb")
aFig <- figure_read("sbs_meta/extract_data/example_2_barPlot.jpg", display = TRUE)

rawData2 <- figure_barPlot("sbs_meta/extract_data/example_2_barPlot.jpg",  
                           horizontal = TRUE, # changed from FALSE since bars are horizontal
                           bar_width = 11, # raised from 9 since bars are wide relative to the figure
                           Y_min = 0,
                           Y_max = 10)

# exclude the false detection
rawData2 <- rawData2[rawData2 < max(rawData2)]
# data are in triplicates with an upper, mean, and lower values, so let's
# stack by three and sort within triplicates from lowest to highest
organizedData <- t(apply(matrix(rawData2, ncol = 3, byrow = TRUE), 1, sort))
# rename rows and columns of these triplicates as presented in the figure
theExtraction_names <- c("lower error", "bar", "upper error")
theBar_names <- c("exclosure", "water", "fertilizer", "control")
dimnames(organizedData) <- list(theBar_names, theExtraction_names)
organizedData

##### EXAMPLE: EXTRACT DATA FROM ROY 2003 #####

#
aFig <- figure_read("sbs_meta/extract_data/Roy2003_fig2_Aspirata_b.jpg", 
                    display = TRUE)

rawData <- figure_scatterPlot("sbs_meta/extract_data/Roy2003_fig2_Aspirata_b.jpg",
                              binary_point_fill = TRUE, 
                              point_size = 5, 
                              binary_threshold = 0.6, 
                              axis_thickness = 5, 
                              X_min = 0,
                              X_max = 3,
                              Y_min = 3.05,
                              Y_max = 3.45)


rawData <- figure_scatterPlot("sbs_meta/extract_data/Roy2003_fig2_Aspirata.jpg")

rawData <- figure_barPlot("sbs_meta/extract_data/Roy2003_fig2_Aspirata.jpg", 
                          axis_thickness = 3)

figure_detectAxis(aFig, 
                  axis_type = "Y", axis_thickness = 5,
                  sensitivity = 0.2)


figure_detectAxis("sbs_meta/extract_data/Roy2003_fig2_Aspirata.jpg", 
                  axis_type = "X", axis_thickness = 5,
                  sensitivity = 0.2)


figure_detectAllPoints(aFig, sensitivity = 0.2,
                       point_shape = "circle", point_size = 5)

figure_display(aFig)
figure_add(aFig)

rawData <- figure_barPlot("sbs_meta/scraped/Roy2003/Roy2003_fig3.png")


##### EXAMPLE: EXTRACT DATA FROM FISHER 2009 #####

#
fisher1 <- figure_read("sbs_meta/extract_data/Fisher_2009_fig1.png", 
                    display = TRUE)

rawData <- figure_scatterPlot("sbs_meta/extract_data/Fisher_2009_fig1_edit.png",
                              binary_threshold = 0.8, 
                              X_min = 18,
                              X_max = 28,
                              Y_min = 18,
                              Y_max = 36)

figure_detectAxis("sbs_meta/extract_data/Fisher_2009_fig1.png")

rawData <- figure_scatterPlot("sbs_meta/extract_data/Fisher_2009_fig1.png",
                              binary_point_fill = TRUE, 
                              point_size = 5, 
                              binary_threshold = 0.6, 
                              axis_thickness = 5, 
                              X_min = 0,
                              X_max = 3,
                              Y_min = 3.05,
                              Y_max = 3.45)


rawData <- figure_scatterPlot("sbs_meta/extract_data/Roy2003_fig2_Aspirata.jpg")

rawData <- figure_barPlot("sbs_meta/extract_data/Roy2003_fig2_Aspirata.jpg", 
                          axis_thickness = 3)

figure_detectAxis(aFig, 
                  axis_type = "Y", axis_thickness = 5,
                  sensitivity = 0.2)


figure_detectAxis("sbs_meta/extract_data/Roy2003_fig2_Aspirata.jpg", 
                  axis_type = "X", axis_thickness = 5,
                  sensitivity = 0.2)


figure_detectAllPoints(aFig, sensitivity = 0.2,
                       point_shape = "circle", point_size = 5)

figure_display(aFig)
figure_add(aFig)
