
## Paine 1971, Table 2
#' Chlorostoma funebralis
#' Makah Bay, Washington State
#' x = ln shell diameter (mm)
#' y = ln dry weight (mg)
#' ln(y) = -5.016 + 3.670*ln(x) 

chfu_mmTOg <- function(x){
  
  exp(-5.016 + 3.670 * log(x)) / 1000 # divide by 1000 to return grams instead of mg
  
} 

## Wootton 1992, page 983
#' Lottia digitalis
#' Tatoosh Island, Washington State
#' x = ln shell length (cm)
#' y = ln dry weight (g)
#' ln(y) = -4.29 + 3.30*ln(x)

lodi_mmTOg <- function(x){
  
  # First convert mm to cm
  x_cm <- x / 10
  
  # Regression takes length in cm, returns weight in g
  exp(-4.29 + 3.30 * log(x_cm))
  
}

## Chow 1987, Table 2
#' Littorina keenae
#' Bodega Bay
#' x = ln shell length (mm)
#' y = ln dry weight (mg)

# These are slope terms for 4 different seasons by sex
the_slope <- data.frame(sex = c(rep("M", 4), rep("F", 4)),
                        season = rep(c("Winter", "Spring", "Summer", "Autumn"), 2), 
                        the_slope = c(2.64, 2.86, 2.811, 3.070, 2.531, 2.763, 2.788, 2.989))

# These are coefficient terms for 4 different seasons by sex
the_coef <- data.frame(sex = c(rep("M", 4), rep("F", 4)), 
                       season = rep(c("Winter", "Spring", "Summer", "Autumn"), 2), 
                       the_coef = c(-3.106, -3.682, -3.479, -4.128, -2.799, -3.292, -3.259, -4.023))

mean_slope <- mean(the_slope$the_slope)
mean_coef <- mean(the_coef$the_coef)

like_mmTOg <- function(x){
  
  exp(mean_coef + mean_slope * log(x)) / 1000 # divide by 1000 to return grams instead of mg
  
  
}

