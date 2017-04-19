################################################################################
##' @title Make plot for HMS open house
##'
##' @author Robin Elahi
##' @contact elahi.robin@gmail.com
##' 
##' @date 2017-04-19
##' 
##' @log 
################################################################################

rm(list=ls(all=TRUE)) 

# load data
source("03_identify_size_cutoff.R")

dat4

##### TRY FACETTING #####

facet_panels <- dat4 %>% select(species, era) %>% distinct() %>%
  arrange(species, era)

facet_panels

facet_panels <- facet_panels %>% 
  mutate(facet_labels = paste(LETTERS)[1:6]) %>% 
  arrange(facet_labels)

dat6 <- dat4 %>% 
  inner_join(., facet_panels, by = c("species", "era"))

## Make separate dataframes

# Littorina keenae 
childsDF <- droplevels(filter(dat6, sp == "LIKE"))
childsPast <- childsDF %>% filter(era == "past")
childsPres <- childsDF %>% filter(era == "present")

# Chlorostoma funebralis
waraDF <- droplevels(filter(dat6, sp == "CHFU"))
waraPast <- waraDF %>% filter(era == "past")
waraPres <- waraDF %>% filter(era == "present")

# Lottia digitalis
hexDF <- droplevels(filter(dat6, sp == "LODI"))
hexPast <- hexDF %>% filter(era == "past")
hexPres <- hexDF %>% filter(era == "present")

##### Chlorostoma #####

wara_sample <- data.frame(size1mm = sample(waraPast$size1mm, size = 200))

wara_sample %>% 
  ggplot(aes(size1mm)) +
  geom_dotplot(binwidth = 2, method = "histodot", dotsize = 0.18, stackratio = 1.1, 
               fill = "red") + 
  ylim(0, 32)

ggsave("hms_open_house/chlorostoma_dotplot.png")

