################################################################################
##' @title Plot snail size-frequency distributions
##'
##' @author Robin Elahi
##' @contact elahi.robin@gmail.com
##' 
##' @date 2016-08-05
##' 
##' @log 
##' 2017-10-24 Wrote functions to get data and choose size thresholds for better automation
################################################################################

##### LOAD PACKAGES, DATA #####

library(grid)
source("R/choose_size_data.R")
source("R/choose_size_threshold.R")

# load data - approximated sizes
dat <- choose_size_data(method = "normal")

# Do not remove any data
dat2 <- choose_size_threshold(x = dat, era = "combined", filter_data = F) %>% 
  filter(!is.na(size1mm))
names(dat2)
unique(dat2$species)

# plotting functions
source("./R/multiplotF.R")

theme_set(theme_bw(base_size = 12) + 
            theme(strip.background = element_blank(), 
                  #panel.grid = element_blank()),
                  strip.text = element_text(face = "italic")))

##### PROBABILITY DENSITY PLOT #####

n_df <- dat2 %>% filter(!is.na(size1mm)) %>%
  group_by(species, era) %>% summarise(n = n()) %>% 
  ungroup()
n_df

yr_df <- dat2 %>% filter(!is.na(size1mm)) %>%
  group_by(species, era) %>% distinct(year) %>% ungroup()

facet_panels <- dat2 %>% select(species) %>% distinct() %>%
  arrange(species)

facet_panels

facet_panels <- facet_panels %>% 
  mutate(facet_labels = paste(LETTERS)[1:3]) %>% 
  arrange(facet_labels) %>% 
  mutate(past_text = c("1963; n=817", "1950; n=493", "1947; n=682"), 
         present_text = c("2014; n=5646", "2015; n=605", "2014; n=733"))

dat3 <- dat2 %>% 
  inner_join(., facet_panels, by = c("species"))

theme_set(theme_bw(base_size = 12) + 
            theme(strip.background = element_blank(), 
                  panel.grid = element_blank(),
                  strip.text = element_text(face = "italic")))

dat3 %>% 
  ggplot(aes(size1mm, fill = era)) + 
  geom_density(alpha = 0.5) + 
  facet_wrap( ~ species) + 
  scale_fill_manual(values = c("red", "black")) + 
  xlab("Size (mm)") + 
  ylab("Probability density") + 
  theme(legend.position = "none") +
  geom_vline(aes(xintercept = size_threshold), 
             linetype = "dashed", color = "black", alpha = 0.65) +  
  geom_text(data = facet_panels, aes(0, 0.15, label = facet_labels), 
            inherit.aes = FALSE, size = 6, nudge_x = 1, nudge_y = 0.01) + 
  geom_text(data = facet_panels, aes(15, 0.115, label = past_text), 
            inherit.aes = FALSE, size = 4, nudge_x = 1, nudge_y = 0.01, color = "red", hjust = 0) + 
  geom_text(data = facet_panels, aes(15, 0.1, label = present_text), 
            inherit.aes = FALSE, size = 4, nudge_x = 1, nudge_y = 0.01, color = "black", hjust = 0) 

ggsave("figs_ms/plot_size_frequency_density.pdf", height = 3.5, width = 7)  

##### SEPARATE PANEL WITH MULTIPLOT #####

## Make separate dataframes

# Littorina keenae 
childsDF <- droplevels(filter(dat2, sp == "LIKE"))
childsPast <- childsDF %>% filter(era == "past")
childsPres <- childsDF %>% filter(era == "present")

# Chlorostoma funebralis
waraDF <- droplevels(filter(dat2, sp == "CHFU"))
waraPast <- waraDF %>% filter(era == "past")
waraPres <- waraDF %>% filter(era == "present")

# Lottia digitalis
hexDF <- droplevels(filter(dat2, sp == "LODI"))
hexPast <- hexDF %>% filter(era == "past")
hexPres <- hexDF %>% filter(era == "present")

# Basic function to plot histogram for any subset of data

plot_histo_panel <- function(df, bin_width = 1) {
  
  p <- ggplot(df,  aes(x = size1mm)) +
    geom_histogram(aes(y = ..count../sum(..count..)), binwidth = bin_width, 
                   color = "black", fill = "gray") +
    xlab("Size (mm)") + ylab("Proportion") +
    geom_vline(aes(xintercept = size_threshold), 
               linetype = "dashed", color = "black", alpha = 0.65) 
  
  return(p)
  
}
  
# Need to customize for each panel:

theme_set(theme_bw(base_size = 10))

# theme_set(theme_bw(base_size = 10) + 
#             theme(panel.grid = element_blank()))

fig1a <- plot_histo_panel(waraPast, bin_width = 2) + 
  scale_x_continuous(limits = c(2, 32)) + 
  scale_y_continuous(limits = c(0, 0.31)) + 
  annotate("text", label = "1963\nn = 817", 
           x = 32, y = 0.31, size = 2.2, vjust = 1, hjust = 1) +
  annotate("text", label = "A", 
           x = 2, y = 0.31, vjust = 1, hjust = -0.05) + 
  ggtitle("Chlorostoma funebralis") + 
  theme(plot.title = element_text(size = 6, face = "italic", 
                                  hjust = 0.5, vjust = 0.9)) 

fig1b <- plot_histo_panel(waraPres, bin_width = 2) + 
  scale_x_continuous(limits = c(2, 32)) + 
  scale_y_continuous(limits = c(0, 0.31)) + 
  annotate("text", label = "2014\nn = 5646", 
           x = 32, y = 0.31, size = 2.2, vjust = 1, hjust = 1) +
  annotate("text", label = "B", 
           x = 2, y = 0.31, vjust = 1, hjust = -0.05) 

fig1c <- plot_histo_panel(hexPast) + 
  scale_x_continuous(limits = c(5, 25)) + 
  scale_y_continuous(limits = c(0, 0.2)) + 
  annotate("text", label = "1950\nn = 493", 
           x = 25, y = 0.2, size = 2.2, vjust = 1, hjust = 1) +
  annotate("text", label = "C", 
           x = 5, y = 0.2, vjust = 1, hjust = -0.05) + 
  ggtitle("Lottia digitalis") + 
  theme(plot.title = element_text(size = 6, face = "italic", 
                                  hjust = 0.5, vjust = 0.9))

fig1d <- plot_histo_panel(hexPres) + 
  scale_x_continuous(limits = c(5, 25)) + 
  scale_y_continuous(limits = c(0, 0.2)) + 
  annotate("text", label = "2015\nn = 605", 
           x = 25, y = 0.2, size = 2.2, vjust = 1, hjust = 1) +
  annotate("text", label = "D", 
           x = 5, y = 0.2, vjust = 1, hjust = -0.05) 

fig1e <- plot_histo_panel(childsPast) + 
  scale_x_continuous(limits = c(1, 21)) + 
  scale_y_continuous(limits = c(0, 0.2)) + 
  annotate("text", label = "1947\nn = 682", 
              x = 21, y = 0.2, size = 2.2, vjust = 1, hjust = 1) +
  annotate("text", label = "E", 
           x = 1, y = 0.2, vjust = 1, hjust = -0.05) + 
  ggtitle("Littorina keenae") + 
  theme(plot.title = element_text(size = 6, face = "italic", 
                                  hjust = 0.5, vjust = 0.9))
  
fig1f <- plot_histo_panel(childsPres) + 
  scale_x_continuous(limits = c(1, 21)) + 
  scale_y_continuous(limits = c(0, 0.2)) + 
  annotate("text", label = "2014\nn = 733", 
           x = 21, y = 0.2, size = 2.2, vjust = 1, hjust = 1) +
  annotate("text", label = "F", 
           x = 1, y = 0.2, vjust = 1, hjust = -0.05) 

multiplot(fig1a, fig1b, fig1c, fig1d, fig1e, fig1f, cols = 3)

##### SAVE MULTI PANEL PLOT #####
# save as 7 x 3.5 pdf
pdf("figs_ms/plot_size_frequency.pdf", 7, 3.5)
multiplot(fig1a, fig1b, fig1c, fig1d, fig1e, fig1f, cols = 3)
dev.off()	

png("figs_ms/plot_size_frequency.png", width = 7, height = 3.5, units = "in", res = 300)
multiplot(fig1a, fig1b, fig1c, fig1d, fig1e, fig1f, cols = 3)
dev.off()	

##### LITTORINA ONLY #####

# Basic function to plot histogram for any subset of data

plot_histo_panel <- function(df, bin_width = 1) {
  
  p <- ggplot(df,  aes(x = size1mm)) +
    geom_histogram(aes(y = ..count../sum(..count..)), binwidth = bin_width, 
                   color = "white", fill = "black") +
    xlab("Size (mm)") + ylab("Proportion") 
  
  return(p)
  
}

# Need to customize for each panel:

theme_set(theme_bw(base_size = 18) + 
            theme(strip.background = element_blank(), 
                  panel.grid = element_blank(),
                  strip.text = element_text(face = "italic")))

fig1e <- plot_histo_panel(childsPast) + 
  scale_x_continuous(limits = c(1, 21)) + 
  scale_y_continuous(limits = c(0, 0.2)) + 
  annotate("text", label = "1947\nn = 682", 
           x = 21, y = 0.2, size = 4.2, vjust = 1, hjust = 1)

fig1f <- plot_histo_panel(childsPres) + 
  scale_x_continuous(limits = c(1, 21)) + 
  scale_y_continuous(limits = c(0, 0.2)) + 
  annotate("text", label = "2014\nn = 733", 
           x = 21, y = 0.2, size = 4.2, vjust = 1, hjust = 1) 

# save as 7 x 3.5 pdf
pdf("figs_ms/plot_size_frequency_littorina.pdf", width = 3.5, height = 7)
multiplot(fig1e, fig1f, cols = 1)
dev.off()	
