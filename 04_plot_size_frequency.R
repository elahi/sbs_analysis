################################################################################
##' @title Plot snail size-frequency distributions
##'
##' @author Robin Elahi
##' @contact elahi.robin@gmail.com
##' 
##' @date 2016-08-05
##' 
##' @log 
################################################################################

# rm(list=ls(all=TRUE)) 

library(grid)

# load data
source("03_identify_size_cutoff.R")

# plotting functions
source("./R/multiplotF.R")

head(dat4)

##### TRY FACETTING #####

facet_panels <- dat4 %>% select(species, era) %>% distinct() %>%
  arrange(species, era)

facet_panels

facet_panels <- facet_panels %>% 
  mutate(facet_labels = paste(LETTERS)[1:6]) %>% 
  arrange(facet_labels)

dat6 <- dat4 %>% 
  inner_join(., facet_panels, by = c("species", "era"))

dat6 %>% 
  ggplot(aes(size1mm)) + 
  geom_histogram(aes(y = ..density.. * 100), binwidth = 1, 
                 color = "black", fill = "gray") +   
  facet_grid(era ~ species, switch = "y") + 
  geom_vline(aes(xintercept = size0.05), linetype = "dashed", color = "red") + 
  labs(x = "Size (mm)", y = "Frequency (%)") + 
  theme(strip.background = element_blank()) + 
  geom_text(data = facet_panels, aes(0.1, 18, label = facet_labels), 
            inherit.aes = FALSE) 

#ggsave("figs/sbs_fig1.png", height = 3.5, width = 7)

##### SEPARATE PANEL WITH MULTIPLOT #####

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



# Basic function to plot histogram for any subset of data

plot_histo_panel <- function(df, bin_width = 1) {
  
  p <- ggplot(df,  aes(x = size1mm)) +
    geom_histogram(aes(y = ..count../sum(..count..)), binwidth = bin_width, 
                   color = "black", fill = "gray") +
    xlab("Size (mm)") + ylab("Proportion") +
    geom_vline(aes(xintercept = size0.05), linetype = "dashed", color = "red") 
  
  return(p)
  
}
  
# Need to customize for each panel:

theme_set(theme_bw(base_size = 10) + 
            theme(panel.grid = element_blank()))

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
  annotate("text", label = "2014\nn = 5995", 
           x = 32, y = 0.31, size = 2.2, vjust = 1, hjust = 1) +
  annotate("text", label = "B", 
           x = 2, y = 0.31, vjust = 1, hjust = -0.05) 

fig1c <- plot_histo_panel(hexPast) + 
  scale_x_continuous(limits = c(5, 25)) + 
  scale_y_continuous(limits = c(0, 0.2)) + 
  annotate("text", label = "1950\nn = 492", 
           x = 25, y = 0.2, size = 2.2, vjust = 1, hjust = 1) +
  annotate("text", label = "C", 
           x = 5, y = 0.2, vjust = 1, hjust = -0.05) + 
  ggtitle("Lottia digitalis") + 
  theme(plot.title = element_text(size = 6, face = "italic", 
                                  hjust = 0.5, vjust = 0.9))

fig1d <- plot_histo_panel(hexPres) + 
  scale_x_continuous(limits = c(5, 25)) + 
  scale_y_continuous(limits = c(0, 0.2)) + 
  annotate("text", label = "2015\nn = 587", 
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
  
###############################
# save as 7 x 3.5 pdf
pdf("./figs/sbs_fig1.pdf", 7, 3.5)
multiplot(fig1a, fig1b, fig1c, fig1d, fig1e, fig1f, cols = 3)
dev.off()	

png("./figs/sbs_fig1.png", width = 7, height = 3.5, units = "in", res = 300)
multiplot(fig1a, fig1b, fig1c, fig1d, fig1e, fig1f, cols = 3)
dev.off()	

####################################################
####################################################
####################################################

# KS-tests: overall
ks.test(waraPast$size1mm, waraPres$size1mm)
ks.test(childsPast$size1mm, childsPres$size1mm)
ks.test(hexPast$size1mm, hexPres$size1mm)

