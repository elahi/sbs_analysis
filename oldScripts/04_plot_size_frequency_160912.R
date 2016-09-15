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

ggsave("figs/sbs_fig1.png", height = 3.5, width = 7)

##### OLD WAY #####

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

## Plot

range(childsPres$tideHTm, na.rm = TRUE)
range(waraPres$tideHTm, na.rm = TRUE)
range(hexPres$tideHTm, na.rm = TRUE)

ULClabel <- theme(plot.title = element_text(hjust = -0.08, vjust = 1, 
                                            size = rel(1)))

theme_set(theme_bw(base_size = 8) + 
            theme(panel.grid = element_blank()))

no_legend <- theme(legend.position = "none")


fig1e <- ggplot(childsPast,  aes(x = size1mm)) +
  geom_histogram(aes(y = ..count../sum(..count..)), binwidth = 1, 
                 color = "black", fill = "gray") +
  scale_x_continuous(limits = c(1, 21)) + 
  scale_y_continuous(limits = c(0, 0.2)) + 
  #labs(title = "E") + ULClabel + 
  xlab("Size (mm)") + ylab("Proportion") +
  annotate("text", label = "Littorina keenae\n1947\nn = 682", 
           x = 21, y = 0.2, size = 2.2, vjust = 1, hjust = 1) + 
  geom_vline(aes(xintercept = size0.05), linetype = "dashed", color = "red") + 
  annotate("text", x = 1, y = 0.2, label = "E", vjust = 1)
  

fig1f <- ggplot(childsPres,  aes(x = size1mm)) +
  geom_histogram(aes(y = ..count../sum(..count..)), binwidth = 1, 
                 color = "black", fill = "gray") +
  scale_x_continuous(limits = c(1, 21)) + 
  scale_y_continuous(limits = c(0, 0.2)) + 
  labs(title = "F") + ULClabel + xlab("Size (mm)") + ylab("Proportion") +
  annotate("text", label = "Littorina keenae\n2014\nn = 733", 
           x = 18, y = 0.16, size = 2.2) + 
  geom_vline(aes(xintercept = size0.05), linetype = "dashed", color = "red")

fig1c <- ggplot(hexPast,  aes(x = size1mm)) +
  geom_histogram(aes(y = ..count../sum(..count..)), binwidth = 1, 
                 color = "black", fill = "gray") +
  scale_x_continuous(limits = c(5, 25)) + 
  scale_y_continuous(limits = c(0, 0.20)) + 
  labs(title = "C") + ULClabel + xlab("Size (mm)") + ylab("Proportion") +
  annotate("text", label = "Lottia digitalis\n1950\nn = 492", 
           x = 20, y = 0.15, size = 2.2) + 
  geom_vline(aes(xintercept = size0.05), linetype = "dashed", color = "red")

fig1d <- ggplot(hexPres,  aes(x = size1mm)) +
  geom_histogram(aes(y = ..count../sum(..count..)), binwidth = 1, 
                 color = "black", fill = "gray") +
  scale_x_continuous(limits = c(5, 25)) + 
  scale_y_continuous(limits = c(0, 0.20)) + 
  labs(title = "D") + ULClabel + xlab("Size (mm)") + ylab("Proportion") +
  annotate("text", label = "Lottia digitalis\n2015\nn = 587", 
           x = 20, y = 0.15, size = 2.2) + 
  geom_vline(aes(xintercept = size0.05), linetype = "dashed", color = "red")


fig1a <- ggplot(waraPast,  aes(x = size1mm)) +
  geom_histogram(aes(y = ..count../sum(..count..)), binwidth = 2, 
                 color = "black", fill = "gray") +
  scale_x_continuous(limits = c(2, 32)) + 
  scale_y_continuous(limits = c(0, 0.31)) + 
  labs(title = "A") + ULClabel + xlab("Size (mm)") + ylab("Proportion") +
  annotate("text", label = "Chlorostoma funebralis\n1963\nn = 817", 
           x = 25, y = 0.25, size = 2.2) + 
  geom_vline(aes(xintercept = size0.05), linetype = "dashed", color = "red")

summary(waraPres)
fig1b <- ggplot(waraPres,  aes(x = size1mm)) +
  geom_histogram(aes(y = ..count../sum(..count..)), binwidth = 2, 
                 color = "black", fill = "gray") +
  scale_x_continuous(limits = c(2, 32)) + 
  scale_y_continuous(limits = c(0, 0.31)) + 
  labs(title = "B") + ULClabel + xlab("Size (mm)") + ylab("Proportion") +
  annotate("text", label = "Chlorostoma funebralis\n2014\nn = 5995", 
           x = 25, y = 0.25, size = 2.2) + 
  geom_vline(aes(xintercept = size0.05), linetype = "dashed", color = "red")


###############################
# save as 7 x 3.5 png
pdf("./figs/sbs_fig1.pdf", 7, 3.5)
multiplot(fig1a, fig1b, fig1c, fig1d, fig1e, fig1f, cols = 3)
dev.off()	

png("./figs/sbs_fig1_old.png", width = 7, height = 3.5, units = "in", res = 300)
multiplot(fig1a, fig1b, fig1c, fig1d, fig1e, fig1f, cols = 3)
dev.off()	

####################################################
####################################################
####################################################

# KS-tests: overall
ks.test(waraPast$size1mm, waraPres$size1mm)
ks.test(childsPast$size1mm, childsPres$size1mm)
ks.test(hexPast$size1mm, hexPres$size1mm)

