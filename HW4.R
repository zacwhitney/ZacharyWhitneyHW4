library(ggplot2)
library(dplyr)
library(lubridate)
library(tidyverse)

#QUESTION 3
cars93 <- MASS::Cars93

ggplot(cars93, aes(x = Price, y = Fuel.tank.capacity)) +
  geom_point(color = "grey60") +
  geom_smooth(se = TRUE, method = "lm", formula = y ~ x, color = "#8fe388") +
  scale_x_continuous(
    name = "price (USD)",
    breaks = c(20, 40, 60),
    labels = c("$20,000", "$40,000", "$60,000") 
  ) +
  scale_y_continuous(name = "fuel-tank capacity\n(US gallons)") +
  ggtitle("lm Smoothing") + theme(plot.title = element_text(size=14)) + theme(plot.title = element_text(colour = "#8fe388")) 

ggplot(cars93, aes(x = Price, y = Fuel.tank.capacity)) +
  geom_point(color = "grey60") +
  geom_smooth(se = TRUE, method = "glm", formula = y ~ x, color = "#fe8d6d") +
  scale_x_continuous(
    name = "price (USD)",
    breaks = c(20, 40, 60),
    labels = c("$20,000", "$40,000", "$60,000")
  ) +
  scale_y_continuous(name = "fuel-tank capacity\n(US gallons)") +
  ggtitle("glm Smoothing") + theme(plot.title = element_text(size=14)) + theme(plot.title = element_text(colour = "#fe8d6d")) 


ggplot(cars93, aes(x = Price, y = Fuel.tank.capacity)) +
  geom_point(color = "grey60") +
  geom_smooth(se = TRUE, method = "gam", formula = y ~ x, color = "#7c6bea") +
  scale_x_continuous(
    name = "price (USD)",
    breaks = c(20, 40, 60),
    labels = c("$20,000", "$40,000", "$60,000")
  ) +
  scale_y_continuous(name = "fuel-tank capacity\n(US gallons)") +
  ggtitle("gam Smoothing") + theme(plot.title = element_text(size=14)) + theme(plot.title = element_text(colour = "#7c6bea")) 

#-------------------------------------------------------------------------------------------------------------------------------------------------
#QUESTION 4

load("C:/users/zacha/documents/college/summer2022/datavis/week5/preprint_growth.rda") #please change the path if needed
head(preprint_growth)

preprint_growth %>% filter(archive == "bioRxiv") %>%
  filter(count > 0) -> biorxiv_growth

#a). 
preprint_growth %>% filter(date > 2004, count>0) %>% drop_na() -> preprint_full

#b). 
preprint_growth %>% filter(date > 2004, count>0) %>% drop_na() %>% 
  filter(archive %in% c('bioRxiv', 'F1000Research')) -> preprint_full


preprints<-preprint_growth %>% filter(archive %in%
                                        c("bioRxiv", "arXiv q-bio", "PeerJ Preprints")) %>%filter(count > 0) %>%
  mutate(archive = factor(archive, levels = c("bioRxiv", "arXiv q-bio", "PeerJ Preprints")))

preprints_final <- filter(preprint_full, date == ymd("2017-01-01")) #had to edit to fix the label not being correctly placed 


#c-f). 
ggplot(preprint_full) +
  aes(date, count, color = archive, fill = archive) +
  geom_line(size = 1) +
  scale_y_continuous(
    limits = c(0, 600), expand = c(0, 0),
    name = "preprints / month",
    sec.axis = dup_axis( #this part is for the second y axis
      breaks = preprints_final$count, #and we use the counts to position our labels
      labels = c("bioRxiv", "F1000Research"),
      name = NULL)
  ) +
  scale_x_date(name = "year",
               limits = c(ymd("2014-02-01"), ymd("2017-01-01"))) +
  scale_color_manual(values = c("#7c6bea", "#fe8d6d"),
                     name = NULL) +
  theme(legend.position = "right") + ggtitle('Preprint Counts')

#--------------------------------------------------------------------------------------------------------------------------------------------




