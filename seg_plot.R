################################################################################################################
#                              Segregation in UK Higher Education: Segplot                                     #
################################################################################################################
#install.packages("segregation")
#install.packages("tidyverse")
#install.packages("googlesheets4)
#install.packages("ggtext")
library(segregation)
library(tidyverse)
library(googlesheets4)
library(ggtext)

## Pushing the datasets for HEI information from the Google Drive
hei_data <- read_sheet('https://docs.google.com/spreadsheets/d/1OSLDU-cZh9BU36VJgCL7R1yPT1QS6VHtuV42YNSQL18/edit#gid=460783501')

# creating a new category for type of HEI 
# we will need to rethink how we organise this dataset if we want to publish the coding :)
names(hei_data)
table(hei_data$hei_type)

hei_data$hei_type <- factor(hei_data$hei_type,
                            levels = c(1,2,3,4),
                            labels = c("Golden Triangle", "Russell Group", "Pre-1992", "Post-1992"))

hei_data$hei_type5 <- factor(hei_data$hei_type5,
                             levels = c(1,2,3,4,5),
                             labels = c("Oxbridge", "Golden Triangle", "Russell Group", "Pre-1992", "Post-1992"))
################################################################################################################
# Sex ----
################################################################################################################
# First degree ----
## Pushing dataset for sex for all first degree students
sex_fd <- read_sheet('https://docs.google.com/spreadsheets/d/17stHJW9QLd8FbEqjaMHmeYtmQ-KGShab2az6-Zfrmmw/edit#gid=747800656')

# Merging the sex for first degree students and HEI info datasets
sex_fd2 <- merge(sex_fd, hei_data, by = 'hei', all.x = T)

# Segplot
#2010
sex_fd2 %>% 
  filter(fte != 'NA') %>%
  filter(acyear == '2010') %>% 
  segplot(., "sex", "hei", weight = "fte",
          secondary_plot = "segregation")
#2020
sex_fd2 %>% 
  filter(fte != 'NA') %>%
  filter(sex != 'Other') %>%
  filter(acyear == '2020') %>% 
  segplot(., "sex", "hei", weight = "fte",
          secondary_plot = "segregation")

################################################################################################################
# Ethnicity ----
################################################################################################################
# First degree ----
## Pushing dataset for ethnicity for all first degree students
ethnicity_fd <- read_sheet('https://docs.google.com/spreadsheets/d/1FXRqnLIw3CFLpIfiDCZ-vZX4eIPEjpstSxzkt5vRkko/edit#gid=653038261')

# Merging the ethnicity for first degree students and HEI info datasets
ethnicity_fd2 <- merge(ethnicity_fd, hei_data, by = 'hei', all.x = T)

# Segplot
# 2010
ethnicity_fd2 %>% 
  filter(fte != 'NA') %>%
  filter(ethnicity != 'Unknown/not applicable' ) %>%
  filter(acyear == '2010') %>% 
  segplot(., "ethnicity", "hei", weight = "fte",
          secondary_plot = "segregation")
# 2020
ethnicity_fd2 %>% 
  filter(fte != 'NA') %>%
  filter(ethnicity != 'Unknown/not applicable' ) %>%
  filter(acyear == '2020') %>% 
  segplot(., "ethnicity", "hei", weight = "fte",
          secondary_plot = "segregation")
