################################################################################################################
#                                 Segregation in UK Higher Education                                           #
################################################################################################################
#install.packages("segregation")
#install.packages("tidyverse")
#install.packages("vegan")
#install.packages("abdiv")
#install.packages("googlesheets4)
#install.packages("ggtext")
library(segregation)
library(tidyverse)
library(vegan)
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

hei_data %>% 
  filter(hei_type == 'Golden Triangle' | hei_type == 'Russell Group') %>% 
  print(n = 24)

hei_data$hei_type5 <- factor(hei_data$hei_type5,
                             levels = c(1,2,3,4,5),
                             labels = c("Oxbridge", "Golden Triangle", "Russell Group", "Pre-1992", "Post-1992"))

hei_data %>% 
  filter(hei_type5 == 'Oxbridge' | hei_type5 == 'Golden Triangle' | hei_type5 == 'Russell Group') %>% 
  print(n = 24) 


################################################################################################################
# Sex ----
################################################################################################################
# First degree ----
# The first part of the analysis will consider all undergraduate students (first year students and non-first year students)

## Pushing dataset for sex for all first degree students
sex_fd <- read_sheet('https://docs.google.com/spreadsheets/d/17stHJW9QLd8FbEqjaMHmeYtmQ-KGShab2az6-Zfrmmw/edit#gid=747800656')

# Excluding 'Other' from the analysis.
table(sex_fd$sex)
sex_fd$female [sex_fd$sex == 'Female'] <- 1
sex_fd$female [sex_fd$sex == 'Male'] <- 0
sex_fd$female [sex_fd$sex == 'Other'] <- NA

sex_fd$female <- factor(sex_fd$female,
                          levels = c(0,1),
                          labels = c("Male", "Female")) # labels


# dissimilarity index for sex
sex_fd %>% filter(sex_fd$female != 'NA') %>% 
  group_by(acyear) %>%
  group_modify(~
                 dissimilarity(data = .x,
                               group = 'female',
                               unit = 'hei',
                               weight = 'fte'))

# Merging the sex for first degree students and HEI info datasets
sex_fd2 <- merge(sex_fd, hei_data, by = 'hei', all.x = T)

# Simpson Index by type of HEI + graph
sex_fd2 %>% 
  filter(fte != 'NA') %>% 
  filter (hei_type5 == 'Oxbridge' | hei_type5 == 'Golden Triangle' | hei_type5 == 'Russell Group') %>% 
  select('acyear', 'hei', 'hei_type5','sex', 'fte') %>% 
  filter(acyear == '2010' | acyear == '2020') %>% 
  group_by(acyear, hei_type5, hei) %>% 
  summarise(simpson = simpson(fte)) %>% 
  ggplot(aes(x=simpson, y=hei, colour = hei_type5)) +
  geom_point(size = 3) +
  theme_minimal()

################################################################################################################
# Ethnicity ----
################################################################################################################
# First degree ----
# The first part of the analysis will consider all undergraduate students (first year students and non-first year students)

## Pushing dataset for ethnicity for all first degree students
ethnicity_fd <- read_sheet('https://docs.google.com/spreadsheets/d/1FXRqnLIw3CFLpIfiDCZ-vZX4eIPEjpstSxzkt5vRkko/edit#gid=653038261')

# indicator variables for ethnicity
# non-white
ethnicity_fd$non_white [ethnicity_fd$ethnicity == 'Asian'] <- 1
ethnicity_fd$non_white [ethnicity_fd$ethnicity == 'Black'] <- 1
ethnicity_fd$non_white [ethnicity_fd$ethnicity == 'Mixed'] <- 1
ethnicity_fd$non_white [ethnicity_fd$ethnicity == 'Other'] <- 1
ethnicity_fd$non_white [ethnicity_fd$ethnicity == 'Unknown/not applicable'] <- NA
ethnicity_fd$non_white [ethnicity_fd$ethnicity == 'White'] <- 0


ethnicity_fd$non_white <- factor(ethnicity_fd$non_white,
                                   levels = c(0,1),
                                   labels = c("White", "Nonwhite")) # labels

# dissimilarity index for non-white
ethnicity_fd %>% filter (ethnicity_fd$non_white != 'NA') %>% 
  group_by(acyear) %>%
  group_modify(~
                 dissimilarity(data = .x,
                               group = 'non_white',
                               unit = 'hei',
                               weight = 'fte'))

# Asian vs White
ethnicity_fd$asian [ethnicity_fd$ethnicity == 'Asian'] <- 1
ethnicity_fd$asian [ethnicity_fd$ethnicity == 'Black'] <- NA
ethnicity_fd$asian [ethnicity_fd$ethnicity == 'Mixed'] <- NA
ethnicity_fd$asian [ethnicity_fd$ethnicity == 'Other'] <- NA
ethnicity_fd$asian [ethnicity_fd$ethnicity == 'Unknown/not applicable'] <- NA
ethnicity_fd$asian [ethnicity_fd$ethnicity == 'White'] <- 0

ethnicity_fd$asian <- factor(ethnicity_fd$asian,
                               levels = c(0,1),
                               labels = c("White", "Asian")) # labels

# dissimilarity for Asian
ethnicity_fd %>% filter (ethnicity_fd$asian != 'NA') %>% 
  group_by(acyear) %>%
  group_modify(~
                 dissimilarity(data = .x,
                               group = 'asian',
                               unit = 'hei',
                               weight = 'fte'))

# Black vs White
ethnicity_fd$black [ethnicity_fd$ethnicity == 'Asian'] <- NA
ethnicity_fd$black [ethnicity_fd$ethnicity == 'Black'] <- 1
ethnicity_fd$black [ethnicity_fd$ethnicity == 'Mixed'] <- NA
ethnicity_fd$black [ethnicity_fd$ethnicity == 'Other'] <- NA
ethnicity_fd$black [ethnicity_fd$ethnicity == 'Unknown/not applicable'] <- NA
ethnicity_fd$black [ethnicity_fd$ethnicity == 'White'] <- 0

ethnicity_fd$black <- factor(ethnicity_fd$black,
                               levels = c(0,1),
                               labels = c("White", "Black")) # labels

# dissimilarity for Black
ethnicity_fd %>% filter (ethnicity_fd$black != 'NA') %>% 
  group_by(acyear) %>%
  group_modify(~
                 dissimilarity(data = .x,
                               group = 'black',
                               unit = 'hei',
                               weight = 'fte'))

# mixed vs white
ethnicity_fd$mixed [ethnicity_fd$ethnicity == 'Asian'] <- NA
ethnicity_fd$mixed [ethnicity_fd$ethnicity == 'Black'] <- NA
ethnicity_fd$mixed [ethnicity_fd$ethnicity == 'Mixed'] <- 1
ethnicity_fd$mixed [ethnicity_fd$ethnicity == 'Other'] <- NA
ethnicity_fd$mixed [ethnicity_fd$ethnicity == 'Unknown/not applicable'] <- NA
ethnicity_fd$mixed [ethnicity_fd$ethnicity == 'White'] <- 0

ethnicity_fd$mixed <- factor(ethnicity_fd$mixed,
                               levels = c(0,1),
                               labels = c("White", "Mixed")) # labels


# dissimilarity for Mixed
ethnicity_fd %>% filter (ethnicity_fd$mixed != 'NA') %>% 
  group_by(acyear) %>%
  group_modify(~
                 dissimilarity(data = .x,
                               group = 'mixed',
                               unit = 'hei',
                               weight = 'fte'))


# Merging the ethnicity for first degree students and HEI info datasets
ethnicity_fd2 <- merge(ethnicity_fd, hei_data, by = 'hei', all.x = T)

# Simpson Index by type of HEI + graph
ethnicity_fd2 %>% 
  filter(fte != 'NA') %>% 
  filter(ethnicity != 'Unknown/not applicable') %>% 
  filter (hei_type5 == 'Oxbridge' | hei_type5 == 'Golden Triangle' | hei_type5 == 'Russell Group') %>% 
  select('acyear', 'hei', 'hei_type5','ethnicity', 'fte') %>% 
  filter(acyear == '2010' | acyear == '2020') %>% 
  group_by(acyear, hei_type5, hei) %>% 
  summarise(simpson = simpson(fte)) %>% 
  ggplot(aes(x=simpson, y=hei, colour = hei_type5)) +
  geom_point(size = 3) +
  theme_minimal()