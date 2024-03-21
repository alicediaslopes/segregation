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

# PGR degree ----
# The first part of the analysis will consider all PGR students (first year students and non-first year students)

## Pushing dataset for sex for all first degree students
sex_pgr <- read_sheet('https://docs.google.com/spreadsheets/d/1Z5TtGZXN6b_loViNuz8by4aSLrX138UA2LFhW-62UgY/edit#gid=399871028')

# Excluding 'Other' from the analysis.
table(sex_pgr$sex)
sex_pgr$female [sex_pgr$sex == 'Female'] <- 1
sex_pgr$female [sex_pgr$sex == 'Male'] <- 0
sex_pgr$female [sex_pgr$sex == 'Other'] <- NA

sex_pgr$female <- factor(sex_pgr$female,
                        levels = c(0,1),
                        labels = c("Male", "Female")) # labels

# dissimilarity index for sex
sex_pgr %>% filter(sex_pgr$female != 'NA') %>% 
  group_by(acyear) %>%
  group_modify(~
                 dissimilarity(data = .x,
                               group = 'female',
                               unit = 'hei',
                               weight = 'fte'))

# Staff ----
# The first part of the analysis will consider all academic staff

## Pushing the dataset for sex for academic staff
sex_staff <- read_sheet('https://docs.google.com/spreadsheets/d/1Kgc4sBxWaMhQwRaCFriOyiL4Zt0g8syi6aFeweLD0Bw/edit#gid=1202832876')

# Excluding 'Other' and 'Not known' from the analysis.
sex_staff$female [sex_staff$sex == 'Female'] <- 1
sex_staff$female [sex_staff$sex == 'Male'] <- 0
sex_staff$female [sex_staff$sex == 'Other'] <- NA
sex_staff$female [sex_staff$sex == 'Not known'] <- NA


sex_staff$female <- factor(sex_staff$female,
                         levels = c(0,1),
                         labels = c("Male", "Female")) # labels

# dissimilarity index for sex
sex_staff %>% filter(sex_staff$female != 'NA') %>% 
  group_by(acyear) %>%
  group_modify(~
                 dissimilarity(data = .x,
                               group = 'female',
                               unit = 'hei',
                               weight = 'fte'))

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
  select('acyear', 'hei', 'hei_type5','ethnicity', 'fte') %>% 
  group_by(acyear, hei_type5, hei) %>% 
  summarise(simpson = simpson(fte)) %>% 
  filter (hei_type5 == 'Oxbridge' | hei_type5 == 'Golden Triangle' | hei_type5 == 'Russell Group') %>% 
  filter(acyear == '2010' | acyear == '2020') %>% 
  ggplot(aes(y=hei, x=simpson, colour = acyear)) +
  geom_point(size = 3) +
  theme_minimal()

# Segplot
ethnicity_fd2 %>% 
  filter(fte != 'NA') %>%
  filter(ethnicity != 'Unknown/not applicable' ) %>%
  filter(acyear == '2020') %>% 
  segplot(., "ethnicity", "hei", weight = "fte",
          secondary_plot = "segregation")

# PGR degree ----
# The first part of the analysis will consider all PGR students (first year students and non-first year students)

## Pushing dataset for ethnicity for all PGR degree students
ethnicity_pgr <- read_sheet('https://docs.google.com/spreadsheets/d/13i-D-dYL7m_oDJNEW8kBHR80OxbjusrQ4Kn5PgAFisg/edit#gid=1368204546')

# indicator variables for ethnicity
# non-white
ethnicity_pgr$non_white [ethnicity_pgr$ethnicity == 'Asian'] <- 1
ethnicity_pgr$non_white [ethnicity_pgr$ethnicity == 'Black'] <- 1
ethnicity_pgr$non_white [ethnicity_pgr$ethnicity == 'Mixed'] <- 1
ethnicity_pgr$non_white [ethnicity_pgr$ethnicity == 'Other'] <- 1
ethnicity_pgr$non_white [ethnicity_pgr$ethnicity == 'Unknown/not applicable'] <- NA
ethnicity_pgr$non_white [ethnicity_pgr$ethnicity == 'White'] <- 0


ethnicity_pgr$non_white <- factor(ethnicity_pgr$non_white,
                                 levels = c(0,1),
                                 labels = c("White", "Nonwhite")) # labels

# dissimilarity index for non-white
ethnicity_pgr %>% filter (ethnicity_pgr$non_white != 'NA') %>% 
  group_by(acyear) %>%
  group_modify(~
                 dissimilarity(data = .x,
                               group = 'non_white',
                               unit = 'hei',
                               weight = 'fte'))

# Asian vs White
ethnicity_pgr$asian [ethnicity_pgr$ethnicity == 'Asian'] <- 1
ethnicity_pgr$asian [ethnicity_pgr$ethnicity == 'Black'] <- NA
ethnicity_pgr$asian [ethnicity_pgr$ethnicity == 'Mixed'] <- NA
ethnicity_pgr$asian [ethnicity_pgr$ethnicity == 'Other'] <- NA
ethnicity_pgr$asian [ethnicity_pgr$ethnicity == 'Unknown/not applicable'] <- NA
ethnicity_pgr$asian [ethnicity_pgr$ethnicity == 'White'] <- 0

ethnicity_pgr$asian <- factor(ethnicity_pgr$asian,
                             levels = c(0,1),
                             labels = c("White", "Asian")) # labels

# dissimilarity for Asian
ethnicity_pgr %>% filter (ethnicity_pgr$asian != 'NA') %>% 
  group_by(acyear) %>%
  group_modify(~
                 dissimilarity(data = .x,
                               group = 'asian',
                               unit = 'hei',
                               weight = 'fte'))

# Black vs White
ethnicity_pgr$black [ethnicity_pgr$ethnicity == 'Asian'] <- NA
ethnicity_pgr$black [ethnicity_pgr$ethnicity == 'Black'] <- 1
ethnicity_pgr$black [ethnicity_pgr$ethnicity == 'Mixed'] <- NA
ethnicity_pgr$black [ethnicity_pgr$ethnicity == 'Other'] <- NA
ethnicity_pgr$black [ethnicity_pgr$ethnicity == 'Unknown/not applicable'] <- NA
ethnicity_pgr$black [ethnicity_pgr$ethnicity == 'White'] <- 0

ethnicity_pgr$black <- factor(ethnicity_pgr$black,
                             levels = c(0,1),
                             labels = c("White", "Black")) # labels

# dissimilarity for Black
ethnicity_pgr %>% filter (ethnicity_pgr$black != 'NA') %>% 
  group_by(acyear) %>%
  group_modify(~
                 dissimilarity(data = .x,
                               group = 'black',
                               unit = 'hei',
                               weight = 'fte'))

# mixed vs white
ethnicity_pgr$mixed [ethnicity_pgr$ethnicity == 'Asian'] <- NA
ethnicity_pgr$mixed [ethnicity_pgr$ethnicity == 'Black'] <- NA
ethnicity_pgr$mixed [ethnicity_pgr$ethnicity == 'Mixed'] <- 1
ethnicity_pgr$mixed [ethnicity_pgr$ethnicity == 'Other'] <- NA
ethnicity_pgr$mixed [ethnicity_pgr$ethnicity == 'Unknown/not applicable'] <- NA
ethnicity_pgr$mixed [ethnicity_pgr$ethnicity == 'White'] <- 0

ethnicity_pgr$mixed <- factor(ethnicity_pgr$mixed,
                             levels = c(0,1),
                             labels = c("White", "Mixed")) # labels

# dissimilarity for Mixed
ethnicity_pgr %>% filter (ethnicity_pgr$mixed != 'NA') %>% 
  group_by(acyear) %>%
  group_modify(~
                 dissimilarity(data = .x,
                               group = 'mixed',
                               unit = 'hei',
                               weight = 'fte'))

# Staff ----
# The first part of the analysis will consider all academic staff

## Pushing dataset for ethnicity for all PGR degree students
ethnicity_staff <- read_sheet('https://docs.google.com/spreadsheets/d/1kbmW8BlrgcaJd5GPUpgXDXSQCDyZY2eRxf0IyAE5CEg/edit#gid=1611015080')

# indicator variables for ethnicity
# non-white
ethnicity_staff$non_white [ethnicity_staff$ethnicity == 'Asian'] <- 1
ethnicity_staff$non_white [ethnicity_staff$ethnicity == 'Black'] <- 1
ethnicity_staff$non_white [ethnicity_staff$ethnicity == 'Mixed'] <- 1
ethnicity_staff$non_white [ethnicity_staff$ethnicity == 'Other'] <- 1
ethnicity_staff$non_white [ethnicity_staff$ethnicity == 'Unknown/not applicable'] <- NA
ethnicity_staff$non_white [ethnicity_staff$ethnicity == 'White'] <- 0


ethnicity_staff$non_white <- factor(ethnicity_staff$non_white,
                                  levels = c(0,1),
                                  labels = c("White", "Nonwhite")) # labels

# dissimilarity index for non-white
ethnicity_staff %>% filter (ethnicity_staff$non_white != 'NA') %>% 
  group_by(acyear) %>%
  group_modify(~
                 dissimilarity(data = .x,
                               group = 'non_white',
                               unit = 'hei',
                               weight = 'fte'))

# Asian vs White
ethnicity_staff$asian [ethnicity_staff$ethnicity == 'Asian'] <- 1
ethnicity_staff$asian [ethnicity_staff$ethnicity == 'Black'] <- NA
ethnicity_staff$asian [ethnicity_staff$ethnicity == 'Mixed'] <- NA
ethnicity_staff$asian [ethnicity_staff$ethnicity == 'Other'] <- NA
ethnicity_staff$asian [ethnicity_staff$ethnicity == 'Unknown/not applicable'] <- NA
ethnicity_staff$asian [ethnicity_staff$ethnicity == 'White'] <- 0

ethnicity_staff$asian <- factor(ethnicity_staff$asian,
                              levels = c(0,1),
                              labels = c("White", "Asian")) # labels

# dissimilarity for Asian
ethnicity_staff %>% filter (ethnicity_staff$asian != 'NA') %>% 
  group_by(acyear) %>%
  group_modify(~
                 dissimilarity(data = .x,
                               group = 'asian',
                               unit = 'hei',
                               weight = 'fte'))

# Black vs White
ethnicity_staff$black [ethnicity_staff$ethnicity == 'Asian'] <- NA
ethnicity_staff$black [ethnicity_staff$ethnicity == 'Black'] <- 1
ethnicity_staff$black [ethnicity_staff$ethnicity == 'Mixed'] <- NA
ethnicity_staff$black [ethnicity_staff$ethnicity == 'Other'] <- NA
ethnicity_staff$black [ethnicity_staff$ethnicity == 'Unknown/not applicable'] <- NA
ethnicity_staff$black [ethnicity_staff$ethnicity == 'White'] <- 0

ethnicity_staff$black <- factor(ethnicity_staff$black,
                              levels = c(0,1),
                              labels = c("White", "Black")) # labels

# dissimilarity for Black
ethnicity_staff %>% filter (ethnicity_staff$black != 'NA') %>% 
  group_by(acyear) %>%
  group_modify(~
                 dissimilarity(data = .x,
                               group = 'black',
                               unit = 'hei',
                               weight = 'fte'))

# mixed vs white
ethnicity_staff$mixed [ethnicity_staff$ethnicity == 'Asian'] <- NA
ethnicity_staff$mixed [ethnicity_staff$ethnicity == 'Black'] <- NA
ethnicity_staff$mixed [ethnicity_staff$ethnicity == 'Mixed'] <- 1
ethnicity_staff$mixed [ethnicity_staff$ethnicity == 'Other'] <- NA
ethnicity_staff$mixed [ethnicity_staff$ethnicity == 'Unknown/not applicable'] <- NA
ethnicity_staff$mixed [ethnicity_staff$ethnicity == 'White'] <- 0

ethnicity_staff$mixed <- factor(ethnicity_staff$mixed,
                              levels = c(0,1),
                              labels = c("White", "Mixed")) # labels

# dissimilarity for Mixed
ethnicity_staff %>% filter (ethnicity_staff$mixed != 'NA') %>% 
  group_by(acyear) %>%
  group_modify(~
                 dissimilarity(data = .x,
                               group = 'mixed',
                               unit = 'hei',
                               weight = 'fte'))
################################################################################################################
# Nationality ----
################################################################################################################
# First degree ----
# The first part of the analysis will consider all undergraduate students (first year students and non-first year students)

## Pushing dataset for nationality for all first degree students
nationality_fd <- read_sheet('https://docs.google.com/spreadsheets/d/1t13n8PYYs_hh30lIwrGsSPwlFvydv014i0he7RqUadc/edit#gid=1229016447')

# non-UK vs UK
nationality_fd$non_UK [nationality_fd$nationality == 'European Union'] <- 1
nationality_fd$non_UK [nationality_fd$nationality == 'Non-European Union'] <- 1
nationality_fd$non_UK [nationality_fd$nationality == 'Not known/stateless'] <- NA
nationality_fd$non_UK [nationality_fd$nationality == 'United Kingdom'] <- 0

nationality_fd$non_UK <- factor(nationality_fd$non_UK,
                                  levels = c(0,1),
                                  labels = c("UK", "non-UK")) # labels

nationality_fd %>% filter (nationality_fd$non_UK != 'NA') %>% 
  group_by(acyear) %>%
  group_modify(~
                 dissimilarity(data = .x,
                               group = 'non_UK',
                               unit = 'hei',
                               weight = 'fte'))

# non_EU vs UK
nationality_fdnon_EU = 99
nationality_fd$non_EU [nationality_fd$nationality == 'European Union'] <- NA
nationality_fd$non_EU [nationality_fd$nationality == 'Non-European Union'] <- 1
nationality_fd$non_EU [nationality_fd$nationality == 'Not known/stateless'] <- NA
nationality_fd$non_EU [nationality_fd$nationality == 'United Kingdom'] <- 0

nationality_fd$non_EU <- factor(nationality_fd$non_EU,
                                  levels = c(0,1),
                                  labels = c("UK", "non-EU")) # labels

nationality_fd %>% filter (nationality_fd$non_EU != 'NA') %>% 
  group_by(acyear) %>%
  group_modify(~
                 dissimilarity(data = .x,
                               group = 'non_EU',
                               unit = 'hei',
                               weight = 'fte'))              

# EU vs UK
nationality_fd$EU [nationality_fd$nationality == 'European Union'] <- 1
nationality_fd$EU [nationality_fd$nationality == 'Non-European Union'] <- NA
nationality_fd$EU [nationality_fd$nationality == 'Not known/stateless'] <- NA
nationality_fd$EU [nationality_fd$nationality == 'United Kingdom'] <- 0


nationality_fd$EU <- factor(nationality_fd$EU,
                              levels = c(0,1),
                              labels = c("UK", "EU")) # labels

nationality_fd %>% filter (nationality_fd$EU != 'NA') %>% 
  group_by(acyear) %>%
  group_modify(~
                 dissimilarity(data = .x,
                               group = 'EU',
                               unit = 'hei',
                               weight = 'fte'))

# PGR degree ----
# The first part of the analysis will consider all PGRR students (first year students and non-first year students)

## Pushing dataset for nationality for all first degree students
nationality_pgr <- read_sheet('https://docs.google.com/spreadsheets/d/1wR-nPWghx2b56uUP29g_CP3y6MqTeX66U7k_kwudRoM/edit#gid=1330270309')

# non-UK vs UK
nationality_pgr$non_UK [nationality_pgr$nationality == 'European Union'] <- 1
nationality_pgr$non_UK [nationality_pgr$nationality == 'Non-European Union'] <- 1
nationality_pgr$non_UK [nationality_pgr$nationality == 'Not known/stateless'] <- NA
nationality_pgr$non_UK [nationality_pgr$nationality == 'United Kingdom'] <- 0

nationality_pgr$non_UK <- factor(nationality_pgr$non_UK,
                                levels = c(0,1),
                                labels = c("UK", "non-UK")) # labels

nationality_pgr %>% filter (nationality_pgr$non_UK != 'NA') %>% 
  group_by(acyear) %>%
  group_modify(~
                 dissimilarity(data = .x,
                               group = 'non_UK',
                               unit = 'hei',
                               weight = 'fte')) 

# non_EU vs UK
nationality_pgrnon_EU = 99
nationality_pgr$non_EU [nationality_pgr$nationality == 'European Union'] <- NA
nationality_pgr$non_EU [nationality_pgr$nationality == 'Non-European Union'] <- 1
nationality_pgr$non_EU [nationality_pgr$nationality == 'Not known/stateless'] <- NA
nationality_pgr$non_EU [nationality_pgr$nationality == 'United Kingdom'] <- 0

nationality_pgr$non_EU <- factor(nationality_pgr$non_EU,
                                  levels = c(0,1),
                                  labels = c("UK", "non-EU")) # labels

nationality_pgr %>% filter (nationality_pgr$non_EU != 'NA') %>% 
  group_by(acyear) %>%
  group_modify(~
                 dissimilarity(data = .x,
                               group = 'non_EU',
                               unit = 'hei',
                               weight = 'fte'))              

# EU vs UK
nationality_pgr$EU [nationality_pgr$nationality == 'European Union'] <- 1
nationality_pgr$EU [nationality_pgr$nationality == 'Non-European Union'] <- NA
nationality_pgr$EU [nationality_pgr$nationality == 'Not known/stateless'] <- NA
nationality_pgr$EU [nationality_pgr$nationality == 'United Kingdom'] <- 0


nationality_pgr$EU <- factor(nationality_pgr$EU,
                              levels = c(0,1),
                              labels = c("UK", "EU")) # labels

nationality_pgr %>% filter (nationality_pgr$EU != 'NA') %>% 
  group_by(acyear) %>%
  group_modify(~
                 dissimilarity(data = .x,
                               group = 'EU',
                               unit = 'hei',
                               weight = 'fte'))

# Staff ----
# The first part of the analysis will consider all academic staff

## Pushing dataset for nationality for all first degree students
nationality_staff <- read_sheet('https://docs.google.com/spreadsheets/d/12NT0-FqsB8smNjxpES3cCXWSDgBbnDYZYKJxoFoLUko/edit#gid=1918533626')

# non-UK vs UK
nationality_staff$non_UK [nationality_staff$nationality == 'European Union'] <- 1
nationality_staff$non_UK [nationality_staff$nationality == 'Non-European Union'] <- 1
nationality_staff$non_UK [nationality_staff$nationality == 'Not known/stateless'] <- NA
nationality_staff$non_UK [nationality_staff$nationality == 'United Kingdom'] <- 0

nationality_staff$non_UK <- factor(nationality_staff$non_UK,
                                 levels = c(0,1),
                                 labels = c("UK", "non-UK")) # labels

nationality_staff %>% filter (nationality_staff$non_UK != 'NA') %>% 
  group_by(acyear) %>%
  group_modify(~
                 dissimilarity(data = .x,
                               group = 'non_UK',
                               unit = 'hei',
                               weight = 'fte')) 

# non_EU vs UK
nationality_staffnon_EU = 99
nationality_staff$non_EU [nationality_staff$nationality == 'European Union'] <- NA
nationality_staff$non_EU [nationality_staff$nationality == 'Non-European Union'] <- 1
nationality_staff$non_EU [nationality_staff$nationality == 'Not known/stateless'] <- NA
nationality_staff$non_EU [nationality_staff$nationality == 'United Kingdom'] <- 0

nationality_staff$non_EU <- factor(nationality_staff$non_EU,
                                 levels = c(0,1),
                                 labels = c("UK", "non-EU")) # labels

nationality_staff %>% filter (nationality_staff$non_EU != 'NA') %>% 
  group_by(acyear) %>%
  group_modify(~
                 dissimilarity(data = .x,
                               group = 'non_EU',
                               unit = 'hei',
                               weight = 'fte'))              

# EU vs UK
nationality_staff$EU [nationality_staff$nationality == 'European Union'] <- 1
nationality_staff$EU [nationality_staff$nationality == 'Non-European Union'] <- NA
nationality_staff$EU [nationality_staff$nationality == 'Not known/stateless'] <- NA
nationality_staff$EU [nationality_staff$nationality == 'United Kingdom'] <- 0


nationality_staff$EU <- factor(nationality_staff$EU,
                             levels = c(0,1),
                             labels = c("UK", "EU")) # labels

nationality_staff %>% filter (nationality_staff$EU != 'NA') %>% 
  group_by(acyear) %>%
  group_modify(~
                 dissimilarity(data = .x,
                               group = 'EU',
                               unit = 'hei',
                               weight = 'fte'))

################################################################################################################
# Disability ----
################################################################################################################
# First degree ----
# The first part of the analysis will consider all undergraduate students (first year students and non-first year students)

## Pushing dataset for disability for all first degree students
disability_fd <-  read_sheet('https://docs.google.com/spreadsheets/d/1G3SXcuOUSyvb5SZSB_kNcTy5LIXQslRIk8FlVVh-zdM/edit#gid=283440639')
names(disability_fd)

class(disability_fd$disability)
disability_fd$disability <- as.factor(disability_fd$disability)

disability_fd %>% 
  group_by(acyear) %>%
  group_modify(~
                 dissimilarity(data = .x,
                               group = 'disability',
                               unit = 'hei',
                               weight = 'fte'))

# PGR degree ----
# The first part of the analysis will consider all PGR students (first year students and non-first year students)

## Pushing dataset for disability for all PGR students
disability_pgr <-  read_sheet('https://docs.google.com/spreadsheets/d/16o4wb0uIM9nL58fuOX5KgGXscuPB3dK0AzxfC0M0emI/edit#gid=412824841')
names(disability_pgr)

class(disability_pgr$disability)
disability_pgr$disability <- as.factor(disability_pgr$disability)

disability_pgr %>% 
  group_by(acyear) %>%
  group_modify(~
                 dissimilarity(data = .x,
                               group = 'disability',
                               unit = 'hei',
                               weight = 'fte'))

# Staff ----
# The first part of the analysis will consider all academic staff

## Pushing dataset for disability for all first degree students
disability_staff <-  read_sheet('https://docs.google.com/spreadsheets/d/1xP0Jl4nZzxXGmpEtcc3eecQHYL_mGUcu6QQVm1s1O4Y/edit#gid=1893830763')
names(disability_staff)

class(disability_staff$disability)
disability_staff$disability <- as.factor(disability_staff$disability)

disability_staff %>% 
  group_by(acyear) %>%
  group_modify(~
                 dissimilarity(data = .x,
                               group = 'disability',
                               unit = 'hei',
                               weight = 'fte'))

################################################################################################################
# Age ----
################################################################################################################
# First degree ----
# The first part of the analysis will consider all undergraduate students (first year students and non-first year students)

## Pushing dataset for age for all first degree students
age_fd <- read_sheet('https://docs.google.com/spreadsheets/d/1R4WRaHcRM8dzELhBWWtmqdiczxfUrPliIgu6c0YkAks/edit#gid=685963593')
names(age_fd)

age_fd$over25 [age_fd$age == '18 years and under'] <- 0
age_fd$over25 [age_fd$age == '19 years'] <- 0
age_fd$over25 [age_fd$age == '20 years'] <- 0
age_fd$over25 [age_fd$age == '21-24 years'] <- 0
age_fd$over25 [age_fd$age == '25-29 years'] <- 1
age_fd$over25 [age_fd$age == '30 years and over'] <- 1
age_fd$over25 [age_fd$age == 'Age unknown'] <- NA

# labels
age_fd$over25 <- factor(age_fd$over25,
                          levels = c(0,1),
                          labels = c("Under 25", "25 and Over"))

age_fd %>% filter (age_fd$over25 != 'NA') %>% 
  group_by(acyear) %>%
  group_modify(~
                 dissimilarity(data = .x,
                               group = 'over25',
                               unit = 'hei',
                               weight = 'fte'))

# PGR degree ----
# The first part of the analysis will consider all PGR students (first year students and non-first year students)

## Pushing dataset for age for all PGR students
age_pgr <- read_sheet('https://docs.google.com/spreadsheets/d/17QjW4AbAARVZbgzqU1zWa5x-2WS0UJpFBYTvGR7x0EY/edit#gid=313802572')
names(age_pgr)

age_pgr$over25 [age_pgr$age == '18 years and under'] <- 0
age_pgr$over25 [age_pgr$age == '19 years'] <- 0
age_pgr$over25 [age_pgr$age == '20 years'] <- 0
age_pgr$over25 [age_pgr$age == '21-24 years'] <- 0
age_pgr$over25 [age_pgr$age == '25-29 years'] <- 1
age_pgr$over25 [age_pgr$age == '30 years and over'] <- 1
age_pgr$over25 [age_pgr$age == 'Age unknown'] <- NA

# labels
age_pgr$over25 <- factor(age_pgr$over25,
                        levels = c(0,1),
                        labels = c("Under 25", "25 and Over"))

age_pgr %>% filter (age_pgr$over25 != 'NA') %>% 
  group_by(acyear) %>%
  group_modify(~
                 dissimilarity(data = .x,
                               group = 'over25',
                               unit = 'hei',
                               weight = 'fte'))


