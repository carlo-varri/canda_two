#cleaning analysing and visualising canadian immigration data 
#sourced from https://github.com/rashida048/Datasets/blob/master/Canada.xlsx
setwd("~/Desktop/Learning R/Canada-immigration")
library(tidyverse)
library(readxl)
library(reclin)
library(stringr)
library(visdat)
library(fuzzyjoin)

###IMPORTING ####

# a function to import the four excel files in my wd
file_list <- list.files(pattern='*.xlsx')
df_list <- lapply(file_list, read_excel, skip = 1) #reading in the files, skipping the first row  
#now I want to delete the header name for df's 2 3 and 4 so I can bind rows 
names(df_list[c(2,3,4)]) <- NULL
#binding rows to create one large dataset
df <- bind_rows(df_list)

### CLEANING ####

names(df) <- tolower(names(df)) #all col names to lower 
sapply(df[1:2], unique) #can see first two cols are uninteresting so will drop them 
df <- select(df,-c(1,2))
#area, reg and dev also have no use to me 
df <- select(df,-c(area, reg, dev))
#now, I want to remove the rows which are all NA.
#let me see if there are any columns in which the only NA's are those which are NA rows 
vis_miss(df)
#second row, areaname is the winner. 
#so, filtering out rows if they have NA in col. 2
df <- df %>% filter(!is.na(areaname)) 
#vis_miss(df) shows this has worked
#also removing row of zeros
df <- df %>% filter(!areaname == 0)
#now, I want to set the areaname and regname to factors
#first, let me the area/ region names are as expected ... 
sapply(df[c("areaname", "regname")], unique)
#okay, I can see we have some typos in both
#luckily, I have data on the correct spellings for both 
spellings <- read_excel("~/Desktop/Learning R/Canada/Correct_spellings.xlsx")
#pulling the unique areanames from spelling
areas <- as.tibble(unique(spellings$AreaName))
names(areas) <- "AreaName"
#creating special use tibble for this 
df_areas <- as.tibble(df$areaname)
names(df_areas) <- "AreaName"

#comparing area entries in the df with correct spellings 
correct_areas <- stringdist_left_join(df_areas, areas, by = "AreaName", method ="qgram")
#introduced 2 NAs - replacing manually 
correct_areas <- replace(correct_areas, is.na(correct_areas), "Latin America and the Caribbean")
#replacing areaname column in df with the corrected names 
df['areaname'] <- correct_areas['AreaName.y']
df <- df %>% select(-areanames) #removing accidenatal new column created 

#now doing the same for country names 
countries <- as.tibble(unique(spellings$OdName))
df_countries <- as.tibble(df$odname)
correct_countries<-  stringdist_left_join(df_countries, 
                                          countries, 
                                          by = 'value', 
                                          method ="qgram")