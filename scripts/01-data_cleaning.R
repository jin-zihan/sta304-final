#### Preamble ####
# Purpose: Clean the survey data downloaded from U.S. Department of Education  [https://collegescorecard.ed.gov/data]
# Author: Zihan Jin
# Data: 30 April 2022
# Contact: zihan.jin@mail.utoronto.ca
# Pre-requisites: 
# - Need to have downloaded the College Scorecard data and saved it to inputs/data

#### Workspace setup ####
# Use R Projects, not setwd().
library(tidyverse)
library(reshape2)
library(patchwork)
library(car)
# Read in the raw data. 
college <- read_csv("Most-Recent-Cohorts-Institution.csv")
              
# convert data to correct type corrosponding to its real meaning
college$SAT_AVG <- as.numeric(college$SAT_AVG)
college$UGDS_MEN <- as.numeric(college$UGDS_MEN)
college$UGDS_WOMEN <- as.numeric(college$UGDS_WOMEN)
college$UGDS_WHITE <- as.numeric(college$UGDS_WHITE)
college$UGDS_BLACK <- as.numeric(college$UGDS_BLACK)
college$UGDS_HISP <- as.numeric(college$UGDS_HISP)
college$UGDS_ASIAN <- as.numeric(college$UGDS_ASIAN)
college$UGDS_AIAN <- as.numeric(college$UGDS_AIAN)
college$UGDS_NHPI <- as.numeric(college$UGDS_NHPI)
college$C150_4_WHITE <- as.numeric(college$C150_4_WHITE)
college$C150_4_BLACK <- as.numeric(college$C150_4_BLACK)
college$C150_4_HISP <- as.numeric(college$C150_4_HISP)
college$C150_4_ASIAN <- as.numeric(college$C150_4_ASIAN)
college$C150_4_AIAN <- as.numeric(college$C150_4_AIAN)
college$C150_4_NHPI <- as.numeric(college$C150_4_NHPI)
college$TUITIONFEE_IN <- as.numeric(college$TUITIONFEE_IN)
college$TUITIONFEE_OUT <- as.numeric(college$TUITIONFEE_OUT)
college$MD_EARN_WNE_P6 <- as.numeric(college$MD_EARN_WNE_P6)
college$MD_EARN_WNE_P8 <- as.numeric(college$MD_EARN_WNE_P8)
college$MD_EARN_WNE_P10 <- as.numeric(college$MD_EARN_WNE_P10)

# drop missing value

college <- college %>%
  filter(!is.na(SAT_AVG) & !is.na(C150_4_WHITE) & !is.na(C150_4_BLACK) & !is.na(C150_4_HISP) & 
         !is.na(C150_4_ASIAN) & !is.na(C150_4_AIAN) & !is.na(C150_4_NHPI) & !is.na(TUITIONFEE_IN ) & 
         !is.na(TUITIONFEE_OUT) & !is.na(UGDS_MEN) & !is.na(UGDS_WOMEN) & !is.na(UGDS_WHITE) & 
         !is.na(UGDS_BLACK) & !is.na(UGDS_HISP) & !is.na(UGDS_ASIAN) & !is.na(UGDS_AIAN) & !is.na(UGDS_NHPI) & 
         !is.na(MD_EARN_WNE_P6) & !is.na(MD_EARN_WNE_P8) & !is.na(MD_EARN_WNE_P10))




















         
