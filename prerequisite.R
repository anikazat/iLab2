# Preparation 

#### LOAD PACKAGES ####
# Specify your packages
packages <- c("tidyverse", "gridExtra", "shiny", "shinythemes", "bslib", "data.table", "ggpubr", 
              "globals", "caret")
# Probably add: "data.table", "ggpubr", "globals", "caret"
# Extract not installed packages
not_installed <- packages[!(packages %in% installed.packages()[ , "Package"])]
# Install not installed packages
if(length(not_installed)) install.packages(not_installed)               

library(tidyverse)
library(gridExtra)
library(shiny)
library(shinythemes)
library(bslib)
library(data.table)
library(ggpubr)
library(globals)
library(dplyr)
library(caret)

#### LOADING DATA ####
# path to folder that holds multiple .csv files
timesheets_path <- "./Animal Logic UTS MDSi 2022 Data Package/timesheets/"
versions_path <- "./Animal Logic UTS MDSi 2022 Data Package/versions/"

# create list of all .csv files in folder
timesheets_file_list <- list.files(path = timesheets_path, pattern = "*.csv")
versions_file_list <- list.files(path = versions_path, pattern = "*.csv")

# read in each .csv file in file_list and rbind them into a data frame called data 
df_timesheets <- 
  do.call("rbind", 
          lapply(timesheets_file_list, 
                 function(x) 
                   read.csv(paste(timesheets_path, x, sep=''), 
                            stringsAsFactors = FALSE)))
df_versions <- 
  do.call("rbind", 
          lapply(versions_file_list, 
                 function(x) 
                   read.csv(paste(versions_path, x, sep=''), 
                            stringsAsFactors = FALSE)))

# Read all other individual csv files
crew_file <- "./Animal Logic UTS MDSi 2022 Data Package/auxiliary/crew.csv"
df_crew <- read.csv(crew_file)

dept_dept_file <- "./Animal Logic UTS MDSi 2022 Data Package/auxiliary/dept-dept.csv"
df_dept_dept <- read.csv(dept_dept_file)

depts_file <- "./Animal Logic UTS MDSi 2022 Data Package/auxiliary/depts.csv"
df_depts <- read.csv(depts_file)

rtypes_file <- "./Animal Logic UTS MDSi 2022 Data Package/auxiliary/rtypes.csv"
df_review_types <- read.csv(rtypes_file)

shot_types_file <- "./Animal Logic UTS MDSi 2022 Data Package/auxiliary/shot-types.csv"
df_shot_types <- read.csv(shot_types_file)

shows_file <- "./Animal Logic UTS MDSi 2022 Data Package/auxiliary/shows.csv"
df_show <- read.csv(shows_file)


#### DATA CLEANING ####
# Convert date, week and months columns from chr to date format
df_timesheets$date <- as.Date(df_timesheets$date, format = "%Y-%m-%d")
df_timesheets$week <- as.Date(df_timesheets$week, format = "%Y-%m-%d")
df_timesheets$month <- as.Date(df_timesheets$month, format = "%Y-%m-%d")

# Convert date, week and months columns from chr to date format
df_versions$date <- as.Date(df_versions$date, format = "%Y-%m-%d")
df_versions$week <- as.Date(df_versions$week, format = "%Y-%m-%d")
df_versions$month <- as.Date(df_versions$month, format = "%Y-%m-%d")

# Convert pkgtype, eftype and state columns from logi to chr format
df_versions$pkgtype <- as.character(df_versions$pkgtype)
df_versions$eftype <- as.character(df_versions$eftype)
df_versions$state <- as.character(df_versions$state)

# Change blank ("") value in dirreviewed & intreviewed to "no"
df_versions$intreviewed[df_versions$intreviewed == ""] <- "no"
df_versions$dirreviewed[df_versions$dirreviewed == ""] <- "no"
df_versions$intreviewed[is.na(df_versions$intreviewed)] <- "no"
df_versions$dirreviewed[is.na(df_versions$dirreviewed)] <- "no"

#### MERGE MAIN & AUXILARY DATASETS ####
# merge dept_dept and depts
departments_df <- merge(x = df_dept_dept, y = df_depts, by = "department", all.x = TRUE)
names(departments_df) <- c("department_name","department_name_short","department_label")

# map departments and crew to timesheet
df_timesheets <- left_join(x=df_timesheets, y=departments_df, by=c("dept" = "department_name_short"))
df_timesheets <- left_join(x=df_timesheets, y=df_crew, by="username")
df_timesheets <- rename(df_timesheets, user_type = type)    

# map review type to version
df_versions <- left_join(x=df_versions, y=df_review_types, by=c("reviewtype"="rtype"))
df_versions <- rename(df_versions, c(review_dept = dept, review_type = type))
df_versions <- left_join(x=df_versions, y=df_crew, by="username")
df_versions <- rename(df_versions, crew_type = type)
df_distinct_departments <- departments_df[,c("department_name","department_label")] %>% 
  distinct(department_name, department_label)
df_versions <- left_join(x=df_versions, y=df_distinct_departments, by=c("review_dept"="department_name"))


df_versions_current <- df_versions
df_timesheets_current <- df_timesheets


#### VISUALISATIONS SET UP ####

departments_list <- as.list(unique(na.omit(df_versions$department_label)))

# Average count of reviews per shot for each department
reviews_test <- df_versions
reviews_test$intreviewed_num <- ifelse(reviews_test$intreviewed == "yes", 1, 0)
reviews_test$dirreviewed_num <- ifelse(reviews_test$dirreviewed == "yes", 1, 0)

# Internal Reviews
compare_int_reviews <- reviews_test %>% 
  # to only include main departments - REMOVE THIS IF YOU WANT TO SEE ALL DEPARTMENTS
  filter(department_label %in% c("Animation", "Assets", "Lighting", "Character FX", "FX", "Layout")) %>%
  # keep only production shots
  #filter(!is.na(shot) & shot != "" & !str_detect(shot, '^z')) %>% 
  # remove rows with no department label
  filter(!is.na(department_label)) %>% 
  # group by shot and department label
  group_by(shot, department_label) %>% 
  # get count of reviews for each shot for each department
  summarise(review_count = sum(intreviewed_num))

# Director Reviews
compare_dir_reviews <- reviews_test %>% 
  # to only include main departments - REMOVE THIS IF YOU WANT TO SEE ALL DEPARTMENTS
  filter(department_label %in% c("Animation", "Assets", "Lighting", "Character FX", "FX", "Layout")) %>%
  # keep only production shots
  #filter(!is.na(shot) & shot != "" & !str_detect(shot, '^z')) %>% 
  # remove rows with no department label
  filter(!is.na(department_label)) %>% 
  # group by shot and department label
  group_by(shot, department_label) %>% 
  # get count of reviews for each shot for each department
  summarise(review_count = sum(dirreviewed_num))


#### LINEAR MODEL DATA SET UP ####
df_versions[is.na(df_versions)]<- ""

df_timesheets[is.na(df_timesheets)]<- ""

temp_v_1 <- df_versions %>%
  group_by(month,department_label,intreviewed,dirreviewed) %>%
  count(intreviewed,dirreviewed) %>%
  setnames(c("month","department_label","intreviewed","dirreviewed","review_count"))

temp_v_2 <- df_versions %>%
  group_by(month,department_label,intreviewed,dirreviewed) %>%
  summarise(sum(bytes,na.rm=TRUE)) %>%
  setnames(c("month","department_label","intreviewed","dirreviewed","total_bytes"))

temp_v <- left_join(x=temp_v_1, y=temp_v_2,by=c("month"="month","department_label"="department_label","intreviewed"="intreviewed","dirreviewed"="dirreviewed"))  
temp_ts <- df_timesheets %>%
  group_by(month,department_label) %>%
  summarise(sum(as.numeric(days_logged),na.rm=TRUE)) %>%
  setnames(c("month","department_label","total_days_logged"))


