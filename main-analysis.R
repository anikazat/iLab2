#### LOAD LIBRARIES ####
library(tidyverse)
library(gridExtra)


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

# Change all blank ("") values in the dataset to NA's
df_timesheets[df_timesheets == ""] <- NA
df_versions[df_versions == ""] <- NA

# View missing data percentages
DataExplorer::plot_missing(df_timesheets)
DataExplorer::plot_missing(df_versions)




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

# View missing data percentages after auxiliary datasets are merged with df_timsheets and df_versions
DataExplorer::plot_missing(df_timesheets)
DataExplorer::plot_missing(df_versions)

#### VISUALISATOINS ####
# Count of Reviews (Internal or Director) For Each Department
final_plot_01 <- df_versions %>% 
  filter(department_label %in% c("Animation", "Assets", "Lighting", "Character FX", "FX", "Layout")) %>% 
  # filters to include only internal and director reviews - REMOVES SURPLUS REVIEWS
  filter(dirreviewed == "yes" | intreviewed == "yes") %>% 
  group_by(month, department_label) %>%
  count(month) %>% 
  ggplot(aes(month, n)) +
  geom_line(aes(col = department_label)) +
  #geom_vline(xintercept = as.numeric(as.Date("2021-05-01")), color = "red", lwd = 0.5) +
  scale_color_brewer(palette = "Set2") +
  scale_x_date(date_breaks = "1 month", date_labels =  "%b %Y") +
  theme_minimal() +
  theme(axis.text.x=element_text(angle=90, hjust=1), legend.position = "top") +
  labs(title = "Count of Reviews (Internal & Director) For Each Department", 
       y = "Count of Reviews", x = "Month", colour = "Department Name:")
final_plot_01
#ggsave(final_plot_01, filename = "./plots/new-plots/cnt-rev-month-together.png")





# Count of Internal & Director Reviews Per Month
data_a <- df_versions %>% 
  filter(department_label == "Assets" & intreviewed == "yes") %>% 
  group_by(month, department_label) %>% 
  summarise(total_count = sum(count))
data_b <- df_versions %>% 
  filter(department_label == "Assets" & dirreviewed == "yes") %>% 
  group_by(month, department_label) %>% 
  summarise(total_count = sum(count))
data_c <- df_versions %>% 
  filter(department_label == "Assets" & dirreviewed == "no" & intreviewed == "no") %>% 
  group_by("month", department_label) %>% 
  summarise(total_count = sum(count))

data_a$review_type <- "internal"
data_b$review_type <- "director"
data_c$review_type <- "surplus"

data_d <- rbind(data_a, data_b, data_c)

data_d %>% 
  ggplot(aes(month, total_count)) +
  geom_line(aes(col = review_type)) +
  scale_color_brewer(palette = "Set2") +
  #scale_colour_manual(values = c("blue", "black", "grey")) +
  scale_x_date(date_breaks = "1 month", date_labels =  "%b %Y") +
  #scale_y_continuous(limits = c(0,2000)) +
  theme_minimal() +
  theme(axis.text.x=element_text(angle=90, hjust=1), legend.position = "top") +
  labs(title = "Count of Internal, Director & Surplus Reviews Per Month", x = "Month", y = "Count", 
       colour = "Review Type:")

# Preparing data
test_a <- df_versions %>% 
  filter(department_label %in% c("Animation", "Assets", "Lighting", "Character FX", "FX", "Layout")) %>% 
  filter(intreviewed == "yes") %>% 
  group_by(month, department_label) %>% 
  summarise(total_count = sum(count)) %>% 
  arrange(department_label, month)
test_b <- df_versions %>% 
  filter(department_label %in% c("Animation", "Assets", "Lighting", "Character FX", "FX", "Layout")) %>% 
  filter(dirreviewed == "yes") %>% 
  group_by(month, department_label) %>% 
  summarise(total_count = sum(count)) %>% 
  arrange(department_label, month)
test_d <- df_versions %>% 
  filter(department_label %in% c("Animation", "Assets", "Lighting", "Character FX", "FX", "Layout")) %>% 
  filter(dirreviewed == "no" & intreviewed == "no") %>% 
  group_by(month, department_label) %>% 
  summarise(total_count = sum(count)) %>% 
  arrange(department_label, month)

test_a$review_type <- "internal"
test_b$review_type <- "director"
test_d$review_type <- "surplus"

test_c <- rbind(test_a, test_b)
test_e <- rbind(test_a, test_b, test_d)

final_plot_02 <- test_c %>% 
  ggplot(aes(month, total_count)) +
  geom_line(aes(col = review_type)) +
  scale_colour_manual(values = c("blue", "black")) +
  scale_x_date(date_breaks = "1 month", date_labels =  "%b %Y") +
  theme_minimal() +
  theme(axis.text.x=element_text(angle=90, hjust=1), legend.position = "top") +
  labs(title = "Count of Internal & Director Reviews Per Month", x = "Month", y = "Count", 
       colour = "Review Type:") +
  facet_wrap(~department_label)
final_plot_02
#ggsave(final_plot_02, filename = "./plots/new-plots/cnt-rev-month-seperate.png", dpi = 600, width = 14, height = 7, units = "in")

final_plot_02b <- test_e %>% 
  ggplot(aes(month, total_count)) +
  geom_line(aes(col = review_type)) +
  scale_colour_manual(values = c("blue", "black", "grey")) +
  scale_x_date(date_breaks = "1 month", date_labels =  "%b %Y") +
  theme_minimal() +
  theme(axis.text.x=element_text(angle=90, hjust=1), legend.position = "top") +
  labs(title = "Count of Internal, Director & Surplus Reviews Per Month", x = "Month", y = "Count", 
       colour = "Review Type:") +
  facet_wrap(~department_label)
final_plot_02b
#ggsave(final_plot_02b, filename = "./plots/new-plots/cnt-all-rev-month-seperate.png", dpi = 600, width = 14, height = 7, units = "in")





# Days worked per month
df_timesheets %>% 
  # filter by department
  filter(department_label == "Animation") %>% 
  group_by(month) %>% 
  summarise(total_days_logged = sum(days_logged)) %>% 
  ggplot(aes(month, total_days_logged)) +
  geom_line() +
  scale_x_date(date_breaks = "1 month", date_labels =  "%b %Y") +
  theme_minimal() +
  theme(axis.text.x=element_text(angle=90, hjust=1), legend.position = "top") +
  labs(title = "Number of Days Logged Per Month", x = "Month", y = "Count")





# Value of Versions (Internal, Director, or Not Reviewed) For Each Department
test <- df_versions
test$weight <- ifelse(test$dirreviewed == "yes", 1, ifelse(test$intreviewed == "yes", 0.75, 0.01))
final_plot_03 <- test %>% 
  filter(department_label %in% c("Animation", "Assets", "Lighting", "Character FX", "FX", "Layout")) %>% 
  group_by(month, department_label) %>%
  summarise(value = sum(weight)) %>% 
  ggplot(aes(month, value)) +
  geom_line(aes(col = department_label)) +
  scale_x_date(date_breaks = "1 month", date_labels =  "%b %Y") +
  theme_minimal() +
  theme(axis.text.x=element_text(angle=90, hjust=1), legend.position = "top") +
  labs(title = "Value of Versions For Each Department", 
       subtitle = "Internal Reviewed, Director Reviewed & Not Reviewed", 
       y = "Value of Versions", x = "Month", colour = "Department Name:")
final_plot_03
#ggsave(final_plot_03, filename = "./plots/new-plots/value-version-month-together.png")





# Average number of days between first and last version of shots - ONLY INCLUDING PROJECTION SHOTS
compare_dates <- df_versions %>% 
  # to only include main departments - REMOVE THIS IF YOU WANT TO SEE ALL DEPARTMENTS
  filter(department_label %in% c("Animation", "Assets", "Lighting", "Character FX", "FX", "Layout")) %>% 
  # keep only production shots
  #filter(!is.na(shot) & shot != "" & !str_detect(shot, '^z')) %>% 
  # remove rows with no department label
  filter(!is.na(department_label)) %>% 
  # group by shot and department label
  group_by(shot, department_label) %>% 
  # filter to keep only first and last date
  filter(date == min(date) | date == max(date)) %>% 
  # create a column for first and last date
  mutate(FirstDate = min(date), LastDate = max(date)) %>% 
  # select only necessary columns
  dplyr::select(shot, department_label, FirstDate, LastDate)
# add column for duration (time difference between first and last date)
compare_dates$active_days <- compare_dates$LastDate - compare_dates$FirstDate + 1

final_plot_04 <- compare_dates %>% 
  group_by(department_label) %>% 
  summarise(avg_active_days = mean(active_days)) %>% 
  ggplot(aes(department_label, avg_active_days)) +
  geom_col() +
  theme_minimal() +
  #theme(axis.text.x=element_text(angle=45, hjust=1)) +
  labs(title = "Average number of days between first and last version of shots", x = "Department", y = "Days")
final_plot_04
#ggsave(final_plot_04, filename = "./plots/new-plots/avg_active_days_on_shots2.png")





# Average days worked per shot
compare_days_logged <- df_timesheets %>% 
  # to only include main departments - REMOVE THIS IF YOU WANT TO SEE ALL DEPARTMENTS
  filter(department_label %in% c("Animation", "Assets", "Lighting", "Character FX", "FX", "Layout")) %>%
  # keep only production shots
  #filter(!is.na(shot) & shot != "" & !str_detect(shot, '^z')) %>% 
  # remove rows with no department label
  filter(!is.na(department_label)) %>% 
  # group by shot and department label
  group_by(shot, department_label) %>% 
  # get total days logged for each shot for each department
  summarise(total_days_logged = sum(days_logged))

final_plot_05 <- compare_days_logged %>% 
  # group by department
  group_by(department_label) %>% 
  # get average days logged per shot for each department
  summarise(average_days_logged_per_shot = mean(total_days_logged)) %>% 
  # plot
  ggplot(aes(department_label, average_days_logged_per_shot)) +
  geom_col() +
  theme_minimal() +
  labs(title = "Average Number of Days Logged Per Shot", x = "Department", y = "Average Number of Days")
final_plot_05
#ggsave(final_plot_05, filename = "./plots/new-plots/avg_days_per_shot.png")






# Average count of reviews per shot for each department
reviews_test <- df_versions
reviews_test$intreviewed_num <- ifelse(reviews_test$intreviewed == "yes", 1, 0)
reviews_test$dirreviewed_num <- ifelse(reviews_test$dirreviewed == "yes", 1, 0)

## FOR ALL SHOTS ##
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

final_plot_06 <- compare_int_reviews %>% 
  # filter to remove versions that were never reviewed
  filter(review_count > 0) %>% 
  # group by department label
  group_by(department_label) %>% 
  # get average number of reviews per shot for each department
  summarise(avg_reviews_per_shot = mean(review_count)) %>% 
  ggplot(aes(department_label, avg_reviews_per_shot)) +
  geom_col(fill = "#0c024d") +
  scale_y_continuous(limits = c(0, 11)) +
  theme_minimal() +
  labs(title = "Average Number of Internal Reviews Per Shot", 
       x = "Department", y = "Average Number of Reviews")
final_plot_06
#ggsave(final_plot_06, filename = "./plots/new-plots/avg_intreviews_per_shot.png")

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

final_plot_07 <- compare_dir_reviews %>% 
  # filter to remove versions that were never reviewed
  filter(review_count > 0) %>% 
  # group by department label
  group_by(department_label) %>% 
  # get average number of reviews per shot for each department
  summarise(avg_reviews_per_shot = mean(review_count)) %>% 
  ggplot(aes(department_label, avg_reviews_per_shot)) +
  geom_col(fill = "#0c024d") +
  scale_y_continuous(limits = c(0, 11)) +
  theme_minimal() +
  labs(title = "Average Number of Director Reviews Per Shot", 
       x = "Department", y = "Average Number of Reviews")
final_plot_07
#ggsave(final_plot_07, filename = "./plots/new-plots/avg_dirreviews_per_shot.png")

grid.arrange(final_plot_06, final_plot_07, final_plot_05, final_plot_04, ncol = 2, nrow = 2)
grid.arrange(final_plot_06, final_plot_07, ncol = 2, nrow = 1)




# Get ratio of director to internal reviews
compare_reviews_a <- compare_int_reviews %>% 
  # filter to remove versions that were never reviewed
  filter(review_count > 0) %>% 
  # group by department label
  group_by(department_label) %>% 
  # get average number of reviews per shot for each department
  summarise(avg_int_reviews_per_shot = mean(review_count))

compare_reviews_b <- compare_dir_reviews %>% 
  # filter to remove versions that were never reviewed
  filter(review_count > 0) %>% 
  # group by department label
  group_by(department_label) %>% 
  # get average number of reviews per shot for each department
  summarise(avg_dir_reviews_per_shot = mean(review_count))

# join internal and director tables
compare_reviews_c <- left_join(x=compare_reviews_a, y=compare_reviews_b, by="department_label")

# create column for ratio of director to internal reviews
compare_reviews_c$ratio <- compare_reviews_c$avg_dir_reviews_per_shot / compare_reviews_c$avg_int_reviews_per_shot

# plot
#highlight_df <- compare_reviews_c %>% filter(ratio == max(ratio))
final_plot_08 <- compare_reviews_c %>% 
  ggplot(aes(department_label, ratio)) +
  geom_col(fill = "#0c024d") +
  # to highlight the max column
  #geom_col(data=highlight_df, aes(department_label, ratio), fill ="light green") +
  theme_minimal() +
  labs(title = "Average Ratio of Director Reviews to Internal Reviews", subtitle = "Production Shots Only",
       x = "Department", y = "Ratio")
final_plot_08
#ggsave(final_plot_08, filename = "./plots/new-plots/avg_ratio_intreviews_to_dirreviews-prod1.png")




## FOR PRODUCTION SHOTS ONLY ##
# Internal Reviews
compare_int_reviews <- reviews_test %>% 
  # to only include main departments - REMOVE THIS IF YOU WANT TO SEE ALL DEPARTMENTS
  filter(department_label %in% c("Animation", "Assets", "Lighting", "Character FX", "FX", "Layout")) %>%
  # keep only production shots
  filter(!is.na(shot) & shot != "" & !str_detect(shot, '^z')) %>% 
  # remove rows with no department label
  filter(!is.na(department_label)) %>% 
  # group by shot and department label
  group_by(shot, department_label) %>% 
  # get count of reviews for each shot for each department
  summarise(review_count = sum(intreviewed_num))

final_plot_06 <- compare_int_reviews %>% 
  # filter to remove versions that were never reviewed
  filter(review_count > 0) %>% 
  # group by department label
  group_by(department_label) %>% 
  # get average number of reviews per shot for each department
  summarise(avg_reviews_per_shot = mean(review_count)) %>% 
  ggplot(aes(department_label, avg_reviews_per_shot)) +
  geom_col(fill = "#0c024d") +
  scale_y_continuous(limits = c(0, 11)) +
  theme_minimal() +
  labs(title = "Average Number of Internal Reviews Per Shot", 
       x = "Department", y = "Average Number of Reviews")
final_plot_06
#ggsave(final_plot_06, filename = "./plots/new-plots/avg_intreviews_per_shot.png")

# Director Reviews
compare_dir_reviews <- reviews_test %>% 
  # to only include main departments - REMOVE THIS IF YOU WANT TO SEE ALL DEPARTMENTS
  filter(department_label %in% c("Animation", "Assets", "Lighting", "Character FX", "FX", "Layout")) %>%
  # keep only production shots
  filter(!is.na(shot) & shot != "" & !str_detect(shot, '^z')) %>% 
  # remove rows with no department label
  filter(!is.na(department_label)) %>% 
  # group by shot and department label
  group_by(shot, department_label) %>% 
  # get count of reviews for each shot for each department
  summarise(review_count = sum(dirreviewed_num))

final_plot_07 <- compare_dir_reviews %>% 
  # filter to remove versions that were never reviewed
  filter(review_count > 0) %>% 
  # group by department label
  group_by(department_label) %>% 
  # get average number of reviews per shot for each department
  summarise(avg_reviews_per_shot = mean(review_count)) %>% 
  ggplot(aes(department_label, avg_reviews_per_shot)) +
  geom_col(fill = "#0c024d") +
  scale_y_continuous(limits = c(0, 11)) +
  theme_minimal() +
  labs(title = "Average Number of Director Reviews Per Shot", 
       x = "Department", y = "Average Number of Reviews")
final_plot_07
#ggsave(final_plot_07, filename = "./plots/new-plots/avg_dirreviews_per_shot.png")

grid.arrange(final_plot_06, final_plot_07, final_plot_05, final_plot_04, ncol = 2, nrow = 2)
grid.arrange(final_plot_06, final_plot_07, ncol = 2, nrow = 1)


# Get ratio of director to internal reviews
compare_reviews_a <- compare_int_reviews %>% 
  # filter to remove versions that were never reviewed
  filter(review_count > 0) %>% 
  # group by department label
  group_by(department_label) %>% 
  # get average number of reviews per shot for each department
  summarise(avg_int_reviews_per_shot = mean(review_count))

compare_reviews_b <- compare_dir_reviews %>% 
  # filter to remove versions that were never reviewed
  filter(review_count > 0) %>% 
  # group by department label
  group_by(department_label) %>% 
  # get average number of reviews per shot for each department
  summarise(avg_dir_reviews_per_shot = mean(review_count))

# join internal and director tables
compare_reviews_c <- left_join(x=compare_reviews_a, y=compare_reviews_b, by="department_label")

# create column for ratio of director to internal reviews
compare_reviews_c$ratio <- compare_reviews_c$avg_dir_reviews_per_shot / compare_reviews_c$avg_int_reviews_per_shot

# plot
#highlight_df <- compare_reviews_c %>% filter(ratio == max(ratio))

final_plot_08 <- compare_reviews_c %>% 
  ggplot(aes(department_label, ratio)) +
  geom_col(fill = "#0c024d") +
  # to highlight the max column
  #geom_col(data=highlight_df, aes(department_label, ratio), fill ="light green") +
  theme_minimal() +
  labs(title = "Average Ratio of Director Reviews to Internal Reviews", subtitle = "Production Shots Only",
       x = "Department", y = "Ratio")
final_plot_08
#ggsave(final_plot_08, filename = "./plots/new-plots/avg_ratio_intreviews_to_dirreviews-prod1.png")







# Calculating efficiency how Animal Logic does it
# Calculating cost
cost <- df_timesheets %>% 
  # to only include main departments - REMOVE THIS IF YOU WANT TO SEE ALL DEPARTMENTS
  filter(department_label %in% c("Animation", "Assets", "Lighting", "FX", "Layout")) %>% 
  group_by(week, department_label) %>%
  summarise(total_days_logged = sum(days_logged)) 
cost %>% 
  ggplot(aes(week, total_days_logged)) +
  geom_line(aes(col = department_label)) +
  scale_x_date(date_breaks = "1 week", date_labels =  "%d %b", 
               limits = as.Date(c("2019-01-21", "2022-06-13"), format="%Y-%m-%d")) +
  theme_minimal() +
  theme(axis.text.x=element_text(angle=90, hjust=1), legend.position = "top") +
  labs(title = "Cost (Human Effort)", 
       y = "Days Logged Per Week", x = "Week", colour = "Departments:")
#ggsave(cost, filename = "./plots/new-plots/cost-days-per-week.png", dpi = 600, width = 18, height = 7, units = "in")

# Calculating output
output <- df_versions %>% 
  filter(department_label %in% c("Animation", "Assets", "Lighting", "FX", "Layout")) %>% 
  group_by(week, department_label) %>%
  count(week) 
output %>% 
  ggplot(aes(week, n)) +
  geom_line(aes(col = department_label)) +
  scale_x_date(date_breaks = "1 week", date_labels =  "%d %b", 
               limits = as.Date(c("2019-01-21", "2022-06-13"), format="%Y-%m-%d")) +
  theme_minimal() +
  theme(axis.text.x=element_text(angle=90, hjust=1), legend.position = "top") +
  labs(title = "Output (Raw Output)", 
       y = "Reviews Produced Per Week", x = "Week", colour = "Departments:")

# Calculating productivity (output / cost)
output <- rename(output, total_reviews_produced = n)    
productivity <- left_join(x=cost, y=output, by=c("week", "department_label"))
productivity$productivity <- productivity$total_reviews_produced / productivity$total_days_logged
productivity %>% 
  ggplot(aes(week, productivity)) +
  geom_line(aes(col = department_label)) +
  scale_x_date(date_breaks = "1 month", date_labels =  "%d %b") +
  #scale_color_manual(values = c("#ba4620", "#5cc6cc", "#7cd9b0", "#ebe154", "#c753f5")) +
  theme_minimal() +
  theme(axis.text.x=element_text(angle=90, hjust=1), legend.position = "top") +
  labs(title = "Productivity (Output / Cost)", 
       y = "Reviews Per Day Worked", x = "Week", colour = "Departments:")





