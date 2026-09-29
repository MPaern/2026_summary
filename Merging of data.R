# initial merging of data from 2026 to 1 csv file + overview

# setup -------------------------------------------------------------------
# libraries used in this project
library(tidyverse)
library(fs)
library(ggplot2)
library(dplyr)
library(suncalc)
#library(suntools)
library(lubridate)
library(readr)
#library(esquisse)
#library(MetBrewer)
#library(purrr)
#library(sp)
#library(sf)
#library(hms)
#library(maptools)
#library(ggcorrplot)


# read location overview data in ------------------------------------------------------------

deployment <-  read_csv("data/survey_deployment.csv")
maintenance <-  read_csv("data/survey_maintenance.csv")
overview_data <-  read_csv("data/overview_table_25.csv")
coordinates <- read_csv("data/GPS_2025.csv")
water <- read_csv("data/distance_from_water_2025.csv")


# make directories of all the sites and add the merged id files so when there is a problem with one of the sites, it can be fixed on it's own -----------

# source and destination directories 
source_dir <- "P:/SW_CoastalMonitoring/Data_collection_2026"
destination_dir <- "data/IDfiles"

# list of all 50 site folders in source_dir
site_folders <- list.dirs(source_dir, recursive = FALSE)

# Find id.csv files in every site and merge them into one all_id.csv file

# Loop through each site folder
for (site in site_folders[15:length(site_folders)]) {
  
  site_wav <- path(site, "WAV")
  site_name <- path_file(site)
  
  cat("Processing:", site_name, "\n")
  
  # Find id.csv recursively
  csv_files <- dir_ls(
    site_wav,
    recurse = TRUE,
    regexp = "id\\.csv$",
    type = "file",
    fail = FALSE
  )
  
  csv_files <- csv_files[!grepl("[/\\\\]Data[/\\\\]", csv_files, ignore.case = TRUE)]
  
  # merging, make date and date-12 characters to make merging easier, make them to be dates later again!
  
  if (length(csv_files) > 0) {
    
    merged_data <- bind_rows(
      lapply(csv_files, function(f) {
        readr::read_csv(
          f,
          col_types = readr::cols(
            DATE = readr::col_character(),
            `DATE-12` = readr::col_character(),
            .default = readr::col_guess()
          ),
          show_col_types = FALSE
        )
      })
    ) 
    
    # Create a corresponding site folder in "data" 
    site_output_folder <- file.path(destination_dir, site_name) 
    if (!dir.exists(site_output_folder)) 
      dir.create(site_output_folder, recursive = TRUE) 
    
    # Save merged file 
    
    write_csv(merged_data, file.path(site_output_folder, "all_id.csv")) }
}


# Combine all into one file for 2026  --------------------------------------------------------------

# Some parsing problems, but are they relevant?

files_dir <- "data/IDfiles"
site_folders <- list.dirs(files_dir, recursive = FALSE)

# read in all the csv-s

# Loop through each site folder
for (site in site_folders) {
  
  # extract site name
  site_name <- basename(site)
  
  # clean name
  site_name_clean <- str_replace_all(site_name, "-", "")
  
  # construct file path using site_name
  csv_file <- file.path(files_dir, site_name, "all_id.csv")
  
  # Read the CSV file
  input_data <- read_csv(csv_file)
  
  # Dynamically assign it to a variable
  assign(paste0(site_name_clean), input_data, envir = .GlobalEnv)
}

# merge csv

# get all CM
dfs <- mget(ls(pattern = "^CM[0-9]{2}$"))

# merge and create Site column
cm_2026 <- bind_rows(dfs, .id = "Site") %>%
  mutate(Site = sub("CM([0-9]{2})", "CM-\\1", Site))

# check cm file

str(cm_2026) 

summary(cm_2026$`AUTO ID*`) 
colSums(is.na(cm_2026))
na_rows <- cm_2026 %>% filter(is.na(`OUT FILE FS`))

# not sure why it doesn't have a OUT FILE FS name, adding one from IN FILE column.

cm_2026 <- cm_2026 %>%
  mutate(
    `OUT FILE FS` = coalesce(`OUT FILE FS`, `IN FILE`)
  )

colSums(is.na(cm_2026))

# change DATE and DATE-12 to date format

unique(cm_2026$DATE)

cm_2026 <- cm_2026 %>%
  mutate(
    DATE = parse_date_time(DATE, orders = c("dmy", "ymd")),
    `DATE-12` = parse_date_time(`DATE-12`, orders = c("dmy", "ymd"))
  )


write.csv(cm_2026, "cm_2026_total.csv")


# one file for 2026 -------------------------------
cm <- read.csv("cm_2025_total.csv")

# check data

str(cm) 
summary(cm) 
summary(cm) 
colSums(is.na(cm))

na_rows <- cm %>% filter(is.na(DATE))

#choose variables for dataset
cm <- cm %>% 
  rename(
    filename = "OUT_FILE_FS") %>% 
  mutate(autoid = factor(autoid)) %>% 
  dplyr::select(OUTDIR, FOLDER, IN_FILE, filename, DURATION, 
                DATE, TIME, HOUR,
                DATE_12, TIME_12, HOUR_12,
                autoid, PULSES, MATCH_RATIO, ALTERNATE_1, Site
  )

# rows that don't have a file name, but have autoid
na_rows <- cm %>% filter(is.na(filename)) 
# delete Noise rows and those that last too little
na_rows <- na_rows[na_rows$autoid != "Noise", ]
na_rows <- na_rows[na_rows$DURATION > 1, ]
# why is there 57 rows of data without filenames?

# 4 locations have the DATE columns in wrong format (CM-07, CM-08, CM-26, CM-45)

# new date column
cm$DATE_clean <- as.Date(NA)

# 2 formats
idx_iso <- grepl("^\\d{4}-\\d{2}-\\d{2}$", cm$DATE)
idx_short <- grepl("^\\d{1,2}-\\d{1,2}-\\d{2}$", cm$DATE)

cm$DATE_clean[idx_iso] <- ymd(cm$DATE[idx_iso])
cm$DATE_clean[idx_short] <- dmy(cm$DATE[idx_short])

# Fix year (all short ones should be 2025)
cm$DATE_clean[idx_short] <- update(cm$DATE_clean[idx_short], year = 2025)

# Final format
cm$DATE_clean <- format(cm$DATE_clean, "%Y-%m-%d")

# Same for DATE_12 column

# new column
cm$DATE_12_clean <- as.Date(NA)

# 2 formats
idx_iso <- grepl("^\\d{4}-\\d{2}-\\d{2}$", cm$DATE_12)
idx_short <- grepl("^\\d{1,2}-\\d{1,2}-\\d{2}$", cm$DATE_12)

# Parse
cm$DATE_12_clean[idx_iso] <- ymd(cm$DATE_12[idx_iso])
cm$DATE_12_clean[idx_short] <- dmy(cm$DATE_12[idx_short])

# Fix year (all short ones should be 2025)
cm$DATE_12_clean[idx_short] <- update(cm$DATE_12_clean[idx_short], year = 2025)

# Final format
cm$DATE_12_clean <- format(cm$DATE_12_clean, "%Y-%m-%d")

# make dates
cm$DATE_clean <- as.Date(cm$DATE_clean)
cm$DATE_12_clean <- as.Date(cm$DATE_12_clean)

# remove other date columns and replace them with new ones

cm <- cm %>%
  select(-DATE) %>% 
  relocate(DATE_clean, .after = DURATION) %>%
  rename (DATE = DATE_clean) %>% 
  select(-DATE_12) %>% 
  relocate(DATE_12_clean, .after = HOUR) %>%
  rename (DATE_12 = DATE_12_clean)


# find PIPPIP and change them to PIPPYG

cm$autoid[cm$autoid == "PIPPIP"] <- "PIPPYG"

# Overview table for 2025--------------------------

# new table called overview_summary

overview_summary <- cm %>%
  group_by(Site) %>%
  summarise(
    total = n(),
    noise = sum(autoid == "Noise"),
    noise_pct = 100 * noise / total,
    pnatcalls = sum(autoid == "PIPNAT"),
    batcalls = sum(autoid!="Noise"),
    pnatcalls_pct = 100 * pnatcalls/ batcalls,
    days_active = sum(n_distinct(DATE)),
    earliest_active = min(DATE),
    last_active = max(DATE),
  )

# add info from other tables
# Make date more readable
deployment <- deployment %>%
  mutate(`Date and Time Deployed` = parse_date_time(`Date and Time Deployed`,
                                                    orders = "m/d/Y H:M"))

retrieval <- maintenance %>% filter(`Type of maintenance` == "Retrival") %>%
  mutate(
    `Date and Time` = parse_date_time(`Date and Time`,
                                      orders = c("m/d/Y H:M:S", "d/m/Y H:M")))

# Join onto deployment
deployment <- deployment %>%
  left_join(retrieval, by = "Site Name")

deployment <- deployment %>% 
  rename(
    Site = "Site Name")

coordinates <- coordinates %>% 
  rename(
    Site = "Name")

overview_data <- overview_data %>% 
  rename(
    Site = Location)

overview_summary <- overview_summary %>%
  left_join(
    deployment %>%
      select(
        Site,
        date_deployed = `Date and Time Deployed`,
        date_retrieved = `Date and Time`,
      ),
    by = "Site"
  )

overview_summary <- overview_summary %>%
  left_join(
    overview_data %>%
      select(Site,
             type = type),
    by= "Site"
  )

overview_summary <- overview_summary %>%
  left_join(
    coordinates %>%
      select(Site,
             latitude = Latitude,
             longitude = Longitude),
    by= "Site"
  )

# distance to water bodies

overview_summary <- overview_summary %>%
  left_join(
    water %>%
      select(Site,
             m_to_freshwater = "distance to freshwater (m)",
             m_to_coast = "distance to coast (m)"),
    by= "Site"
  )


# one working file for 2025--------------


# found 2 gaps (CM-35, CM-44) that are not explained, ran Kaleidoscope for those timeframes again to get an id.csv file

df35 <- read.csv("P:/SW_CoastalMonitoring/Data_collection_2025/CM-35/WAV/KPRO_V1/New_25.09.2025/id.csv")
df44 <- read.csv("P:/SW_CoastalMonitoring/Data_collection_2025/CM-44/WAV/KPRO_V1/New_30.09.2025/id.csv")

df35$Site <- "CM-35"
df44$Site <- "CM-44"

df35$filename <- df35$IN.FILE
df44$filename <- df44$IN.FILE

df35 <- df35 %>% 
  rename(
    IN_FILE = "IN.FILE",
    DATE_12 = "DATE.12",
    TIME_12 = "TIME.12",
    HOUR_12 = "HOUR.12",
    autoid = "AUTO.ID.",
    MATCH_RATIO = "MATCH.RATIO",
    ALTERNATE_1 = "ALTERNATE.1") %>% 
  mutate(autoid = factor(autoid),
         DATE_12 = as.Date(DATE_12, format = "%d/%m/%Y"),
         DATE = as.Date(DATE, format = "%d/%m/%Y")) %>% 
  dplyr::select(OUTDIR, FOLDER, IN_FILE, filename, DURATION, 
                DATE, TIME, HOUR,
                DATE_12, TIME_12, HOUR_12,
                autoid, PULSES, MATCH_RATIO, ALTERNATE_1, Site
  )

df44 <- df44 %>% 
  rename(
    IN_FILE = "IN.FILE",
    DATE_12 = "DATE.12",
    TIME_12 = "TIME.12",
    HOUR_12 = "HOUR.12",
    autoid = "AUTO.ID.",
    MATCH_RATIO = "MATCH.RATIO",
    ALTERNATE_1 = "ALTERNATE.1") %>% 
  mutate(autoid = factor(autoid),
         DATE_12 = as.Date(DATE_12, format = "%d/%m/%Y"),
         DATE = as.Date(DATE, format = "%d/%m/%Y")) %>% 
  dplyr::select(OUTDIR, FOLDER, IN_FILE, filename, DURATION, 
                DATE, TIME, HOUR,
                DATE_12, TIME_12, HOUR_12,
                autoid, PULSES, MATCH_RATIO, ALTERNATE_1, Site
  )

# find what files are not in cm and add those there

new_from_35 <- df35 %>%
  anti_join(cm, by = "filename")

new_from_44 <- df44 %>%
  anti_join(cm, by = "filename")

cm <- cm %>%
  bind_rows(new_from_35, new_from_44)


# full file

write.csv(cm, "cm_2025.csv")

cm <- read.csv("cm_2025.csv")

# Where is the most noise, visualisation  -------------------------------------------------------------------

# plot of noise and nr of bat calls
ggplot(overview_summary) +
  aes(x = `noise_pct`, y = `batcalls`, colour = type) +
  geom_point(size = 3.35, 
             shape = "diamond") +
  scale_color_viridis_d(option = "cividis", direction = 1) +
  labs(x = "Percentage of noise files in recordings", 
       y = "Number of recordings with bat calls", title = "Correlation between noise and bat calls", subtitle = "By location") +
  theme_classic() +
  theme(legend.text = element_text(face = "bold"), legend.title = element_text(face = "bold"))

barnoise <- ggplot(cm) + 
  geom_bar(aes(x= Site, fill = autoid), position = "fill") +
  scale_fill_manual(name = "AutoID", values = met.brewer("Signac", n = 14)) + 
  ylab("Proportion of recordings") + 
  xlab("Site") +
  theme(text = element_text(size = 10)) 

barnoise


# visualize recording period


ggplot(cm) + 
  geom_bin2d(aes(x = DATE_12, y = Site), bins = 100) +  # Adjust bins for detail
  scale_fill_viridis_c() +  # Better color scale for density
  xlab("Month") + ylab("Site") +
  scale_x_date(date_breaks = "1 month", , date_labels = "%b") +
  ggtitle("Recording period 2025") + 
  theme_minimal()


# gaps in dataset, more info for overview ---------------------------------------------------------
# DATE_12 is the start of the night eg. the start date. 

# all dates all sites should be active on
complete_dates <- overview_summary %>% 
  mutate(
    BeginDate = as.Date(date_deployed),
    EndDate   = coalesce(as.Date(date_retrieved), as.Date("2025-10-22")) - 1,
    full_dates = map2(BeginDate, EndDate, ~ seq.Date(from = .x, to = .y, by = "day"))
  ) %>%
  select(Site, full_dates) %>%
  unnest(full_dates) %>%
  rename(ExpectedDate = full_dates)

# all dates I have in the dataset for each site
available_dates <- cm %>%
  mutate(ActualDate = as.Date(DATE_12)) %>%
  reframe(ActualDate = unique(ActualDate), .by = "Site")

# missing nights for each site
missing_dates <- complete_dates %>%
  anti_join(available_dates, by = c("Site", "ExpectedDate" = "ActualDate"))

# View missing nights by site
ggplot(missing_dates) + 
  geom_point(aes(x = ExpectedDate, y = Site)) +  
  scale_fill_viridis_c() + 
  xlab("Date in season") + ylab(" ") +
  ggtitle("Missing dates") + 
  theme_minimal()

# add missing nights to overview_summary

n_missing_days <- missing_dates %>%
  group_by(Site) %>%
  summarise(
    missing_days = n(),
  )

overview_summary <- overview_summary %>%
  left_join(
    n_missing_days %>%
      select(Site,
             missing_days = missing_days),
    by= "Site"
  )

# change NA to 0 in missing_days

overview_summary <- overview_summary %>% 
  mutate(missing_days = ifelse(is.na(missing_days), 0, missing_days))

# some types still wrong

overview_summary[1,13] = "inland" 
overview_summary[29,13] = "coast" 
overview_summary[34,13] = "coast" 

# change missing retrival dates to 22-10-2025

overview_summary <- overview_summary %>% 
  mutate(date_retrieved = if_else(
    is.na(date_retrieved),
    as.Date("2025-10-22"),
    date_retrieved
  ))

#final product ----------------------------------------------------------------------------

write.csv(overview_summary, "overview_2025.csv")

# going of from here --------------------

overview_summary <- read.csv("overview_2025.csv")

# write.csv(missing_dates, "Missing_dates.csv")

write.csv(missing_dates, "missing_dates_2025.csv")


#found gaps in cm total-------------------

cmtotal <- read.csv("cm_2025_total.csv")
cmtotal[1]<- NULL

table(cmtotal$Site)

str(cmtotal) 
colSums(is.na(cmtotal))

# gaps in CM-35 and CM-44

df35 <- read.csv("P:/SW_CoastalMonitoring/Data_collection_2025/CM-35/WAV/KPRO_V1/New_25.09.2025/id.csv")
df44 <- read.csv("P:/SW_CoastalMonitoring/Data_collection_2025/CM-44/WAV/KPRO_V1/New_30.09.2025/id.csv")

# These SD cards have been run trough K twice, so IN FILE is the filename aka OUT FILE FS

df35$Site <- "CM-35"
df35 <- df35 %>%
  relocate(Site, .before =  INDIR)
df44$Site <- "CM-44"
df44 <- df44 %>%
  relocate(Site, .before =  INDIR)

str(df35)
str(df44)

# make new df-s the same format as cmtotal

# same column names
names(df35) <- names(cmtotal)
names(df44) <- names(cmtotal)

# same column classes
df35[] <- Map(function(x, y) as(y, class(x)), cmtotal, df35)
df44[] <- Map(function(x, y) as(y, class(x)), cmtotal, df44)

# make out file fs same as infile

df35$OUT_FILE_FS <- df35$IN_FILE
df44$OUT_FILE_FS <- df44$IN_FILE

# find what files are not in cm and add those there

new_from_35 <- df35 %>%
  anti_join(cmtotal, by = "OUT_FILE_FS")

new_from_44 <- df44 %>%
  anti_join(cmtotal, by = "OUT_FILE_FS")

cmtotal <- cmtotal %>%
  bind_rows(new_from_35, new_from_44)

table(cmtotal$Site)

str(cmtotal) 
colSums(is.na(cmtotal))

# replace NA in OUT_FILE_FS with IN_FILE

cmtotal <- cmtotal %>% 
  mutate(OUT_FILE_FS = coalesce(OUT_FILE_FS, IN_FILE))

# write new file

write.csv(cmtotal, "cm_2025_total.csv")

# fix filenames in cm_2025.csv----------------------------

str(cm) 
colSums(is.na(cm))
cm <- cm %>% 
  mutate(filename = coalesce(filename, IN_FILE))

write.csv(cm, "cm_2025.csv")


# working with cm

cm <- read.csv("cm_2025.csv")
cm$DATE <- as.Date(cm$DATE)
cm$DATE_12 <- as.Date(cm$DATE_12)

# date and time as chr

cm <- cm %>%
  mutate(
    dt_str = paste(DATE, TIME)
  )

# make column for wrong daylight saving times

cm <- cm %>%
  mutate(
    dst_gap = DATE == "2025-03-30" &
      TIME >= "02:00:00" &
      TIME <  "03:00:00"
  )


# fix gap 

cm <- cm %>%
  mutate(
    dt_fixed = if_else(
      dst_gap,
      paste(DATE, sprintf("%02d:%s",
                          as.integer(substr(TIME, 1, 2)) + 1,
                          substr(TIME, 4, 5))),
      paste(DATE, TIME)
    )
  )

# fix dst to real datetime, now all the sites with wrong start with 03:00-

cm <- cm %>%
  mutate(
    dt_str_fixed = if_else(
      dst_gap,
      paste(
        DATE,
        sprintf(
          "%02d:%s",
          as.integer(substr(TIME, 1, 2)) + 1,
          substr(TIME, 4, 8)
        )
      ),
      dt_str
    )
  )

# make datetimes column for all the dates 

cm <- cm %>%
  mutate(
    datetime_tz = ymd_hms(dt_str_fixed, tz = "Europe/Oslo")
  )

# add an hour to all days detectors were out after dst before maintenance

group1 <- c("CM-06", "CM-17", "CM-20", "CM-18", "CM-04")
group2 <- c("CM-03", "CM-22", "CM-27", "CM-35", "CM-25", "CM-51", "CM-28", "CM-52", "CM-56", "CM-05")
group3 <- c("CM-26", "CM-49")

end1 <- as.POSIXct("2025-04-22 16:15:00", tz = "Europe/Oslo")
end2 <- as.POSIXct("2025-04-25 17:57:00", tz = "Europe/Oslo")
end3 <- as.POSIXct("2025-04-26 11:19:00", tz = "Europe/Oslo")

cm <- cm %>%
  mutate(
    datetime = case_when(
      
      Site %in% group1 &
        datetime_tz >= as.POSIXct("2025-03-30 03:00:00", tz = "Europe/Oslo") &
        datetime_tz <= end1 &
        !dst_gap ~ datetime_tz + hours(1),
      
      Site %in% group2 &
        datetime_tz >= as.POSIXct("2025-03-30 03:00:00", tz = "Europe/Oslo") &
        datetime_tz <= end2 &
        !dst_gap ~ datetime_tz + hours(1),
      
      Site %in% group3 &
        datetime_tz >= as.POSIXct("2025-03-30 03:00:00", tz = "Europe/Oslo") &
        datetime_tz <= end3 &
        !dst_gap ~ datetime_tz + hours(1),
      
      TRUE ~ datetime_tz
    )
  )

# make datetime 12 column

cm <- cm %>%
  mutate(
    datetime_12 = datetime - hours(12)
  )


# clean 2025 data and upload it again, remove extra columns. 

cm[1]<- NULL

cm_new <- subset(cm, select = -c(dt_str, dst_gap, dt_fixed, dt_str_fixed, datetime_tz))
write.csv(cm_new, "cm_2025.csv")


# working with data

cm <- read.csv("cm_2025.csv")
