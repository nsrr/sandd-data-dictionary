ver="0.1.0"

library(dplyr)
library(readxl)
library(foreign)

setwd("/Volumes/BWH-SLEEPEPI-NSRR-STAGING/20240318-carskadon-sandd/original")

sav_files <- list.files(pattern = "\\.sav$", full.names = TRUE)

data_frames <- lapply(sav_files, function(file) {
  read.spss(file, to.data.frame = TRUE, use.value.labels = F)
})

names(data_frames) <- tools::file_path_sans_ext(basename(sav_files))

#rename variables
data_frames[["S&D S1 BDI_Scored_DeID 3.20.24"]] <- data_frames[["S&D S1 BDI_Scored_DeID 3.20.24"]] %>%
  rename(bdi_score2 = bdi_score.)
data_frames[["S&D S1 Dems_Scores_dlmo_ETOHhx_season_pds_DeID 8.22.24"]] <- data_frames[["S&D S1 Dems_Scores_dlmo_ETOHhx_season_pds_DeID 8.22.24"]] %>%
  rename(SHS_PDS_score2 = SHS_PDS_score.)
data_frames[["S&D S1 Dems_Scores_dlmo_ETOHhx_season_pds_DeID 8.22.24"]] <- data_frames[["S&D S1 Dems_Scores_dlmo_ETOHhx_season_pds_DeID 8.22.24"]] %>%
  rename(SMITH_TOT2 = SMITH_TOT.)
data_frames[["S&D S1 Dems_Scores_dlmo_ETOHhx_season_pds_DeID 8.22.24"]] <- data_frames[["S&D S1 Dems_Scores_dlmo_ETOHhx_season_pds_DeID 8.22.24"]] %>%
  rename(SHS_Pubertal_Category2 = SHS_Pubertal_Category.)
data_frames[["S&D S1 Mood_sleepiness_DeID 3.21.24"]] <- data_frames[["S&D S1 Mood_sleepiness_DeID 3.21.24"]] %>%
  rename(Mood_Year_Study = Year_Study)
data_frames[["S&D S1 Mood_sleepiness_DeID 3.21.24"]] <- data_frames[["S&D S1 Mood_sleepiness_DeID 3.21.24"]] %>%
  rename(mood_day_sequence = day_sequence)
data_frames[["S&D S1 Mood_sleepiness_DeID 3.21.24"]] <- data_frames[["S&D S1 Mood_sleepiness_DeID 3.21.24"]] %>%
  rename(study_cond = cond)
data_frames[["S&D S1 ScoredActigraphy_withSessionSeason_DeID 3.19.24"]] <- data_frames[["S&D S1 ScoredActigraphy_withSessionSeason_DeID 3.19.24"]] %>%
  rename(Actigraphy_Year_Study = Year_Study)
data_frames[["S&D S1 ScoredActigraphy_withSessionSeason_DeID 3.19.24"]] <- data_frames[["S&D S1 ScoredActigraphy_withSessionSeason_DeID 3.19.24"]] %>%
  rename(actigraphy_day_sequence = day_sequence)
data_frames[["S&D S1 CESD_Scored_DeID 6.25.24"]] <- data_frames[["S&D S1 CESD_Scored_DeID 6.25.24"]] %>%
  rename(cesd_TOT2 = cesd_TOT.)

if ("S&D S1 Dems_Scores_dlmo_ETOHhx_season_pds_DeID 8.22.24" %in% names(data_frames)) {
  data_frames[["S&D S1 Dems_Scores_dlmo_ETOHhx_season_pds_DeID 8.22.24"]] <- data_frames[["S&D S1 Dems_Scores_dlmo_ETOHhx_season_pds_DeID 8.22.24"]]
  data_frames[["S&D S1 Dems_Scores_dlmo_ETOHhx_season_pds_DeID 8.22.24"]]$Session <- ifelse(data_frames[["S&D S1 Dems_Scores_dlmo_ETOHhx_season_pds_DeID 8.22.24"]]$Session == "Initial/Baseline", "1",
                       ifelse(data_frames[["S&D S1 Dems_Scores_dlmo_ETOHhx_season_pds_DeID 8.22.24"]]$Session == "6 mo [.5]", "2",
                              ifelse(data_frames[["S&D S1 Dems_Scores_dlmo_ETOHhx_season_pds_DeID 8.22.24"]]$Session == "12 mo [1 yr]", "3",
                                     ifelse(data_frames[["S&D S1 Dems_Scores_dlmo_ETOHhx_season_pds_DeID 8.22.24"]]$Session == "18 mo [1.5 yr]", "4",
                                            ifelse(data_frames[["S&D S1 Dems_Scores_dlmo_ETOHhx_season_pds_DeID 8.22.24"]]$Session == "24 mo [2 yr]", "5",
                                                   ifelse(data_frames[["S&D S1 Dems_Scores_dlmo_ETOHhx_season_pds_DeID 8.22.24"]]$Session == "30 mo [2.5 yr]", "6", data_frames[["S&D S1 Dems_Scores_dlmo_ETOHhx_season_pds_DeID 8.22.24"]]$Session)
                                            )
                                     )
                              )
                       )
  )
}

merged_data <- data_frames[[1]]
datasets_to_exclude <- c("S&D S1 Mood_sleepiness_DeID 3.21.24",
                         "S&D S1 ScoredActigraphy_withSessionSeason_DeID 3.19.24")

for (i in 2:length(data_frames)) {
  dataset_name <- names(data_frames)[i]
  
  if (!(dataset_name %in% datasets_to_exclude)) {
    merged_data <- merge(merged_data, data_frames[[i]], by = c("ID", "Session"), all = TRUE)
  }
}

#merged_df2 <- merged_data[!duplicated(merged_data), ]
# check which variables in merged_data is not in sandd-data-dictionary
df <- read_excel("/Users/isabellaliu/Desktop/BWH/sandd/sandd-data-dictionary.xlsx")
missing_cols <- setdiff(names(merged_data), df$id)
exclude_cols <- c("druguse","bdi_score2", "cesd_TOT2", "SHS_PDS_score2", "SHS_Pubertal_Category2", "SMITH_TOT2")
missing_cols <- setdiff(missing_cols, exclude_cols)
merged_data2<- merged_data[, !(names(merged_data) %in% missing_cols)]

# convert time type to hh:mm
variables_to_convert <- c("fastart", "fasend", "strpstrt", "strpend", "trails_strt", "trails_end", "Rey_copy_start", "Rey_copy_time", "Rey_IR_end", "Rey_IR_time")
for (variable in variables_to_convert) {
  merged_data2[[variable]] <- format(as.POSIXct(paste(floor(merged_data2[[variable]] / 3600), floor((merged_data2[[variable]] %% 3600) / 60), merged_data2[[variable]] %% 60, sep=":"), format="%H:%M"), "%H:%M")
}

actigraphy_data <- data_frames[["S&D S1 ScoredActigraphy_withSessionSeason_DeID 3.19.24"]]
variables_to_convert <- c("stime", "etime", "mid")
for (variable in variables_to_convert) {
  actigraphy_data[[variable]] <- format(as.POSIXct(paste(floor(actigraphy_data[[variable]] / 3600), floor((actigraphy_data[[variable]] %% 3600) / 60), actigraphy_data[[variable]] %% 60, sep=":"), format="%H:%M"), "%H:%M")
}

mood_sleepy_data <- data_frames[["S&D S1 Mood_sleepiness_DeID 3.21.24"]]
variables_to_convert <- c("schtim", "actime")
for (variable in variables_to_convert) {
  mood_sleepy_data[[variable]] <- format(as.POSIXct(paste(floor(mood_sleepy_data[[variable]] / 3600), floor((mood_sleepy_data[[variable]] %% 3600) / 60), mood_sleepy_data[[variable]] %% 60, sep=":"), format="%H:%M"), "%H:%M")
}

names(merged_data2) <- tolower(names(merged_data2))
names(actigraphy_data) <- tolower(names(actigraphy_data))
names(mood_sleepy_data) <- tolower(names(mood_sleepy_data))

# remove variables
merged_data2 <- subset(merged_data2, select = -c(study_name, rey_tester))
actigraphy_data <- subset(actigraphy_data, select = -c(actmode))

write.csv(merged_data2, file = "/Volumes/BWH-SLEEPEPI-NSRR-STAGING/20240318-carskadon-sandd/nsrr-prep/_releases/sandd-dataset-0.1.0.pre.csv", row.names = FALSE, na='')
write.csv(mood_sleepy_data,file = "/Volumes/BWH-SLEEPEPI-NSRR-STAGING/20240318-carskadon-sandd/nsrr-prep/_releases/sandd-mood-0.1.0.pre.csv", row.names = FALSE, na='')
write.csv(actigraphy_data,file = "/Volumes/BWH-SLEEPEPI-NSRR-STAGING/20240318-carskadon-sandd/nsrr-prep/_releases/sandd-ScoredActigraphy-0.1.0.pre.csv", row.names = FALSE, na='')


#harmonized dataset
# age, race, gender, ethnicity_hispanicorlatino
harmonized_data<-merged_data2[,c("id", "session","agedec_stdate","race","female_yesno","ethnicity_hispanicorlatino")]%>%
  dplyr::mutate(nsrrid=id,
                nsrr_age=agedec_stdate,
                nsrr_race=dplyr::case_when(
                  race==1 ~ "white",
                  race==2 ~ "black or african american",
                  race==3 ~ "hispanic",
                  race==4 ~ "asian",
                  race==5 ~ "american indian or alaska native",
                  race==6 ~ "multiple",
                  race==7 ~ "other",
                  TRUE ~ "not reported"
                ),
                nsrr_sex=dplyr::case_when(
                  female_yesno==0 ~ "male",
                  female_yesno==1 ~ "female",
                  TRUE ~ "not reported"
                ),
                nsrr_ethnicity=dplyr::case_when(
                  ethnicity_hispanicorlatino==1 ~ "hispanic or latino",
                  ethnicity_hispanicorlatino==0 ~ "not hispanic or latino",
                  TRUE ~ "not reported"
                ))%>%select(nsrrid, session, nsrr_age, nsrr_race, nsrr_sex, nsrr_ethnicity)
write.csv(harmonized_data,file = "/Volumes/BWH-SLEEPEPI-NSRR-STAGING/20240318-carskadon-sandd/nsrr-prep/_releases/sandd-harmonized-dataset-0.1.0.csv", row.names = FALSE, na='')
