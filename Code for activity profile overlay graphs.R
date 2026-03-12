### Making overlaying activity profile graphs ### 
### Made by Lianna Soriano and CHATGPT #### 


#=================================================================#
## CGM Glucose ##
#=================================================================#

### For individual participants ------------------------------------
library(tidyverse)

# Load files
setwd("C:/Users/liann/Downloads/HL2/Activity profiles (ind & all)/CGM activity profiles/cgm activity profiles csvs")
BL  <- read.csv("636 BL CGM glucose_Activity Profile_Table Data.csv")
INT <- read.csv("636 INT cgm glucose_Activity Profile_Table Data.csv")

# participant ID - change
participant_id <- "636"

# Add group labels
BL$Group <- "BL"
INT$Group <- "INT"

# Combine
combined <- bind_rows(BL, INT)

# Clean and format
combined <- combined %>%
  rename(Phase = `Angle..deg.`) %>%      # rename column
  mutate(
    Phase = as.numeric(Phase),
    Amplitude = as.numeric(Amplitude),
    SEM = as.numeric(SEM),
    Time_h = Phase / 15                  # convert degrees to 0–24h
  )

#calculate sunrise and sunset 
library(suncalc)

# sun BL 
sun <- getSunlightTimes(
  date = as.Date("2024-12-30"),
  lat = 37.87,
  lon = -122.27,
  keep = c("sunrise","sunset"),
  tz = "America/Los_Angeles"
)

sunriseBL <- as.numeric(format(sun$sunrise, "%H")) +
  as.numeric(format(sun$sunrise, "%M"))/60

sunsetBL <- as.numeric(format(sun$sunset, "%H")) +
  as.numeric(format(sun$sunset, "%M"))/60

# sun INT
sunINT <- getSunlightTimes(
  date = as.Date("2025-03-30"),
  lat = 37.87,
  lon = -122.27,
  keep = c("sunrise","sunset"),
  tz = "America/Los_Angeles"
)

sunriseINT <- as.numeric(format(sunINT$sunrise, "%H")) +
  as.numeric(format(sunINT$sunrise, "%M"))/60

sunsetINT <- as.numeric(format(sunINT$sunset, "%H")) +
  as.numeric(format(sunINT$sunset, "%M"))/60


# Plot with SEM ribbon
ggplot(combined, aes(x = Time_h, y = Amplitude, color = Group, fill = Group)) +
  geom_line(linewidth = 1.2) +
  geom_ribbon(aes(ymin = Amplitude - SEM,
                  ymax = Amplitude + SEM),
              alpha = 0.2,
              color = NA) +
  scale_x_continuous(
    limits = c(0, 24),
    breaks = seq(0, 24, 4)
  ) +
  labs(
    title = paste(participant_id, "CGM Activity Profile"),
    x = "Circadian Time (h)",
    y = "Blood Glucose Amplitude",
    color = "Group",
    fill = "Group"
  ) +
  theme_classic(base_size = 14) +
  annotate("text", x = sunriseBL,  y = 14, label = "SR BL",  size = 4) +
  annotate("text", x = sunriseINT, y = 13.5, label = "SR INT", size = 4) +
  annotate("text", x = sunsetBL,   y = 14, label = "SS BL",  size = 4) +
  annotate("text", x = sunsetINT,  y = 13.5, label = "SS INT", size = 4) +
  geom_vline(xintercept = sunriseBL, linetype = "dashed", color = "orange") +
  geom_vline(xintercept = sunsetBL, linetype = "dashed", color = "blue") +
  geom_vline(xintercept = sunriseINT, linetype = "dashed", color = "darkorange") +
  geom_vline(xintercept = sunsetINT, linetype = "dashed", color = "darkblue") +
  
  scale_x_continuous(
    limits = c(0, 24),
    breaks = c(seq(0, 24, 4)),
    labels = function(x) round(x)
  ) 


#----------------------------------------------------------------------------
### CGM Activity Profile – Group Average --------------------------------


library(tidyverse)
library(suncalc)

# Set folder
setwd("C:/Users/liann/Downloads/HL2/cgm activity profiles csvs")

# Load all CSV files
files <- list.files(pattern = "*.csv", full.names = TRUE)

data_list <- lapply(files, function(file){
  
  df <- read.csv(file)
  
  # Extract participant ID
  participant <- stringr::str_extract(basename(file), "^[0-9]+")
  
  # Determine BL vs INT
  group <- ifelse(str_detect(file, "BL"), "BL", "INT")
  
  df$Participant <- participant
  df$Group <- group
  
  return(df)
})

combined <- bind_rows(data_list)

# Clean data
combined <- combined %>%
  rename(Phase = `Angle..deg.`) %>%
  mutate(
    Phase = as.numeric(Phase),
    Amplitude = as.numeric(Amplitude),
    Time_h = Phase / 15
  )

# ---- CALCULATE GROUP MEAN + SEM ----
summary_data <- combined %>%
  group_by(Group, Time_h) %>%
  summarise(
    mean_amp = mean(Amplitude, na.rm = TRUE),
    sem = sd(Amplitude, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  )

# ---- PLOT ----

ggplot(summary_data,
       aes(x = Time_h, y = mean_amp, color = Group, fill = Group)) +
  
  geom_line(linewidth = 1.5) +
  
  geom_ribbon(
    aes(ymin = mean_amp - sem,
        ymax = mean_amp + sem),
    alpha = 0.25,
    color = NA
  ) +
  
  scale_x_continuous(
    limits = c(0,24),
    breaks = seq(0,24,4)
  ) +
  
  labs(
    title = "CGM Activity Profile (Average ± SEM of All Participants)",
    x = "Circadian Time (h)",
    y = "Blood Glucose Amplitude",
    color = "Condition",
    fill = "Condition"
  ) +
  
  theme_classic(base_size = 14)


#=================================================================#
## GENEActiv Movement ##
#=================================================================#

### For individual participants ------------------------------------
library(tidyverse)

# Load files
setwd("C:/Users/liann/Downloads/HL2/Activity profiles (ind & all)/GENE movement activity profiles/GENE movement activity profiles csvs")
BL  <- read.csv("636 BL CGM glucose_Activity Profile_Table Data.csv")
INT <- read.csv("636 INT cgm glucose_Activity Profile_Table Data.csv")

# participant ID - change
participant_id <- "636"

# Add group labels
BL$Group <- "BL"
INT$Group <- "INT"

# Combine
combined <- bind_rows(BL, INT)

# Clean and format
combined <- combined %>%
  rename(Phase = `Angle..deg.`) %>%      # rename column
  mutate(
    Phase = as.numeric(Phase),
    Amplitude = as.numeric(Amplitude),
    SEM = as.numeric(SEM),
    Time_h = Phase / 15                  # convert degrees to 0–24h
  )

#calculate sunrise and sunset 
library(suncalc)

# sun BL 
sun <- getSunlightTimes(
  date = as.Date("2024-12-30"),
  lat = 37.87,
  lon = -122.27,
  keep = c("sunrise","sunset"),
  tz = "America/Los_Angeles"
)

sunriseBL <- as.numeric(format(sun$sunrise, "%H")) +
  as.numeric(format(sun$sunrise, "%M"))/60

sunsetBL <- as.numeric(format(sun$sunset, "%H")) +
  as.numeric(format(sun$sunset, "%M"))/60

# sun INT
sunINT <- getSunlightTimes(
  date = as.Date("2025-03-30"),
  lat = 37.87,
  lon = -122.27,
  keep = c("sunrise","sunset"),
  tz = "America/Los_Angeles"
)

sunriseINT <- as.numeric(format(sunINT$sunrise, "%H")) +
  as.numeric(format(sunINT$sunrise, "%M"))/60

sunsetINT <- as.numeric(format(sunINT$sunset, "%H")) +
  as.numeric(format(sunINT$sunset, "%M"))/60


# Plot with SEM ribbon
ggplot(combined, aes(x = Time_h, y = Amplitude, color = Group, fill = Group)) +
  geom_line(linewidth = 1.2) +
  geom_ribbon(aes(ymin = Amplitude - SEM,
                  ymax = Amplitude + SEM),
              alpha = 0.2,
              color = NA) +
  scale_x_continuous(
    limits = c(0, 24),
    breaks = seq(0, 24, 4)
  ) +
  labs(
    title = paste(participant_id, "GENEActiv Movement Activity Profile"),
    x = "Circadian Time (h)",
    y = "Blood Glucose Amplitude",
    color = "Group",
    fill = "Group"
  ) +
  theme_classic(base_size = 14) +
  annotate("text", x = sunriseBL,  y = 14, label = "SR BL",  size = 4) +
  annotate("text", x = sunriseINT, y = 13.5, label = "SR INT", size = 4) +
  annotate("text", x = sunsetBL,   y = 14, label = "SS BL",  size = 4) +
  annotate("text", x = sunsetINT,  y = 13.5, label = "SS INT", size = 4) +
  geom_vline(xintercept = sunriseBL, linetype = "dashed", color = "orange") +
  geom_vline(xintercept = sunsetBL, linetype = "dashed", color = "blue") +
  geom_vline(xintercept = sunriseINT, linetype = "dashed", color = "darkorange") +
  geom_vline(xintercept = sunsetINT, linetype = "dashed", color = "darkblue") +
  
  scale_x_continuous(
    limits = c(0, 24),
    breaks = c(seq(0, 24, 4)),
    labels = function(x) round(x)
  ) 


#----------------------------------------------------------------------------
### GENEActiv movement Activity Profile – Group Average --------------------------------


library(tidyverse)
library(suncalc)

# Set folder
setwd("C:/Users/liann/Downloads/HL2/Activity profiles (ind & all)/GENE movement activity profiles/GENE movement activity profiles csvs")

# Load all CSV files
files <- list.files(pattern = "*.csv", full.names = TRUE)

data_list <- lapply(files, function(file){
  
  df <- read.csv(file)
  
  # Extract participant ID
  participant <- stringr::str_extract(basename(file), "^[0-9]+")
  
  # Determine BL vs INT
  group <- ifelse(str_detect(file, "BL"), "BL", "INT")
  
  df$Participant <- participant
  df$Group <- group
  
  return(df)
})

combined <- bind_rows(data_list)

# Clean data
combined <- combined %>%
  rename(Phase = `Angle..deg.`) %>%
  mutate(
    Phase = as.numeric(Phase),
    Amplitude = as.numeric(Amplitude),
    Time_h = Phase / 15
  )

# ---- CALCULATE GROUP MEAN + SEM ----
summary_data <- combined %>%
  group_by(Group, Time_h) %>%
  summarise(
    mean_amp = mean(Amplitude, na.rm = TRUE),
    sem = sd(Amplitude, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  )

# ---- PLOT ----

ggplot(summary_data,
       aes(x = Time_h, y = mean_amp, color = Group, fill = Group)) +
  
  geom_line(linewidth = 1.5) +
  
  geom_ribbon(
    aes(ymin = mean_amp - sem,
        ymax = mean_amp + sem),
    alpha = 0.25,
    color = NA
  ) +
  
  scale_x_continuous(
    limits = c(0,24),
    breaks = seq(0,24,4)
  ) +
  
  labs(
    title = "GENEActiv Movement Activity Profile (Average ± SEM of All Participants)",
    x = "Circadian Time (h)",
    y = "Blood Glucose Amplitude",
    color = "Condition",
    fill = "Condition"
  ) +
  
  theme_classic(base_size = 14)

#=================================================================#
## GENEActiv Temperature ##
#=================================================================#

### For individual participants ------------------------------------
library(tidyverse)

# Load files
setwd("C:/Users/liann/Downloads/HL2/Activity profiles (ind & all)/GENE temperature activity profiles/GENE temperature activity profiles csvs")
BL  <- read.csv("636 BL CGM glucose_Activity Profile_Table Data.csv")
INT <- read.csv("636 INT cgm glucose_Activity Profile_Table Data.csv")

# participant ID - change
participant_id <- "636"

# Add group labels
BL$Group <- "BL"
INT$Group <- "INT"

# Combine
combined <- bind_rows(BL, INT)

# Clean and format
combined <- combined %>%
  rename(Phase = `Angle..deg.`) %>%      # rename column
  mutate(
    Phase = as.numeric(Phase),
    Amplitude = as.numeric(Amplitude),
    SEM = as.numeric(SEM),
    Time_h = Phase / 15                  # convert degrees to 0–24h
  )

#calculate sunrise and sunset 
library(suncalc)

# sun BL 
sun <- getSunlightTimes(
  date = as.Date("2024-12-30"),
  lat = 37.87,
  lon = -122.27,
  keep = c("sunrise","sunset"),
  tz = "America/Los_Angeles"
)

sunriseBL <- as.numeric(format(sun$sunrise, "%H")) +
  as.numeric(format(sun$sunrise, "%M"))/60

sunsetBL <- as.numeric(format(sun$sunset, "%H")) +
  as.numeric(format(sun$sunset, "%M"))/60

# sun INT
sunINT <- getSunlightTimes(
  date = as.Date("2025-03-30"),
  lat = 37.87,
  lon = -122.27,
  keep = c("sunrise","sunset"),
  tz = "America/Los_Angeles"
)

sunriseINT <- as.numeric(format(sunINT$sunrise, "%H")) +
  as.numeric(format(sunINT$sunrise, "%M"))/60

sunsetINT <- as.numeric(format(sunINT$sunset, "%H")) +
  as.numeric(format(sunINT$sunset, "%M"))/60


# Plot with SEM ribbon
ggplot(combined, aes(x = Time_h, y = Amplitude, color = Group, fill = Group)) +
  geom_line(linewidth = 1.2) +
  geom_ribbon(aes(ymin = Amplitude - SEM,
                  ymax = Amplitude + SEM),
              alpha = 0.2,
              color = NA) +
  scale_x_continuous(
    limits = c(0, 24),
    breaks = seq(0, 24, 4)
  ) +
  labs(
    title = paste(participant_id, "GENEActiv Temperature Activity Profile"),
    x = "Circadian Time (h)",
    y = "Blood Glucose Amplitude",
    color = "Group",
    fill = "Group"
  ) +
  theme_classic(base_size = 14) +
  annotate("text", x = sunriseBL,  y = 14, label = "SR BL",  size = 4) +
  annotate("text", x = sunriseINT, y = 13.5, label = "SR INT", size = 4) +
  annotate("text", x = sunsetBL,   y = 14, label = "SS BL",  size = 4) +
  annotate("text", x = sunsetINT,  y = 13.5, label = "SS INT", size = 4) +
  geom_vline(xintercept = sunriseBL, linetype = "dashed", color = "orange") +
  geom_vline(xintercept = sunsetBL, linetype = "dashed", color = "blue") +
  geom_vline(xintercept = sunriseINT, linetype = "dashed", color = "darkorange") +
  geom_vline(xintercept = sunsetINT, linetype = "dashed", color = "darkblue") +
  
  scale_x_continuous(
    limits = c(0, 24),
    breaks = c(seq(0, 24, 4)),
    labels = function(x) round(x)
  ) 


#----------------------------------------------------------------------------
### GENEActiv temperature Activity Profile – Group Average --------------------------------


library(tidyverse)
library(suncalc)

# Set folder
setwd("C:/Users/liann/Downloads/HL2/Activity profiles (ind & all)/GENE temperature activity profiles/GENE temperature activity profiles csvs")

# Load all CSV files
files <- list.files(pattern = "*.csv", full.names = TRUE)

data_list <- lapply(files, function(file){
  
  df <- read.csv(file)
  
  # Extract participant ID
  participant <- stringr::str_extract(basename(file), "^[0-9]+")
  
  # Determine BL vs INT
  group <- ifelse(str_detect(file, "BL"), "BL", "INT")
  
  df$Participant <- participant
  df$Group <- group
  
  return(df)
})

combined <- bind_rows(data_list)

# Clean data
combined <- combined %>%
  rename(Phase = `Angle..deg.`) %>%
  mutate(
    Phase = as.numeric(Phase),
    Amplitude = as.numeric(Amplitude),
    Time_h = Phase / 15
  )

# ---- CALCULATE GROUP MEAN + SEM ----
summary_data <- combined %>%
  group_by(Group, Time_h) %>%
  summarise(
    mean_amp = mean(Amplitude, na.rm = TRUE),
    sem = sd(Amplitude, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  )

# ---- PLOT ----

ggplot(summary_data,
       aes(x = Time_h, y = mean_amp, color = Group, fill = Group)) +
  
  geom_line(linewidth = 1.5) +
  
  geom_ribbon(
    aes(ymin = mean_amp - sem,
        ymax = mean_amp + sem),
    alpha = 0.25,
    color = NA
  ) +
  
  scale_x_continuous(
    limits = c(0,24),
    breaks = seq(0,24,4)
  ) +
  
  labs(
    title = "GENEActiv Temperature Activity Profile (Average ± SEM of All Participants)",
    x = "Circadian Time (h)",
    y = "Blood Glucose Amplitude",
    color = "Condition",
    fill = "Condition"
  ) +
  
  theme_classic(base_size = 14)


