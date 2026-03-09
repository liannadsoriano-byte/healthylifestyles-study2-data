## This code includes code for graphing Activity Profile summaries of 
# all participants from BL to INT including both IS and IV graph 

# Includes code for graphing Periodogram summaries 

## Made by Lianna Soriano and CHATGPT 


###### Graph for Activity Profile summaries of all participants BL to INT  #######

library(tidyverse)

# Read data
data <- read_csv("C:/Users/liann/Downloads/HL2 Summary Sheets/light&move_Activity Profile_Table Data.csv")

# Clean and rename columns
data <- data %>%
  rename(
    ID = Filename,
    Timepoint = `...2`
  ) %>%
  mutate(
    Timepoint = str_trim(Timepoint),
    Timepoint = str_to_upper(Timepoint),
    Timepoint = factor(Timepoint, levels = c("BL", "INT"))
  )


##### Activity Profile IS graph ######
library(ggrepel)

ggplot(data, aes(x = Timepoint, y = IS, group = ID, color = factor(ID))) +
  
  # Individual lines + points
  geom_line(alpha = 0.7) +
  geom_point(size = 3) +
  
  
  # Group mean line
  stat_summary(aes(group = 1),
               fun = mean,
               geom = "line",
               size = 1.2,
               color = "black") +
  
  # Group mean points
  stat_summary(aes(group = 1),
               fun = mean,
               geom = "point",
               size = 3,
               color = "black") +
  
  # Label the group mean values
  stat_summary(aes(group = 1,
                   label = round(after_stat(y), 3)),
               fun = mean,
               geom = "text",
               color = "black",
               size = 4,
               vjust = -1) +
  
  theme_classic() +
  labs(
    title = "BL vs INT – IS",
    x = "Timepoint",
    y = "Interdaily Stability (IS)",
    color = "Participant ID"
  )


##### Activity Profile IV graph ######
library(ggrepel)

ggplot(data, aes(x = Timepoint, y = IV, group = ID, color = factor(ID))) +
  
  # Individual lines + points
  geom_line(alpha = 0.7) +
  geom_point(size = 3) +
  
  
  # Group mean line
  stat_summary(aes(group = 1),
               fun = mean,
               geom = "line",
               size = 1.2,
               color = "black") +
  
  # Group mean points
  stat_summary(aes(group = 1),
               fun = mean,
               geom = "point",
               size = 3,
               color = "black") +
  
  # Label the group mean values
  stat_summary(aes(group = 1,
                   label = round(after_stat(y), 3)),
               fun = mean,
               geom = "text",
               color = "black",
               size = 4,
               vjust = -1) +
  
  theme_classic() +
  labs(
    title = "BL vs INT – IV",
    x = "Timepoint",
    y = "Intradaily variability (IV)",
    color = "Participant ID"
  )




###### Graph for Periodogram summaries of all participants BL to INT  #######

library(tidyverse)
library(ggrepel)

data <- read_csv("C:/Users/liann/Downloads/HL2 Summary Sheets/gene move_Periodogram_Table summary.csv")

# Clean columns
data <- data %>%
  rename(
    ID = Filename,
    Timepoint = `...2`
  ) %>%
  mutate(
    Timepoint = str_trim(Timepoint),
    Timepoint = str_to_upper(Timepoint),
    Timepoint = factor(Timepoint, levels = c("BL", "INT"))
  )

ggplot(data, aes(x = Timepoint, 
                 y = `Amplitude 1`, 
                 group = ID, 
                 color = factor(ID))) +
  
  geom_line(alpha = 0.7) +
  geom_point(size = 3) +
  
  # Group mean line
  stat_summary(aes(group = 1),
               fun = mean,
               geom = "line",
               size = 1.2,
               color = "black") +
  
  stat_summary(aes(group = 1),
               fun = mean,
               geom = "point",
               size = 4,
               color = "black") +
  
  stat_summary(aes(group = 1,
                   label = round(after_stat(y), 0)),
               fun = mean,
               geom = "text",
               color = "black",
               size = 4,
               vjust = -1) +
  
  theme_classic() +
  labs(
    title = "24-Hour Periodogram Amplitude (BL vs INT)",
    x = "Timepoint",
    y = "Amplitude (24h)",
    color = "Participant ID"
  )

