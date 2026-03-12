## This code includes code for graphing Activity Profile summaries of 
# all participants from BL to INT including both IS and IV graph 

# Includes code for graphing Periodogram summaries 

## Includes code for GENEActiv Movement, Temperature, and CGM glucose graphs 

## Made by Lianna Soriano and CHATGPT 

#--------------------------------------------------------------------------------------
###### Graph for Activity Profile summaries of all participants BL to INT  #######

#### This is for GENEActiv Movement
library(tidyverse)

# Read data # MUST change this!
setwd("C:/Users/liann/Downloads/HL2/HL2 Summary Sheets/data tables")
data <- read_csv("3.11.26.gene move activity prof.csv")

# Clean and rename columns
data <- data %>%
  rename(
    ID = Filename,
    Timepoint = `...2`
  ) %>%
  mutate(
    Timepoint = str_trim(Timepoint),
    Timepoint = str_to_upper(Timepoint),
    Timepoint = recode(Timepoint,
                       "BL" = "Baseline",
                       "INT" = "Intervention"),
    Timepoint = factor(Timepoint, levels = c("Baseline", "Intervention"))
  )

#-------------------------------------
##### Activity Profile IS graph ######
library(ggrepel)

ggplot(data, aes(x = Timepoint, y = IS, group = ID, color = factor(ID))) +
  
  # Individual lines + points
  geom_line(alpha = 0.7) +
  geom_point(size = 3) +
  
  #SEM Error bars 
  stat_summary(aes(group = 1),
               fun.data = mean_se,
               geom = "errorbar",
               width = 0.1,
               color = "black",
               size = 0.5) +
  
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
    title = "GENEActiv Movement BL vs INT – IS",
    x = "Timepoint",
    y = "Interdaily Stability (IS)",
    color = "Participant ID"
  )

#-------------------------------------
##### Activity Profile IV graph ######
library(ggrepel)

ggplot(data, aes(x = Timepoint, y = IV, group = ID, color = factor(ID))) +
  
  # Individual lines + points
  geom_line(alpha = 0.7) +
  geom_point(size = 3) +
  
  #SEM Error bars 
  stat_summary(aes(group = 1),
               fun.data = mean_se,
               geom = "errorbar",
               width = 0.1,
               color = "black",
               size = 0.5) +
  
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
    title = "GENEActiv Movement BL vs INT – IV",
    x = "Timepoint",
    y = "Intradaily variability (IV)",
    color = "Participant ID"
  )

#----------------------------------------------------------------------------------
#### This is for GENEActiv Temperature ################
library(tidyverse)

# Read data # MUST change this! 
setwd("C:/Users/liann/Downloads/HL2/HL2 Summary Sheets/data tables")
data <- read_csv("3.11.26.gene temp activity prof.csv")

# Clean and rename columns
data <- data %>%
  rename(
    ID = Filename,
    Timepoint = `...2`
  ) %>%
  mutate(
    Timepoint = str_trim(Timepoint),
    Timepoint = str_to_upper(Timepoint),
    Timepoint = recode(Timepoint,
                       "BL" = "Baseline",
                       "INT" = "Intervention"),
    Timepoint = factor(Timepoint, levels = c("Baseline", "Intervention"))
  )

#-------------------------------------
##### Activity Profile IS graph ######
library(ggrepel)

ggplot(data, aes(x = Timepoint, y = IS, group = ID, color = factor(ID))) +
  
  # Individual lines + points
  geom_line(alpha = 0.7) +
  geom_point(size = 3) +
  
  #SEM Error bars 
  stat_summary(aes(group = 1),
               fun.data = mean_se,
               geom = "errorbar",
               width = 0.1,
               color = "black",
               size = 0.5) +
  
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
    title = "GENEActiv Temperature BL vs INT – IS",
    x = "Timepoint",
    y = "Interdaily Stability (IS)",
    color = "Participant ID"
  )

#-------------------------------------
##### Activity Profile IV graph ######
library(ggrepel)

ggplot(data, aes(x = Timepoint, y = IV, group = ID, color = factor(ID))) +
  
  # Individual lines + points
  geom_line(alpha = 0.7) +
  geom_point(size = 3) +
  
  #SEM Error bars 
  stat_summary(aes(group = 1),
               fun.data = mean_se,
               geom = "errorbar",
               width = 0.1,
               color = "black",
               size = 0.5) +
  
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
    title = "GENEActiv Temperature BL vs INT – IV",
    x = "Timepoint",
    y = "Intradaily variability (IV)",
    color = "Participant ID"
  )

#-------------------------------------------------------------------------------------
#### This is for CGM Glucose 
library(tidyverse)

# Read data # MUST change this!
setwd("C:/Users/liann/Downloads/HL2/HL2 Summary Sheets/data tables")
data <- read_csv("3.11.26.cgm activity prof.csv")

# Clean and rename columns
data <- data %>%
  rename(
    ID = Filename,
    Timepoint = `...2`
  ) %>%
  mutate(
    Timepoint = str_trim(Timepoint),
    Timepoint = str_to_upper(Timepoint),
    Timepoint = recode(Timepoint,
                       "BL" = "Baseline",
                       "INT" = "Intervention"),
    Timepoint = factor(Timepoint, levels = c("Baseline", "Intervention"))
  )

#--------------------------------------
##### Activity Profile IS graph ######
library(ggrepel)

ggplot(data, aes(x = Timepoint, y = IS, group = ID, color = factor(ID))) +
  
  # Individual lines + points
  geom_line(alpha = 0.7) +
  geom_point(size = 3) +
  
  #SEM Error bars 
  stat_summary(aes(group = 1),
               fun.data = mean_se,
               geom = "errorbar",
               width = 0.1,
               color = "black",
               size = 0.5) +
  
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
    title = "CGM Glucose BL vs INT – IS",
    x = "Timepoint",
    y = "Interdaily Stability (IS)",
    color = "Participant ID"
  )

#--------------------------------------
##### Activity Profile IV graph ######
library(ggrepel)

ggplot(data, aes(x = Timepoint, y = IV, group = ID, color = factor(ID))) +
  
  # Individual lines + points
  geom_line(alpha = 0.7) +
  geom_point(size = 3) +
  
  #SEM Error bars 
  stat_summary(aes(group = 1),
               fun.data = mean_se,
               geom = "errorbar",
               width = 0.1,
               color = "black",
               size = 0.5) +
  
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
    title = "CGM Glucose BL vs INT – IV",
    x = "Timepoint",
    y = "Intradaily variability (IV)",
    color = "Participant ID"
  )


#-----------------------------------------------------------------------------
#-----------------------------------------------------------------------------
###### Graph for Periodogram summaries of all participants BL to INT  #######

#### This is for GENEActiv Movement 
library(tidyverse)
library(ggrepel)

# Read data # MUST change this! 
setwd("C:/Users/liann/Downloads/HL2/HL2 Summary Sheets/data tables")
data <- read_csv("3.10.26.gene move periodogram.csv")


# Clean columns
data <- data %>%
  rename(
    ID = Filename,
    Timepoint = `...2`
  ) %>%
  mutate(
    Timepoint = str_trim(Timepoint),
    Timepoint = str_to_upper(Timepoint),
    Timepoint = recode(Timepoint,
                       "BL" = "Baseline",
                       "INT" = "Intervention"),
    Timepoint = factor(Timepoint, levels = c("Baseline", "Intervention"))
  )


# Replace "NaN" and NA with 0
data_block[data_block == "NaN"] <- 0
data_block[is.na(data_block)] <- 0

ggplot(data, aes(x = Timepoint, 
                 y = `Amplitude 1`, 
                 group = ID, 
                 color = factor(ID))) +
  
  geom_line(alpha = 0.7) +
  geom_point(size = 3) +
  
  #SEM Error bars 
  stat_summary(aes(group = 1),
               fun.data = mean_se,
               geom = "errorbar",
               width = 0.1,
               color = "black",
               size = 0.5) + 
  
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
    title = "GENEActiv Movement BL vs INT - 24hr Periodogram Amplitude",
    x = "Timepoint",
    y = "Amplitude (24h)",
    color = "Participant ID"
  )



#--------------------------------------------------------------------------------------
#### This is for GENEActiv Temperature 
library(tidyverse)
library(ggrepel)

# Read data # MUST change this! 
setwd("C:/Users/liann/Downloads/HL2/HL2 Summary Sheets/data tables")
data <- read_csv("3.10.26.gene temp periodogram.csv")


# Clean columns
data <- data %>%
  rename(
    ID = Filename,
    Timepoint = `...2`
  ) %>%
  mutate(
    Timepoint = str_trim(Timepoint),
    Timepoint = str_to_upper(Timepoint),
    Timepoint = recode(Timepoint,
                       "BL" = "Baseline",
                       "INT" = "Intervention"),
    Timepoint = factor(Timepoint, levels = c("Baseline", "Intervention"))
  )

ggplot(data, aes(x = Timepoint, 
                 y = `Amplitude 1`, 
                 group = ID, 
                 color = factor(ID))) +
  
  geom_line(alpha = 0.7) +
  geom_point(size = 3) +
  
  #SEM Error bars 
  stat_summary(aes(group = 1),
               fun.data = mean_se,
               geom = "errorbar",
               width = 0.1,
               color = "black",
               size = 0.5) +
  
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
    title = "GENEActiv Temperature BL vs INT - 24hr Periodogram Amplitude",
    x = "Timepoint",
    y = "Amplitude (24h)",
    color = "Participant ID"
  )

#----------------------------------------------------------------------------------
#### This is for CGM Glucose  
library(tidyverse)
library(ggrepel)

# Read data # MUST change this! 
setwd("C:/Users/liann/Downloads/HL2/HL2 Summary Sheets/data tables")
data <- read_csv("3.10.26.cgm periodogram.csv")


# Clean columns
data <- data %>%
  rename(
    ID = Filename,
    Timepoint = `...2`
  ) %>%
  mutate(
    Timepoint = str_trim(Timepoint),
    Timepoint = str_to_upper(Timepoint),
    Timepoint = recode(Timepoint,
                       "BL" = "Baseline",
                       "INT" = "Intervention"),
    Timepoint = factor(Timepoint, levels = c("Baseline", "Intervention"))
  )

ggplot(data, aes(x = Timepoint, 
                 y = `Amplitude 1`, 
                 group = ID, 
                 color = factor(ID))) +
  
  geom_line(alpha = 0.7) +
  geom_point(size = 3) +
  
  #SEM Error bars 
  stat_summary(aes(group = 1),
               fun.data = mean_se,
               geom = "errorbar",
               width = 0.1,
               color = "black",
               size = 0.5) +
  
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
    title = "CGM Glucose BL vs INT - 24hr Periodogram Amplitude",
    x = "Timepoint",
    y = "Amplitude (24h)",
    color = "Participant ID"
  )
