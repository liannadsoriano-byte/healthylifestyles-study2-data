### making overlaying activity profile graphs for cgm ### 
### Made by Lianna Soriano and CHATGPT #### 

library(tidyverse)

# Load files
setwd("C:/Users/liann/Downloads/HL2/cgm activity profiles")
BL  <- read.csv("973 BL CGM glucose_Activity Profile_Table Data.csv")
INT <- read.csv("973 INT CGM glucose_Table Data.csv")


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
    title = "CGM Activity Profile",
    x = "Circadian Time (h)",
    y = "Blood Glucose Amplitude",
    color = "Group",
    fill = "Group"
  ) +
  theme_classic(base_size = 14)








