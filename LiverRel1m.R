# Load required packages
library(ggplot2)
library(dplyr)
library(readxl)

CTFPACo <- read_excel("OrganWeightsPFAS.xlsx", sheet='CTFPA', na = c("", "NA")) 
View(CTFPACo)
# Include only male (rows 1:60)
CTFPACom <- CTFPACo[(1:60),]
View(CTFPACom)

#For debugging
CTFPACom$LiverRel #indicates a NA
sum(is.na(CTFPACom$LiverRel)) # counts NA values as 1
CTFPACom <- CTFPACom %>% filter(!is.na(LiverRel)) #Remove NAs
CTFPACom$LiverRel # NO NAs now
#y_max <- max(CTFPACom$LiverRel)
#View(y_max)
# Ensure Group is treated as an ordered factor 
dose_levels <- c(0, 1.9, 3.8, 7.5, 15, 30) # Define correct dose order
CTFPACom$Group <- factor(CTFPACom$Group, levels = dose_levels, ordered = TRUE)

#Get overall max LiverRel to set y-axis limits
y_max  <- max(CTFPACom$LiverRel, na.rm = TRUE) +1
View(y_max)

#max(PFHICom$LiverRel
# Manually select top 4 mg/kg-day groups
top_doses <- CTFPACom %>%
  filter(Group %in% c(3.8,7.5,15,30)) %>% #select only the top four mg/kg-day doses
  group_by(Group) %>%
  summarise(max_LiverRel = max(LiverRel, na.rm = TRUE)) %>%
  distinct(Group, .keep_all =TRUE) %>% #keep only 1 row per dose group 
  # arrange(desc(max_LiverRel)) %>% # Sort by median values
  # slice(1:2) %>% # Select the two highest median values
  mutate(y_position = max_LiverRel + 0.5) # Adjust y-position for asterisk placement

# Create the violin + box plot
LiverRel1m <- ggplot(CTFPACom, aes(x = Group, y = LiverRel, fill = Group)) +
  geom_violin(trim = TRUE, alpha = 0.7, color = "black") + # Violin plot
  geom_boxplot(width = 0.1, color = "black", alpha = 0.9) + # Box plot
  
  # Ensure the median is properly displayed
  stat_summary(fun = median, geom = "point", shape = 23, size = 3, fill = "red") +
  
  # Add asterisks above the 100 and 200 mg/kg-day groups 
  geom_text(data = top_doses, aes(x = Group, y = y_position, label = "*"),
            size = 6, color = "black", vjust = -0.5) + #Move the asterisk up
  
  #Manually set y-axis limits to prevent asterisk cut-off
  ylim(NA, y_max) +
  
  labs(title = "CTFPA - Male Rat Liver/Body Weight 90-Day Study",
       x = "Dose Group (mg/kg-day)", y = "Liver/BW Ratio (%)") +
  
  scale_fill_brewer(palette = "Set2", name = "Group") + # Colorful palette
  theme_bw() +
  theme(legend.position = "none")

# Save the plot as a PNG file.
ggsave(filename = "LiverRel1m.png", plot = LiverRel1m, width = 8, height = 6, dpi = 300)


# Display the plot
LiverRel1m