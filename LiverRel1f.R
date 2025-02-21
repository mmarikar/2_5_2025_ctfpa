# Load required packages
library(ggplot2)
library(dplyr)
library(readxl)

CTFPACo <- read_excel("OrganWeightsPFAS.xlsx", sheet='CTFPA', na = c("", "NA")) 
View(CTFPACo)
# Include only females (rows 61-120)
CTFPACof <- PFHICo[c(61:120),]
View(CTFPACof)
# Ensure Group is treated as an ordered factor 
dose_levels <- c(0, 6.3, 12.5, 25, 50, 100) # Define correct dose order
CTFPACof$Group <- factor(CTFPACof$Group, levels = dose_levels, ordered = TRUE)

# Manually select 100 and 200 mg/kg-day groups
top_doses <- CTFPACof %>%
  filter(Group %in% c(25,50,100)) %>% #select only 100 and 200 mg/kg-day
  group_by(Group) %>%
  summarise(max_LiverRel = max(LiverRel, na.rm = TRUE)) %>%
  # arrange(desc(max_LiverRel)) %>% # Sort by median values
  # slice(1:2) %>% # Select the two highest median values
  mutate(y_position = max_LiverRel + 0.2) # Adjust y-position for asterisk placement

#Get overall max LiverRel to set y-axis limits
y_max <- max(CTFPACof$LiverRel + 0.3) #Increase spacing to avoiad cutoff


# Create the violin + box plot
LiverRel1f <- ggplot(CTFPACof, aes(x = Group, y = LiverRel, fill = Group)) +
  geom_violin(trim = TRUE, alpha = 0.7, color = "black") + # Violin plot
  geom_boxplot(width = 0.1, color = "black", alpha = 0.9) + # Box plot
  
  # Ensure the median is properly displayed
  stat_summary(fun = median, geom = "point", shape = 23, size = 3, fill = "red") +
  
  # Add asterisks above the 25,50 and 100 mg/kg-day groups 
  geom_text(data = top_doses, aes(x = Group, y = y_position, label = "*"),
            size = 6, color = "black", vjust = -0.5) + #Move the asterisk up
  
  #Manually set y-axis limits to prevent asterisk cut-off
  ylim(NA, y_max) +
  
  labs(title = "CTFPA - Female Rat Liver/Body Weight 90-Day Study",
       x = "Dose Group (mg/kg-day)", y = "Liver/BW Ratio (%)") +
  
  scale_fill_brewer(palette = "Set2", name = "Group") + # Colorful palette
  theme_bw() +
  theme(legend.position = "none")

# Save the plot as a PNG file
ggsave(filename = "LiverRel1f.png", plot = LiverRel1f, width = 8, height = 6, dpi = 300)

# Display the plot
LiverRel1f