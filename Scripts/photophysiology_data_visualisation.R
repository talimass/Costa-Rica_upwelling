## Costa Rica photophysiology
options(scipen = 999)


library(tidyverse)
library(ggpubr)
library(dplyr)
library(vegan)
library(ggplot2)
library(cowplot)
library(RColorBrewer)
library(car)
library(tidyr)
library(emmeans)
library(reshape2)
library(scales)
library(ARTool)
library(multcomp)
library(rcompanion)
library(psych)
fire.data <- read_csv("/Users/talimass/Documents/Documents - MacBook Pro/GitHub/Costa-Rica_upwelling/Data/Samara_anotated _xls.csv")## you need to path this to your data file


#FIRe Data
#fire.data$Site_Species_depth <- paste(fire.data$Site, fire.data$Species, fire.data$depth, sep = " - ")
#fire.data$Site_Species <- paste(fire.data$Site, fire.data$Species, sep = " - ")

fire.data$Site_depth <- paste(fire.data$Site, fire.data$depth, sep = " - ")
fire_data <- fire.data %>% na.omit()


# fvfm

str(fire_data$fv_fm)
class(fire_data$fv_fm)
# Convert character or factor to numeric
fire_data$fv_fm <- as.numeric(as.character(fire_data$fv_fm))

# Now compute quartiles
quartiles <- quantile(fire_data$fv_fm, probs = c(0.25, 0.75), na.rm = TRUE)
IQR_cor <- IQR(fire_data$fv_fm)

lower <- quartiles[1] - 1.5*IQR_cor
upper <- quartiles[2] + 1.5*IQR_cor

fvfm_no_outlier <- subset(fire_data, fire_data$fv_fm >lower & fire_data$fv_fm< upper)


#fvfm <- ggplot(data = fvfm_no_outlier,aes(x= Site_Species, y=fv_fm, fill= Species))+
 # geom_boxplot() + theme_classic() + stat_boxplot(geom = 'errorbar', width = 0.75, position = "dodge") +
 # labs(title="Quantum yield of photochemistry in PSII", y="Fv’/Fm’") +
  #theme(legend.position = "none") +
  #theme(axis.title.y = element_blank()) +
 # scale_x_discrete(limits = rev) +
  #coord_flip() 

#fvfm



#plot of Each species is in a separate panel using

fvfm <- ggplot(data = fvfm_no_outlier, aes(x = fv_fm, y = Site_depth, fill = Species)) +
  geom_boxplot() + 
  stat_boxplot(geom = "errorbar", width = 0.75) +  # Removed position="dodge"
  theme_classic() +
  labs(title = "Quantum Yield of Photochemistry in PSII", 
       x = "Fv’/Fm’", 
       y = "Site - depth") +  # Y-axis now represents Site- depth
  theme(legend.position = "none") +
  facet_wrap(~ Species, scales = "free_y") +  # Each species gets a separate panel
  scale_y_discrete(limits = rev(levels(factor(fvfm_no_outlier$Site_depth))))  # Reverse order for readability



# Display the plot in R before saving
print(fvfm)
# Define the file name and path for saving
file_name <- paste0(output_folder, "/fvfm.png")

# Save the plot
ggsave(filename = file_name, plot = fvfm, width = 8, height = 6, dpi = 300)

# Print the saved file location
print(paste("Saved the combined plot at", file_name))


#plot with all data points (scatter points) on top of the boxplots


# Create the plot with 4 panels, one for each species
fvfm_plot <- ggplot(data = fvfm_no_outlier, aes(x = fv_fm, y = Site_depth, fill = Species)) +
  geom_boxplot(alpha = 0.6, outlier.shape = NA) +  # Boxplots with transparency
  geom_jitter(color = "black", width = 0.1, size = 1, alpha = 0.6) +  # Black scatter points
  stat_boxplot(geom = "errorbar", width = 0.75) +  # Error bars for boxplots
  theme_classic() +
  labs(title = "Quantum Yield of PSII by Species", 
       x = "Fv’/Fm’", 
       y = "Site-depth") +  
  theme(legend.position = "none") +
  facet_wrap(~ Species, scales = "free_y", ncol = 2) +  # Facet by Species with 2 columns
  scale_y_discrete(limits = rev(levels(factor(fvfm_no_outlier$Site_depth))))  

# Display the plot in R before saving
print(fvfm_plot)

# Define the file name and path for saving
file_name <- paste0(output_folder, "/fvfm_plot.png")

# Save the plot
ggsave(filename = file_name, plot = fvfm_plot, width = 8, height = 6, dpi = 300)

# Print the saved file location
print(paste("Saved the combined plot at", file_name))



### sigma

#quartiles <- quantile(fire_data$Sigma, probs = c(.25,.75))

str(fire_data$Sigma)
class(fire_data$Sigma)
# Convert character or factor to numeric
fire_data$Sigma <- as.numeric(as.character(fire_data$Sigma))


# Ensure Sigma is numeric
fire_data$Sigma <- as.numeric(as.character(fire_data$Sigma))

# Calculate quartiles (25th and 75th percentiles)
quartiles <- quantile(fire_data$Sigma, probs = c(0.25, 0.75), na.rm = TRUE)

# Calculate IQR (Interquartile Range)
IQR_cor <- IQR(fire_data$Sigma, na.rm = TRUE)

# Calculate lower and upper bounds for outliers
lower <- quartiles[1] - 1.5 * IQR_cor
upper <- quartiles[2] + 1.5 * IQR_cor

# Subset the data to remove outliers based on Sigma
sigma_no_outlier <- subset(fire_data, Sigma > lower & Sigma < upper)

# Create the plot
sigma_plot <- ggplot(data = sigma_no_outlier, aes(x = Sigma, y = Site_depth, fill = Species)) +  # Use 'Sigma' here
  geom_boxplot() +
  stat_boxplot(geom = "errorbar", width = 0.75) +  # Removed position="dodge"
  theme_classic() +
  labs(title = "Functional Absorption Cross-section of PSII", 
       x = "σPSII’(A2)", 
       y = "Site - Depth") +  # Y-axis now represents Site-Depth
  theme(legend.position = "none") +
  facet_wrap(~ Species, scales = "free_y") +  # Each species gets a separate panel
  scale_y_discrete(limits = rev(levels(factor(sigma_no_outlier$Site_depth))))  # Reverse order for readability

# Print the plot
print(sigma_plot)

# Save the plot
output_folder <- "/Users/talimass/Documents/Documents - MacBook Pro/GitHub/Costa-Rica_upwelling/Output"
file_name <- paste0(output_folder, "/sigma_plot.png")

# Use the correct plot object (sigma_plot)
ggsave(filename = file_name, plot = sigma_plot, width = 8, height = 6, dpi = 300)

# Print the saved file location
print(paste("Saved the plot at", file_name))


#plot with all data points (scatter points) on top of the boxplots

# Create the plot with 4 panels, one for each species

sigma <- ggplot(data = sigma_no_outlier, aes(x = Sigma, y = Site_depth, fill = Species)) +
  geom_boxplot(alpha = 0.6, outlier.shape = NA) +  # Boxplot with slight transparency, hides outliers
  geom_jitter(color = "black", width = 0.1, size = 1, alpha = 0.6) +  # Black points, smaller size
  stat_boxplot(geom = "errorbar", width = 0.75) +  # Error bars for boxplots
  theme_classic() +
  labs(title = "Functional Absorption Cross-section of PSII", 
       x = "σPSII’(A2)", 
       y = "Site - Depth") +  # Y-axis now represents Site-Depth +  
  
  theme(legend.position = "none") +
  facet_wrap(~ Species, scales = "free_y", ncol = 2) +  # Facet by Species with 2 columns
  scale_y_discrete(limits = rev(levels(factor(sigma_no_outlier$Site_depth)))) 

# Display the plot in R before saving
print(sigma)

# Define the file name and path for saving
file_name <- paste0(output_folder, "/sigma.png")

# Save the plot
ggsave(filename = file_name, plot = sigma, width = 8, height = 6, dpi = 300)

# Print the saved file location
print(paste("Saved the combined plot at", file_name))



#### pmax

quartiles <- quantile(fire_data$Pmax.e.s, probs = c(.25,.75))
IQR_cor <- IQR(fire_data$Pmax.e.s)

lower <- quartiles[1] - 1.5*IQR_cor
upper <- quartiles[2] + 1.5*IQR_cor

Pmax.e.s_no_outlier <- subset(fire_data, fire_data$Pmax.e.s >lower & fire_data$Pmax.e.s< upper)


# Create the plot
pmax_plot <- ggplot(data = Pmax.e.s_no_outlier, aes(x = Pmax.e.s, y = Site_depth, fill = Species)) +  # Use 'pmax' here
  geom_boxplot() +
  stat_boxplot(geom = "errorbar", width = 0.75) +  # Removed position="dodge"
  theme_classic() +
  labs(title = "Maximum photosynthetic rate", 
       x = "Pmax (electron s-1 PSII-1)", 
       y = "Site - Depth") +  # Y-axis now represents Site-Depth
  theme(legend.position = "none") +
  facet_wrap(~ Species, scales = "free_y") +  # Each species gets a separate panel
  scale_y_discrete(limits = rev(levels(factor(Pmax.e.s_no_outlier$Site_depth))))  # Reverse order for readability

# Print the plot
print(pmax_plot)

# Save the plot
output_folder <- "/Users/talimass/Documents/Documents - MacBook Pro/GitHub/Costa-Rica_upwelling/Output"
file_name <- paste0(output_folder, "/pmax_plot.png")

# Use the correct plot object (pmax_plot)
ggsave(filename = file_name, plot = pmax_plot, width = 8, height = 6, dpi = 300)

# Print the saved file location
print(paste("Saved the plot at", file_name))


#plot with all data points (scatter points) on top of the boxplots

# Create the plot with 4 panels, one for each species

pmax <- ggplot(data = Pmax.e.s_no_outlier, aes(x = Pmax.e.s, y = Site_depth, fill = Species)) +
  geom_boxplot(alpha = 0.6, outlier.shape = NA) +  # Boxplot with slight transparency, hides outliers
  geom_jitter(color = "black", width = 0.1, size = 1, alpha = 0.6) +  # Black points, smaller size
  stat_boxplot(geom = "errorbar", width = 0.75) +  # Error bars for boxplots
  theme_classic() +
  labs(title = "Maximum photosynthetic rate", 
       x = "Pmax (electron s-1 PSII-1)", 
       y = "Site - Depth") +  # Y-axis now represents Site-Depth
  
  theme(legend.position = "none") +
  facet_wrap(~ Species, scales = "free_y", ncol = 2) +  # Facet by Species with 2 columns
  scale_y_discrete(limits = rev(levels(factor(Pmax.e.s_no_outlier$Site_depth)))) 

# Display the plot in R before saving
print(pmax)

# Define the file name and path for saving
file_name <- paste0(output_folder, "/pmax.png")

# Save the plot
ggsave(filename = file_name, plot = pmax, width = 8, height = 6, dpi = 300)

# Print the saved file location
print(paste("Saved the combined plot at", file_name))

#### p

#quartiles <- quantile(fire_data$p, probs = c(.25,.75))


str(fire_data$p)
class(fire_data$p)
# Convert character or factor to numeric
fire_data$p <- as.numeric(as.character(fire_data$p))

# Now compute quartiles
quartiles <- quantile(fire_data$p, probs = c(0.25, 0.75), na.rm = TRUE)
IQR_cor <- IQR(fire_data$p)

lower <- quartiles[1] - 1.5*IQR_cor
upper <- quartiles[2] + 1.5*IQR_cor


p_no_outlier <- subset(fire_data, fire_data$p >lower & fire_data$p< upper)



# Create the plot
p_plot <- ggplot(data = p_no_outlier, aes(x = p, y = Site_depth, fill = Species)) +  # Use 'pmax' here
  geom_boxplot() +
  stat_boxplot(geom = "errorbar", width = 0.75) +  # Removed position="dodge"
  theme_classic() +
  labs(title = "connectivity parameter", 
       x = "p", 
       y = "Site - Depth") +  # Y-axis now represents Site-Depth
  theme(legend.position = "none") +
  facet_wrap(~ Species, scales = "free_y") +  # Each species gets a separate panel
  scale_y_discrete(limits = rev(levels(factor(p_no_outlier$Site_depth))))  # Reverse order for readability

# Print the plot
print(p_plot)

# Save the plot
output_folder <- "/Users/talimass/Documents/Documents - MacBook Pro/GitHub/Costa-Rica_upwelling/Output"
file_name <- paste0(output_folder, "/p_plot.png")

# Use the correct plot object (pmax_plot)
ggsave(filename = file_name, plot = p_plot, width = 8, height = 6, dpi = 300)

# Print the saved file location
print(paste("Saved the plot at", file_name))


#plot with all data points (scatter points) on top of the boxplots

# Create the plot with 4 panels, one for each species

p <- ggplot(data = p_no_outlier, aes(x = p, y = Site_depth, fill = Species)) +
  geom_boxplot(alpha = 0.6, outlier.shape = NA) +  # Boxplot with slight transparency, hides outliers
  geom_jitter(color = "black", width = 0.1, size = 1, alpha = 0.6) +  # Black points, smaller size
  stat_boxplot(geom = "errorbar", width = 0.75) +  # Error bars for boxplots
  theme_classic() +
  labs(title = "connectivity parameter", 
       x = "p", 
       y = "Site - Depth") +  # Y-axis now represents Site-Depth
  
  theme(legend.position = "none") +
  facet_wrap(~ Species, scales = "free_y", ncol = 2) +  # Facet by Species with 2 columns
  scale_y_discrete(limits = rev(levels(factor(p_no_outlier$Site_depth)))) 

# Display the plot in R before saving
print(p)

# Define the file name and path for saving
file_name <- paste0(output_folder, "/p.png")

# Save the plot
ggsave(filename = file_name, plot = p, width = 8, height = 6, dpi = 300)

# Print the saved file location
print(paste("Saved the combined plot at", file_name))


#### Fm

#quartiles <- quantile(fire_data$Fm, probs = c(.25,.75))


str(fire_data$Fm)
class(fire_data$Fm)
#Convert character or factor to numeric
fire_data$Fm <- as.numeric(as.character(fire_data$Fm))
sum(is.na(fire_data$Fm))

# Now compute quartiles
quartiles <- quantile(fire_data$Fm, probs = c(0.25, 0.75), na.rm = TRUE)
IQR_cor <- IQR(fire_data$Fm)

lower <- quartiles[1] - 1.5*IQR_cor
upper <- quartiles[2] + 1.5*IQR_cor

Fm_no_outlier <- subset(fire_data, fire_data$Fm >lower & fire_data$Fm< upper)


# Create the plot
Fm_plot <- ggplot(data = Fm_no_outlier, aes(x = Fm, y = Site_depth, fill = Species)) +  # Use 'pmax' here
  geom_boxplot() +
  stat_boxplot(geom = "errorbar", width = 0.75) +  # Removed position="dodge"
  theme_classic() +
  labs(title = "Max Fluorescent", 
       x = "Fm", 
       y = "Site - Depth") +  # Y-axis now represents Site-Depth
  theme(legend.position = "none") +
  facet_wrap(~ Species, scales = "free_y") +  # Each species gets a separate panel
  scale_y_discrete(limits = rev(levels(factor(Fm_no_outlier$Site_depth))))  # Reverse order for readability

# Print the plot
print(Fm_plot)

# Save the plot
output_folder <- "/Users/talimass/Documents/Documents - MacBook Pro/GitHub/Costa-Rica_upwelling/Output"
file_name <- paste0(output_folder, "/Fm_plot.png")

# Use the correct plot object (Fm_plot)
ggsave(filename = file_name, plot = Fm_plot, width = 8, height = 6, dpi = 300)

# Print the saved file location
print(paste("Saved the plot at", file_name))


#plot with all data points (scatter points) on top of the boxplots

# Create the plot with 4 panels, one for each species

Fm <- ggplot(data = Fm_no_outlier, aes(x = Fm, y = Site_depth, fill = Species)) +
  geom_boxplot(alpha = 0.6, outlier.shape = NA) +  # Boxplot with slight transparency, hides outliers
  geom_jitter(color = "black", width = 0.1, size = 1, alpha = 0.6) +  # Black points, smaller size
  stat_boxplot(geom = "errorbar", width = 0.75) +  # Error bars for boxplots
  theme_classic() +
  labs(title = "Max Fluorescent", 
       x = "Fm", 
       y = "Site - Depth") +  # Y-axis now represents Site-Depth
  
  theme(legend.position = "none") +
  facet_wrap(~ Species, scales = "free_y", ncol = 2) +  # Facet by Species with 2 columns
  scale_y_discrete(limits = rev(levels(factor(Fm_no_outlier$Site_depth)))) 

# Display the plot in R before saving
print(Fm)

# Define the file name and path for saving
file_name <- paste0(output_folder, "/Fm.png")

# Save the plot
ggsave(filename = file_name, plot = Fm, width = 8, height = 6, dpi = 300)

# Print the saved file location
print(paste("Saved the combined plot at", file_name))


