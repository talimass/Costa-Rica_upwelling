# title: "Function groups CR upwelling
# author: "Tali Mass"
# date: "15/05/2026"

######Convert raw data from wide to long format 



library(readxl)
library(dplyr)
library(tidyr)
library(readr)

# Load Book2
book2 <- read_excel("/Users/talimass/Desktop/CR_benthic/Transact raw data.xlsx")

# Columns that identify each sample/image
id_cols <- c("Site", "Transect", "PIC")

# Convert from wide to long format
book2_converted <- book2 %>%
  pivot_longer(
    cols = -all_of(id_cols),
    names_to = "LABEL",
    values_to = "Count"
  ) %>%
  filter(Count > 0) %>%              # remove zero counts
  rename(SITE = Site) %>%
  mutate(
    GROUP = case_when(
      grepl("Pavona|Pocillopora|Porites|Psammocora", LABEL) ~ "Hard Coral",
      grepl("Ascidian|Sponge", LABEL) ~ "Invertebrate",
      grepl("Caulerpa|Galaxaura|Jania|Padina|Turf algae", LABEL) ~ "Algae",
      grepl("CCA", LABEL) ~ "CCA",
      grepl("Seagrass", LABEL) ~ "Algae",
      grepl("Unknown", LABEL) ~ "Unknown",
      TRUE ~ "Other"
    ),
    
    # optional placeholders (you can refine later)
    
    Live_Non_live = ifelse(GROUP %in% c("Hard Coral", "Algae", "CCA", "Seagrass"), "Live", "Non-live")
  ) %>%
  
  select(PIC, SITE, GROUP, LABEL, Live_Non_live, Count)
# Save as CSV
write_csv(book2_converted, "Book2_converted.csv")

write_csv(book2_converted, "/Users/talimass/Desktop/CR_benthic/Book2_converted.csv")


#########################

library(tidyverse)
library(scales)

# Paths
data_path <- "/Users/talimass/Desktop/CR_benthic/Annotatations_FG.csv"
output_path <- "/Users/talimass/Desktop/CR_benthic"
dir.create(output_path, showWarnings = FALSE, recursive = TRUE)

# Load data
fg <- read_csv(data_path)

# Make sure variables are factors
fg <- fg %>%
  mutate(
    SITE = factor(SITE),
    PIC = factor(PIC),
    TRANSACT = factor(TRANSACT),
    GROUP = factor(GROUP)
  )

# Check groups
print(levels(fg$GROUP))


###Live covarage by site
# Total points per picture
pic_totals <- fg %>%
  group_by(SITE, TRANSACT, PIC) %>%
  summarise(Total_points = sum(Count, na.rm = TRUE), .groups = "drop")

# Live count per picture
live <- fg %>%
  filter(Live_Non_live == "Live") %>%
  group_by(SITE, TRANSACT, PIC) %>%
  summarise(Live_count = sum(Count, na.rm = TRUE), .groups = "drop") %>%
  left_join(pic_totals, by = c("SITE", "TRANSACT", "PIC")) %>%
  mutate(
    prop = Live_count / Total_points,
    Cov_percent = prop * 100
  )

# Check values
summary(live$Cov_percent)
max(live$Cov_percent)

p_live_site <- ggplot(live, aes(x = SITE, y = prop, color = SITE)) +
  geom_boxplot(outlier.shape = NA, linewidth = 0.5) +
  geom_point(position = position_jitter(width = 0.12), alpha = 0.6, size = 1.5) +
  scale_color_manual(values = c(
    "Papagayo" = "lightseagreen",
    "Samara" = "navyblue"
  )) +
  scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(0, 1)) +
  theme_bw() +
  labs(x = "Site", y = "Live coverage (%)", color = "Site") +
  theme(
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 14),
    legend.position = "none",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )

print(p_live_site)

ggsave(file.path(output_path, "Live_site.png"), p_live_site, width = 8, height = 6, dpi = 300)
ggsave(file.path(output_path, "Live_site.pdf"), p_live_site, width = 8, height = 6)


######Function group by site:
group_order <- c(
  "Hard Coral", "Soft Coral", "Algae", "CCA", "Ascidian",
  "Diadema", "Sponge", "Substrate", "Other", "Unknown"
)

group_colors <- c(
  "Hard Coral" = "deepskyblue4",
  "Soft Coral" = "cyan3",
  "Algae" = "darkseagreen2",
  "CCA" = "deeppink4",
  "Ascidian" = "purple",
  "Diadema" = "black",
  "Sponge" = "darksalmon",
  "Substrate" = "bisque3",
  "Other" = "burlywood4",
  "Unknown" = "gray60"
)

Fun_groups_site <- fg %>%
  group_by(SITE, GROUP) %>%
  summarise(Count = sum(Count, na.rm = TRUE), .groups = "drop") %>%
  group_by(SITE) %>%
  mutate(Percentage = Count / sum(Count)) %>%
  ungroup() %>%
  mutate(GROUP = factor(GROUP, levels = group_order))

p_groups_site <- ggplot(Fun_groups_site, aes(x = SITE, y = Percentage, fill = GROUP)) +
  geom_bar(stat = "identity", width = 0.6) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  scale_fill_manual(values = group_colors, drop = FALSE) +
  theme_bw() +
  labs(x = "Site", y = "Percentage", fill = "Functional group") +
  theme(
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 14),
    legend.title = element_text(size = 18),
    legend.text = element_text(size = 14),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )

print(p_groups_site)

ggsave(file.path(output_path, "Functional_groups_by_site.png"), p_groups_site, width = 8, height = 6, dpi = 300)
ggsave(file.path(output_path, "Functional_groups_by_site.pdf"), p_groups_site, width = 8, height = 6)


######Function group coverage by picture: 
Groups_all <- fg %>%
  group_by(SITE, TRANSACT, PIC, GROUP) %>%
  summarise(Count = sum(Count, na.rm = TRUE), .groups = "drop") %>%
  left_join(pic_totals, by = c("SITE", "TRANSACT", "PIC")) %>%
  mutate(
    prop = Count / Total_points,
    Cov_percent = prop * 100
  )

Groups_all_long <- Groups_all %>%
  dplyr::select(SITE, TRANSACT, PIC, GROUP, Cov_percent) %>%
  complete(SITE, TRANSACT, PIC, GROUP, fill = list(Cov_percent = 0)) %>%
  mutate(GROUP = factor(GROUP, levels = group_order))

#######Statistics for each functional group by site

stats_all_groups <- Groups_all_long %>%
  group_by(GROUP) %>%
  summarise(
    n = n(),
    Papagayo_median = median(Cov_percent[SITE == "Papagayo"], na.rm = TRUE),
    Samara_median = median(Cov_percent[SITE == "Samara"], na.rm = TRUE),
    p_value = kruskal.test(Cov_percent ~ SITE)$p.value,
    .groups = "drop"
  ) %>%
  mutate(
    p_adj_BH = p.adjust(p_value, method = "BH")
  )

print(stats_all_groups)

write.csv(
  stats_all_groups,
  file.path(output_path, "Functional_groups_site_statistics.csv"),
  row.names = FALSE
)

####################### HARD CORALS: ART ANOVA test & plot by SITE

library(ARTool)
library(rcompanion)
library(ggplot2)
library(scales)

# Create Hard Coral dataframe from the corrected functional-group table
HC_df <- Groups_all_long %>%
  dplyr::filter(GROUP == "Hard Coral") %>%
  dplyr::mutate(
    SITE = as.factor(SITE),
    value = Cov_percent
  )

str(HC_df)

### Aligned ranks ANOVA
model_HC <- art(value ~ SITE, data = HC_df)

# Check model
model_HC

# Conduct ANOVA
anova(model_HC)

# Posthoc pairwise comparison for SITE
SITE_HC_posthoc <- art.con(model_HC, "SITE")
SITE_HC_posthoc

Sum_SITE_HC <- as.data.frame(SITE_HC_posthoc)

# Statistical group letters
HC_site_stat_letters <- cldList(p.value ~ contrast, data = Sum_SITE_HC)

# Save results
write.csv(
  Sum_SITE_HC,
  file.path(output_path, "HC_site_posthoc.csv"),
  row.names = FALSE
)

write.csv(
  HC_site_stat_letters,
  file.path(output_path, "HC_site_posthoc_letters.csv"),
  row.names = FALSE
)

############### Hard Coral plot

# Total annotation points per picture
pic_totals <- fg %>%
  dplyr::group_by(SITE, PIC) %>%
  dplyr::summarise(Total_points = sum(Count, na.rm = TRUE), .groups = "drop")

# Hard coral coverage per picture, site, and label
HC_df <- fg %>%
  dplyr::filter(GROUP == "Hard Coral") %>%
  dplyr::group_by(SITE, PIC, LABEL) %>%
  dplyr::summarise(Count = sum(Count, na.rm = TRUE), .groups = "drop") %>%
  dplyr::left_join(pic_totals, by = c("SITE", "PIC")) %>%
  dplyr::mutate(
    Cov_percent = (Count / Total_points) * 100,
    SITE = as.factor(SITE),
    LABEL = as.factor(LABEL)
  )

# Check
names(HC_df)
head(HC_df)

# Plot
p_hard <- ggplot(HC_df, aes(x = LABEL, y = Cov_percent)) +
  geom_boxplot(fill = "lightblue", outlier.shape = NA) +
  geom_point(position = position_jitter(width = 0.2), alpha = 0.6) +
  facet_wrap(~SITE) +
  theme_bw() +
  labs(
    title = "Hard Coral coverage (%)",
    x = "Species",
    y = "Coverage (%)"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title = element_text(size = 14),
    axis.text  = element_text(size = 12),
    strip.text = element_text(size = 14),
    panel.grid = element_blank()
  )

print(p_hard)

# Save plot
ggsave(
  filename = file.path(output_path, "Hard_Coral_coverage_by_site.png"),
  plot = p_hard,
  width = 7,
  height = 5,
  dpi = 300
)

ggsave(
  filename = file.path(output_path, "Hard_Coral_coverage_by_site.pdf"),
  plot = p_hard,
  width = 7,
  height = 5
)

