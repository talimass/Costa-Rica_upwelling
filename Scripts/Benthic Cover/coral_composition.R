# title: "Hard coral composition by site"
# author: "Tali Mass"
# date: "15/05/2026"

library(tidyverse)
library(scales)

windowsFonts("Times New Roman" = windowsFont("Times New Roman"))

# Set Times New Roman as default font for all plots
theme_set(
  theme_bw(base_family = "serif", base_size = 12) +
  theme(
    strip.text   = element_text(family = "serif", face = "bold",    size = 14),
    axis.title   = element_text(family = "serif",                   size = 12),
    axis.text    = element_text(family = "serif",                   size = 11),
    legend.title = element_text(family = "serif",                   size = 12),
    legend.text  = element_text(family = "serif",                   size = 11),
    plot.title   = element_text(family = "serif", face = "bold",    size = 14),
    plot.tag     = element_text(family = "serif", face = "bold",    size = 16),
    panel.grid   = element_blank()
  )
)

# Paths
data_path   <- "C:/Costa_rica-2025/Coral_net/output/new/Only_corals.csv"
output_path <- "C:/Costa_rica-2025/Coral_net/output/new/OUTPUT"
dir.create(output_path, showWarnings = FALSE, recursive = TRUE)

# Load data
corals_raw <- read_csv(data_path)

# Clean column names
names(corals_raw) <- gsub('"', '', names(corals_raw))  # remove stray quotes
print(names(corals_raw))

# ── Keep only Site + 4 coral species ──────────────────────────────────────────
coral_cols <- c("Pavona spp.", "Pocillopora spp.", "Porites spp.", "Psammocora stellata")

corals <- corals_raw %>%
  select(Site, all_of(coral_cols)) %>%
  rename(SITE = Site)

# ── Pivot to long format ───────────────────────────────────────────────────────
corals_long <- corals %>%
  pivot_longer(
    cols      = all_of(coral_cols),
    names_to  = "Species",
    values_to = "Cov_percent"
  ) %>%
  mutate(
    SITE = ifelse(SITE == "Samara", "Sámara", SITE),
    SITE = factor(SITE, levels = c("Papagayo", "Sámara")),
    Species = factor(Species, levels = coral_cols)
  )

# ── Mean coverage per site per species ────────────────────────────────────────
coral_means <- corals_long %>%
  group_by(SITE, Species) %>%
  summarise(Mean_cov = mean(Cov_percent, na.rm = TRUE), .groups = "drop")

# ── Normalize to relative composition within corals (sum to 100% per site) ───
coral_means <- coral_means %>%
  group_by(SITE) %>%
  mutate(Rel_cov = Mean_cov / sum(Mean_cov) * 100) %>%
  ungroup()

print(coral_means)

# ── Colours ───────────────────────────────────────────────────────────────────
# Paul Tol "Muted" colorblind-safe palette
# Chosen to differ from each other AND from functional group colors
coral_colors <- c(
  "Pavona spp."         = "#332288",   # dark indigo
  "Pocillopora spp."    = "#117733",   # dark forest green
  "Porites spp."        = "#DDCC77",   # warm golden
  "Psammocora stellata" = "#882255"    # dark wine/maroon
)

# ── Horizontal stacked bar ────────────────────────────────────────────────────
# Reverse species order so legend matches bar from left to right
coral_means_plot <- coral_means %>%
  mutate(Species = factor(Species, levels = rev(coral_cols)))

p_coral <- ggplot(coral_means_plot,
                  aes(y = SITE, x = Rel_cov, fill = Species)) +
  geom_bar(stat = "identity", width = 0.5,
           color = "black", linewidth = 0.4) +
  scale_x_continuous(
    labels = function(x) paste0(x, "%"),
    limits = c(0, 100),
    expand = c(0, 0)
  ) +
  scale_fill_manual(values = coral_colors, drop = FALSE,
                    guide  = guide_legend(reverse = TRUE)) +
  labs(
    x     = "Relative composition (%)",
    y     = NULL,
    fill  = "Species",
    title = "Hard coral composition by site",
    tag   = "(b)"
  ) +
  theme(
    plot.background    = element_rect(fill = "white", color = NA),
    axis.title.x       = element_text(size = 16),
    axis.text.y        = element_text(size = 16, face = "bold", color = "black"),
    axis.text.x        = element_text(size = 13, color = "black"),
    legend.title       = element_text(size = 14),
    legend.text        = element_text(size = 12),
    plot.title         = element_text(size = 18, hjust = 0.5, face = "bold"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor   = element_blank(),
    plot.tag           = element_text(size = 26, face = "bold"),
    plot.tag.position  = c(0.02, 0.98),
    plot.margin        = margin(10, 15, 10, 10)
  )

print(p_coral)
ggsave(file.path(output_path, "Hard_coral_composition_horiz.png"), p_coral,
       width = 11, height = 4, dpi = 300, bg = "white")
ggsave(file.path(output_path, "Hard_coral_composition_horiz.pdf"), p_coral,
       width = 11, height = 4, bg = "white")

message("\n✓ Saved to: ", output_path)
