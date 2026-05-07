# ==============================================================
# CoralNet - Corals only
# IQR box + median
# Mean + SD (UPWARD ONLY, no lower cap)
# Raw points black
# Significance stars inside panel
# Updates requested:
#   - Remove the black box around facet titles (strip background/border)
#   - Increase legend + axis text sizes a bit
# ==============================================================

library(tidyverse)
library(rstatix)
library(ggtext)

# ==== PATHS ====
data_path     <- "C:\\Costa_rica-2025\\Coral_net\\for-r.csv"
output_folder <- "C:\\Costa_rica-2025\\Coral_net\\output\\all-together"
dir.create(file.path(output_folder, "plots"), recursive = TRUE, showWarnings = FALSE)

# ==== LOAD DATA ====
dat <- read.csv(data_path, check.names = FALSE, stringsAsFactors = FALSE)
dat$Site <- factor(dat$Site, levels = c("Papagayo", "Samara"))

num_cols <- names(dat)[sapply(dat, is.numeric)]

long <- dat |>
  pivot_longer(all_of(num_cols), names_to = "Variable", values_to = "Value") |>
  filter(is.finite(Value)) |>
  mutate(
    Var_clean = gsub('"', "", Variable),
    Site_num  = as.numeric(Site)
  )

# ==== FILTER CORALS ====
keep_vars <- c(
  "Pavona spp.",
  "Pocillopora spp.",
  "Porites spp.",
  "Psammocora stellata"
)
long_filt <- long |> filter(Var_clean %in% keep_vars)

# ==== COLORS ====
site_colors <- c("Papagayo" = "#0072B2", "Samara" = "#E69F00")

# ==== LABELS ====
label_html <- c(
  "Pavona spp."         = "<i>Pavona</i> spp.",
  "Pocillopora spp."    = "<i>Pocillopora</i> spp.",
  "Porites spp."        = "<i>Porites</i> spp.",
  "Psammocora stellata" = "<i>Psammocora stellata</i>"
)

# ==== IQR BOX (Q1–Q3 + MEDIAN) ====
iqr_df <- long_filt |>
  group_by(Var_clean, Site, Site_num) |>
  summarise(
    q1  = quantile(Value, 0.25, na.rm = TRUE),
    q3  = quantile(Value, 0.75, na.rm = TRUE),
    med = median(Value, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    xmin = Site_num - 0.35,
    xmax = Site_num + 0.35
  )

# ==== MEAN + SD (ONE-SIDED; UPWARD ONLY) ====
sd_df <- long_filt |>
  group_by(Var_clean, Site, Site_num) |>
  summarise(
    n    = sum(is.finite(Value)),
    mean = mean(Value, na.rm = TRUE),
    sd   = sd(Value, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    sd   = ifelse(is.finite(sd) & n > 1, sd, 0),
    y_top = mean + sd
  )

# ==== SIGNIFICANCE (Kruskal–Wallis) ====
stats_df <- long_filt |>
  group_by(Var_clean) |>
  kruskal_test(Value ~ Site) |>
  mutate(
    sig = case_when(
      p <= 0.001 ~ "***",
      p <= 0.01  ~ "**",
      p <= 0.05  ~ "*",
      TRUE ~ ""
    )
  )

range_df <- long_filt |>
  group_by(Var_clean) |>
  summarise(
    ymax = max(Value, na.rm = TRUE),
    ymin = min(Value, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(rng = ymax - ymin)

lab_df <- stats_df |>
  left_join(range_df, by = "Var_clean") |>
  filter(sig != "") |>
  mutate(
    x = 1.5,
    y = ymax - 0.08 * rng
  )

# ==== PLOT ====
p <- ggplot(long_filt, aes(x = Site_num, y = Value)) +
  
  # IQR box
  geom_rect(
    data = iqr_df,
    aes(xmin = xmin, xmax = xmax, ymin = q1, ymax = q3, fill = Site),
    inherit.aes = FALSE,
    color = "black",
    linewidth = 0.9
  ) +
  
  # median
  geom_segment(
    data = iqr_df,
    aes(x = xmin, xend = xmax, y = med, yend = med),
    inherit.aes = FALSE,
    color = "black",
    linewidth = 1
  ) +
  
  # raw points
  geom_jitter(
    width = 0.10,
    size = 2,
    alpha = 0.9,
    color = "black"
  ) +
  
  # SD vertical line ONLY (mean → mean+SD)
  geom_segment(
    data = sd_df,
    aes(x = Site_num, xend = Site_num, y = mean, yend = y_top),
    inherit.aes = FALSE,
    linewidth = 1,
    color = "black"
  ) +
  
  # SD top cap ONLY
  geom_segment(
    data = sd_df,
    aes(
      x = Site_num - 0.09, xend = Site_num + 0.09,
      y = y_top, yend = y_top
    ),
    inherit.aes = FALSE,
    linewidth = 1,
    color = "black"
  ) +
  
  # mean point
  geom_point(
    data = sd_df,
    aes(x = Site_num, y = mean),
    inherit.aes = FALSE,
    size = 4,
    color = "red"
  ) +
  
  # significance stars
  geom_text(
    data = lab_df,
    aes(x = x, y = y, label = sig),
    inherit.aes = FALSE,
    size = 16,
    fontface = "bold"
  ) +
  
  facet_wrap(
    ~Var_clean,
    scales = "free_y",
    labeller = as_labeller(label_html)
  ) +
  
  scale_fill_manual(values = site_colors) +
  scale_x_continuous(breaks = c(1, 2), labels = c("Papagayo", "Samara")) +
  coord_cartesian(ylim = c(0, NA)) +
  labs(x = NULL, y = "Benthic cover (%)") +
  
  theme_classic(base_size = 22) +
  theme(
    # ✅ remove the black box around facet titles
    strip.background = element_blank(),
    strip.text = element_markdown(size = 28),
    strip.placement = "outside",
    
    # ✅ bigger axes + legend
    axis.text = element_text(size = 26),
    axis.title = element_text(size = 30),
    
    legend.position = "bottom",
    legend.text  = element_text(size = 30),
    legend.title = element_text(size = 24),
    
    # keep x labels hidden (as you did before)
    axis.text.x  = element_blank(),
    axis.ticks.x = element_blank(),
    
    # keep panel border
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.9)
  ) +
  
  # legend: solid squares only
  guides(
    fill = guide_legend(
      override.aes = list(
        shape  = 15,
        colour = NA,
        alpha  = 1,
        size   = 10
      )
    )
  )

# ==== SAVE ====
ggsave(
  file.path(output_folder, "plots", "Corals_only_IQR_mean_SD_upward_clean_noStripBox_bigText.png"),
  p, width = 14, height = 10, dpi = 300
)

library(writexl)

out_stats <- stats_df %>%
  select(Var_clean, statistic, p, sig) %>%
  arrange(Var_clean)

write_xlsx(
  out_stats,
  file.path(output_folder, "results", "Corals_only_KW_stats.xlsx")
)

cat("✅ Done. Plot saved to:", normalizePath(output_folder), "\n")
