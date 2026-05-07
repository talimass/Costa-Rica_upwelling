library(tidyverse)
library(rstatix)
library(writexl)
library(ggtext)
library(readr)

# ==========================================================
# PATHS
# ==========================================================
data_path     <- "C:\\Costa_rica-2025\\Coral_net\\for-r-Edit.csv"
output_folder <- "C:\\Costa_rica-2025\\Coral_net\\output\\all-together"

dir.create(file.path(output_folder, "results"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(output_folder, "plots"),   recursive = TRUE, showWarnings = FALSE)

# ==========================================================
# LOAD DATA
# ==========================================================
dat <- read.csv(data_path, check.names = FALSE, stringsAsFactors = FALSE)
names(dat) <- trimws(names(dat))

# enforce order (x=1 Papagayo, x=2 Samara)
dat$Site <- factor(dat$Site, levels = c("Papagayo", "Samara"))

# ==========================================================
# FORCE 'Corals' column numeric (optional)
# ==========================================================
corals_col <- names(dat)[tolower(names(dat)) == "corals"]
if (length(corals_col) == 1) {
  dat[[corals_col]] <- readr::parse_number(as.character(dat[[corals_col]]))
} else if (length(corals_col) == 0) {
  message("⚠️ No column named 'Corals' found (case-insensitive).")
} else {
  stop("More than one column matches 'corals' (case-insensitive). Please keep only one.")
}

# ==========================================================
# LONG FORMAT
# ==========================================================
num_cols   <- names(dat)[sapply(dat, is.numeric)]
pivot_cols <- unique(c(num_cols, corals_col))

long <- dat |>
  pivot_longer(all_of(pivot_cols), names_to = "Variable", values_to = "Value") |>
  filter(is.finite(Value)) |>
  mutate(
    Variable  = trimws(Variable),
    Var_clean = gsub('"', "", Variable),
    Site_num  = as.numeric(Site)
  )

# ==========================================================
# COLORS + LABELS
# ==========================================================
site_colors <- c("Samara" = "#E69F00", "Papagayo" = "#0072B2")

label_html <- c(
  "Pavona spp."         = "<i>Pavona</i> spp.",
  "Pocillopora spp."    = "<i>Pocillopora</i> spp.",
  "Porites spp."        = "<i>Porites</i> spp.",
  "Psammocora stellata" = "<i>Psammocora stellata</i>",
  "Diadema spp."        = "<i>Diadema</i> spp."
)
if (length(corals_col) == 1) label_html[corals_col] <- "Corals"

# ==========================================================
# ⭐ STAR HEIGHT CONTROL — PER VARIABLE (works for CCA / SEA STAR)
# ==========================================================
clean_key <- function(x) {
  x <- as.character(x)
  x <- trimws(x)
  x <- gsub('"', "", x)
  x
}

STAR_DEFAULT_FRAC <- 0.22

STAR_FRAC_MAP <- c(
  "CCA"      = 0.15,
  "Sea star" = 0.12
  # add more if needed:
  # ,"Corals" = 0.25
)

get_star_frac <- function(var_name) {
  key <- clean_key(var_name)
  if (key %in% names(STAR_FRAC_MAP)) {
    as.numeric(STAR_FRAC_MAP[[key]])
  } else {
    STAR_DEFAULT_FRAC
  }
}

# ==========================================================
# RESULTS STORAGE
# ==========================================================
master_main <- list()

# ==========================================================
# LOOP — INDIVIDUAL PLOTS
# Changes applied:
#   1) SD (not SE)
#   2) SD shown UPWARD ONLY (mean -> mean+SD) with TOP CAP ONLY
#   3) Remove strip/title box styling (N/A here; using plot title)
#   4) Increase legend + axis text sizes
# ==========================================================
for (v in unique(long$Variable)) {
  
  sub <- long |> filter(Variable == v)
  
  # ---- statistics ----
  kw <- kruskal_test(sub, Value ~ Site)
  p_value <- kw$p
  
  stars <- case_when(
    p_value <= 0.001 ~ "***",
    p_value <= 0.01  ~ "**",
    p_value <= 0.05  ~ "*",
    TRUE ~ ""
  )
  
  dunn <- dunn_test(sub, Value ~ Site, p.adjust.method = "bonferroni")
  
  safe_name <- gsub("[^A-Za-z0-9_-]", "_", v)
  
  write.csv(kw,   file.path(output_folder, "results", paste0(safe_name, "_kruskal.csv")), row.names = FALSE)
  write.csv(dunn, file.path(output_folder, "results", paste0(safe_name, "_dunn.csv")),    row.names = FALSE)
  
  master_main[[v]] <- data.frame(
    Variable  = v,
    statistic = kw$statistic,
    p         = p_value,
    sig       = stars
  )
  
  # ---- IQR + median ----
  iqr_df <- sub |>
    group_by(Site, Site_num) |>
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
  
  # ---- Mean + SD (UPWARD ONLY) ----
  sd_df <- sub |>
    group_by(Site, Site_num) |>
    summarise(
      n    = sum(is.finite(Value)),
      mean = mean(Value, na.rm = TRUE),
      sd   = sd(Value, na.rm = TRUE),
      .groups = "drop"
    ) |>
    mutate(
      sd    = ifelse(is.finite(sd) & n > 1, sd, 0),
      y_top = mean + sd
    )
  
  # ---- star position (per-variable) ----
  ymin_v <- min(sub$Value, na.rm = TRUE)
  ymax_v <- max(sub$Value, na.rm = TRUE)
  rng_v  <- ymax_v - ymin_v
  y_star <- ymax_v - get_star_frac(v) * rng_v
  
  # ---- plot ----
  p <- ggplot(sub, aes(x = Site_num, y = Value)) +
    
    geom_rect(
      data = iqr_df,
      aes(xmin = xmin, xmax = xmax, ymin = q1, ymax = q3, fill = Site),
      inherit.aes = FALSE,
      color = "black",
      linewidth = 0.9
    ) +
    
    geom_segment(
      data = iqr_df,
      aes(x = xmin, xend = xmax, y = med, yend = med),
      inherit.aes = FALSE,
      color = "black",
      linewidth = 1
    ) +
    
    geom_jitter(width = 0.10, size = 2, color = "black") +
    
    # ✅ SD vertical line ONLY (mean -> mean+SD)
    geom_segment(
      data = sd_df,
      aes(x = Site_num, xend = Site_num, y = mean, yend = y_top),
      inherit.aes = FALSE,
      linewidth = 1,
      color = "black"
    ) +
    
    # ✅ SD top cap ONLY (no lower cap)
    geom_segment(
      data = sd_df,
      aes(x = Site_num - 0.09, xend = Site_num + 0.09, y = y_top, yend = y_top),
      inherit.aes = FALSE,
      linewidth = 1,
      color = "black"
    ) +
    
    geom_point(
      data = sd_df,
      aes(x = Site_num, y = mean),
      inherit.aes = FALSE,
      size = 6,
      color = "red"
    ) +
    
    annotate(
      "text",
      x = 1.5, y = y_star, label = stars,
      size = 14, fontface = "bold"
    ) +
    
    scale_fill_manual(values = site_colors) +
    scale_x_continuous(breaks = c(1, 2), labels = levels(dat$Site)) +
    coord_cartesian(ylim = c(0, NA)) +
    labs(title = clean_key(v), x = NULL, y = "Benthic cover (%)") +
    theme_classic(base_size = 18) +
    theme(
      plot.title = element_text(hjust = 0.5, size = 22),
      legend.position = "bottom",
      
      # ✅ bigger axes + legend
      axis.text  = element_text(size = 20),
      axis.title = element_text(size = 24),
      legend.text  = element_text(size = 22),
      legend.title = element_text(size = 18),
      
      axis.text.x  = element_blank(),
      axis.ticks.x = element_blank(),
      
      panel.border = element_rect(color = "black", fill = NA, linewidth = 0.9)
    ) +
    guides(
      fill = guide_legend(
        override.aes = list(shape = 15, colour = NA, alpha = 1, size = 10)
      )
    )
  
  ggsave(
    file.path(output_folder, "plots", paste0(safe_name, ".png")),
    p, width = 7, height = 5, dpi = 300
  )
}

# ==========================================================
# SUMMARY EXCEL
# ==========================================================
summary_df <- bind_rows(master_main)
write_xlsx(summary_df, file.path(output_folder, "results", "ALL_main_tests.xlsx"))

# ==========================================================
# FACETED MAIN PLOT
# Changes applied:
#   1) SD (not SE)
#   2) SD shown UPWARD ONLY (mean -> mean+SD) with TOP CAP ONLY
#   3) Remove black box around facet titles (strip.background blank)
#   4) Increase legend + axis text sizes a bit
# ==========================================================
ranges_df <- long |>
  group_by(Variable) |>
  summarise(
    ymin = min(Value, na.rm = TRUE),
    ymax = max(Value, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    rng = ymax - ymin,
    star_frac = map_dbl(Variable, get_star_frac),
    y = ymax - star_frac * rng,
    Var_clean = clean_key(Variable),
    x = 1.5
  )

lab_df <- summary_df |>
  filter(sig != "") |>
  left_join(ranges_df |> select(Variable, Var_clean, x, y), by = "Variable")

iqr_all <- long |>
  group_by(Variable, Var_clean, Site, Site_num) |>
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

sd_all <- long |>
  group_by(Variable, Var_clean, Site, Site_num) |>
  summarise(
    n    = sum(is.finite(Value)),
    mean = mean(Value, na.rm = TRUE),
    sd   = sd(Value, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    sd    = ifelse(is.finite(sd) & n > 1, sd, 0),
    y_top = mean + sd
  )

p_all <- ggplot(long, aes(x = Site_num, y = Value)) +
  geom_rect(
    data = iqr_all,
    aes(xmin = xmin, xmax = xmax, ymin = q1, ymax = q3, fill = Site),
    inherit.aes = FALSE,
    color = "black",
    linewidth = 0.9
  ) +
  geom_segment(
    data = iqr_all,
    aes(x = xmin, xend = xmax, y = med, yend = med),
    inherit.aes = FALSE,
    color = "black",
    linewidth = 1
  ) +
  geom_jitter(width = 0.10, size = 1.8, color = "black") +
  
  # ✅ SD vertical line ONLY (mean -> mean+SD)
  geom_segment(
    data = sd_all,
    aes(x = Site_num, xend = Site_num, y = mean, yend = y_top),
    inherit.aes = FALSE,
    linewidth = 1,
    color = "black"
  ) +
  
  # ✅ SD top cap ONLY
  geom_segment(
    data = sd_all,
    aes(x = Site_num - 0.09, xend = Site_num + 0.09, y = y_top, yend = y_top),
    inherit.aes = FALSE,
    linewidth = 1,
    color = "black"
  ) +
  
  geom_point(
    data = sd_all,
    aes(x = Site_num, y = mean),
    inherit.aes = FALSE,
    size = 4,
    color = "red"
  ) +
  geom_text(
    data = lab_df,
    aes(x = x, y = y, label = sig),
    inherit.aes = FALSE,
    size = 13,
    fontface = "bold"
  ) +
  facet_wrap(
    ~Var_clean,
    scales = "free_y",
    labeller = as_labeller(function(x) ifelse(x %in% names(label_html), label_html[x], x))
  ) +
  scale_fill_manual(values = site_colors) +
  scale_x_continuous(breaks = c(1, 2), labels = levels(dat$Site)) +
  coord_cartesian(ylim = c(0, NA)) +
  labs(x = NULL, y = "Benthic cover (%)") +
  theme_classic(base_size = 20) +
  theme(
    legend.position = "bottom",
    
    # ✅ remove black box around facet titles
    strip.text = ggtext::element_markdown(size = 18),
    strip.background = element_blank(),
    
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.9),
    
    # ✅ bigger axes + legend
    axis.text  = element_text(size = 22),
    axis.title = element_text(size = 26),
    legend.text  = element_text(size = 26),
    legend.title = element_text(size = 20),
    
    axis.text.x  = element_blank(),
    axis.ticks.x = element_blank(),
    
    legend.key = element_blank(),
    legend.background = element_blank()
  ) +
  guides(
    fill = guide_legend(
      override.aes = list(shape = 15, colour = NA, alpha = 1, size = 10)
    )
  )

ggsave(
  file.path(output_folder, "plots", "ALL_variables_facet.png"),
  p_all, width = 14, height = 10, dpi = 300
)

cat("✅ Done. SD (upward only, no lower cap) + bigger text + no strip box in facet plot.\n")
