# title: "Function groups CR upwelling"
# author: "Tali Mass"
# date: "15/05/2026"

######Convert raw data from wide to long format

library(readxl)
library(dplyr)
library(tidyr)
library(readr)

windowsFonts("Times New Roman" = windowsFont("Times New Roman"))

# Load Book2
book2 <- read_excel("C:/Costa_rica-2025/Coral_net/output/new/Transact_raw data.xlsx")

id_cols <- c("Site", "Transect", "PIC")

book2_converted <- book2 %>%
  pivot_longer(
    cols = -all_of(id_cols),
    names_to = "LABEL",
    values_to = "Count"
  ) %>%
  filter(Count > 0) %>%
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
    Live_Non_live = ifelse(GROUP %in% c("Hard Coral", "Algae", "CCA", "Seagrass"), "Live", "Non-live")
  ) %>%
  select(PIC, SITE, GROUP, LABEL, Live_Non_live, Count)

write_csv(book2_converted, "Book2_converted.csv")
write_csv(book2_converted, "C:/Costa_rica-2025/Coral_net/output/new/OUTPUT/Book2_converted.csv")


#########################

library(tidyverse)
library(scales)

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
data_path   <- "C:/Costa_rica-2025/Coral_net/output/new/OUTPUT/Annotatations_FG.csv"
output_path <- "C:/Costa_rica-2025/Coral_net/output/new/OUTPUT"
dir.create(output_path, showWarnings = FALSE, recursive = TRUE)

# Load data
fg <- read_csv(data_path)

fg <- fg %>%
  mutate(
    SITE     = factor(SITE),
    PIC      = factor(PIC),
    TRANSACT = factor(TRANSACT),
    GROUP = case_when(
      GROUP == "Hard Coral"               ~ "Stony corals",
      GROUP == "Algae"                    ~ "Macroalgae",
      GROUP %in% c("Sponge", "Ascidian") ~ "Filter feeders",
      GROUP %in% c("Diadema", "Substrate", "Other", "Unknown") ~ "Others",
      TRUE ~ as.character(GROUP)
    ),
    GROUP = factor(GROUP)
  )

# ── Group order & colours ──────────────────────────────────────────────────────
group_order <- c(
  "Stony corals", "CCA", "Macroalgae", "Soft Coral",
  "Filter feeders", "Others"
)

# Okabe-Ito colorblind-safe palette (blue & orange reserved for sites)
group_colors <- c(
  "Stony corals"   = "#009E73",   # teal green
  "CCA"            = "#CC79A7",   # reddish purple
  "Macroalgae"     = "#56B4E9",   # sky blue (lighter than site blue)
  "Soft Coral"     = "#D55E00",   # vermillion
  "Filter feeders" = "#A67C52",   # muted brown
  "Others"         = "#999999"    # gray
)


###############################################################################
# HORIZONTAL STACKED BAR – one bar per site
###############################################################################

Fun_groups_site <- fg %>%
  group_by(SITE, GROUP) %>%
  summarise(Count = sum(Count, na.rm = TRUE), .groups = "drop") %>%
  group_by(SITE) %>%
  mutate(Percentage = Count / sum(Count)) %>%
  ungroup() %>%
  mutate(GROUP = factor(GROUP, levels = group_order))

horiz_data <- Fun_groups_site %>%
  filter(Percentage > 0) %>%
  mutate(
    SITE  = ifelse(as.character(SITE) == "Samara", "Sámara", as.character(SITE)),
    SITE  = factor(SITE),
    GROUP = factor(GROUP, levels = rev(group_order))
  )

p_horiz <- ggplot(horiz_data, aes(y = SITE, x = Percentage, fill = GROUP)) +
  geom_bar(stat = "identity", width = 0.5, color = "black", linewidth = 0.4) +
  scale_x_continuous(labels = percent_format(accuracy = 1), expand = c(0, 0)) +
  scale_fill_manual(values = group_colors, drop = FALSE,
                    guide  = guide_legend(reverse = TRUE)) +
  labs(x = "Mean cover (%)", y = NULL, fill = "Functional group",
       title = "Functional group composition by site",
       tag = "(a)") +
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

print(p_horiz)
ggsave(file.path(output_path, "Functional_groups_horiz.png"), p_horiz,
       width = 11, height = 4, dpi = 300, bg = "white")
ggsave(file.path(output_path, "Functional_groups_horiz.pdf"), p_horiz,
       width = 11, height = 4, bg = "white")


###############################################################################
# STATISTICS – normality → homogeneity → appropriate test
###############################################################################

library(car)

pic_totals <- fg %>%
  group_by(SITE, TRANSACT, PIC) %>%
  summarise(Total_points = sum(Count, na.rm = TRUE), .groups = "drop")

Groups_all <- fg %>%
  group_by(SITE, TRANSACT, PIC, GROUP) %>%
  summarise(Count = sum(Count, na.rm = TRUE), .groups = "drop") %>%
  left_join(pic_totals, by = c("SITE", "TRANSACT", "PIC")) %>%
  mutate(Cov_percent = Count / Total_points * 100)

Groups_all_long <- Groups_all %>%
  select(SITE, TRANSACT, PIC, GROUP, Cov_percent) %>%
  complete(SITE, TRANSACT, PIC, GROUP, fill = list(Cov_percent = 0)) %>%
  mutate(GROUP = factor(GROUP, levels = group_order))

transect_means <- Groups_all_long %>%
  group_by(SITE, TRANSACT, GROUP) %>%
  summarise(Cov_percent = mean(Cov_percent, na.rm = TRUE), .groups = "drop")

sites <- levels(factor(transect_means$SITE))
stopifnot(length(sites) == 2)

stat_results <- transect_means %>%
  group_by(GROUP) %>%
  group_modify(function(d, g) {

    x1 <- d$Cov_percent[d$SITE == sites[1]]
    x2 <- d$Cov_percent[d$SITE == sites[2]]
    n1 <- length(x1); n2 <- length(x2)

    sw1 <- if (n1 >= 3 && length(unique(x1)) > 1) shapiro.test(x1) else list(p.value = NA)
    sw2 <- if (n2 >= 3 && length(unique(x2)) > 1) shapiro.test(x2) else list(p.value = NA)
    both_normal <- !is.na(sw1$p.value) && sw1$p.value > 0.05 &&
                   !is.na(sw2$p.value) && sw2$p.value > 0.05

    lev_p <- tryCatch({
      leveneTest(Cov_percent ~ factor(SITE), data = d)$`Pr(>F)`[1]
    }, error = function(e) NA_real_)
    equal_var <- !is.na(lev_p) && lev_p > 0.05

    if (both_normal) {
      tt <- t.test(x1, x2, var.equal = equal_var)
      tibble(test_used = if (equal_var) "Student t-test" else "Welch t-test",
             statistic = round(tt$statistic, 3), p_value = tt$p.value)
    } else {
      wt <- wilcox.test(x1, x2, exact = FALSE)
      tibble(test_used = "Mann-Whitney U",
             statistic = round(wt$statistic, 3), p_value = wt$p.value)
    }
  }) %>%
  ungroup() %>%
  mutate(
    p_adj_BH    = p.adjust(p_value, method = "BH"),
    significant = p_adj_BH < 0.05
  )

print(stat_results)
write.csv(stat_results,
          file.path(output_path, "Functional_groups_site_statistics.csv"),
          row.names = FALSE)

message("\n✓ All outputs saved to: ", output_path)
