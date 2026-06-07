# title: "Sea urchin density by site"
# author: "Tali Mass"
# date: "15/05/2026"

library(tidyverse)
library(readxl)
library(scales)
library(car)

windowsFonts("Times New Roman" = windowsFont("Times New Roman"))

theme_set(
  theme_bw(base_family = "serif", base_size = 12) +
  theme(
    axis.title   = element_text(family = "serif", size = 13),
    axis.text    = element_text(family = "serif", size = 12),
    plot.tag     = element_text(family = "serif", face = "bold", size = 20),
    panel.grid   = element_blank()
  )
)

# Paths
data_path   <- "C:/Costa_rica-2025/Coral_net/output/new/%cover/Urchin_counts.xlsx"
output_path <- "C:/Costa_rica-2025/Coral_net/output/new/OUTPUT"
dir.create(output_path, showWarnings = FALSE, recursive = TRUE)

# Load data
df <- read_excel(data_path, sheet = "Urchin_counts")

# Transect means → density (ind./m²)
transect_means <- df %>%
  group_by(Site, Location, Transect) %>%
  summarise(Mean_count = mean(Count, na.rm = TRUE), .groups = "drop") %>%
  mutate(
    Density = Mean_count / 0.25,   # quadrat area = 0.25 m²
    Site = factor(Site, levels = c("Papagayo", "Sámara"))
  )

# ── Statistics ────────────────────────────────────────────────────────────────
x_pap <- transect_means$Density[transect_means$Site == "Papagayo"]
x_sam <- transect_means$Density[transect_means$Site == "Sámara"]

sw_pap <- shapiro.test(x_pap)
sw_sam <- shapiro.test(x_sam)
lev    <- leveneTest(Density ~ Site, data = transect_means)

cat("Shapiro-Wilk Papagayo: W =", round(sw_pap$statistic, 4), ", p =", round(sw_pap$p.value, 4), "\n")
cat("Shapiro-Wilk Sámara:   W =", round(sw_sam$statistic, 4), ", p =", round(sw_sam$p.value, 4), "\n")
cat("Levene's test:         F =", round(lev$`F value`[1], 4), ", p =", round(lev$`Pr(>F)`[1], 4), "\n")

both_normal <- sw_pap$p.value > 0.05 & sw_sam$p.value > 0.05
equal_var   <- lev$`Pr(>F)`[1] > 0.05

if (both_normal & equal_var) {
  test_result <- t.test(x_pap, x_sam, var.equal = TRUE)
  test_name   <- "Student t-test"
} else if (both_normal & !equal_var) {
  test_result <- t.test(x_pap, x_sam, var.equal = FALSE)
  test_name   <- "Welch t-test"
} else {
  test_result <- wilcox.test(x_pap, x_sam, exact = FALSE)
  test_name   <- "Mann-Whitney U"
}

cat("Test:", test_name, "\n")
cat("Statistic:", round(test_result$statistic, 4), "\n")
cat("p-value:", test_result$p.value, "\n")

# Significance label
p_val     <- test_result$p.value
sig_label <- case_when(
  p_val < 0.001 ~ "***",
  p_val < 0.01  ~ "**",
  p_val < 0.05  ~ "*",
  TRUE          ~ "ns"
)

# ── Plot ──────────────────────────────────────────────────────────────────────
y_max <- max(transect_means$Density) * 1.08

p_urchin <- ggplot(transect_means, aes(x = Density, y = Site, fill = Site)) +
  geom_boxplot(outlier.shape = NA, linewidth = 0.6, width = 0.5) +
  annotate("segment",
           y = 1, yend = 2,
           x = y_max, xend = y_max,
           linewidth = 0.6) +
  annotate("text",
           y = 1.5, x = y_max * 1.03,
           label = sig_label,
           size = 6, family = "serif", hjust = 0) +
  scale_fill_manual(values = c("Papagayo" = "#0072B2", "Sámara" = "#E69F00")) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(
    y   = NULL,
    x   = expression("Sea urchin density (ind. m"^{-2}*")"),
    tag = "(c)"
  ) +
  theme(
    legend.position   = "none",
    axis.text.y       = element_text(size = 14, face = "bold", color = "black"),
    plot.tag.position = c(0.02, 0.98)
  )

print(p_urchin)
ggsave(file.path(output_path, "Urchin_density.png"), p_urchin,
       width = 7, height = 3.5, dpi = 300, bg = "white")
ggsave(file.path(output_path, "Urchin_density.pdf"), p_urchin,
       width = 7, height = 3.5, bg = "white")

message("\n✓ Saved to: ", output_path)
