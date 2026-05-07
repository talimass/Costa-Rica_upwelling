# ---------- Libraries ----------
library(tidyverse)
library(car)        
library(rstatix)    
library(ggpubr)     
library(writexl)

# ---------- Paths ----------
output_folder <- "C:\\Costa_rica-2025\\Photogrammetry\\Rugosity\\output-R"
data_path     <- "C:\\Costa_rica-2025\\Photogrammetry\\Rugosity\\For-r.csv"

dir.create(file.path(output_folder, "results"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(output_folder, "plots"),   recursive = TRUE, showWarnings = FALSE)

# ---------- Load data ----------
df <- read.csv(data_path, stringsAsFactors = FALSE) %>%
  select(Site, Mean) %>%
  mutate(Site = factor(Site))

# ---------- Assumption checks ----------
shapiro_res <- df %>%
  group_by(Site) %>%
  shapiro_test(Mean) %>%
  ungroup()

lev_obj <- leveneTest(Mean ~ Site, data = df)
lev_df  <- as.data.frame(lev_obj)
levene_p <- lev_df$`Pr(>F)`[1]

normal_ok <- all(shapiro_res$p > 0.05, na.rm = TRUE)
levene_ok <- isTRUE(levene_p > 0.05)
k <- nlevels(df$Site)

# ---------- Choose test ----------
test_used  <- NULL
stat_value <- NA_real_
p_value    <- NA_real_

if (normal_ok && levene_ok) {
  fit <- aov(Mean ~ Site, data = df)
  anova_tab <- summary(fit)[[1]]
  stat_value <- as.numeric(anova_tab["Site", "F value"])
  p_value    <- as.numeric(anova_tab["Site", "Pr(>F)"])
  test_used  <- "ANOVA"
} else {
  if (k == 2) {
    wt <- wilcox_test(Mean ~ Site, data = df, detailed = TRUE)
    stat_value <- wt$statistic
    p_value    <- wt$p
    test_used  <- "Wilcoxon"
  } else {
    kw <- kruskal_test(df, Mean ~ Site)
    stat_value <- kw$statistic
    p_value    <- kw$p
    test_used  <- "Kruskal-Wallis"
  }
}

# ---------- Significance stars ----------
stars <- case_when(
  p_value <= 0.001 ~ "***",
  p_value <= 0.01  ~ "**",
  p_value <= 0.05  ~ "*",
  TRUE             ~ ""
)

# ---------- Save results ----------
summary_df <- data.frame(
  test_used = test_used,
  statistic = round(stat_value, 3),
  p = signif(p_value, 3),
  sig = stars
)

write.csv(shapiro_res, file.path(output_folder, "results", "shapiro_by_site.csv"), row.names = FALSE)
write.csv(lev_df,      file.path(output_folder, "results", "levene_test.csv"),    row.names = FALSE)
write_xlsx(summary_df, file.path(output_folder, "results", "main_test_result.xlsx"))

# ---------- Plot ----------
site_colors <- c("Samara" = "#E69F00", "Papagayo" = "#0072B2")
y_pos <- max(df$Mean, na.rm = TRUE) * 1.15

p <- ggboxplot(df, x = "Site", y = "Mean", fill = "Site",
               add = "jitter", palette = site_colors) +
  stat_summary(fun = mean, geom = "point",
               shape = 20, size = 3.5, color = "red") +
  stat_summary(fun.data = mean_sdl, geom = "errorbar",
               width = 0.18, color = "black") +
  annotate("text", x = 1.5, y = y_pos, label = stars,
           size = 8, fontface = "bold") +
  
  # ❗ הסרת המילה SITE מהציר
  labs(title = "",
       x = NULL,             # <-- אין "Site"
       y = "Mean") +
  
  # ❗ רקע לבן נקי לחלוטין
  theme_classic(base_size = 16) +
  
  theme(
    axis.text.y = element_text(size = 14),
    axis.text.x = element_text(size = 0),  # להעלים טקסט
    axis.title = element_text(size = 16, face = "bold"),
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    
    # אין גריד
    panel.grid = element_blank(),
    
    # אין קווי ציר שחורים
    axis.line = element_blank(),
    
    # מקרא
    legend.position = "bottom",
    legend.text = element_text(size = 14)
  )

ggsave(file.path(output_folder, "plots", "mean_by_site.png"),
       p, width = 7, height = 5, dpi = 300)

print(p)

cat("✅ Done! Files saved to:", normalizePath(output_folder), "\n")
