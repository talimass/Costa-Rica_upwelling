# Load libraries
library(tidyverse)
library(car)         # for Levene test
library(rstatix)     # for normality and posthoc tests
library(ggpubr)      # for nice plots

# ==== SET OUTPUT FOLDER HERE ====
output_folder <- "C:\\Costa_rica-2025\\Coral_net\\output"

# Create folders
if(!dir.exists(output_folder)) dir.create(output_folder)
if(!dir.exists(file.path(output_folder, "results"))) dir.create(file.path(output_folder, "results"))
if(!dir.exists(file.path(output_folder, "plots"))) dir.create(file.path(output_folder, "plots"))

# Load data
data <- read.csv("C:\\Costa_rica-2025\\Coral_net\\for-r.csv")

# Make sure Site is factor
data$Site <- as.factor(data$Site)

# Convert to long format
df_long <- data %>%
  pivot_longer(-Site, names_to = "Variable", values_to = "Value")

# Keep only numeric values
df_long <- df_long %>% filter(is.numeric(Value))

# Run Kruskal-Wallis + Dunn for all variables
all_results <- list()

for (var in unique(df_long$Variable)) {
  
  cat("Processing:", var, "\n")
  
  subset_data <- df_long %>% filter(Variable == var)
  
  # Check that we have at least 2 groups with >1 value
  if(length(unique(subset_data$Site)) < 2) {
    cat("⚠️ Skipping", var, "- not enough groups\n")
    next
  }
  
  # Kruskal test
  kw <- tryCatch({
    kruskal_test(Value ~ Site, data = subset_data)
  }, error = function(e) NULL)
  
  if (is.null(kw)) {
    cat("⚠️ Skipping", var, "- Kruskal failed\n")
    next
  }
  
  # Dunn post-hoc
  dunn <- tryCatch({
    dunn_test(Value ~ Site, data = subset_data, p.adjust.method = "bonferroni")
  }, error = function(e) NULL)
  
  if (!is.null(kw)) {
    write.csv(
      kw,
      file.path(output_folder, "results", paste0(var, "_kruskal.csv")),
      row.names = FALSE
    )
  }
  if (!is.null(dunn)) {
    write.csv(
      dunn,
      file.path(output_folder, "results", paste0(var, "_dunn.csv")),
      row.names = FALSE
    )
  }
  
  # Save plot only if stats worked
  if (!is.null(dunn)) {
    dunn_sig <- dunn %>% add_xy_position(x = "Site")
    
    p <- ggboxplot(subset_data, x = "Site", y = "Value", fill = "Site",
                   add = "jitter", palette = "jco") +
      stat_summary(fun = mean, geom = "point",
                   shape = 20, size = 3, color = "red") +
      stat_summary(fun.data = mean_sdl, geom = "errorbar",
                   width = 0.2, color = "black") +
      stat_pvalue_manual(dunn_sig, hide.ns = TRUE, tip.length = 0.01) +
      labs(
        title = paste(var, "- Kruskal-Wallis"),
        y = "Value",
        x = ""                 # אין "Site" בציר X
      ) +
      # רקע לבן, בלי גריד, בלי שמות Papagayo/Samara בציר X
      theme_classic(base_size = 14) +
      theme(
        axis.text.x  = element_blank(),
        axis.ticks.x = element_blank(),
        plot.title   = element_text(size = 16, face = "bold", hjust = 0.5),
        axis.title   = element_text(size = 14, face = "bold"),
        legend.position = "none"
      )
    
    # ✅ להציג את הגרף בחלון של R
    print(p)
    
    # לשמור לקובץ כמו קודם
    ggsave(
      filename = file.path(output_folder, "plots", paste0(var, ".png")),
      plot = p, width = 6, height = 4, dpi = 300
    )
  }
}

cat("✅ Done. Check folder:", output_folder, "\n")
