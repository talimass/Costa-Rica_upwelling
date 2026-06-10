



# Loading data 
setwd("/Users/talimass/Desktop/Costa-Rica_upwelling/Data")
lc_sf.data<-read.csv('Function groups_Annotatation.csv',header=TRUE, stringsAsFactors = TRUE)
str(lc_sf.data)
levels(lc_sf.data$GROUP)

# because we do not really want to tread depth as continuous value, 
# we will change it to be treated as factor:
lc_sf.data$TRANSACT=as.factor(lc_sf.data$TRANSACT)


############################################
# Community-level benthic assemblage analysis
############################################

library(dplyr)
library(tidyr)
library(vegan)
library(ggplot2)

# Make variables factors
lc_sf.data$SITE <- as.factor(lc_sf.data$SITE)
lc_sf.data$TRANSACT <- as.factor(lc_sf.data$TRANSACT)
lc_sf.data$PIC <- as.factor(lc_sf.data$PIC)

# Create unique sample ID for each photoquadrat
lc_sf.data$SampleID <- paste(lc_sf.data$SITE,
                             lc_sf.data$TRANSACT,
                             lc_sf.data$PIC,
                             sep = "_")

# Summarize benthic groups per photoquadrat
comm_data <- lc_sf.data %>%
  group_by(SampleID, SITE, TRANSACT, PIC, GROUP) %>%
  summarise(Count = sum(Count), .groups = "drop")

# Convert to wide community matrix
comm_wide <- comm_data %>%
  pivot_wider(
    id_cols = c(SampleID, SITE, TRANSACT, PIC),
    names_from = GROUP,
    values_from = Count,
    values_fill = 0
  )

# Metadata
metadata <- comm_wide %>%
  dplyr::select(SampleID, SITE, TRANSACT, PIC)

# Community matrix
comm <- comm_wide %>%
  dplyr::select(-SampleID, -SITE, -TRANSACT, -PIC)

# Convert to relative abundance / proportional cover
comm_rel <- decostand(comm, method = "total")

# Bray-Curtis dissimilarity
bray_dist <- vegdist(comm_rel, method = "bray")

############################################
# Test homogeneity of multivariate dispersion
############################################

disp_site <- betadisper(bray_dist, metadata$SITE)

anova(disp_site)
permutest(disp_site, permutations = 999)

############################################
# PERMANOVA: community differences by location
############################################

permanova_site <- adonis2(
  comm_rel ~ SITE,
  data = metadata,
  method = "bray",
  permutations = 999
)

print(permanova_site)
############################################
# Save PERMANOVA results
############################################

permanova_df <- as.data.frame(permanova_site)

write.csv(
  permanova_df,
  file = "/Users/talimass/Desktop/Costa-Rica_upwelling/PERMANOVA_benthic_assemblage_by_site.csv",
  row.names = TRUE
)

############################################
# NMDS ordination
############################################

set.seed(123)

nmds <- metaMDS(
  comm_rel,
  distance = "bray",
  k = 2,
  trymax = 100
)

nmds$stress

nmds_scores <- as.data.frame(scores(nmds, display = "sites"))

nmds_scores$SITE <- metadata$SITE
nmds_scores$TRANSACT <- metadata$TRANSACT
nmds_scores$PIC <- metadata$PIC

p_nmds <- ggplot(nmds_scores,
                 aes(x = NMDS1,
                     y = NMDS2,
                     colour = SITE)) +
  geom_point(size = 3, alpha = 0.8) +
  stat_ellipse(aes(group = SITE),
               linewidth = 1,
               linetype = 2) +
  scale_colour_manual(
    values = c(
      "Papagayo" = "#2E75B6",  # blue
      "Samara"   = "#E6A500"   # yellow-orange
    )
  ) +
  theme_bw() +
  xlab("NMDS1") +
  ylab("NMDS2") +
  theme(
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    legend.title = element_blank(),
    legend.text = element_text(size = 12),
    panel.grid = element_blank()
  )

print(p_nmds)
############################################
# Save NMDS plot
############################################

ggsave(
  filename = "/Users/talimass/Desktop/Costa-Rica_upwelling/NMDS_benthic_assemblage_by_site.png",
  plot = p_nmds,
  width = 10,
  height = 8,
  dpi = 300
)

ggsave(
  filename = "/Users/talimass/Desktop/Costa-Rica_upwelling/NMDS_benthic_assemblage_by_site.pdf",
  plot = p_nmds,
  width = 10,
  height = 8
)

############################################
# SIMPER: groups contributing to differences
############################################

simper_site <- simper(comm_rel, metadata$SITE)

summary(simper_site)

capture.output(
  simper_site,
  file = "/Users/talimass/Desktop/Costa-Rica_upwelling/SIMPER_benthic_assemblage_by_site.txt"
)

simper_df <- as.data.frame(simper_site[[1]])

write.csv(
  simper_df,
  file = "/Users/talimass/Desktop/Costa-Rica_upwelling/SIMPER_benthic_assemblage_by_site.csv",
  row.names = TRUE
)




