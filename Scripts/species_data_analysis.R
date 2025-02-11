####### SPECIES ANALYSIS
options(scipen = 999)


install.packages("FSA")
library(FSA)

fire.data<- read_csv("/Users/talimass/Documents/Documents - MacBook Pro/GitHub/Costa-Rica_upwelling/Data/Samara_anotated _xls.csv")## you need to path this to your data file

#FIRe Data
fire.data$depth <- as_factor(fire.data$depth)
fire_data <- fire.data %>% na.omit()

fire.data$depth <- as_factor(fire.data$depth)

fire_data <- fire.data %>% na.omit()

## thin data
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

str(fire_data$Sigma)
class(fire_data$Sigma)
# Convert character or factor to numeric
fire_data$Sigma <- as.numeric(as.character(fire_data$Sigma))

# Now compute quartiles
quartiles <- quantile(fire_data$Sigma, probs = c(0.25, 0.75), na.rm = TRUE)


IQR_cor <- IQR(fire_data$Sigma)

lower <- quartiles[1] - 1.5*IQR_cor
upper <- quartiles[2] + 1.5*IQR_cor

sigma_no_outlier <- subset(fire_data, fire_data$Sigma >lower & fire_data$Sigma< upper)

quartiles <- quantile(fire_data$Pmax.e.s, probs = c(.25,.75))
IQR_cor <- IQR(fire_data$Pmax.e.s)

lower <- quartiles[1] - 1.5*IQR_cor
upper <- quartiles[2] + 1.5*IQR_cor

Pmax.e.s_no_outlier <- subset(fire_data, fire_data$Pmax.e.s >lower & fire_data$Pmax.e.s< upper)

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

#### TESTING BETWEEN SPECIES

KW_fvfm <- kruskal.test(fv_fm ~ Species, data = fvfm_no_outlier)
KW_fvfm
DT_fvfm <- dunnTest(fvfm_no_outlier$fv_fm ~ fvfm_no_outlier$Species, method = "bonferroni")
DT_fvfm

# sigma stats analysis
KW_sigma<- kruskal.test(Sigma ~ Species, data = sigma_no_outlier)
KW_sigma 
dt_sigma <- dunnTest(sigma_no_outlier$Sigma ~ sigma_no_outlier$Species, method = "bonferroni")
dt_sigma

# p stats analysis
KW_p <- kruskal.test(p ~ Species, data = p_no_outlier)
KW_p
DT_p <- dunnTest(p_no_outlier$p ~ p_no_outlier$Species, method = "bonferroni")
DT_p

#Pmax stats analysis
KW_pmax <- kruskal.test(Pmax.e.s ~ Species, data = Pmax.e.s_no_outlier)
KW_pmax
DT_pmax <- dunnTest(Pmax.e.s_no_outlier$Pmax.e.s ~ Pmax.e.s_no_outlier$Species, method = "bonferroni")
DT_pmax

#Fm stats analysis
KW_Fm <- kruskal.test(Fm ~ Species, data = Pmax.e.s_no_outlier)
KW_Fm
DT_Fm <- dunnTest(Fm_no_outlier$Pmax.e.s ~ Pmax.e.s_no_outlier$Species, method = "bonferroni")
DT_Fm

