#Packages for data#
library(tidyverse)
library(broom)
library(patchwork)
library(dplyr)
library(data.table)
#FOR PLOTS#
library(ggplot2)
library(ggeffects)
library(ggplotify)
library(wesanderson)
library(ggpubr)
library(grid)
library(effects)
library(readxl)
library(FactoMineR)
library(factoextra)
#FOR STATS#
library(lme4)
library(nlme)
library(lmerTest)
library(r2glmm)
library(sjPlot)
library(MuMIn)
library(gridExtra)
library(sjstats)
library(car)
library(scales)
library(ggeffects)
library(glmmTMB)
library(emmeans)
library(jtools)
library(visreg)
library(multcomp)
library(DHARMa)

#Q1-A Does rainfall exclusion decrease soil moisture availability and properties by a greater extent at forest edges than interiors?

sm_data <- read_excel("soil_moisture.xlsx")

# Summarize, format, and pivot data for the Supplementary table 
sm_summary <- sm_data %>%
  group_by(effect, Treatment, mon) %>%
  summarise(
    mean_se = paste0(
      round(mean(soil_moisture_percent, na.rm = TRUE), 2),
      " ± ",
      round(sd(soil_moisture_percent, na.rm = TRUE) / sqrt(n()), 2)
    ),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = c(effect, Treatment),
    values_from = mean_se
  )

#write.csv(sm_summary, file = "sm_summary.csv")


#################### MOdel for the soil moisture
# Rescale soil_moisture_percent if necessary (assuming original scale is 0-100)
sm_data <- sm_data %>% mutate(soil_moisture_rescaled = soil_moisture_percent / 100)

# Define custom levels for months
month_levels <- c( "March", "April", "May", "June", 
                   "July", "August", "September", "October", "November", "December", "January")

# Convert "mon" to a factor with custom levels
sm_data$mon <- factor(sm_data$mon, levels = month_levels)

# Fit the beta regression model with random slope for Treatment by month
sm_model_beta <- glmmTMB(
  (soil_moisture_rescaled) ~ effect * Treatment +
    (1 | plot_id) + (Treatment | mon),# Random slope for Treatment within mon
  control = glmmTMBControl(optimizer=optim, optArgs=list(method="BFGS")),
  data = sm_data,
  family = beta_family(link="logit")
)

# Model summary
summary(sm_model_beta)
#tuckey test # SI table
tuckey<-lsmeans(sm_model_beta, pairwise ~ Treatment : effect)
tuckey_contrasts_df <- as.data.frame(tuckey$contrasts) 
#write.csv(tuckey_contrasts_df, file="tuckey_smdat.csv")
#lsmeans(sm_model_beta, pairwise ~ effect|Treatment)

ranef(sm_model_beta)  # Extract random effects without back transform


get_model_data(sm_model_beta, type = "eff", terms= c("Treatment", "effect"))
plot_model(sm_model_beta, type = "eff", terms= c("Treatment", "effect"))
plot_model(sm_model_beta, type = "eff", terms= c("effect", "Treatment"))

#month wise
get_model_data(sm_model_beta, type = "pred", terms= c("mon", "Treatment", "effect"),
               pred.type = c("re"),interval = "confidence") %>%
  print(n = Inf)

plot_model(sm_model_beta, type = "pred", terms= c("mon", "Treatment", "effect"),
           pred.type = c("re"),interval = "confidence")
tab_model(sm_model_beta,show.aic = T,show.re.var = F)



#Figure S1 Manuscript; Predicted Soil moisture between forest habitats having paired treatments without months

plot_model(sm_model_beta, type = "pred", terms = c("effect", "Treatment"),
                                  dodge = 0.4, transform = "percent") +
  labs(title = NULL,
       x = "Forest habitat",
       y = "Soil moisture percent/100") +
  scale_y_continuous(limits = c(0, 0.40), breaks = seq(0, 0.40, 0.1), expand = c(0, 0)) +
  # scale_x_discrete(expand = 0.6) + # Reduce gaps between categories
  # scale_x_discrete("Forest habitat", labels=c("Edge", "Interior"))+
  scale_color_manual(values = c("Control" = "blue", "Drought" = "red")) + # Assign colors
  theme_classic() +
  theme(axis.text.x = element_text(size = 10, margin = margin(t = 4)),  # Add margin to move x-axis text down
        axis.text.y = element_text(size = 10),  
        axis.title.x = element_text(size = 14), 
        axis.title.y = element_text(size = 14),
        panel.border = element_rect(colour = "black", fill = NA))

#Figure ~1 Manuscript; Predicted Soil moisture between forest habitats having paired treatments with months

plot_model(sm_model_beta, type = "pred", 
           terms = c("mon", "Treatment", "effect"),
           pred.type = c("re"), interval = "confidence") +
  labs(title = NULL,
       x = "Month",
       y = "Soil moisture percent/100") +
  scale_y_continuous(limits = c(0, 0.40), breaks = seq(0, 0.40, 0.1), expand = c(0, 0)) +
  scale_color_manual(values = c("Control" = "blue", "Drought" = "red")) + # Assign colors
  theme_classic() +
  #facet_wrap(~effect, nrow = 2, scales = "free_x") +  # Two rows for facets
  theme(axis.text.x = element_text(size = 10, angle = 90, hjust = 1, vjust = 0.5), # Rotate x-axis labels
        axis.text.y = element_text(size = 10),  
        axis.title.x = element_text(size = 14), 
        axis.title.y = element_text(size = 14),
        strip.text = element_text(size = 12),  # Adjust facet label size
        panel.border = element_rect(colour = "black", fill = NA))

#Figure 1 Manuscript; Raw Soil moisture between forest habitats having paired treatments with months

# Define custom levels for months
month_levels <- c( "March", "April", "May", "June", 
                   "July", "August", "September", "October", "November", "December", "January")

# Convert "mon" to a factor with custom levels
sm_data$mon <- factor(sm_data$mon, levels = month_levels)
# Define colors for treatments
treatment_colors <- c("Control" = "blue", "Drought" = "red")

ggplot(sm_data, aes(x = mon, y = soil_moisture_percent, color = Treatment)) +
  geom_boxplot(outlier.shape = NA) +
  geom_point(position = position_dodge(width = 0.75)) +
  scale_color_manual(values = treatment_colors) +
  facet_wrap(~ effect, ncol = 1) +
  labs(x="Month", y="Soil Moisture (%)")+
  theme_classic() +
  theme(panel.grid = element_blank(), axis.title = element_text(size = 15)) +
  theme(
    panel.grid = element_blank(),
    axis.title = element_text(size = 20, face = "bold"),  # Bold axis titles and adjust their size
    axis.text.x = element_text(size = 18, angle = 90, vjust = 0.5),  # Adjust size and rotation of x-axis labels
    axis.text.y = element_text(size = 18),  # Adjust size of y-axis labels
    legend.position = "top",
    legend.justification = "center",
    legend.text = element_text(size = 18),  # Adjust size of legend labels
    strip.text = element_text(size = 18)  # Adjust size of faceted labels
  )
#Q1-B Does rainfall exclusion decrease soil moisture availability and properties by a greater extent at forest edges than interiors?
nut_dat <- read_excel("soil_properties.xlsx")

sp_data<-nut_dat%>% dplyr::select(plot_id)

# Subset columns 5 to 13
nut_subset <- nut_dat[, 7:15]

# Run PCA (scaling is usually recommended)
pca_result <- PCA(nut_subset, scale.unit = TRUE, graph = FALSE)
pca_var<-get_pca_var(pca_result)[["coord"]] #### loadings of varibales

# Extract row coordinates properly
pca_coord <- as.data.frame(pca_result$ind$coord)

# Combine with species data
combined_data <- bind_cols(sp_data, pca_coord)


# Select PC1 and PC2 columns
pc1_pc2 <- combined_data %>% dplyr::select(plot_id, Dim.1, Dim.2)

# Merge PC1 and PC2 columns back into the original Traits_Dat table
nut_dat_with_PC <- nut_dat %>% left_join(pc1_pc2, by = "plot_id")

# Figure S4 PCA of soil physical and chemical properties
# Plot PCA variable contribution using specified theme and color
fviz_pca_var(pca_result, 
             col.var = "cos2", 
             gradient.cols = c("red", "skyblue", "blue"),
             labelsize = 5, 
             arrowsize = 1.2,
             repel = TRUE) + 
  labs(title = NULL) + 
  theme_classic() + 
  theme(text = element_text(size = 15),
        axis.title = element_text(size = 15),
        plot.title = NULL) +
  guides(col = guide_colourbar(title = "Loadings"))


#PC1
mod_PC1<-glmmTMB(
  Dim.1 ~ effect * treatment + (1 | plot_id),
  control = glmmTMBControl(optimizer=optim, optArgs=list(method="BFGS")),
  data = nut_dat_with_PC,
  family = gaussian()  # Default; can be omitted for continuous response
)
summary(mod_PC1)
tab_model(mod_PC1, show.aic = T, show.re.var = F)
lsmeans(mod_PC1, pairwise ~ treatment : effect)


#PC2
mod_PC2<-glmmTMB(
  Dim.2 ~ effect * treatment + (1 | plot_id),
  control = glmmTMBControl(optimizer=optim, optArgs=list(method="BFGS")),
  data = nut_dat_with_PC,
  family = gaussian()  # Default; can be omitted for continuous response
)
summary(mod_PC2)
tab_model(mod_PC2, show.aic = T, show.re.var = F)
lsmeans(mod_PC2, pairwise ~ treatment : effect)

#org_c
mod_orgc<-glmmTMB(
  Org_C ~ effect * treatment,
  data = nut_dat_with_PC,
  family = gaussian()  # Default; can be omitted for continuous response
)
tab_model(mod_orgc, show.aic = T, show.re.var = F)
summary(mod_orgc)
lsmeans(mod_orgc, pairwise ~ treatment : effect)

#pH
mod_pH<-glmmTMB(
  pH ~ effect * treatment + (1 | plot_id),
  data = nut_dat_with_PC,
  family = gaussian()  # Default; can be omitted for continuous response
)
tab_model(mod_pH, show.aic = T, show.re.var = F)
lsmeans(mod_pH, pairwise ~ treatment : effect)

#E_cond
mod_E_cond<-glmmTMB(
  E_cond ~ effect * treatment + (1 | plot_id),
  data = nut_dat_with_PC,
  family = gaussian()  # Default; can be omitted for continuous response
)
tab_model(mod_E_cond, show.aic = T, show.re.var = F)
lsmeans(mod_E_cond, pairwise ~ treatment : effect)

#Available_P
mod_Available_P<-glmmTMB(
  Available_P ~ effect * treatment + (1 | plot_id),
  control = glmmTMBControl(optimizer=optim, optArgs=list(method="BFGS")),
  data = nut_dat_with_PC,
  family = gaussian()  # Default; can be omitted for continuous response
)
tab_model(mod_Available_P, show.aic = T, show.re.var = F)
lsmeans(mod_PC2, pairwise ~ treatment : effect)

#Available_K
mod_Available_K<-glmmTMB(
  Available_K ~ effect * treatment + (1 | plot_id),
  data = nut_dat_with_PC,
  family = gaussian()  # Default; can be omitted for continuous response
)
tab_model(mod_Available_K, show.aic = T, show.re.var = F)
lsmeans(mod_PC2, pairwise ~ treatment : effect)

#Available_Zn
mod_Available_Zn<-glmmTMB(
  Available_Zn ~ effect * treatment + (1 | plot_id),
  data = nut_dat_with_PC,
  family = gaussian()  # Default; can be omitted for continuous response
)
tab_model(mod_Available_Zn, show.aic = T, show.re.var = F)
lsmeans(mod_PC2, pairwise ~ treatment : effect)

#Available_Fe
mod_Available_Fe<-glmmTMB(
  Available_Fe ~ effect * treatment + (1 | plot_id),
  data = nut_dat_with_PC,
  family = gaussian()  # Default; can be omitted for continuous response
)
tab_model(mod_Available_Fe, show.aic = T, show.re.var = F)
lsmeans(mod_PC2, pairwise ~ treatment : effect)

#Available_Cu
mod_Available_Cu<-glmmTMB(
  Available_Cu ~ effect * treatment + (1 | plot_id),
  data = nut_dat_with_PC,
  family = gaussian()  # Default; can be omitted for continuous response
)
tab_model(mod_Available_Cu, show.aic = T, show.re.var = F)
lsmeans(mod_PC2, pairwise ~ treatment : effect)

#Available_Mn
mod_Available_Mn<-glmmTMB(
  Available_Mn ~ effect * treatment + (1 | plot_id),
  data = nut_dat_with_PC,
  family = gaussian()  # Default; can be omitted for continuous response
)
tab_model(mod_Available_Mn, show.aic = T, show.re.var = F)
lsmeans(mod_PC2, pairwise ~ treatment : effect)

#Q2 Does the effect of drought on the survival of forest seedlings vary between forest edge vs. interior?
Data <- read_excel("survival_dat.xls")

sur_mod<-glmmTMB(Survival ~ effect * treatment  + 
                   #(1|unique)+ # for repeated measures
                   (1+effect*treatment|species), # random slope model(the effect of treatment and effect can differ among species)
                 family = binomial(link = "logit"),
                 control = glmmTMBControl(optimizer=optim,optArgs=list(method="BFGS")), 
                 data = Data)
summary(sur_mod)
#tuckey test # SI table
tuckey<-lsmeans(sur_mod, pairwise ~ treatment : effect)
tuckey_contrasts_df <- as.data.frame(tuckey$contrasts) 
#write.csv(tuckey_contrasts_df, file="tuckey_survival.csv")

plot_model(sur_mod, type = "eff", terms= c("treatment", "effect"))
plot_model(sur_mod, type = "eff", terms= c("effect", "treatment"))
get_model_data(sur_mod, type = "eff", terms= c("treatment", "effect"))
tab_model(sur_mod)

################### for species wise random slope
get_model_data(sur_mod, type = "pred", terms= c("species", "treatment", "effect"),
               pred.type = c("re"),interval = "confidence", show.p = T,
               p.val = "wald") %>%
  print(n = Inf)

plot_model(sur_mod, type = "pred", terms= c("species","treatment","effect"),
           pred.type = "re",interval = "confidence")


#Figure S3 Mean survival probabilities between the treatmentsat forest habitats

survival_probability<- plot_model(sur_mod, type = "eff", terms= c("effect", "treatment"),dodge = 0.4)+
  labs(title = NULL,
       x = "Forest habitat",
       y = "Survival probability") +
  scale_y_continuous(limits = c(0.20, 1),breaks = seq(0.20, 1, 0.20), expand = c(0, 0)) +
  scale_color_manual(values = c("Control" = "blue", "Drought" = "red")) + # Assign colors
  theme_classic() +
  #facet_wrap(~effect, nrow = 2, scales = "free_x") +  # Two rows for facets
  theme(axis.text.x = element_text(size = 12, hjust = 0.5, vjust = 0.5), # Rotate x-axis labels
        axis.text.y = element_text(size = 12),  
        axis.title.x = element_text(size = 14), 
        axis.title.y = element_text(size = 14),
        strip.text = element_text(size = 12),  # Adjust facet label size
        panel.border = element_rect(colour = "black", fill = NA))

#Figure 2 plot of Survival probability using random effects
plot_model(sur_mod, type = "pred", terms= c("species", "treatment", "effect"),dodge = 0.5,
                             pred.type = c("re"),interval = "confidence")+
  labs(title = NULL,
       x = "Species",
       y = "Survival probability") +
  #scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 20), expand = c(0, 0)) +
  scale_color_manual(values = c("Control" = "blue", "Drought" = "red")) + # Assign colors
  theme_classic() +
  #facet_wrap(~effect, nrow = 2, scales = "free_x") +  # Two rows for facets
  theme(axis.text.x = element_text(size = 12,angle = 90, hjust = 0.5, vjust = 0.5), # Rotate x-axis labels
        axis.text.y = element_text(size = 12),  
        axis.title.x = element_text(size = 14), 
        axis.title.y = element_text(size = 14),
        strip.text = element_text(size = 12),  # Adjust facet label size
        panel.border = element_rect(colour = "black", fill = NA))

#Q3-A Do plant traits mediate drought response of seedlings and does this vary between forest edge and interior?
Data <- read_excel("survival_dat.xls")

# Summarize the data to count the number of survivors for each species in each treatment and effect
survival_summary <- Data %>%
  group_by(species, effect, treatment) %>%
  summarize(survivors = sum(Survival), .groups = 'drop')

# Separate the data into drought and control treatments
drought_data <- survival_summary %>% filter(treatment == "Drought")
control_data <- survival_summary %>% filter(treatment == "Control")

# Merge drought and control data to calculate the ratio
merged_data <- merge(drought_data, control_data, by = c("species", "effect"), suffixes = c("_drought", "_control"))

# Calculate the ratio of drought survivors to control survivors
drought_response_data <- merged_data %>%
  mutate(ratio = survivors_drought / survivors_control) %>%
  dplyr::select(species, effect, ratio)

Traits_Dat <- read_excel("Traits_Dat.xls")

dat <- na.omit(Traits_Dat[, c(2:7)]) # making table of traits for PCA
sp_data<-Traits_Dat%>% dplyr::select(species) # taking species in different object to merge later
#########
pca_result <- prcomp(dat, scale = TRUE)

#Figure S4 PCA of traits
fviz_pca_var(pca_result, col.var = "cos2",
             gradient.cols = c("red","skyblue" ,"blue"),labelsize=5, arrowsize=1.2,
             repel = TRUE)+ # Avoid text overlapping)+
  labs(title = NULL)+ 
  theme_classic()+theme(text = element_text(size = 15),  # Adjust the text size as needed
                        #axis.text = element_text(size = 14),  # Adjust axis text size as needed
                        axis.title = element_text(size = 15),  # Adjust axis title size as needed
                        #axis.line = element_line(size = 1),  # Adjust line thickness as needed
                        #axis.ticks = element_line(size = 1.5),  # Adjust tick thickness as needed
                        plot.title = NULL)+
  guides(col = guide_colourbar(title = "Loadings"))

pca_var<-get_pca_var(pca_result)[["coord"]] #### loadings of varibales

pca_coord<- as.data.frame(pca_result[["x"]]) ### co-ordinates of the rows

# Combine species data with PCA coordinates
combined_data <- bind_cols(sp_data, pca_coord)


#merging traits and pc of traits
# Select PC1 and PC2 columns
pc1_pc2 <- combined_data %>% dplyr::select(species, PC1, PC2)

# Merge PC1 and PC2 columns back into the original Traits_Dat table
Traits_Dat_with_PC <- Traits_Dat %>% left_join(pc1_pc2, by = "species")



# Join traits data (with PC1 and PC2) to the drought response data
traits_drought_response_data <- drought_response_data %>% 
  left_join(Traits_Dat_with_PC, by = "species")


#drought response model, output and plot

# Remove rows with missing data in the columns of interest
traits_drought_response_data <- traits_drought_response_data %>%
  filter(!is.na(PC1))
traits_drought_response_data$effect <- relevel(as.factor(traits_drought_response_data$effect), ref = "Interior")

#Quantile data for all traits
# Select only the trait columns
traits <- c("SSD", "MRSD", "FRSD", "Leaf_Area", "LMA", "LDMC", "PC1", "PC2")

# Apply quantile function to each trait
quantile_results <- sapply(traits_drought_response_data[traits],
                           function(x) quantile(x, probs = c(0.05, 0.95), na.rm = TRUE))

# Transpose for readability
quantile_results <- t(quantile_results)
quantile_results


### With both PCs
PC_models<- glmmTMB(ratio ~ effect*PC1+effect*PC2 + (1+effect| species),
                    family = Gamma(link = "log"),
                    control = glmmTMBControl(optimizer = optim, 
                                             optArgs = list(method = "BFGS")),
                    data = traits_drought_response_data)
summary(PC_models) 


summary(glht(PC_models, linfct = emm(pairwise ~ PC1|effect, at = list(PC1 = c(2.16,-2.57)))))
summary(glht(PC_models, linfct = emm(pairwise ~ PC2|effect, at = list(PC2 = c(2.08,-1.31)))))
#
# Simulate residuals
simulationOutput <- simulateResiduals(fittedModel = PC_models, n = 1000)
# Plot diagnostics
plot(simulationOutput)

# Formal tests
testUniformity(simulationOutput)
testDispersion(simulationOutput)
testZeroInflation(simulationOutput)
contrast(em_PC1, method = "eff", intercation=T, adjust = "Tukey")
emmeans(PC_models, c("PC1", "effect"))
get_model_data(PC_models, type = "eff", terms= c("PC1", "effect"))
plot_model(PC_models, type = "eff", terms= c("PC1", "effect"))
get_model_data(PC_models, type = "eff", terms= c("PC2", "effect"))
plot_model(PC_models, type = "eff", terms= c("PC2", "effect"))
tab_model(PC_models,show.aic = T,show.re.var = F)


traits_drought_response_data$effect <- relevel(as.factor(traits_drought_response_data$effect), ref = "Interior")

### with LMA
LMA_model<- glmmTMB(ratio ~ effect*LMA + (1 +effect| species),
                    family = Gamma(link = "log"),
                    control = glmmTMBControl(optimizer = optim, 
                                             optArgs = list(method = "BFGS")),
                    data = traits_drought_response_data)

#summary of model
summary(LMA_model) 
summary(glht(LMA_model, linfct = emm(pairwise ~ LMA|effect, at = list(LMA = c(0.94,0.29)))))

# Simulate residuals
simulationOutput <- simulateResiduals(fittedModel = LMA_model, n = 1000)
# Plot diagnostics
plot(simulationOutput)

# Formal tests
testUniformity(simulationOutput)
testDispersion(simulationOutput)
testZeroInflation(simulationOutput)
#
get_model_data(LMA_model, type = "eff", terms= c("LMA", "effect"))
plot_model(LMA_model, type = "eff", terms= c("LMA", "effect"))
visreg(LMA_model, "LMA", plot = TRUE, trans = plogis)
tab_model(LMA_model,show.aic = T,show.re.var = F)

traits_drought_response_data$effect <- relevel(as.factor(traits_drought_response_data$effect), ref = "Interior")

### with LDMC
LDMC_model<- glmmTMB(ratio ~ effect*LDMC + (1 +effect| species),
                     family = Gamma(link = "log"),
                     control = glmmTMBControl(optimizer = optim, 
                                              optArgs = list(method = "BFGS")),
                     data = traits_drought_response_data)
#summary of model
summary(LDMC_model)
summary(glht(LDMC_model, linfct = emm(pairwise ~ LDMC|effect, at = list(LDMC = c(0.44,0.18)))))


simulationOutput <- simulateResiduals(fittedModel = LDMC_model, n = 1000)
# Plot diagnostics
plot(simulationOutput)

# Formal tests
testUniformity(simulationOutput)
testDispersion(simulationOutput)
testZeroInflation(simulationOutput)
#
get_model_data(LDMC_model, type = "eff", terms= c("LDMC", "effect"))
plot_model(LDMC_model, type = "eff", terms= c("LDMC", "effect"))
visreg(LDMC_model, "LDMC", plot = TRUE, trans = plogis)
tab_model(LDMC_model, show.aic = T, show.re.var = F)

traits_drought_response_data$effect <- relevel(as.factor(traits_drought_response_data$effect), ref = "Interior")

### with Leaf_area
LA_model<- glmmTMB(ratio ~ effect*Leaf_Area + (1 +effect| species),
                   family = Gamma(link = "log"),
                   control = glmmTMBControl(optimizer = optim, 
                                            optArgs = list(method = "BFGS")),
                   data = traits_drought_response_data)
#summary of model
summary(LA_model)
summary(glht(LA_model, linfct = emm(pairwise ~ Leaf_Area|effect, at = list(Leaf_Area = c(133.40,29.66)))))

simulationOutput <- simulateResiduals(fittedModel = LA_model, n = 1000)
# Plot diagnostics
plot(simulationOutput)

# Formal tests
testUniformity(simulationOutput)
testDispersion(simulationOutput)
testZeroInflation(simulationOutput)
#
get_model_data(LA_model, type = "eff", terms= c("Leaf_Area", "effect"))
plot_model(LA_model, type = "eff", terms= c("Leaf_Area", "effect"))
visreg(LA_model, "Leaf_Area", plot = TRUE, trans = plogis)
tab_model(LA_model,show.aic = T,show.re.var = F)

traits_drought_response_data$effect <- relevel(as.factor(traits_drought_response_data$effect), ref = "Interior")

### with SSD
SSD_model<- glmmTMB(ratio ~ effect*SSD + (1 +effect| species),
                    family = Gamma(link = "log"),
                    control = glmmTMBControl(optimizer = optim, 
                                             optArgs = list(method = "BFGS")),
                    data = traits_drought_response_data)
#summary of model
summary(SSD_model)
summary(glht(SSD_model, linfct = emm(pairwise ~ SSD|effect, at = list(SSD = c(0.78,0.25)))))

simulationOutput <- simulateResiduals(fittedModel = SSD_model, n = 1000)
# Plot diagnostics
plot(simulationOutput)

# Formal tests
testUniformity(simulationOutput)
testDispersion(simulationOutput)
testZeroInflation(simulationOutput)
#
summary(glht(SSD_model, linfct = emm(pairwise ~ SSD|effect, at = list(SSD = c(0.78,0.25)))))
get_model_data(SSD_model, type = "eff", terms= c("SSD", "effect"))
plot_model(SSD_model, type = "eff", terms= c("SSD", "effect"))
visreg(SSD_model, "SSD", plot = TRUE, trans = plogis)
tab_model(SSD_model,show.aic = T,show.re.var = F)


traits_drought_response_data$effect <- relevel(as.factor(traits_drought_response_data$effect), ref = "Interior")

### with MRSD
MRSD_model<- glmmTMB(ratio ~ effect*MRSD + (1 +effect| species),
                     family = Gamma(link = "log"),
                     control = glmmTMBControl(optimizer = optim, 
                                              optArgs = list(method = "BFGS")),
                     data = traits_drought_response_data)
#summary of model
summary(MRSD_model)
summary(glht(MRSD_model, linfct = emm(pairwise ~ MRSD|effect, at = list(MRSD = c(0.66,0.06)))))


simulationOutput <- simulateResiduals(fittedModel = MRSD_model, n = 1000)
# Plot diagnostics
plot(simulationOutput)

# Formal tests
testUniformity(simulationOutput)
testDispersion(simulationOutput)
testZeroInflation(simulationOutput)
#
summary(glht(MRSD_model, linfct = emm(pairwise ~ MRSD|effect, at = list(MRSD = c(0.66,0.06)))))
get_model_data(MRSD_model, type = "eff", terms= c("MRSD", "effect"))
plot_model(MRSD_model, type = "eff", terms= c("MRSD", "effect"))
visreg(MRSD_model, "MRSD", plot = TRUE, trans = plogis)
tab_model(MRSD_model,show.aic = T,show.re.var = F)

traits_drought_response_data$effect <- relevel(as.factor(traits_drought_response_data$effect), ref = "Interior")

### with FRSD
FRSD_model<- glmmTMB(ratio ~ effect*FRSD + (1 +effect| species),
                     family = Gamma(link = "log"),
                     control = glmmTMBControl(optimizer = optim, 
                                              optArgs = list(method = "BFGS")),
                     data = traits_drought_response_data)

#summary of model
summary(FRSD_model) 
summary(glht(FRSD_model, linfct = emm(pairwise ~ FRSD|effect, at = list(FRSD = c(0.78,0.25)))))


simulationOutput <- simulateResiduals(fittedModel = FRSD_model, n = 1000)
# Plot diagnostics
plot(simulationOutput)

# Formal tests
testUniformity(simulationOutput)
testDispersion(simulationOutput)
testZeroInflation(simulationOutput)
#
summary(glht(FRSD_model, linfct = emm(pairwise ~ FRSD|effect, at = list(FRSD = c(0.78,0.25)))))
get_model_data(FRSD_model, type = "eff", terms= c("FRSD", "effect"))
plot_model(FRSD_model, type = "eff", terms= c("FRSD", "effect"))
visreg(FRSD_model, "FRSD", plot = TRUE, trans = plogis)
tab_model(FRSD_model,show.aic = T,show.re.var = F)


#all models tab_model
tab_model(PC_models,LMA_model, LDMC_model, LA_model, SSD_model, MRSD_model,FRSD_model,
          show.aic = T,show.re.var = F)

################################## Visualizing the drought response data

PC1<-plot_model(PC_models, type = "eff", terms= c("PC1", "effect"))+
  #geom_point(alpha = 1.3,size = 3) +
  geom_line(aes(y = predicted), size = 1.5,linetype="solid") +
  ggtitle("(a)")+
  scale_colour_manual(values = c("#F0E442", "#009E73"), labels = c("Edge", "Interior"), name = "") +
  scale_fill_manual(values = c("#F0E442", "#009E73"), labels = c("Edge", "Interior"), name = "") +
  theme_classic() +
  labs(x = "PC1",
       y= NULL) +
  #scale_x_continuous(c(-2,2), breaks = seq(-2, 2, by = 1)) +
  theme(
    panel.grid = element_blank(),
    axis.title = element_text(size = 15),
    plot.title = element_text(hjust = 0),
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = "top",
    legend.justification = "centre",
    strip.text.x = element_text(size = 15, face = "bold")
  )
PC2<-plot_model(PC_models, type = "eff", terms= c("PC2", "effect"))+
  #geom_point(alpha = 1.3,size = 3) +
  geom_line(aes(y = predicted), size = 1.5,linetype="solid") +
  ggtitle("(b)")+
  scale_colour_manual(values = c("#F0E442", "#009E73"), labels = c("Edge", "Interior"), name = "") +
  scale_fill_manual(values = c("#F0E442", "#009E73"), labels = c("Edge", "Interior"), name = "") +
  theme_classic() +
  labs(x = "PC2",
       y= NULL) +
  #scale_x_continuous(c(-2,2), breaks = seq(-2, 2, by = 1)) +
  theme(
    panel.grid = element_blank(),
    axis.title = element_text(size = 15),
    plot.title = element_text(hjust = 0),
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = "top",
    legend.justification = "centre",
    strip.text.x = element_text(size = 15, face = "bold")
  )

LMA<-plot_model(LMA_model, type = "eff", terms= c("LMA", "effect"))+
  #geom_point(alpha = 1.3, size=3) +
  ggtitle("(c)")+
  geom_line(aes(y = predicted), size = 1.5,linetype = "dashed") +
  scale_colour_manual(values = c("#F0E442", "#009E73"), labels = c("Edge", "Interior"), name = "") +
  scale_fill_manual(values = c("#F0E442", "#009E73"), labels = c("Edge", "Interior"), name = "") +
  #scale_x_continuous(limits= c(0.2,1), breaks = seq(0.2,1, by = 0.2))+
  theme_classic() +
  labs(title = ,
       x = expression("LMA (g" ~ cm^-2 ~ ")"),
       y = NULL) +
  theme(
    panel.grid = element_blank(),
    axis.title = element_text(size = 15),
    plot.title = element_text(hjust = 0),
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = "top",
    legend.justification = "centre",
    strip.text.x = element_text(size = 15, face = "bold")
  )

LDMC<-plot_model(LDMC_model, type = "eff", terms= c("LDMC", "effect"))+
  #geom_point(alpha = 1.3, size=3) +
  ggtitle("(d)")+
  geom_line(aes(y = predicted), size = 1.5,linetype="solid") +
  #scale_x_continuous(limits= c(0.2,1), breaks = seq(0.2,1, by = 0.2))+
  scale_colour_manual(values = c("#F0E442", "#009E73"), labels = c("Edge", "Interior"), name = "") +
  scale_fill_manual(values = c("#F0E442", "#009E73"), labels = c("Edge", "Interior"), name = "") +
  theme_classic() +
  labs(title = ,
       x = "LDMC",
       y = NULL) +
  theme(
    panel.grid = element_blank(),
    axis.title = element_text(size = 15),
    plot.title = element_text(hjust = 0),
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = "top",
    legend.justification = "centre",
    strip.text.x = element_text(size = 15, face = "bold")
  )

SSD<-plot_model(SSD_model, type = "eff", terms= c("SSD", "effect"))+
  #geom_point(alpha = 1.3, size=3) +
  ggtitle("(f)")+
  geom_line(aes(y = predicted), size = 1.5,linetype="solid") +
  scale_colour_manual(values = c("#F0E442", "#009E73"), labels = c("Edge", "Interior"), name = "") +
  scale_fill_manual(values = c("#F0E442", "#009E73"), labels = c("Edge", "Interior"), name = "") +
  theme_classic() +
  labs(title = ,
       x = expression("SSD (g" ~ cm^-3~")"),
       y = NULL) +
  theme(
    panel.grid = element_blank(),
    axis.title = element_text(size = 15),
    plot.title = element_text(hjust = 0),
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = "top",
    legend.justification = "centre",
    strip.text.x = element_text(size = 15, face = "bold")
  )

LA<-plot_model(LA_model, type = "eff", terms= c("Leaf_Area", "effect"))+
  #geom_point(alpha = 1.3, size=3) +
  ggtitle("(e)")+
  #geom_line(aes(y = predicted), size = 2,linetype="dashed") +
  scale_colour_manual(values = c("#F0E442", "#009E73"), labels = c("Edge", "Interior"), name = "") +
  scale_fill_manual(values = c("#F0E442", "#009E73"), labels = c("Edge", "Interior"), name = "") +
  theme_classic() +
  labs(title = ,
       x = expression("Leaf Area (cm"^2*")"),
       y = NULL) +
  theme(
    panel.grid = element_blank(),
    axis.title = element_text(size = 15),
    plot.title = element_text(hjust = 0),
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = "top",
    legend.justification = "centre",
    strip.text.x = element_text(size = 15, face = "bold")
  )
MRSD<-plot_model(MRSD_model, type = "eff", terms= c("MRSD", "effect"))+
  #geom_point(alpha = 1.3, size=3) +
  ggtitle("(g)")+
  #geom_line(aes(y = predicted), size = 1,linetype="solid") +
  scale_colour_manual(values = c("#F0E442", "#009E73"), labels = c("Edge", "Interior"), name = "") +
  scale_fill_manual(values = c("#F0E442", "#009E73"), labels = c("Edge", "Interior"), name = "") +
  theme_classic() +
  labs(title = ,
       x = expression("MRSD (g" ~ cm^-3~")"),
       y = NULL) +
  theme(
    panel.grid = element_blank(),
    axis.title = element_text(size = 15),
    plot.title = element_text(hjust = 0),
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = "top",
    legend.justification = "centre",
    strip.text.x = element_text(size = 15, face = "bold")
  )

FRSD<-plot_model(FRSD_model, type = "eff", terms= c("FRSD", "effect"))+
  #geom_point(alpha = 1.3, size=3) +
  ggtitle("(h)")+
  #geom_line(aes(y = predicted), size = 2,linetype="dashed") +
  scale_colour_manual(values = c("#F0E442", "#009E73"), labels = c("Edge", "Interior"), name = "") +
  scale_fill_manual(values = c("#F0E442", "#009E73"), labels = c("Edge", "Interior"), name = "") +
  theme_classic() +
  labs(title = ,
       x = expression("FRSD (g" ~ cm^-3~")"),
       y = NULL) +
  theme(
    panel.grid = element_blank(),
    axis.title = element_text(size = 15),
    plot.title = element_text(hjust = 0),
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = "top",
    legend.justification = "centre",
    strip.text.x = element_text(size = 15, face = "bold")
  )
Drought_response<- ggarrange(PC1,PC2,LMA,LDMC,LA,SSD,MRSD,FRSD,
                             common.legend = T,
                             #labels = paste(letters[1:8], ".", sep=""),
                             font.label = list(size=15),
                             vjust = 0.5, ncol = 2,nrow=4)


Drought_response<-annotate_figure(Drought_response,
                                  #top = text_grob("Visualizing len", color = "red", face = "bold", size = 14),
                                  left = text_grob("Drought Response", 
                                                   color = "black", rot = 90,size = 15),
                                  #right = "I'm done, thanks :-)!",
                                  #fig.lab = "Figure 1", fig.lab.face = "bold"
)

#Figure 2 Triats mediating drought response at edge but not interior
print(Drought_response)

#Q3-B Do plant traits mediate individual survival of seedlings and does this vary between treatments of forest edge and interior?

Data <- read_excel("survival_dat.xls")



#making trait space to add pc1 and pc2 for the traits table
Traits_Dat <- read_excel("Traits_Dat.xls")

dat <- na.omit(Traits_Dat[, c(2:7)]) # making table of traits for PCA
sp_data<-Traits_Dat%>% dplyr::select(species) # taking species in different object to merge later
#########
pca_result <- prcomp(dat, scale = TRUE)

pca_var<-get_pca_var(pca_result)[["coord"]] #### loadings of varibales

pca_coord<- as.data.frame(pca_result[["x"]]) ### co-ordinates of the rows

# Combine species data with PCA coordinates
combined_data <- bind_cols(sp_data, pca_coord)


#merging traits and pc of traits
# Select PC1 and PC2 columns
pc1_pc2 <- combined_data %>% dplyr::select(species, PC1, PC2)

# Merge PC1 and PC2 columns back into the original Traits_Dat table
Traits_Dat_with_PC <- Traits_Dat %>% left_join(pc1_pc2, by = "species")

# Join traits data (with PC1 and PC2) to the drought response data
traits_survival_data <- Data %>% 
  left_join(Traits_Dat_with_PC, by = "species")

#################################################### with PC1
traits_survival_data$effect <- relevel(as.factor(traits_survival_data$effect), ref = "Interior")


model_PC1 <- glmmTMB(Survival ~ PC1 * effect * treatment  + (1+effect + treatment|species), 
                     family = binomial(link = "logit"),control = glmmTMBControl(optimizer=optim, 
                                                                                optArgs=list(method="BFGS")), 
                     data = traits_survival_data) # there is significance b/w edge control to interior drought

model_PC2 <- glmmTMB(Survival ~ PC2 * effect * treatment  + (1+effect + treatment|species), 
                     family = binomial(link = "logit"),control = glmmTMBControl(optimizer=optim, 
                                                                                optArgs=list(method="BFGS")), 
                     data = traits_survival_data) # there is significance b/w edge control to interior drought

model_LMA <- glmmTMB(Survival ~ LMA * effect * treatment  + (1+effect + treatment|species), 
                     family = binomial(link = "logit"),control = glmmTMBControl(optimizer=optim, 
                                                                                optArgs=list(method="BFGS")), 
                     data = traits_survival_data)

model_LDMC <- glmmTMB(Survival ~ LDMC * effect * treatment  + (1+effect + treatment|species), 
                      family = binomial(link = "logit"),control = glmmTMBControl(optimizer=optim, 
                                                                                 optArgs=list(method="BFGS")), 
                      data = traits_survival_data)

model_LA <- glmmTMB(Survival ~ Leaf_Area * effect * treatment  + (1+effect + treatment|species), 
                    family = binomial(link = "logit"),control = glmmTMBControl(optimizer=optim, 
                                                                               optArgs=list(method="BFGS")), 
                    data = traits_survival_data)

model_SSD <- glmmTMB(Survival ~ SSD * effect * treatment  + (1+effect + treatment|species), 
                     family = binomial(link = "logit"),control = glmmTMBControl(optimizer=optim, 
                                                                                optArgs=list(method="BFGS")), 
                     data = traits_survival_data)

model_MRSD <- glmmTMB(Survival ~ MRSD * effect * treatment  + (1+effect + treatment|species), 
                      family = binomial(link = "logit"),control = glmmTMBControl(optimizer=optim, 
                                                                                 optArgs=list(method="BFGS")), 
                      data = traits_survival_data)

model_FRSD <- glmmTMB(Survival ~ FRSD * effect * treatment  + (1+effect + treatment|species), 
                      family = binomial(link = "logit"),control = glmmTMBControl(optimizer=optim, 
                                                                                 optArgs=list(method="BFGS")), 
                      data = traits_survival_data)

#summary of model
summary(model_PC1) 
summary(model_PC2)
summary(model_LMA)
summary(model_LDMC)
summary(model_LA)
summary(model_SSD)
summary(model_MRSD)
summary(model_FRSD)


#Visualization
a<-plot_model(model_PC1, type = "pred", terms = c("PC1", "treatment", "effect"), show.data = TRUE) +
  ggtitle("(a)") + 
  xlab("PC1") + 
  ylab(NULL) +
  geom_smooth(se = FALSE, linetype = 1, size = 2) +
  scale_colour_manual(values = c("#56B4E9", "#D55E00"), labels = c("Control", "Drought"), name = "Treatment") +
  scale_fill_manual(values = c("#56B4E9", "#D55E00"), labels = c("Control", "Drought"), name = "Treatment") +
  theme_classic() +  
  theme(
    panel.grid = element_blank(),
    axis.title = element_text(size = 15),
    plot.title = element_text(hjust = 0),
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = "top",
    legend.justification = "centre",
    strip.text.x = element_text(size = 15, face = "bold")
  )

b<-plot_model(model_PC2, type = "pred", terms = c("PC2", "treatment", "effect"), show.data = TRUE) +
  ggtitle("(b)") + 
  xlab("PC2") + 
  ylab(NULL) +
  geom_smooth(se = FALSE, linetype = 1, size = 2) +
  scale_colour_manual(values = c("#56B4E9", "#D55E00"), labels = c("Control", "Drought"), name = "Treatment") +
  scale_fill_manual(values = c("#56B4E9", "#D55E00"), labels = c("Control", "Drought"), name = "Treatment") +
  theme_classic() +  
  theme(
    panel.grid = element_blank(),
    axis.title = element_text(size = 15),
    plot.title = element_text(hjust = 0),
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = "top",
    legend.justification = "centre",
    strip.text.x = element_text(size = 15, face = "bold")
  )

c<-plot_model(model_LMA, type = "pred", terms = c("LMA", "treatment", "effect"), show.data = TRUE) +
  ggtitle("(c)") + 
  xlab("LMA") + 
  ylab(NULL) +
  geom_smooth(se = FALSE, linetype = 1, size = 2) +
  scale_colour_manual(values = c("#56B4E9", "#D55E00"), labels = c("Control", "Drought"), name = "Treatment") +
  scale_fill_manual(values = c("#56B4E9", "#D55E00"), labels = c("Control", "Drought"), name = "Treatment") +
  theme_classic() +  
  theme(
    panel.grid = element_blank(),
    axis.title = element_text(size = 15),
    plot.title = element_text(hjust = 0),
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = "top",
    legend.justification = "centre",
    strip.text.x = element_text(size = 15, face = "bold")
  )
d<-plot_model(model_LDMC, type = "pred", terms = c("LDMC", "treatment", "effect"), show.data = TRUE) +
  ggtitle("(d)") + 
  xlab("LDMC") + 
  ylab(NULL) +
  geom_smooth(se = FALSE, linetype = 1, size = 2) +
  scale_colour_manual(values = c("#56B4E9", "#D55E00"), labels = c("Control", "Drought"), name = "Treatment") +
  scale_fill_manual(values = c("#56B4E9", "#D55E00"), labels = c("Control", "Drought"), name = "Treatment") +
  theme_classic() +  
  theme(
    panel.grid = element_blank(),
    axis.title = element_text(size = 15),
    plot.title = element_text(hjust = 0),
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = "top",
    legend.justification = "centre",
    strip.text.x = element_text(size = 15, face = "bold")
  )
e<-plot_model(model_LA, type = "pred", terms = c("Leaf_Area", "treatment", "effect"), show.data = TRUE) +
  ggtitle("(e)") + 
  xlab("Leaf_Area") + 
  ylab(NULL) +
  geom_smooth(se = FALSE, linetype = 1, size = 2) +
  scale_colour_manual(values = c("#56B4E9", "#D55E00"), labels = c("Control", "Drought"), name = "Treatment") +
  scale_fill_manual(values = c("#56B4E9", "#D55E00"), labels = c("Control", "Drought"), name = "Treatment") +
  theme_classic() +  
  theme(
    panel.grid = element_blank(),
    axis.title = element_text(size = 15),
    plot.title = element_text(hjust = 0),
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = "top",
    legend.justification = "centre",
    strip.text.x = element_text(size = 15, face = "bold")
  )
f<-plot_model(model_SSD, type = "pred", terms = c("SSD", "treatment","effect"), show.data = TRUE) +
  ggtitle("(f)") + 
  xlab("SSD") + 
  ylab(NULL) +
  geom_smooth(se = FALSE, linetype = 1, size = 2) +
  scale_colour_manual(values = c("#56B4E9", "#D55E00"), labels = c("Control", "Drought"), name = "Treatment") +
  scale_fill_manual(values = c("#56B4E9", "#D55E00"), labels = c("Control", "Drought"), name = "Treatment") +
  theme_classic() +  
  theme(
    panel.grid = element_blank(),
    axis.title = element_text(size = 15),
    plot.title = element_text(hjust = 0),
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = "top",
    legend.justification = "centre",
    strip.text.x = element_text(size = 15, face = "bold")
  )
g<-plot_model(model_MRSD, type = "pred", terms = c("MRSD", "treatment", "effect"), show.data = TRUE) +
  ggtitle("(g)") + 
  xlab("MRSD") + 
  ylab(NULL) +
  geom_smooth(se = FALSE, linetype = 1, size = 2) +
  scale_colour_manual(values = c("#56B4E9", "#D55E00"), labels = c("Control", "Drought"), name = "Treatment") +
  scale_fill_manual(values = c("#56B4E9", "#D55E00"), labels = c("Control", "Drought"), name = "Treatment") +
  theme_classic() +  
  theme(
    panel.grid = element_blank(),
    axis.title = element_text(size = 15),
    plot.title = element_text(hjust = 0),
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = "top",
    legend.justification = "centre",
    strip.text.x = element_text(size = 15, face = "bold")
  )
h<-plot_model(model_FRSD, type = "pred", terms = c("FRSD", "treatment", "effect"), show.data = TRUE) +
  ggtitle("(h)") + 
  xlab("FRSD") + 
  ylab(NULL) +
  geom_smooth(se = FALSE, linetype = 1, size = 2) +
  scale_colour_manual(values = c("#56B4E9", "#D55E00"), labels = c("Control", "Drought"), name = "Treatment") +
  scale_fill_manual(values = c("#56B4E9", "#D55E00"), labels = c("Control", "Drought"), name = "Treatment") +
  theme_classic() +  
  theme(
    panel.grid = element_blank(),
    axis.title = element_text(size = 15),
    plot.title = element_text(hjust = 0),
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = "top",
    legend.justification = "centre",
    strip.text.x = element_text(size = 15, face = "bold")
  )

traits_survival<- ggarrange(a,b,c,d,e,f,g,h,
                            common.legend = T,
                            #labels = paste(letters[1:8], ".", sep=""),
                            font.label = list(size=15),
                            vjust = 0.5, ncol = 2, nrow = 4)


traits_survival<-annotate_figure(traits_survival,
                                 #top = text_grob("Visualizing len", color = "red", face = "bold", size = 14),
                                 left = text_grob("Survival Probability", color = "black", rot = 90,size = 15),
                                 #right = "I'm done, thanks :-)!",
                                 #fig.lab = "Figure 1", fig.lab.face = "bold"
)

print(traits_survival)
