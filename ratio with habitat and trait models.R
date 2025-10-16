### With both PCs
PC_models<- glmmTMB(ratio ~ effect*PC1+effect*PC2 + (1+effect| species),
                    family = Gamma(link = "log"),
                    control = glmmTMBControl(optimizer = optim, 
                                             optArgs = list(method = "BFGS")),
                    data = traits_drought_response_data)
summary(PC_models) 


summary(glht(PC_models, linfct = emm(pairwise ~ PC1|effect, at = list(PC1 = c(2.16,-2.57)))))
summary(glht(PC_models, linfct = emm(pairwise ~ PC2|effect, at = list(PC2 = c(2.08,-1.31)))))

### with LMA
LMA_model<- glmmTMB(ratio ~ effect*LMA + (1 +effect| species),
                    family = Gamma(link = "log"),
                    control = glmmTMBControl(optimizer = optim, 
                                             optArgs = list(method = "BFGS")),
                    data = traits_drought_response_data)

#summary of model
summary(LMA_model) 
summary(glht(LMA_model, linfct = emm(pairwise ~ LMA|effect, at = list(LMA = c(0.94,0.29)))))

### with LDMC
LDMC_model<- glmmTMB(ratio ~ effect*LDMC + (1 +effect| species),
                     family = Gamma(link = "log"),
                     control = glmmTMBControl(optimizer = optim, 
                                              optArgs = list(method = "BFGS")),
                     data = traits_drought_response_data)
#summary of model
summary(LDMC_model)
summary(glht(LDMC_model, linfct = emm(pairwise ~ LDMC|effect, at = list(LDMC = c(0.44,0.18)))))

### with Leaf_area
LA_model<- glmmTMB(ratio ~ effect*Leaf_Area + (1 +effect| species),
                   family = Gamma(link = "log"),
                   control = glmmTMBControl(optimizer = optim, 
                                            optArgs = list(method = "BFGS")),
                   data = traits_drought_response_data)
#summary of model
summary(LA_model)
summary(glht(LA_model, linfct = emm(pairwise ~ Leaf_Area|effect, at = list(Leaf_Area = c(133.40,29.66)))))

### with SSD
SSD_model<- glmmTMB(ratio ~ effect*SSD + (1 +effect| species),
                    family = Gamma(link = "log"),
                    control = glmmTMBControl(optimizer = optim, 
                                             optArgs = list(method = "BFGS")),
                    data = traits_drought_response_data)
#summary of model
summary(SSD_model)
summary(glht(SSD_model, linfct = emm(pairwise ~ SSD|effect, at = list(SSD = c(0.78,0.25)))))

### with MRSD
MRSD_model<- glmmTMB(ratio ~ effect*MRSD + (1 +effect| species),
                     family = Gamma(link = "log"),
                     control = glmmTMBControl(optimizer = optim, 
                                              optArgs = list(method = "BFGS")),
                     data = traits_drought_response_data)
#summary of model
summary(MRSD_model)
summary(glht(MRSD_model, linfct = emm(pairwise ~ MRSD|effect, at = list(MRSD = c(0.66,0.06)))))

### with FRSD
#convergenece problem random effect changed from (1+effect|species) to (1|species)
FRSD_model<- glmmTMB(ratio ~ effect*FRSD + (1 | species),
                     family = Gamma(link = "log"),
                     control = glmmTMBControl(optimizer = optim, 
                                              optArgs = list(method = "BFGS")),
                     data = traits_drought_response_data)

#summary of model
summary(FRSD_model) 
summary(glht(FRSD_model, linfct = emm(pairwise ~ FRSD|effect, at = list(FRSD = c(0.78,0.25)))))
