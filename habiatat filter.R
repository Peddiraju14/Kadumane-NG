
######################################### Edge data

edge.traits <- traits_drought_response_data %>%
  filter(effect == "Edge")

#----------------------------With PC1 and PC2

PC_models<- glmmTMB(ratio ~ PC1+PC2 + (1 | species),
                    family = Gamma(link = "log"),
                    control = glmmTMBControl(optimizer = optim, 
                                             optArgs = list(method = "BFGS")),
                    data = edge.traits)
summary(PC_models) 


#----------------------------- With LMA
#model convergence problem and the random effect removed
LMA_model<- glmmTMB(ratio ~ LMA ,
                    family = Gamma(link = "log"),
                    control = glmmTMBControl(
                      optimizer = "nlminb",
                      optCtrl = list(eval.max = 1e5, iter.max = 1e5)
                    ),
                    data = edge.traits)
summary(LMA_model) 


#----------------------------- LDMC

LDMC_model<- glmmTMB(ratio ~ LDMC + (1 | species),
                     family = Gamma(link = "log"),
                     control = glmmTMBControl(optimizer = optim, 
                                              optArgs = list(method = "BFGS")),
                     data = edge.traits)
#summary of model
summary(LDMC_model)

#------------------------------LA
LA_model<- glmmTMB(ratio ~ Leaf_Area + (1 | species),
                   family = Gamma(link = "log"),
                   control = glmmTMBControl(optimizer = optim, 
                                            optArgs = list(method = "BFGS")),
                   data = edge.traits)
#summary of model
summary(LA_model)


#------------------------------SSD
#model convergence problem and the random effect removed
SSD_model<- glmmTMB(ratio ~ SSD ,
                    family = Gamma(link = "log"),
                    control = glmmTMBControl(optimizer = optim, 
                                             optArgs = list(method = "BFGS")),
                    data = edge.traits)
#summary of model
summary(SSD_model) 

#-----------------------------MRSD
MRSD_model<- glmmTMB(ratio ~ MRSD + (1 | species),
                     family = Gamma(link = "log"),
                     control = glmmTMBControl(optimizer = optim, 
                                              optArgs = list(method = "BFGS")),
                     data = edge.traits)
#summary of model
summary(MRSD_model) 

#-----------------------------FRSD

FRSD_model<- glmmTMB(ratio ~FRSD + (1 | species),
                     family = Gamma(link = "log"),
                     control = glmmTMBControl(optimizer = optim, 
                                              optArgs = list(method = "BFGS")),
                     data = edge.traits)

#summary of model
summary(FRSD_model) 

#-------------------- Interior data--------------------------------


interior.traits <- traits_drought_response_data %>%
  filter(effect == "Interior")

#----------------------------With PC1 and PC2

PC_models<- glmmTMB(ratio ~ PC1+PC2 + (1 | species),
                    family = Gamma(link = "log"),
                    control = glmmTMBControl(optimizer = optim, 
                                             optArgs = list(method = "BFGS")),
                    data = interior.traits)
summary(PC_models) 


#----------------------------- With LMA
LMA_model<- glmmTMB(ratio ~ LMA + (1 | species),
                    family = Gamma(link = "log"),
                    control = glmmTMBControl(optimizer = optim, 
                                             optArgs = list(method = "BFGS")),
                    data = interior.traits)
summary(LMA_model) 


#----------------------------- LDMC

LDMC_model<- glmmTMB(ratio ~ LDMC + (1 | species),
                     family = Gamma(link = "log"),
                     control = glmmTMBControl(optimizer = optim, 
                                              optArgs = list(method = "BFGS")),
                     data = interior.traits)
#summary of model
summary(LDMC_model)

#------------------------------LA
LA_model<- glmmTMB(ratio ~ Leaf_Area + (1 | species),
                   family = Gamma(link = "log"),
                   control = glmmTMBControl(optimizer = optim, 
                                            optArgs = list(method = "BFGS")),
                   data = interior.traits)
#summary of model
summary(LA_model)


#------------------------------SSD
SSD_model<- glmmTMB(ratio ~ SSD + (1 | species),
                    family = Gamma(link = "log"),
                    control = glmmTMBControl(optimizer = optim, 
                                             optArgs = list(method = "BFGS")),
                    data = interior.traits)
#summary of model
summary(SSD_model) 

#-----------------------------MRSD
MRSD_model<- glmmTMB(ratio ~ MRSD + (1 | species),
                     family = Gamma(link = "log"),
                     control = glmmTMBControl(optimizer = optim, 
                                              optArgs = list(method = "BFGS")),
                     data = interior.traits)
#summary of model
summary(MRSD_model) 

#-----------------------------FRSD

FRSD_model<- glmmTMB(ratio ~FRSD + (1 | species),
                     family = Gamma(link = "log"),
                     control = glmmTMBControl(optimizer = optim, 
                                              optArgs = list(method = "BFGS")),
                     data = interior.traits)

#summary of model
summary(FRSD_model) 

