setwd("~/JCT work/CoArc/CoArc Newfoundland/CL for MCMC")
NLRV_variance_data <- read.csv("~/JCT work/CoArc/CoArc Newfoundland/CL for MCMC/NLRV_variance_data.csv")

NL_data<-NLRV_variance_data
NL_data<-NL_data[NL_data$DIV %in% c("2J", "3K", "3L", "3N", "3O"),c("Season", "DIV", "year","GROUP_NAME",
                                                                  "COMMON_NAME", "Fgroup", "WEIGHT_IN_TONS", 
                                                                  "VAR_WEIGHT_TERM")]

ind_spp_allyears_weight<-aggregate (WEIGHT_IN_TONS~GROUP_NAME+COMMON_NAME+DIV+year+Fgroup, data=NL_data, mean)
ind_spp_allyears_var<-aggregate (VAR_WEIGHT_TERM~GROUP_NAME+COMMON_NAME+DIV+year+Fgroup, data=NL_data, mean)

ind_spp_allyears_w<-aggregate(WEIGHT_IN_TONS~year+GROUP_NAME+COMMON_NAME+Fgroup, 
                              data=ind_spp_allyears_weight, sum)
ind_spp_allyears_v<-aggregate(VAR_WEIGHT_TERM~year+GROUP_NAME+COMMON_NAME+Fgroup, 
                              data=ind_spp_allyears_var, sum)

ind_spp_allyears_w<-transform(ind_spp_allyears_w, total_weight=WEIGHT_IN_TONS/495000)
ind_spp_allyears_v<-transform(ind_spp_allyears_v, total_VAR=VAR_WEIGHT_TERM/495000)

var_terms<-ind_spp_allyears_v[, c("VAR_WEIGHT_TERM", "total_VAR")]
ind_sp_weight_var<-cbind(ind_spp_allyears_w, var_terms)
ind_sp_weight_var_prop<-transform(ind_sp_weight_var, var_prop=sqrt(VAR_WEIGHT_TERM)/WEIGHT_IN_TONS)

save(ind_sp_weight_var_prop, file="var_prop_allsp_01.RData")

#NL_stdz_m<-transform(NL_data, STD_WEIGHT=WEIGHT_IN_TONS/SURVEY_AREA_KM2)
#NL_stdz<-NL_stdz_m[NL_stdz_m$DIV %in% c("2J", "3K", "3L", "3N", "3O"),]
sbenth<-ind_sp_weight_var_prop[ind_sp_weight_var_prop$Fgroup %in% c("Small Benthivore"),]
pisc<-ind_sp_weight_var_prop[ind_sp_weight_var_prop$Fgroup %in% c("Piscivore"),]
lbenth<-ind_sp_weight_var_prop[ind_sp_weight_var_prop$Fgroup %in% c("Large Benthivore"),]
mbenth<-ind_sp_weight_var_prop[ind_sp_weight_var_prop$Fgroup %in% c("Medium Benthivore"),]
plank_pisc<-ind_sp_weight_var_prop[ind_sp_weight_var_prop$Fgroup %in% c("PlankPiscivore"),]
plankt<-ind_sp_weight_var_prop[ind_sp_weight_var_prop$Fgroup %in% c("Planktivores"),]
shellfish<-ind_sp_weight_var_prop[ind_sp_weight_var_prop$Fgroup %in% c("Shellfish"),]

pisc_8587<-pisc[pisc$year %in% c(1985, 1986, 1987),]
sbenth_8587<-sbenth[sbenth$year %in% c(1985,1986, 1987),]
mbenth_8587<-mbenth[mbenth$year %in% c(1985, 1986, 1987),]
lbenth_8587<-lbenth[lbenth$year %in% c(1985, 1986, 1987),]
plank_pisc_8587<-plank_pisc[plank_pisc$year %in% c(1985, 1986, 1987),]
plankt_8587<-plankt[plankt$year %in% c(1985, 1986, 1987),]
shellfish_8587<-shellfish[shellfish$year %in% c(1985, 1986, 1987),]

pisc_spp_8587<-aggregate(var_prop~COMMON_NAME, data=pisc_8587, mean)
plank_pisc_spp_8587<-aggregate(var_prop~COMMON_NAME, data=plank_pisc_8587, mean)
lbenth_spp_8587<-aggregate(var_prop~COMMON_NAME, data=lbenth_8587, mean)
mbenth_spp_8587<-aggregate(var_prop~COMMON_NAME, data=mbenth_8587, mean)
sbenth_spp_8587<-aggregate(var_prop~COMMON_NAME, data=sbenth_8587, mean)
plankt_spp_8587<-aggregate(var_prop~COMMON_NAME, data=plankt_8587, mean)
shellfish_spp_8587<-aggregate(var_prop~COMMON_NAME, data=shellfish_8587, mean)

pisc_1315<-pisc[pisc$year %in% c(2013, 2014, 2015),]
sbenth_1315<-sbenth[sbenth$year %in% c(2013, 2014, 2015),]
mbenth_1315<-mbenth[mbenth$year %in% c(2013, 2014, 2015),]
lbenth_1315<-lbenth[lbenth$year %in% c(2013, 2014, 2015),]
plank_pisc_1315<-plank_pisc[plank_pisc$year %in% c(2013, 2014, 2015),]
plankt_1315<-plankt[plankt$year %in% c(2013, 2014, 2015),]
shellfish_1315<-shellfish[shellfish$year %in% c(2013, 2014, 2015),]

pisc_spp_1315<-aggregate(var_prop~COMMON_NAME, data=pisc_1315, mean)
plank_pisc_spp_1315<-aggregate(var_prop~COMMON_NAME, data=plank_pisc_1315, mean)
lbenth_spp_1315<-aggregate(var_prop~COMMON_NAME, data=lbenth_1315, mean)
mbenth_spp_1315<-aggregate(var_prop~COMMON_NAME, data=mbenth_1315, mean)
sbenth_spp_1315<-aggregate(var_prop~COMMON_NAME, data=sbenth_1315, mean)
plankt_spp_1315<-aggregate(var_prop~COMMON_NAME, data=plankt_1315, mean)
shellfish_spp_1315<-aggregate(var_prop~COMMON_NAME, data=shellfish_1315, mean)