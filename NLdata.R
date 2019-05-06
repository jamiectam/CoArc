install.packages("reshape2")
install.packages("dplyr")
install.packages ("tidyr")
install.packages ("vegan")

library(reshape2)
library(dplyr)
library (tidyr)
library(vegan)

#https://github.com/jamiectam/NLdata.git


NL_2J3KLNO_RV_Data_jct <- read.csv("~/JCT work/CoArc/CoArc Newfoundland/NL_2J3KLNO_RV_Data_jct.csv")
 View(NL_2J3KLNO_RV_Data_jct)
  
NLdata<-NL_2J3KLNO_RV_Data_jct

#total biomass of focal grouping for 2J3KLMO
#species weight for each survey, NAFO region, year
#group species of interest, subdivide by year and div, average by season, sum division, conversion from Engels, 
#average by year, multiply by total area

####To aggregate biomass for 1985-87 model for RV survey FISH
# species_2J3KLNO<-aggregate(WEIGHT_IN_TONS~GROUP_NAME+year+Fgroup, data=NLdata, sum)
# 
# #for individual species
# spp1985a<-species_2J3KLNO[species_2J3KLNO$year %in% c(1985, 1986, 1987),]
# spp1985_1987<-aggregate (WEIGHT_IN_TONS~GROUP_NAME+Fgroup, data=spp1985a, mean)
# 
# #for cod and other length separated species
# size_spp_2J3KLNOb<-aggregate(WEIGHT_IN_TONS~GROUP_NAME+year+Fgroup+Max_Length_cm_ASSIGNED, data=NLdata, sum)
# spp1985b<-species_2J3KLNO[species_2J3KLNO$year %in% c(1985, 1986, 1987),]
# spp1985_small<-spp1985b[which(spp1985b$Max_Length_cm_ASSIGNED <= 35),]
# spp1985_large<-spp1985b[which(spp1985b$Max_Length_cm_ASSIGNED > 35),]
# spp1985_1987_small<-aggregate (WEIGHT_IN_TONS~GROUP_NAME+Fgroup, data=spp1985_small, mean)
# spp1985_1987_large<-aggregate (WEIGHT_IN_TONS~GROUP_NAME+Fgroup, data=spp1985_large, mean)
# 
# ###Extract diet info
# diets1985<-NL_2J3KLNO_diet[NL_2J3KLNO_diet$YEAR %in% c(1985, 1986, 1987),]
# consumption_spp<-diets1985[,c(-1,-2,-3,-4)]
# tconsumption<-mutate(consumption_spp, consumption=sum(consumption_spp))
# vars<-diets1985[,1:4]
# tconsumption<-tconsumption[,'consumption']
# tconsumption<-cbind(vars,tconsumption)
# meandiets1985<-aggregate(PRED_COMM_NAME~YEAR, data=diets1985, mean)

###summarise biomass (tonnes) for CoArc functional groups for each year (1977-2016)
###
#rename dataframe
NLdata<-NL_2J3KLNO_RV_Data_jct

#NL_data<-transform(NLdata, SURVEY_AREA_KM2=SURVEY_AREA_MN2*3.4299)
NL_data<-NLdata[NLdata$DIV %in% c("2J", "3K", "3L", "3N", "3O"),c("Season", "DIV", "year","GROUP_NAME", "COMMON_NAME", 
                                                                  "Fgroup","NUMBER_IN_MILLIONS", "WEIGHT_IN_TONS")]
ind_spp_allyears_div<-aggregate (WEIGHT_IN_TONS~GROUP_NAME+COMMON_NAME+DIV+year+Fgroup, data=NL_data, mean)
ind_spp_allyears_w<-aggregate(WEIGHT_IN_TONS~year+GROUP_NAME+COMMON_NAME+Fgroup, data=ind_spp_allyears_div, sum)
ind_spp_allyears<-transform(ind_spp_allyears_w, total_weight=WEIGHT_IN_TONS/495000)
#NL_stdz_m<-transform(NL_data, STD_WEIGHT=WEIGHT_IN_TONS/SURVEY_AREA_KM2)
#NL_stdz<-NL_stdz_m[NL_stdz_m$DIV %in% c("2J", "3K", "3L", "3N", "3O"),]
sbenth<-ind_spp_allyears_w[ind_spp_allyears_w$Fgroup %in% c("Small Benthivore"),]
pisc<-ind_spp_allyears_w[ind_spp_allyears_w$Fgroup %in% c("Piscivore"),]
lbenth<-ind_spp_allyears_w[ind_spp_allyears_w$Fgroup %in% c("Large Benthivore"),]
mbenth<-ind_spp_allyears_w[ind_spp_allyears_w$Fgroup %in% c("Medium Benthivore"),]
plank_pisc<-ind_spp_allyears_w[ind_spp_allyears_w$Fgroup %in% c("PlankPiscivore"),]
plankt<-ind_spp_allyears_w[ind_spp_allyears_w$Fgroup %in% c("Planktivores"),]
shellfish<-ind_spp_allyears_w[ind_spp_allyears_w$Fgroup %in% c("Shellfish"),]

pisc_8587<-pisc[pisc$year %in% c(1985, 1986, 1987),]
sbenth_8587<-sbenth[sbenth$year %in% c(1985,1986, 1987),]
mbenth_8587<-mbenth[mbenth$year %in% c(1985, 1986, 1987),]
lbenth_8587<-lbenth[lbenth$year %in% c(1985, 1986, 1987),]
plank_pisc_8587<-plank_pisc[plank_pisc$year %in% c(1985, 1986, 1987),]
plankt_8587<-plankt[plankt$year %in% c(1985, 1986, 1987),]
shellfish_8587<-shellfish[shellfish$year %in% c(1985, 1986, 1987),]

pisc_spp_8587<-aggregate(WEIGHT_IN_TONS~GROUP_NAME, data=pisc_8587, mean)
plank_pisc_spp_8587<-aggregate(WEIGHT_IN_TONS~GROUP_NAME, data=plank_pisc_8587, mean)
lbenth_spp_8587<-aggregate(WEIGHT_IN_TONS~GROUP_NAME, data=lbenth_8587, mean)
pisc_spp_8587<-aggregate(WEIGHT_IN_TONS~GROUP_NAME, data=pisc_8587, mean)
pisc_spp_8587<-aggregate(WEIGHT_IN_TONS~GROUP_NAME, data=pisc_8587, mean)
pisc_spp_8587<-aggregate(WEIGHT_IN_TONS~GROUP_NAME, data=pisc_8587, mean)
pisc_spp_8587<-aggregate(WEIGHT_IN_TONS~GROUP_NAME, data=pisc_8587, mean)

pisc_1315<-pisc[pisc$year %in% c(2013, 2014, 2015),]
sbenth_1315<-sbenth[sbenth$year %in% c(2013, 2014, 2015),]
mbenth_1315<-mbenth[mbenth$year %in% c(2013, 2014, 2015),]
lbenth_1315<-lbenth[lbenth$year %in% c(2013, 2014, 2015),]
plank_pisc_1315<-plank_pisc[plank_pisc$year %in% c(2013, 2014, 2015),]
plankt_1315<-plankt[plankt$year %in% c(2013, 2014, 2015),]
shellfish_1315<-shellfish[shellfish$year %in% c(2013, 2014, 2015),]

pisc_spp_1315<-aggregate(WEIGHT_IN_TONS~GROUP_NAME, data=pisc_1315, mean)
plank_pisc_spp_1315<-aggregate(WEIGHT_IN_TONS~GROUP_NAME, data=plank_pisc_1315, mean)

pisc_8587unique<-unique(pisc_8587$GROUP_NAME,pisc_8587$COMMON_NAME)
sbenth_8587unique<-unique(sbenth_8587$COMMON_NAME)
mbenth_8587unique<-unique(mbenth_8587$GROUP_NAME)
lbenth_8587unique<-unique(lbenth_8587$GROUP_NAME)
plank_pisc_8587unique<-unique(plank_pisc_8587$GROUP_NAME)
plankt_8587unique<-unique(plankt_8587$GROUP_NAME)
shellfish_8587unique<-unique(shellfish_8587$GROUP_NAME)

                     
pisc_unique<-unique(pisc$GROUP_NAME)
sbenth_unique<-unique(sbenth$GROUP_NAME)
mbenth_unique<-unique(mbenth$GROUP_NAME)
lbenth_unique<-unique(lbenth$GROUP_NAME)
plank_pisc_unique<-unique(plank_pisc$GROUP_NAME)
plankt_unique<-unique(plankt$GROUP_NAME)
shellfish_unique<-unique(shellfish$GROUP_NAME)

pisc_common_names<-unique(pisc$COMMON_NAME)
sbenth_common_names<-unique(sbenth$COMMON_NAME)
mbenth_common_names<-unique(mbenth$COMMON_NAME)
lbenth_common_names<-unique(lbenth$COMMON_NAME)
plank_pisc_common_names<-unique(plank_pisc$COMMON_NAME)
plankt_common_names<-unique(plankt$COMMON_NAME)
shellfish_common_names<-unique(shellfish$COMMON_NAME)

write.csv(pisc_8587unique, file="pisc_8587unique.csv")
write.csv(sbenth_8587unique, file="sbenth_8587unique.csv")
write.csv(mbenth_8587unique, file="mbenth_8587unique.csv")
write.csv(lbenth_8587unique, file="lbenth_8587unique.csv")
write.csv(plank_pisc_8587unique, file="plank_pisc_8587unique.csv")
write.csv(plankt_8587unique, file="plankt_8587unique.csv")
write.csv(shellfish_8587unique, file="shellfish_8587unique.csv")

write.csv(pisc_unique, file="pisc_unique.csv")
write.csv(sbenth_unique, file="sbenth1_unique.csv")
write.csv(mbenth_unique, file="mbenth_unique.csv")
write.csv(lbenth_unique, file="lbenth_unique.csv")
write.csv(plank_pisc_unique, file="plank_pisc_unique.csv")
write.csv(plankt_unique, file="plankt_unique.csv")
write.csv(shellfish_unique, file="shellfish_unique.csv")

write.csv(pisc_common_names, file="pisc_common_name.csv")
write.csv(sbenth_common_names, file="sbenth_common1_name.csv")
write.csv(mbenth_common_names, file="mbenth_common_name.csv")
write.csv(lbenth_common_names, file="lbenth_common_name.csv")
write.csv(plank_pisc_common_names, file="plank_pisc_common_name.csv")
write.csv(plankt_common_names, file="plankt_common_name.csv")
write.csv(shellfish_common_names, file="shellfish_common_name.csv")

sbenth_t<-aggregate (WEIGHT_IN_TONS~year+Fgroup, data=sbenth, sum)
pisc_t<-aggregate(WEIGHT_IN_TONS~year+GROUP_NAME, data=pisc, sum)
pisc_t8587<-pisc_t[pisc_t$year %in% c(1985, 1986, 1987),]
pisc8587_mean<-aggregate(WEIGHT_IN_TONS~GROUP_NAME, data=pisc_t8587, mean)

sbenth_unique<-unique(sbenth$GROUP_NAME)
pisc_div_unique<-unique(pisc_div$GROUP_NAME)
lbenth_unique<-unique(lbenth$GROUP_NAME)
mbenth_unique<-unique(mbenth$GROUP_NAME)
sbenth_unique<-unique(sbenth$GROUP_NAME)
sbenth_unique<-unique(sbenth$GROUP_NAME)

pisc_div8587<-pisc_div[pisc_div$year %in% c(1985, 1986, 1987),]
white_hake_div<-pisc_div[pisc_div$GROUP_NAME %in% c("UROPHYCIS TENUIS"),]
white_hake_ts<-aggregate(WEIGHT_IN_TONS~year+GROUP_NAME, data=white_hake_div, sum)
#For individual spp
   # ind_spp_allyears_div<-NLdata[,c("Season", "DIV", "year", "GROUP_NAME","Fgroup", "WEIGHT_IN_TONS")]
      #average by season
   # ind_spp_allyears_div<-aggregate (STD_WEIGHT~GROUP_NAME+DIV+year+Fgroup, data=NL_stdz, mean)
      #average by DIV
   # ind_spp_allyears<-aggregate(STD_WEIGHT~year+GROUP_NAME+Fgroup, data=ind_spp_allyears_div, mean)
    #ind_spp_allyearsmean<-transform(ind_spp_allyears, S2.86356+2.77446+2.94642
TD_WEIGHT_mean=STD_WEIGHT/5)
    #for weight in t*km-2
    #ind_spp_allyears<-transform(ind_spp_allyears, total_weight=STD_WEIGHT)
    #ind_spp_allyears<-transform(ind_spp_allyearsmean, total_weight=STD_WEIGHT_mean)
 
#for separated by division
    # ind_spp_allyears_div<-aggregate (STD_WEIGHT~GROUP_NAME+DIV+year+Fgroup, data=NL_stdz, mean)
    # #for weight in t*km-2
    # ind_spp_div<-transform(ind_spp_allyears_div, total_weight=STD_WEIGHT)
    #### For Fgroup
    #fgroup_2J<-ind_spp_allyears[ind_spp_allyears$DIV %in% c("2J"),]
    #fgroup_ts_2J<-aggregate(total_weight~year+Fgroup, data=fgroup_2J, sum)
    #fgroup_3K<-ind_spp_allyears[ind_spp_allyears$DIV %in% c("3K"),]
    #fgroup_ts_3K<-aggregate(total_weight~year+Fgroup, data=fgroup_3K, sum)    
    #fgroup_3L<-ind_spp_allyears[ind_spp_allyears$DIV %in% c("3L"),]
    #fgroup_ts_3L<-aggregate(total_weight~year+Fgroup, data=fgroup_3L, sum)
    #fgroup_3N<-ind_spp_allyears[ind_spp_allyears$DIV %in% c("3N"),]
    #fgroup_ts_3N<-aggregate(total_weight~year+Fgroup, data=fgroup_3N, sum)
    #fgroup_3O<-ind_spp_allyears[ind_spp_allyears$DIV %in% c("3O"),]
    #fgroup_ts_3O<-aggregate(total_weight~year+Fgroup, data=fgroup_3O, sum)
#----------------------------------    
#For Fgroup

fgroup_allyears<-aggregate(total_weight~year+Fgroup, data=ind_spp_allyears, sum)

#create conversion factors
# fgroup_9394<-fgroup_allyears[fgroup_allyears$year %in% c(1993, 1994),]
# fgroup_9697<-fgroup_allyears[fgroup_allyears$year %in% c(1996, 1997),]
# fgroup_cv9394<-aggregate(total_weight~Fgroup, data=fgroup_9394, mean)
# fgroup_cv9697<-aggregate(total_weight~Fgroup, data=fgroup_9697, mean)
# fgroup_cv9697<-fgroup_cv9697[-7,]
# fgroup_cv1<-cbind(fgroup_cv9394, fgroup_cv9697)
# colnames(fgroup_cv1)<-c("Fgroup", "tw_9394", "Fgroup", "tw_9697")
# fgroup_cf<-transform(fgroup_cv1, cf=tw_9697/tw_9394)
# #for coarc groupings (Fgroup-specific spp weight) for 2J3KLNO
# 
# write.csv(fgroup_cf, file="fgroup_cf.csv")
#--------------------------------------------------------------
#To explore groups by div 
haddock.div<-NL_data[NL_data$GROUP_NAME %in% c("MELANOGRAMMUS AEGLEFINUS"),]
haddock.div.early<-haddock.div[haddock.div$year %in% c(1985, 1986, 1987),]
haddock.div.8587<-aggregate(WEIGHT_IN_TONS~DIV, data=haddock.div.early, mean) 
haddock.div.y<-haddock.div[haddock.div$year %in% c(2013, 2014, 2015),]
haddock.div.1315<-aggregate(WEIGHT_IN_TONS~DIV, data=haddock.div.y, mean)

ghalibut.div<-NL_data[NL_data$GROUP_NAME %in% c("REINHARDTIUS HIPPOGLOSSOIDES"),]
ghalibut.div.early<-ghalibut.div[ghalibut.div$year %in% c(1985, 1986, 1987),]
ghalibut.div.8587<-aggregate(WEIGHT_IN_TONS~DIV, data=ghalibut.div.early, mean) 
ghalibut.div.y<-ghalibut.div[ghalibut.div$year %in% c(2013, 2014, 2015),]
ghalibut.div.1315<-aggregate(WEIGHT_IN_TONS~DIV, data=ghalibut.div.y, mean)

aplaice.div<-NL_data[NL_data$GROUP_NAME %in% c("HIPPOGLOSSOIDES PLATESSOIDES"),]
aplaice.div.early<-aplaice.div[aplaice.div$year %in% c(1985, 1986, 1987),]
aplaice.div.8587<-aggregate(WEIGHT_IN_TONS~DIV, data=aplaice.div.early, mean) 
aplaice.div.y<-aplaice.div[aplaice.div$year %in% c(2013, 2014, 2015),]
aplaice.div.1315<-aggregate(WEIGHT_IN_TONS~DIV, data=aplaice.div.y, mean)
#---------------------------------  
  
cod_allyears<-ind_spp_allyears[ind_spp_allyears$GROUP_NAME %in% c("GADUS MORHUA"),]
  cod_weight<-transform(cod_allyears, cod_weight=total_weight)
  cod_w_ts<-cod_weight[,c("year", "cod_weight")]
  
haddock_allyears<-ind_spp_allyears[ind_spp_allyears$GROUP_NAME %in% c("MELANOGRAMMUS AEGLEFINUS"),]
  haddock_weight<-transform(haddock_allyears, haddock_weight=total_weight)
  haddock_w_ts<-haddock_weight[,c("year", "haddock_weight")]
  
thornyskate_allyears<-ind_spp_allyears[ind_spp_allyears$GROUP_NAME %in% c("RAJA RADIATA"),]
  thornyskate_weight<-transform(thornyskate_allyears, thornyskate_weight=total_weight)
  thornyskate_w_ts<-thornyskate_weight[,c("year", "thornyskate_weight")]
  
Ghalibut_allyears<-ind_spp_allyears[ind_spp_allyears$GROUP_NAME %in% c("REINHARDTIUS HIPPOGLOSSOIDES"),]
  Ghalibut_weight<-transform(Ghalibut_allyears, Ghalibut_weight=total_weight)
  Ghalibut_w_ts<-Ghalibut_weight[,c("year", "Ghalibut_weight")]

Gshark_allyears<-ind_spp_allyears[ind_spp_allyears$GROUP_NAME %in% c("SOMNIOSUS MICROCEPHALUS"),]
  Gshark_weight<-transform(Gshark_allyears, Gshark_weight=total_weight)
  Gshark_w_ts<-Gshark_weight[,c("year", "Gshark_weight")]
  
ShakeSaithe_allyears<-ind_spp_allyears[ind_spp_allyears$GROUP_NAME %in% c("MERLUCCIUS BILINEARIS","POLLACHIUS VIRENS"),]
  ShakeSaithe_weight<-aggregate(total_weight~year, data=ShakeSaithe_allyears, sum)
  ShakeSaithe_weight<-transform(ShakeSaithe_weight, ShakeSaithe_weight=total_weight)
  ShakeSaithe_w_ts<-ShakeSaithe_weight[,c("year", "ShakeSaithe_weight")]

  
Redfish_allyears<-ind_spp_allyears[ind_spp_allyears$GROUP_NAME %in% c("SEBASTES MARINUS", "SEBASTES MENTELLA"),]
  Redfish_weight<-aggregate(total_weight~year, data=Redfish_allyears, sum)
  Redfish_w_ts<-transform(Redfish_weight, Redfish_weight=total_weight)
  Redfish_w_ts<-Redfish_w_ts[,c("year", "Redfish_weight")]
  
PolarCod_allyears<-ind_spp_allyears[ind_spp_allyears$GROUP_NAME %in% c("BOREOGADUS SAIDA"),]
  PolarCod_weight<-transform(PolarCod_allyears, PolarCod_weight=total_weight)
  PolarCod_w_ts<-PolarCod_weight[,c("year", "PolarCod_weight")]
  
capelin_allyears<-ind_spp_allyears[ind_spp_allyears$GROUP_NAME %in% c("MALLOTUS VILLOSUS"),]
  capelin_weight<-transform(capelin_allyears, capelin_weight=total_weight)
  capelin_w_ts<-capelin_weight[,c("year", "capelin_weight")]

sandlance_allyears<-ind_spp_allyears[ind_spp_allyears$GROUP_NAME %in% c("AMMODYTES DUBIUS"),]
  sandlance_weight<-transform(sandlance_allyears, sandlance_weight=total_weight)
  sandlance_w_ts<-sandlance_weight[,c("year", "sandlance_weight")]

herring_allyears<-ind_spp_allyears[ind_spp_allyears$GROUP_NAME %in% c("CLUPEA HARENGUS"),]
  herring_weight<-transform(herring_allyears, herring_weight=total_weight)
  herring_w_ts<-herring_weight[,c("year", "herring_weight")]

YTF_allyears<-ind_spp_allyears[ind_spp_allyears$GROUP_NAME %in% c("LIMANDA FERRUGINEA"),]
  YTF_weight<-transform(YTF_allyears, YTF_weight=total_weight)
  YTF_w_ts<-YTF_weight[,c("year", "YTF_weight")]

Aplaice_allyears<-ind_spp_allyears[ind_spp_allyears$GROUP_NAME %in% c("HIPPOGLOSSOIDES PLATESSOIDES"),]
  Aplaice_weight<-transform(Aplaice_allyears, Aplaice_weight=total_weight)
  Aplaice_w_ts<-Aplaice_weight[,c("year", "Aplaice_weight")]

witchflounder_allyears<-ind_spp_allyears[ind_spp_allyears$GROUP_NAME %in% c("GLYPTOCEPHALUS CYNOGLOSSUS"),]
  witchflounder_weight<-transform(witchflounder_allyears, witchflounder_weight=total_weight)
  witchflounder_w_ts<-witchflounder_weight[,c("year", "witchflounder_weight")]
  
Dshanny_allyears<-ind_spp_allyears[ind_spp_allyears$GROUP_NAME %in% c("LUMPENUS MACULATUS"),]
  Dshanny_weight<-transform(Dshanny_allyears, Dshanny_weight=total_weight)
  Dshanny_w_ts<-Dshanny_weight[,c("year", "Dshanny_weight")]
  
snowcrab_allyears<-ind_spp_allyears[ind_spp_allyears$GROUP_NAME %in% c("CHIONOECETES OPILIO"),]
  snowcrab_weight<-transform(snowcrab_allyears, snowcrab_weight=total_weight)
  snowcrab_w_ts<-snowcrab_weight[,c("year", "snowcrab_weight")]
  
shrimp_allyears<-ind_spp_allyears[ind_spp_allyears$GROUP_NAME %in% c("PANDALUS BOREALIS"),]
  shrimp_weight<-aggregate(total_weight~year, data=shrimp_allyears, sum)
  shrimp_w_ts<-transform(shrimp_weight, shrimp_weight=total_weight)
  shrimp_w_ts<-shrimp_w_ts[,c("year", "shrimp_weight")]
  
othershrimp_allyears<-ind_spp_allyears[ind_spp_allyears$GROUP_NAME %in% c("PANDALUS MONTAGUI", "PANDALUS PROPINQUUS"),]
  othershrimp_weight<-aggregate(total_weight~year, data=othershrimp_allyears, sum)
  othershrimp_w_ts<-transform(othershrimp_weight, othershrimp_weight=total_weight)
  othershrimp_w_ts<-othershrimp_w_ts[,c("year", "othershrimp_weight")]
  
longfinhake_allyears<-ind_spp_allyears[ind_spp_allyears$GROUP_NAME %in% c("UROPHYCIS CHESTERI"),]
  longfinhake_weight<-aggregate(total_weight~year, data=longfinhake_allyears, sum)
  longfinhake_w_ts<-transform(longfinhake_weight, longfinhake_weight=total_weight)
  longfinhake_w_ts<-longfinhake_w_ts[,c("year", "longfinhake_weight")]
  
#generate true Fgroup weights for CoArc ecopath model
   
pisc_allyears<-fgroup_allyears[fgroup_allyears$Fgroup %in% c("Piscivore"),]
  pisc_t_allyears<-transform(pisc_allyears, allpisc_weight=total_weight)
  pisc_t<-pisc_t_allyears[,c("year", "allpisc_weight")]

  pisc0<-full_join(pisc_t, cod_w_ts, by= "year")
  pisc01<-full_join(pisc0, Ghalibut_w_ts, by="year")
  pisc02<-full_join(pisc01, ShakeSaithe_w_ts, by="year")
  pisc03<-full_join(pisc02, Gshark_w_ts, by="year")
                              #, Ghalibut_weight, ShakeSaithe_weight)
  pisc03[is.na(pisc03)]<-0
coarc_pisc<-transform(pisc03, other_pisc=allpisc_weight-cod_weight-Ghalibut_weight-ShakeSaithe_weight-Gshark_weight)

#write.csv(coarc_pisc, file="coarc_pisc1.csv")
#---------------------------
 
  plankpisc_allyears<-fgroup_allyears[fgroup_allyears$Fgroup %in% c("PlankPiscivore"),]
  plankpisc_t_allyears<-transform(plankpisc_allyears, plankpisc_weight=total_weight)
  plankpisc_t<-plankpisc_t_allyears[,c("year", "plankpisc_weight")]

  plankpisc0<-full_join(plankpisc_t, Redfish_w_ts, by= "year")
  plankpisc01<-full_join(plankpisc0, PolarCod_w_ts, by="year")
  
plankpisc01[is.na(plankpisc01)]<-0
coarc_plankpisc<-transform(plankpisc01, other_plankpisc=plankpisc_weight-Redfish_weight-PolarCod_weight)
coarc_plankpisc_engles<-coarc_plankpisc*3.073212
#write.csv(coarc_plankpisc, file="coarc_plankpisc1.csv")
#----------------------------------
plank_allyears<-fgroup_allyears[fgroup_allyears$Fgroup %in% c("Planktivores"),]
plank_t_allyears<-transform(plank_allyears, plank_weight=total_weight)
plank_t<-plank_t_allyears[,c("year", "plank_weight")]

plank0<-full_join(plank_t, capelin_w_ts, by= "year")
plank01<-full_join(plank0, sandlance_w_ts, by="year")
plank02<-full_join(plank01, herring_w_ts, by="year")

plank02[is.na(plank02)]<-0
coarc_plank<-transform(plank02, other_plank=plank_weight-capelin_weight-sandlance_weight-herring_weight)

#write.csv(coarc_plank, file="coarc_plank1.csv")
#-------------------------------------------
#For large benthivores (Aplaice, thornyskate, haddock)
  Lbenth_allyears<-fgroup_allyears[fgroup_allyears$Fgroup %in% c("Large Benthivore"),]
Lbenth_t_allyears<-transform(Lbenth_allyears, allLbenth_weight=total_weight)
Lbenth_t<-Lbenth_t_allyears[,c("year", "allLbenth_weight")]

Lbenth0<-full_join(Lbenth_t, Aplaice_w_ts, by= "year")
Lbenth01<-full_join(Lbenth0, thornyskate_w_ts, by="year")
Lbenth02<-full_join(Lbenth01, haddock_w_ts, by="year")

Lbenth02[is.na(Lbenth02)]<-0
coarc_Lbenth<-transform(Lbenth02, other_Lbenth=allLbenth_weight-Aplaice_weight-thornyskate_weight-haddock_weight)  

#write.csv(coarc_Lbenth, file="coarc_Lbenth1.csv")
#--------------------------------------------
  # Medium benthivores (YTF, witch flounder, other M benth)
  Mbenth_allyears<-fgroup_allyears[fgroup_allyears$Fgroup %in% c("Medium Benthivore"),]
Mbenth_t_allyears<-transform(Mbenth_allyears, allMbenth_weight=total_weight)
Mbenth_t<-Mbenth_t_allyears[,c("year", "allMbenth_weight")]

Mbenth0<-full_join(Mbenth_t, YTF_w_ts, by= "year")
Mbenth01<-full_join(Mbenth0, witchflounder_w_ts, by="year")

Mbenth01[is.na(Mbenth01)]<-0
coarc_Mbenth<-transform(Mbenth01, other_Mbenth=allMbenth_weight-YTF_weight-witchflounder_weight)  

#write.csv(coarc_Mbenth, file="coarc_Mbenth1.csv")
#-------------------------------------------------
  #Small benthivores
  Sbenth_allyears<-fgroup_allyears[fgroup_allyears$Fgroup %in% c("Small Benthivore"),]
Sbenth_t_allyears<-transform(Sbenth_allyears, allSbenth_weight=total_weight)
Sbenth_t<-Sbenth_t_allyears[,c("year", "allSbenth_weight")]

Sbenth0<-full_join(Sbenth_t, Dshanny_w_ts, by= "year")

Sbenth0[is.na(Sbenth0)]<-0
coarc_Sbenth<-transform(Sbenth0, other_Sbenth=allSbenth_weight-Dshanny_weight)  

#write.csv(coarc_Sbenth, file="coarc_Sbenth1.csv")
#---------------------------------------------
shell_allyears<-fgroup_allyears[fgroup_allyears$Fgroup %in% c("Shellfish"),]
shell_t_allyears<-transform(shell_allyears, shell_weight=total_weight)
shell_t<-shell_t_allyears[,c("year", "shell_weight")]

shell0<-full_join(shell_t, shrimp_w_ts, by= "year")
shell01<-full_join(shell0, othershrimp_w_ts, by="year")
shell02<-full_join(shell01, snowcrab_w_ts, by="year")

shell02[is.na(shell02)]<-0
coarc_shell<-transform(shell02, other_shell=shell_weight-shrimp_weight-othershrimp_weight-snowcrab_weight)
write.csv(coarc_shell, file="coarc_shell.csv")
#-------------------------------------------------------
# coarc_plankpisc_engles<-coarc_plankpisc*3.109172
# coarc_plank_engles<-coarc_plank*30.602539
# coarc_Sbenth_engles<-coarc_Sbenth*25.473733
# coarc_Mbenth_engles<-coarc_Mbenth*3.593379
# coarc_Lbenth_engles<-coarc_Lbenth*2.448773
# coarc_pisc_engles<-coarc_pisc*2.483807

coarc_fish<-cbind(coarc_pisc, coarc_plankpisc, coarc_plank, coarc_Lbenth, coarc_Mbenth, coarc_Sbenth)
coarc_fish_engles_b<-cbind(coarc_pisc_engles, coarc_plankpisc_engles, coarc_plank_engles, 
                  coarc_Lbenth_engles, coarc_Mbenth_engles, coarc_Sbenth_engles)
coarc_fish85<-coarc_fish[c(15:46),]
coarc_ms85<-coarc_multistanza_tkm2_2j3klno_02[c(9:40),]

write.csv(coarc_fish, file="coarc_fish_b.csv")
write.csv(coarc_fish_engles_b, file="coarc_fish_engles_b.csv")
write.csv(shrimp_w_ts, file="coarc_shrimp_b.csv")
write.csv(snowcrab_w_ts, file="coarc_snowcrab_b.csv")

------------------------
  #individual spp conversion factor
  dumpyear<-c("year")
year<-c(1971:2016)
coarc_fish<-coarc_fish[,!names(coarc_fish) %in% dumpyear]
coarc_fish<-cbind(year, coarc_fish)
  coarc_fish_9394<-coarc_fish[c(23,24),]
coarc_fish_9394_long<-melt(coarc_fish_9394, by="year")
coarc_fish_9394_mean<-aggregate(value~variable, data=coarc_fish_9394_long, mean)

coarc_fish_9697<-coarc_fish[c(26,27),]
coarc_fish_9697_long<-melt(coarc_fish_9697, by="year")
coarc_fish_9697_mean<-aggregate(value~variable, data=coarc_fish_9697_long, mean)

coarc_fish_cf<-cbind(coarc_fish_9394_mean, coarc_fish_9697_mean)
colnames(coarc_fish_cf)<-c("y9394", "mean9394", "y9697", "mean9697")
coarc_fish_cf<-transform(coarc_fish_cf, cf=mean9697/mean9394)
coarc_fish_cf<-coarc_fish_cf[-1,]
coarc_fish_cf$year<-1
coarc_cf<-dcast(coarc_fish_cf, year~y9394, value.var="cf")

rep.row<-function(x,n){
  matrix(rep(x,each=n),nrow=n)
}
rep.col<-function(x,n){
  matrix(rep(x,each=n), ncol=n, byrow=TRUE)
}

coarc_cf<-rep.row(coarc_cf, 24)#this is where things mess up

engles<-coarc_fish[c(1:24),]
engles_converted<-engles*coarc_cf
campelen<-coarc_fish[c(25:46),]
coarc_fish_converted<-rbind(engles_converted, campelen)

coarc_fish_mean<-aggregate()

#write.csv(coarc_fish_cf, file="coarc_fish_cf_9397.csv")





          
############################################Create multistanza groups for specific species
cod_turbot_plaice_2J3KLNO_strat1_at_len_ests <- read.csv("E:/CoArc/CoArc Newfoundland/cod_turbot_plaice_2J3KLNO_strat1_at_len_ests.csv")
  View(cod_turbot_plaice_2J3KLNO_strat1_at_len_ests)
  
cod_turbot_plaice_length_ts<-cod_turbot_plaice_2J3KLNO_strat1_at_len_ests[,c(1,2,4,5,13,16,17)]
  
 #pisc_8587<-pisc[pisc$year %in% c(1985, 1986, 1987),]
#ctp_2J3KLNO<-aggregate(total~survey.year+length+species+NAFOdiv, data=cod_turbot_plaice_length_ts, mean)
cod_y_div<-cod_turbot_plaice_length_ts[cod_turbot_plaice_length_ts$species %in% c("Atlantic Cod"),]
#------  
cod_y_2J3KL<- cod_y_div[cod_y_div$NAFOdiv %in% c("2J", "3K", "3L"),]
cod_y_3NO<- cod_y_div[cod_y_div$NAFOdiv %in% c("3N", "3O"),]
cod_small<-cod_y_2J3KL[which(cod_y_2J3KL$length <= 35),]
cod_large<-cod_y_2J3KL[which(cod_y_2J3KL$length > 35),]
cod_small_ss_2j3kl<-aggregate (total~survey.year+NAFOdiv+season, data=cod_small, sum)
cod_large_ss_2j3kl<-aggregate (total~survey.year+NAFOdiv+season, data=cod_large, sum)
cod_small_s_2j3kl<-aggregate (total~survey.year+NAFOdiv, data=cod_small_ss_2j3kl, mean)
cod_large_s_2j3kl<-aggregate (total~survey.year+NAFOdiv, data=cod_large_ss_2j3kl, mean)
cod_juvenile_2j3kl<-aggregate (total~survey.year, data=cod_small_s_2j3kl, sum)
cod_adult_2j3kl<-aggregate (total~survey.year, data=cod_large_s_2j3kl, sum)
colnames(cod_juvenile_2j3kl)<-c("year", "cod_2j3kl_s")
colnames(cod_adult_2j3kl)<-c("year", "cod_2j3kl_l")
cod_stanza_2j3kl<-cbind(cod_juvenile_2j3kl, cod_adult_2j3kl)
coarc_codstanza_2j3kl<-transform(cod_stanza_2j3kl, cod_2j3kl_s_t=cod_2j3kl_s*0.001, 
                                 cod_2j3kl_l_t=cod_2j3kl_l*0.001)

cod_small_3NO<-cod_y_3NO[which(cod_y_3NO$length <= 35),]
cod_large_3NO<-cod_y_3NO[which(cod_y_3NO$length > 35),]
cod_small_ss_3NO<-aggregate (total~survey.year+NAFOdiv+season, data=cod_small_3NO, sum)
cod_large_ss_3NO<-aggregate (total~survey.year+NAFOdiv+season, data=cod_large_3NO, sum)
cod_small_s_3NO<-aggregate (total~survey.year+NAFOdiv, data=cod_small_ss_3NO, mean)
cod_large_s_3NO<-aggregate (total~survey.year+NAFOdiv, data=cod_large_ss_3NO, mean)
cod_juvenile_3NO<-aggregate (total~survey.year, data=cod_small_s_3NO, sum)
cod_adult_3NO<-aggregate (total~survey.year, data=cod_large_s_3NO, sum)
colnames(cod_juvenile_3NO)<-c("year", "cod_3NO_s")
colnames(cod_adult_3NO)<-c("year", "cod_3NO_l")
cod_stanza_3NO<-cbind(cod_juvenile_3NO, cod_adult_3NO)
coarc_codstanza_3NO<-transform(cod_stanza_3NO, cod_3NO_s_t=cod_3NO_s*0.001, 
                                 cod_3NO_l_t=cod_3NO_l*0.001)
#-----
Ghalibut_y_div<-cod_turbot_plaice_length_ts[cod_turbot_plaice_length_ts$species %in% c("Greenland Halibut"),]
Aplaice_y_div<-cod_turbot_plaice_length_ts[cod_turbot_plaice_length_ts$species %in% c("American Plaice"),]

cod_small<-cod_y_div[which(cod_y_div$length <= 35),]
cod_large<-cod_y_div[which(cod_y_div$length > 35),]
cod_small_ss_2j3klno<-aggregate (total~survey.year+NAFOdiv+season, data=cod_small, sum)
cod_large_ss_2j3klno<-aggregate (total~survey.year+NAFOdiv+season, data=cod_large, sum)
cod_small_s_2j3klno<-aggregate (total~survey.year+NAFOdiv, data=cod_small_ss_2j3klno, mean)
cod_large_s_2j3klno<-aggregate (total~survey.year+NAFOdiv, data=cod_large_ss_2j3klno, mean)
cod_juvenile_2j3klno<-aggregate (total~survey.year, data=cod_small_s_2j3klno, sum)
cod_adult_2j3klno<-aggregate (total~survey.year, data=cod_large_s_2j3klno, sum)
colnames(cod_juvenile_2j3klno)<-c("year", "cod_length_s")
colnames(cod_adult_2j3klno)<-c("year", "cod_length_l")

Ghalibut_small<-Ghalibut_y_div[which(Ghalibut_y_div$length <= 40),]
Ghalibut_large<-Ghalibut_y_div[which(Ghalibut_y_div$length > 40),]
Ghalibut_small_ss_2j3klno<-aggregate (total~survey.year+NAFOdiv+season, data=Ghalibut_small, sum)
Ghalibut_large_ss_2j3klno<-aggregate (total~survey.year+NAFOdiv+season, data=Ghalibut_large, sum)
Ghalibut_small_s_2j3klno<-aggregate (total~survey.year+NAFOdiv, data=Ghalibut_small_ss_2j3klno, mean)
Ghalibut_large_s_2j3klno<-aggregate (total~survey.year+NAFOdiv, data=Ghalibut_large_ss_2j3klno, mean)
Ghalibut_juvenile_2j3klno<-aggregate (total~survey.year, data=Ghalibut_small_s_2j3klno, sum)
Ghalibut_adult_2j3klno<-aggregate (total~survey.year, data=Ghalibut_large_s_2j3klno, sum)
colnames(Ghalibut_juvenile_2j3klno)<-c("year", "Ghalibut_length_s")
colnames(Ghalibut_adult_2j3klno)<-c("year", "Ghalibut_length_l")

Aplaice_small<-Aplaice_y_div[which(Aplaice_y_div$length <= 35),]
Aplaice_large<-Aplaice_y_div[which(Aplaice_y_div$length > 35),]
Aplaice_small_ss_2j3klno<-aggregate (total~survey.year+NAFOdiv+season, data=Aplaice_small, sum)
Aplaice_large_ss_2j3klno<-aggregate (total~survey.year+NAFOdiv+season, data=Aplaice_large, sum)
Aplaice_small_s_2j3klno<-aggregate (total~survey.year+NAFOdiv, data=Aplaice_small_ss_2j3klno, mean)
Aplaice_large_s_2j3klno<-aggregate (total~survey.year+NAFOdiv, data=Aplaice_large_ss_2j3klno, mean)
Aplaice_juvenile_2j3klno<-aggregate (total~survey.year, data=Aplaice_small_s_2j3klno, sum)
Aplaice_adult_2j3klno<-aggregate (total~survey.year, data=Aplaice_large_s_2j3klno, sum)
colnames(Aplaice_juvenile_2j3klno)<-c("year", "Aplaice_length_s")
colnames(Aplaice_adult_2j3klno)<-c("year", "Aplaice_length_l")

# Ghalibut_small<-Ghalibut_y_div[which(Ghalibut_y_div$length <= 40),]
# Ghalibut_large<-Ghalibut_y_div[which(Ghalibut_y_div$length > 40),]
# Ghalibut_small_ss_2j3klno<-aggregate (mean~survey.year+NAFOdiv, data=Ghalibut_small, mean)
# Ghalibut_large_ss_2j3klno<-aggregate (mean~survey.year+NAFOdiv, data=Ghalibut_large, mean)
# Ghalibut_juvenile_2j3klno<-aggregate (mean~survey.year, data=Ghalibut_small_ss_2j3klno, sum)
# Ghalibut_adult_2j3klno<-aggregate (mean~survey.year, data=Ghalibut_large_ss_2j3klno, sum)
# colnames(Ghalibut_juvenile_2j3klno)<-c("year", "Ghalibut_length_s")
# colnames(Ghalibut_adult_2j3klno)<-c("year", "Ghalibut_length_l")
# 
# Aplaice_small<-Aplaice_y_div[which(Aplaice_y_div$length <= 35),]
# Aplaice_large<-Aplaice_y_div[which(Aplaice_y_div$length > 35),]
# Aplaice_small_ss_2j3klno<-aggregate (mean~survey.year+NAFOdiv, data=Aplaice_small, mean)
# Aplaice_large_ss_2j3klno<-aggregate (mean~survey.year+NAFOdiv, data=Aplaice_large, mean)
# Aplaice_juvenile_2j3klno<-aggregate (mean~survey.year, data=Aplaice_small_ss_2j3klno, sum)
# Aplaice_adult_2j3klno<-aggregate (mean~survey.year, data=Aplaice_large_ss_2j3klno, sum)
# colnames(Aplaice_juvenile_2j3klno)<-c("year", "Aplaice_length_s")
# colnames(Aplaice_adult_2j3klno)<-c("year", "Aplaice_length_l")

coarc_multistanza<-cbind(cod_juvenile_2j3klno, cod_adult_2j3klno, Ghalibut_juvenile_2j3klno, Ghalibut_adult_2j3klno, Aplaice_juvenile_2j3klno, Aplaice_adult_2j3klno)

coarc_multistanza_t<-transform(coarc_multistanza, cod_length_s_t=cod_length_s*0.001/495000, 
                               cod_length_l_t=cod_length_l*0.001/495000, 
                               Ghalibut_length_s_t=Ghalibut_length_s*0.001/495000,
                               Ghalibut_length_l_t=Ghalibut_length_l*0.001/495000, 
                               Aplaice_length_s_t=Aplaice_length_s*0.001/495000, 
                               Aplaice_length_l_t=Aplaice_length_l*0.001/495000)


write.csv(coarc_multistanza_t, file="coarc_multistanza_tkm2_2j3klno_02.csv")
#-------------------------------------------------------------------------
  
######Invertebrate data#################

NL_2J3KLNO_Invert_Data <- read.csv("~/JCT work/CoArc/CoArc Newfoundland/NL_2J3KLNO_Invert_Data.csv")
#View(NL_2J3KLNO_Invert_Data)

NL_invertdata<-NL_2J3KLNO_Invert_Data

NLinvertdata<-transform(NL_invertdata, SURVEY_AREA_KM2=SURVEY_AREA_MN2*3.4299)

NL__invert_stdz<-transform(NLinvertdata, STD_WEIGHT=WEIGHT_IN_TONS/SURVEY_AREA_KM2)



#For individual spp
# ind_spp_allyears_div<-NLdata[,c("Season", "DIV", "year", "GROUP_NAME","Fgroup", "WEIGHT_IN_TONS")]
#sum by invert category
Inverts_allyears_div<-aggregate (STD_WEIGHT~Invertebrate_Category+DIV+year+COMMON_NAME, data=NL_invert_stdz, sum)
#average by DIV
Inverts_allyears_m<-aggregate(STD_WEIGHT~year+Invertebrate_Category, data=Inverts_allyears_div, sum)
Inverts_allyears<-transform(Inverts_allyears_m, STD_WEIGHT_m=STD_WEIGHT/5)
#for weight in t*km-2
Inverts_allyears<-transform(Inverts_allyears, total_weight=STD_WEIGHT_m)

Sfeeder_inverts_allyears<-Inverts_allyears[Inverts_allyears$Invertebrate_Category %in% c("Sponges", "Sea Anemones", "Bivalves", 
                                                                                           "Brittle/Basket Stars", "Ascidiacea", 
                                                                                           "Coral"),]
Sfeeder_weight<-aggregate(total_weight~year, data=Sfeeder_inverts_allyears, sum)
Sfeeder_weight<-transform(Sfeeder_weight, Sfeeder_weight=total_weight)
Sfeeder_w_ts<-Sfeeder_weight[,c("year", "Sfeeder_weight")]

Dfeeder_inverts_allyears<-Inverts_allyears[Inverts_allyears$Invertebrate_Category %in% c("Other Echinoderms", "Isopods", "Chaetognatha"),]
Dfeeder_weight<-aggregate(total_weight~year, data=Dfeeder_inverts_allyears, sum)
Dfeeder_weight<-transform(Dfeeder_weight, Dfeeder_weight=total_weight)
Dfeeder_w_ts<-Dfeeder_weight[,c("year", "Dfeeder_weight")]

Pfeeder_inverts_allyears<-Inverts_allyears[Inverts_allyears$Invertebrate_Category %in% c("Other Shrimp", "Other Mollusks", "Other Crustaceans",
                                                                                         "Polychaetes", "Other Crabs", "Toad Crab"),]
Pfeeder_weight<-aggregate(total_weight~year, data=Pfeeder_inverts_allyears, sum)
Pfeeder_weight<-transform(Pfeeder_weight, Pfeeder_weight=total_weight)
Pfeeder_w_ts<-Pfeeder_weight[,c("year", "Pfeeder_weight")]

shrimp_inverts_allyears<-Inverts_allyears[Inverts_allyears$Invertebrate_Category %in% c("Pandalus Shrimp"),]
shrimp_weight<-transform(shrimp_inverts_allyears, shrimp_weight=total_weight)
shrimp_w_ts<-shrimp_weight[,c("year", "shrimp_weight")]

squid_inverts_allyears<-Inverts_allyears[Inverts_allyears$Invertebrate_Category %in% c("Cephalopoda"),]
squid_weight<-transform(squid_inverts_allyears, squid_weight=total_weight)
squid_w_ts<-squid_weight[,c("year", "squid_weight")]

Macrozoop_inverts_allyears<-Inverts_allyears[Inverts_allyears$Invertebrate_Category %in% c("Gelatinous Zooplankton", "Euphausiids", "Amphipods"),]
Macrozoop_weight<-aggregate(total_weight~year, data=Macrozoop_inverts_allyears, sum)
Macrozoop_weight<-transform(Macrozoop_weight, Macrozoop_weight=total_weight)
Macrozoop_w_ts<-Macrozoop_weight[,c("year", "Macrozoop_weight")]

Lmesozoop_inverts_allyears<-Inverts_allyears[Inverts_allyears$Invertebrate_Category %in% c("Mysids"),]
Lmesozoop_weight<-transform(Lmesozoop_inverts_allyears, Lmesozoop_weight=total_weight)
Lmesozoop_w_ts<-Lmesozoop_weight[,c("year", "Lmesozoop_weight")]

#only copepods in 2010
Smesozoop_inverts_allyears<-Inverts_allyears[Inverts_allyears$Invertebrate_Category %in% c("Copepods"),]
Smesozoop_weight<-transform(Smesozoop_inverts_allyears, Smesozoop_weight=total_weight)
Smesozoop_w_ts<-Smesozoop_weight[,c("year", "Smesozoop_weight")]

snowcrab_inverts_allyears<-Inverts_allyears[Inverts_allyears$Invertebrate_Category %in% c("Snow Crab"),]
snowcrab_weight<-transform(snowcrab_inverts_allyears, snowcrab_weight=total_weight)
snowcrab_w_ts<-snowcrab_weight[,c("year", "snowcrab_weight")]

# coarc_inverts0<-full_join(snowcrab_w_ts, Smesozoop_w_ts, by= "year")
# coarc_inverts01<-full_join(Lmesozoop_w_ts, Macrozoop_w_ts, by="year")
# coarc_inverts02<-full_join(shrimp_w_ts, squid_w_ts, by="year")
# coarc_inverts03<-full_join(Dfeeder_w_ts, Sfeeder_w_ts, by="year")
# coarc_inverts04<-full_join(coarc_inverts0, Pfeeder_w_ts, by="year")
# coarc_inverts05<-full_join(coarc_inverts04. coarc_inverts03, by="year")
# coarc_inverts06<-full_join(coarc_inverts02, coarc_inverts01, by="year")
coarc_inverts<-cbind(snowcrab_w_ts, Smesozoop_w_ts, Lmesozoop_w_ts, shrimp_w_ts, squid_w_ts, Dfeeder_w_ts, Pfeeder_w_ts, Sfeeder_w_ts, Macrozoop_w_ts)


write.csv(coarc_inverts, file="coarc_inverts1b.csv")

squid<-Inverts_allyears_div[Inverts_allyears_div$Invertebrate_Category %in% c("Cephalopoda"),]
squid_unique<-unique(squid$COMMON_NAME)
write.csv(squid_unique, file="squid_unique.csv")
#----------------------------------------------------------------
  #################SEABIRDS#########################

#Seabird.all.species.csv <- read.csv("D:/CoArc/CoArc Newfoundland/Seabird all species.csv.csv")
seabird_cws<-read.csv("Seabird_cws.csv")
seabirds_yspp<-aggregate(Count~Year+English, data=seabird_cws, sum)
seabirds_bpkm<-aggregate(BirdsPerKm~Year+English, data=seabird_cws, sum)

####################fish diets############
NL_2J3KLNO_diet_8587b <- read.csv("~/JCT work/CoArc/CoArc Newfoundland/NL_2J3KLNO_diet_8587b.csv")
NLdiet8587<-NL_2J3KLNO_diet_8587b  
#View(NL_2J3KLNO_diet_8587b)
NLdiet8587_div<-aggregate (NLdiet8587, by=list(NLdiet8587$PRED_COMM_NAME), data=NLdiet8587, FUN=mean, na.rm=TRUE)
write.csv(NLdiet8587_div, file="NLdiet8587_div.csv")
NLdiet8587_diet<-aggregate (NLdiet8587_div_long, by=list(NLdiet8587_div_long$species), data=NLdiet8587_div_long, FUN=sum, na.rm=TRUE)
write.csv(NLdiet8587_diet, file="NLdiet8587_dietd.csv")
##------------
require(data.table)
require(reshape2)
NL_diet_CoArc_ts <- read.csv("~/JCT work/CoArc/CoArc Newfoundland/NL_diet_ts_1.csv")
NL_diet_long<-melt(NL_diet_CoArc_ts, id.vars=1:4, measure=c("capelin", "sandlance", "sbenth", "shrimp", 
                                                           "lbenth", "macrozooplankton", 
                                                           "snow.crab", "redfish", "greenland.halibut", 
                                                        "squid", "predatory.invertebrate", "cod",
                                                           "mbenth", "arctic.cod", 
                                                            "deposit.feeding.invertebrate",
                                                              "thorny.skate", 
                                                           "other.pisc", "suspension.feeding.invertebrate",
                                                            "L.mesozooplankton"))
NL_coarc_diet_long<-aggregate(value~variable+YEAR+DIV+PRED_COMM_NAME, data=NL_diet_long, FUN=mean)
NL_coarc_diet_l_y<-aggregate(value~YEAR+PRED_COMM_NAME+variable, data=NL_diet_long, FUN=sum)
NL_coarc_dietmat<-dcast(NL_coarc_diet_l_y,...~variable, value.var="value")

NL_coarc_diet1315<-NL_coarc_dietmat[NL_coarc_dietmat$YEAR %in% c(2013, 2014, 2015),]
NL_coarc_diet1315mean<-aggregate(NL_coarc_diet1315, by=list(NL_coarc_diet1315$PRED_COMM_NAME),data=NL_coarc_diet1315, FUN=mean, na.rm=F)
write.csv(NL_coarc_diet1315mean, file="NLcoarcdiet1315mean.csv")

------------------------------

#fill NAs in data with rolling means 
  library(zoo)
capelin_ts_roll<-na.fill(capelin_ts, "extend")

POCmean<-aggregate( POC~year,data=POCazmp_date, mean)
Sfeeder_weight<-aggregate(total_weight~year, data=Sfeeder_inverts_allyears, sum)