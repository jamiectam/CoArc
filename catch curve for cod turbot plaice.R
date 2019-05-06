#catch curve for Atlantic Cod 2J3KLNO
cod_turbot_plaice_2J3KLNO_strat1_at_len_ests <- read.csv("~/JCT work/CoArc/CoArc Newfoundland/cod_turbot_plaice_2J3KLNO_strat1_at_len_ests.csv")
cod_turbot_plaice_length_ts<-cod_turbot_plaice_2J3KLNO_strat1_at_len_ests[,c(1,2,4,5,13,16,17)
                                                                          
cod_y_div<-cod_turbot_plaice_length_ts[cod_turbot_plaice_length_ts$species %in% c("Atlantic Cod"),]
Ghalibut_y_div<-cod_turbot_plaice_length_ts[cod_turbot_plaice_length_ts$species %in% c("Greenland Halibut"),]
Aplaice_y_div<-cod_turbot_plaice_length_ts[cod_turbot_plaice_length_ts$species %in% c("American Plaice"),]

cod_2j3klno<-aggregate (total~survey.year+NAFOdiv+season+length, data=cod_y_div, sum)
cod_size_2j3klno<-aggregate (total~survey.year+NAFOdiv+length, data=cod_2j3klno, mean)
cod_size_2j3klno<-aggregate (total~survey.year+length, data=cod_size_2j3klno, sum)
cod_13<-cod_size_2j3klno[cod_size_2j3klno$survey.year %in% c("2013"),]
cod_13_data<-cod_13[,c(2,3)]
cod_13b_data <-transform(cod_13_data, age=1:48)
cod_13_data<-cod_13b_data[,c(2,3)]
colnames(cod_13_data)<-c("catch", "age")
cod_cc2 <- catchCurve(catch~age,data=cod_13_data, ages2use = 12:36)