tbl = read_csv("https://www.dropbox.com/s/erhs9hoj4vhrz0b/eddypro.csv?dl=1") 
tbl 
class(tbl) 
tbl = read_csv("https://www.dropbox.com/s/erhs9hoj4vhrz0b/eddypro.csv?dl=1", skip =1) 
tbl 
tbl = read_csv("https://www.dropbox.com/s/erhs9hoj4vhrz0b/eddypro.csv?dl=1", skip = 1, comment=c("[")) 
tbl 
tbl = read_csv("https://www.dropbox.com/s/erhs9hoj4vhrz0b/eddypro.csv?dl=1", skip = 1, na =c("","NA","-9999","-9999.0"), comment=c("[")) 
tbl = tbl[-1,] 
tbl 
glimpse(tbl) 
tbl = select(tbl, -(roll)) 
tbl = tbl %>% mutate_if(is.character, factor) 
names(tbl) = str_replace_all(names(tbl), "[!]","_emph_") 
names(tbl) = names(tbl) %>% 
  str_replace_all("[!]","_emph_") %>% 
  str_replace_all("[?]","_quest_") %>% 
  str_replace_all("[*]","_star_") %>% 
  str_replace_all("[+]","_plus_") %>% 
  str_replace_all("[-]","_minus_") %>% 
  str_replace_all("[@]","_at_") %>% 
  str_replace_all("[$]","_dollar_") %>% 
  str_replace_all("[#]","_hash_") %>% 
  str_replace_all("[/]","_div_") %>% 
  str_replace_all("[%]","_perc_") %>% 
  str_replace_all("[&]","_amp_") %>% 
  str_replace_all("[\\^]","_power_") %>% 
  str_replace_all("[()]","_") 
glimpse(tbl) 
sapply(tbl,is.numeric) 
tbl_numeric = tbl[,sapply(tbl,is.numeric) ] 
cor_td = cor(tbl_numeric) 
cor_td = cor(drop_na(tbl_numeric)) 
cor_td 
cor_td = cor(drop_na(tbl_numeric)) %>% as.data.frame %>% select(co2_flux) 
vars = row.names(cor_td)[cor_td$co2_flux^2 > .1] %>% na.exclude 
tbl_numeric = na.omit(tbl_numeric) 
tbl_numeric[, c(20,21,22,86,87,88,89,90,91,92,93,94)] = NULL 
cor_td = cor(tbl_numeric$co2_flux, tbl_numeric) 
class(cor_td) 
cor_td = as.data.frame(cor_td) 
class(cor_td)
cor_td$co2_time_lag = NULL 
cor.names = names(cor_td[, abs(cor_td) > 0.4]) 
cor.names
cor_td
model1 = lm(tbl_numeric$co2_flux~tbl_numeric$rand_err_Tau + tbl_numeric$LE + tbl_numeric$rand_err_LE + tbl_numeric$h2o_flux + tbl_numeric$rand_err_h2o_flux + tbl_numeric$air_temperature + tbl_numeric$RH + tbl_numeric$VPD + tbl_numeric$T_star_ + tbl_numeric$un_LE + tbl_numeric$un_co2_flux + tbl_numeric$un_h2o_flux + tbl_numeric$h2o_var + tbl_numeric$w_div_co2_cov + tbl_numeric$w_div_h2o_cov + tbl_numeric$flowrate, data = tbl_numeric)
model1
summary(lm(tbl_numeric$co2_flux~tbl_numeric$rand_err_Tau + tbl_numeric$LE + tbl_numeric$rand_err_LE + tbl_numeric$h2o_flux + tbl_numeric$rand_err_h2o_flux + tbl_numeric$air_temperature + tbl_numeric$RH + tbl_numeric$VPD + tbl_numeric$T_star_ + tbl_numeric$un_LE + tbl_numeric$un_co2_flux + tbl_numeric$un_h2o_flux + tbl_numeric$h2o_var + tbl_numeric$w_div_co2_cov + tbl_numeric$w_div_h2o_cov + tbl_numeric$flowrate, data = tbl_numeric))
model2 = lm(tbl_numeric$co2_flux~tbl_numeric$rand_err_Tau + LE + tbl_numeric$rand_err_LE + tbl_numeric$h2o_flux + tbl_numeric$rand_err_h2o_flux + tbl_numeric$air_temperature + tbl_numeric$RH + tbl_numeric$VPD + tbl_numeric$T_star_ + tbl_numeric$un_LE + tbl_numeric$un_co2_flux + tbl_numeric$un_h2o_flux + tbl_numeric$h2o_var + tbl_numeric$w_div_co2_cov + tbl_numeric$w_div_h2o_cov + tbl_numeric$flowrate, data = tbl_numeric)
model2 
lm(tbl_numeric$co2_flux~rand_err_Tau + LE + rand_err_LE + h2o_flux + rand_err_h2o_flux +  T_star_ + un_LE + un_co2_flux + un_h2o_flux + h2o_var +  w_div_h2o_cov + flowrate, data = tbl_numeric)
summary(lm(tbl_numeric$co2_flux~rand_err_Tau + LE + rand_err_LE + h2o_flux + rand_err_h2o_flux +  T_star_ + un_LE + un_co2_flux + un_h2o_flux + h2o_var +  w_div_h2o_cov + flowrate, data = tbl_numeric))
model3 = lm(tbl_numeric$co2_flux~rand_err_Tau + LE + rand_err_LE + h2o_flux + rand_err_h2o_flux +  T_star_ + un_LE + un_co2_flux + un_h2o_flux + h2o_var + flowrate, data = tbl_numeric)
summary(model3)        
anova(model3)
