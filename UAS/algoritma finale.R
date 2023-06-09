setwd("~/209_ITB/Semester II/Research Method/UAS")

rm(list=ls())

library(dplyr)

rdas = list.files("~/209_ITB/Semester II/Research Method/UAS/Hasil Perhitungan/",full.names = T)

ompr = vector("list",10)
spiral = vector("list",10)
simulasi = vector("list",10)
sama = rep(NA,10)
ompr_sum = rep(NA,10)
spiral_sum = rep(NA,10)

i = 1

for(i in 1:10){
  load(rdas[i])
  
  ompr[[i]] = 
    df_hit %>% 
    filter(pilih_ompr == 1) %>% 
    summarise(budget = sum(burn_3m),
              revenue = sum(cost_benefit))
  
  spiral[[i]] = 
    df_hit %>% 
    filter(pilih_spiral == 1) %>% 
    summarise(budget = sum(burn_3m),
              revenue = sum(cost_benefit))
  
  simulasi[[i]] = 
    df_hit %>% 
    filter(pilih_simulasi == 1) %>% 
    summarise(budget = sum(burn_3m),
              revenue = sum(cost_benefit))
  
  sama[i] = sum(df_hit$pilih_ompr == df_hit$pilih_spiral)
  
  ompr_sum[i] = sum(df_hit$pilih_ompr)
  spiral_sum[i] = sum(df_hit$pilih_spiral)
}

# ==============================
gabung_ompr = do.call(rbind,ompr)
gabung_spiral = do.call(rbind,spiral)
gabung_simulasi = do.call(rbind,simulasi)

gabung_all = cbind(gabung_ompr,gabung_simulasi,gabung_spiral)
colnames(gabung_all)[1:2] = paste0("eksak_",colnames(gabung_all)[1:2])
colnames(gabung_all)[3:4] = paste0("existing_",colnames(gabung_all)[3:4])
colnames(gabung_all)[5:6] = paste0("spiral_",colnames(gabung_all)[5:6])

save(gabung_all,
     sama,
     ompr_sum,
     spiral_sum,
     file = "bahan review.rda")



