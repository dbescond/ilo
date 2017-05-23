## ---- eval=FALSE---------------------------------------------------------
#  
#  get_ilo(collection = 'YI', info) %>% count(indicator, mode = substr(info,5,7)) %>% spread(mode, n) %>% ungroup() %>%mutate(TOT = rowSums(.[2:3], na.rm = TRUE))
#  

