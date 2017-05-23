## ----install, eval=FALSE-------------------------------------------------
#  # install.packages("ilo")

## ----install2, eval=FALSE------------------------------------------------
#  require(devtools)
#  install_github("dbescond/ilo")

## ---- eval=TRUE----------------------------------------------------------
require(ilo)
as.data.frame(ls("package:ilo")) 

## ---- eval=FALSE---------------------------------------------------------
#  help_ilo()

## ---- eval=FALSE---------------------------------------------------------
#  init_ilo()

## ---- eval=FALSE---------------------------------------------------------
#  init_ilo(update, lang = 'fr')

## ---- eval=FALSE---------------------------------------------------------
#  close_ilo()

## ---- eval=FALSE---------------------------------------------------------
#  ilo$segment

## ---- eval=FALSE---------------------------------------------------------
#  names(ilo$code) %>% sort

## ---- eval=FALSE---------------------------------------------------------
#  # get monthly indicator (from STI collection) of Canada (geo = 'CAN')
#  get_ilo(collection = 'STI', freq = 'M', ref_area = 'CAN')

## ---- eval=FALSE---------------------------------------------------------
#  # get quarterly data (from ST collection) for unemployment rate (indicator = 'UNE_DEAP_RT')
#  get_ilo(collection = 'STI', freq = 'Q', ref_area = 'CAN', Query = "filter(indicator %in% 'UNE_DEAP_SEX_AGE_RT')")
#  

## ---- eval=FALSE---------------------------------------------------------
#  # label all variables and back in a wide style
#  
#  Mydata <- get_ilo(collection = 'STI', freq = 'M', ref_area = 'ALB')
#  Mydata %>% switch_ilo()
#  

## ---- eval=FALSE---------------------------------------------------------
#  # label only indicator
#  Mydata %>% switch_ilo(indicator)
#  
#  

## ---- eval=FALSE---------------------------------------------------------
#  Mydata <- get_ilo(collection = 'STI', freq = 'Q', ref_area = 'FRA') %>% switch_ilo(classif1, indicator)
#  Mydata %>% filter_ilo(classif1 = 'Aggregate age bands')
#  Mydata %>% filter_ilo(indicator = 'Unemployment')
#  

## ---- eval=FALSE---------------------------------------------------------
#  get_ilo(collection = 'STI', freq = 'Q', ref_area = 'ALB') %>%
#    switch_ilo() %>%
#    view_ilo()
#  

## ---- eval=FALSE---------------------------------------------------------
#  LAST <- full_join(
#      get_ilo( query = "  select(ref_area, time, source) %>%
#                                      filter(str_sub(time,1,4) > '2013')",
#                         collection = 'STI',
#                         freq = 'Q',
#                         package= 'stringr') %>%
#          group_by(ref_area, time, source) %>%
#          summarise(n = n()) %>%
#          ungroup() %>%
#          spread(time, n) %>% filter(!substr(source,1,3) %in% 'GA:')
#          ,
#  # add the ref_area weight from collection = 'ILOEST' indicator %in% EAP_2EAP_NB
#        get_ilo( query = "    filter( as.character(time) %in% '2014',
#                                        indicator %in% 'EAP_2EAP_SEX_AGE_NB',
#                                        sex %in% 'SEX_T',
#                            classif1 %in% 'AGE_5YRBANDS_TOTAL') %>%
#                                    select(ref_area, obs_value)",
#                     collection = 'ILOEST',
#                     freq = 'A') %>% mutate(obs_value = round(obs_value, 0)) %>%
#        rename(EAP2014 = obs_value)
#  , by = 'ref_area') %>%
#  select(ref_area, source, EAP2014, starts_with('20')) %>%
#  arrange(desc(EAP2014)) %>% # sort by the weight
#  mutate(iso3 = ref_area) %>% switch_ilo(ref_area)
#  

## ---- eval=FALSE---------------------------------------------------------
#  X <- get_ilo(collection = 'STI', freq = 'Q', ref_area = 'ALB')
#  Y <- get_ilo(ref_area = 'ALB')
#  
#  save_ilo(X = 'InSTI', Y = 'InYI')
#  save_ilo(X,Y)

## ---- eval=FALSE---------------------------------------------------------
#  X <- get_ilo(collection = 'STI', freq = 'Q', ref_area = 'ALB')
#  save_ilo(X, format = 'csv')

## ---- eval=FALSE---------------------------------------------------------
#  X <- get_ilo(collection = 'STI', freq = 'Q', ref_area = 'ALB')
#  save_ilo(X, format = 'del')

## ---- eval=FALSE---------------------------------------------------------
#  X <- get_ilo(collection = 'STI', freq = 'Q', ref_area = 'ALB')
#  save_ilo(X, format = 'rev')

## ----sessioninfo, message=FALSE, warning=FALSE---------------------------
sessionInfo()

