

library(shiny,quietly=TRUE)
library(shinydashboard,quietly=TRUE)
library(DT,quietly=TRUE)
library(Rilostat,quietly=TRUE)
library(doSNOW,quietly=TRUE)
library(dplyr,quietly=TRUE)
library(tidyr,quietly=TRUE)
library(stringr,quietly=TRUE)

# ref clustare type
# cl_type <- "SOCK" # or "SOCK", "PSOCK", "FORK" == shiny io
cl_core <- max(1, parallel::detectCores() - 1)
# ref codelist to download
ref <- c("CL_COLLECTION","CL_COUNTRY", "CL_FREQ") #, "CL_SOURCE", "CL_REPRESENTED_VARIABLE","CL_SUBJECT", "CL_SURVEY")

# create and register clusters
cl <- parallel::makeCluster(cl_core) ; registerDoSNOW(cl)

# assign codelist on a list CODE_ORA try :: names(CODE_ORA)
CODE_ORA 	<- 	foreach(i=1:length(ref),.inorder=TRUE, .packages = c("Rilostat", "dplyr")) %dopar% 
				eval(parse(text= paste0("getCodelist(codelist = '",ref[i],"')")))			
names(CODE_ORA) <- ref


# create type of collection
CODE_ORA$CL_COLLECTION_TYPE <- data_frame(	code = c('M','D','A'), 
											label = c('Main indicators','Details indicators','Ad hoc indicators'))


ref <- as.numeric(format(Sys.time(),"%Y"))
# init collection
ref <- data_frame(	code = c('YI', 'STI', 'PSE', 'MIG', 'YTH', 'EAPEP', 'KI', 'GWR', 'ODR', 'KILMNSO', 'RUR', 'KILMEST', 'KILMNSOORIG', 'MPI', 'KIST'), 
					minYear = c(1945, 1930, 1985, 1986, 2000, 1980, 1945, 1995, 1950, 1945, 1945, 1945, 1945, 1945, 1930), 
					maxYear = c(ref, ref, 2009, 2008, 2013, 2030, 2012, 2013, 2013, ref, ref, ref, ref, 2013, ref),
					collection_type = c('D', 'D', 'A', 'A', 'A', 'D', 'M', 'D', 'A', 'D', 'A', 'D', 'D', 'A', 'M'), 
					Monthly = c(0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1),
					Quarterly = c(0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1), 
					Annual = c(1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0),
					sort_ = c(4, 5, 14, 12, 10, 3, 1, 6, 15, 7, 11, 8, 9, 13, 2)
				) %>% arrange(as.numeric(sort_))
			
			
CODE_ORA$CL_COLLECTION <- CODE_ORA$CL_COLLECTION %>% filter(code %in% ref$code) %>% left_join(ref, by = "code")



ref <- CODE_ORA$CL_COLLECTION %>% 	group_by(collection_type) %>% 
									summarise(	minYear = min(as.numeric(minYear)), 
												maxYear = max(as.numeric(maxYear)),
												collectionCode = paste0(code, collapse = ", ")) %>%
									ungroup() %>% 
									rename(code = collection_type)
										
										
CODE_ORA$CL_COLLECTION_TYPE <- CODE_ORA$CL_COLLECTION_TYPE %>% left_join(ref, by = "code")
												

# ref codelist "Indicatr_collection" to download		
ref	<- 	CODE_ORA$CL_COLLECTION %>% select(code) %>% t %>% c %>% paste0("CL_INDICATOR_", .)


# assign in CL_INDICATOR_PLUS (ie. indicator by collection) on CODE_ORA try :: names(CODE_ORA)
CODE_ORA$CL_INDICATOR_PLUS 		<- 	foreach(i=1:length(ref),.inorder=FALSE, .packages = c("Rilostat", "dplyr")) %dopar% 
									mutate(eval(parse(text= paste0("getCodelist(codelist = '",ref[i],"')"))), collection = gsub("CL_INDICATOR_", "",ref[i])) %>% 
									bind_rows %>% arrange(sort)
									
# close clusters
stopCluster(cl) ; rm(ref, cl); gc()

# Improve codelist
# CODE_ORA$CL_SURVEY 		<-  CODE_ORA$CL_SURVEY %>% separate(label, c("country", "source", "label"), sep=" - ", extra = "merge")

