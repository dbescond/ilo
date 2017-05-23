#' helper to calculate ilo indicator (volume and ration)
#'
#' faster method to recode, order, add label of categorical variables
#'
#' Support for recoding variable, check \code{?car::Recode}, for labelling variable 
#'
#' @param df microdataset.
#' @param ... Specification of variables to get.
#' @section Specification:
#' others functions allow advanced manipulation.
#' \itemize{
#'  \item \code{scope}: check set as TRUE if indicator, sex, classif1, classif2 are ok, FALSE else,
#'  \item \code{version}: check set as TRUE if collection indicator configuration is ok , FALSE else,
#'  \item \code{indicator}: check set as TRUE if collection indicator, FALSE else,
#' }
#' @return data frame with check columns
#' @author ILO / bescond  
#' @keywords ILO, microdataset, preprocessing
#' @examples
#' ## Not run:
#'
#' ## End(**Not run**)

#' @export

check_ilo <- function(df, ...){

invisible(gc(reset = TRUE))

.dots  = lazyeval::lazy_dots(...)

variable <- sapply(.dots, function(x) {ifelse(is.null(as.character(x$expr)), NULL ,paste0(as.character(tolower(x$expr)), '/'))})
rm(.dots)
if(length(variable) == 0){variable <- 'scope'}
if(!length(variable) == 0) {variable <- paste0('/',tolower(variable), collapse = '')}



if(str_detect(variable, '/scope/')){
df <- df %>%	
		mutate(class_ref = str_sub(indicator, 10, -4)) %>% 
		separate(class_ref, c('sex_test', 'classif1_test', 'classif2_test'), sep = '_', remove = TRUE, fill = 'right', extra = 'drop') %>% 
		mutate(	classif2_test = ifelse(!sex_test %in% 'SEX' & !classif1_test %in% NA, classif1_test, classif2_test), 
				classif1_test = ifelse(!sex_test %in% 'SEX' & !classif1_test %in% NA, sex_test, classif1_test), 
				sex_test = ifelse(!sex_test %in% 'SEX',NA, sex_test), 				
				sex_test = str_sub(paste0(sex), 1,3) == paste0(sex_test), 
				classif1_test =  str_sub(paste0(classif1), 1,3) == str_sub(paste0(classif1_test),1,3), 
				classif2_test = str_sub(paste0(classif2), 1,3) == str_sub(paste0(classif2_test),1,3), 
				check =  sex_test & classif1_test & classif2_test, 
				sex_test =NULL, classif1_test =NULL, classif2_test = NULL) 
				
	}
	
	
	
	
	
if(str_detect(variable, '/version/')){	


	ilo:::init_ilo(-cl)
	ref_config <- ilo$code$cl_col_ind_clv %>% filter(collection %in% unique(df$collection), indicator %in% unique(df$indicator))
	test_version <- ref_config %>% mutate(TEST = paste0(indicator, "/", classif_vs)) %>% select(TEST) %>% distinct(TEST) %>% t %>% c  
	test_version <-c(test_version, paste(ref_config$indicator, "NA", sep = "/"))

df <- 	df %>% 
			select(-contains('_version'))%>% 
			switch_ilo(version) %>%
			mutate( sex_test = paste0(indicator, "/", sex_version),
					sex_test = paste0(sex_test) %in% test_version,
					classif1_test = paste0(indicator, "/", classif1_version), 
					classif1_test = paste0(classif1_test) %in% test_version, 
					classif2_test = paste0(indicator, "/", classif2_version), 
					classif2_test = paste0(classif2_test) %in% test_version, 
					check =  sex_test & classif1_test & classif2_test,
					sex_test =NULL, classif1_test =NULL, classif2_test = NULL) %>% select(-contains('_version'))  
	}

	
if(str_detect(variable, '/indicator/')){	

ilo:::init_ilo(-cl)
	ref_config <- ilo$code$cl_col_ind_clv %>% filter(collection %in% unique(df$collection), indicator %in% unique(df$indicator)) %>% select(indicator)

df <- 	df %>% mutate(check = ifelse(indicator %in% ref_config$indicator, TRUE, FALSE))

}

	
df	
	
}











