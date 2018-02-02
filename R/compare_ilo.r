#' R tools to compare ilo datasets with a self made datasets
#'
#' wraper 
#'
#' @param df ilo tbl data frame to manipulate.
#' @param ... Specification of columns and pattern.
#' @section Specification:
#' These functions allow you to select database variables based on their names.
#' \itemize{
#'  \item \code{collection}: select collection, characters verctor, default is 'YI', see ilo$segment, also available 'all',
#'  \item \code{freq}: select freqency, characters verctor, default is 'A', see ilo$segment, also available 'all',
#'  \item \code{ref_area}: select ref_area, characters verctor, default is 'all',
#'  \item \code{indicator}: used for searching character string in indicator variable,
#'  \item \code{source}: used for searching character string in source variable,
#'  \item \code{source.ty}: used for searching character string with the 2 first characters of source variable,
#'  \item \code{time}: used for searching character string in time variable,
#'  \item \code{obs_status}: used for searching character string in obs_status variable,
#'  \item \code{note_indicator}: used for searching character string in note_indicator variable,
#'  \item \code{note_source}: used for searching character string in note_source variable,
#'  \item \code{info}: used for searching character string in info administrative variable, if not call will not appear,
#'  \item \code{web}: base on backoffice data and meta config, return value that are publish on the website,
#' }

#' To learn more about ilo, start with the online vignettes:
#' \code{compare_ilo()}
#'
#' @name compare_ilo
#' @import dplyr
#'
#'
#' @return a tbl data frame
#'
#' @examples
#' res <- get_ilo(ref_area = 'AFG')
#'
#' res %>% filter_ilo(note_source = 'T3')

#' @export

compare_ilo <- function(df, ...	){

#################

invisible(gc(reset = TRUE))


.dots  = lazyeval::lazy_dots(...)

variable <- sapply(.dots, function(x) {ifelse(is.null(as.character(x$expr)), NULL ,paste0(as.character(x$expr), '/'))})
rm(.dots)
if(length(variable) == 0){TEST <- tolower(paste0('/',paste0(paste0(colnames(df), '/'), collapse = '')))	}
if(!length(variable) == 0) {TEST <- paste0('/',tolower(variable), collapse = '')}
if(length(variable) == 1) {if(variable %in% 'keep/') TEST <- tolower(paste0('/',paste0(paste0(c('keep',colnames(df)), '/'), collapse = '')))}

variable <- tolower(TEST); rm(TEST)

MYwarnings <- FALSE
	
if(str_detect(variable, '/check/')){MYwarnings = TRUE}	
	
	

UPDATE <- 0

pass_query = paste0("filter(", 
					ifelse(str_detect(variable, '/source/'), paste0("source %in% c('",paste0(unique(df$source), collapse = "', '"),"'), "), ''),
					ifelse(str_detect(variable, '/source.type/'), paste0("substr(source,1,2) %in% c('",paste0(unique(substr(df$source,1,2)), collapse = "', '"),"'), "), ''),
					ifelse(str_detect(variable, '/indicator/'), paste0("indicator %in% c('",paste0(unique(df$indicator), collapse = "', '"),"'), "), ''), 
					")") %>% gsub(", )", ")", .)


query <- paste0("check <- ilo:::get_ilo(package = 'ilo', ",				
		ifelse(str_detect(variable, '/collection/'), paste0("collection = c('",paste0(unique(df$collection), collapse = "', '"),"'), "), ''), 
		ifelse(str_detect(variable, '/ref_area/'), paste0("ref_area = c('", paste0(unique(df$ref_area), collapse= "', '"), "'), "), ''),
		ifelse(str_detect(variable, '/freq/'), paste0("freq = c('", paste0(unique(substr(df$time,5,5) %>% gsub('', 'A', ., fixed = TRUE)), collapse= "', '"), "'), "), ''),
		ifelse(str_detect(variable, '/source/') | str_detect(variable, '/indicator/') | str_detect(variable, '/source.type/'), paste0("query = pass_query, add = pass_query"), ''),
				")"	) %>% gsub(", )", ")", .) 
				
eval(parse(text = query)) 

check <- check %>% arrange_(.dots = c("collection", "ref_area", "source", "indicator", "sex", "classif1", "classif2", "time"))
df <-  bind_rows(check %>% slice(0), df %>% mutate_at(vars(-contains('obs_value')),funs(as.character))) %>% arrange_(.dots = c("collection", "ref_area", "source", "indicator", "sex", "classif1", "classif2", "time"))


if( setequal(df, check)) return(print('data up to date'))

if(! setequal(df, check)){


rev <- anti_join(df,check, by = c("collection", "ref_area", "source", "indicator", "sex", "classif1", "classif2", "time", "obs_value", "obs_status", "note_classif", "note_indicator", "note_source")) %>% 
		ungroup 
df <- df %>% switch_ilo(version, keep)
check <- check %>% 	select(collection, ref_area, source, indicator, sex, classif1, classif2, time) %>% 
					switch_ilo(version) %>% 
					distinct(collection, ref_area, source, indicator, sex_version, classif1_version, classif2_version, time) %>% 
					mutate(REV = 1)

rev <- rev %>% 	select(collection, ref_area, source, indicator, sex, classif1, classif2, time) %>% 
			switch_ilo(version) %>% distinct(collection, ref_area, source, indicator, sex_version, classif1_version, classif2_version, time) %>% 
			mutate(keep = 1)

REV <- df %>% left_join(rev, by = c("collection", "ref_area", "source", "indicator", "sex_version", "classif1_version", "classif2_version", "time")) %>% 
				filter(keep == 1) %>% 
				select(-keep) %>% 
				left_join(check, by = c("collection", "ref_area", "source", "indicator", "sex_version", "classif1_version", "classif2_version", "time"))

if(nrow(REV %>% filter(REV == 1)) > 0) {write_csv(REV %>% filter(REV == 1) %>% select(-REV, -dplyr:::contains('_version')), path = paste0(getwd(), '/output/REV_',last(str_split(getwd(), '/', simplify = TRUE)[-1]),'.csv'), na = ''); UPDATE <- 1}
if(nrow(REV %>% filter(!REV == 1)) > 0) {write_csv(REV %>% filter(!REV == 1) %>% select(-REV, -dplyr:::contains('_version')), path = paste0(getwd(), '/output/NEW_',last(str_split(getwd(), '/', simplify = TRUE)[-1]),'.csv'), na = ''); UPDATE <- 1}

				
df <- df %>% 
			select(collection, ref_area, source, indicator, sex, classif1, classif2, time) %>% 
			switch_ilo(version) %>% distinct(collection, ref_area, source, indicator, sex_version, classif1_version, classif2_version, time) 
				
DEL <- anti_join(check %>% select(-REV), df, by = c("collection", "ref_area", "source", "indicator", "sex_version", "classif1_version", "classif2_version", "time"))
if(nrow(DEL) > 0) {write_csv(DEL, path = paste0(getwd(), '/output/DEL_',str_sub(getwd(), str_locate(getwd(), 'DATA/')[1,2] + 1, -1),'.csv'), na = ''); UPDATE <- 1}

rm(REV, DEL)
}

if(UPDATE %in% 1) {

	if(MYwarnings) {
		list.files(paste0(getwd(), '/output')) %>% as_data_frame %>% 
				filter(str_detect(value, '.csv')) %>% 
				rename(path = value) %>%
				mutate(path = paste0(paste0(getwd(), '/output/',path ))) %>% 
				write_csv(., path = paste0(str_sub(getwd(), 1, str_locate(getwd(), 'DATA/')[1,2]), 'Plse_check_', str_sub(getwd(), str_locate(getwd(), 'DATA/')[1,2] + 1, -1), '.csv'))
				
				
				
		}
return(paste0('update available on ',paste0(getwd(), '/output'), ' folder'))
}

invisible(gc(reset = TRUE))
}









