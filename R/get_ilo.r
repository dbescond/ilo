#' R tools to access internal data on oracle / ilostat
#'
#'
#' @param ... Specification of selection and/or queries.
#' @section Specification:
#' These functions allow you to select database variables based on their names.
#' \itemize{
#'  \item \code{collection}: select collection, characters verctor, default is 'YI', see ilo$segment, also available 'all',
#'  \item \code{freq}: select freqency, characters verctor, default is 'A', see ilo$segment, also available 'all',
#'  \item \code{ref_area}: select ref_area, characters verctor, default is 'all',
#'  \item \code{indicator}: used for searching character string in indicator variable,
#'  \item \code{source}: used for searching character string in source variable,
#'  \item \code{sex}: used for searching character string in sex variable,
#'  \item \code{classif1}: used for searching character string in classif1 variable,
#'  \item \code{classif2}: used for searching character string in classif2 variable,
#'  \item \code{time}: used for searching character string in time variable,
#'  \item \code{obs_status}: used for searching character string in obs_status variable,
#'  \item \code{note_classif}: used for searching character string in note_classif variable,
#'  \item \code{note_indicator}: used for searching character string in note_indicator variable,
#'  \item \code{note_source}: used for searching character string in note_source variable,
#'  \item \code{info}: used for searching character string in info administrative variable, if not call will not appear,
#'  \item \code{web}: base on backoffice data and meta config, return value that are publish on the website,
#' }
#'
#' others functions allow advanced manipulation.
#' \itemize{
#'  \item \code{label}: return label and allow to work with label (not capital sensitive) for the variables mentionned above,
#'  \item \code{timefrom}: filter time >= chracter string of year, ie year >= 2000 is timefrom = '2000',
#'  \item \code{timeto}: filter time <= chracter string of year, ie year <= 2005 is timeto = '2005',
#'  \item \code{style}: to be completed, default 'none' also available 'ts',
#'  \item \code{query}: used for inserting dplyr manipulation code at inside each collection/freq/ref_area segments,
#'  \item \code{add}: R object that should be add if needed for the parallel process of the query,
#'  \item \code{package}: R package that should be add if needed for the parallel process of the query,
#' }
#' To learn more about ilo, start with the online vignettes:
#' \code{help_ilo()}
#'
#' @name get_ilo
#' @importFrom plyr llply mapvalues
#' @importFrom foreach foreach "%dopar%"
#' @importFrom doSNOW registerDoSNOW
#' @importFrom rappdirs user_data_dir
#' @importFrom openxlsx createWorkbook addWorksheet writeData saveWorkbook
#' @import dplyr
#' @importFrom tidyr expand unite
#' @importFrom stringr str_replace_all str_detect str_split_fixed fixed str_c str_split str_replace
#'
#'
#' @return a tbl data frame
#'
#' @examples
#'
#' init_ilo()
#'
#'
#' ### quarterly time serie of female unemployed as from 2000 in united states
#'
#' get_ilo(collection = 'STI', 
#'		freq = 'Q', ref_area = 'USA', 
#'		indicator = 'UNE_TUNE_SEX_AGE_NB', 
#'		timefrom = '2000', sex = 'F', classif1 = 'AGGREGATE_TOTAL', 
#'		query = 'select(ref_area:obs_value) %>% spread(time, obs_value)', package = 'tidyr')
#'
#' ### Annual data of Afganistan from Yearly Collection
#'
#' get_ilo(ref_area = 'AFG')
#'
#' ### Annual collected data  from Yearly Collection
#'
#' get_ilo(info = 'COL')
#'
#' ### Working with label
#'
#' get_ilo(label, ref_area = 'AFG', source = 'Census', indicator = 'population')
#'
#' @export
get_ilo <- function(...){

invisible(gc(reset = TRUE))
invisible(gc(reset = TRUE))

'%cl%' <- foreach::'%dopar%'
	collection = 'YI'
	freq = 'A' 
	ref_area = NULL
	indicator = NULL
	source  = NULL
	sex = NULL
	classif1 = NULL
	classif2 = NULL
	timefrom = NULL
	timeto = NULL
	time = NULL
	obs_status = NULL 
	note_classif = NULL 
	note_indicator = NULL 
	note_source = NULL
	query = NULL 
	style = 'none' 
	add = NULL 
	package = NULL
	info = NULL
	web <- NULL
	set <- list()
	.dots  = lazyeval::lazy_dots(...)

	set$para <- .dots[!names(.dots)%in%'']
	if(length(set$para)>0){
		for (i in 1:length(names(set$para))){
							
			if(tolower(names(set$para[i])) %in% 'add'){
				eval(expression(assign(as.character(set$para[[i]]$expr), eval(set$para[[i]]$expr, envir =  set$para[[i]]$env))))
				add <- as.character(set$para[[i]]$expr)
			} else {
				eval(expression(assign(names(set$para[i]), eval( set$para[[i]]$expr, envir =  set$para[[i]]$env))))
			}
		}	
	} ; set$para <- NULL
	set$keep <- .dots[names(.dots)%in%''] 
	if(!is.null(info)) { 
			info <- paste0("filter(stringr::str_detect(info, '",info,"'))") 
			if(length(set$keep) > 0) {set$keep <- c(set$keep, lazyeval::lazy_dots(info))
			} else set$keep <- lazyeval::lazy_dots(info)
			} 
	
	set$g1 <- '\''; set$g2 <- paste0(set$g1, '/', set$g1)

	set$TS <- ifelse(style %in% 'ts', TRUE, FALSE)

	if(collection %in% 'all') 	collection <- ilo$segment %>% distinct(collection) %>% t %>% as.character
	if(freq %in% 'all') 		freq <- ilo$segment %>% distinct(freq) %>% t %>% as.character
	keep = NULL
	set$info <- FALSE
	set$label <- FALSE
	set$web <- FALSE
	set$vs <- FALSE
	if(length(set$keep) > 0){
		for (i in 1:length(set$keep)){
			condi <- set$keep[[i]]$expr %>% as.character
			neg <- NULL
			if(length(condi) ==2) {
				neg <- condi[1] 
				condi <- condi[2]
			}

			if(condi %in% c('info', 'label', 'web')){
				if(condi %in% 'info') {set$info <- TRUE}
				if(condi %in% 'label') {set$label <- TRUE; package <- c(package, 'ilo')}
				if(condi %in% 'web') {set$web <- TRUE; package <- c(package, 'ilo')}
			} else{
				condi <- paste0(neg, 'dplyr::starts_with(', set$g1,condi,set$g1, ')')
				keep <- paste0( c(keep, condi), collapse = ', ')
			}
		}	
		if(!is.null(keep)) {
			keep <- paste0('select(',keep,')')	
		}
	}

	if(freq %in% c('Q', 'M') & collection %in% c(NULL, 'YI')) {collection <- 'STI'}
	set$Q3 <- keep; set$keep <- NULL
	set$query <- query ; rm(query) 
	if(!is.null(indicator)) { set$indicator <- paste0("filter(stringr::str_detect(indicator, '",indicator,"'))")} else set$indicator <- indicator; rm(indicator)
	if(!is.null(source)) { set$source <- paste0("filter(stringr::str_detect(source, '",source,"'))")} else set$source <- source; rm(source)
	if(!is.null(sex)) { set$sex <- paste0("filter(stringr::str_detect(sex, '",sex,"'))")} else set$sex <- sex; rm(sex)
	if(!is.null(classif1)) { set$classif1 <- paste0("filter(stringr::str_detect(classif1, '",classif1,"'))")} else set$classif1 <- classif1; rm(classif1)
	if(!is.null(classif2)) { set$classif2 <- paste0("filter(stringr::str_detect(classif2, '",classif2,"'))")} else set$classif2 <- classif2; rm(classif2)
	if(!is.null(time)) { set$time <- paste0("filter(stringr::str_detect(time, '",time,"'))")} else set$time <- time #; rm(time)
	if(!is.null(timefrom)) { set$timefrom <- paste0("filter(as.numeric(substr(time,1,4)) > {as.numeric(",timefrom,")-1})")} else set$timefrom <- timefrom; rm(timefrom)
	if(!is.null(timeto)) { set$timeto <- paste0("filter(as.numeric(substr(time,1,4)) < {as.numeric(",timeto,")+1})")} else set$timeto <- timeto; rm(timeto)
	
	if(!is.null(obs_status)) { set$obs_status <- paste0("filter(stringr::str_detect(obs_status, '",obs_status,"'))")} else set$obs_status <- obs_status; rm(obs_status)
	if(!is.null(note_classif)) { set$note_classif <- paste0("filter(stringr::str_detect(note_classif, '",note_classif,"'))")} else set$note_classif <- note_classif; rm(note_classif)
	if(!is.null(note_indicator)) { set$note_indicator <- paste0("filter(stringr::str_detect(note_indicator, '",note_indicator,"'))")} else set$note_indicator <- note_indicator; rm(note_indicator)
	
	if(!is.null(note_source) ) {if(!stringr:::str_detect(note_source, '!')) {set$note_source <- paste0("filter(stringr::str_detect(note_source, '",note_source,"'))")}} else set$note_source <- note_source
	if(!is.null(note_source) ) {if(stringr:::str_detect(note_source, '!')) { note_source <- stringr:::str_replace(note_source, '!', '') ; set$note_source <- paste0("filter(!stringr::str_detect(note_source, '",note_source,"') | note_source %in% NA)")}} else set$note_source <- note_source; rm(note_source)
	
	if(set$web){set$web <- "filter(stringr:::str_sub(info,-1,-1) %in% '1') %>% mutate(note_indicator = discard_note(., 'ind'), note_source = discard_note(., 'src'))"} else(set$web <- NULL)
	
	if(!is.null(ref_area)) {if(unique(ref_area %in% 'all') %in% TRUE) ref_area <- NULL}

	options(warn = -1)
	
	set$ref_col <- paste0(paste0('dplyr::starts_with(',set$g1,  c(	ifelse(set$TS, 'key', ''),
																	ifelse(!set$TS, 'collection', ''), 
																	ifelse(!set$TS, 'ref_area', ''), 
																	ifelse(!set$TS, 'source', ''), 
																	ifelse(!set$TS, 'indicator', ''), 
																	ifelse(!set$TS, 'sex', ''), 
																	ifelse(!set$TS, 'classif1', ''), 
																	ifelse(!set$TS, 'classif2', ''), 
																	'time', 'obs_value', 'obs_status', 'note', 
																	ifelse(set$info, 'info', '')), set$g1, ')'), collapse = ', ') %>% 
															stringr::str_replace_all(stringr::fixed(paste0(', dplyr::starts_with(',set$g1,'', set$g1, ')')), '') %>% 
															stringr::str_replace_all(stringr::fixed(paste0('dplyr::starts_with(',set$g1,'', set$g1, '), ')), '')
	set$Q2 <- paste0(	c(paste0('select(', set$ref_col	,')')), 
					collapse = ' %>% ')	
	set$info <- info
	set$DB <- cbind(collection, freq) %>% as.data.frame %>% as.tbl %>% mutate_if(is.factor, funs(as.character))%>% tidyr::expand(collection, freq) %>% tidyr::unite(a , collection, freq) %>% t %>% as.character
	set$DB <- set$DB[set$DB %in% {ilo$segment %>% distinct(files) %>% t %>% as.character}]
	path_ilo <- lazyeval::lazy_dots(ilo$path)

	path_ilo <- eval(path_ilo[[1]]$expr, envir =  path_ilo[[1]]$env) %>% paste0(.,'/data/cou')
 
	plyr::llply(as.list(set$DB),
		function(j){	
			
			ENV <- new.env()
			if( !is.null(ref_area) ) {
				test = try(lazyLoad(paste0(path_ilo,'/',j ), ENV, filter = function(x) x %in% c(ref_area)), silent = TRUE)
				if(!is.null(test)) return(NULL)
				if(!ref_area %in% names(ENV)) return(NULL)
			} else {
				test = try(lazyLoad(paste0(path_ilo,'/',j ), ENV), silent = TRUE)
				if(!is.null(test)) return(NULL)
			}
		ref <- names(ENV) %>% sort
			foreach::foreach(i=1:length(ref), .export = c('ENV', 'set', add, 'package'), .packages=c(package, 'dplyr')) %cl%
					{		if(!is.null(package)) {if(package %in% 'ilo') init_ilo(-cl)}
							if(set$label %in% TRUE){
							MyQuery <- paste(c(
								paste0( 'res <- ENV[[',paste('',ref[i],'', sep=set$g1),']]'),
								paste0(	'{if(!is.null(.)) mutate(., collection =',paste('',strsplit(j, '_')[[1]][1],'', sep=set$g1),') else .}'),
								paste0(	'{if(!is.null(.)) mutate(., ref_area =',paste('',ref[i],'', sep=set$g1),') else .}') ,
								set$web,
								'switch_ilo(keep)', 
								gsub('indicator', 'indicator.label', set$indicator),
								gsub('source', 'source.label', set$source),
								gsub('sex', 'sex.label',set$sex),
								gsub('classif1.label', 'classif1.label', set$classif1),
								gsub('classif2.label', 'classif2.label',set$classif2),
								set$time,
								set$timefrom,
								set$timeto,
								gsub('obs_status','obs_status.label',set$obs_status),
								gsub('note_classif','note_classif.label',set$note_classif),
								gsub('note_indicator','note_indicator.label',set$note_indicator),
								gsub('note_source','note_source.label',set$note_source),
								set$info,
								set$Q2,
								set$query,
								set$Q3
								)
								, collapse=' %>% ')
							} else {
							MyQuery <- paste(c(
								paste0( 'res <- ENV[[',paste('',ref[i],'', sep=set$g1),']]'),
								paste0(	'{if(!is.null(.)) mutate(., collection =',paste('',strsplit(j, '_')[[1]][1],'', sep=set$g1),') else .}'),
								paste0(	'{if(!is.null(.)) mutate(., ref_area =',paste('',ref[i],'', sep=set$g1),') else .}') ,
								set$web,
								set$indicator,
								set$source,
								set$sex,
								set$classif1,
								set$classif2,
								set$time,
								set$timefrom,
								set$timeto,
								set$obs_status,
								set$note_classif,
								set$note_indicator,
								set$note_source,
								set$info,
								paste0('{if(set$TS & !is.null(.)) tidyr::unite(., notes,  obs_status, note_classif, note_indicator, note_source, sep = ',set$g2,', remove = TRUE) else .}'),
								paste0('{if(set$TS & !is.null(.)) tidyr::unite(., key,  collection, ref_area, source, indicator, sex, classif1, classif2, sep = ',set$g2,', remove = TRUE) else .}'),
								set$Q2,
								set$query,
								set$Q3
								)
								, collapse=' %>% ')
							}
							
							eval(parse(text=MyQuery)) %>% 
						{invisible(gc(reset = TRUE)); .}
					} %>% 
					bind_rows  %>% {invisible(gc(reset = TRUE)); .}
		} 
	 ) %>% 
	bind_rows %>% 
	# mutate_if(is.character, funs(factor(., exclude = NULL)))  %>% 
	
	{invisible(gc(reset = TRUE)); .}
	


}

#' @export
discard_note <- function(X, columns){


if(columns %in% 'ind' & nrow(ilo$code$cl_note_delete %>% filter(type %in% 'ind'))>0 ){
if(nrow(X %>% filter(!note_indicator %in% NA))==0) {return(X %>% select(note_indicator) %>% t %>% as.character)}

X <- X %>% select(indicator, note_indicator)
if(nrow(ilo$code$cl_note_delete %>% filter(indicator %in% NA, type %in% 'ind')) >0 ){

ind <- paste0(ilo$code$cl_note_delete %>% filter(indicator %in% NA, type %in% 'ind') %>% mutate(note = paste0('_',note,'_')) %>% select(note) %>% t %>% as.character, collapse = "|" )
	 X <- X %>% mutate(note_indicator = paste0("_", note_indicator %>% str_replace_all('_', '__'),"_"), 
						note_indicator = note_indicator %>% str_replace_all(ind, ''), 
						note_indicator = note_indicator %>% str_replace_all('__', '_'), 
						note_indicator = note_indicator %>% str_sub(2,-2), 
						note_indicator = ifelse(note_indicator %in% c('', 'NA' ), NA, note_indicator))
rm(ind) 
} 
 
if(nrow(ilo$code$cl_note_delete %>% filter(!indicator %in% NA, type %in% 'ind', indicator %in% unique(X$indicator))) >0 ){ 
test <-  ilo$code$cl_note_delete %>% filter(!indicator %in% NA , type %in% 'ind', indicator %in% unique(X$indicator)) 
ref <-  paste0('_', unique(test$note), '_')
X <- X %>% mutate(note_indicator = paste0("_", note_indicator %>% str_replace_all('_', '__'),"_")) 
 for (i in 1:length(ref)){
 
	ind <-  paste0(ref[i], collapse = "|" )

 a <- as.character(test %>% filter(note %in% ref[i]) %>% select(indicator) %>% t )
 
 X <- X %>% mutate(	note_indicator = 	ifelse( indicator %in% a, 
												yes =  note_indicator %>% str_replace_all(src, ''), 
												no = note_indicator))
				
rm(ind, a)
 }
  X <- X %>% mutate(note_indicator = note_indicator %>% str_replace_all('__', '_'), 
					note_indicator = note_indicator %>% str_sub(2,-2), 
					note_indicator = ifelse(note_indicator %in% c('', 'NA' ), NA, note_indicator)) 
  

 rm(test, ref, i)
} 

return(X %>% select(note_indicator) %>% t %>% as.character)

} 

if(columns %in% 'src' & nrow(ilo$code$cl_note_delete %>% filter(type %in% 'src'))>0 ){ 
if(nrow(X %>% filter(!note_source %in% NA))==0) {return(X %>% select(note_indicator) %>% t %>% as.character)}
X <- X %>% select(indicator, note_source)
if(nrow(ilo$code$cl_note_delete %>% filter(indicator %in% NA, type %in% 'src')) >0 ){

src <- paste0(ilo$code$cl_note_delete %>% filter(indicator %in% NA, type %in% 'src') %>% mutate(note = paste0('_',note,'_')) %>% select(note) %>% t %>% as.character, collapse = "|" )
 X <- X %>% mutate(	note_source = paste0("_", note_source %>% str_replace_all('_', '__'),"_"), 
					note_source = note_source %>% str_replace_all(src, ''), 
					note_source = note_source %>% str_replace_all('__', '_'), 
					note_source = note_source %>% str_sub(2,-2), 
					note_source = ifelse(note_source %in% c('', 'NA' ), NA, note_source))
rm(src) 				
	
}

if(nrow(ilo$code$cl_note_delete %>% filter(!indicator %in% NA, type %in% 'src', indicator %in% unique(X$indicator))) >0 ){ 
test <-  ilo$code$cl_note_delete %>% filter(!indicator %in% NA , type %in% 'src', indicator %in% unique(X$indicator)) 
ref <-  paste0('_', unique(test$note), '_')
 X <- X %>% mutate(note_source = paste0("_", note_source %>% str_replace_all('_', '__'),"_")) 
 for (i in 1:length(ref)){
 
		src <- paste0(ref[i], collapse = "|" )

		a <- as.character(test %>% filter(note %in% ref[i]) %>% select(indicator) %>% t )
 
		X <- X %>% mutate(	note_source = 	ifelse( indicator %in% a, 
												yes =  note_source %>% str_replace_all(src, ''), 
												no = note_source))
				
rm(src, a)
 }
  X <- X %>% mutate(note_source = note_source %>% str_replace_all('__', '_'), 
					note_source = note_source %>% str_sub(2,-2), 
					note_source = ifelse(note_source %in% c('', 'NA' ), NA, note_source)) 
  
 rm(test, ref, i)

}
return(X %>% select(note_source) %>% t %>% as.character)
}


}

#' @export
get_ilo2 <- function(...){

invisible(gc(reset = TRUE))
invisible(gc(reset = TRUE))

'%cl%' <- foreach::'%dopar%'
	collection = 'YI'
	freq = 'A' 
	ref_area = NULL
	indicator = NULL
	source  = NULL
	sex = NULL
	classif1 = NULL
	classif2 = NULL
	timefrom = NULL
	timeto = NULL
	time = NULL
	obs_status = NULL 
	note_classif = NULL 
	note_indicator = NULL 
	note_source = NULL
	query = NULL 
	style = 'none' 
	add = NULL 
	package = NULL
	info = NULL
	web <- NULL
	set <- list()
	.dots  = lazyeval::lazy_dots(...)

	set$para <- .dots[!names(.dots)%in%'']
	if(length(set$para)>0){
		for (i in 1:length(names(set$para))){
							
			if(tolower(names(set$para[i])) %in% 'add'){
				eval(expression(assign(as.character(set$para[[i]]$expr), eval(set$para[[i]]$expr, envir =  set$para[[i]]$env))))
				add <- as.character(set$para[[i]]$expr)
			} else {
				eval(expression(assign(names(set$para[i]), eval( set$para[[i]]$expr, envir =  set$para[[i]]$env))))
			}
		}	
	} ; set$para <- NULL
	set$keep <- .dots[names(.dots)%in%''] 
	if(!is.null(info)) { 
			info <- paste0("filter(stringr::str_detect(info, '",info,"'))") 
			if(length(set$keep) > 0) {set$keep <- c(set$keep, lazyeval::lazy_dots(info))
			} else set$keep <- lazyeval::lazy_dots(info)
			} 
	
	set$g1 <- '\''; set$g2 <- paste0(set$g1, '/', set$g1)

	set$TS <- ifelse(style %in% 'ts', TRUE, FALSE)

	if(collection %in% 'all') 	collection <- ilo$segment %>% distinct(collection) %>% t %>% as.character
	if(freq %in% 'all') 		freq <- ilo$segment %>% distinct(freq) %>% t %>% as.character
	keep = NULL
	set$info <- FALSE
	set$label <- FALSE
	set$web <- FALSE
	set$vs <- FALSE
	if(length(set$keep) > 0){
		for (i in 1:length(set$keep)){
			condi <- set$keep[[i]]$expr %>% as.character
			neg <- NULL
			if(length(condi) ==2) {
				neg <- condi[1] 
				condi <- condi[2]
			}

			if(condi %in% c('info', 'label', 'web')){
				if(condi %in% 'info') {set$info <- TRUE}
				if(condi %in% 'label') {set$label <- TRUE; package <- c(package, 'ilo')}
				if(condi %in% 'web') {set$web <- TRUE; package <- c(package, 'ilo')}
			} else{
				condi <- paste0(neg, 'dplyr::starts_with(', set$g1,condi,set$g1, ')')
				keep <- paste0( c(keep, condi), collapse = ', ')
			}
		}	
		if(!is.null(keep)) {
			keep <- paste0('select(',keep,')')	
		}
	}

	set$Q3 <- keep; set$keep <- NULL
	set$query <- query ; rm(query) 
	if(!is.null(ref_area)) { set$ref_area <- paste0("filter(stringr::str_detect(ref_area, '",ref_area,"'))")} else set$ref_area <- ref_area; rm(ref_area)
	if(!is.null(source)) { set$source <- paste0("filter(stringr::str_detect(source, '",source,"'))")} else set$source <- source; rm(source)
	if(!is.null(sex)) { set$sex <- paste0("filter(stringr::str_detect(sex, '",sex,"'))")} else set$sex <- sex; rm(sex)
	if(!is.null(classif1)) { set$classif1 <- paste0("filter(stringr::str_detect(classif1, '",classif1,"'))")} else set$classif1 <- classif1; rm(classif1)
	if(!is.null(classif2)) { set$classif2 <- paste0("filter(stringr::str_detect(classif2, '",classif2,"'))")} else set$classif2 <- classif2; rm(classif2)
	if(!is.null(time)) { set$time <- paste0("filter(stringr::str_detect(time, '",time,"'))")} else set$time <- time #; rm(time)
	if(!is.null(timefrom)) { set$timefrom <- paste0("filter(as.numeric(substr(time,1,4)) > {as.numeric(",timefrom,")-1})")} else set$timefrom <- timefrom; rm(timefrom)
	if(!is.null(timeto)) { set$timeto <- paste0("filter(as.numeric(substr(time,1,4)) < {as.numeric(",timeto,")+1})")} else set$timeto <- timeto; rm(timeto)
	
	if(!is.null(obs_status)) { set$obs_status <- paste0("filter(stringr::str_detect(obs_status, '",obs_status,"'))")} else set$obs_status <- obs_status; rm(obs_status)
	if(!is.null(note_classif)) { set$note_classif <- paste0("filter(stringr::str_detect(note_classif, '",note_classif,"'))")} else set$note_classif <- note_classif; rm(note_classif)
	if(!is.null(note_indicator)) { set$note_indicator <- paste0("filter(stringr::str_detect(note_indicator, '",note_indicator,"'))")} else set$note_indicator <- note_indicator; rm(note_indicator)
	if(!is.null(note_source)) { set$note_source <- paste0("filter(stringr::str_detect(note_source, '",note_source,"'))")} else set$note_source <- note_source; rm(note_source)
	if(set$web){set$web <- "filter(stringr:::str_sub(info,-1,-1) %in% '1') %>% mutate(note_indicator = discard_note(., 'ind'), note_source = discard_note(., 'src'))"} else(set$web <- NULL)
	
	if(!is.null(indicator)) {if(unique(indicator %in% 'all') %in% TRUE) indicator <- NULL}

	options(warn = -1)
	
	set$ref_col <- paste0(paste0('dplyr::starts_with(',set$g1,  c(	ifelse(set$TS, 'key', ''),
																	ifelse(!set$TS, 'collection', ''), 
																	ifelse(!set$TS, 'ref_area', ''), 
																	ifelse(!set$TS, 'source', ''), 
																	ifelse(!set$TS, 'indicator', ''), 
																	ifelse(!set$TS, 'sex', ''), 
																	ifelse(!set$TS, 'classif1', ''), 
																	ifelse(!set$TS, 'classif2', ''), 
																	'time', 'obs_value', 'obs_status', 'note', 
																	ifelse(set$info, 'info', '')), set$g1, ')'), collapse = ', ') %>% 
															stringr::str_replace_all(stringr::fixed(paste0(', dplyr::starts_with(',set$g1,'', set$g1, ')')), '') %>% 
															stringr::str_replace_all(stringr::fixed(paste0('dplyr::starts_with(',set$g1,'', set$g1, '), ')), '')
	set$Q2 <- paste0(	c(paste0('select(', set$ref_col	,')')), 
					collapse = ' %>% ')	
	set$info <- info
	set$DB <- cbind(collection, freq) %>% as.data.frame %>% as.tbl %>% mutate_if(is.factor, funs(as.character))%>% tidyr::expand(collection, freq) %>% tidyr::unite(a , collection, freq) %>% t %>% as.character
	set$DB <- set$DB[set$DB %in% {ilo$segment %>% distinct(files) %>% t %>% as.character}]
	path_ilo <- lazyeval::lazy_dots(ilo$path)

	path_ilo <- eval(path_ilo[[1]]$expr, envir =  path_ilo[[1]]$env) %>% paste0(.,'/data/ind')
 
	plyr::llply(as.list(set$DB),
		function(j){	
			
			ENV <- new.env()
			if( !is.null(indicator) ) {
				test = try(lazyLoad(paste0(path_ilo,'/',j ), ENV, filter = function(x) x %in% c(indicator)), silent = TRUE)
				if(!is.null(test)) return(NULL)
				if(!indicator %in% names(ENV)) return(NULL)
			} else {
				test = try(lazyLoad(paste0(path_ilo,'/',j ), ENV), silent = TRUE)
				if(!is.null(test)) return(NULL)
			}
		ref <- names(ENV) %>% sort
			foreach::foreach(i=1:length(ref), .export = c('ENV', 'set', add, 'package'), .packages=c(package, 'dplyr')) %cl%
					{		if(!is.null(package)) {if(package %in% 'ilo') init_ilo(-cl)}
							if(set$label %in% TRUE){
							MyQuery <- paste(c(
								paste0( 'res <- ENV[[',paste('',ref[i],'', sep=set$g1),']]'),
								paste0(	'{if(!is.null(.)) mutate(., collection =',paste('',strsplit(j, '_')[[1]][1],'', sep=set$g1),') else .}'),
								paste0(	'{if(!is.null(.)) mutate(., indicator =',paste('',ref[i],'', sep=set$g1),') else .}') ,
								set$web,
								'switch_ilo(keep)', 
								gsub('ref_area', 'ref_area.label', set$ref_area),
								gsub('source', 'source.label', set$source),
								gsub('sex', 'sex.label',set$sex),
								gsub('classif1.label', 'classif1.label', set$classif1),
								gsub('classif2.label', 'classif2.label',set$classif2),
								set$time,
								set$timefrom,
								set$timeto,
								gsub('obs_status','obs_status.label',set$obs_status),
								gsub('note_classif','note_classif.label',set$note_classif),
								gsub('note_indicator','note_indicator.label',set$note_indicator),
								gsub('note_source','note_source.label',set$note_source),
								set$info,
								set$Q2,
								set$query,
								set$Q3
								)
								, collapse=' %>% ')
							} else {
							MyQuery <- paste(c(
								paste0( 'res <- ENV[[',paste('',ref[i],'', sep=set$g1),']]'),
								paste0(	'{if(!is.null(.)) mutate(., collection =',paste('',strsplit(j, '_')[[1]][1],'', sep=set$g1),') else .}'),
								paste0(	'{if(!is.null(.)) mutate(., indicator =',paste('',ref[i],'', sep=set$g1),') else .}') ,
								set$web,
								set$ref_area,
								set$source,
								set$sex,
								set$classif1,
								set$classif2,
								set$time,
								set$timefrom,
								set$timeto,
								set$obs_status,
								set$note_classif,
								set$note_indicator,
								set$note_source,
								set$info,
								paste0('{if(set$TS & !is.null(.)) tidyr::unite(., notes,  obs_status, note_classif, note_indicator, note_source, sep = ',set$g2,', remove = TRUE) else .}'),
								paste0('{if(set$TS & !is.null(.)) tidyr::unite(., key,  collection, ref_area, source, indicator, sex, classif1, classif2, sep = ',set$g2,', remove = TRUE) else .}'),
								set$Q2,
								set$query,
								set$Q3
								)
								, collapse=' %>% ')
							}
							
							eval(parse(text=MyQuery)) %>% 
						{invisible(gc(reset = TRUE)); .}
					} %>% 
					bind_rows  %>% {invisible(gc(reset = TRUE)); .}
		} 
	 ) %>% 
	bind_rows %>% 
	# mutate_if(is.character, funs(factor(., exclude = NULL)))  %>% 
	
	{invisible(gc(reset = TRUE)); .}
	


}




