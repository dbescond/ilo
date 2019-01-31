#' R tools to manipulate ilo datasets code / label
#'
#' ilo package provides a flexible grammar of ilo data manipulation. Associate with dplyr, tidyr and others packages developped by Hadley Wickham & Co, it provide a faster way to work labour statistics from the ilo. 
#'
#' @param df ilo tbl data frame to manipulate.
#' @param ... Specification of variables to get.
#' @section Specification:
#' others functions allow advanced manipulation.
#' \itemize{
#'  \item \code{version}: return data frame with classif version code,
#'  \item \code{keep}: keep code when label is ask,
#' }
#' To learn more about ilo, start with the online vignettes:
#'
#' @name switch_ilo
#' @importFrom plyr llply mapvalues
#' @importFrom foreach foreach "%dopar%"
#' @importFrom doSNOW registerDoSNOW
#' @importFrom rappdirs user_data_dir
#' @importFrom openxlsx createWorkbook addWorksheet writeData saveWorkbook
#' @import dplyr
#' @importFrom tidyr expand unite separate
#' @importFrom stringr str_replace_all str_detect str_split_fixed fixed str_c str_split str_replace
#'
#'
#' @return a tbl data frame
#'
#' @examples
#' ### quarterly female unemployed as from 2000 in united states
#'
#' res <-  get_ilo(collection = 'STI', 
#'		freq = 'Q', ref_area = 'USA', 
#'		indicator = 'UNE_TUNE_SEX_AGE_NB', 
#'		timefrom = '2000', sex = 'F', classif1 = 'AGGREGATE_TOTAL')
#' res %>% switch_ilo(keep)
#'
#' ### get version code
#'
#' res %>% switch_ilo(version)
#'
#' ### get label of collection and contry only (don't keep code)
#'
#' res %>% switch_ilo(collection, ref_area)
#'
#' @export

switch_ilo <- function(df, ...){

invisible(gc(reset = TRUE))
invisible(gc(reset = TRUE))

style = FALSE
.dots  = lazyeval::lazy_dots(...)

variable <- sapply(.dots, function(x) {ifelse(is.null(as.character(x$expr)), NULL ,paste0(as.character(x$expr), '/'))})
rm(.dots)
if(length(variable) == 0){TEST <- tolower(paste0('/',paste0(paste0(colnames(df), '/'), collapse = '')))	}
if(!length(variable) == 0) {TEST <- paste0('/',tolower(variable), collapse = '')}
if(length(variable) == 1) {if(variable %in% 'keep/') TEST <- tolower(paste0('/',paste0(paste0(c('keep',colnames(df)), '/'), collapse = '')))}

variable <- TEST; rm(TEST)

df %>% 	
{if(str_detect(variable, '/collection/')){	
			separate(., collection, c("collection.label"), sep=" ", extra = "drop", remove = FALSE)  %>%
			mutate(collection.label 		= 	factor(	collection, exclude = NULL) 	%>%
								plyr:::mapvalues(  from 	= 	filter(select(ilo$code$cl_indicator_collection, code), code %in% levels(.)) %>% t %>% as.character, 
											to 		= 	filter(select_(ilo$code$cl_indicator_collection, .dots = c('code', label = paste0('label_',ilo$lang))), code %in% levels(.)) %>% select(label) %>% t %>% as.character, 
									warn_missing = FALSE)) %>% 
		{if(!str_detect(variable, '/keep/')) select(., -collection) else .}
	} else .}	%>% {invisible(gc(reset = TRUE)); .} %>%
{if(str_detect(variable, '/ref_area/')){	
			separate(., ref_area, c("ref_area.label"), sep=" ", extra = "drop", remove = FALSE)  %>%
			mutate(ref_area.label 		= 	factor(	ref_area, exclude = NULL) 	%>%
								plyr:::mapvalues(  from 	= 	filter(select(ilo$code$cl_country, code), code %in% levels(.)) %>% t %>% as.character, 
											to 		= 	filter(select_(ilo$code$cl_country, .dots = c('code', label = paste0('label_',ilo$lang))), code %in% levels(.)) %>% select(label) %>% t %>% as.character, 
									warn_missing = FALSE)) %>% 
		{if(!str_detect(variable, '/keep/')) select(., -ref_area) else .}
	}else .}	%>% {invisible(gc(reset = TRUE)); .} %>%

{if(str_detect(variable, '/source/')){	
			separate(., source, c("source.label"), sep=" ", extra = "drop", remove = FALSE)  %>%
			mutate(source.label 		= 	factor(as.character(readr::parse_number(source)), exclude = NULL) %>%
								plyr:::mapvalues(	from 	= 	filter(select(ilo$code$cl_survey %>% mutate(code = as.character(readr::parse_number(code))), code), code %in% levels(.)) %>% t %>% as.character, 
											to 		= 	filter(select_(ilo$code$cl_survey %>% mutate(code = as.character(readr::parse_number(code))), .dots = c('code', label = paste0('label_',ilo$lang))), code %in% levels(.)) %>% select(label) %>% t %>% as.character, 
									warn_missing = FALSE)) %>% 
		{if(!str_detect(variable, '/keep/')) select(., -source) else .}
	} else .}	%>% {invisible(gc(reset = TRUE)); .} %>%

{if(str_detect(variable, '/indicator/')){	
			separate(., indicator, c("indicator.label"), sep=" ", extra = "drop", remove = FALSE)  %>%
			mutate(indicator.label 	= 	factor(	indicator, exclude = NULL) %>%
								plyr:::mapvalues(	from 	= 	filter(select(ilo$code$cl_indicator, code), code %in% levels(.)) %>% t %>% as.character, 
											to 		= 	filter(select_(ilo$code$cl_indicator, .dots = c('code', label = paste0('label_',ilo$lang))), code %in% levels(.)) %>% select(label) %>% t %>% as.character, 
										warn_missing = FALSE)) %>% 
		{if(!str_detect(variable, '/keep/')) select(., -indicator) else .}
	} else .}	%>% {invisible(gc(reset = TRUE)); .} %>%

{if(str_detect(variable, '/sex/|/classif1/|/classif2/')){	
			separate(., sex, c("sex.label"), sep=" ", extra = "drop", remove = FALSE)  %>%
			separate(classif1, c("classif1.label"), sep=" ", extra = "drop", remove = FALSE)  %>%
			separate(classif2, c("classif2.label"), sep=" ", extra = "drop", remove = FALSE)  %>%
			mutate(sex.label 		= 	factor(sex , exclude = NULL) %>%
								plyr:::mapvalues(	from 	= 	filter(select(ilo$code$cl_classif, code), code %in% levels(.)) %>% t %>% as.character, 
											to 		= 	filter(select_(ilo$code$cl_classif, .dots = c('code', label = paste0('label_',ilo$lang))), code %in% levels(.)) %>% select(label) %>% t %>% as.character, 
										warn_missing = FALSE),
				classif1.label	=	factor(classif1 , exclude = NULL)  %>%
								plyr:::mapvalues(	from 	= 	filter(select(ilo$code$cl_classif, code), code %in% levels(.)) %>% t %>% as.character, 
											to 		= 	filter(select_(ilo$code$cl_classif, .dots = c('code', label = paste0('label_',ilo$lang))), code %in% levels(.)) %>% select(label) %>% t %>% as.character, 
										warn_missing = FALSE),
				classif2.label	=	factor(classif2 , exclude = NULL) %>%
								plyr:::mapvalues(	from 	= 	filter(select(ilo$code$cl_classif, code), code %in% levels(.)) %>% t %>% as.character, 
											to 		= 	filter(select_(ilo$code$cl_classif, .dots = c('code', label = paste0('label_',ilo$lang))), code %in% levels(.)) %>% select(label) %>% t %>% as.character, 
										warn_missing = FALSE)) %>% 
		{if(!str_detect(variable, '/keep/')) select(., -sex,-classif1,-classif2) else .}
	} else .}	%>% {invisible(gc(reset = TRUE)); .} %>%
{if(str_detect(variable, '/classif/'))
		mutate	(., 
				classif	= 	factor(classif , exclude = NULL), 
				sex 		= 	classif %>% 
								plyr:::mapvalues( 	from 	= 	levels(.), 
											to 		= 	str_split_fixed(levels(.), fixed('/'), n = 2)[,1] , 
									warn_missing = FALSE) %>%
								plyr:::mapvalues(	from 	= 	filter(select(ilo$code$cl_classif, code), code %in% levels(.)) %>% t %>% as.character, 
											to 		= 	filter(select_(ilo$code$cl_classif, .dots = c('code', label = paste0('label_',ilo$lang))), code %in% levels(.)) %>% select(label) %>% t %>% as.character, 
										warn_missing = FALSE),
				classif1	=	classif %>% 
								plyr:::mapvalues( 	from 	= 	levels(.), 
											to 		= 	str_split_fixed(levels(.), fixed('/'), n = 3)[,2] , 
									warn_missing = FALSE)%>%
								plyr:::mapvalues(	from 	= 	filter(select(ilo$code$cl_classif, code), code %in% levels(.)) %>% t %>% as.character, 
											to 		= 	filter(select_(ilo$code$cl_classif, .dots = c('code', label = paste0('label_',ilo$lang))), code %in% levels(.)) %>% select(label) %>% t %>% as.character, 
										warn_missing = FALSE),
				classif2	=	classif %>% 
								plyr:::mapvalues( 	from 	= 	levels(.), 
											to 		= 	str_split_fixed(levels(.), fixed('/'), n = 3)[,3] , 
									warn_missing = FALSE)%>%
								plyr:::mapvalues(	from 	= 	filter(select(ilo$code$cl_classif, code), code %in% levels(.)) %>% t %>% as.character, 
											to 		= 	filter(select_(ilo$code$cl_classif, .dots = c('code', label = paste0('label_',ilo$lang))), code %in% levels(.)) %>% select(label) %>% t %>% as.character, 
										warn_missing = FALSE)) %>%
				select(-classif) %>%
				{if(style) mutate(., classif = str_c(sex,classif1, classif2, sep = '/')) %>% select(-sex, -classif1, -classif2) else .} 
	else .}	%>% {invisible(gc(reset = TRUE)); .} %>%
{if(str_detect(variable, '/obs_status/|/note_classif/|/note_indicator/|/note_source/')){	
			separate(., obs_status, c("obs_status.label"), sep=" ", extra = "drop", remove = FALSE)  %>%
			separate(note_classif, c("note_classif.label"), sep=" ", extra = "drop", remove = FALSE)  %>%
			separate(note_indicator, c("note_indicator.label"), sep=" ", extra = "drop", remove = FALSE)  %>%
			separate(note_source, c("note_source.label"), sep=" ", extra = "drop", remove = FALSE)  %>%
			mutate(obs_status.label		=	factor(obs_status, exclude = NULL) %>% 
								plyr:::mapvalues(	from 	= 	filter(select(ilo$code$cl_note, code), code %in% levels(.)) %>% t %>% as.character, 
											to 		= 	filter(select_(ilo$code$cl_note, .dots = c('code', label = paste0('label_',ilo$lang))), code %in% levels(.)) %>% select(label) %>% t %>% as.character, 
										warn_missing = FALSE),
				note_classif.label	=	factor(note_classif, exclude = NULL) %>%
								plyr:::mapvalues( 	from 	= 	levels(.), 
											to 		= 	str_split(levels(.),fixed('_')) %>% llply(function(x){
																x %>% 	plyr:::mapvalues(	from 	= 	filter(select(ilo$code$cl_note, code), code %in% levels(as.factor(.))) %>% t %>% as.character, 
																					to 		= 	filter(select_(ilo$code$cl_note, .dots = c('code', label = paste0('label_',ilo$lang))), code %in% levels(as.factor(.))) %>% select(label) %>% t %>% as.character, 
																		warn_missing = FALSE) %>%
																		str_c(., collapse = ' | ')})  %>% unlist , 
									warn_missing = FALSE),
				note_indicator.label	=	factor(note_indicator, exclude = NULL) %>%
								plyr:::mapvalues( 	from 	= 	levels(.), 
											to 		= 	str_split(levels(.),fixed('_')) %>% llply(function(x){
																x %>% 	plyr:::mapvalues(	from 	= 	filter(select(ilo$code$cl_note, code), code %in% levels(as.factor(.))) %>% t %>% as.character, 
																					to 		= 	filter(select_(ilo$code$cl_note, .dots = c('code', label = paste0('label_',ilo$lang))), code %in% levels(as.factor(.))) %>% select(label) %>% t %>% as.character, 
																		warn_missing = FALSE) %>%
																		str_c(., collapse = ' | ')})  %>% unlist , 
									warn_missing = FALSE),
				note_source.label		=	factor(note_source, exclude = NULL) %>% 
								plyr:::mapvalues( 	from 	= 	levels(.), 
											to 		= 	str_split(levels(.),fixed('_')) %>% llply(function(x){
																x %>% 	plyr:::mapvalues(	from 	= 	filter(select(ilo$code$cl_note, code), code %in% levels(as.factor(.))) %>% t %>% as.character, 
																					to 		= 	filter(select_(ilo$code$cl_note, .dots = c('code', label = paste0('label_',ilo$lang))), code %in% levels(as.factor(.))) %>% select(label) %>% t %>% as.character, 
																		warn_missing = FALSE) %>%
																		str_c(., collapse = ' | ')})  %>% unlist ,  
									warn_missing = FALSE)) %>%
				{if(style) mutate(., Notes = str_c(obs_status, note_classif, note_indicator, note_source, sep = '|')) %>% select(-obs_status, -note_classif, -note_indicator, -note_source) else .} %>% 
		{if(!str_detect(variable, '/keep/')) select(., -obs_status,-note_classif,-note_indicator,-note_source) else .}
	} else .}	%>% {invisible(gc(reset = TRUE)); .} %>%	
{if(str_detect(variable, '/note/'))	
		mutate	(.,
				note = factor(note, exclude = NULL),
				obs_status	=	note %>% 
								plyr:::mapvalues( 	from 	= 	levels(.), 
											to 		= 	str_split_fixed(levels(.), fixed('/'), n = 2)[,1] , 
									warn_missing = FALSE) %>%
								plyr:::mapvalues(	from 	= 	filter(select(ilo$code$cl_note, code), code %in% levels(.)) %>% t %>% as.character, 
											to 		= 	filter(select_(ilo$code$cl_note, .dots = c('code', label = paste0('label_',ilo$lang))), code %in% levels(.)) %>% select(label) %>% t %>% as.character, 
										warn_missing = FALSE),
				note_classif	=	note %>% 
								plyr:::mapvalues( 	from 	= 	levels(.), 
											to 		= 	str_split_fixed(levels(.), fixed('/'), n = 3)[,2] , 
									warn_missing = FALSE)%>% 
								plyr:::mapvalues( 	from 	= 	levels(.), 
											to 		= 	str_split(levels(.),fixed('_')) %>% llply(function(x){
																x %>% 	plyr:::mapvalues(	from 	= 	filter(select(ilo$code$cl_note, code), code %in% levels(as.factor(.))) %>% t %>% as.character, 
																					to 		= 	filter(select_(ilo$code$cl_note, .dots = c('code', label = paste0('label_',ilo$lang))), code %in% levels(as.factor(.))) %>% select(label) %>% t %>% as.character, 
																		warn_missing = FALSE) %>%
																		str_c(., collapse = ' | ')})  %>% unlist , 
									warn_missing = FALSE),
				note_indicator	=	note %>% 
								plyr:::mapvalues( 	from 	= 	levels(.), 
											to 		= 	str_split_fixed(levels(.), fixed('/'), n = 4)[,3] , 
									warn_missing = FALSE) %>% 
								plyr:::mapvalues( 	from 	= 	levels(.), 
											to 		= 	str_split(levels(.),fixed('_')) %>% llply(function(x){
																x %>% 	plyr:::mapvalues(	from 	= 	filter(select(ilo$code$cl_note, code), code %in% levels(as.factor(.))) %>% t %>% as.character, 
																					to 		= 	filter(select_(ilo$code$cl_note, .dots = c('code', label = paste0('label_',ilo$lang))), code %in% levels(as.factor(.))) %>% select(label) %>% t %>% as.character, 
																		warn_missing = FALSE) %>%
																		str_c(., collapse = ' | ')})  %>% unlist , 
									warn_missing = FALSE),
				note_source	=	note %>% 
								plyr:::mapvalues( 	from 	= 	levels(.), 
											to 		= 	str_split_fixed(levels(.), fixed('/'), n = 5)[,4] , 
									warn_missing = FALSE) %>%
								plyr:::mapvalues( 	from 	= 	levels(.), 
											to 		= 	str_split(levels(.),fixed('_')) %>% llply(function(x){
																x %>% 	plyr:::mapvalues(	from 	= 	filter(select(ilo$code$cl_note, code), code %in% levels(as.factor(.))) %>% t %>% as.character, 
																					to 		= 	filter(select_(ilo$code$cl_note, .dots = c('code', label = paste0('label_',ilo$lang))), code %in% levels(as.factor(.))) %>% select(label) %>% t %>% as.character, 
																		warn_missing = FALSE) %>%
																		str_c(., collapse = ' | ')})  %>% unlist , 
									warn_missing = FALSE)) %>%
			select(-note) %>%
				{if(style) mutate(., note = str_c(obs_status, note_classif, note_indicator, note_source, sep = '/')) %>% select(-obs_status, -note_classif, -note_indicator, -note_source) else .} 
	else .}	%>% {invisible(gc(reset = TRUE)); .} %>%
{if(str_detect(variable, '/info/'))	
		mutate	(.,
				info = factor(info, exclude = NULL),
				info_laststatus	=	info %>% 
								plyr:::mapvalues( 	from 	= 	levels(.), 
											to 		= 	str_split_fixed(levels(.), fixed(':'), n = 2)[,1] , 
									warn_missing = FALSE) %>%
								plyr:::mapvalues(	from 	= 	filter(select(ilo$code$cl_z, code), code %in% levels(.)) %>% t %>% as.character, 
											to 		= 	filter(select_(ilo$code$cl_z, .dots = c('code', label = paste0('label_',ilo$lang))), code %in% levels(.)) %>% select(label) %>% t %>% as.character, 
										warn_missing = FALSE),
				info_loadmode	=	info %>% 
								plyr:::mapvalues( 	from 	= 	levels(.), 
											to 		= 	str_split_fixed(levels(.), fixed(':'), n = 3)[,2] , 
									warn_missing = FALSE)%>% 
								plyr:::mapvalues(	from 	= 	filter(select(ilo$code$cl_z, code), code %in% levels(.)) %>% t %>% as.character, 
											to 		= 	filter(select_(ilo$code$cl_z, .dots = c('code', label = paste0('label_',ilo$lang))), code %in% levels(.)) %>% select(label) %>% t %>% as.character, 
										warn_missing = FALSE) , 
				info_lastcheckdate	=	info %>% 
								plyr:::mapvalues( 	from 	= 	levels(.), 
											to 		= 	str_split_fixed(levels(.), fixed(':'), n = 4)[,3] , 
									warn_missing = FALSE),
				info_checkstatus	=	info %>% 
								plyr:::mapvalues( 	from 	= 	levels(.), 
											to 		= 	str_split_fixed(levels(.), fixed(':'), n = 5)[,4] , 
									warn_missing = FALSE) %>% 
								plyr:::mapvalues(	from 	= 	filter(select(ilo$code$cl_z, code), code %in% levels(.)) %>% t %>% as.character, 
											to 		= 	filter(select_(ilo$code$cl_z, .dots = c('code', label = paste0('label_',ilo$lang))), code %in% levels(.)) %>% select(label) %>% t %>% as.character, 
										warn_missing = FALSE) , 
				info_checkuser	=	info %>% 
								plyr:::mapvalues( 	from 	= 	levels(.), 
											to 		= 	str_split_fixed(levels(.), fixed(':'), n = 6)[,5] , 
									warn_missing = FALSE) %>%
								plyr:::mapvalues(	from 	= 	filter(select(ilo$code$cl_z, code), code %in% levels(.)) %>% t %>% as.character, 
											to 		= 	filter(select_(ilo$code$cl_z, .dots = c('code', label = paste0('label_',ilo$lang))), code %in% levels(.)) %>% select(label) %>% t %>% as.character, 
										warn_missing = FALSE) , 
				info_channel	=	info %>% 
								plyr:::mapvalues( 	from 	= 	levels(.), 
											to 		= 	str_split_fixed(levels(.), fixed(':'), n = 7)[,6] , 
									warn_missing = FALSE) %>%
								plyr:::mapvalues(	from 	= 	filter(select(ilo$code$cl_z, code), code %in% levels(.)) %>% t %>% as.character, 
											to 		= 	filter(select_(ilo$code$cl_z, .dots = c('code', label = paste0('label_',ilo$lang))), code %in% levels(.)) %>% select(label) %>% t %>% as.character, 
										warn_missing = FALSE) ,
				info_web	=	info %>% 
								plyr:::mapvalues( 	from 	= 	levels(.), 
											to 		= 	str_split_fixed(levels(.), fixed(':'), n = 8)[,7] , 
									warn_missing = FALSE) %>%
								plyr:::mapvalues(	from 	= 	c(1,0), 
											to 		= 	c('Yes','No'), 
										warn_missing = FALSE)										) %>%
			select(-info) %>%
				{if(style) mutate(., info = str_c(info_laststatus, info_loadmode, info_lastcheckdate, info_checkstatus, info_checkuser, info_channel, info_web,  sep = '/')) %>% select(-info_laststatus, -info_loadmode, -info_lastcheckdate, -info_checkstatus, -info_checkuser, -info_channel, -info_web) else .} 
	else .}	%>% {invisible(gc(reset = TRUE)); .} %>% 
{if(str_detect(variable, '/version/'))
		separate (., sex,c("sex_version"), sep="_", extra = "drop", remove = FALSE) %>%
		mutate(sex_version = gsub('NA', NA,sex_version,  fixed = TRUE) %>% as.factor) %>%
		separate(classif1,c('CODE_CLACL1','CODE_VSCL1'), sep="_", extra = "drop", remove = FALSE) %>%
		unite(	classif1_version,CODE_CLACL1,CODE_VSCL1, sep = "_", remove = TRUE) %>% 
		mutate(classif1_version = gsub('NA_NA', NA,classif1_version, fixed = TRUE)) %>%
		separate(classif2,c('CODE_CLACL1','CODE_VSCL1'), sep="_", extra = "drop", remove = FALSE) %>%
		unite(	classif2_version,CODE_CLACL1,CODE_VSCL1, sep = "_", remove = TRUE) %>%
		mutate(classif2_version = gsub('NA_NA',NA,classif2_version, fixed = TRUE)) 	
	else .}	%>% {invisible(gc(reset = TRUE)); mutate_if(., is.factor, as.character)}  
	
}

