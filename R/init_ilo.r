#' R tools to manipulate ilo datasets code / label
#'
#' init ilo packages by loading codelist and cluster for parallel computation 
#'
#' @param ... Specification for extenal update.
#' @section Specification:
#' others functions allow advanced manipulation.
#' \itemize{
#'  \item \code{update}: update database for external users,
#' }
#' To learn more about ilo, start with the online vignettes:
#' \code{help_ilo()}
#'
#' @name init_ilo
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
#' @return ilo an R environment that contains cluster and codelist
#'
#' @examples
#' init_ilo()
#'
#' ilo
#' ilo %>% names
#' ilo$lastupdate
#' ilo$code %>% names
#' ilo$code$cl_country
#' @export

init_ilo <- function(...){

path_ilo <- ilo:::path_for_ilo()
TEST <- TRUE
cl_ilo = TRUE

	init_time <- Sys.time() 
	update = FALSE
	lang = 'en'

	set <- list()
	.dots  = lazyeval::lazy_dots(...)


	set$para <- .dots[!names(.dots)%in%'']
	set$COL <- FALSE
	set$FRQ <- FALSE
	if(length(set$para)>0){
		for (i in 1:length(names(set$para))){
							
			if(tolower(names(set$para[i])) %in% 'collection'){set$COL <- TRUE} 
			if(tolower(names(set$para[i])) %in% 'freq'){set$FRQ <- TRUE} 
			eval(expression(assign(tolower(names(set$para[i])), eval( set$para[[i]]$expr, envir =  set$para[[i]]$env))))
			
		}	
	}	
	set$para <- .dots[names(.dots)%in%'']
	set$UPDATE <- FALSE
	if(length(set$para) > 0){
		for (i in 1:length(set$para)){
			test <- set$para[[i]]$expr %>% as.character %>% paste0(.,collapse = '')
			if(tolower(test) %in% 'update'){
				set$UPDATE <- TRUE
			}
			if(test %in% '-cl'){cl_ilo <- NULL}
		}	
	}
		test1 <- try( lazyLoad(paste0(path_ilo,'\\settings\\set'), refset <- new.env()), silent = T)
		test2 <- try( lazyLoad(paste0(path_ilo,'\\settings\\cl'), refcl <- new.env()), silent = T)
	
	
	if(!is.null(cl_ilo)){ # cluster registration
				cl_ilo <- parallel::makeCluster(max(1, parallel::detectCores() - 1)) 
				doSNOW::registerDoSNOW(cl_ilo)
	}
		# test2 <- try( lazyLoad(paste0(path_ilo,'\\settings\\cl'), refcl <- new.env()), silent = T)

	e <- new.env()

	assign('path', path_ilo, envir = e)
	assign('cl', cl_ilo, envir = e)
	assign('lang', lang, envir = e)
	assign('code', refcl, envir = e)
	Seg <- refset$segment %>% 
					{if(set$COL) rename(., col_ = collection) %>% filter(col_ %in% collection) %>% rename(collection = col_)else .} %>%
					{if(set$FRQ) rename(., fre_ = freq) %>% filter(fre_ %in% freq) %>% rename(freq = fre_) else .}
	assign('segment', Seg, envir = e); rm(Seg)
	assign('lastupdate', refset$lastupdate, envir = e); rm(refset)
	ilo 	<<- e
	invisible(gc(reset = T))
	final_time <- Sys.time()
	return(paste0('Files ready, clusters created. Database available. ', round(final_time - init_time ,2)))
	


}
