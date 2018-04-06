#' R tools to manipulate ilo datasets code / label
#'
#' wraper for filter(string::str_detect(string, pattern))
#'
#' @param df ilo tbl data frame to manipulate.
#' @param ... Specification of columns and pattern.
#' @section Specification:

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
#' @return a tbl data frame
#'
#' @examples
#' \dontrun{
#' res <- get_ilo(ref_area = 'AFG')
#'
#' res %>% filter_ilo(note_source = 'T3')
#' }
#' @export

filter_ilo <- function(df, ...){

gui <- "\'"
.dots  = lazyeval::lazy_dots(...)
	query = NULL

	for (i in 1:length(.dots)){
		ref <- tolower(names(.dots)[i])
		condi <- .dots[[i]]$expr %>% as.character
		neg <- NULL
		if(length(condi) ==2) {neg <- condi[1]; neg <- ifelse(neg %in% '-','!',neg); condi <- condi[2]}
		condi <- paste0(gui,condi,gui)
		ref <- paste0(neg, 'str_detect(',ref,', fixed(',  condi,'))')
		query <- paste(c(query, ref), collapse = ', ')
	}	
	query <- paste0('df %>% filter(',query,')')
	
	
	
invisible(gc(reset = TRUE))
	eval(parse(text=query))	
	
	
	
	
	
	
}
