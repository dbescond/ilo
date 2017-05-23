#' R tools to manipulate ilo datasets code / label
#'
#' save and open easily your dataset in an xls, csv, dta files format 
#'
#' @param df ilo tbl data frame 
#' @param ... Specification of savinf format.
#' @section Specification:
#' specification allow for saving.
#' \itemize{
#'  \item \code{format}: specify format to save, if format is not specify, xlsx will be used.
#' }
#' setting format .
#' \itemize{
#'  \item \code{xls}: allow to save multiple dataset on different .xlsx sheet (could be also rename),
#'  \item \code{csv}: allow one dataset to be save in .csv, is also use for ilo dataset for uploading data,
#'  \item \code{dta}: allow one dataset to be save in .dta,
#'  \item \code{del}: store dataset in deletion ilo format .csv,
#'  \item \code{rev}: store dataset in del ilo format as well as upload ilo format in .xlsx,
#'  \item \code{path}: if set will store file in your path directory, see example.
#'  \item \code{open}: if path is set, you could just save file and not open it (set as FALSE), default = TRUE.
#' }
#' To learn more about ilo, start with the online vignettes:
#' \code{help_ilo()}
#'
#' @name save_ilo
#' @importFrom openxlsx createWorkbook addWorksheet writeData saveWorkbook
#' @importFrom tidyr expand unite
#' @importFrom stringr str_replace_all str_detect str_split_fixed fixed str_c str_split str_replace
#'
#'
#' @return external file open in temp workspace
#'
#' @examples
#' ## Not run:
#' ### quarterly female unemployed as from 2000 in united states
#'
#' res <-  get_ilo(collection = 'STI', 
#'		freq = 'Q', ref_area = 'USA', 
#'		indicator = 'UNE_TUNE_SEX_AGE_NB', 
#'		timefrom = '2000', sex = 'F', classif1 = 'AGGREGATE_TOTAL')
#' save_ilo(res = 'MyNewTab')
#' res %>% save_ilo(format = 'csv')
#' res %>% save_ilo(format = 'del')
#' res %>% save_ilo(format = 'rev')
#'
#' ### save multi shhet xls file
#' sum = res %>% count(time) 
#'
#' save_ilo(sum = 'summary' , res = 'raw')
#'
#' ### save file on disk
#'
#' save_ilo(res , path = 'D:/test.csv')
#'
#' ## End(**Not run**)
#' @export

save_ilo <- function(...){

type <- NULL
path <- NULL
dts  = lazyeval::lazy_dots(...)


if(is.null(dts$path)){path <- NULL} else {path <- dts$path$expr; dts[['path']] <- NULL}

if(is.null(dts$format)){
	if(is.null(path)){type <- 'xls' } else {type <- str_sub(path,-4,-1) %>% str_replace('.', '') }
	
} else{
	type <- tolower(dts$format$expr) %>% as.character
}
if(is.null(dts$open)){ open = TRUE} else{open = dts$open$expr}
dts[['format']] <- NULL
dts[['open']] <- NULL
	
if(!substr(type,1,3) %in% c('csv','xls','dta', 'del', 'delete', 'rev', 'revision')){return(print('not recognize format, only csv, xls, dta are possible as well as del for csv delete mode'))}



if(type %in% 'xls')	{
wb <- createWorkbook('ilo')
if(!is.null(path)){tfile <- path} else  {tfile <- tempfile(fileext = ".xlsx")}

for (i in 1:length(dts)){
	if(names(dts[i]) %in% '' & names(dts[i+1]) %in% '.') {dts[[i]] <- NULL; next}
	
}

for (i in 1:length(dts)){
	if(names(dts[i]) %in% '') {
		addWorksheet(wb, dts[[i]]$expr)
		writeData(wb, sheet = i, eval( dts[[i]]$expr, envir =  dts[[i]]$env) %>% as.data.frame)
	} else {
		addWorksheet(wb, as.character(dts[[i]]$expr))
		writeData(wb, sheet = i, eval( parse(text = names(dts[i])), envir =  dts[[i]]$env) %>% as.data.frame)
	}
}
saveWorkbook(wb, tfile, overwrite = TRUE)
}

if(type %in% c('csv', 'del', 'delete')){

	if(length(dts)>1){return(print('could not save 2 dataset in csv'))
	} else{
		if(!is.null(path)){tfile <- path} else  {tfile <- tempfile(fileext = ".csv")}
		if(names(dts[1]) %in% '') {
			ref <-  eval( dts[[1]]$expr, envir =  dts[[1]]$env)
			} else {
				ref <- eval( parse(text = names(dts[1])), envir =  dts[[1]]$env) 
		}
		
		if(type %in% c('del', 'delete')){
			refcol 	<- c('collection', 'ref_area', 'source', 'indicator', 'sex_version', 'classif1_version', 'classif2_version',  'time')
			refDELETE <- as_data_frame(t(refcol))
			colnames(refDELETE) <- refDELETE[1,]
			refDELETE <- refDELETE %>% slice(-1)
			ref  <-	refDELETE %>% 
					bind_rows(	ref %>% 
								select(collection:time) %>% 
								switch_ilo(version) %>% select(-sex, -classif1, -classif2) 
							) %>% 
					mutate(classif1_version = gsub('NOC_VALUE', 'NOC', classif1_version)) %>%
					distinct_(.dots = refcol, .keep_all = TRUE)
		}
		
		data.table:::fwrite(ref, tfile)
		
		} 

}

if(type %in% c('rev','revision')){

		if(names(dts[1]) %in% '') {
			ref <-  eval( dts[[1]]$expr, envir =  dts[[1]]$env)
			} else {
				ref <- eval( parse(text = names(dts[1])), envir =  dts[[1]]$env) 
		}
		
		wb <- createWorkbook('ilo')
		if(!is.null(path)){tfile <- path} else  {tfile <- tempfile(fileext = ".xlsx")}
		addWorksheet(wb, 'ToRev')
		writeData(wb, sheet = 1, ref %>% as.data.frame)
		
		ref  <-	ref %>% select_(.dots = c('collection', 'ref_area', 'source', 'indicator', 'sex', 'classif1', 'classif2',  'time')) %>%
								select(collection:time) %>% 
								switch_ilo(version) %>% select(-sex, -classif1, -classif2) %>% 
					mutate(classif1_version = gsub('NOC_VALUE', 'NOC', classif1_version)) %>%
					distinct_(.dots = c('collection', 'ref_area', 'source', 'indicator', 'sex_version', 'classif1_version', 'classif2_version',  'time'), .keep_all = TRUE)
		
		
		
		addWorksheet(wb, 'ToDel')
		writeData(wb, sheet = 2,  ref %>% as.data.frame )
		saveWorkbook(wb, tfile, overwrite = TRUE)		
		
		
}

if(type %in% 'dta'){
	if(length(dts)>1){return(print('could not save 2 dataset in dta'))
	} else{
		if(!is.null(path)){tfile <- path} else  {tfile <- tempfile(fileext = ".dta")}
			if(names(dts[1]) %in% '') {
			ref <-  eval( dts[[1]]$expr, envir =  dts[[1]]$env)
			} else {
				ref <- eval( parse(text = names(dts[1])), envir =  dts[[1]]$env) 
			}
		readstata13::save.dta13(ref, file = tfile, 
				version = 115, 
				compress = TRUE)
		} 
}


if(type %in% 'sav'){
	if(length(dts)>1){return(print('could not save 2 dataset in sav'))
	} else{
	if(!is.null(path)){tfile <- path} else  {tfile <- tempfile(fileext = ".sav")}
			if(names(dts[1]) %in% '') {
			ref <-  eval( dts[[1]]$expr, envir =  dts[[1]]$env)
			} else {
				ref <- eval( parse(text = names(dts[1])), envir =  dts[[1]]$env) 
			}
	
haven::write_sav(ref, path = tfile)
		} 
}
if(open){
shell.exec(file = tfile)
}
invisible(gc(reset = TRUE))

}
