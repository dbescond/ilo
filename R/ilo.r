#' R tools to access data from [ILO Statistics department]{http://www.ilo.org/ilostat/)}
#'
#' ilo package provides a flexible grammar of ilo data manipulation. Associate with dplyr, tidyr and others packages developped by Hadley Wickham & Co, it provide a faster way to work labour statistics from the ilo. 
#'
#' It has four main goals:
#'
#' \itemize{
#' \item Identify and get data from ilo database.
#' \item Support reproductible analysis.
#' \item View and map data or summary for analytical purpose (in progress).
#' \item Provide fast performance for in-memory data by using dplyr and parallel computation.
#' }
#'
#' ilo package always need to be initialise: code{init_ilo()} in order to prepare parallel computation and setting codelist
#'
#' To learn more about ilo, start with the online vignettes:
#' \code{help_ilo()}
#'
#' @name ilo
#' @importFrom plyr llply mapvalues
#' @importFrom foreach foreach "%dopar%"
#' @importFrom doSNOW registerDoSNOW
#' @importFrom rappdirs user_data_dir
#' @importFrom openxlsx createWorkbook addWorksheet writeData saveWorkbook
#' @import dplyr
#' @importFrom tidyr expand unite
#' @importFrom stringr str_replace_all str_detect str_split_fixed fixed str_c str_split str_replace
#'
#' @examples
#' \dontrun{
#' init_ilo()
#' get_ilo(ref_area = 'AFG')
#' }
rnm <- function(x) {
	paste0(	gsub('_' , ' ', tolower(x), fixed = TRUE) %>% 
			gsub('\\b(\\w)'	, '\\U\\1', ., perl=TRUE) %>%	 
			gsub(' ' , '_', ., fixed = TRUE)
		, collapse = '')
}

path_for_ilo <- function(){ 

	if(dir.exists(paste0(ilo:::path$tools, 'R/data/'))) return(paste0(ilo:::path$tools,'R/data'))
	else	rappdirs::user_data_dir(appauthor = 'ILO')

}


view_ilo <- function(df){

require(shiny)
require(miniUI)
require(leaflet)
require(ggplot2)
require(rpivotTable)
ui <- miniPage(
	gadgetTitleBar("Shiny gadget example"),
	miniTabstripPanel(
		miniTabPanel("Data", icon = icon("table"),
			miniContentPanel(
			DT::dataTableOutput("table")
			)
		),
		miniTabPanel("Pivot", icon = icon("th-list"),
			miniContentPanel(
				rpivotTable::rpivotTableOutput("pivot")
			)
		),
		miniTabPanel("Parameters", icon = icon("sliders"),
			miniContentPanel(
				sliderInput("year", "Year", 1978, 2010, c(2000, 2010), sep = "")
				)
			),
		miniTabPanel("Visualize", icon = icon("area-chart"),
			miniContentPanel(
			plotOutput("cars", height = "100%")
			)
		),
		miniTabPanel("Map", icon = icon("map-o"),
			miniContentPanel(padding = 0,
				leafletOutput("map", height = "100%")
			),
			miniButtonBlock(
				actionButton("resetMap", "Reset")
			)
		)
	)
)


server <- function(input, output, session) {
	output$cars <- renderPlot({
		require(ggplot2)
		ggplot(cars, aes(speed, dist)) + geom_point()
	})

	output$map <- renderLeaflet({
		force(input$resetMap)
		leaflet(quakes, height = "100%") %>% addTiles() %>%
			addMarkers(lng = ~long, lat = ~lat)
	})

 
	output$table <- DT::renderDataTable({
		df
	})
	
	output$pivot <- rpivotTable::renderRpivotTable({
        rpivotTable(data = df %>% select(-contains('note'), -contains('info')), rendererName="Table", rows = c('source', 'indicator', 'sex', 'classif1', 'classif2'), col = 'time', aggregatorName="Sum", vals = 'obs_value', width="100%", height="500px")
   })

	observeEvent(input$done, {
		stopApp(TRUE)
	})
}


runGadget(shinyApp(ui, server), viewer = paneViewer())

}

#' @export
close_ilo <- function(...){

delete = FALSE; lang = 'en'

	set <- list()
	.dots  = lazyeval::lazy_dots(...)
	set$para <- .dots[names(.dots)%in%'']
	set$DELETE <- FALSE
	if(length(set$para) > 0){
		for (i in 1:length(set$para)){
			test <- set$para[[i]]$expr %>% as.character
			if(tolower(test) %in% 'delete'){
				set$DELETE <- TRUE
			}
		}	
	}

path_ilo <- ilo:::path_for_ilo()

options(warn = -1)

 test <- try(parallel::stopCluster(ilo$cl), silent = T)

 test <- try(rm(ilo, envir = .GlobalEnv), silent = T)
invisible(gc(reset = TRUE))
invisible(gc(reset = TRUE))
 return('Clusters and settings closed')
 

}

