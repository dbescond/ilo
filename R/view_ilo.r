#' R tools to visualyze pivot ilo data frame
#'
#'
#' @param df, ilo tbl data frame. 
#'
#'
#' @name pivot_ilo
#'
#'
#' @return pivot html table
#'
#' @examples
#' \dontrun{
#'
#' init_ilo()
#'
#'
#' ### quarterly time serie of female unemployed as from 2000 in united states
#'
#' X <- get_ilo(ref_area = 'AFG')
#' pivot_ilo(X)
#' }
#' @export
pivot_ilo <- function(df)
{
    require(shiny)
    require(miniUI)
    #require(leaflet)
    #require(ggplot2)
    require(rpivotTable)
    ui <- miniPage(
		gadgetTitleBar("Shiny gadget example"), 
			miniTabstripPanel(
				#miniTabPanel("Data", icon = icon("table"), miniContentPanel(DT::dataTableOutput("table"))), 
				miniTabPanel("Pivot", icon = icon("th-list"), miniContentPanel(rpivotTable::rpivotTableOutput("pivot"))) #, 
				#miniTabPanel("Parameters", icon = icon("sliders"), 
				#	miniContentPanel(sliderInput("year", "Year", 1978, 2010, c(2000, 2010), sep = ""))), 
				#miniTabPanel("Visualize", icon = icon("area-chart"), 
				#	miniContentPanel(plotOutput("cars",  height = "100%"))), 
				#miniTabPanel("Map", icon = icon("map-o"), 
				#	miniContentPanel(padding = 0, leafletOutput("map", height = "100%")), 
				#	miniButtonBlock(actionButton("resetMap", "Reset"))
				#	)
			)
		)
    server <- function(input, output, session) {
        #output$cars <- renderPlot({
         #   require(ggplot2)
          #  ggplot(cars, aes(speed, dist)) + geom_point()
        #})
        #output$map <- renderLeaflet({
         #   force(input$resetMap)
         #   leaflet(quakes, height = "100%") %>% addTiles() %>% 
         #       addMarkers(lng = ~long, lat = ~lat)
        #})
        #output$table <- DT::renderDataTable({
         #   df
        #})
        output$pivot <- rpivotTable::renderRpivotTable({
            rpivotTable(data = df %>% select(	-contains("note"), 
												-contains("info"), 
												-contains("obs_status")), 
						rendererName = "Table", 
						rows = c("source", "indicator", "sex", "classif1", "classif2"), 
						col = "time", 
						aggregatorName = "Sum", 
						vals = "obs_value", width = "100%", height = "500px")
        })
        observeEvent(input$done, {
            stopApp(TRUE)
        })
    }
    runGadget(shinyApp(ui, server), viewer = paneViewer())
}





