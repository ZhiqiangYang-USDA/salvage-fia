library(shiny)
library(bslib)
library(kableExtra)
library(shinyFeedback)
library(leaflet)
library(leaflet.extras)
source("helpers.R")
source("modules/disaster_area_module.R")
source("modules/query_results_module.R")
library(shinyjs)
library(FIESTA)
library(shinytail)
library(mirai)

mirai::daemons(2)

log_dir <- here::here("logs")

if (dir.exists(log_dir)) {
    clean_dir(log_dir)
} else {
    dir.create(log_dir)    
}

log_file_path <- file.path(log_dir, basename(tempfile(fileext = '.txt')))

ui <- page_fluid(
    useShinyjs(),
    title = "Salvage FIA",
    navset_card_underline(
        title = "Salvage FIA",
        nav_panel(
            "",
            disasterAreaUI("disaster_area"),
            queryResultsUI("query_results")
        )
    )
)

server <- function(input, output, session) {
    query_busy <- reactiveVal(FALSE)

    disaster_area <- disasterAreaServer(
        id = "disaster_area",
        query_busy = reactive(query_busy())
    )

    queryResultsServer(
        id = "query_results",
        query_request = disaster_area$query_request,
        log_file_path = log_file_path,
        on_busy_change = query_busy
    )
}

onStop(function() {
    clean_dir(here::here("temp_dir"))
})

shinyApp(
    ui = ui, 
    server = server
)