confirm_modal <- function(id, download_details) {
    ns <- NS(id)
    state_string <- paste0(download_details$states, collapse = ", ")

    modalDialog(
        title = "Confirm query",
        glue::glue(
            "Query will download ~{download_details$size} GB of data for {state_string}. Are you sure you want to run this query?"
        ),
        footer = tagList(
            actionButton(ns("cancel_query"), "Cancel"),
            actionButton(ns("confirm_query"), "Run", class = "btn btn-success")
        )
    )
}

disasterAreaUI <- function(id) {
    ns <- NS(id)

    card(
        id = "inputs",
        full_screen = TRUE,
        card_header("Disaster area"),
        layout_sidebar(
            sidebar = sidebar(
                "Disaster Area",
                radioButtons(
                    ns("polygon_type"),
                    "Input type",
                    choices = c("Draw", "Upload"),
                    selected = "Draw"
                ),
                actionButton(ns("run_query_btn"), "Run Query"),
                position = "right",
                open = "open"
            ),
            conditionalPanel(
                condition = sprintf("input['%s'] == 'Draw'", ns("polygon_type")),
                leafletOutput(ns("map"))
            ),
            conditionalPanel(
                condition = sprintf("input['%s'] == 'Upload'", ns("polygon_type")),
                fileInput(
                    ns("upload_shape"),
                    "",
                    multiple = FALSE,
                    accept = c(".geojson", ".gpkg", ".kml"),
                    width = NULL,
                    buttonLabel = "Browse...",
                    placeholder = "No file selected",
                    capture = NULL
                )
            )
        )
    )
}

disasterAreaServer <- function(id, query_busy) {
    moduleServer(id, function(input, output, session) {
        ns <- session$ns

        draw_polygons <- reactiveVal(NULL)
        upload_files <- reactiveVal(NULL)
        pending_request <- reactiveVal(NULL)

        states <- sf::st_read(
            here::here("data/cb_2024_us_state_500k/cb_2024_us_state_500k.shp"),
            quiet = TRUE
        )

        can_run_query <- reactive({
            (input$polygon_type == "Draw" &&
                !is.null(draw_polygons()) && nrow(draw_polygons()) > 0) ||
                (input$polygon_type == "Upload" &&
                    !is.null(upload_files()) && nrow(upload_files()) > 0)
        })

        observe({
            if (isTRUE(query_busy())) {
                shinyjs::disable(ns("polygon_type"), asis=TRUE)
                shinyjs::disable(ns("run_query_btn"), asis=TRUE)
            } else {
                shinyjs::enable(ns("polygon_type"), asis=TRUE)
                if (can_run_query()) {
                    shinyjs::enable(ns("run_query_btn"), asis=TRUE)
                } else {
                    shinyjs::disable(ns("run_query_btn"), asis=TRUE)
                }
            }
        })

        output$map <- renderLeaflet({
            leaflet() %>%
                setView(lng = -98.5795, lat = 39.8283, zoom = 4) |>
                addTiles(layerId = "map_click") |>
                addDrawToolbar(
                    targetGroup = "disaster_extent",
                    circleOptions = FALSE,
                    polylineOptions = FALSE,
                    rectangleOptions = FALSE,
                    circleMarkerOptions = FALSE,
                    markerOptions = FALSE,
                    editOptions = editToolbarOptions(
                        remove = TRUE,
                        edit = FALSE
                    )
                )
        })

        observeEvent(input$map_draw_deleted_features, {
            feature <- req(input$map_draw_deleted_features)

            delete_ids <- lapply(feature$features, function(ft) {
                ft$properties$`_leaflet_id`
            }) |> unlist()

            draw_polygons(draw_polygons() |>
                dplyr::filter(!id %in% delete_ids))
        })

        observeEvent(input$map_draw_new_feature, {
            feature <- req(input$map_draw_new_feature)
            coords <- feature$geometry$coordinates[[1]]
            coords_matrix <- matrix(unlist(coords), ncol = 2, byrow = TRUE)

            new_poly <- sf::st_sf(
                id = feature$properties$`_leaflet_id`,
                geometry = sf::st_sfc(
                    sf::st_polygon(list(coords_matrix)),
                    crs = "EPSG:4326"
                )
            )

            if (is.null(draw_polygons())) {
                draw_polygons(new_poly)
            } else {
                draw_polygons(dplyr::bind_rows(draw_polygons(), new_poly))
            }
        })

        observeEvent(input$upload_shape, {
            upload_files(input$upload_shape)
        })

        observeEvent(input$run_query_btn, {
            query_polygon <- if (input$polygon_type == "Draw") {
                sf::st_union(draw_polygons())
            } else {
                sf::st_read(upload_files() |> tail(1) |> dplyr::pull(datapath)) |>
                    sf::st_union()
            }

            query_states <- states |>
                sf::st_filter(
                    sf::st_transform(query_polygon, sf::st_crs(states)),
                    .predicate = sf::st_intersects
                ) |>
                sf::st_drop_geometry() |>
                dplyr::select(NAME, STUSPS)

            exists_query_states <- nrow(query_states) > 0

            if (!exists_query_states) {
                showNotification(
                    "Disaster polygon must intersect with United States!",
                    type = "error"
                )
                return(invisible(NULL))
            }

            pending_request(list(states = query_states, polygon = query_polygon))

            showModal(
                confirm_modal(
                    id,
                    list(
                        states = query_states$NAME,
                        size = round((nrow(query_states) * 50) / 1000, 2)
                    )
                )
            )
        })

        observeEvent(input$cancel_query, {
            pending_request(NULL)
            showNotification("Query cancelled")
            removeModal()
        })

        confirmed_request <- reactiveVal(NULL)

        observeEvent(input$confirm_query, {
            removeModal()
            confirmed_request(pending_request())
            pending_request(NULL)
        })

        list(
            query_request = reactive(confirmed_request())
        )
    })
}