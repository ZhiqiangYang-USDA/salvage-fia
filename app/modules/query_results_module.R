queryResultsUI <- function(id) {
    ns <- NS(id)

    card(
        id = "query",
        card(
            card_header("Query Results"),
            card(
                radioButtons(
                    inputId = ns("estimation_group"),
                    label = "Estimate group",
                    choices = c("Area", "Tree count", "Volume", "AGB / Carbon"),
                    selected = "Area"
                )
            ),
            card_body(htmlOutput(ns("query_results"))),
            shinytail::shinyTail(ns("logs")),
            downloadButton(ns("download_data_btn"), "Download Results")
        )
    )
}

queryResultsServer <- function(id, query_request, log_file_path, on_busy_change) {
    moduleServer(id, function(input, output, session) {
        ns <- session$ns

        if (is.null(on_busy_change)) {
            on_busy_change <- function(...) invisible(NULL)
        }

        shinyjs::disable(ns("download_data_btn"), asis=TRUE)
        shinyjs::disable(ns("estimation_group"), asis=TRUE)

        get_query_results <- ExtendedTask$new(function(query_states, query_polygon, log_file) {
            mirai(
                {
                    execute_query(query_states, query_polygon, log_file)
                },
                execute_query = execute_query,
                download_db = download_db,
                get_affected_plot_ids = get_affected_plot_ids,
                get_affected_inventory = get_affected_inventory,
                log_file = log_file,
                query_polygon = query_polygon,
                query_states = query_states,
                get_fiesta_est = get_fiesta_est
            )
        })

        log_data <- reactiveVal(value = NULL, label = "log")
        log_tail_process <- shinytail::tailFile(log_file_path)

        observe({
            shinytail::readStream(log_data, log_tail_process)
        })

        observeEvent(query_request(), {
            req(query_request())

            on_busy_change(TRUE)
            shinyjs::disable(ns("download_data_btn"), asis=TRUE)
            shinyjs::disable(ns("estimation_group"), asis=TRUE)

            get_query_results$invoke(
                query_request()$states,
                query_request()$polygon,
                log_file_path
            )

            output$logs <- renderText({
                paste(log_data(), collapse = "\n")
            })
        })

        observeEvent(get_query_results$status(), {
            if (get_query_results$status() == "success") {
                on_busy_change(FALSE)
                shinyjs::enable(ns("estimation_group"))
                shinyjs::enable(ns("download_data_btn"))

                output$download_data_btn <- downloadHandler(
                    filename = function() {
                        paste("salvage-fia-results-", Sys.Date(), ".zip", sep = "")
                    },
                    content = function(file) {
                        download_csvs(file, get_query_results$result())
                    }
                )

                output$logs <- renderText("")
                log_data(NULL)
            } else if (get_query_results$status() == "error") {
                on_busy_change(FALSE)
                shinyjs::disable(ns("download_data_btn"))
            }
        })

        output$query_results <- renderUI({
            req(get_query_results$status() == "success")
            results <- render_query_results(get_query_results$result())
            results <- map_estimation_group_to_result(input$estimation_group, results)
            build_html(input$estimation_group, results)
        })
    })
}