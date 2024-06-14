#' Choices UI
#'
#' @param id An ID string that corresponds with the ID used in the modules server function.
#' @param ... Arguments supplied to selectInput controls.
#'
#' @return `choicesUI()` returns a list of three shiny selectizeInputs and an action button control.
#'
choicesUI <- function(id, ...) {

  ns <- shiny::NS(id)

  so <- list(closeAfterSelect = TRUE)

  list(

    inp_res = shiny::selectizeInput(ns("res"),
                                    label = NULL,
                                    choices = NULL,
                                    multiple = TRUE,
                                    options = so,
                                    ...),

    inp_sta = shiny::selectizeInput(ns("status"),
                                    label = NULL,
                                    choices = NULL,
                                    multiple = TRUE,
                                    options = so,
                                    ...),

    inp_drl = shiny::selectizeInput(ns("drill"),
                                    label = NULL,
                                    choices = NULL,
                                    multiple = TRUE,
                                    options = so,
                                    ...),

    render = shiny::actionButton(inputId = ns("render"), label = "Render Wells")
  )
  }

#' Choices Server and Filtered Data
#'
#' Creates an internal reactive when sl dataset is received, which calculates the possible choices for
#' reservoir, well status and hole direction, then populates the corresponding UI components. The final
#' result is a reactive which is the subset of the sl data reflecting the selected choices.
#'
#' @param id An ID string that corresponds with the ID used to call the module's UI function.
#' @param data reactive named list containing the data needed for the map and filtered data.
#' @param date input from the date control for First Production Date used to filter.
#'
#' @return `choicesServer` returns a reactive filtered superlocation dataset based on selected UI choices.
#'
choicesServer <- function(id, data, date) {

  shiny::moduleServer(id, function(input, output, session) {

    choices <- shiny::reactive(label = "Calc input choices", {

      shiny::req(data())

      combinations <- data()$well_layers |>
        sf::st_drop_geometry() |>
        dplyr::select(reservoir, well_status, hole_direction, first_prod_date) |>
        dplyr::distinct()

      getChoices(       header = combinations,
                     reservoir = input$res,
                   well_status = input$status,
                hole_direction = input$drill,
                 include_empty = FALSE)
    })

    shiny::observe(label = "Update selectize inputs", {

      shiny::updateSelectizeInput(
        inputId = "res",
        choices = choices()$reservoir,
        selected = input$res
      )
      shiny::updateSelectizeInput(
        inputId = "status",
        choices = choices()$well_status,
        selected = input$status
      )
      shiny::updateSelectizeInput(
        inputId = "drill",
        choices = choices()$hole_direction,
        selected = input$drill
      )
    })

    shiny::reactive(label = "Generate filtered()", {

      fill <- data()$well_layers

      if (!is.null(input$res) & !identical(input$res, "")) {
        fill <- fill |> dplyr::filter(reservoir %in% input$res)
      }
      if (!is.null(input$status) & !identical(input$status, "")) {
        fill <- fill |> dplyr::filter(well_status %in% input$status)
      }
      if (!is.null(input$drill) & !identical(input$drill, "")) {
        fill <- fill |> dplyr::filter(hole_direction %in% input$drill)
      }
      if (!is.null(date) & !identical(date, "")) {
        fill <- fill |> dplyr::filter(first_prod_date >= date | is.na(first_prod_date)) # keep NA
      }

      list(
        fill = fill,
        multi = data()$multi
        )

    }) |> shiny::bindEvent(input$render)
  })
}
