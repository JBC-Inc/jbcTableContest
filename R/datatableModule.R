#' Data table User Interface
#'
#' @param id An ID string that corresponds with the ID used in the modules server function.
#' @param ... Additional arguments.
#'
#' @return `datatableUI` returns a list of:
#'
#' * `DT::DTOutput()` main datatable output.
#' * `shiny::actionButton()` used to close the filter Modal.
#' * `shiny::textOuput()` to display the active filter status.
#'
datatableUI <- function(id, ...) {

  ns <- shiny::NS(id)

  list(
    dt = DT::DTOutput(outputId = ns("summary")),

    close = shiny::actionButton(inputId = ns("close_modal"), label = "Done"),

    filter_active = shiny::textOutput(ns("filter_active"))
  )
}

#' Data table Server
#'
#' A `DT` datatable with reactive dependency on filtered SL dataset, also Normalized Lateral Length input.
#' Initial summary data is generated with user interaction, then any filters are applied and passed to the
#' `DT::renderDT` reactive output. Additional invalidation are made when any filters change or the
#' Normalized Lateral Length input changes, in which case the table utilizes a proxy which replaces the
#' current data set while retaining it's settings.
#'
#' @param id An ID string that corresponds with the ID used to call the modules UI functions.
#' @param filtered filtered subset of the SL dataset based on selected inputs.
#' @param eur usfsrpmsc EUR data.
#' @param nll reactive normalized lateral length input.
#'
#' @return `datatableServer` has side effects of creating and updating the summary data table, along with
#' custom filter input modal.
#'
datatableServer <- function(id, filtered, eur, nll) {

  shiny::moduleServer(id, function(input, output, session) {

    ns <- shiny::NS(id)

    summary_data <- shiny::reactive(label = "Calc summary data", {

      shiny::req(filtered())

      computeSummary(filtered = filtered()$fill,
                     multi = filtered()$multi,
                     eur = eur,
                     nll = nll(),
                     summary = TRUE,
                     magnitude = 1e9)
    })

    filtered_summary_data <- shiny::reactive(label = "Filter summary data", {

      sd <- summary_data()

      if(nrow(sd) > 0 ) {
        if(!is.null(input$rad_filter)){
          sd <- dplyr::filter(sd, aoi_region %in% input$rad_filter)
        }
        if(!is.null(input$res_filter)){
          sd <- dplyr::filter(sd, reservoir %in% input$res_filter)
        }
      }
      sd
    })

    shiny::observe(label = "Filter active", {

      if(!is.null(input$rad_filter) | !is.null(input$res_filter)){

        shinyjs::hide("fa-break", asis = TRUE)
        shinyjs::show("fa-container", asis = TRUE)
        output$filter_active <- shiny::renderText({ "Filter Active on Table" })

      } else {

        shinyjs::show("fa-break", asis = TRUE)
        shinyjs::hide("fa-container", asis = TRUE)
      }
    })

    shiny::observe(label = "No data available notification", {
      fsd <- filtered_summary_data()

      if (dim(fsd)[1] == 0) {
        shinyalert::shinyalert(
          title = "No Summary Data available.",
          text = "Wells are outside of SL radius range. (Try selecting a different reservoir.)",
          type = "info",
          size = "xs"
        )
      }
    })

    output$summary <- DT::renderDT(expr = {

      shiny::req(filtered_summary_data())

      if (nrow(filtered_summary_data()) == 0) {

        createNoDatatable(session = session)

      } else {

        createDatatable(shiny::isolate(filtered_summary_data()), session = session)
      }
    })

    dtProxy <- DT::dataTableProxy('summary')

    shiny::observeEvent(nll(), label = "Normalized Lateral Length", {

      DT::replaceData(proxy = dtProxy,
                      data = filtered_summary_data(),
                      resetPaging = FALSE)

      }, ignoreInit = TRUE)

    shiny::observeEvent(input$filter, label = "Datatable filters", {

      shiny::showModal(ui = modalDialog(
        title = "Datatable filters",
        shiny::tags$div(class = "select-shell",
          shiny::tags$div(class = "label-input",
            shiny::tags$div(class = "lbl", "Area"),
            shiny::tags$div(class = "inputt",
                            shiny::selectizeInput(inputId = ns("rad_filter"),
                                                  label = NULL,
                                                  choices = unique(summary_data()$aoi_region),
                                                  selected = input$rad_filter,
                                                  multiple = TRUE,
                                                  width = "150px")
                            )
            ),
          shiny::tags$div(class = "label-input",
            shiny::tags$div(class = "lbl", "Reservoir"),
            shiny::tags$div(class = "inputt",
                            shiny::selectizeInput(inputId = ns("res_filter"),
                                                  label = NULL,
                                                  choices = sort(unique(summary_data()$reservoir)),
                                                  selected = input$res_filter,
                                                  multiple = TRUE,
                                                  width = "150px")
                            )
            )
        ),
        footer = shiny::tagList(
          shiny::tags$div(
            datatableUI("summary")$close
            )
          ),
        size = "s"
        )
        )
      })

    shiny::observeEvent(input$close_modal, label = "Close filter modal", {
      shiny::removeModal()
      })
  })
}
