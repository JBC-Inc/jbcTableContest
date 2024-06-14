#' Reservoir Override UI
#'
#' @param id An ID string that corresponds with the ID used in the modules server function.
#'
#' @return `reservoirUI()` returns an file upload control which accepts .csv input.
#'
reservoirUI <- function(id) {

  ns <- shiny::NS(id)

  shiny::fileInput(
    inputId = ns("reservoir_override"),
    label = "Reservoir Override",
    multiple = TRUE,
    accept = '.csv'
  ) |>
    shinyhelper::helper(
      icon = "file-upload",
      type = "inline",
      title = "Reservoir Name Override",
      content = c("Requires <b>comma-separated values .csv</b> file formatted as:<br>",
                  "<table border='1'>
	                  <thead>
		                  <tr>
			                  <th style='padding:0px 10px'>PDENID</th>
			                  <th style='padding:0px 10px'>Reservoir</th>
		                  </tr>
	                  </thead>
	                  <tbody>
		                  <tr>
			                  <td style='padding:0px 10px'>-245774</td>
			                  <td style='padding:0px 10px'>WOLFCAMP</td>
		                  </tr>
		                  <tr>
			                  <td style='padding:0px 10px'>1230291828</td>
			                  <td style='padding:0px 10px'>New Reservoir Name</td>
		                  </tr>
	                  </tbody>
                  </table>"),
      size = "s"
    )
  }

#' Reservoir Override File Upload
#'
#' Processes an Comma Separated Values file which contains name value pairs of Reservoir/Entity IDs. Used
#' to overwrite the original `Superlocation$well_layers` reservoir names to custom names.
#'
#' @param id An ID string that corresponds with the ID used to call the module's UI function.
#' @param data `reactive` sl_out object.
#'
#' @return `reservoirServer()` returns a reactive value containing the same data as the fileInputModule,
#' but with updated reservoir names. Also adds a copy of the original override CSV data.
#'
reservoirServer <- function(id, data) {

  shiny::moduleServer(id, function(input, output, session) {

    shiny::reactive(label = "Calc superlocation w/reservoir override names", {

      shiny::req(data())

      if (is.null(input$reservoir_override)) {
        data()
      } else {
        ext <- tools::file_ext(input$reservoir_override$name)
        switch(ext,
               csv = vroom::vroom(input$reservoir_override$datapath, delim = ","),
               shiny::validate("Invalid file; Please upload a .csv file")
               )

        override_data <- readr::read_csv(file = input$reservoir_override$datapath,
                                         show_col_types = FALSE,
                                         col_types = "ic"
                                         )
        wl <- data()$well_layers

        well_layers <- dplyr::left_join(x = wl, y = override_data, by = c("entity_id" = "PDENID")) |>
          dplyr::mutate(reservoir = dplyr::case_when(!is.na(Reservoir) ~ Reservoir,
                                                     TRUE ~ as.character(reservoir))) |>
          dplyr::select(-Reservoir)

        list(
          reservoir_override = override_data,     # override
          shapefile          = data()$shapefile,
          shapename          = data()$shapename,
          aoi_polys          = data()$aoi_polys,
          well_layers        = well_layers,       # override
          grids              = data()$grids,
          counties           = data()$counties,
          center_lng         = data()$center_lng,
          center_lat         = data()$center_lat,
          zoom               = data()$zoom,
          multi              = data()$multi
        )
        }
      })
    })
  }
