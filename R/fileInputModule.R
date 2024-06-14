#' File Input UI
#'
#' @param id An ID string that corresponds with the ID used in the modules server function.
#'
#' @return `fileInputUI()` returns a file upload control for input and text output element.
#'
fileInputUI <- function(id) {

  ns <- shiny::NS(id)

  list(
    reset_app = shiny::actionButton(ns("reload"), label = "Reset App"),
    load_sample = shiny::actionButton(ns("ls"), label = "Load Data")
    )
}

#' File Upload
#'
#' Processes an shapefile and calculates all wells within specified radius.
#' Check for existence of .prj projection file.
#'
#' @param id An ID string that corresponds with the ID used to call the module's UI function.
#' @param wells well data with simple feature geometries for well location and trajectory.
#' @param texas_counties [sf] with all Texas counties.
#' @param texas_grids [sf] with all Texas survey areas/grids.
#'
#' @return `fileInputServer()` returns a reactive list:
#'  - `shapefile` Original shapefile
#'  - `input$upload$name` Name of the shapefile
#'  - `center_lng/center_lat` Center points of the shapefile in longitude/latitude
#'  - `multi` Whether or not the shapefile has one or more polygons
#'  - `aoi_polys` Area of Interest Region `sf` polygons
#'  - `well_layers` Well Layers `data.frame`, where each well belongs to a specific region
#'  - `counties` County `sf` polygons
#'  - `grids` Grids `sf` polygons within the specific counties
#'  - `zoom` Zoom level for the leaflet based on total area
#'  - `got_data` Whether or not the input method contained data (file) or was manually selected
#'
fileInputServer <- function(id, wells, texas_counties, texas_grids) {

  shiny::moduleServer(id, function(input, output, session) {

    ns <- shiny::NS(id)

    data <- shiny::reactive(label = "Input|Sample data", {

      shiny::req(shiny::isTruthy(input$upload) || shiny::isTruthy(input$ls))

      if(!is.null(input$upload)) {
        dir <- tempfile()
        dir.create(dir)
        suppressWarnings(utils::unzip(input$upload$datapath, exdir = dir))

        if (validExtensions(dir)) {
          return()
        } else {
          shapefile <- suppressWarnings(
            sf::st_read(dsn = paste(dir, list.files(pattern = "*.shp$"), sep = "/"),
                                                    quiet = TRUE))
          if (crsExist(shapefile)) {
            return()
          } else {
            shapefile <- sf::st_transform(x = shapefile, crs = 4326)
          }
          if (validCRS(shapefile)) {
            return()
          }
        }
        shapefile
      } else {
        data.frame(NULL)
        }
      }) |>
      shiny::bindEvent(c(input$upload, input$ls))

    sample_data <- shiny::reactiveVal(NULL)

    shiny::observe(label = "Single polygon", {

      sample_data(
        sf::st_read(dsn = "./inst/data-raw/shape.shp", quiet = TRUE) |>
          sf::st_transform(crs = 4326)
        )

      shinyjs::runjs(
        paste0("Shiny.setInputValue('", ns("unique_ids"), "', 'radius', {priority: 'event'});")
        )
    })

    shiny::reactive(label = "Process shapefile", {

      shiny::req(input$unique_ids)

      shiny::removeModal()

      waiter::waiter_show(html = shiny::tagList(waiter::spin_puzzle(), shiny::h4("Drilling Wells...")))

      if(!is.null(sample_data())) {
        shapefile <- sample_data()
      } else {
        shapefile <- data()
      }

      epsg_code <- calcEPSG(shapefile)

      acres <- floor((as.numeric(sf::st_area(shapefile)) * 10.764) / 43560)
      center_lng <- sf::st_centroid(sf::st_geometry(shapefile)[[1]])[1]
      center_lat <- sf::st_centroid(sf::st_geometry(shapefile)[[1]])[2]

      aoi_polys <- addBuffers(shapefile, epsg_code)
      aoi_regions <- c(0, 1, 5, 10, 15, 25)
      multi <- FALSE
      shape_ids <- NULL

      counties <- sf::st_join(x = texas_counties,
                              y = sf::st_buffer(x = shapefile, dist = mtf(0.01)),
                              join = sf::st_intersects,
                              left = FALSE) |>
        sf::st_transform(crs = sf::st_crs("EPSG:4326")) |>
        dplyr::distinct(COUNTYID, COUNTYAPI, COUNTYNAME, geometry)

      grids <- texas_grids |> dplyr::filter(COUNTYID %in% counties$COUNTYID)

      zoom <-
        dplyr::case_when(
          acres >=      0 & acres <=    2000 ~ 11,
          acres >    2001 & acres <= 1000000 ~ 10,
          acres > 1000000 & acres <= 3500000 ~ 9,
          acres > 3500000                    ~ 8,
          TRUE ~ 11
        )

      output$datetime <- shiny::renderText({
        paste0(as.character(as.POSIXlt(Sys.Date(), tz = "UTC")), " UTC") # Sys.time()
      })

      well_layers <- sf::st_as_sf(
        x = dplyr::bind_rows(purrr::map(.x = aoi_regions,
                                        .f = \(x) wellsInArea(wells, shapefile, aoi_region = x)))) |>
        dplyr::select(-tidyselect::any_of(c("SL_id", "wells", "Shape_Leng", "Shape_Area")))

      if (!input$unique_ids == "radius") {

        well_layers <- well_layers |> dplyr::select(-aoi_region)

        well_layers$aoi_region <- well_layers |>
          sf::st_drop_geometry() |>
          dplyr::select(input$unique_ids) |>
          dplyr::pull()

        shape_ids <- shapefile |>
          sf::st_drop_geometry() |>
          dplyr::select(input$unique_ids) |>
          dplyr::pull()
      }

      waiter::waiter_hide()

      list(
        aoi_polys   = aoi_polys,
        center_lat  = center_lat,
        center_lng  = center_lng,
        counties    = counties,
        grids       = grids,
        multi       = multi,
        shape_ids   = shape_ids,
        shapefile   = shapefile,
        shapename   = input$upload$name,
        well_layers = well_layers,
        zoom        = zoom
      )
    })
  })
}
