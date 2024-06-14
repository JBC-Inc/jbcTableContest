
#' Miles to Feet
#'
#' @param miles numeric value.
#'
#' @return `mtf()` returns numeric value.
#'
mtf <- function(miles) {

  feet <- measurements::conv_unit(miles, "mi", "ft")
  return(feet)
}

#' Miles to Meters
#'
#' @param miles numeric
#'
#' @return `mtm` return numeric value of meters.
#'
mtm <- function(miles) {

  measurements::conv_unit(miles, "mi", "m")
}

#' Add Circle Layers
#'
#' Creates a leaflet proxy with SL well and trajectory polygons.
#'
#' @param map leaflet map object
#' @param df_list list of data.frames of well data split by reservoir name.
#' @param palette leaflet color mapping object which maps data values to colors according to a given palette.
#'
#' @return `addCircleLayers()` returns a leaflet proxy object
#'
addCircleLayers <- function(map, df_list, palette) {
  purrr::reduce(df_list, function(map, data) {
    map |>
      leaflet::addPolylines(
        data = data$traj,
        group = data$reservoir[1],
        color = 'black',
        weight = 0.7,
        opacity = 0.2
      ) |>
      leaflet::addCircleMarkers(
        data = data,

        radius = 4,
        group = data$reservoir[1],

        color = 'black',
        weight = 1,
        opacity = 1,

        fill = TRUE,
        fillColor = ~ palette(reservoir),
        fillOpacity = 0.7,
        popup = mapLabels(data),
        popupOptions = leaflet::popupOptions(closeOnClick = FALSE, autoClose = FALSE),
        label = mapLabels(data),
        labelOptions = leaflet::labelOptions(
          style = list(
            "font-weight" = "normal",
            "font-size" = "14px",
            "direction" = "auto",
            "text-align" = "left"
          ),
          noHide = F,
          direction = 'auto'
        ),

        options = leaflet::pathOptions(pane = "markerPane")
      )

  }, .init = map)

}

#' Map Labels
#'
#' @param wdf Filtered well data
#'
#' @return formatted HTML for well data popups on the leaflet map.
#'
mapLabels <- function(wdf) {
  return(
    sprintf(
      "<strong>WELL DATA</strong><br/>
      <b style=color:blue>RESERVOIR:</b>     &nbsp; %s<br/>
      <b style=color:blue>UWI:</b>           &nbsp; %s<br/>
      <b style=color:blue>API:</b>           &nbsp; %s<br/>
      <b style=color:blue>EUR:</b>           &nbsp; %s<br/>
      <b style=color:blue>NAME:</b>          &nbsp; %s<br/>
      <b style=color:blue>OP:</b>            &nbsp; %s<br/>
      <b style=color:blue>LATERAL LENGTH:</b>&nbsp; %s<br/>
      <b style=color:blue>FIRST PROD:</b>    &nbsp; %s<br/>",
      wdf$reservoir,
      wdf$uwi,
      wdf$api,
      format(
        round(wdf$concensus_eur, 0),
        big.mark = ",",
        scientific = FALSE
      ),
      wdf$well_name,
      wdf$operator,
      (wdf$lower_perf - wdf$upper_perf),
      wdf$first_prod_date
    ) |>
      lapply(htmltools::HTML)
  )
}

#' Create Input Choices
#'
#' Used to calculate all combinations of "Reservoir", "Status" and "Drill" used to update interdependent
#' updateSelectize controls.
#'
#' @param header calculated superlocation well data
#' @param ... additional arguments, typically reservoir, status, drill and date inputs.
#' @param include_empty accounts for NA values
#'
#' @return list of possible combinations in the SL for:
#' * `Reservoir`
#' * `Well Status`
#' * `Hole Direction`
#' * `First Production Date`
#'
getChoices <- function(header, ..., include_empty = FALSE) {
  filters <- list(...)

  filters <- filters[lengths(filters) > 0]

  if (length(filters) > 0) {
    filters <- filters[sapply(filters, function(x)
      ! all(x == ""))]
  }

  choices <- vector("list", length(names(header)))

  names(choices) <- names(header)

  for (i in names(header)) {
    x <- header
    filters_sans_self <- filters[!names(filters) %in% i]

    for (j in names(filters_sans_self)) {
      x <- x |> dplyr::filter(!!as.symbol(j) %in% filters_sans_self[[j]])
    }

    if (include_empty) {
      choices[[i]] <- c("", sort(unique(x[[i]])))
    } else {
      choices[[i]] <- sort(unique(x[[i]]))
    }
  }

  return(choices)
}

#' Calculate Scaled EUR
#'
#' Normalize EUR|VOL by lateral length of well (EUR/ft), return NA if any values missing
#' eur, norm_lat_len = 10000 , ll = lower_perf - upper_perf
#' 424/2567  # 16% Horizontal well are missing upper or lower perfs
#' scaled is 'normalized' volume
#'
#' @param vol Well volume.
#' @param norm_lat_len Hypothetical lateral length of the well.
#' @param lat_len Actual lateral length of the well.
#'
#' @return The scaled volume of the hypothesized lateral length compared to the original.
#'
scaledVol <- function(vol, norm_lat_len, lat_len) {

  # (vol / lat_len) * norm_lat_len

  vol * (norm_lat_len/lat_len)^0.775
}

#' Compute Summary Data
#'
#'   UI DT::datatable       summary =  TRUE & sumEUR = FALSE = tibble for DT EUR & EUR w/NLL
#'                                                    `is.null` = return empty tibble
#'
#'   Downloadable CSV(s)    summary = FALSE & sumEUR = FALSE = datatable in wide format
#'                          summary = FALSE & sumEUR = TRUE  = summary with EUR data
#'
#' @param filtered filtered subset of the SL dataset.
#' @param multi whether or not the shapefile has one or more polygons.
#' @param eur usfsrpmsc entity EUR data.
#' @param nll reactive Normalized Lateral Length
#' @param summary flag indicating whether or not data is used for the actual UI `DTOutput`
#' @param sumEUR flag indicating whether or not to generate data in wide or EUR formats.
#' @param magnitude at what point(magnitude) do represented numbers get changed to scientific notation.
#'
#' @return `computeSummary` returns one of three data sets depending on its flags. See above.
#'
computeSummary <- function(filtered, multi, eur,
                           nll = 10000, summary = FALSE, sumEUR = FALSE, magnitude = 1e20) {

  status_order <- c("Active", "Inactive", "P & A", "TA", "Other")
  drill_order  <- c("H", "D", "V")

  # filtered can be empty if
  #    1) first_prod_date is selected that does not exist
  #    2) Only "25 Miles" radius is available (should not be part of data set)

  if(dim(filtered)[1] == 0 ||
     length(unique(filtered$aoi_region)) == 1 && filtered$aoi_region[1] == "25 Miles") {

    return(all_aoi <- tibble::tibble())

  } else {

    # filtered - full count -
    # all wells are repeated for every layer:
    # (super_location, 5, 10, 15, NO --- 25 mile radius)

    filtered <- filtered |>
      sf::st_drop_geometry() |>
      dplyr::select(-traj) |>
      dplyr::filter(aoi_region != "25 Miles")

    # CSV Summary EUR -----------------------------------------------------------------------------------#

    if (isTRUE(sumEUR)) {
      summary_w_eur <- filtered |>
        dplyr::mutate(prod_type = stringr::str_to_lower(prod_type)) |>
        dplyr::left_join(eur, by = c("entity_id" = "pden_id"), multiple = "all") |>
        dplyr::mutate(eur = concensus_eur,
                      scaled_eur = scaledVol(eur, nll, ll),
                      .keep = 'unused') |>
        tidyr::pivot_wider(
          names_from = product,
          values_from = sd:scaled_eur,
          names_expand = TRUE
        ) |>
        dplyr::select(-tidyr::ends_with(c("_na")))

      return(summary_w_eur)
    }

    # Summary EUR & Scaled EUR --------------------------------------------------------------------------#

    filtered <- filtered |>
          dplyr::select(entity_id, aoi_region, reservoir, well_status, hole_direction, ll)

    eur <- eur |>
      tidyr::pivot_wider(id_cols = c("pden_id"),
                         names_from = 'product',
                         values_from = 'concensus_eur')

    header_eur <- dplyr::left_join(x = filtered,
                                   y = eur ,
                                   by = c("entity_id" = "pden_id"),
                                   multiple = "all") |>
      dplyr::filter(!hole_direction %in% c("D", "V")) |>
      dplyr::filter(!reservoir %in% c("Bone Spring", "Bone Springs", "(N", "Cons", "Shale", "Fusselman",
                                      "Woodford", "Mississippian", "Chester Lime", "Devonian"))

    d <- header_eur |>
      dplyr::mutate(
        OilEUR = oil,
        GasEUR = gas,
        WatEUR = water,
        normll = nll,
        #ll = (lower_perf - upper_perf),
        sOilEUR = ifelse(hole_direction == "H", scaledVol(OilEUR, normll, ll), NA),
        sGasEUR = ifelse(hole_direction == "H", scaledVol(GasEUR, normll, ll), NA),
        sWatEUR = ifelse(hole_direction == "H", scaledVol(WatEUR, normll, ll), NA),
        status = dplyr::case_when(
          well_status ==   'ACTIVE' ~ 'Active',
          well_status == 'INACTIVE' ~ 'Inactive',
          well_status ==    'P & A' ~ 'P & A',
          well_status ==       'TA' ~ 'TA',
          TRUE ~ as.character('Other')
        )
      ) |>

      dplyr::group_by(aoi_region, reservoir, hole_direction) |>
      dplyr::summarize(
        Total = dplyr::n(),
        Active = sum(status == 'Active'),
        Inactive = sum(status == 'Inactive'),
        `P & A` = sum(status == 'P & A'),
        `TA` = sum(status == 'TA'),
        Other = sum(status == 'Other'),

        # EUR ------------------------------------------------------------#

        Oil.Tol = sum(!is.na(OilEUR)) / Total * 100,
        Oil.n = sum(!is.na(OilEUR)),
        Oil.Average = mean(OilEUR, na.rm = TRUE),
        Oil.Median = median(OilEUR, na.rm = TRUE),
        Oil.P10 = quantile(OilEUR, probs = .1, na.rm = TRUE),
        Oil.P90 = quantile(OilEUR, probs = .9, na.rm = TRUE),
        Oil.PHat = (Oil.Average + Oil.Median) / 2,
        Oil.P90P10 = Oil.P90 / Oil.P10,

        Gas.Tol = sum(!is.na(GasEUR)) / Total * 100,
        Gas.n = sum(!is.na(GasEUR)),
        Gas.Average = mean(GasEUR, na.rm = TRUE),
        Gas.Median = median(GasEUR, na.rm = TRUE),
        Gas.P10 = quantile(GasEUR, probs = .1, na.rm = TRUE),
        Gas.P90 = quantile(GasEUR, probs = .9, na.rm = TRUE),
        Gas.PHat = (Gas.Average + Gas.Median) / 2,
        Gas.P90P10 = Gas.P90 / Gas.P10,

        Water.Tol = sum(!is.na(WatEUR)) / Total * 100,
        Water.n = sum(!is.na(WatEUR)),
        Water.Average = mean(WatEUR, na.rm = TRUE),
        Water.Median = median(WatEUR, na.rm = TRUE),
        Water.P10 = quantile(WatEUR, probs = .1, na.rm = TRUE),
        Water.P90 = quantile(WatEUR, probs = .9, na.rm = TRUE),
        Water.PHat = (Water.Average + Water.Median) / 2,
        Water.P90P10 = Water.P90 / Water.P10,

        # Scaled EUR ----------------------------------------------------#

        Oil.AvgLL = mean(ll, na.rm = TRUE),
        Oil.ns = sum(!is.na(sOilEUR)),
        Oil.sAverage = mean(sOilEUR, na.rm = TRUE),
        Oil.sMedian = median(sOilEUR, na.rm = TRUE),
        Oil.sP10 = quantile(sOilEUR, probs = .1, na.rm = TRUE),
        Oil.sP90 = quantile(sOilEUR, probs = .9, na.rm = TRUE),
        Oil.sPHat = (Oil.sAverage + Oil.sMedian) / 2,
        Oil.sP90P10 = Oil.sP90 / Oil.sP10,

        Gas.AvgLL = mean(ll, na.rm = TRUE),
        Gas.ns = sum(!is.na(sGasEUR)),
        Gas.sAverage = mean(sGasEUR, na.rm = TRUE),
        Gas.sMedian = median(sGasEUR, na.rm = TRUE),
        Gas.sP10 = quantile(sGasEUR, probs = .1, na.rm = TRUE),
        Gas.sP90 = quantile(sGasEUR, probs = .9, na.rm = TRUE),
        Gas.sPHat = (Gas.sAverage + Gas.sMedian) / 2,
        Gas.sP90P10 = Gas.sP90 / Gas.sP10,

        Water.AvgLL = mean(ll, na.rm = TRUE),
        Water.ns = sum(!is.na(sWatEUR)),
        Water.sAverage = mean(sWatEUR, na.rm = TRUE),
        Water.sMedian = median(sWatEUR, na.rm = TRUE),
        Water.sP10 = quantile(sWatEUR, probs = .1, na.rm = TRUE),
        Water.sP90 = quantile(sWatEUR, probs = .9, na.rm = TRUE),
        Water.sPHat = (Water.sAverage + Water.sMedian) / 2,
        Water.sP90P10 = Water.sP90 / Water.sP10,

        .groups = 'drop'
      ) |>
      tidyr::pivot_longer(cols = Oil.Tol:Water.sP90P10) |>
      tidyr::separate(name, c("Phase", "B")) |>
      tidyr::pivot_wider(names_from = B, values_from = value)

    # Summary Data Hiding (Total:Other) -------------------

    if (isTRUE(summary)) {
      d[seq(2, nrow(d), 3), 4:9] <- NA
      d[seq(3, nrow(d), 3), 4:9] <- NA
    }

    # Average:sP90P10 - NA & NaN = 0 ----------------------

    d <- d |> dplyr::mutate(dplyr::across(Average:sP90P10, ~tidyr::replace_na(., 0)))

    # Sort by radius if single polygon --------------------

    if (multi) {
      all_aoi <- d |>
        dplyr::mutate(Tol = as.numeric(Tol),
                      Tol = Tol / 100)
    } else {
      all_aoi <- d |>
        dplyr::mutate(range = as.numeric(readr::parse_number(aoi_region))) |>
        dplyr::arrange(range, reservoir, match(hole_direction, drill_order)) |>
        dplyr::mutate(aoi_region = dplyr::case_when(aoi_region == "0 Super Location" ~ "Super Location",
                                                    TRUE ~ as.character(aoi_region)),
                      Tol = as.numeric(Tol),
                      Tol = Tol / 100
        ) |>
        dplyr::select(-range)
    }

    # Summary Data pre-format -----------------------------

    all_aoi <- all_aoi |>
      dplyr::mutate(dplyr::across(
        .cols = Average:sP90P10,
        .fns = ~ dplyr::case_when(
          . > magnitude ~ format(
            x = signif(., digits = 2),
            trim = TRUE,
            scientific = TRUE
          ),
          TRUE ~ format(
            x = round(., digits = 0),
            trim = TRUE,
            nsmall = 0L,
            big.mark = ",",
            scientific = FALSE
          )
        )
      ))

    # Summary Data Hiding (hole direction) ----------------

    if (isTRUE(summary)) {
      all_aoi[seq(2, nrow(all_aoi), 3), 3] <- NA
      all_aoi[seq(3, nrow(all_aoi), 3), 3] <- NA

      return(all_aoi)

    } else {

      # CSV Summary Wide Format -------------------------------------------------------------------------#

      all_aoi <- all_aoi |>
        tidyr::pivot_wider(
          id_cols = aoi_region:Other,
          names_from = Phase,
          values_from = Tol:sP90P10
        )

      return(all_aoi)
    }
  }
}

#' Calculate Rate Time
#'
#' Selected filtered wells, combine USFSRPMSC production data (decline rate) and calculate new output
#  with original and hypothesized entity normalized lateral length.
#'
#' @param filtered filtered subset of the SL dataset.
#' @param production usfsrpmsc production data.
#' @param nll reactive normalized lateral length.
#'
#' @return `calcRateTime` returns a list of:
#'  * `raw rate-time                well level production`
#'  * `scaled rate-time             well level production`
#'  * `aggregated rate-time        group level production`
#'  * `scaled aggregated rate-time group level production`
#'
calcRateTime <- function(filtered, production, nll = 10000) {

  ff <- filtered |>
    sf::st_drop_geometry() |>
    dplyr::filter(aoi_region != '25 Miles') |>
    dplyr::select(-traj) |>
    dplyr::inner_join(production, by = c("entity_id" = "pden_id"), multiple = "all") |>
    dplyr::select(entity_id, aoi_region, reservoir, hole_direction, ll, product, volume, month) |>
    dplyr::mutate(id = dplyr::row_number()) |>
    tidyr::pivot_wider(id_cols = c("entity_id", "aoi_region", "reservoir", "hole_direction", "ll", "month"),
                       names_from = product,
                       values_from = volume)

  # case when product (oil, gas, water) is missing from production data

  ff <- dplyr::bind_rows(ff, tidyr::tibble(gas = double(), oil = double(), water = double()))

  if(nrow(ff) == 0) {

    return(NULL)

  } else {

  entity_level <- ff |>
    dplyr::mutate(  scaled_oil = scaledVol(oil, nll, ll),
                    scaled_gas = scaledVol(gas, nll, ll),
                  scaled_water = scaledVol(water, nll, ll)) |>
    dplyr::select(entity_id, aoi_region, reservoir, hole_direction, ll, month,
                  oil, gas, water, scaled_oil, scaled_gas, scaled_water)

  group_level <- ff |>
    dplyr::group_by(aoi_region, reservoir, hole_direction, month) |>
    dplyr::summarize(avg_oil = mean(oil, na.rm = TRUE),
                     avg_gas = mean(gas, na.rm = TRUE),
                     avg_wat = mean(water, na.rm = TRUE),
                     avg_scaled_oil = mean(scaledVol(oil, nll, ll), na.rm = TRUE),
                     avg_scaled_gas = mean(scaledVol(gas, nll, ll), na.rm = TRUE),
                     avg_scaled_wat = mean(scaledVol(water, nll, ll), na.rm = TRUE),
                     .groups = 'keep') |>
    dplyr::select(aoi_region, reservoir, hole_direction, month, avg_oil, avg_gas,
                  avg_wat, avg_scaled_oil, avg_scaled_gas, avg_scaled_wat)

  return(list(entity_level = entity_level,
               group_level = group_level))

  }

}

#' Create datatable
#'
#' Wrapper for `DT::datatable` object. Adds a custom Shiny input button which allows the user filter options.
#'
#' @param summary_data tibble created by `computeSummary()` function - summary data.
#' @param session current session info for namespacing
#'
#' @return `createDataTable()` returns `DT::datatable` object.
#'
createDatatable <- function(summary_data, session) {
  DT::datatable(
    data = summary_data,

    colnames = NULL,
    class = "compact",
    extensions = c('RowGroup', 'Buttons'),

    options = list(
      scrollX = TRUE,
      ordering = FALSE,
      lengthMenu = c(6, 45, 90, 180),
      dom = 'Blrtip',
      buttons = list(list(
        extend = "collection",
        text = 'Filter',
        action =
          DT::JS(
            sprintf(
              "function ( e, dt, node, config ) {
                Shiny.setInputValue('%s', true, {priority: 'event'});
              }",
              session$ns("filter")
            )
          )
      )),

      columnDefs = list(
        list(visible = FALSE, targets = c(0:2)),
        list(className = 'dt-center', targets = c(3:12))
      ),

      rowGroup = list(
        dataSrc = c(1, 2),
        startRender = DT::JS(readr::read_file("./inst/app/www/datatable_row_group"))
      )
    )
  ) |>
    DT::formatPercentage(columns = 'Tol', 0) |>
    DT::formatRound(columns = c(18, 26), 1) |>
    DT::formatStyle(columns = c(11, 18), `border-right` = 'solid 1px')  |>
    DT::formatStyle(
      columns = 'Phase',
      target = 'row',
      backgroundColor = DT::styleEqual(
        levels = c('Oil', 'Gas', 'Water'),
        values = c('#EAFCEA', '#FFE5E9', '#CDE6EF')
      )
    )
}

#' Create NO datatable
#'
#' When summary_data is returned that has zero rows, need to retain the filter button.
#'
#' @param session current session info for namespacing.
#'
#' @return `createNoDatatable()` returns an empty `DT::datatable` object, as a filler with custom message.
#'
createNoDatatable <- function(session) {

  DT::datatable(
    data = data.frame(no_data = integer()),
    colnames = NULL,
    class = 'compact',
    extensions = c('Buttons'),
    options = list(
      dom = 'Blrtip',
      language =
        list(emptyTable = "Wells are outside of Super Location range (>15mi)."),
      buttons = list(
        list(
          extend = 'collection',
          text = 'Filter',
          action = DT::JS(sprintf("function(e,dt,node,config){
                                  Shiny.setInputValue('%s', true, {priority:'event'});
                                  }", session$ns("filter")))
        )
      )
    )
  )
}

#' Wells in Area
#'
#' Calculate the wells within a given radius area or polygon layer area.
#' Default is wells within the current polygon layer.
#'
#' @param wells SF object, all wells          WGS84 EPSG:4326
#' @param shapefile single polygon SF object  WGS84 EPSG:4326
#' @param aoi_region radius in miles
#'
#' @return `wellsInArea()` returns the wells object with `aoi_region` column WGS84 EPSG:4326
#'
wellsInArea <- function(wells, shapefile, aoi_region = 0) {
  text <- dplyr::case_when(aoi_region ==  0 ~ "0 Super Location",
                           aoi_region ==  1 ~ "1 Mile",
                           aoi_region ==  5 ~ "5 Miles",
                           aoi_region == 10 ~ "10 Miles",
                           aoi_region == 15 ~ "15 Miles",
                           aoi_region == 25 ~ "25 Miles",
                           TRUE ~ as.character("0 Super Location"))

  return(sf::st_join(x = wells,
                     y = sf::st_buffer(shapefile, mtm(aoi_region)),
                     join = sf::st_intersects,
                     left = FALSE) |>
           dplyr::mutate(aoi_region = text) |>
           sf::st_transform(4326))
}

#' Calculate Time Series Data
#'
#' Mutate rate time data to long format for `RscShiny::TSPlot`.
#' Any NA product values will calculate as 1.
#'
#' @param x entity or group level data.frame
#'
#' @return `makeTSData()` returns either entity or group level list with 3 data.frames:
#'
#'  * `time series(id, t, y`
#'    TODO: created attrib
#'  * `metadata(id, group_id, source, type, product)`
#'  * `annotations`
#'
makeTSData <- function(x) {

  if ("entity_id" %in% colnames(x)) {
    reservoir_data <- x |>
      # HACK: Handling of NA values needs addressed
      # This handling of NA values is a temporary patch which can be changed
      # once JBC-Inc/RscShiny/issues/20 is fixed.

      dplyr::mutate(dplyr::across(.cols = oil:scaled_water,
                                  .fns = ~replace(., is.na(.), 1))) |>  # <---
      tidyr::pivot_longer(cols = 7:12,
                          names_to = "product",
                          values_to = "y") |>
      dplyr::mutate(id = paste(
        stringr::str_pad(
          string = stringr::str_trim(stringr::str_sub(aoi_region, start = 1, end = 2)),
          width = 2,
          side = "left",
          pad = "0"
        ),
        reservoir,
        hole_direction,
        product,
        sep = "-"
      )) |>

      dplyr::mutate(t = as.Date("1900-01-01") + months(month)) |>
      dplyr::arrange(id, t) |>
      dplyr::ungroup()

  } else {

    reservoir_data <- x |>
      dplyr::rename(
        oil = avg_oil,
        gas = avg_gas,
        water = avg_wat,
        scaled_oil = avg_scaled_oil,
        scaled_gas = avg_scaled_gas,
        scaled_water = avg_scaled_wat
      ) |>
      tidyr::pivot_longer(cols = 5:10,
                          names_to = "product",
                          values_to = "y") |>
      dplyr::mutate(id = paste(
        # stringr::str_pad(
        #   string = stringr::str_trim(stringr::str_sub(
        #     aoi_region, start = 1, end = 2
        #   )),
        #   width = 2,
        #   side = "left",
        #   pad = "0"
        # ),
        trimws(aoi_region),
        reservoir,
        hole_direction,
        product,
        sep = "-"
      )) |>

      dplyr::mutate(t = lubridate::ymd("1900-01-01") + months(month)) |>
      dplyr::arrange(id, t) |>
      dplyr::ungroup()
  }

  ts_data = list(time_series = NULL, metadata = NULL, annotation = NULL)

  ts_data$time_series <- reservoir_data |>
    dplyr::select(id, t, y) |>
    dplyr::filter(y > 0)

  ts_data$metadata <- reservoir_data |>
    dplyr::group_by(id) |>
    dplyr::summarise_all(\(x) `[`(x, 1)) |>
    dplyr::mutate(source = "Odin", type = "FCST") |>
    dplyr::mutate(group_id = paste(hole_direction, aoi_region, sep = "-")) |>
    dplyr::select(id, group_id, source, type, product) |>
    dplyr::mutate(
      units = dplyr::case_when(
        product == "oil" ~ "bbl/month",
        product == "water" ~ "bbl/month",
        product == "gas" ~ "mcf/month",
        product == "scaled_oil" ~ "bbl/month",
        product == "scaled_water" ~ "bbl/month",
        product == "scaled_gas" ~ "mcf/month"
      )
    )

  ts_data$annotation <- tibble::tibble(
    id = ts_data$time_series$id[1],
    t = ts_data$time_series$t[1],
    y = ts_data$time_series$y[1],
    name = "fcst_hist_end"
  )

  ts_data
}

#' Leaflet Plugin - Control Grouped Layer
#'
#' The leaflet map is currently limited to 2 types of group layers - Basegroups and Overlay.
#' The plugin allows new Layer Controls to be added to the map, such as variable map layers.
#' Load the JS and CSS files for groupedLayerControl library plugin, register the plugin function with
#' leaflet object.
#'
#' https://stackoverflow.com/questions/67496042/leaflet-groupedlayercontrol-using-group-layers-in-r
#'
#' @param source_path path where .css and .js scripts reside
#'
#' @return `ctrlGrouped` returns an HTML dependency the leaflet can implement for the plugin.
#'
ctrlGrouped <- function(source_path) {

  htmltools::htmlDependency(
    name = 'ctrlGrouped',
    version = "1.0.0",
    # works in R and Shiny - download js/css files, then use this:
    src = c(file = source_path),
    script = "L.Control.groupedlayer.js",
    stylesheet = "L.Control.groupedlayer.css"
  )
}

#' Register Leaflet Plugin
#'
#' @param map leaflet map html widget
#' @param plugin plugin to add to map dependency
#'
#' @return leaflet map widget with added plugin.
#'
registerPlugin <- function(map, plugin) {

  map$dependencies <- c(map$dependencies, list(plugin))
  map
}

#' Current Session Information
#'
#' Obtain session info of reports created.
#'
#' @param input session input
#' @param eval_date_time date time object when report was generated
#' @param sl_basename name of original super location file
#'
#' @return `analysisInfo` returns a .csv with evaluation time, first_prod_date, reservoir, well_status,
#' hole_direction, normalized lateral length, units and superlocation file used to generate the report.
#'
analysisInfo <- function(input, eval_date_time, sl_basename) {

  x <- shiny::reactiveValuesToList(input)

  t <- tidyr::tibble(superlocation_filename = sl_basename,
                     evaluation_date = as.character(eval_date_time),
                     production_date = as.character(x$date),
                     reservoir = ifelse(is.null(x$res),
                                        NA, paste(unlist(x$res),    sep = '', collapse = ', ')),
                     status = ifelse(is.null(x$status),
                                     NA, paste(unlist(x$status), sep = '', collapse = ', ')),
                     hole_direction = ifelse(is.null(x$drill),
                                             NA, paste(unlist(x$drill),  sep = '', collapse = ', ')),
                     normalized_lateral_length = x$nll,
                     units = c("oil & water: bbl; gas: mcf"))

  return(as.data.frame(t(t)))
}

#' Validate files in superloc.zip
#'
#' When a user uploads a superlocation zipfile, it should contain the four file types needed:
#'  - .dbf  Feature Attributes
#'  - .prj  Projection Definition (CRS)
#'  - .shx  Indexed Geometry
#'  - .shp  Actual Shapefile Geometry
#'
#'  If any file types are missing: returns FALSE, user is notified which file extension is missing.
#'
#' @param dir temp dir generated by application where files are stored to be processed.
#'
#' @return logical operator indicating failure or success (TRUE).
#'
validExtensions <- function(dir) {
  needs_ext <- c("dbf", "prj", "shp", "shx")
  has_ext <- tools::file_ext(base::list.files(dir))
  missing <- !(needs_ext %in% has_ext)
  if (any(missing)) {
    shinyalert::shinyalert(
      title = "Input Error",
      text = paste0("Shapefile missing: \'", needs_ext[missing], "\' file.\n"),
      type = "error",
      size = "xs"
      )
    waiter::waiter_hide()
    return(TRUE)
  }
  return(FALSE)
}

#' Validate Coordinate Reference System
#'
#' When processing initial uploaded shapefile data, function checks to see if the shapefile contains a valid
#' coordinate reference system. If CRS is not valid, user is notified and file processing is halted.
#'
#' @param shapefile `sf` Simple Features object.
#'
#' @return logical operator indicating failure or success (TRUE).
#'
validCRS <- function(shapefile) {
  if (is.na(sf::st_crs(shapefile))) {
    shinyalert::shinyalert(
      title = "Input Error",
      text = paste0("Shapefile does not have a CRS\n",
                    "or Coordinate Reference System is malformed."),
      type = "error",
      size = "xs"
    )
    return(TRUE)
  }
  return(FALSE)
}

crsExist <- function(shapefile) {
  if (is.na(sf::st_crs(shapefile)[1]$input)) {
    shinyalert::shinyalert(
      title = "Input Error",
      text = paste0("Shapefile does not have a CRS\n",
                    "or Coordinate Reference System is malformed."),
      type = "error",
      size = "xs"
    )
    waiter::waiter_hide()
    return(TRUE)
  }
  return(FALSE)
}

#' Calculate the EPSG code needed to do calculations/buffers/transformations.
#' Will use the centeroid of the shapefile area given.
#'
#' @param shapefile Simple Features object with CRS User Input: EPSG:4326
#'
#' @return WGS84 EPSG code representing the appropriate UTM with hemisphere
#'
calcEPSG <- function(shapefile) {
  latlng <- sf::st_geometry(shapefile) |>
    sf::st_centroid() |>
    sf::st_coordinates()
  utm = (floor((latlng[1] + 180) / 6) %% 60) + 1
  if (latlng[2] > 0) {
    code <- utm + 32600 # WGS84 / UTM northern hemisphere
  } else{
    code <- utm + 32700 # WGS84 / UTM southern hemisphere
  }
  code
}

#' Create aoi_regions, polygon layers around shapefile
#'
#' @param shapefile single polygon CRS WGS84 EPSG:4326
#' @param epsg_code EPSG projected CRS code for specific shapefile area
#'
#' @return SF object EPSG:4326 with 5 polygons with a specific radius
#'
addBuffers <- function(shapefile, epsg_code) {

  distances <- c(1, 5, 10, 15)

  sf_projected <- sf::st_transform(shapefile, crs = epsg_code)

  buffers <- purrr::map(
    .x = distances,
    .f = \(x) sf::st_buffer(x = sf_projected,
                            dist = measurements::conv_unit(x, "mi", "m"))
  )

  names(buffers) <- paste0(distances, " Miles")

  buffers <- dplyr::bind_rows(buffers, .id = "aoi_region") |>
    sf::st_transform(crs = sf::st_crs("EPSG:4326"))

  buffers
}
