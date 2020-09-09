#' Quite white spaces and then transform to numeric
#'
#' More detailed description
#'
#' @param x vector to transform to numeric
#'
#' @importFrom dplyr "%>%"
#'
#' @return numeric vector
#'
#' @examples
#' \dontrun{
#' x <- c(" 12", "13 ", " 14 ")
#' trimws_to_numeric(x)
#' }
#'
trimws_to_numeric = function(x){ trimws(x) %>% as.numeric()}

#' Remove blanks spaces and then transform to numeric
#'
#' More detailed description
#'
#' @param LOCATION_NAME vector that include country and location in the same string
#'
#' @importFrom dplyr "%>%"
#' @importFrom stringr str_to_title
#'
#' @return vector that contains only location
#'
#' @examples
#' \dontrun{
#' x <- c("JORDAN:  BAB-A-DARAA,AL-KARAK", "SYRIA:  UGARIT",
#'        "TURKMENISTAN:  W", "GREECE:  THERA ISLAND (SANTORINI)", "ISRAEL:  ARIHA (JERICHO)" )
#' eq_location_clean(x)
#' }
#'
#' @export
eq_location_clean <- function(LOCATION_NAME){

        strsplit(LOCATION_NAME, split = ":")%>%
                lapply(FUN = function(x){ x[length(x)]}) %>%
                unlist() %>%
                trimws() %>% stringr::str_to_title()
}

#' Clean Earthquake data
#'
#' More detailed description
#'
#' @param Data Earthquake data
#'
#' @importFrom dplyr "%>%"
#' @importFrom dplyr mutate
#' @importFrom lubridate ymd years days
#'
#' @return Earthquake data Clean
#'
#' @examples
#' \dontrun{
#' eq_clean_data(Data)
#' }
#' @export
eq_clean_data <- function(Data){

        Data%>%
                dplyr::mutate(MONTH = ifelse(is.na(MONTH),0, MONTH),
                              DAY = ifelse(is.na(DAY),0, DAY),
                              DEATHS = as.numeric(DEATHS),
                              EQ_PRIMARY = as.numeric(EQ_PRIMARY)) %>%
                dplyr::mutate(DATE = ifelse(MONTH>0,
                                            ifelse(DAY>0,
                                                   lubridate::ymd("0000-01-01")+lubridate::years(YEAR)+months(MONTH-1)+lubridate::days(DAY-1),
                                                   lubridate::ymd("0000-01-01")+lubridate::years(YEAR)+months(MONTH-1)+lubridate::days(DAY)),
                                            lubridate::ymd("0000-01-01")+lubridate::years(YEAR)+months(MONTH)+lubridate::days(DAY)),
                              DATE = as.Date(DATE, origin = "1970-01-01"),
                              LATITUDE = trimws_to_numeric(LATITUDE),
                              LONGITUDE = trimws_to_numeric(LONGITUDE),
                              LOCATION_NAME = eq_location_clean(LOCATION_NAME))

}



#' Plot timeline earthquakes
#'
#' More detailed description
#'
#' @return Earthquake data Clean
#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomTimeline <- ggplot2::ggproto("GeomTimeline", ggplot2::Geom,
                        required_aes = c("x"),
                        default_aes = ggplot2::aes(shape = 20, color = "black", size = 1, alpha = 0.6),
                        draw_key = ggplot2::draw_key_point,
                        draw_panel = function(data, panel_scales, coord) {

                                coords <- coord$transform(data, panel_scales)

                                if(is.null(coords$y)){ coords$y  = 0.3}

                                points <- grid::pointsGrob(x = coords$x,
                                                           y = coords$y,
                                                           size = grid::unit(coords$size, "mm"),
                                                           pch = coords$shape,
                                                           gp = grid::gpar(alpha = coords$alpha,
                                                                           col = coords$colour))


                                coords$xmin = min(coords$x)
                                coords$xmax = max(coords$x)

                                lower <- grid::segmentsGrob(x0 = coords$xmin,
                                                            x1 = coords$xmax,
                                                            y0 = coords$y,
                                                            y1 = coords$y,
                                                            gp = grid::gpar(alpha = 0.6,
                                                                            lwd = 1,
                                                                            col = "grey60"))

                                base <- grid::segmentsGrob(x0 = 0,
                                                           x1 = 1,
                                                           y0 = 0,
                                                           y1 = 0,
                                                           gp = grid::gpar(alpha = coords$alpha))

                                grid::gTree(children = grid::gList(points, lower, base))

                        }
)

#' Plot timeline earthquakes
#'
#' More detailed description
#'
#' @param mapping Set of aesthetic mappings created by aes() or aes_()
#' @param data The data to be displayed in this layer. There are three options:
#'         If NULL, the default, the data is inherited from the plot data as specified in the call to ggplot().
#'         data.frame, or other object, will override the plot data. All objects will be fortified to produce a data frame. See
#'         fortify() for which variables will be created.
#'         A function will be called with a single argument, the plot data.
#'         The return value must be a data.frame, and will be used as the layer data. A function can be created from a formula (e.g. ~ head(.x, 10)).
#' @param stat stat parameter
#' @param position position parameter
#' @param show.legend show.legend parameter
#' @param na.rm na.rm parameter
#' @param inherit.aes  inherit.aes parameter
#' @param ... other parameters
#'
#' @return Earthquake data Clean
#'
#' @examples
#' \dontrun{
#' eq_clean_data(Data)
#' }
#' @export
geom_timeline <- function(mapping = NULL, data = NULL, stat = "identity",
                          position = "identity", show.legend = NA,
                          na.rm = FALSE, inherit.aes = TRUE, ...) {
        layer(
                data = data,
                mapping = mapping,
                stat = stat,
                geom = GeomTimeline,
                position = position,
                show.legend = show.legend,
                inherit.aes = inherit.aes,
                params = list(na.rm = na.rm, ...)
        )
}

#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomTimelineLabel <- ggplot2::ggproto("GeomTimelineLabel", ggplot2::Geom,
                             required_aes = c("x", "label"),
                             default_aes = ggplot2::aes(n_max = 10),
                             draw_key = ggplot2::draw_key_point,
                             draw_panel = function(data, panel_scales, coord) {
                                     coords <- coord$transform(data, panel_scales)
                                     # print(coords)
                                     if(is.null(coords$y)){ coords$y  = 0.3}
                                     if(is.null(coords$n_max)){ coords$n_max  = 10}


                                     n_max = coords$n_max[1]

                                     coords_n_max <- coords %>% group_by(y) %>% top_n(n=n_max[1], wt = size)

                                     text_HQ <- grid::textGrob(label = coords_n_max$label,
                                                               x = coords_n_max$x,
                                                               y = coords_n_max$y*1.1,vjust = 0, hjust = 0, rot = 45)

                                     lines_HQ <- grid::segmentsGrob(x0 = coords_n_max$x,
                                                                    y0 = coords_n_max$y,
                                                                    x1 = coords_n_max$x,
                                                                    y1 = coords_n_max$y*1.1,
                                                                    gp = grid::gpar(alpha = 0.6,
                                                                                    lwd = 1,
                                                                                    col = "grey60")
                                     )

                                     grid::gTree(children = grid::gList(text_HQ, lines_HQ))
                             }
)

#' Plot timeline earthquakes
#'
#' More detailed description
#'
#' @inheritParams geom_timeline
#'
#' @examples
#' \dontrun{
#' eq_clean_data(Data)
#' }
#' @export
geom_timelinelabel <- function(mapping = NULL, data = NULL, stat = "identity",
                               position = "identity", show.legend = NA,
                               na.rm = FALSE, inherit.aes = TRUE, ...) {
        layer(
                data = data,
                mapping = mapping,
                stat = stat,
                geom = GeomTimelineLabel,
                position = position,
                show.legend = show.legend,
                inherit.aes = inherit.aes,
                params = list(na.rm = na.rm, ...)
        )
}


#' Maps the epicenters
#'
#' More detailed description
#'
#' @param DATA Earthquake data
#' @param annot_col column with information to show
#' @param radius radius circle
#'
#' @importFrom dplyr "%>%"
#' @import leaflet
#'
#' @return Earthquake data Clean
#'
#' @examples
#' \dontrun{
#'file_name <- system.file("extdata", "signif.txt", package = "Capstone")
#'raw_data  <-  file_name %>%
#'        readr::read_tsv() %>%
#'        eq_clean_data() %>%
#'        dplyr::filter(COUNTRY == "MEXICO" & lubridate::year(DATE) >= 2000) %>%
#'        dplyr::mutate(popup_text = eq_create_label(.)) %>%
#'        eq_map(annot_col = "popup_text")
#' }
#' @export
eq_map <- function(DATA, annot_col = "DATE", radius = 15){

        print(leaflet() %>%
                      addTiles() %>%
                      addCircleMarkers(data = DATA, radius = radius,
                                       lng = ~ LONGITUDE, lat = ~ LATITUDE, popup = ~ eval(expr = parse(text = annot_col))))


}

#' Creates an HTML label
#'
#' More detailed description
#'
#' @param DATA Earthquake data
#'
#' @return Earthquake data Clean
#'
#' @examples
#' \dontrun{
#' file_name <- system.file("extdata", "signif.txt", package = "Capstone")
#' raw_data  <-  file_name %>%
#'        readr::read_tsv() %>%
#'        eq_clean_data() %>%
#'        dplyr::filter(COUNTRY == "MEXICO" & lubridate::year(DATE) >= 2000) %>%
#'        dplyr::mutate(popup_text = eq_create_label(.)) %>%
#'        eq_map(annot_col = "popup_text")
#' }
#' @export
eq_create_label <- function(DATA){

        paste(
                ifelse(!is.na(DATA$LOCATION_NAME),paste("<b>Location:</b>",DATA$LOCATION_NAME), ""),
                ifelse(!is.na(DATA$EQ_PRIMARY),paste("<b>Magnitude:</b>",DATA$EQ_PRIMARY), ""),
                ifelse(!is.na(DATA$DEATHS),paste("<b>Total deaths:</b>",DATA$DEATHS), ""), sep = "<br/>")

}
