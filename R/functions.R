#' Quite white spaces and then transform to numeric
#'
#' More detailed description
#'
#' @param x vector to transform to numeric
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
                                                   ymd("0000-01-01")+years(YEAR)+months(MONTH-1)+days(DAY-1),
                                                   ymd("0000-01-01")+years(YEAR)+months(MONTH-1)+days(DAY)),
                                            ymd("0000-01-01")+years(YEAR)+months(MONTH)+days(DAY)),
                              DATE = as.Date(DATE, origin = "1970-01-01"),
                              LATITUDE = trimws_to_numeric(LATITUDE),
                              LONGITUDE = trimws_to_numeric(LONGITUDE),
                              LOCATION_NAME = eq_location_clean(LOCATION_NAME))

}

