## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----packages, echo=FALSE-----------------------------------------------------
library(Capstone)
library(dplyr)
library(readr)
library(lubridate)
library(leaflet)

file_name <- system.file("extdata", "signif.txt", package = "Capstone")
raw_data  <-  file_name %>%
              readr::read_tsv() %>%
              eq_clean_data() %>% 
              dplyr::filter(COUNTRY == "MEXICO" & lubridate::year(DATE) >= 2000) %>% 
              dplyr::mutate(popup_text = eq_create_label(.)) %>% 
              eq_map(annot_col = "popup_text")


