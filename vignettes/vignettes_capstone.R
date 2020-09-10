## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----loading, message=F, warning=F--------------------------------------------
library(Capstone)
library(dplyr)
library(readr)
library(lubridate)
library(leaflet)

file_name <- system.file("extdata", "signif.txt", package = "Capstone")
clean_data <- file_name %>%
              readr::read_tsv() %>%
              eq_clean_data() %>% 
              filter(COUNTRY == "CHILE", LOCATION_NAME == "Santiago") 
              

## ----timeline-----------------------------------------------------------------
library(ggplot2)
clean_data %>% 
ggplot( aes(x = DATE, 
            y = COUNTRY, 
            color = DEATHS, 
            size = EQ_PRIMARY, 
            fill = DEATHS)) + 
geom_timeline() +
geom_timelinelabel(aes(label = LOCATION_NAME, n_max = 5)) + 
        theme(legend.position="bottom",
              axis.title.x = element_blank(),
              axis.title.y = element_blank())+
        labs(size = "Richter scale value", color = "# deaths")


## ----plot, message=F, warning=F-----------------------------------------------

clean_data %>%  
  dplyr::mutate(popup_text = eq_create_label(.)) %>% 
  eq_map(annot_col = "popup_text")


