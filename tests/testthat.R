library(dplyr)
library(testthat)


test_that("filename", {


        file_name       <- system.file("extdata", "signif.txt", package = "Capstone")

        loading_data    <- file_name %>%
                                readr::read_tsv() %>%
                                Capstone::eq_clean_data()

        max_magnitude   <- loading_data %>%
                        dplyr::filter(COUNTRY == "JORDAN") %>%
                        dplyr::summarize(mag_mean = mean(EQ_PRIMARY)) %>% dplyr::pull(mag_mean)

        expect_that(loading_data %>% nrow(), is_identical_to( 6200L ))

        expect_that(max_magnitude, is_identical_to( 6.3 ))

})
