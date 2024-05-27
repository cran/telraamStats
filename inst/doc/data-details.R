## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

knitr::knit_engines$set(yml = function(options) {
})

knitr::opts_chunk$set(eval = FALSE)


## ----setup, eval = TRUE-------------------------------------------------------
library(telraamStats)

## ----eval=FALSE, results='asis'-----------------------------------------------
#  head(traffic)

## ----echo=FALSE, eval=TRUE----------------------------------------------------
knitr::kable(head(traffic))

## -----------------------------------------------------------------------------
#  if (!require("usethis")) install.packages("usethis") #if you haven't installed 'usethis' already
#  usethis::edit_r_environ()

## -----------------------------------------------------------------------------
#  set_telraam_token(token = 'YourTelraamAPIToken')

## -----------------------------------------------------------------------------
#  get_telraam_token()

## ----eval=FALSE---------------------------------------------------------------
#  create_config(create_directory = TRUE)

## ----eval=FALSE, echo=TRUE----------------------------------------------------
#  list_of_segments = list("Burel"= "9000002156", "Vitre" = "9000001844")
#  create_config(segments = list_of_segments, create_directory = FALSE)

## ----eval=FALSE---------------------------------------------------------------
#  retrieve_sensor(segment_name = "segment-01",
#                  start_date = as.Date('2023-09-01'),
#                  end_date = as.Date('2023-09-30'))

## ----eval=FALSE---------------------------------------------------------------
#  write_update_data(segment_name = "segment-01",
#                    start_date = as.Date('2023-09-01'),
#                    end_date = as.Date('2023-09-30'),
#                    create_directory = FALSE) #TRUE if you want to save the data in a permanent folder

## -----------------------------------------------------------------------------
#  import_sensor(list_sensor = c("segment-01","segment-02"))

