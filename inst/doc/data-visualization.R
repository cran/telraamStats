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

## ----eval=TRUE----------------------------------------------------------------
plot(
  gg_availability(traffic)
)

## ----eval=TRUE----------------------------------------------------------------
plot(
  gg_traffic_evolution(traffic)
)

## ----eval=TRUE----------------------------------------------------------------
plot(
  gg_traffic_evolution(traffic,
                       date_range = c('2022-01-01','2022-03-01'),
                       segment = 'RteVitre-06',
                       mode = c('car','pedestrian'),
                       direction = 'lft',
                       smoothed = FALSE,
                       agg_day = FALSE)
)

## ----eval=TRUE----------------------------------------------------------------
plot(
  gg_traffic_avg(traffic)
)

## ----eval=TRUE----------------------------------------------------------------
plot(
  gg_traffic_avg(traffic,
                 date_range = c('2022-07-01','2022-09-01'),
                 segment = 'RteVitre-06',
                 mode = 'car',
                 direction = 'rgt',
                 weekday = c('monday','friday')
                 )
)

## ----eval=TRUE----------------------------------------------------------------
plot(
  gg_traffic_avg(traffic, 
                 aggregated_by = "segment_name")
)

## ----eval=TRUE----------------------------------------------------------------
plot(
  gg_traffic_avg(traffic, 
                 aggregated_by = 'segment_name',
                 weekday = 'sunday',
                 mode = 'car',
                 direction = 'rgt')
)

## ----eval=TRUE----------------------------------------------------------------
plot(
  gg_traffic_avg(traffic, 
                 aggregated_by = 'direction',
                 weekday = 'monday',
                 mode = 'car',
                 segments = 'RteVitre-06'
                 )
)

## ----eval=TRUE----------------------------------------------------------------
plot(
  gg_traffic_avg(traffic, 
                 aggregated_by = 'mode',
                 mode = c('heavy','car'),
                 segment = 'RteVitre-06')
)

## ----eval=TRUE----------------------------------------------------------------
plot(
  gg_car_speed_histogram(traffic)
)

## ----eval=TRUE----------------------------------------------------------------
plot(
  gg_car_speed_histogram(traffic, 
                         aggregated_by = 'segment_name')
)

## ----eval=TRUE----------------------------------------------------------------
plot(
  gg_car_speed_histogram(traffic, 
                         weekday = c('monday','sunday'),
                         segments = 'RteVitre-06',
                         hours = 17:20,
                         aggregated_by = "weekday")
)

## ----eval=TRUE----------------------------------------------------------------
plot(
  gg_car_speed_v85(traffic)
)

## ----eval=TRUE----------------------------------------------------------------
plot(
  gg_car_speed_v85(traffic,
                   aggregated_by = 'segment_name')
)

## ----eval=TRUE----------------------------------------------------------------
plot(
  gg_car_speed_v85(traffic,
                   aggregated_by = 'weekday',
                   date_range = c('2022-01-01','2022-03-01'),
                   segments = 'RteVitre-06')
)

