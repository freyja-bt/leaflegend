
# leaflegend

This package provides extensions to the leaflet package to 
customize leaflet legends without adding an outside css file to the output 
to style legends. The legend extensions allow the user to add images to 
legends, style the labels of the  legend items, change orientation of the 
legend items, use different symbologies, and style axis ticks. Syntax and
style is consistent with the leaflet package.

## Installation

You can install the released version of leaflegend from [CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("leaflegend")
```

## Example

Use `addLegend*()` to create easily customizable legends for leaflet.

``` r
library(leaflegend)
library(leaflet)
data(quakes)
quakes[['group']] <- sample(c('A', 'B', 'C'), nrow(quakes), replace = TRUE)
factorPal <- colorFactor('Dark2', quakes$group)
leaflet() %>%
  addTiles() %>%
  addCircleMarkers(
    data = quakes,
    lat = ~ lat,
    lng = ~ long,
    color = ~ factorPal(group),
    opacity = 1,
    fillOpacity = 1
  ) %>%
  addLegendFactor(
    pal = factorPal,
    title = htmltools::tags$div('addLegendFactor', style = 'font-size: 24px; color: red;'),
    labelStyle = 'font-size: 18px; font-weight: bold;',
    orientation = 'horizontal',
    values = quakes$group,
    position = 'topright',
    shape = 'triangle',
    width = 30,
    height = 30
  )
```

