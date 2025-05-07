#' @export
#'
#' @rdname addLeafLegends
#'
addLegendDashFactor <- function(map,
                            pal,
                            values,
                            title = NULL,
                            labelStyle = '',
                            shape = 'rect',
                            orientation = c('vertical', 'horizontal'),
                            width = 24,
                            height = 24,
                            opacity = 1,
                            fillOpacity = opacity,
                            group = NULL,
                            className = 'info legend leaflet-control',
                            naLabel = 'NA',
                            data = leaflet::getMapData(map),
                            dashArray = "none",
                            ...) {
  stopifnot( attr(pal, 'colorType') == 'factor' )
  stopifnot( width >= 0 && height >= 0 )
  orientation <- match.arg(orientation)
  values <- parseValues(values = values, data = data)
  hasNa <- any(is.na(values))
  values <- sort(unique(values))
  labels <- sprintf(' %s', values)
  colors <- pal(values)
  htmlElements <- makeLegendDashCategorical(shape = shape, labels = labels,
                                        colors = colors,
                                        labelStyle = labelStyle,
                                        height = height, width = width,
                                        opacity = opacity,
                                        fillOpacity = fillOpacity,
                                        orientation = orientation,
                                        title = title,
                                        hasNa = hasNa,
                                        naLabel = naLabel,
                                        naColor = pal(NA),
                                        dashArray = dashArray)
  leaflegendAddControl(map, html = htmltools::tagList(htmlElements),
                       className = className, group = group, ...)
}

makeLegendDashCategorical <- function(shape, labels, colors, labelStyle, height,
                                  width, opacity, fillOpacity, orientation, title,
                                  hasNa, naLabel, naColor, dashArray = "none") {
  htmlElements <- Map(
    f = makeLegendSymbol,
    shape = shape,
    label = labels,
    color = colors,
    labelStyle = labelStyle,
    height = height,
    width = width,
    opacity = opacity,
    fillOpacity = fillOpacity,
    dashArray = dashArray,
    'stroke-width' = 1
  )
  if ( orientation == 'vertical' ) {
    htmlElements <- lapply(htmlElements, htmltools::tagList,
                           htmltools::tags$br())
  }
  htmlElements <- addTitle(title = title, htmlElements = htmlElements)
  htmlElements <- addNa(hasNa = hasNa, htmlElements = htmlElements,
                        shape = shape, labels = naLabel, colors = naColor, labelStyle = labelStyle,
                        height = height, width = width, opacity = fillOpacity,
                        fillOpacity = fillOpacity, strokeWidth = 1)
  htmlElements
}
