#' Plot a forecast
#' 
#' @export
#' @param cfg configuration list
#' @param x stars object
#' @param wrap logical, if TRUE create a facet wrapped single image, otherwise
#'   a list of ggplot objects are returned.  If only one time is provided in 
#'   x then this ignored and a list is returned
#' @param crop NULL or bbox to crop the data
#' @return either a single ggplot object (facet wrapped by time) or 
#'   a list of ggplot objects (one per unit of time)
plot_forecast = function(cfg,
                         x = read_raster(cfg),
                         wrap = length(dim(x)) > 2,
                         crop = NULL){
  coastline = read_coastline()
  cities = read_cities()
  if (!is.null(crop)){
    coastline = sf::st_crop(coastline, crop)
    cities = sf::st_crop(cities, crop)
    x = st_crop(x, crop)
  }
  
  if (length(dim(x)) == 2){
    time = as.Date("1776-07-02")
  } else {
    time = stars::st_get_dimension_values(x, "time")
  }
  
  if (wrap){
    gg = ggplot2::ggplot() +
      stars::geom_stars(data = x,
                        na.action = na.omit) +
      viridis::scale_fill_viridis(limits = c(0,1)) + 
      ggplot2::geom_sf(data = coastline, color = "orange") + 
      ggplot2::labs(fill = "", 
                    x= NULL, 
                    y = NULL,
                    title = cfg$longname) + 
      ggplot2::theme(axis.text.x = ggplot2::element_blank(), 
                     axis.ticks.x = ggplot2::element_blank(),
                     axis.text.y = ggplot2::element_blank(), 
                     axis.ticks.y = ggplot2::element_blank()) + 
      ggplot2::facet_wrap(~time)
  } else {
    gg = lapply(seq_along(time),
                function(i){
                  ggplot2::ggplot() +
                    stars::geom_stars(data = dplyr::slice(x, "time", i),
                                      na.action = na.omit) +
                    viridis::scale_fill_viridis(name = "",
                                                limits = c(0,1),
                                                breaks = c(0, 1),
                                                labels = c("less likely", 
                                                           "more likely")) + 
                    ggplot2::geom_sf(data = coastline, color = "gray25", linewidth = 2) + 
                    ggplot2::geom_sf_label(data = cities,
                                           ggplot2::aes(label = .data$city)) + 
                    ggplot2::labs(title = sprintf("%s %s",
                                                  cfg$longname, 
                                                  format(time[i], "%Y-%m-%d")),
                                  x = NULL, 
                                  y = NULL) + 
                    ggplot2::theme(axis.text.x = ggplot2::element_blank(), 
                                   axis.ticks.x = ggplot2::element_blank(),
                                   axis.text.y = ggplot2::element_blank(), 
                                   axis.ticks.y = ggplot2::element_blank()) 
                })
    names(gg) = format(time, "%Y-%m-%d")
    
  }
  invisible(gg)
}