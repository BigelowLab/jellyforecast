#' Retrieve a bounding box
#' 
#' @export
#' @param reg chr, the name of the region
#' @return a bounding box ("bbox" class)
get_bb = function(reg = "gom"){
  switch(tolower(reg[1]),
         "gom" = c(xmin = -72, ymin = 39, xmax = -63, ymax = 46) |>
           sf::st_bbox(crs = 4326),
         stop("Region not known:", reg[1]))
}
