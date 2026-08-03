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

#' Create a path if it doesn't exist
#' 
#' @export
#' @param path str one or more path descriptions
#' @param recursive logical, if TRUE then make intermediary paths as needed
#' @param ... other arguments for `dir.create`
#' @return the input path(s)
make_path = function(path, recursive = TRUE, ...){
  sapply(path,
         function(p){
           if (!dir.exists(p)) {
             ok = dir.create(p, recursive = recursive[1],...)
           }
           p
         }) |>
    unname()
}