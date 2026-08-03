#' Retrieve the package or package data path
#' 
#' The package path refers to the installed package path, while data path 
#' refers to the development package data path (inst/extdata). 
#' 
#' @export
#' @param ... file path segments to append to the root path
#' @param root the root package or package data path
#' @return package or package data path
data_path = function(...,
  root =  "/mnt/s1/projects/ecocast/corecode/R/ecopmo_forecast/jellyforecast/inst/extdata"){
  file.path(root, ...)
}


#' @export
#' @rdname data_path
package_path = function(...,
  root =  system.file("extdata", package = "jellyforecast")){
  file.path(root, ...)
}

#' Read the product table
#' 
#' @export
#' @param path chr data path
#' @param extra chr, vector of extras to add as columns such as "daily" 
#'   and "wrapped" images as well as raw "data" (default "daily", "wrapped")
#' @return data frame with species, version, cfg and possibly daily, wrapped and data columns
read_products = function(path = package_path(),
                         extra = c("daily", "wrapped", "data")[1:2]){
  y = list_configs(path = path)
  if (length(y) > 0){
    cfg = lapply(y, yaml::read_yaml)
    x = dplyr::tibble(
      species = sapply(cfg, "[[", "species"),
      version = sapply(cfg, "[[", "version"),
      longname = sapply(cfg, "[[", "longname"),
      cfg = cfg)
    if ("daily" %in% extra){
      x = x |>
        dplyr::mutate(daily = lapply(x$cfg,
                                     function(cfg){
                                       list_images(cfg, what = "daily")
                                     }))
    } # daily?
    if ("wrapped" %in% extra){
      x = x |>
        dplyr::mutate(wrapped = lapply(x$cfg,
                                     function(cfg){
                                       list_images(cfg, what = "wrapped")
                                     }))
    } # wrapped?   
    if ("data" %in% extra){
      x = x |>
        dplyr::mutate(data = lapply(x$cfg,
                                     function(cfg){
                                       read_rasters(cfg)
                                     }))
    } # data?   
    
  } else {
    x = dplyr::tibble(
      species = "",
      version = "",
      longname = "",
      cfg = list() ) |>
      dplyr::slice(0)
  }
  x
}


#' Read and write raster data
#' 
#' @export
#' @param x stars object
#' @param cfg configuration list
#' @param path chr data path
#' @return for `write_raster` and `read_raster` a stars object
write_raster = function(x, 
                        cfg,
                        path = data_path()){
  filename = file.path(path, "species", cfg$species, cfg$version, "data.Rds")
  odir = jellyforecast::make_path(dirname(filename))
  saveRDS(x, filename[1])
  invisible(x)
}

#' @rdname write_raster
#' @export
read_raster = function(cfg,
                       path = package_path()){
  filename = file.path(path, "species", cfg$species, cfg$version, "data.Rds")
  readRDS(filename[1])
}


#' Read, write and list configuration lists
#' 
#' @export
#' @param x configuration list
#' @param path chr, data path
#' @return for `write_config` and `read_config` a configuration list
write_config = function(x,
                        path = data_path()){
  filename = file.path(path, "species", x$species, x$version, "config.yaml")
  odir = jellyforecast::make_path(dirname(filename))
  yaml::write_yaml(x, filename[1])
  invisible(x)
}

#' @rdname write_config
#' @export
read_config = function(x,
                       path = package_path()){
  filename = file.path(path, "species", cfg$species, cfg$version, "config.yaml")
  yaml::read_yaml(filename[1])
}

#' @rdname write_config
#' @export
list_configs = function(path = package_path()){
  list.files(file.path(path, "species"),
             pattern =  "^.*\\.yaml$",
             recursive = TRUE,
             full.names = TRUE)
}

#' Read a coastline or cities
#' 
#' Coastline made with Natural Earth. Free vector and raster map data at naturalearthdata.com.
#'  
#' @export
#' @param path chr data path
#' @return sfc (geometry) object
read_coastline = function(path = package_path()){
  filename = file.path(path, "coastline.Rds")
  readRDS(filename[1])
}

#' @rdname read_coastline
#' @export
read_cities = function(path = package_path()){
  filename = file.path(path, "cities.Rds")
  readRDS(filename[1])
}



#' Save graphics as PNGs
#' 
#' @export
#' @param x either a ggplot object (facte wrapped by time) or a list
#'   of daily graphics
#' @param path the path to write to
#' @param wipe logical, when writing daily files, wipe the existing ones first?
#' @return the input object
save_graphics = function(cfg, 
                         x = plot_forecast(cfg),
                         path = data_path(),
                         wipe = TRUE){
  opath = file.path(path, "species",  cfg$species, cfg$version)
  if (inherits(x, "ggplot")){
    # one item
    ofile = file.path(opath, "wrapped.png")
    suppressMessages(ggplot2::ggsave(ofile, plot = x))
  } else {
    if (wipe){
      imagespath = file.path(opath,"images")
      if (dir.exists(imagespath)){
        files = list.files(imagespath, full.names = TRUE)
        if (length(files) > 0) ok = file.remove(files)
      }
    }
    opath = make_path(imagespath)
    # a named list
    ok = lapply(names(x),
      function(nm){
        ofile = file.path(imagespath, sprintf("%s.png", nm))
        suppressMessages(ggplot2::ggsave(ofile, plot = x[[nm]]))
      })
  }
  invisible(x)
}


#' List images
#' 
#' @export
#' @param cfg configuration list
#' @param what chr either "wrapped" (default) or "daily"
#' @param path chr the path to search
#' @param chr vector of files (possibly named)
list_images = function(cfg,
                       what = c("wrapped", "daily")[1],
                       path = package_path()){
  path = file.path(path, "species", cfg$species, cfg$version)
  switch(tolower(what[1]),
         "wrapped" = file.path(path, "wrapped.png"),
         "daily" = {
           files = list.files(file.path(path, "images"), full.names = TRUE)
           names(files) = gsub(".png", "", basename(files), fixed = TRUE)
           files
         },
         stop("what option not known: ", what[1]))
}