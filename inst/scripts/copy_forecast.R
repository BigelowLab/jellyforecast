# usage: copy_forecast.R [--] [--help] [--species SPECIES]
#   [--version VERSION] [--start_date START_DATE] [--path
#                                                PATH] [--crop CROP]
# 
# Copy an ecomon forecast and make a graphic
# 
# flags:
#   -h, --help     show this help message and exit
# 
# optional arguments:
#   -s, --species  the species [default: salps+coel+siph]
#   -v, --version  the version [default: v1.02+v1.02+v1.02]
#   --start_date   the starting date [default: 2026-07-30]
#   -p, --path     the destination path [default:
#                                        /mnt/s1/projects/ecocast/corecode/R/ecopmo_forecast/jellyforecast/inst/extdata]
#   -c, --crop     the region to crop to, 'none' to skip cropping [default: gom]


suppressPackageStartupMessages({
  library(ecopmodb)
  library(dplyr)
  library(argparser)
  library(stars)
  library(charlier)
  library(jellyforecast)
})

split_arg = function(x = "salps+coel+siph", sep = "+"){
  strsplit(x, sep, fixed = TRUE)[[1]]
}


Args = argparser::arg_parser("Copy an ecomon forecast and make a graphic",
                             name = "copy_forecast.R", 
                             hide.opts = TRUE) |>
  add_argument("--species",
               help = "the species",
               default = "siph+salps+coel",
               type = "character") |>  
  add_argument("--version",
               help = "the version",
               default = 'v1.02',
               type = "character") |>
  add_argument("--start_date",
               help = "the starting date",
               default = format(Sys.Date(), '%Y-%m-%d'),
               type = "character") |>
  add_argument("--path",
               help = "the destination path",
               default = jellyforecast::data_path()) |>
  add_argument("--crop",
               help = "the region to crop to, 'none' to skip cropping",
               default = "gom") |>
  parse_args()

TBL = dplyr::tibble(
  species = split_arg(Args$species),
  version = split_arg(Args$version))

OUTPATH = file.path(Args$path)
date = as.Date(Args$start_date, format = '%Y-%m-%d')
dates = seq(from = date - 5, to = date + 10, by = 'day')
CROP = if(Args$crop != "none") {
    calfinforecast::get_bb(Args$crop)
  } else {
    NULL
  }


#' Copy the raw data 
#' @return stars object
copy_rawdata = function(db, cfg){
  rawfiles = ecopmodb::compose_filename(db, cfg)
  s = stars::read_stars(rawfiles, 
                        along = list(time = db$date)) |>
    rlang::set_names("q050")

  jellyforecast::write_raster(s, cfg)
}

# Here rebuild and install the package then push
# the function looks outside of its own scope for variables - lazy, I know!
git = function(){
  devtools::document(Args$path)
  devtools::install(Args$path, upgrade = FALSE)
  
  orig = setwd(Args$path)
  
  # add
  ok = system("git add *")
  
  # commit
  date = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  msg = sprintf("git commit -a -m 'auto update %s'", date)
  ok = system(msg)
  
  # now push
  ok = system("git push origin main")
  setwd(orig)
  return(0)
}

if (!interactive()){
  
  TBL = TBL |>
    dplyr::rowwise() |>
    dplyr::group_map(
      function(tbl, key){
        cfg = ecopmodb::read_configuration(species = tbl$species,
                                           version = tbl$version)
        #path = ecopmodb::version_path(cfg)
        db  = ecopmodb::read_database(cfg) |>
          dplyr::filter(per == "day",
                        type == "q050",
                        .data$date %in% dates)
        
        
        s = copy_rawdata(db, cfg)
        cfg = jellyforecast::write_config(cfg)
        gg =jellyforecast::plot_forecast(cfg, 
                                         x = s,
                                         wrap = TRUE, 
                                         crop = CROP)
        gg = jellyforecast::save_graphics(cfg, x = gg)
        gg = jellyforecast::plot_forecast(cfg,
                                          x = s, 
                                          wrap = FALSE, 
                                          crop = CROP)
        gg = jellyforecast::save_graphics(cfg, x = gg)
        tbl |>
          dplyr::mutate(cfg = list(cfg))
      }) |>
    dplyr::bind_rows()
  r = git()
  quit(save = "no", status = r)
}
