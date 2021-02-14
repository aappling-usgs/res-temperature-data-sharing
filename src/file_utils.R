#' Call gd_get (without any scipiper target builds) on a file in another repo on
#' this file system, downloading that file from a shared Drive cache into that
#' other repo
#'
#' @param ind_file the indicator file of the data file in the other repo, to be
#'   downloaded to the corresponding location in that other repo
#' @param repo the relative file path of the other repo. Usually it'll be right
#'   alongside the current repo, e.g., '../lake-temperature-model-prep'
gd_get_elsewhere <- function(ind_file, repo, ...) {
  # switch to the elsewhere repo, with plans to always switch back to this one before exiting the function
  this_repo <- getwd()
  on.exit(setwd(this_repo))
  setwd(repo)
  
  # fetch the file down to that elsewhere repo
  gd_get(ind_file, ...)
}

# Write a layer of an sf object as a zipped-up shapefile
sf_to_zip <- function(zip_filename, sf_object, layer_name){
  cdir <- getwd()
  on.exit(setwd(cdir))
  dsn <- tempdir()
  
  sf::st_write(sf_object, dsn = dsn, layer = layer_name, driver="ESRI Shapefile", delete_dsn=TRUE) # overwrites
  
  files_to_zip <- data.frame(filepath = dir(dsn, full.names = TRUE), stringsAsFactors = FALSE) %>%
    mutate(filename = basename(filepath)) %>%
    filter(str_detect(string = filename, pattern = layer_name)) %>% pull(filename)
  
  setwd(dsn)
  zip(file.path(cdir, zip_filename), files = files_to_zip)
  setwd(cdir)
}