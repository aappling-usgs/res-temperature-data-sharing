# Functions for munging data from the lake-temperature-model-prep repo

# in_ind and in_repo are partially redundant by design: in_ind is a complete
# file path and thus can be hashed by scipiper to detect changes, while in_repo
# allows an unambiguous distinction between the part of the in_ind path that
# navigates you into another repo and the part that should be used to call
# gd_get within that repo.

fetch_filter_res_polygons <- function(out_rds, in_ind, in_repo, site_ids) {
  # pull the data file down to that other repo
  gd_get_elsewhere(gsub(in_repo, '', in_ind, fixed=TRUE), in_repo)
  
  # read and filter to just the specified sites
  as_data_file(in_ind) %>%
    readRDS() %>%
    filter(site_id %in% !!site_ids) %>%
    st_zm(drop = TRUE, what = "ZM") # the canonical lakes file has 3D multipolygons but the Z range is 0 to 0, so let's drop it down to 2D
  # return object rather than writing file as other functions in this .R file do
}

fetch_filter_tibble <- function(out_rds, in_ind, in_repo, site_ids) {
  # pull the data file down to that other repo
  gd_get_elsewhere(gsub(in_repo, '', in_ind, fixed=TRUE), in_repo)
  
  # read and filter to just the specified sites
  as_data_file(in_ind) %>%
    readRDS() %>%
    filter(site_id %in% !!site_ids) %>%
    saveRDS(out_rds)
}

fetch_filter_nml <- function(out_rds, in_ind, in_repo, site_ids) {
  # pull the data file down to that other repo
  gd_get_elsewhere(gsub(in_repo, '', in_ind, fixed=TRUE), in_repo)
  
  # read and filter to just the specified sites
  as_data_file(in_ind) %>%
    readRDS() %>%
    .[site_ids] %>%
    saveRDS(out_rds)
}

confirm_meteo_staged <- function(csv_file) {
  stopifnot(file.exists(csv_file))
}
#' @param nml_rds the *filtered* nml_list, for which all values of meteo_fl are files that we want to pull
fetch_meteo_files <- function(out_yml, nml_rds) {
  nml_list <- readRDS(nml_rds)
  task_plan <- create_task_plan(
    task_names = sort(unique(purrr::map_chr(nml_list, 'meteo_fl'))),
    task_steps = list(
      fetch = create_task_step(
        step_name = 'fetch',
        target_name = function(task_name, step_name, ...) {
          sprintf('in_data/%s', task_name)
        },
        command = function(task_name, step_name, target_name, ...) {
          # for now, we're putting meteo files manually into in_data, so all we
          # do here is confirm that the requisite files exist. Eventually we
          # should pull data from a neighboring repo, or Yeti, or something,
          # with this command
          sprintf("confirm_meteo_staged(I('%s'))", target_name)
        }
      )
    ),
    add_complete = FALSE
  )
  
  task_yml <- 'meteo_tasks.yml'
  create_task_makefile(
    task_plan = task_plan,
    makefile = 'meteo_tasks.yml',
    final_targets = out_yml,
    as_promises = TRUE,
    include = c(),
    packages = 'scipiper',
    sources = c('src/fetch_filter.R'))
  
  loop_tasks(task_plan, task_yml, num_tries=1)
  
  file.remove(task_yml)
  
}

#' Read a feather file from another repo, filter it to the specified site_ids, and write a copy locally. This function blindly assumes the source file is up to date in the other repo, no checking
copy_filter_feather <- function(out_feather, in_feather, site_ids) {
  
  # read and filter to just the specified sites
  arrow::read_feather(in_feather) %>%
    filter(res_id %in% site_ids) %>%
    arrow::write_feather(out_feather)
}
