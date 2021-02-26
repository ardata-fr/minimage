#' @importFrom tools R_user_dir
working_directory <- function(){
  dir <- R_user_dir(package = "minimage", which = "data")
  file.path(dir, "compress-images")
}

#' @title Is 'compress-images' available
#' @description Checks if 'compress-images' is available within a directory of the user.
#' @return a single logical value.
#' @export
#' @examples
#' compress_images_available()
#' @family tools for 'compress-images'
compress_images_available <- function(){
  dir.exists(working_directory())
}

#' @export
#' @title Uninstall 'compress-images'
#' @description Removes 'compress-images'.
#' @return a single logical value, FALSE if the operation failed, TRUE otherwise.
#' @family tools for 'compress-images'
#' @examples
#' library(locatexec)
#'
#' if(exec_available("npm") &&
#'    compress_images_available()) {
#'   compress_images_uninstall()
#'   compress_images_install()
#' }
#' @family tools for 'compress-images'
#' @return a single logical value, TRUE if success.
compress_images_uninstall <- function(){
  app_dir <- working_directory()
  unlink(app_dir, recursive = TRUE, force = TRUE)
  TRUE
}


#' @export
#' @importFrom locatexec npm_exec
#' @title Install 'compress-images'
#' @description Downloads and installs 'compress-images'
#' (a "JavaScript" tool for image compression) in the user data directory.
#' @param force Whether to force to install (override) 'compress-images'.
#' @param verbose should a log be printed in the console, default to TRUE.
#' @return a single logical value, FALSE if the operation failed, TRUE otherwise.
#' @family tools for 'compress-images'
#' @examples
#' library(locatexec)
#' if(exec_available("node") && !compress_images_available()){
#'   compress_images_install()
#'   compress_images_uninstall()
#' }
#' @return a single logical value, TRUE if success.
compress_images_install <- function(force = FALSE, verbose = TRUE){

  exec_available("npm", error = TRUE)

  app_dir <- working_directory()

  de <- dir.exists(app_dir)
  if(de && !force){
    stop("The directory \"", app_dir, "\" exists. Please either delete it, ",
         "or use compress_images_install(force = TRUE).")
  } else if(de && force){
    unlink(app_dir, recursive = TRUE, force = TRUE)
  }

  dir.create(app_dir, showWarnings = FALSE, recursive = TRUE)

  package.json <- system.file(package = "minimage", "compress-images", "package.json")
  file.copy(package.json, app_dir, overwrite = TRUE)

  info <- NULL
  success <- TRUE

  curr_wd <- getwd()
  setwd(app_dir)
  tryCatch({
    info <-
      system2(
        npm_exec(),
        args = "install",
        stderr = TRUE, stdout = TRUE)
  },
  warning = function(e) {
    success <- FALSE
  },
  error = function(e) {
    success <- FALSE
  },
  finally = {
    setwd(curr_wd)
  })

  if(length(info) < 1) {
    success <- FALSE
    if(verbose) message("unknown error on operation")
    unlink(app_dir, recursive = TRUE, force = TRUE)
    return(success)
  } else if(grepl("ENOENT", info[1])) {
    success <- FALSE
    if(verbose) message(paste0(info, collapse = "\n"))
    return(success)
  }
  if(verbose && success) message(paste0(info, collapse = "\n"))

  index.js <- system.file(package = "minimage", "compress-images", "index.js")
  file.copy(index.js, to = app_dir, overwrite = TRUE)
}
