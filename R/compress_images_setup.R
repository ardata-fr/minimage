minimage_global <- new.env(parent = emptyenv())
default_minimage_settings <- list(
  wd = NULL
)
minimage_global$defaults <- default_minimage_settings

#' @export
#' @importFrom utils modifyList
#' @title Modify minimage defaults settings
#'
#' @description The current settings are automatically used to
#' find compress image utility path for example. This function let you
#' set the values of these settings.
#' @param compimg_path compress image node project installed path.
#' the original project is located at `system.file(package = "minimage", "compress-images")`.
set_minimage_defaults <- function(compimg_path = NULL){
  x <- list()

  if( !is.null(compimg_path) ){
    x$wd <- compimg_path
  }

  minimage_defaults <- minimage_global$defaults

  minimage_new_defaults <- modifyList(minimage_defaults, x)
  minimage_global$defaults <- minimage_new_defaults
  invisible(minimage_defaults)
}

#' @export
#' @title Get minimage defaults settings
#'
#' @description The current settings are returned by this function.
#' @return a list containing default values.
#' @examples
#' get_minimage_defaults()
get_minimage_defaults <- function(){
  x <- minimage_global$defaults
  class(x) <- "minimage_defaults"
  x
}


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
  dir.exists(get_minimage_defaults()$wd)
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
  app_dir <- get_minimage_defaults()$wd
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

  app_dir <- get_minimage_defaults()$wd

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
