absolute_path <- function(x){

  if (length(x) != 1L)
    stop("'x' must be a single character string")
  epath <- path.expand(x)
  epath <- normalizePath(epath, "/", mustWork = file.exists(epath))
  epath
}
.onLoad <- function(libname, pkgname) {
  set_minimage_defaults(compimg_path = working_directory())
}
