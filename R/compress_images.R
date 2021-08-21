#' @export
#' @importFrom locatexec exec_available node_exec
#' @title Minify size your images
#' @description Minify size of images located in a directory
#' and output results in another directory.
#'
#' You will be able to use this function only if
#'
#' * the program 'node.js' is installed on your machine,
#' * the command [compress_images_install()] has been executed once
#' on your machine (it installs a set of "npm" packages).
#' @param input,output input and output directories
#' @param verbose should a log be printed in the console, default to TRUE.
#' @param overwrite logical; should existing destination files be overwritten?
#' @param jpg_quality Scale quantization tables to adjust image quality. Quality
#' is "0" (worst) to "100" (best); default is "75".
#' @param png_quality defined as `"min-max"`, instructs pngquant to use the least
#' amount of colors required to meet or exceed the max quality. If conversion results
#' in quality below the min quality the image won't be saved.
#' @param gif_colors Reduce the number of distinct colors in each output GIF to `gif_colors` or less.
#' It must be between 2 and 256. This can be used to shrink output GIFs or eliminate any local color tables.
#' @examples
#' library(minimage)
#' library(locatexec)
#'
#' was_avail <- compress_images_available()
#'
#' if(exec_available("node") && exec_available("npm")){
#'
#'   if(!was_avail)
#'     compress_images_install(force = TRUE)
#'
#'   # generate dest folder
#'   new_dir <- tempfile()
#'   dir.create(new_dir)
#'
#'   # run compression
#'   z <- compress_images(system.file(package = "minimage", "test-files"), new_dir)
#'
#'   print(z)
#'
#'   if(!was_avail)
#'     compress_images_uninstall()
#'
#' }
#' @return a data.frame with details about input files and compressed files.
compress_images <- function(input, output = NULL,
                            verbose = TRUE, overwrite = FALSE,
                            jpg_quality = "75", png_quality = "20-50", gif_colors = "64"){

  if(!dir.exists(input)){
    stop(shQuote(input), " does not exist")
  }
  if(!dir.exists(output)){
    stop(shQuote(output), " does not exist")
  }

  compimg_dir <- get_minimage_defaults()$wd
  if(!compress_images_available())
    stop("compress-images is not available, run `compress_images_install()` to install it.")
  if(substr(output, nchar(output), nchar(output) ) != "/") {
    output <- paste0(output, "/")
  }
  if(substr(input, nchar(input), nchar(input) ) != "/") {
    input <- paste0(input, "/")
  }

  init_output <- output
  init_input <- input

  output <- paste0(absolute_path(output), "/")
  input <- paste0(absolute_path(input), "/")

  exec_available("node", error = TRUE)
  exec_available("npm", error = TRUE)

  arg0 <- shQuote(paste0(input, "**/*.{jpg,JPG,jpeg,JPEG,png,svg,gif}"), type = "cmd")

  if(!compress_images_available()){
    stop("'compress-images' is not in your user data directory,",
         " run `compress_images_install()` to install it")
  }
  path_log <- tempfile()
  dir.create(path_log)
  tmp_out <- paste0(absolute_path(tempfile()), "/")
  dir.create(tmp_out)

  info <- NULL
  success <- TRUE

  png_quality <- paste0("--quality=", png_quality)


  curr_wd <- getwd()
  setwd(compimg_dir)

  tryCatch({
    info <-
      system2(
        node_exec(),
        args = c(
          "index.js",
          arg0,
          shQuote(tmp_out, type = "cmd"),
          shQuote(path_log, type = "cmd"),
          shQuote(jpg_quality, type = "cmd"),
          shQuote(png_quality, type = "cmd"),
          shQuote(gif_colors, type = "cmd")
        ),
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

  if(success) {
    success <- all(info[grep("## error", info, fixed = TRUE) + 1] %in% "null")
  }

  if(!success){
    msg <- paste0(extract_errors(info) , collapse = "\n")
  }
  if(verbose && !success){
    message("#> something wrong happen :\n")
    message(msg)
  }

  # ----
  log_files <- list.files(path_log, full.names = TRUE)
  if(success && verbose && length(log_files)>0) {
    log_str <- lapply(log_files, readLines)
    log_str <- vapply(log_str, paste0, FUN.VALUE = "", collapse = "\n")
    log_str <- paste0(log_str , collapse = "\n-------------\n")
    success <- FALSE
    message(log_str)
    out <- process_data()
  } else if(success && !is.null(info)) {
    out <- extract_statistics(info)
  } else {
    success <- FALSE
    if(verbose) message("unknown error on operation")
    out <- process_data()
  }
  out$path_out_new <- out$path_out
  out$path_out <- gsub(tmp_out, output, out$path_out_new, fixed = TRUE)

  for(i in unique(dirname(out$path_out))){
    dir.create(i, recursive = TRUE, showWarnings = FALSE)
  }

  result <- file.copy(from = out$path_out_new, to = out$path_out, overwrite = overwrite)

  out$copied <- result

  out$path_out_new <- NULL
  out$path_out <- gsub(output, init_output, out$path_out, fixed = TRUE)
  out$input <- gsub(input, init_input, out$input, fixed = TRUE)

  attr(out, "success") <- success

  if(verbose && success) {
    message("#> all images have been treated with no issue")
  } else if(verbose && !success) {
    message("#> something went with an error or a warning, see logs upper")
  }

  out
}

extract_file <- function(txt, reg_marker){
  input <- txt[grepl(reg_marker, txt)]
  gmatch <- regexpr("'(.*)'", input)
  result <- regmatches(input,gmatch)
  gsub("(^'|'$)", "", result)
}
extract_num <- function(txt, reg_marker){
  input <- txt[grepl(reg_marker, txt)]
  gmatch <- regexpr("[[:digit:]\\.]+", input)
  as.numeric(regmatches(input,gmatch))
}

extract_errors <- function(info){
  sta_index_start <- grep("## error", info, fixed = TRUE) + 1
  sta_index_end <- grep("## completed", info, fixed = TRUE) - 1
  info_list <- list()
  for(id in seq_along(sta_index_start)){
    info_list[[id]]  <- info[sta_index_start[id]:sta_index_end[id]]
  }
  info_list <- do.call(c, info_list)
  info_list
}

extract_statistics <- function(info){
  sta_index_start <- grep("## Statistics", info, fixed = TRUE) + 1
  sta_index_end <- grep("## error", info, fixed = TRUE) - 1
  info_list <- list()
  for(id in seq_along(sta_index_start)){
    info_list[[id]]  <- info[sta_index_start[id]:sta_index_end[id]]
  }
  info_list <- do.call(c, info_list)

  out <- mapply(
    FUN = process_log,
    start = which(info_list %in% "{"),
    end = which(info_list %in% "}"),
    MoreArgs = list(txt = info_list),
    SIMPLIFY = FALSE)
  out <- Filter(is.data.frame, out)
  out$stringsAsFactors <- FALSE
  do.call(rbind, out)
}

process_data <- function(input = character(), size_in = numeric(),
                         path_out = character(),
                         size_out = numeric(),
                         percent = numeric(),
                         algorithm = character()){
  data.frame(input = input, size_in = size_in,
             path_out = path_out,
             size_out = size_out,
             percent = percent,
             algorithm = algorithm,
             stringsAsFactors = FALSE
  )
}


process_log <- function(start, end, txt){
  txt <- txt[start:end]
  algorithm <- extract_file(txt, "[ ]*algorithm:")
  input <- extract_file(txt, "[ ]*input:")
  path_out_new <- extract_file(txt, "[ ]*path_out_new:")
  size_in <- extract_num(txt, "[ ]*size_in:")
  size_output <- extract_num(txt, "[ ]*size_output:")
  percent <- extract_num(txt, "[ ]*percent:")
  process_data(input = input, size_in = size_in,
             path_out = path_out_new,
             size_out = size_output,
             percent = percent,
             algorithm = algorithm)
}
