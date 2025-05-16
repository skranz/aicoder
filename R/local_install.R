# direct_local_install.R
# Build + install a local package and capture the full console log
# Uses only base R + stringi. Suitable for run_with_log() or standalone.

#' Build and install a local source package and collect a complete log
#'
#' @param pkg_dir       Path to the package source directory.
#' @param build_args    Extra flags for \code{R CMD build}.  Vector, default empty.
#' @param install_args  Extra flags for \code{R CMD INSTALL}. Vector, default empty.
#' @param env           Named character vector of environment variables for both calls.
#' @return An object of class \code{direct_install_result} with elements
#'   \itemize{
#'     \item \strong{log}            – single character string, concatenated stdout+stderr
#'     \item \strong{has_error}      – \code{TRUE} if either command returned non-zero
#'     \item \strong{build_status}   – exit status from \code{R CMD build}
#'     \item \strong{install_status} – exit status from \code{R CMD INSTALL}
#'     \item \strong{tarball}        – path to the built \code{*.tar.gz}
#'   }
#' @examples
#' res = direct_local_install("mypkg")
#' cat(res$log)
#' if (res$has_error) stop("package failed to build / install")
local_install_with_log = function(pkg_dir,
                                build_args = character(),
                                install_args = character(),
                                env = character()) {

  if (!dir.exists(pkg_dir)) {
    stop("Directory not found: ", pkg_dir)
  }

  r_bin = file.path(R.home("bin"), "R")          # full path to R executable
  pkg_dir_norm = normalizePath(pkg_dir)

  # ---- build ----
  build_cmd = c("CMD", "build", build_args, shQuote(pkg_dir_norm))
  build_out = system2(r_bin,
                      args = build_cmd,
                      stdout = TRUE,
                      stderr = TRUE,
                      env = env)
  build_status = attr(build_out, "status")
  if (is.null(build_status)) build_status = 0L

  # Try to discover the tarball mentioned in the build log
  tarball_candidates = stringi::stri_extract_first_regex(build_out,
                                                         "[^[:space:]]+\\.tar\\.gz")
  tarball_candidates = tarball_candidates[!is.na(tarball_candidates)]
  tarball_path = if (length(tarball_candidates)) {
    normalizePath(tarball_candidates[length(tarball_candidates)],
                  mustWork = FALSE)
  } else {
    NA_character_
  }

  # Fallback: newest *.tar.gz in current working dir
  if (!is.na(tarball_path) && !file.exists(tarball_path)) {
    tgz_files = dir(pattern = "\\.tar\\.gz$", full.names = TRUE)
    if (length(tgz_files)) {
      o = order(file.info(tgz_files)$mtime, decreasing = TRUE)
      tarball_path = normalizePath(tgz_files[o[1]])
    }
  }

  # ---- install ----
  install_cmd = c("CMD", "INSTALL", install_args, shQuote(tarball_path))
  install_out = system2(r_bin,
                        args = install_cmd,
                        stdout = TRUE,
                        stderr = TRUE,
                        env = env)
  install_status = attr(install_out, "status")
  if (is.null(install_status)) install_status = 0L

  # ---- assemble result ----
  full_log = stringi::stri_flatten(c(build_out, install_out), "\n")
  has_error = build_status != 0L || install_status != 0L

  structure(list(log = full_log,
                 has_error = has_error,
                 build_status = build_status,
                 install_status = install_status,
                 tarball = tarball_path),
            class = "direct_install_result")
}

# Pretty printer -----------------------------------------------------------
#' @export
print.direct_install_result = function(x, ...) {
  cat(x$log, sep = "\n")
  invisible(x)
}
