example = function() {
  file = "C:/libraries/aicoder/stata2r/aicoder_work/tests/do1/test_do1.R"
  res = source_and_capture(file)
  res$has_error
  write_utf8(res$log,"C:/libraries/aicoder/stata2r/aicoder_work/tests/do1/log.txt")
}

source_and_capture <- function(r_file_path) {
  # Capture all output, including errors
  result <- capture.output({
    # Use try to catch errors but continue execution
    error_status <- try(source(r_file_path, echo = TRUE), silent = TRUE)
  }, type = "output")

  # Return TRUE if no error, FALSE if there was an error
  has_error = inherits(error_status, "try-error")
  if (has_error) {
    result = c(result, as.character(error_status))
  }
  list(log = merge_lines(result), has_error=has_error)
}


# run_with_log.R
# Utility to execute R code and capture console‑like output as a string.
# Relies on the CRAN package "evaluate" for robust capture of source, output,
# messages, warnings, and errors. Uses only standard ASCII characters and
# the "stringi" package for string handling, per user coding preferences.

#' Run R code and return a structured log of its execution
#'
#' Instead of writing to a file, this function returns a list (class
#' `run_with_log_result`) containing:
#'   * log        – A single character string with the console‑style log
#'   * error      – `NULL` or the error condition (if execution stopped)
#'   * n_warnings – Integer count of warnings emitted
#'   * value      – The value of the last evaluated expression (invisible)
#'
#' @param code      An expression wrapped in braces `{}` to evaluate.
#' @param env       Environment in which to evaluate the code.
#'                  Defaults to the calling environment (`parent.frame()`).
#' @param code_str  A character string containing R code.
#' @param code_file Path to a file containing R code.
#' @return An object of class `run_with_log_result` (see above).
#' @examples
#' # Code block
#' out = run_with_log({
#'   library(dplyr)
#'   x = 5
#'   x + 2
#' })
#' cat(out$log)
#'
#' # String
#' run_with_log(code_str = "x = 1:3; sum(x)")
#'
#' # File
#' writeLines(c("y = 10", "y^2"), "snippet.R")
#' run_with_log(code_file = "snippet.R")
#'
#' # Custom environment inside another function
#' f = function() {
#'   e = new.env(parent = baseenv())
#'   res = run_with_log({ z = 7; z * 2 }, env = e)
#'   res$value  # 14
#' }
#' f()
run_with_log = function(code = NULL,
                        env = parent.frame(),
                        code_str = NULL,
                        code_file = NULL) {
  # Validate exclusive code source
  src_flags = c(!is.null(code), !is.null(code_str), !is.null(code_file))
  if (sum(src_flags) != 1) {
    stop("Provide exactly one of 'code', 'code_str', or 'code_file'.")
  }
  if (!is.environment(env)) {
    stop("'env' must be an environment.")
  }
  if (!requireNamespace("evaluate", quietly = TRUE)) {
    stop("Package 'evaluate' is required but not installed.")
  }

  # Convert the chosen source to a single character string of code
  if (!is.null(code)) {
    #expr = substitute(code)
    deparsed = deparse(code)
    if (length(deparsed) >= 2 &&
        grepl("^\\s*\\{\\s*$", deparsed[1]) &&
        grepl("^\\s*\\}\\s*$", deparsed[length(deparsed)])) {       deparsed = deparsed[-c(1, length(deparsed))]
    }
    code_text = paste(deparsed, collapse = "\n")
    #code_text = paste(deparse(expr), collapse = "\n")

  } else if (!is.null(code_str)) {
    code_text = code_str
  } else {
    # code_file provided
    if (!file.exists(code_file)) {
      stop("File not found: ", code_file)
    }
    code_lines = readLines(code_file, warn = FALSE)
    code_text = stringi::stri_flatten(code_lines, "\n")
  }
  restore.point("shdjhsfhs")

  #res_list = repboxEvaluate::repbox_evaluate(code_text, stop_on_error = 1L,keep_message = TRUE, keep_warning = TRUE, log_echo=FALSE)

  #Evaluate and capture via the 'evaluate' package
  res_list = evaluate::evaluate(code_text,
                                envir = env,
                                stop_on_error = 0,
                                keep_warning = TRUE,
                                log_warning = TRUE,
                                log_echo = FALSE)

  # Prepare log assembly helpers
  as_console = function(val) {
    capture.output(print(val))
  }

  lines = character()
  last_value = NULL
  n_warn = 0L
  err_obj = NULL

  for (item in res_list) {
    if (inherits(item, "source")) {
      lines = c(lines, item$src)
    } else if (inherits(item, "message")) {
      lines = c(lines, conditionMessage(item))
    } else if (inherits(item, "warning")) {
      n_warn = n_warn + 1L
      lines = c(lines, paste0("Warning: ", conditionMessage(item)))
    } else if (inherits(item, "error")) {
      err_obj = item
      lines = c(lines, paste0("Error: ", conditionMessage(item)))
    } else if (inherits(item, "recordedplot")) {
      # Ignore graphics (could be extended to save)
    } else {
      # Value returned by an expression
      last_value = item
      lines = c(lines, as_console(item))
    }
  }

  log_string = stringi::stri_flatten(lines, "\n")

  structure(list(log = log_string,
                 has_error = !is.null(err_obj),
                 error = err_obj,
                 n_warnings = n_warn,
                 value = last_value),
            class = "run_with_log_result")
}

#' @export
print.run_with_log_result = function(x, ...) {
  cat(x$log, sep = "\n")
  invisible(x)
}


