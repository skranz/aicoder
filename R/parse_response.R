example = function() {
  text = readLines("C:/libraries/aicoder/ai_resp.txt")
  resp = aic_parse_response(text)
}

aic_parse_response <- function(aic, text) {
  # Load required package
  if (!requireNamespace("stringi", quietly = TRUE)) {
    stop("Package 'stringi' is required. Please install it with install.packages('stringi')")
  }

  text = paste0(text, collapse="\n")

  change_df = rem_df = NULL

  # Initialize output list
  result <- list(
    change_files = list(),
    remove_files = character()
  )

  # Extract CHANGE_FILE blocks
  change_pattern <- "!CHANGE_FILE\\s+([^\\n]+)\\n(.*?)!END_CHANGE_FILE\\s+\\1"
  change_matches <- stringi::stri_match_all_regex(text, change_pattern, dotall = TRUE,,omit_no_match = TRUE)[[1]]

  if (nrow(change_matches) > 0) {
    file_paths <- change_matches[, 2]
    file_contents <- change_matches[, 3]

    change_df = data.frame(type="change", file = file_paths, content=file_contents, ext = tools::file_ext(file_paths))
    change_df$content = sapply(seq_len(NROW(change_df)), function(i) {
      aic_correct_content(change_df$content[i], change_df$ext[i])
    })
  }

  # Extract REMOVE_FILE instructions
  remove_pattern <- "!REMOVE_FILE\\s+([^\\n]+)"
  remove_matches <- stringi::stri_match_all_regex(text, remove_pattern,omit_no_match = TRUE)[[1]]

  if (nrow(remove_matches) > 0) {
    rem_df = data.frame(type="remove", file = remove_matches[, 2], content="", ext = tools::file_ext(file_paths))
  }
  aic$resp_text = text
  aic$changes = bind_rows(change_df, rem_df)
  aic
}

aic_correct_content = function(content, ext) {
  restore.point("aic_correct_content")
  ext = tolower(ext)
  if (ext %in% c("md","rmd")) return(content)
  content = sep_lines(content)
  omit = startsWith(content,"```")
  content = content[!omit]
  merge_lines(content)
}
