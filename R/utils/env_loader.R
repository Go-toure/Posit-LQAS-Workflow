# ============================================================
# env_loader.R
# Tiny .env-style loader for the LQAS pipeline's R scripts.
#
# Mirrors the Python _env_loader.py pattern already used by
# im_workflow and the AFRO-SIA scope project: reads simple KEY=VALUE
# lines from config/secrets.env (git-ignored -- see
# config/secrets.env.example) and sets them via Sys.setenv(), WITHOUT
# overwriting a variable that is already set in the real environment
# (so a value set via `setx` / the OS environment always wins over the
# file, matching normal .env conventions).
# ============================================================

#' Load <base_dir>/config/secrets.env into the R session's environment
#' variables, if the file exists.
#' @param base_dir Project root directory (defaults to the current
#'   working directory, since every pipeline script in this project is
#'   run with cwd already set to the project root).
load_secrets_env <- function(base_dir = getwd()) {
  path <- file.path(base_dir, "config", "secrets.env")
  if (!file.exists(path)) {
    return(invisible(NULL))
  }

  lines <- readLines(path, warn = FALSE)
  for (line in lines) {
    line <- trimws(line)
    if (nchar(line) == 0 || startsWith(line, "#") || !grepl("=", line, fixed = TRUE)) {
      next
    }
    parts <- strsplit(line, "=", fixed = TRUE)[[1]]
    key <- trimws(parts[1])
    value <- trimws(paste(parts[-1], collapse = "="))
    if (nchar(key) == 0) {
      next
    }
    # Only set if not already set in the real environment (setdefault-style).
    if (identical(Sys.getenv(key), "")) {
      do.call(Sys.setenv, setNames(list(value), key))
    }
  }
  invisible(NULL)
}
