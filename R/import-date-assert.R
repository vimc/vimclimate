# NOTE: temporary home for these functions before they move to {reside.utils}
assert_date <- function(
  x,
  name = deparse(substitute(x)),
  arg = name,
  call = parent.frame()
) {
  if (!lubridate::is.Date(x)) {
    cli::cli_abort(
      "Expected '{name}' to be a Date",
      call = call,
      arg = arg
    )
  }
  invisible(x)
}

assert_scalar_date <- function(
  x,
  name = deparse(substitute(x)),
  allow_null = FALSE,
  arg = name,
  call = parent.frame()
) {
  if (allow_null && is.null(x)) {
    return(invisible(x))
  }
  assert_scalar(x, name, arg = arg, call = call)
  assert_date(x, name, arg = arg, call = call)
  assert_nonmissing(x, name, arg = arg, call = call)
}
