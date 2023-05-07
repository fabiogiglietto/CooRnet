# zzz.R

.onLoad <- function(libname, pkgname) {
  reticulate::use_virtualenv("coornet_env", required = TRUE)
}
