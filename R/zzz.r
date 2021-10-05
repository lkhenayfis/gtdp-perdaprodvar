.onLoad <- function(libname, pkgname) {
    temp <- tempdir()
    Sys.setenv(".perdaprodvarTEMP" = temp)
}