# load all dependencies, functions, data and application server and ui
pkgload::load_all(export_all = FALSE, helpers = FALSE, attach_testthat = FALSE)

# launch the app:
fabryka::fabryka()
