.onLoad <- function(libname, pkgname) {
  ### system.file is a devtools shim that works both during package dev and once installed by end user
  ### it will provide the actual path
  reticulate::py_run_file(system.file("python/getContractValue.py",package="Tutils"))
}
