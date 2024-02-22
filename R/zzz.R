.onAttach <- function(libname, pkgname) {
 packageStartupMessage("This is version ", packageVersion(pkgname),
                       " of ", pkgname,". \n Please report any issues at https://github.com/hendr1km/dropout/issues")
}
