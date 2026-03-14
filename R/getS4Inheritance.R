.directinheritance <- function(x, pkg){
    # Get info
    o <- getClassDef(Class=x, package=pkg)

    # Find direct extensions
    o <- vapply(o@contains, function(i) i@distance, FUN.VALUE = double(1))
    o <- o[o == 1]

    # Format as edges
    if(length(o) > 0){
        o <- data.frame(Superclass=x, Subclass=names(o))
    }else{
        o <- data.frame(Superclass=character(0), Subclass=double(0))
    }

    # Return
    o
}

#' Helper: Get S4 inheritance for a set of S4 classes
#'
#' This function uses getClassDef to extract inheritance information between S4
#' classes.
#'
#' @param S4Classes character: S4-classes
#' @param packages character: R-package of each S4-class
#'
#' @returns data.frame
#' @export
#'
#' @examples
#' # Single package
#' one_package <- getS4Classes("GenomicRanges")
#' getS4Inheritance(S4Classes = one_package$S4Class,
#'                   packages = one_package$Package)
#'
#' # Several packages
#' two_packages <- getS4Classes(c("IRanges", "GenomicRanges"))
#' getS4Inheritance(S4Classes = two_packages$S4Class,
#'                   packages = two_packages$Package)
getS4Inheritance <- function(S4Classes, packages){
    checkmate::assertCharacter(S4Classes, any.missing = FALSE)
    checkmate::assertCharacter(packages, any.missing = FALSE,
                               len=length(S4Classes))

    # Extract edges
    o <- mapply(FUN=.directinheritance, S4Classes, packages, SIMPLIFY = FALSE)

    # Collapse
    o <- do.call(rbind, o)
    rownames(o) <- NULL

    # Return
    o
}
