# List S4 classes defined in a package
.s4classes <- function(pkg){
    o <- getClasses(where=asNamespace(pkg))
    o
}

# Count subclasses WITHIN package
.subclasses <- function(x, pkg){
    o <- getClassDef(Class=x, package=pkg)
    o <- length(o@subclasses)
    o
}

# Count superclasses ACROSS packages
.superclasses <- function(x, pkg){
    o <- getClassDef(Class=x, package=pkg)
    o <- length(o@contains)
    o
}

# Determine if class i virtual
.virtual <- function(x, pkg){
    isVirtualClass(Class=x, where=asNamespace(pkg))
}

#' Helper: Get S4-classes defined in a set of packages
#'
#' This function uses getClasses to get S4-classes from packages and
#' isVirtualClass and isClassUnion to describe them.
#'
#' @param packages character: R packages
#'
#' @returns A data.frame (Superclasses is across packages and Subclasses is
#'   within a package)
#' @export
#'
#' @examples
#' # Single package
#' getS4Classes("S4Vectors")
#'
#' # Multiple packages
#' getS4Classes(c("IRanges", "GenomicRanges"))
getS4Classes <- function(packages){
    checkmate::assertCharacter(packages,
                               any.missing = FALSE,
                               min.len = 1,
                               unique = TRUE)
    BiocBaseUtils::checkInstalled(packages)

    # Get classes
    o <- lapply(packages, .s4classes)

    # Format as data.frame
    names(o) <- packages
    o <- utils::stack(o)
    colnames(o) <- c("S4Class", "Package")
    o$Package <- as.character(o$Package)

    # Describe classes
    o$Virtual <- mapply(FUN=.virtual, o$S4Class, o$Package)
    o$Union <- vapply(o$S4Class, FUN = isClassUnion, FUN.VALUE = logical(1))
    o$Superclasses <- mapply(FUN=.superclasses, o$S4Class, o$Package)
    o$Subclasses <- mapply(FUN=.subclasses, o$S4Class, o$Package)

    # Return
    o
}


