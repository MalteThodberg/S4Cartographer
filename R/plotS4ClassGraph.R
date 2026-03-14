#' Plot S4 classes and inheritance as a graph
#'
#' This function plots all S4-classes from one or more R-packages as graph,
#' where each node is an S4 class and directed edges represents inheritance.
#' Virtual classes are indicated via node shapes. The graph stops at S4 defined
#' outside the set or R-packages in question.
#'
#' @param packages character: R-packages
#' @param plot logical: Whether to plot (TRUE) or just return the underlying
#'   plot data (FALSE)
#'
#' @returns ggplot if plot=TRUE or list if plot=FALSE
#' @importFrom rlang .data
#' @export
#'
#' @examples
#' # Single package
#' plotS4ClassGraph("GenomicRanges")
#'
#' # Several packages:
#' plotS4ClassGraph(c("IRanges", "GenomicRanges"))
plotS4ClassGraph <- function(packages, plot=TRUE){
    checkmate::assertCharacter(packages, any.missing = FALSE,
                               min.len = 1,
                               unique = TRUE)
    checkmate::assertFlag(plot)

    # Classes as nodes
    s4_nodes <- getS4Classes(packages)

    # Direct inheritance as edges
    s4_edges <- getS4Inheritance(S4Classes = s4_nodes$S4Class,
                                 packages = s4_nodes$Package)

    # Annotate missing nodes
    s4_orphans <- data.frame(S4Class=setdiff(s4_edges$Subclass,
                                             s4_nodes$S4Class),
                             Package=NA,
                             Virtual=NA,
                             Union=NA,
                             Superclasses=NA,
                             Subclasses=NA)

    s4_nodes <- rbind(s4_nodes, s4_orphans)

    # Format as factor to retain ordering
    s4_nodes$Package <- factor(s4_nodes$Package, levels=packages)

    # Remove duplicated nodes
    if(anyDuplicated(s4_nodes$S4Class)){
        warning("Duplicated S4Class definitions: Removing these according to input ordering of packages")

        # Find duplicated
        tmp <- duplicated(s4_nodes$S4Class)
        s4_duplicated <- s4_nodes[tmp,]

        # Remove from nodes
        s4_nodes <- s4_nodes[!tmp,]
    }

    if(isFALSE(plot)){
        o <- list(nodes=s4_nodes, edges=s4_edges)
    }else{
        # Format as igraph
        o <- igraph::graph_from_data_frame(s4_edges, vertices=s4_nodes)
        o <- tidygraph::as_tbl_graph(o)

        # Plot
        o <- ggraph::ggraph(o, layout = 'fr') +
            ggraph::geom_edge_link(arrow = ggplot2::arrow(length = ggplot2::unit(1, 'mm')),
                                   end_cap = ggraph::circle(2, 'mm'),
                                   color="grey75",
                                   alpha=0.75) +
            ggraph::geom_node_point(ggplot2::aes(color=.data$Package,
                                                 shape=.data$Virtual),
                                    size=2) +
            ggraph::geom_node_text(ggplot2::aes(label=.data$name),
                                   repel=TRUE,
                                   size=2,
                                   max.overlaps=Inf) +
            ggraph::theme_graph(foreground = 'steelblue',
                                fg_text_colour = 'white',
                                base_family = "sans") +
            ggplot2::scale_shape_manual(values=c(19,1),
                                        na.value=13) +
            ggthemes::scale_color_colorblind(na.value="grey")
    }

    # Return
    o
}
