
setGeneric("cutting",  function(h,  ...){print(NULL)})

##################################

#' @export
#' @title class to represent a tree for nice hierarchy
#' @slot node node id
#' @slot pnode parent node id
#' @slot lab doc code
#' @slot plab parent doc code
#' @slot text nice textual info
#' @slot drugnames common names for drugs where known
#' @slot lev level of each node (root node is 0)
#' @slot Nnode number of nodes in the tree excluding root node
#' @slot Nlev number of levels in the tree excluding root node
setClass("nicetree",
         representation( node="numeric",
                         pnode="numeric",
                         lab="character",
                         text="character",
                         drugnames="character",
                         plab="character",
                         lev="numeric",
                         Nnode="numeric",
                         Nlev="numeric"
         )
)


setOldClass("plot")

#############################################################
## use plotly
## main benefit is hovering!

#' @export
#' @title plot the tree as a network graph
#' @param x an object of class nicetree
#' @param circle whether or not to arrange graph over a circle
#' @param add_text whether or not to label with textual info
#' @param leaf_label whether or not leaf nodes will be labelled
#' @param stem_label whether or not stem nodes will be labelled
#' @param include_names whether or not to include common names in text
#' @param text_size annotation text size
#' @param text_angle annotation text angle
#' @param width graph width
#' @param height graph height
#' @param hover_msg whether to remind the user that they can hover over nodes
#' @details
#' @aliases plot
#' @examples
#'
setMethod("plot",
          signature=c("nicetree"),

          function(x, circle=FALSE, add_text=FALSE,
                   leaf_label=FALSE, stem_label=TRUE,
                   include_names=FALSE,
                   text_size=10, text_angle=45,
                   width=500, height=500, hover_msg=TRUE) {

            if (hover_msg) message("Remember you can hover over nodes to see text")

            if (add_text==FALSE) {
              labvec <- x@lab
            } else {
              labvec <- x@text
            }

            if (leaf_label==FALSE) {
              labvec[x@lev==max(unique(x@lev))] <- ""
            }
            if (stem_label==FALSE) {
              labvec[x@lev!=max(unique(x@lev))] <- ""
            }

            nV <- length(x@node) #, na.rm=TRUE) #number of vertices
            #        V <- 1:nV #vertices

            A <- matrix(0,nrow=nV, ncol=nV)
            for (ii in 1:nV) {
              A[ii, which(x@node==x@pnode[ii])] <- 1
            }
            #create adjacency matrix

            G <- igraph::graph.adjacency(A)

            if (circle) {
              #            L <- igraph::layout_as_tree(G,circular=TRUE)
              L <- graphlayouts::layout_with_centrality(G, cent=4-x@lev)  #, cent=c(8.2,8,6,5,3,1)[x@lev+1])
              Xn <- L[,1]
              Yn <- L[,2]

            } else {
              L <- igraph::layout_with_sugiyama(G, layers=x@lev) #, layers=c(1,2,5,7,8,9)[x@lev+1])
              Xn <- L$layout[,1]
              Yn <- L$layout[,2]
            }

            es <- as.data.frame(igraph::get.edgelist(G))

            if (nrow(es)==0) stop("Nothing to plot")

            edge_shapes <- list()
            for(i in 1:nrow(es)) {
              v0 <- es[i,]$V1
              v1 <- es[i,]$V2

              edge_shape = list(
                type = "line",
                opacity=0.25,
                line = list(color = "#030303", width = 0.3),
                x0 = Xn[v0],
                y0 = Yn[v0],
                x1 = Xn[v1],
                y1 = Yn[v1]
              )

              edge_shapes[[i]] <- edge_shape
            }


            network <- plotly::plot_ly(x = ~Xn, y = ~Yn,
                                       width = width,
                                       height = height,
                                       type="scatter",
                                       mode = "markers+text",
                                       hovertext = if (include_names==FALSE) {
                                         paste(x@lab, x@text, sep=":")
                                       } else {
                                         paste0(x@lab, ": ", x@text,
                                                if_else(is.na(x@drugnames), "", paste0(" [",x@drugnames,"]"))
                                         )
                                       },

                                       hoverinfo = "text"
            )

            axis <- list(title = "", showgrid = FALSE, showticklabels = FALSE, zeroline = FALSE)



            fig <- plotly::layout(
              autosize = F,
              network,
              shapes = edge_shapes,
              xaxis=axis,
              yaxis=axis,
              margin=list(
                l = 0,
                r = 0,
                b = 0,
                t = 0
              )
              #font=list(size=text_size)
            ) |>

              #        plotly::add_text(text=x@lab,
              #                 textfont = list(size = 8, textposition=2)
              #                 ) |>

              plotly::add_annotations(
                text=labvec,
                showarrow=leaf_label & stem_label,
                arrowsize=0.3,
                arrowwidth=0.1,
                ax=-10,
                ay=-10,
                font=list(size=text_size),
                #showarrow=FALSE,
                textangle=text_angle)
            # if wish to rotate the bottom nodes need to do this


            fig
          }
)


################################################



setMethod("cutting",
          signature("nicetree"),

          function (h, nodelabel=NULL) {

            if (is.null(nodelabel)) stop("No cutting node supplied")
            if (length(nodelabel)>1) stop("When cutting supply just one node")
            #            if (intree(h,nodelabel)==FALSE) stop("This cutting node is not in the tree")

            dataf <- as(h, "data.frame")

            ref_node <- filter(dataf, lab==nodelabel)$node
            child_nodes <- filter(dataf, pnode==ref_node)$node
            dataf <-  filter(dataf, pnode %in% child_nodes | pnode==ref_node | node==ref_node)
            # ... because some of these nodes will have parents (child_nodes) whose own parent is *not* the ref_node

            dataf$pnode[dataf$lab==nodelabel] <- 0 #dataf$node[dataf$lab=="0"]

            h2 <- new("nicetree",
                      pnode = dataf$pnode,
                      node = dataf$node,
                      lab = dataf$lab,
                      text = dataf$text,
                      lev = dataf$lev,
                      Nnode = length(dataf$node)-1
            )

            return(h2)
            }
)

#####################################

setAs("nicetree", "data.frame",
      function(from) {

        df <- data.frame(lab=from@lab,
                         text=from@text,
                         node=from@node,
                         lev=from@lev,
                         pnode=from@pnode)


        #        df <- df |>
        #          dplyr::relocate(pnode, .after=pATC) |>
        #          dplyr::relocate(plen, .after=plev)

        df <- unique(df)

        return(df)
      })
