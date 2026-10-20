
#' @export
#' @title constructor for atctree objects
#' @param schema a conceptual subset of the ATC hierarchy, or all of it ('full'), or something bespoke ('none')
#' @param whichlevs subset of levels to use from integers 1:5
#' @details
#' either schema or whichlevs should be specified; (e.g. `schema=full` corresponds to `whichlevs=1:5`)
#' @returns an object of class atctree
#' @examples
#' h <- atctree(schema="anatomical")
#' # tree will contain levels 1 and 5
#' h <- atctree(whichlevs=c(1,2,5))
#' # bespoke structure
#'
#'
nicetree <-   function() {


# ... here make dataf ...


  .Object <- methods::new("atctree",
                          pnode = dataf$pnode,
                          node = dataf$node,
                          lab = dataf$ATC,
                          text = dataf$text,
                          drugnames=dataf$name,
                          plab = dataf$pATC,
                          lev = dataf$lev,
                          Nnode = length(dataf$node)-1    # don't count the root node
  )
  .Object@Nlev <- nlevs(.Object)

  return(.Object)
}
