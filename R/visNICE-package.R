#' @keywords internal
"_PACKAGE"

utils::globalVariables(names=c("dataf","L1","L2","L3","node_L1","node_L2","pnode_L2","lab","lab2","node","pnode","lev","doctype"))
# stop some R CMD notes

ignore_unused_imports <- function() {
  igraph::add.edges()
  tidyr::all_of()
  graphlayouts::annotate_circle()
  plotly::add_annotations()
}

#' @importFrom dplyr mutate
#' @importFrom dplyr filter
#' @importFrom dplyr bind_rows
#' @importFrom dplyr arrange
#' @importFrom dplyr rename
#' @importFrom dplyr select
#' @importFrom dplyr left_join
#' @importFrom dplyr inner_join
#' @importFrom dplyr if_else
#' @importFrom dplyr case_when
#' @importFrom dplyr last
#' @importFrom methods show
#' @importFrom methods new
#' @importFrom utils data
#' @importFrom rlang .data
#' @importFrom methods as
#' @importFrom graphics text

NULL
