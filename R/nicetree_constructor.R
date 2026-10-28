
#' @export
#' @title constructor for nicetree objects
#' @param schema a conceptual subset of the ATC hierarchy, or all of it ('full'), or something bespoke ('none')
#' @param whichlevs subset of levels to use from integers 1:5
#' @details
#' either schema or whichlevs should be specified; (e.g. `schema=full` corresponds to `whichlevs=1:5`)
#' @returns an object of class nicetree
#' @examples
#' h <- nicetree()
#' # tree will contain levels 1 and 5

nicetree <-   function() {

  dataf <- nicedf

  dataf <- distinct(nicedf[,1:2])

  #### set up first and second level nodes:
  dataf$node_L1 <- as.numeric(dataf$L1)
  dataf$node_L2 <- as.numeric(dataf$L2)+ max(dataf$node_L1)
  dataf$pnode_L2 <- dataf$node_L1



  dataf_L1 <- select(dataf, L1, node_L1) |>
    rename(text=L1, node=node_L1) |>
    mutate(pnode=0, lev=1) |> #root note is parent of level 1
    distinct()

  dataf_L2 <- select(dataf, L2, node_L2, pnode_L2) |>
    rename(text=L2, node=node_L2, pnode=pnode_L2) |>
    mutate(lev=2)



  #### set up third level nodes:

  tmp <- select(nicedf, L2, L3) |>
    rename(text=L2)

  dataf_L3 <-  left_join(tmp, select(dataf_L2, text, node), by="text") |>
    select(-text) |>
    rename(pnode=node, text=L3) |>
    mutate(lev=3) |>
    mutate(node=as.numeric(factor(text)) + max(dataf_L2$node))

  ####

  dataf <- bind_rows(dataf_L1, dataf_L2, dataf_L3)

  dataf <- rbind(data.frame(text="root", node=0, pnode=NA, lev=0),      dataf )
  # add root node

  ##############
  ####  create lab and doctype:

  #round brackets:
  xtractr <- function(x) {
                          substr(x,
                               start=last(gregexpr("\\([[:upper:]]",x)[[1]])+1,
                               stop=last(gregexpr("[[:digit:]]\\)",x)[[1]])
                          )
  }

  #square brackets:
  xtracts <- function(x) {
    substr(x,
           start=last(gregexpr("\\[[[:upper:]]",x)[[1]])+1,
           stop=last(gregexpr("[[:digit:]]\\]",x)[[1]])
    )
  }

  dataf <- dataf |>
    mutate(lab = map_chr(text, .f=xtractr)) |>
    mutate(lab2 = map_chr(text, .f=xtracts)) |>
    mutate(lab = if_else(nchar(lab)==0, lab2, lab)) |>
    select(-lab2)

  # remove the digits from lab to get doctype:
  dataf <- dataf |>
    mutate(doctype =
                case_when(
                      lev==3 ~  gsub("[[:digit:]]","",lab)
                    )
    )

  ## check:
  tmp <- dataf |>
    filter(lev==3 & doctype %in% c("ID","AMR","CG","CSG","HST","HTG","NG","PH","TA" )==FALSE)
  if (nrow(tmp)>0) {
    message("Some labels were incorrectly parsed")
    View(tmp)
    stop()
  }

  # abbreviate:
  dataf$lab <- with(dataf,
                    case_when(
                      lev==0 ~ "root",
                      lev==1 ~ str_trim(toupper(substr(text,1,7))),
                      lev==2 ~ (abbreviate(text,minlength=4)),
                      lev==3 ~ lab
                    )
  )
  ########

  .Object <- methods::new("nicetree",
                          pnode = dataf$pnode,
                          node = dataf$node,
                          lab = dataf$lab,
                          text = dataf$text,
                          lev = dataf$lev,
                          doctype = dataf$doctype,
                          Nnode = length(dataf$node)-1    # don't count the root node
  )
  #.Object@Nlev <- nlevs(.Object)

  return(.Object)
}
