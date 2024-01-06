
## fall back data retrieval in case ExperimentHub is down
.sesameDataGet_fallback <- function(title) {
    u1 <- sprintf('%s/%s.rda', alt_base, title)
    sesameDataGet_assignEnv(title, get(load(u1)))
}

.sesameDataGet <- function(title) {
    u1 <- sprintf('%s/%s.rda', alt_base, title)
    sesameDataGet_assignEnv(title, get(load(u1)))
}

#' Get SeSAMe data
#'
#' @param title title of the data
#' @param verbose whether to output ExperimentHub message
#' @return data object
#' @import ExperimentHub
#' @import AnnotationHub
#' @importFrom stringr str_replace
#' @examples
#'
#' sesameDataCacheExample()
#' EPIC.1.SigDF <- sesameDataGet('EPIC.1.SigDF')
#' @export
sesameDataGet <- function(title, verbose = FALSE) {
    u1 <- sprintf('%s/%s.rda', alt_base, title)
    sesameDataGet_assignEnv(title, get(load(u1)))
}

#' List all SeSAMe data
#'
#' @param filter keyword to filter title, optional
#' @param full whether to display all columns
#' @return all titles from SeSAMe Data
#' @examples
#' sesameDataList("KYCG")
#' @export
sesameDataList <- function(filter = NULL, full = FALSE) {
    df <- df_master
    if (!full) {
        df <- df[,c("EHID","Title")]
    }
    if (!is.null(filter)) {
        df <- df[grep(filter, df$Title),]
    }
    df
}

#' Whether sesameData has
#'
#' @param data_titles data titles to check
#' @return a boolean vector the same length as data_titles
#' @examples
#' sesameDataHas(c("EPIC.address","EPIC.address.Nonexist"))
#' @export
sesameDataHas <- function(data_titles) {
    data_titles %in% sesameDataList()$Title
}

