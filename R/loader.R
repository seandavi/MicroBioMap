.getCompendiumData <- function(version, bfc) {
    print(paste('Retrieving compendium version',version))
    url <- .data_url[[version]]
    rpath <- bfcrpath(bfc, url)
    data.table::fread(rpath)
}

.getCompendiumColdata <- function(version, bfc) {
    url <- .coldata_url[[version]]
    rpath <- bfcrpath(bfc, url)
    sampdat <- as.data.frame(data.table::fread(rpath))
    rownames(sampdat) <- paste(sampdat[[2]], sampdat[[3]], sep = "_")
    sampdat
}

#' load all compendium data into a TreeSummarizedExperiment
#'
#' @param bfc BiocFileCache object to use
#'
#' @returns a `TreeSummarizedExperiment`
#'
#' @importFrom data.table fread
#' @importClassesFrom Matrix TsparseMatrix
#' @import TreeSummarizedExperiment
#' @import R.utils
#' @import ape
#' @importFrom BiocFileCache BiocFileCache bfcrpath
#'
#' @export
#'
#' @examples
#' cpd <- getCompendium()
#'
#' dim(cpd)
#' cpd
#' assayNames(cpd)
#' head(colData(cpd))
#'

getCompendium <- function(version='1.0.1', bfc = BiocFileCache::BiocFileCache()) {
    dat <-.getCompendiumData(version, bfc)
    coldat <- .getCompendiumColdata(version, bfc)

    sampnames <- dat[[2]]

    coldat <- coldat[match(sampnames, rownames(coldat)), ]

    taxa <- colnames(dat)[3:ncol(dat)]
    requireNamespace("Matrix")
    # mat = as(as.matrix(dat[,3:ncol(dat)]), 'TsparseMatrix')
    mat <- as.matrix(dat[, 3:ncol(dat)])
    rownames(mat) <- sampnames
    colnames(mat) <- taxa
    sampinfo <- do.call(rbind, strsplit(sampnames, "_"))
    colnames(sampinfo) <- c("project", "sample")
    coldata <- data.frame(sampinfo)
    rownames(coldata) <- sampnames
    splittaxa <- do.call(rbind, lapply(
        strsplit(taxa, "\\."),
        function(x) {
            c(x, rep(NA, 8 - length(x)))
        }
    ))
    colnames(splittaxa) <- c(
        "kingdom",
        "phylum",
        "class",
        "order",
        "family",
        "genus",
        "species",
        "strain"
    )
    rowdata <- data.frame(splittaxa)
    rownames(rowdata) <- taxa
    td <- TreeSummarizedExperiment::TreeSummarizedExperiment(
        colData = coldat,
        rowData = rowdata,
        assays = list(counts = t(mat))
    )
    td
}

taxonname2edgelist <- function(taxon) {
    v <- strsplit(taxon, "\\.")[[1]]
    v <- v[!v == "NA"]
    if (length(v) > 1) {
        lv <- length(v)
        df <- data.frame(from = v[seq_len(lv - 1)], to = v[1+seq_len(lv-1)])
    } else {
        df <- data.frame()
    }
    df
}

taxa2edgelist <- function(taxa) {
    taxa_edgelist <- lapply(taxa, taxonname2edgelist)
    df <- unique(do.call(rbind, taxa_edgelist))
    return(df)
    unique_names <- unique(c(df$from, df$to))
    l <- seq_along(unique_names)
    names(l) <- unique_names
    parents <- l[df$from]
    nodes <- l[df$to]
    df$parent <- parents
    df$node <- nodes
    df$label <- df$to
    df
}

taxa2phylo <- function(taxa) {
    edgelist <- taxa2edgelist(taxa)
    edgelist <- as.matrix(edgelist)

    edgelist <- edgelist[!is.na(edgelist[, 1]) & !is.na(edgelist[, 2]), ]

    from <- edgelist[, 1]
    to <- edgelist[, 2]
    ids <- unique(c(edgelist[, 1], edgelist[, 2]))

    tip.label <- setdiff(ids, from)
    node.label <- unique(from)

    # make a map from taxonomy ID to internal 1:n ids
    idmap <- seq_along(c(tip.label, node.label))
    names(idmap) <- c(tip.label, node.label)

    # make a phylo object
    tree <- list(
        edge       = matrix(c(idmap[as.character(from)], idmap[as.character(to)]), ncol = 2),
        tip.label  = unname(tip.label),
        # node.label = unname(node.label),
        Nnode      = length(node.label)
    )
    class(tree) <- "phylo"

    tree
}
