#' load data
#'
#' @param bfc BiocFileCache object to use
#' @param ... passe
#'
#'
#' @importFrom data.table fread
#' @importClassesFrom Matrix TsparseMatrix
#' @import TreeSummarizedExperiment
#' @import ape
#' @importFrom BiocFileCache BiocFileCache bfcrpath
#'
#' @export
#'
get_compendium <- function(bfc = BiocFileCache::BiocFileCache(), ...) {
  url = "https://zenodo.org/record/8186994/files/taxonomic_table.csv.gz"
  path = bfcrpath(bfc, url)
  dat = data.table::fread(path, ...)
  sampnames = dat[[2]]
  taxa = colnames(dat)[3:ncol(dat)]
  requireNamespace('Matrix')
  #mat = as(as.matrix(dat[,3:ncol(dat)]), 'TsparseMatrix')
  mat = as.matrix(dat[,3:ncol(dat)])
  rownames(mat) = sampnames
  colnames(mat) = taxa
  sampinfo = do.call(rbind,strsplit(sampnames,'_'))
  colnames(sampinfo) = c('project','sample')
  coldata = data.frame(sampinfo)
  rownames(coldata) = sampnames
  splittaxa = do.call(rbind,lapply(
    strsplit(taxa,'\\.'),
    function(x) {c(x, rep(NA,8-length(x)))})
    )
  colnames(splittaxa) = c(
    "kingdom",
    "phylum",
    "class",
    "order",
    "family",
    "genus",
    "species",
    "strain")
  rowdata = data.frame(splittaxa)
  rownames(rowdata) = taxa
  td = TreeSummarizedExperiment::TreeSummarizedExperiment(
    colData=coldata,
    rowData=rowdata,
    assays=list(counts=t(mat))
  )
  td
}

taxonname2edgelist <- function(taxon) {
  print(taxon)
  v = strsplit(taxon,'\\.')[[1]]
  v = v[!v=='NA']
  if(length(v)>1) {
    lv = length(v)
    df = data.frame(from=v[1:(lv-1)], to = v[2:lv])
  } else {
    df = data.frame()
  }
  df
}

taxa2edgelist <- function(taxa) {
  taxa_edgelist <- lapply(taxa,taxonname2edgelist)
  df = unique(do.call(rbind, taxa_edgelist))
  return(df)
  unique_names = unique(c(df$from,df$to))
  l = seq_along(unique_names)
  names(l) = unique_names
  parents = l[df$from]
  nodes = l[df$to]
  df$parent=parents
  df$node = nodes
  df$label = df$to
  df
}

taxa2phylo <- function(taxa) {
  edgelist = taxa2edgelist(taxa)
  edgelist = as.matrix(edgelist)

  edgelist = edgelist[!is.na(edgelist[,1]) & !is.na(edgelist[,2]),]

  from <- edgelist[,1]
  to <- edgelist[,2]
  ids <- unique(c(edgelist[,1], edgelist[,2]))

  tip.label <- setdiff(ids, from)
  node.label <- unique(from)

  # make a map from taxonomy ID to internal 1:n ids
  idmap <- 1:(length(tip.label) + length(node.label))
  names(idmap) <- c(tip.label, node.label)

  # make a phylo object
  tree <- list(
    edge       = matrix(c(idmap[as.character(from)], idmap[as.character(to)]), ncol=2),
    tip.label  = unname(tip.label),
    # node.label = unname(node.label),
    Nnode      = length(node.label)
  )
  class(tree) <- 'phylo'

  tree

}
