#'
#' @export
#'
loadme <- function(fname) {
  dat = data.table::fread(fname)
  sampnames = dat[,2]
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
  td = TreeSummarizedExperiment(
    colData=coldata,
    rowData=rowdata,
    assays=list(counts=t(mat))
  )
  td
}
