#' Aligning sample strends with existing covid strands
#'
#' perform alignment between current COVID strands and one or more user-provided
#' genome sequence samples of COVID virus and identify the strand each sample
#' belongs to with a graphical visualization of the mutation site in the sequence.
#' I would like to improve it so that this package can analyze multiple sequence
#' samples and propose the possible relation between samples to construct an
#' epidemiological transmission link if exits.
#' @param spl user-imported sample fasta data path name
#' @param ref user-imported sample fasta data path name
#'
#' @return Returns the persentage of different nucleotides
#'
#' @examples
#' load(file='data/refseq.rda')
#' load(file='data/splseq.rda')
#' matchPer(refseq, splseq)
#'
#‘ @export
#'
matchPer <- function(spl, ref) {
  spl <- unlist(strsplit(spl, ""))
  ref <- unlist(strsplit(ref, ""))

  # check input sample, do convertion
  if ((typeof(spl) == "character") == FALSE) {
    stop("the imput data type is not character")
  }
  spl <- unlist(strsplit(spl, ""))

  # check length
  if (length(ref) != length(spl)) {
    warning("unequal sequence length warning: incomplete sample sequence can
    yield a meaningless result")
  }
  count = 0
  cmplen <- min(length(ref),length(spl))
  for (i in 1:cmplen) {
    if (ref[i] != spl[i]) { count = count + 1 }
  }
  return (count / cmplen)
}

#' Representing Aligned sequence with muatation sites
#
#' perform alignment between multiple sequence fasta file
#' genome sequence samples of COVID virus
#'
#' @param spl of user-imported sample fasta sequence
#' @param st start position of the sequence to visulize
#' @param en end position of the sequence to visulize
#'
#' @return Returns the visualization of the selected site in the alignment
#'
#' @references L Zhou, T Feng, S Xu, F Gao, TT Lam, Q Wang, T Wu, H Huang,
#' L Zhan, L Li, Y Guan, Z Dai*, G Yu* ggmsa: a visual exploration tool for
#' multiple sequence alignment and associated data. Briefings in Bioinformatics.
#'  DOI:10.1093/bib/bbac222]
#'  \href{https://github.com/YuLab-SMU/ggmsa}
#'
#' @examples
#' fasta = "./inst/extdata/sample.fasta"
#' siteVisual(fasta, 1, 20)
#'
#' @importFrom ggmsa ggmsa
#‘ @export
#'
siteVisual <- function(fasta, st , en) {
  Gra <- ggmsa(fasta, start = st, end = en, color="Chemistry_NT")
  return(Gra)
}

