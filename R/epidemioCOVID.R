#' Aligning sample strends with existing covid strands
#
#' perform alignment between current COVID strands and one or more user-provided
#' genome sequence samples of COVID virus and identify the strand each sample
#' belongs to with a graphical visualization of the mutation site in the sequence.
#' I would like to improve it so that this package can analyze multiple sequence
#' samples and propose the possible relation between samples to construct an
#' epidemiological transmission link if exits.
#' @param spl user-imported sample fasta data path name
#'
#' @return Returns the persentage of different nucleotides
#
match.percent <- function(spl){
  spl <- readLines(spl)
  spl <- c(spl[1], paste(spl[-1], sep = "", collapse = ""))
  spl <- spl[2]
  spl <- unlist(strsplit(spl, ""))

  load(file='data/refseq.rda')

  ref <- unlist(strsplit(refseq, ""))
  # check input sample, do convertion
  if ((typeof(spl) == "character") == FALSE) {
    print("the imput data type is not character")
    exit(1)
  }
  spl <- unlist(strsplit(spl, ""))
  # check length
  if (length(ref) != length(spl)) {
    print("unequal sequence length warning: incomplete sample sequence can yield
    a meaningless result")
  }

  count = 0
  cmplen <- min(length(ref),length(spl))

  for (i in 1:cmplen) {
    if (ref[i] != spl[i]) {
      count = count + 1
    }
  }
  return (count / cmplen)
}
#' Representing Aligned sequence with muatation sites
#
#' perform alignment between current COVID strands and one or more user-provided
#' genome sequence samples of COVID virus and identify the strand each sample
#'
#' @param spl of user-imported sample fasta data path name
#'
#' @return Returns the persentage of different nucleotides
#
mutsite.present <- function(splfasta, start, end) {
  ref <- system.file("extdata", "referseq.fasta", package = "epidemioCOVID")
  mySequences <- readDNAStringSet(c(splfasta, ref))
  g <- ggmsa(mySequences, start, end)
  return()
}

