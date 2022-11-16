#' Aligning sample strends with existing covid strands
#
#' perform alignment between current COVID strands and one or more user-provided
#' genome sequence samples of COVID virus and identify the strand each sample
#' belongs to with a graphical visualization of the mutation site in the sequence.
#' I would like to improve it so that this package can analyze multiple sequence
#' samples and propose the possible relation between samples to construct an
#' epidemiological transmission link if exits.
#'
#' @param datasap of user-imported sample strand
#'
#' @param datatar of targeting aligning strand of the original virus sequence
#'
#' @return Returns the persentage of different nucleotides

#
similaritypc <- function(spl){
  load(file='data/refseq.rda')
  ref <- row.names(refseq) # I dont know why the data go into row names
  ref <- unlist(strsplit(ref, ""))
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

  for (i in cmplen) {
    if (ref[i] != spl[i]) {
      count = count + 1
    }
  }
  return (i / cmplen)
}
