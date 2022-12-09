#' read the FASTA format file
#'
#' read the input in FASTA format
#' @param  FA Input file name (or text connection)
#'
#' @return Returns data.frame
#' $header char  the FASTA header lines $seq   char  the actual sequences
#'
#' @references
#' Boris Steipe. “Introduction to R.”
#' \href{http://steipe.biochemistry.utoronto.ca/bio/RPR-Introduction.html}{Link}
#'
#' @examples
#' FA <- system.file("extdata", "sample.fasta", package = "epidemioCOVID")
#' seqs <- readFASTA(FA)
#'
#' @examples
#' FA <- system.file("extdata", "sampleseq.fasta", package = "epidemioCOVID")
#' seqs <- readFASTA(FA)
#'
#' @export
#'
readFASTA <- function(FA) {
  # Note: if length(FA) is one, it is assumed to be a filename
  if (length(FA) == 1) {
    FA <- readLines(FA)
  }
  FA <- FA[! grepl("^$", FA)]   # drop all empty lines
  iHead <- grep("^>", FA) # find all headers
  myFA <- data.frame(head = FA[iHead],
                     refID = character(length(iHead)),
                     seq  = character(length(iHead)))

  for (i in seq_along(iHead)) {
    header <- FA[iHead[i]]
    refidlast <- unlist(gregexpr('\\|', header))[1] - 2
    myFA$refID[i] <- paste0(substring(header, 2, last= refidlast), collapse = "")
    first <- iHead[i] + 1   # first line of each sequence
    last  <- ifelse(i < length(iHead), iHead[i + 1] - 1, length(FA)) # ...last
    myFA$seq[i] <- paste0(FA[first:last], collapse = "")
  }
  return(myFA)
}

#' Prepare sample strends with reference covid strands for Alignment
#'
#' Make a Mutiplealignment objects by Biostring package
#' @param seqs user-imported sample multiple sequence fasta data path name
#' @param refseq refaseq we use, by defaul is the NC_045512.2
#'
#' @return Returns an alignment Set
#'
#' @references
#' Boris Steipe. “Multiple Sequence Alignment.”
#' \href{http://steipe.biochemistry.utoronto.ca/bio/BIN-ALI-MSA.html}{Link}
#'
#' @examples
#' FA <- system.file("extdata", "samplefake.fasta", package = "epidemioCOVID")
#' seqs <- epidemioCOVID::readFASTA(FA)
#' msaSet <- preAlign(seqs, refseq = fakeref)
#'
#' @importFrom Biostrings DNAStringSet
#'
#' @export
#'
preAlign <- function(seqs, refseq = refseq) {
  # preparing string set
  seqset <- Biostrings::DNAStringSet(c(seqs$seq, refseq))
  # Give names
  seqset@ranges@NAMES <- c(seqs$refID, "RefSeq")
  return (seqset)
}



#' Aligning sample strends with reference covid strands with Muscle
#'
#' perform alignment between current COVID strands and one or more user-provided
#' genome sequence samples of COVID virus. Print the alignment.
#' @param seqset an alignment Set return by preAlign()
#'
#' @return Returns an alignment object
#'
#' @references
#' Boris Steipe. “Multiple Sequence Alignment.”
#' \href{http://steipe.biochemistry.utoronto.ca/bio/BIN-ALI-MSA.html}{Link}
#'
#' @examples
#' FA <- system.file("extdata", "samplefake.fasta", package = "epidemioCOVID")
#' seqs <- epidemioCOVID::readFASTA(FA)
#' msaSet <- preAlign(seqs, refseq = fakeref)
#' msa <- alignMSA(msaSet)
#'
#' @importFrom msa msaMuscle
#' @importFrom msa print
#'
#' @export
#'
alignMSA <- function(seqset) {
  # run alignment with "Muscle"
  (msa <-  msa::msaMuscle(seqset, order = "aligned"))
  msa::print(msa, show=c("alignment", "complete"), showConsensus=FALSE)
  return (msa)
}



#' Ploting the alignment result to show conserved and diverged regions
#'
#' perform alignment between current COVID strands and one or more user-provided
#' genome sequence samples of COVID virus and identify the strand each sample
#' belongs to.
#' @param msa the alignment object generate by alignMSA()
#'
#' @return Returns plot showing the alinment regions
#'
#' @references
#' Boris Steipe. “Multiple Sequence Alignment.”
#' \href{http://steipe.biochemistry.utoronto.ca/bio/BIN-ALI-MSA.html}{Link}
#'
#' @examples
#' FA <- system.file("extdata", "samplefake.fasta", package = "epidemioCOVID")
#' seqs <- epidemioCOVID::readFASTA(FA)
#' msaSet <- preAlign(seqs, refseq = fakeref)
#' msa <- alignMSA(msaSet)
#' MP <- msaPlot(msa)
#'
#' @examples
#' \dontrun{
#' FA <- system.file("extdata", "sample2.fasta", package = "epidemioCOVID")
#' seqs <- epidemioCOVID::readFASTA(FA)
#' MSA <- alignMSA(seqs)
#' MP <- msaPlot(MSA)
#' }
#'
#' @importFrom msa msaConservationScore
#' @importFrom graphics points
#' @importFrom utils data
#'
#' @export
#'
msaPlot <- function(msa) {
  # preparing string set

  # Plot the well aligned region
  msaMScores <- msa::msaConservationScore(msa, substitutionMatrix = BLOSUM62)
  p <- plot(msaMScores, type = "l", col = "#205C5E",
            xlab = "Alignment Position")

  wRadius <- 15     # we take the mean of all values around a point +- wRadius
  len <- length(msaMScores)
  v <- msaMScores

  for (i in (1 + wRadius):(len - wRadius)) {
    v[i] <- mean(msaMScores[(i - wRadius):(i + wRadius)]) # mean of values in
    # window around i
  points(v, col = "#FFFFFF", type = "l", lwd = 4.5)
  points(v, col = "#3DAEB2", type = "l", lwd = 3)
  }
  return (p)
}


#' Representing Aligned sequence with muatation sites
#
#' perform alignment between multiple sequence fasta file
#' genome sequence samples of COVID virus
#'
#' @param fasta the path for FASTA format multiple sequences
#' @param start start position of the sequence to visulize
#' @param end end position of the sequence to visulize
#' @param legend logical. Should this layer be included in the legends
#'
#' @return Returns the visualization of the selected site in the alignment
#'
#' @references L Zhou, T Feng, S Xu, F Gao, TT Lam, Q Wang, T Wu, H Huang,
#' L Zhan, L Li, Y Guan, Z Dai*, G Yu* ggmsa: a visual exploration tool for
#' multiple sequence alignment and associated data. Briefings in Bioinformatics.
#'  DOI:10.1093/bib/bbac222]
#' \href{https://github.com/YuLab-SMU/ggmsa}{Link}
#'
#' @examples
#' \dontrun{
#' fasta <- system.file("extdata", "samplefake.fasta", package = "epidemioCOVID")
#' siteVisual(fasta, 45, 55)
#' }
#' @examples
#' \dontrun{
#' fasta <- system.file("extdata", "sample2.fasta", package = "epidemioCOVID")
#' siteVisual(fasta, 1, 20)
#' }
#' @importFrom ggmsa ggmsa
#' @importFrom ggmsa geom_seqlogo
#' @importFrom ggmsa geom_msaBar
#'
#' @export
#'
siteVisual <- function(fasta, start, end,  legend = TRUE) {
  gra <- (ggmsa(fasta, start = start, end = end, color = "Chemistry_NT",
                char_width = 0.5, seq_name = T, show.legend = legend)
          + geom_seqlogo()
          + geom_msaBar()
          )
  return (gra)
}




