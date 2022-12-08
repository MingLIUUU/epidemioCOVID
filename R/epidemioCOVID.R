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
#‘ @export
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



#' Aligning sample strends with existing covid strands
#'
#' perform alignment between current COVID strands and one or more user-provided
#' genome sequence samples of COVID virus and identify the strand each sample
#' belongs to.
#' @param spl user-imported sample fasta data path name
#' @param ref user-imported sample fasta data path name
#'
#' @return Returns data.frame
#'      $header char the FASTA header lines $seq double percentage of alignment
#'
#' @examples
#' FA <- system.file("extdata", "referseq.fasta", package = "epidemioCOVID")
#' seqs <- readFASTA(FA)
#' MP <- matchPer(seqs)
#'
#‘ @export
#'
matchPer <- function(seqs) {
  load("./data/refseq.rda")
  ref <- unlist(strsplit(refseq, ""))

  # initiate a dataframe for percentage data
  myDF <- data.frame(refID = seqs$refID,
                     percent = double(nrow(seqs)))
  # Do alignment for match percentage
  for (j in 1:nrow(seqs)) {
    spl <- unlist(strsplit(seqs$seq[j], ""))
    count = 0
    cmplen <- min(length(ref),length(spl))

    for (i in 1:cmplen) {
      if ((ref[i] == spl[i])) { count = count + 1 }
    }
    myDF$percent[j] <- count/cmplen
    cat(sprintf("%s has a coverage of %4.2f %%.\n", seqs$refID[j],
                myDF$percent[j]))
  }
  return (myDF)
}

#' Representing Aligned sequence with muatation sites
#
#' perform alignment between multiple sequence fasta file
#' genome sequence samples of COVID virus
#'
#' @param fasta of user-imported sample fasta sequence
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
#' fasta <- system.file("extdata", "sample2.fasta", package = "epidemioCOVID")
#' siteVisual(fasta, 1, 20)
#'
#' @importFrom ggmsa ggmsa
#‘ @export
#'
siteVisual <- function(fasta, st, en) {
  Gra <- (ggmsa(fasta, start = st, end = en, color="Chemistry_NT")
  + geom_seqlogo() + geom_msaBar())
  return(Gra)
}


