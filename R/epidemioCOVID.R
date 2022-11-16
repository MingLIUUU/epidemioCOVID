#' Aligning sample strends with existing covid strands
#
#' perform alignment between current COVID strands and one or more user-provided
#' genome sequence samples of COVID virus and identify the strand each sample
#' belongs to with a graphical visualization of the mutation site in the sequence.
#' I would like to improve it so that this package can analyze multiple sequence
#' samples and propose the possible relation between samples to construct an
#' epidemiological transmission link if exits.
#'
#' @param datasap of user provides sample RNA strand
#'
#' @param datatar of targeting aligning strand
#'
#' @return Returns the algnment persentages

#
# GeneCounts <- read.csv("~/Desktop/GeneCounts.csv",
#                        row.names = 1)
# dim(GeneCounts) # 30 3
# typeof(GeneCounts)
# head(GeneCounts)
