% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/epidemioCOVID.R
\name{siteVisual}
\alias{siteVisual}
\title{Representing Aligned sequence with muatation sites
perform alignment between multiple sequence fasta file
genome sequence samples of COVID virus}
\usage{
siteVisual(fasta, start, end, legend = TRUE)
}
\arguments{
\item{fasta}{the path for FASTA format multiple sequences}

\item{start}{start position of the sequence to visulize}

\item{end}{end position of the sequence to visulize}

\item{legend}{logical. Should this layer be included in the legends}
}
\value{
Returns the visualization of the selected site in the alignment
}
\description{
Representing Aligned sequence with muatation sites
perform alignment between multiple sequence fasta file
genome sequence samples of COVID virus
}
\examples{
\dontrun{
fa <- system.file("extdata", "samplefake.fasta", package = "epidemioCOVID")
siteVisual(fa, 45, 55)
}
\dontrun{
fasta <- system.file("extdata", "sample2.fasta", package = "epidemioCOVID")
siteVisual(fasta, 1, 20)
}
}
\references{
L Zhou, T Feng, S Xu, F Gao, TT Lam, Q Wang, T Wu, H Huang,
L Zhan, L Li, Y Guan, Z Dai*, G Yu* ggmsa: a visual exploration tool for
multiple sequence alignment and associated data. Briefings in Bioinformatics.
DOI:10.1093/bib/bbac222]
\href{https://github.com/YuLab-SMU/ggmsa}{Link}
}
