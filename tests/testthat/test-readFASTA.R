# context("Checking for readFASTA input in epidemioCOVID")
# library(epidemioCOVID)

test_that("Checking valid input in FASTA format", {
  # single sequence
  fa1 <- system.file("extdata", "sampleseq.fasta", package = "epidemioCOVID")
  # more than one sequence in one file
  fa2 <- system.file("extdata", "samplefake.fasta", package = "epidemioCOVID")

  seq1 <- readFASTA(fa1)
  seq2 <- readFASTA(fa2)

  expect_that(nrow(seq1), equals(1))
  expect_that(nrow(seq2), equals(6))
})



test_that("Checking for invalid user input", {
  # test case adaped from code by Boris Steipe
  # no header lines in input
  expect_error(
    readFASTA(c("aca", "gc"))
  )

  # adjacent header lines in input
  expect_error(
    readFASTA(fc(">aqwrqr", "ca", ">qrwq", ">caqr", "cc"))
  )

  # invalid character in sequence
  expect_error(
    readFASTA(c("", ">aqwe", "ac", "c_a", "", ">aweqe", "ac", ""))
    )

  # only header, no sequence
  expect_error(
    readFASTA(c(" ", ">qweq", "aa", "ccca", "   ", ">qweqw"))
  )

  # in valid tab at last
  expect_error(
    readFASTA(c(" ", ">abc", "caca", "   ", "aca", ">ojz", "aca", "\t"))
  )


})
# [END]
