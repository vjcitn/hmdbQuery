library(hmdbQuery)

context("communication and accessors")

test_that("query resolves", {
 lk1 = HmdbEntry()
 expect_true(class(lk1)=="HmdbEntry")
 expect_true(length(diseases(lk1))>2)
 expect_true(length(tissues(lk1))>1)
 expect_true(length(biofluids(lk1))>2)
})
