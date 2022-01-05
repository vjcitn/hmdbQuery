library(hmdbQuery)

context("communication and accessors")

test_that("query resolves", {
 lk1 = HmdbEntry()
 expect_true(class(lk1)=="HmdbEntry")
 expect_true(length(diseases(lk1))>2)
 expect_true(length(tissues(lk1))>=1)
 expect_true(length(biospecimens(lk1))>2)
})

context("stored tables")

test_that("stored data are stable", {
 data(hmdb_disease)
 expect_true(nrow(hmdb_disease)==75360)
 expect_true(ncol(hmdb_disease)==3)
})
