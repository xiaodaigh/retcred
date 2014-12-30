library(retcred)

context("rwa")

test_that("RWA asset.class must be defined", {
  expect_error(rwa(pd = 0, lgd = 0, asset.class = "something"))
})

test_that("PD, LGD, and r must be between 0 and 1", {
  expect_equal(rwa(pd = 2, lgd = 0.5),NaN)
  expect_warning(rwa(pd = 2, lgd = 0.5))
  expect_equal(rwa(pd = 0.5, lgd = 2),NaN)
  expect_warning(rwa(pd = 0.5, lgd = 2))
  expect_equal(rwa(pd = 2, lgd = Inf),NaN)
  expect_warning(rwa(pd = 2, lgd = Inf))
  expect_equal(rwa(pd = -1, lgd = 0.5),NaN)
  expect_warning(rwa(pd = -1, lgd = 0.5))
  expect_equal(rwa(pd = 0.5, lgd = -0.5),NaN)
  expect_warning(rwa(pd = 0.5, lgd = -0.5))
  expect_equal(rwa(pd = -1, lgd = -Inf),NaN)
  expect_warning(rwa(pd = -1, lgd = -Inf))
  expect_equal(rwa(pd = 0.5, lgd = 0.5, r = 1),NaN)  
  expect_equal(rwa(pd = 0.5, lgd = 0.5, r = -1),NaN)
  expect_warning(rwa(pd = 0.5, lgd = 0.5, r = -1))
})


# test_that("Mortgage, Other Retail and QRRE", {
#   expect_equals(rwa(pd = c(0.1,0.5), lgd = c(0.1,0.5),"M"))
#   expect_equals(rwa(pd = 0.1, lgd = 0.1,"O"))
#   expect_equals(rwa(pd = 0.1, lgd = 0.1,"Q"))
#   expect
# })