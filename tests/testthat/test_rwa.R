library(retcred)

context("rwa")

test_that("RWA asset.class must be defined", {
  expect_error(rwa(pd = 0, lgd = 0, asset.class = "something"))
})

test_that("PD, LGD, and r must be between 0 and 1", {
  expect_error(rwa(pd = 2, lgd = 0.5))
  expect_error(rwa(pd = 2, lgd = 0.5))
  expect_error(rwa(pd = 0.5, lgd = 2))
  expect_error(rwa(pd = 0.5, lgd = 2))
  expect_error(rwa(pd = 2, lgd = Inf))
  expect_error(rwa(pd = 2, lgd = Inf))
  expect_error(rwa(pd = -1, lgd = 0.5))
  expect_error(rwa(pd = -1, lgd = 0.5))
  expect_error(rwa(pd = 0.5, lgd = -0.5))
  expect_error(rwa(pd = 0.5, lgd = -0.5))
  expect_error(rwa(pd = -1, lgd = -Inf))
  expect_error(rwa(pd = -1, lgd = -Inf))
  expect_error(rwa(pd = 0.5, lgd = 0.5, r = 1))  
  expect_error(rwa(pd = 0.5, lgd = 0.5, r = -1))
  expect_error(rwa(pd = 0.5, lgd = 0.5, r = -1))
})