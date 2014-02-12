#!/usr/bin/env Rscript
library("testthat")

source("my.R")

test.my.common.len <- function() {
  expect_that(len(c(1,2,5)), equals(3))
}

test.my.common.lm_eqn <- function (){
  expect_that(lm_eqn(2:10, 3:11), equals("italic(\"y\") == \"1\" + \"1\" %.% italic(\"x\") * \",\" ~ ~italic(r)^2 ~ \"=\" ~ \"1\""))
  expect_that(lm_eqn(2:10, 2*(2:10)+3), equals("italic(\"y\") == \"3\" + \"2\" %.% italic(\"x\") * \",\" ~ ~italic(r)^2 ~ \"=\" ~ \"1\""))
}

test.my.common.len()
test.my.common.lm_eqn()

print("Passes all tests")
