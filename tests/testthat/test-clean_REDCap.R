test_that("factor_table", {
  expect_equal(factor_table("0, No | 1, Yes"),
               tibble(key = c("0", "1"), value = c("No", "Yes")))
})

test_that("factor_convert", {

  dat <- tibble(var = c("0", "1"))
  dictionary <- tibble(`Variable / Field Name` = "var", `Choices, Calculations, OR Slider Labels` = "0, No | 1, Yes")

  expect_equal(
    mutate(dat, across("var", factor_convert, d = dat, dict = dictionary)),
    tibble(var = factor(c("No", "Yes"), levels = c("No", "Yes")))
  )
})

test_that("checkbox_labels", {

  dat <- tibble(var___0 = c("0", "1"), var___1 = c("1", "0"))
  dictionary <- tibble(`Variable / Field Name` = "var", `Choices, Calculations, OR Slider Labels` = "0, No | 1, Yes")

  o <- list("No", "Yes")
  names(o) <- paste("var", 0:1, sep = "___")

  expect_equal(checkbox_labels("var", dictionary), o)

})

test_that("variable_labels", {

  dat <- tibble(var = 1)
  dictionary <- tibble(`Variable / Field Name` = "var", `Field Label` = "Label")

  expect_equal(variable_labels(dat, dictionary), "Label")

})

test_that("clean_REDCap", {

  expect_equal(clean_REDCap(clean_REDCap_dat, clean_REDCap_dat_dictionary), clean_REDCap_validation)

})
