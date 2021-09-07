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

  dat <- tibble(var = 1, var2___1 = 1, var2___2 = 0)
  dictionary <- tibble(`Variable / Field Name` = c("var", "var2"),
                       `Field Label` = c("Label for field 'Var'", NA),
                       `Field Type` = c(NA, "checkbox"),
                       `Choices, Calculations, OR Slider Labels` = c(NA, "1, First checkbox label | 2, Second checkbox label"))

  expect_equal(labelled::var_label(variable_labels(dat, dictionary)),
               list(var = "Label for field 'Var'",
                    var2___1 = "First checkbox label",
                    var2___2 = "Second checkbox label"))

})

test_that("yesno_vars", {

  dat <- tibble(var = factor(c("Yes", "Yes", "No", "Yes"), levels = c("Yes", "No")))

  expect_equal(yesno_vars(dat), "var")

})

test_that("clean_REDCap", {

  expect_equal(clean_REDCap(clean_REDCap_dat, clean_REDCap_dat_dictionary, yesno_to_bool = TRUE), clean_REDCap_validation)

})
