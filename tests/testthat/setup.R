# Run before any test
load("../resources/clean_REDCap.RData")

# Run after all tests
withr::defer(teardown_env())
