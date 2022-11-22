load("data/testMatrix3.rda")
load("data/testMatrix.rda")

# test if testMatrix3 is a matrix with eigenvectors different than one
testthat::expect_error(scm_verifyScm(testMatrix3))

# fix the matrix
fixed<- scm_fixEigenvectorLowerThanOne(testMatrix3)

# test
testthat::expect_equal(scm_verifyScm(fixed),fixed)

# add difference to the data
fixed[1,1]<- fixed[1,1]-0.1

# should raise error
testthat::expect_error(scm_fixEigenvectorLowerThanOne(fixed))


