data<- list(
  x=1:100,
  y=list(1:25,26:50,51:75,76:100)
)

x<- data[['x']]
y<- unname(data[['y']])
testthat::expect_equal(util_splitIntoChunks(x,4),y)