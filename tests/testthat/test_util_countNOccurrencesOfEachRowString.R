data<- data.frame(matrix(c(
    "foo", "foo", "foo","foo",
    "bar", "bot", "bet", "boing",
    "foo", "foo", "foo", "foo",
    "bar", "bar", "bar", "bar",
    "bar", "bar", "bar", "bar",
    "bar", "bar", "bar", "bar",
    "chip", "poing", "puf", "toing"
), nrow=7, ncol=4, byrow=TRUE))

uniques<- unique(data)

result<- util_countNOccurrencesOfEachRowString(uniques, data)

testthat::expect_equivalent(result, c(2,1,3,1))
