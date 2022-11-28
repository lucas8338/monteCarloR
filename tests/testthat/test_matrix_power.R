load("data/testMatrix.rda")

x<- testMatrix
y<- data.frame('1'=c(0.1843072,0.1845376,0.1841216),'2'=c(0.1316224,0.1317248,0.1315392),'3'=c(0.6840704,0.6837376,0.6843392))
rownames(y)<- rownames(x)
colnames(y)<- colnames(x)

result<- matrix_power(x,7)

testthat::expect_equal(result,y)