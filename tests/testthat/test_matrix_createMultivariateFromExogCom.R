# data taken from the paper: A Simplified Higher-Order Markov Chain Model, wang et all
# doi: doi.org/10.5281/zenodo.1089379

S1<- as.factor(c(2,1,3,4,4,3,3,1,3,3,2,3))
S2<- as.factor(c(2,4,4,4,4,2,3,3,1,4,3,3))

y21<- matrix(c(1/2,0,0,1/2,
               0,0,1/2,1/2,
               0,0,3/5,2/5,
               0,1/2,0,1/2
               ),nrow = 4,ncol = 4,byrow = TRUE)

y112<- matrix(c(0,0,1/2,1/2,
                0,0,1,0,
                1/5,1/5,2/5,1/5,
                0,0,1,0), nrow = 4,ncol = 4,byrow = TRUE)

testthat::expect_equivalent(matrix_createMultivariateFromExogCom(S2,S1,1) %>% matrix_transitionProbabilities() %>% unlist(),y21 %>% unlist() )
testthat::expect_equivalent(matrix_createMultivariateFromExogCom(S1,S1,2) %>% matrix_transitionProbabilities() %>% unlist(),y112 %>% unlist() )