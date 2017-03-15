context("BoundaryClump")

test_that("Metacommunity and BoundaryClump agree", {
	set.seed(100);
	test <- matrix(rbinom(100,1,0.5), ncol=10)
	mc.test <- Metacommunity(test, sims=100, method='swap')
	bc.test <- BoundaryClump(test)
	expect_equal(mc.test$Boundary[1], bc.test[1])
})
