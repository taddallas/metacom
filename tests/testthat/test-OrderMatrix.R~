context("findHost")

test_that("findHost finds the host", {
	mousey = findHost(genus="Peromyscus", species="leucopus")
	expect_is(mousey, "data.frame")
	expect_more_than(nrow(mousey), 1)
	nothing = findHost(genus="Nothing")
	expect_equal(nrow(nothing),0)
})
	
