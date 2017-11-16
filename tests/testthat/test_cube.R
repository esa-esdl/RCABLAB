cubepath <- "/Net/Groups/BGI/scratch/DataCube/v1.0.0/low-res"
cube  <- open.cube(cubepath)

test_that("number of output dimensions dimensions of the cube", {
  for (v in cube$data.dir.entries) {
    d <- read.cube(
      cube,
      variable = v,
      latitude = c(40),
      longitude = c(40),
      time = cube$config$start_time
    )
    expect_equal(length(dim(d[[v]])), 3)
  }
})
