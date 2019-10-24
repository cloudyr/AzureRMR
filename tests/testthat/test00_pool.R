context("pool")

skip_on_cran()

test_that("Background process pool works",
{
    expect_false(pool_exists())

    init_pool(5)
    expect_true(pool_exists())
    expect_identical(pool_size(), 5L)

    res <- pool_sapply(1:5, function(x) x)
    expect_identical(res, 1:5)

    res2 <- pool_lapply(1:5, function(x) x)
    expect_identical(res2, list(1L, 2L, 3L, 4L, 5L))

    res3 <- pool_map(function(x, y) x + y, 1:5, 2)
    expect_identical(res3, list(3, 4, 5, 6, 7))

    y <- 42
    pool_export("y", environment())
    rm(y)  # work around testthat environment shenanigans
    res <- pool_sapply(1:5, function(x) y)
    expect_identical(res, rep(42, 5))

    init_pool(5)
    expect_true(all(sapply(pool_evalq(ls()), is_empty)))
    expect_error(pool_sapply(1:5, function(x) y))

    delete_pool()
    expect_false(pool_exists())
})


test_that("Pool functionality falls back correctly if pool doesn't exist",
{
    expect_false(pool_exists())

    res <- pool_sapply(1:5, function(x) x)
    expect_identical(res, 1:5)

    res2 <- pool_lapply(1:5, function(x) x)
    expect_identical(res2, list(1L, 2L, 3L, 4L, 5L))

    res3 <- pool_map(function(x, y) x + y, 1:5, 2)
    expect_identical(res3, list(3, 4, 5, 6, 7))

    y <- 42
    pool_export("y", environment())
    res <- pool_sapply(1:5, function(x) y)
    expect_identical(res, rep(42, 5))

    rm(y)
    expect_false(pool_evalq(exists("y")))
    expect_error(pool_sapply(1:5, function(x) y))
})