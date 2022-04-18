test_that("PLOT - visualizacao de gamperda", {
    dts <- agregasemana(dummydata)

    mod1 <- suppressWarnings(fitgam_perda(dts))
    mod2 <- suppressWarnings(fitgam_perda(dts, 5))
    mod3 <- suppressWarnings(fitgam_perda(dts, 7))

    p1 <- plot(mod1, plot = FALSE)
    expect_equal(class(p1), c("gg", "ggplot"))

    p2 <- plot(mod1, mod2, mod3, plot = FALSE)
    expect_equal(class(p2), c("gg", "ggplot"))
})

test_that("PLOT - visualizacao de gridperda", {
    dts <- agregasemana(dummydata)

    mod1 <- suppressWarnings(fitgam_perda(dts))
    grid <- extraigrid(mod1, 10)

    p1 <- plot(grid, plot = FALSE)
    expect_equal(class(p1), c("gg", "ggplot"))

    grid <- extraigrid(mod1, 20)
    p2 <- plot(grid, plot = FALSE)
    expect_equal(class(p2), c("gg", "ggplot"))
})

test_that("PLOT - visualizacao de varreduraperda", {
    dts <- agregasemana(dummydata)

    mod1 <- suppressWarnings(fitgam_perda(dts))
    opt  <- optgrid(mod1, full.output = TRUE)

    p1 <- plot(opt$optgrid, plot = FALSE)
    expect_equal(class(p1), c("gg", "ggplot"))

    p2 <- plot(opt$varredura, plot = FALSE)
    expect_equal(class(p2), c("gg", "ggplot"))
})

test_that("PLOT - visualizacao de gamprod", {
    dts <- agregasemana(dummydata)

    mod1 <- fitgam_prod(dts)

    p1 <- plot(mod1, plot = FALSE)
    expect_equal(class(p1), c("plotly", "htmlwidget"))
})

test_that("PLOT - visualizacao de gridprod", {
    dts <- agregasemana(dummydata)

    mod1 <- fitgam_prod(dts)
    grid <- extraigrid(mod1, c(10, 10))

    p1 <- plot(grid, plot = FALSE)
    expect_equal(class(p1), c("plotly", "htmlwidget"))

    grid <- extraigrid(mod1, c(20, 20))
    p2 <- plot(grid, plot = FALSE)
    expect_equal(class(p2), c("plotly", "htmlwidget"))
})

test_that("PLOT - visualizacao de varreduraprod", {
    dts <- agregasemana(dummydata)

    mod1 <- fitgam_prod(dts)
    opt  <- optgrid(mod1, range.quedal = 20:40, range.vazao = 20:40, full.output = TRUE)

    p1 <- plot(opt$optgrid, plot = FALSE)
    expect_equal(class(p1), c("plotly", "htmlwidget"))

    p2 <- plot(opt$varredura, plot = FALSE)
    expect_equal(class(p2), c("plotly", "htmlwidget"))
})
