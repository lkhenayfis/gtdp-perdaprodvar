test_that("Grades de perda", {
    dts <- agregasemana(dummydata)
    mod <- fitgam_perda(dts, 7)
    grd <- extraigrid(mod, 20)

    expect_equal(class(grd), "gridperda")
    expect_equal(names(grd), c("grid", "model"))
    expect_equal(class(grd[[1]])[1], "data.table")
    expect_equal(class(grd[[2]]), "gamperda")
    expect_equal(grd[[2]], mod)

    # Metodos ------------------------------------------------------------

    expect_snapshot_value(fitted(grd), style = "serialize")
    expect_snapshot_value(residuals(grd), style = "serialize")

    expect_snapshot_value(predict(grd, newdata = data.frame(vazao = 1:200)), style = "serialize")
    expect_snapshot_value(predict(grd, newdata = data.frame(vazao = c(10, 1, 130, 50, 60, 20, 190))),
        style = "serialize")

    # previsao out of bounds
    predna <- predict(grd, newdata = data.frame(vazao = c(-10, -1, 250, 300)))
    expect_true(all(is.na(predna)))

    # Otimizacao de grade ------------------------------------------------

    opt <- optgrid(mod, full.output = TRUE)
    grd2 <- extraigrid(mod, 27)

    expect_equal(class(opt[[1]]), "gridperda")
    expect_equal(opt[[1]], grd2)

    expect_equal(class(opt[[2]]), "varreduraperda")
    expect_equal(names(opt[[2]]), c("razao", "range", "R", "front"))
    expect_equal(opt[[2]]$range, 5:50)

    varr <- opt[[2]]
    expect_true(all(varr$razao[varr$range >= 27] < 1.01))

    opt <- optgrid(mod, R = 1.05, range.vazao = 2:100, full.output = TRUE)
    grd2 <- extraigrid(mod, 15)

    expect_equal(class(opt[[1]]), "gridperda")
    expect_equal(opt[[1]], grd2)

    expect_equal(class(opt[[2]]), "varreduraperda")
    expect_equal(names(opt[[2]]), c("razao", "range", "R", "front"))
    expect_equal(opt[[2]]$range, 2:100)

    varr <- opt[[2]]
    expect_true(all(varr$razao[varr$range >= 15] < 1.05))
})