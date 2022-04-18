test_that("Grades de produtibilidade", {
    dts <- agregasemana(dummydata)
    mod <- fitgam_prod(dts)
    grd <- extraigrid(mod, c(10, 10))

    expect_equal(class(grd), "gridprod")
    expect_equal(names(grd), c("grid", "model"))
    expect_equal(class(grd[[1]])[1], "data.table")
    expect_equal(class(grd[[2]]), "gamprod")
    expect_equal(grd[[2]], mod)

    # Metodos ------------------------------------------------------------

    expect_snapshot_value(fitted(grd), style = "serialize")
    expect_snapshot_value(residuals(grd), style = "serialize")

    expect_snapshot_value(predict(grd, newdata = expand.grid(quedal = c(20, 20.5, 21), vazao = 10 + 0:9 * 10)),
        style = "serialize")

    geradf <- function(rows = 10, seed = 12334) {
        set.seed(seed)
        data.frame(quedal = runif(10, 20, 21), vazao = sample(10:100, 10))
    }

    expect_snapshot_value(predict(grd, newdata = geradf()), style = "serialize")

    # previsao out of bounds
    predna <- predict(grd, newdata = expand.grid(quedal = c(18, 19, 22, 23), vazao = c(-10, -1, 250, 300)))
    expect_true(all(is.na(predna)))

    # Otimizacao da dimensao de grade ------------------------------------

    opt <- optgrid(mod, range.quedal = 15:25, range.vazao = 25:35, full.output = TRUE)
    grd2 <- extraigrid(mod, c(16, 32))

    expect_equal(class(opt[[1]]), "gridprod")
    expect_equal(opt[[1]], grd2)

    expect_equal(class(opt[[2]]), "varreduraprod")
    expect_equal(names(opt[[2]]), c("razao", "range.quedal", "range.vazao", "R", "persis", "front", "minprod"))
    expect_equal(opt[[2]]$range.quedal, 15:25)
    expect_equal(opt[[2]]$range.vazao, 25:35)
})