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
})