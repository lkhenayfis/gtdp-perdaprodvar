test_that("Fit de modelo para perda", {
    dts <- agregasemana(dummydata)
    mod <- suppressWarnings(fitgam_perda(dts))

    printout <- capture.output(print(mod))
    expect_snapshot_value(printout, style = "serialize")

    expect_snapshot_value(AIC(mod), style = "serialize")
    expect_snapshot_value(BIC(mod), style = "serialize")

})

test_that("PERDA - Diferentes dimensoes de base", {
    dts <- agregasemana(dummydata)

    # Testando diferentes dimensoes de base ------------------------------

    for(n in c(5, 10, 15, 20)) {
        mod <- suppressWarnings(fitgam_perda(dts, n))

        smooth <- mod$model[[2]]$smooth[[1]]
        expect_equal(smooth$bs.dim, n)
    }
})

test_that("PERDA - Diferentes splines livres", {
    dts <- agregasemana(dummydata)

    # Testando multiplos tipos de spline ---------------------------------

    for(ts in c("ps", "tp", "ts", "cr", "cs", "ds")) {
        mod <- suppressWarnings(fitgam_perda(dts, ts = ts))

        expect_equal(class(mod), "gamperda")
        expect_true(all(mapply("-", mod$dat, dts[, .(vazao, perda)]) == 0))

        expect_equal(length(mod$model), 3)
        expect_equal(sapply(mod$model, class), list("lm", c("gam", "glm", "lm"), "lm"))

        smooth <- mod$model[[2]]$smooth[[1]]
        expect_equal(class(smooth)[1],
            switch(ts,
                "ps" = "pspline.smooth", tp = "tprs.smooth", "ts" = "ts.smooth",
                "cr" = "cr.smooth", "cs" = "cs.smooth", "ds" = "duchon.spline")
        )

        expect_snapshot_value(fitted(mod$model[[2]]), style = "serialize")
    }
})

test_that("PERDA - Shape constrained splines", {
    dts <- agregasemana(dummydata)

    # Testando multiplos tipos de spline ---------------------------------

    for(ts in c("mpi", "cx")) {
        mod <- suppressWarnings(fitgam_perda(dts, ts = ts))

        expect_equal(class(mod), "gamperda")
        expect_true(all(mapply("-", mod$dat, dts[, .(vazao, perda)]) == 0))

        expect_equal(length(mod$model), 3)
        expect_equal(sapply(mod$model, class), list("lm", c("scam", "glm", "lm"), "lm"))

        smooth <- mod$model[[2]]$smooth[[1]]
        expect_equal(class(smooth)[1], switch(ts, "mpi" = "mpi.smooth", cx = "cx.smooth"))

        expect_snapshot_value(fitted(mod$model[[2]]), style = "serialize")
    }
})

test_that("PROD - Diferentes distribuicoes", {
    dts <- agregasemana(dummydata)
    mod <- suppressWarnings(fitgam_perda(dts, dist = Gamma(link = "log")))

    printout <- capture.output(print(mod))
    expect_snapshot_value(printout, style = "serialize")

    expect_equal(class(mod), "gamperda")
    expect_true(all(mapply("-", mod$dat, dts[, .(vazao, perda)]) == 0))
    expect_equal(sapply(mod$model, class), list("lm", c("gam", "glm", "lm"), "lm"))

    expect_snapshot_value(AIC(mod), style = "serialize")
    expect_snapshot_value(BIC(mod), style = "serialize")
})

test_that("PERDA - Diferentes tipos de extrapolacao e quantis", {
    dts <- agregasemana(dummydata)

    # Sem extrapolacao ---------------------------------------------------

    mod_00 <- fitgam_perda(dts, extrap = c(0, 0))

    expect_equal(coefficients(mod_00$model[[1]]), c(0, NA))
    expect_equal(coefficients(mod_00$model[[3]]), c(NA, NA))
    expect_equal(unname(attr(mod_00, "cortes")), c(-1, Inf))

    # Testando tipos de extrapolacao -------------------------------------

    mod_11 <- suppressWarnings(fitgam_perda(dts, extrap = c(1, 1)))
    mod_12 <- suppressWarnings(fitgam_perda(dts, extrap = c(1, 2)))
    mod_21 <- suppressWarnings(fitgam_perda(dts, extrap = c(2, 1)))
    mod_22 <- suppressWarnings(fitgam_perda(dts, extrap = c(2, 2)))

    expect_equal(coefficients(mod_11$model[[1]]), coefficients(mod_12$model[[1]]))
    expect_equal(coefficients(mod_11$model[[3]]), coefficients(mod_21$model[[3]]))
    expect_equal(coefficients(mod_21$model[[1]]), coefficients(mod_22$model[[1]]))
    expect_equal(coefficients(mod_12$model[[3]]), coefficients(mod_22$model[[3]]))

    expect_equal(attr(mod_11, "cortes")[1], attr(mod_12, "cortes")[1])
    expect_equal(attr(mod_11, "cortes")[2], attr(mod_21, "cortes")[2])
    expect_equal(attr(mod_12, "cortes")[2], attr(mod_22, "cortes")[2])
    expect_equal(attr(mod_21, "cortes")[1], attr(mod_22, "cortes")[1])

    expect_equal(coefficients(mod_11$model[[3]])[1], 0)
    expect_equal(coefficients(mod_21$model[[3]])[1], 0)
    expect_true(coefficients(mod_12$model[[3]])[1] != 0)
    expect_true(coefficients(mod_22$model[[3]])[1] != 0)

    expect_equal(unname(attr(mod_11, "cortes")[1]), min(mod_11$dat$vazao))
    expect_equal(unname(attr(mod_12, "cortes")[1]), min(mod_12$dat$vazao))

    # Testando diferentes quantis ----------------------------------------

    expect_warning(mod1 <- fitgam_perda(dts, quantil = c(.01, .99)))
    expect_warning(mod2 <- fitgam_perda(dts, quantil = c(.025, .975)))
    expect_warning(mod3 <- fitgam_perda(dts, quantil = c(.05, .95)))
    mod4 <- fitgam_perda(dts, quantil = c(.1, .9))

})

test_that("PERDA - Metodos", {
    dts <- agregasemana(dummydata)

    # Testando metodos ---------------------------------------------------

    suppressWarnings(mod <- fitgam_perda(dts))

    expect_snapshot_value(fitted(mod), style = "serialize")
    expect_snapshot_value(residuals(mod), style = "serialize")
    expect_snapshot_value(predict(mod, newdata = data.frame(vazao = seq(0, attr(dts, "qmax"), 10))),
        style = "serialize")
})

test_that("PERDA - Otimizacao da dimensao de base", {
    dts <- agregasemana(dummydata)
    optmod <- suppressWarnings(optgam_perda(dts, 5:10))

    printout <- capture.output(print(optmod))
    expect_snapshot_value(printout, style = "serialize")

    expect_equal(class(optmod), "gamperda")
    expect_true(all(mapply("-", optmod$dat, dts[, .(vazao, perda)]) == 0))
    expect_equal(length(optmod$model), 3)
    expect_equal(sapply(optmod$model, class), list("lm", c("gam", "glm", "lm"), "lm"))

    expect_snapshot_value(AIC(optmod), style = "serialize")
    expect_snapshot_value(BIC(optmod), style = "serialize")
})