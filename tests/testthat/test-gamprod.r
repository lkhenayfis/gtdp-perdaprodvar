test_that("Fit de moddelo para prod", {
    dts <- agregasemana(dummydata)
    mod <- fitgam_prod(dts)

    printout <- capture.output(print(mod))
    expect_snapshot_value(printout, style = "serialize")

    expect_equal(class(mod), "gamprod")
    expect_true(all(mapply("-", mod$dat, dts[, .(quedal, vazao, prod)]) == 0))
    expect_equal(class(mod$model), c("gam", "glm", "lm"))

    expect_snapshot_value(AIC(mod), style = "serialize")
    expect_snapshot_value(BIC(mod), style = "serialize")
})

test_that("PROD - Diferentes dimensoes de base", {
    dts <- agregasemana(dummydata)

    # Testando diferentes dimensoes de base ------------------------------

    dims <- expand.grid(c(5, 7, 10), c(5, 7, 10))
    for(i in seq(nrow(dims))) {
        mod <- fitgam_prod(dts, unlist(dims[i, ]))

        smooth <- mod$model$smooth[[1]]
        expect_equal(smooth$margin[[1]]$bs.dim, dims[i, 1])
        expect_equal(smooth$margin[[2]]$bs.dim, dims[i, 2])
    }
})

test_that("PROD - Diferentes splines e modos", {
    dts <- agregasemana(dummydata)

    # Testando multiplos tipos de spline e modos -------------------------

    tipos <- expand.grid(c("ps", "tp", "ts", "cr", "cs", "ds"),
        c("ps", "tp", "ts", "cr", "cs", "ds"),
        c("tensor", "multivar", "simples"),
        stringsAsFactors = FALSE)

    for(i in seq(nrow(tipos))) {

        if((!(tipos[i, 1] %in% c("tp", "ts")) | !(tipos[i, 2] %in% c("tp", "ts"))) & (tipos[i, 3] == "multivar")) {
            expect_error(
                mod <- fitgam_prod(dts, ts = unlist(tipos[i, 1:2]), modo = tipos[i, 3])
            )
            next
        }
        mod <- fitgam_prod(dts, ts = unlist(tipos[i, 1:2]))

        smooth <- mod$model$smooth[[1]]
        for(j in seq(2)) {
            expect_equal(class(smooth$margin[[j]])[1],
                switch(tipos[i, j],
                    "ps" = "pspline.smooth", tp = "tprs.smooth", "ts" = "ts.smooth",
                    "cr" = "cr.smooth", "cs" = "cs.smooth", "ds" = "duchon.spline")
            )
        }

        expect_snapshot_value(fitted(mod$model), style = "serialize", )
    }
})

test_that("PROD - Metodos", {
    dts <- agregasemana(dummydata)

    # Testando metodos ---------------------------------------------------

    mod <- fitgam_prod(dts)

    expect_snapshot_value(fitted(mod), style = "serialize")
    expect_snapshot_value(residuals(mod), style = "serialize")
    expect_snapshot_value(predict(mod,
        newdata = data.frame(quedal = seq(20.5, 21, .1), vazao = seq(150, 200, 10))),
        style = "serialize")
})

test_that("PROD - Otimizacao da dimensao de base", {
    dts <- agregasemana(dummydata)
    optmod <- optgam_prod(dts, list(5:10, 5:10))

    printout <- capture.output(print(optmod))
    expect_snapshot_value(printout, style = "serialize")

    expect_equal(class(optmod), "gamprod")
    expect_true(all(mapply("-", optmod$dat, dts[, .(quedal, vazao, prod)]) == 0))
    expect_equal(class(optmod$model), c("gam", "glm", "lm"))

    expect_snapshot_value(AIC(optmod), style = "serialize")
    expect_snapshot_value(BIC(optmod), style = "serialize")
})