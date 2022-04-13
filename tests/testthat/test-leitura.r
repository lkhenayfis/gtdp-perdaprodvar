test_that("Testes de leitura", {
    arq1 <- system.file("extdata/dummyusi1.xlsx", package = "perdaprodvar")
    dt1  <- leplanilha(arq1)

    expect_equal(attr(dt1, "cod"), 999)
    expect_equal(attr(dt1, "nmaq"), 2)
    expect_equal(attr(dt1, "qmax"), 300)

    expect_equal(nrow(dt1), 168)

    expect_equal(dt1$prod[1], 0.0090082663879671)
    expect_equal(dt1$prod[10], 0.00900840929132227)
    expect_equal(dt1$prod[168], 0.0090092935492718)

    expect_equal(dt1$energia[1], 41.782297)
    expect_equal(dt1$energia[10], 44.061)
    expect_equal(dt1$energia[168], 91.515)

    expect_equal(dt1$quedal[1], 29.615771682782)
    expect_equal(dt1$quedal[10], 29.644046770156)
    expect_equal(dt1$quedal[168], 30.598206930368)

    expect_equal(dt1$perda[1], 0.388861700341704)
    expect_equal(dt1$perda[10], 0.388423285877094)
    expect_equal(dt1$perda[168], 0.149992289907335)

    expect_equal(dt1$vazao[1], 258.732086226258)
    expect_equal(dt1$vazao[10], 258.552751791839)
    expect_equal(dt1$vazao[168], 134.138292304421)

    arq2 <- system.file("extdata/dummyusi2.xlsx", package = "perdaprodvar")
    dt2  <- leplanilha(arq2)

    expect_equal(attr(dt2, "cod"), 999)
    expect_equal(attr(dt2, "nmaq"), 2)
    expect_equal(attr(dt2, "qmax"), 300)

    expect_equal(nrow(dt2), 168)

    expect_equal(dt2$prod, dt1$prod / (9.81 * 1000) * (9.9 * 990))
    expect_equal(dt2$energia, dt1$energia)
    expect_equal(dt2$quedab, dt1$quedab)
    expect_equal(dt2$perda, dt1$perda)
    expect_equal(dt2$vazao, dt1$vazao)
})

test_that("Testes de agregacao semanal", {
    dts <- agregasemana(dummydata)

    expect_equal(colnames(dummydata), colnames(dts))

    atrs <- c("cod", "nome", "nmaq", "qmax")
    expect_equal(attributes(dummydata)[atrs], attributes(dts)[atrs])
})