############################### AJUSTE DE MODELOS ADITIVOS AOS DADOS ###############################

fitgam_perda <- function(dat, ns.vazao = 10, ts.vazao = "tp", extrap = c(1, 1), quantil = c(.05, .95)) {

    if(!(all(extrap %in% 1:2))) stop("extrap so pode assumir valor 1 ou 2")

    atributos <- attributes(dat)[c("cod", "nome", "nmaq", "qmax")]

    form <- as.formula(paste0("perda ~ s(vazao, bs = '", ts.vazao, "', k = ", ns.vazao, ")"))
    mod  <- mgcv::gam(form, data = dat)

    # Extrapolacao inferior ----------------------------------------------

    dI <- dat[vazao < quantile(vazao, quantil[1])]

    if(extrap[1] == 1) {
        coefI  <- dI[which.min(perda), predict(mod, .SD) / vazao^2]
        corteI <- dI[which.min(perda), vazao][1]
    } else {
        r     <- lm(perda ~ I(vazao^2) - 1, data = dI, )
        coefI <- r$coefficients

        vx <- seq(0, max(dI$vazao), length.out = 1000)
        vextrap <- coefI * vx^2
        vgam    <- predict(mod, newdata = data.frame(vazao = vx))

        vdiff <- vextrap - vgam
        vsign <- sign(vdiff)
        corteI <- which(diff(vsign) != 0)

        if(length(corteI) != 0) {
            corteI <- vx[corteI + 1][1]
        } else {
            corteI <- which.min(abs(vdiff))
            corteI <- vx[corteI][1]
            warning("Não há cruzamento entre a extrapolação inferior escolhida e o ajuste do GAM - foi utilizado o ponto mais próximo")
        }
    }

    # Extrapolacao superior ----------------------------------------------

    dS <- dat[vazao > quantile(vazao, quantil[2])]
    vx <- seq(min(min(dS$vazao), atributos$qmax), atributos$qmax, length.out = 1000)

    if(extrap[2] == 1) {
        coefS <- dS[which.max(vazao), perda / vazao^2]
        coefS <- c(0, coefS)
    } else {
        X   <- cbind(1, dS$vazao^2)
        Y   <- dS$perda
        e   <- matrix(c(1, 0, 0, 1), 2)
        f   <- c(0, 0)
        coefS <- lsei::lsei(X, Y, e = e, f = f)
    }

    vextrap <- coefS[1] + coefS[2] * vx^2
    vgam    <- predict(mod, newdata = data.frame(vazao = vx))

    vdiff <- vextrap - vgam
    vsign <- sign(vdiff)
    corteS <- which(diff(vsign) != 0)

    if(length(corteS) != 0) {
        corteS <- vx[corteS + 1][1]
    } else {
        corteS <- which.min(abs(vdiff))
        corteS <- vx[corteS][1]
        warning("Não há cruzamento entre a extrapolação superior escolhida e o ajuste do GAM - foi utilizado o ponto mais próximo")
    }

    # Monta objeto de saida ----------------------------------------------

    new_gamperda(dat, mod, coefI, corteI, coefS, corteS, atributos)
}