library(MPV)

y.lm <- lm(y ~ x1 + x2 + x3 + x4 + x5, data = table.b7)

summary(y.lm)


p.lm <- lm(purity ~ hydro, data = p2.7)

summary(p.lm)
 

X <- model.matrix(p.lm)

XX <- t(X)%*%X

XXinv <- qr.solve(XX)

XXinv
