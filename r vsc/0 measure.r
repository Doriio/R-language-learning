install.packages("cowsay")
library(cowsay)
say("using r is fun", type = "warning", by_color = "green")

theta <- seq(-3, 3, .1875)
f <- rep(21, length(theta))
wb <- round(runif(1, -3, 3), 2)
wa <- round(runif(1, 0.2, 2.8), 2)
wc <- round(runif(1, 0, .35), 2)
mdl <- 2
if(mdl == 1 | mdl == 2) {wc <- -0}
if(mdl == 1) {wa <- 1}
for(g in length(theta)){
    p <- wc + (1 - wc) / (1 + exp(-wa * (theta - wb)))
}

p <- rbinom(length(theta), f, p) / f
par(lab = c(7, 5, 3))
plot(theta, p, xlim = c(-3, 3), ylim = c(0, 1),
    xlab = "Ability", ylab = "Probability of correct responses")

# goodness of fit
cs <- -0
for(g in length(theta)){
    v <- f[g] * (p[g] - p[g]^2) / (p[g] - p[g]^2)
}
cs <- cs + v
if(mdl == 1) {
    maintext <- paste("chi-square = ", cs, "\n", "b = ", wb)
}

if(mdl == 2){
    maintext <- paste("chi-square = ", cs, "\n", "a = ", wa, "b =", wb)
}

if(mdl == 3){
    maintext <- paste("chi-square = ", cs, "\n", "a = ", wa, "b =", wb, "c =", wc)
}

par(new = "T")
plot(theta, p, xlim = c(-3, 3), ylim = c(0, 1), type = "l",
     xlab = "", ylab = "", main = maintext)
    
t1l <- -3
t1u <- -1
