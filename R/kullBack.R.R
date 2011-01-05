kullBack.R <-
function (dat, par){
dat = as.matrix(dat, ncol = 2)
x = dat[, 1]
y = dat[, 2]
fhat = y/sum(y)
fdensity = par[3]*dpois(dat,par[1]) + (1-par[3])*dpois(dat-1,par[2])
fhat = ifelse(fhat > 0, fhat, 0.001)
fdensity = ifelse(fdensity > 0, fdensity, 0.001)
t1 = abs(sum(fhat * log(fhat/fdensity)))
list(t1 = t1)
}

