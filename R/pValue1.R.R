pValue1.R <-
function(x,par,B)
{
xstar =dat.star(x,par,B)
parsboot = matrix(t(sapply(1:B,function(i) mixtureEM.R(xstar[[i]])$pars)),ncol=3)
tstar= unlist(sapply(1:B,function(i) kullBack.R(xstar[[i]],parsboot[i,])$t1))
p1val=(1/B)*length(which(tstar>=kullBack.R(x,par)$t1))
list(p1val=p1val)
}

