dataGen <-
function(n,lambda1,lambda2,w,...)
{
x = ifelse(runif(n) < w,rpois(n,lambda1),rpois(n,lambda2)+1)
z=cbind(sort(unique(x)),table(x)[])
return(z)
}

