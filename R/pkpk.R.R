pkpk.R <-
function(dat1,dat2,h)
{
d6 = dat1$data
mono.y <- dat1$mono.y
colnames(dat2)=c("m.z","y")
d2 <- Baseline.R(dat2, 0.0251,FALSE)
d3 <- Denoising.R(d2,h)
pos.mono=lapply(1:length(mono.y),function(i) d3$m.z[d3$y==mono.y[[i]]])
mono = unlist(lapply(1:length(pos.mono), function(i) max(pos.mono[[i]][na.omit(pmatch(as.character(t(d6[[i]][1])), 
as.character(round(pos.mono[[i]]))))])))
mz=mono
y=mono.y
pk.data=data.frame(cbind(mz,y))
}

