set.seed(12345)
# Dow Jones, S&P500, Nasdaq
pp = c(0.20,0.50,0.30)	 # weight
nd=rmultinom(1,10000,pp)[ ,1]	# sample size		
k = c(7,7,7)  # k degrees of freedom
s = c(0.967,1.153,1.692)    # scale
m = c(0.850 ,1.00 ,1.411)   # center/mean

tt = rt(100000, 7)
yy1= s[1]*tt + m[1]
yy2= s[2]*tt + m[2]
yy3= s[3]*tt + m[3]
print(var(yy1))
print(var(yy2))
print(var(yy3))


dt_func <- function(yy, m, k, s) {
  return (dt((yy-m)/s , k)/s)
}

yy_seq = seq(-5, 5, length.out=1000)

combined_pdf <- pp[1]*dt_func(yy_seq, m[1], k[1], s[1]) + pp[2]*dt_func(yy_seq, m[2], k[2], s[2]) +
  pp[3]*dt_func(yy_seq, m[3], k[3], s[3])

plot(yy_seq, dt_func(yy_seq, m[1], k[1], s[1]), type="l", lwd=2.5, col="red",
     xlab="Price Change (%)", ylab="Density")
lines(yy_seq, dt_func(yy_seq, m[2], k[2], s[2]), type="l", lwd=2.5, col="blue")
lines(yy_seq, dt_func(yy_seq, m[3], k[3], s[3]), type="l", lwd=2.5, col="gold")
lines(yy_seq, combined_pdf, lwd=2.5, col="cyan")
legend(x="topleft", legend=c("Dow Jones", "S&P500", "Nasdaq", "Combined"), col=c("red", "blue", "gold", "cyan"),
       lwd=2.5)
