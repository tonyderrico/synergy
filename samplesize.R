library('pwr')
library(pwrss)
delta <- 20
sigma <- 60
d <- delta/sigma
d = 0.196 #d effect size
pwr.t.test(d=d, sig.level=.01, power = .80, type = 'two.sample')

pwrss.f.reg(r2 = 30, k = 4, power = 0.80, alpha = 0.004)
pwrss.t.reg(beta1 = 0.796, k = 4, r2 = 0.5,
          power = .80, alpha = 0.004, alternative = "not equal")
