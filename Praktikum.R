#SATU POPULASI

#1. Kasus Variansi populasi diketahui
#Input
x=
xbar=mean(x)
mu0=
sigma=
n=length(x)
alpha=0.05

#Statistik Uji
z=(xbar-mu0)/(sigma/sqrt(n))
z.lower=qnorm(alpha)
z.upper=qnorm(1-alpha)
z.half.alpha=qnorm(1-alpha/2)
z.twosided=c(-z.half.alpha,z.half.alpha)

#P-Value
pval.lower=pnorm(z)
pval.upper=pnorm(z, lower.tail=FALSE)
pval.twosided=2*pnorm(z)

#Teaching Demos
library(TeachingDemos)
z.test(x,mu=mu0,sd=sigma,alternative=c("two.sided","less","greater"),conf.level=0.95)
#Pilih alternatif sesuai hipotesis H1

#2. Kasus Variansi Populasi tidak diketahui
#Input
x=
xbar=mean(x)
mu0=
S=sd(x)
n=length(x)
alpha=0.05

#Statistik Uji
t=(xbar-mu0)/(S/sqrt(n))
t.lower=qt(alpha,df=n-1)
t.upper=qt(1-alpha,df=n-1)
t.half.alpha=qt(1-alpha/2,df=n-1)
t.twosided=c(-t.half.alpha,t.half.alpha)

#P-Value
pval.lower=pt(t,df=n-1)
pval.upper=pt(t,df=n-1,lower.tail=FALSE)
pval.twosided=2*pt(t,df=n-1)

#Function
t.test(x,mu=mu0,alternative=c("two.sided","less","greater"),conf.level=0.95)
#pilih alternatif sesuai hipoesis H1


#DUA POPULASI
#1. Kasus variansi populasi 1 dan populasi 2 diketahui
#Input
x1=
x2=
xbar1=
xbar2=
mu0=
sigma1=
sigma2=
n1=
n2=
alpha=0.05

#Statistik uji 
xbar=xbar1-xbar2
z=(xbar-mu0)/sqrt((sigma1^2/n1)+(sigma2^2/n2))
z.lower=qnorm(alpha)
z.upper=qnorm(1-alpha)
z.half.alpha=qnorm(1-alpha/2)
z.twosided=c(-z.half.alpha,z.half.alpha)

#P-Value
pval.lower=pnorm(z)
pval.upper=pnorm(z, lower.tail=FALSE)
pval.twosided=2*pnorm(z)

#2.Kasus Variansi populasi 1 dan populasi 2 tidak diketahui
x1=
x2=
xbar1=
xbar2=
mu0=
S1=
S2=
n1=
n2=
alpha=0.05
xbar=xbar1-xbar2

#Statistik Uji
#Kedua variansi dianggap sama
df=n1+n2-2
Sp=(((n1-1)*S1^2)+((n2-1)*S2^2))/(df)
t=(xbar-mu0)/(sqrt(Sp)*(sqrt((1/n1)+(1/n2))))

#Kedua variansi dianggap berbeda
df=((S1^2/n1)+(S2^2/n2))^2/(((1/(n1-1))*(S1^2/n1)^2)+((1/(n2-1))*(S2^2/n2)^2))
t=(xbar-mu0)/(sqrt((S1^2/n1)+(S2^2/n2)))

t.lower=qt(alpha,df)
t.upper=qt(1-alpha,df)
t.half.alpha=qt(1-alpha/2,df)
t.twosided=c(-t.half.alpha,t.half.alpha)

#P-Value
pval.lower=pt(t,df)
pval.upper=pt(t,df,lower.tail=FALSE)
pval.twosided=2*pt(t,df)

#Function
t.test(x1,x2,mu=mu0,var.equal=c(TRUE,FALSE),alternative=c("two.sided","less","greater"),conf.level=0.95)
#pilih alternatif sesuai hipoesis H1

#3. Kasus Berpasangan
#Input
d=x1-x2
dbar=mean(d)
sd=sd(d)
n=length(d)
mu0=0
alpha=0.05

#Statistik uji
t=(dbar-mu0)/(sd/sqrt(n))
t.lower=qt(alpha,df=n-1)
t.upper=qt(1-alpha,df=n-1)
t.half.alpha=qt(1-alpha/2,df=n-1)
t.twosided=c(-t.half.alpha,t.half.alpha)

#P-Value
pval.lower=pt(t,df=n-1)
pval.upper=pt(t,df=n-1,lower.tail=FALSE)
pval.twosided=2*pt(t,df=n-1)

#Function
t.test(x1,x2,mu=0,paired=TRUE,alternative=c("two.sided","less","greater"),conf.level=0.95)


#Uji Hipotesis Variansi
x=
n=
S=
sigma0=
alpha=0.05

#Statistik Uji
chi=(n-1)*S^2/sigma0
chi.lower=qchisq(alpha,n-1)
chi.upper=qchisq(1-alpha,n-1)
chi.half.alpha=qchisq(1-alpha/2,n-1)
chi.twosided=c(-chi.half.alpha,chi.half.alpha)

#P-Value
pval.lower=pchisq(chi,n-1)
pval.upper=pchisq(chi,n-1,lower.tail=FALSE)
pval.twosided=2*pchisq(chi,n-1)

#Funnction
sigma.test(x,sigma=sigma0,sigmasq=sigma^2,alternative=c("two.sided","less","greater"),conf.level=0.95)

#Variansi dua Populasi
#Input
x1=
x2=
n1=
n2=
S1=
S2=
alpha=0.05

#Statistik uji
f=S1^2/S2^2
f.lower=qf(alpha,n1-1,n2-1)
f.upper=qf(1-alpha,n1-1,n2-1,lower.tail = FALSE)
f.half.alpha=qf(1-alpha/2,n1-1,n2-1)
f.twosided=c(-f.half.alpha,f.half.alpha)

#P Value
pval.lower=pf(f,n1-1,n2-1)
pval.upper=pf(f,n1-1,n2-1,lower.tail = FALSE)
pval.twosided=2*pf(f,n1-1,n2-1)

var.test(x1,x2,ratio=1,alternative=c("two.sided","less","greater"),conf.level=0.95)
