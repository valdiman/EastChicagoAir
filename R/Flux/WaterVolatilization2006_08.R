
#################### BEGIN OF FUNCTION DEFINITION ####################

Delta = function(a, MW.PCB, b, nOrtho.Cl, c)
{
	(a*MW.PCB-b*nOrtho.Cl+c)*1000
}

Hoff = function(K, DeltaUaw, R, T, T.water)
{
	K*exp(-DeltaUaw/R*(1/(T.water+273.15)-1/T))
}

Hoff2 = function(K, T.air, T.water)   
{
	K*T.water/T.air
}

diff.water = function(T.air, P)
{
	10^(-3)*1013.25*((273.15+T.air)^1.75*((1/28.97)+(1/18.0152))^(0.5))/P/(20.1^(1/3)+9.5^(1/3))^2

}

dens.water = function(T.water)
{
	(999.83952+16.945176*T.water-7.9870401*10^-3*T.water^2-46.170461*10^-6*3+105.56302*10^-9*T.water^4-280.54253*10^-12*T.water^5)/(1+16.87985*10^-3*T.water)
}


visc.water = function(T.water)
{
	10^(-4.5318-220.57/(149.39-(273.15+T.water)))
}

diff.co2 = function(T.water, R)
{
       0.05019*exp(-19.51*1000/(273.15+T.water)/R)
}

#################### END OF FUNCTION DEFINITION ######################

final.result = function(MW.PCB, H0.mean, H0.error, 
         C.PCB.water.mean, C.PCB.water.error, C.PCB.air.mean, C.PCB.air.error, nOrtho.Cl)
{
# fixed parameters

R = 8.3144
T = 298.15

F.PCB.aw = NULL
for (replication in 1:10000)
{

	# random parameters

	a = rnorm(1, 0.085, 0.007)
	b = rnorm(1, 1, 0.5)
	c = rnorm(1, 32.7, 1.6)
	H0 = 10^(rnorm(1, H0.mean, H0.error))	
	P = rnorm(1, 1018.767, 3.856223)					#sampling days
	u = 10^(rnorm(1, 0.40307316, 0.376360108))
	C.PCB.water = rnorm(1, C.PCB.water.mean, C.PCB.water.error)		
	C.PCB.air = rnorm(1, C.PCB.air.mean, C.PCB.air.error)
	T.water = rnorm(1, 18.71771, 0.3257)
	T.air = rnorm(1, 23.63229, 1.5928)


# computed values

DeltaUaw = Delta(a, MW.PCB, b, nOrtho.Cl, c)

K = H0*101325/(R*T)
K.air.water = Hoff(K, DeltaUaw, R, T, T.water)
K.final = Hoff2(K.air.water, T.water, T.air)

D.PCB.air = diff.water(T.air, P)*(MW.PCB/18.0152)^(-0.5)
V.water.air = 0.2*u +0.3
V.PCB.air = V.water.air*(D.PCB.air/diff.water(T.air, P))^(2/3)


v.water = visc.water(T.water)/dens.water(T.water)*10000
D.PCB.water = diff.co2(T.water, R)*(MW.PCB/44.0094)^(-0.5)
Sc.PCB.water = v.water/D.PCB.water
V.PCB.water = 0.001389*(Sc.PCB.water/600)^(-0.5)

F.PCB.aw = c(F.PCB.aw,100^2*((1/V.PCB.water+1/(V.PCB.air*K.final))^(-1)*(C.PCB.water-C.PCB.air/K.final/10^6))*3600*24/1000)
}

mmm = mean(F.PCB.aw)	#ng/m2/day
sss = sd(F.PCB.aw)	#ng/m2/day

c(mmm, sss, mmm-qnorm(1-.05/2)*sss, mmm+qnorm(1-0.05/2)*sss)
}

pars = read.csv("Data/Flux/Data20060800.csv")
Congener = pars$Congener
MW.PCB = pars$MW.PCB
H0.mean = pars$H0
H0.error = pars$H0.error
C.PCB.water.mean = pars$C.PCB.water
C.PCB.water.error = pars$C.PCB.water.error
C.PCB.air.mean = pars$C.PCB.air
C.PCB.air.error = pars$C.PCB.air.error
nOrtho.Cl = pars$nOrtho.Cl

Num.Congener = length(Congener)

result = NULL
for (i in 1:Num.Congener)
{
	result = rbind(result, final.result(MW.PCB[i], H0.mean[i], H0.error[i], 
         C.PCB.water.mean[i], C.PCB.water.error[i], C.PCB.air.mean[i], C.PCB.air.error[i], nOrtho.Cl[i]))
}

final.result = data.frame(Congener, result)
names(final.result) = c("Congener", "Mean", "Std", "Lower Bound", "Upper Bound")
write.csv(final.result, row.names=F, file="Data/csv/volatilization20060800.csv")





