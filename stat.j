NB.
NB.
NB.



NB. moving average
NB.
mavg =: (+/%#)\


NB. moving average shifted
NB. to match data length
NB.
fitted_mavg =: 4 : 0
	L =. ((x-1)#0)		NB. left padding
	R =. (x mavg y)
	L,R
)


NB. covariance
NB.
cov =: 4 : 0
	m=.(+/%#)x
	n=.(+/%#)y
	l=._1+#y
	(+/(x-m)*(y-n))%l
)


NB. standard deviation
NB.
stddev =: 3 : 0
	a=.(+/%#)y
	%:(+/%#)((y-a)^2)
)


NB. correlation factor
NB.
corr =: 4 : 0
	m=.x-(+/%#)x
	n=.y-(+/%#)y
	t=.+/(m*n)
	b=.(+/(m^2))*(+/(n^2))
	t%%:b
)


NB. polynomial coefficiants for data
NB.
coeff =: '(%. ^/~@x:@i.@#)'


NB. x polynomial coefficiants for data y
NB.
ncoeff =: 4 : 0
	 y %. (i.x) ^/~ i.# y
)

