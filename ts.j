NB. (_)_):::::::D ~~~ *
NB.

I	=. 0 0 0 0 20 0 0 0 15 15
O	=. 0 0 3 8 3 2 0 0 2 20
Z	=. 30

SAFE	=. 20
SPAN	=. 180
DATEFMT	=. 'YYYY-MM-DD'

A	=. 4 : '+/"1((i."0)x){y'
C	=. 3 : 'Z+(y A I)-(y A O)'
SIM	=. 3 : 'C 1+i.y'
UNSAFE	=. 3 : '(SIM y)<SAFE'
DATE	=. 3 : '(6!:0)DATEFMT'
RELDAYS	=. 3 : '365*((getdate y)tsdiff(getdate DATE 0))'
ADDAT	=. 4 : '(SPAN$x)*(y=i.SPAN)'
DAYS	=. 3 : 0
	out=.''
	for_d. y do.
		out=.out,0{LF splitstring shell'date -d "',(":d),' day" +"%Y-%m-%d"'
	end.
)

