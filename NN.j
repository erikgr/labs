NB. Neural Network. 
NB. No Backpropagation yet.
NB. (lol probably never)



NB. weights for current NN
w=:''
NB. biases for current NN
b=:''


NB. Sigmoid function
sigmoid =: 3 : 0
	e =. 1x1		NB. eulers number
	(e^y)%((e^y)+1)		NB. sigmoid function
)


NB. Creates new layer and
NB. cross connects all neurons
NB. in neighboring layers.
create_layer =: 3 : 0
	L	=. 0{y
	R	=. 1{y
	b	=: b,<(?R$10)
	w	=: w,<(?(L,R)$0)
)
	

NB. Initiates new neural network
NB. with the shape of Y, where
NB. each index corresponds to a
NB. layer of neurons, and each
NB number corresponds to the amount
NB of neurons in current layer
init =: 3 : 0
	w =:<(1,(0{y)$1)	NB. reset global weights
	b =:<(1,(0{y)$1)	NB. reset global biases
	2 (create_layer\) y
)


NB. dot product
dotp =: '+/ . *'


NB. weights for layer at index Y
W =: 3 : '>y{w'

NB. biases for layer at index Y
B =: 3 : '>y{b'


NB. some dumb input data
data =: 3 : '1 3 $ 1 2 3'


NB. Recursive activation function for layer Y.
A =: 3 : 0
	if. y=0 do.
		data''
	else.
		sigmoid"0 ((A(y-1)) +/ . * (W y))(+"1)(B y)
	end.	
)


NB. create NN
init 3 4 4 3
