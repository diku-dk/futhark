def tuple : (i32,bool,) = (1,true,)
def record : {x:i32,y:bool,} = {x=0,y=true,}
def array = [1,2,3,]
def index = (unflatten (iota (2*3)))[0,1,]
def attr = #[trail(a,b,)] true
def tpat (x,y,) = true
def rpat {x,y,} = true
