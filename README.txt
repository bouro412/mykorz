This is the stub README.txt for the "mykorz" project.

TopExp := ( 'progn' {Exp} )
    | ( 'method' ( Ctxt ) id ( {Prm} ) {Exp} )
    | ( 'var' ( Ctxt ) id Exp? )
    | ( 'def' ( Ctxt ) id Exp )
    | ( 'if' Exp Exp Exp?  )
    | ( 'let' ({(id Exp)}) {Exp})
    | ( 'set' id Exp )
    | ( {Exp} Ctxt )
    | ID
    | immediate value
    ;

Exp := ( 'progn' {Exp} )
    | ( 'if' Exp Exp Exp?  )
    | ( 'let' ({(id Exp)}) {Exp})
    | ( 'set' id Exp )
    | ( {Exp} Ctxt )
    | ID
    | immediate value
    ;   

Prm := id
    | ( id Exp)
Ctxt := { Dim Exp }
Dim := keyword
id := symbol
immediate value := number
	         | string
                 | false
		 | true

method {rcvr <= a, degmart} main(){}
のような場合ctxtにdimのみが入っている
どう対応する?

-> (:rcvr a :degmart t)で対応する?

軸の名前で軸の中身が取り出せる

korz : x.y.z -> colz : ?

変数の文脈指定法

{-dim}

(car head) = head.car
(car :rcvr head)
(exp*) = slot-call?

引数と文脈の区別

((fib n) coord) = coord.fib(n)

()の意味に差が生じる?

(slot-name slot-arg* (:keyword coord)*) where is context?
dim is only keyword-symbol ?

cood:fib:asd = coord.fib.asd
