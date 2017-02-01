type id = string

datatype binop = Plus | Minus | Times | Div

datatype stm = CompoundStm of stm * stm
             | AssignStm of id * exp
             | PrintStm of exp list

     and exp = IdExp of id
             | NumExp of int
             | OpExp of exp * binop * exp
             | EseqExp of stm * exp

type table = (id * int) list

exception LookUp

fun lookup nil id =
     (print "Error: undefined id "; print id; print "\n";
      raise LookUp)
  | lookup ((n,v)::t) id =
       if n = id then v else lookup t id

fun update nil id v = [(id,v)]
  | update ((id',v')::t) id v =
       if id = id' then (id,v) :: t
       else (id',v') :: update t id v

val prog1 =
 CompoundStm(AssignStm("a",OpExp(NumExp 5, Plus, NumExp 3)),
  CompoundStm(AssignStm("b",
      EseqExp(PrintStm[IdExp"a",OpExp(IdExp"a", Minus,NumExp 1)],
           OpExp(NumExp 10, Times, IdExp"a"))),
   PrintStm[IdExp "b"]))

val prog2 = CompoundStm(AssignStm("a",OpExp(NumExp 5, Plus, NumExp 3)),
  CompoundStm(AssignStm("b",
      EseqExp(PrintStm[IdExp"a",EseqExp(PrintStm [IdExp "a",
                                                  IdExp "a",
                                                  IdExp "a"],
                                        OpExp(IdExp"a", Minus,NumExp 1))],
           OpExp(NumExp 10, Times, IdExp"a"))),
   PrintStm[IdExp "b"]))

val prog3 =
 CompoundStm(AssignStm("a",OpExp(NumExp 5, Plus, NumExp 3)),
  CompoundStm(AssignStm("b",
      EseqExp(PrintStm[IdExp"a",EseqExp(AssignStm("a",
                                                  OpExp(IdExp "a",
                                                        Times,
                                                        NumExp 5)),
                                        OpExp(IdExp"a", Minus,NumExp 1))],
           OpExp(NumExp 10, Times, IdExp"a"))),
   PrintStm[IdExp "b"]))

val prog4 =
  CompoundStm(
    AssignStm("a",OpExp(NumExp 5, Plus, NumExp 3)),
    CompoundStm(AssignStm("b",
                EseqExp(PrintStm[IdExp"a",
                                 EseqExp(PrintStm [IdExp "a",
                                                   IdExp "a",
                                                   IdExp "a"],
                                         OpExp(IdExp"a", Minus,NumExp 1)),
                                 EseqExp(PrintStm[NumExp 4,
                                                  NumExp 4,
                                                  NumExp 4,
                                                  NumExp 4
                                                 ],
                                         OpExp( IdExp "a", Minus, NumExp 3))],
                        OpExp(NumExp 10, Times, IdExp"a"))),
                PrintStm[IdExp "b"]))

fun max a b = if a > b then a else b

fun countlength nil = 0
  | countlength (hd::tl) = 1 + countlength tl

fun maxargs init_stm =
  let
    fun maxargs_stm (CompoundStm(stm1,stm2)) = max (maxargs_stm stm1) (maxargs_stm stm2)
      | maxargs_stm (AssignStm(id1, exp1)) = maxargs_exp exp1
      | maxargs_stm (PrintStm(explist1)) = max (maxargs_explist explist1) (countlength (explist1))
    and maxargs_exp (IdExp(id1)) = 0
      | maxargs_exp (NumExp(int1)) = 0
      | maxargs_exp (OpExp(exp1, binop1, exp2)) = max (maxargs_exp exp1) (maxargs_exp exp2)
      | maxargs_exp (EseqExp(stm1, exp1)) = max (maxargs_stm stm1) (maxargs_exp exp1)
    and maxargs_explist (nil) = 0
      | maxargs_explist (hd::tl) = max (maxargs_exp hd) (maxargs_explist tl)
  in
    maxargs_stm init_stm
  end
