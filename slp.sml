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
exception ErrorBinOp

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

fun interp init_stm =
  let
    fun interp_stm (CompoundStm(stm1, stm2)) init_table =
      let
        val table_stm1 = interp_stm stm1 init_table
      in
        interp_stm stm2 table_stm1
      end
      | interp_stm (AssignStm(id1, exp1)) init_table =
        let
          val (table_exp1, return_val) = interp_exp exp1 init_table
        in
          (id1, return_val) :: table_exp1
        end
      | interp_stm (PrintStm(nil)) init_table = init_table
      | interp_stm (PrintStm(hd::restexplist)) init_table =
        let
          val (table_exphd, return_val) = interp_exp hd init_table
        in
          interp_stm (PrintStm(restexplist)) table_exphd
        end
    and interp_exp (IdExp(id1)) init_table = (init_table, lookup init_table id1)
      | interp_exp (NumExp(int1)) init_table = (init_table, int1)
      | interp_exp (OpExp(exp1, binop1, exp2)) init_table =
        let
          val (table_exp1, val_exp1) = interp_exp exp1 init_table
        in
          let
            val (table_exp2, val_exp2) = interp_exp exp2 table_exp1
          in
            if binop1 = Plus then (table_exp2, val_exp1 + val_exp2)
            else if binop1 = Minus then (table_exp2, val_exp1 - val_exp2)
            else if binop1 = Times then (table_exp2, val_exp1 * val_exp2)
            else if binop1 = Div then (table_exp2, val_exp1 div val_exp2)
            else raise (print "Error: undefined Operator"; raise ErrorBinOp)
          end
        end
      | interp_exp (EseqExp(stm1, exp1)) init_table =
        let
          val table_stm1 = interp_stm stm1 init_table
        in
          interp_exp exp1 table_stm1
        end
  in
    interp_stm init_stm nil
  end
