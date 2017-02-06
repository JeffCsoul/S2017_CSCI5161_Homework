signature IntMapSig =
sig
  type 'a map
  exception NotFound
  val apply : 'a map * int -> 'a
end;

signature ValSig =
sig
  type value
end;

signature SymSig =
sig
  eqtype sym
  val hash : sym -> int
end;

functor SymTblFct(
  structure IntMap : IntMapSig
  structure Val : ValSig
  structure Sym : SymSig) :

  sig
    type table
    exception Lookup
    val lookup : table * Sym.sym -> Val.value
    val update : table * Sym.sym * Val.value -> table
  end =

  struct
    datatype table = TBL of
    (Sym.sym * Val.value) list IntMap.map

    exception Lookup

    fun find (sym,[]) = raise Lookup
     | find (sym, (sym',v)::rest) =
          if sym = sym' then v
          else find (sym,rest)

    fun lookup (TBL map, s) =
          let val n = Sym.hash(s)
              val l = IntMap.apply(map,n)
          in find (s,l)
          end handle IntMap.NotFound => raise Lookup

    fun replace (sym,[],v) = raise Lookup
      | replace (sym, (sym', v')::rest, v) =
          if sym = sym' then (sym, v) :: rest
          else (sym', v') :: replace(sym, rest, v)

    fun update (TBL map, s, v) =
      let val n = Sym.hash(s)
          val l = IntMap.apply(map,n)
      in TBL (replace (s,l,v))
      end handle IntMap.NotFound => raise Lookup
  end
