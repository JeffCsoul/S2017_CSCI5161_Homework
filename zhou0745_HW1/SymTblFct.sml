signature IntMapSig =
sig
  type 'a map
  exception NotFound
  val apply : 'a map * int -> 'a
  val update : int * 'a * 'a map -> 'a map
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

signature SymTblSig =
sig
  structure ValS : ValSig
  structure SymS : SymSig
  type table
  exception Lookup
  val lookup : table * SymS.sym -> ValS.value
  val update : table * SymS.sym * ValS.value -> table
end

functor SymTblFct(
  structure IntMap : IntMapSig
  structure Val : ValSig
  structure Sym : SymSig) : SymTblSig =

  struct
    structure ValS = Val
    structure SymS = Sym
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

    fun update (TBL map, s, v) =
      let val n = Sym.hash(s)
          val l = IntMap.apply(map,n)
      in TBL (IntMap.update(n, (s, v)::l, map))
      end handle IntMap.NotFound => raise Lookup
  end

functor IntMapFct() : IntMapSig =
  struct
    type 'a map = (int * 'a) list
    exception NotFound

    fun apply([], intval) = raise NotFound
      | apply((i, a)::rest, intval) =
        if i = intval then a
        else apply(rest, intval)

    fun update(intval, corval, smap) =
      (intval, corval)::smap

  end

functor ValFct() : ValSig =
  struct
    type value = int
  end

functor SymFct() : SymSig =
  struct
    type sym = string

    fun gethashvalue "" = 0
      | gethashvalue astring =
        let val hdval = ord(String.sub(astring, 0))
            val reststr = String.extract(astring, 1, NONE)
        in  hdval + 256 * gethashvalue reststr end

    fun hash asym = gethashvalue asym
  end
