signature ITEM =
sig
  type item
  val lt : item * item -> bool
  val eq : item * item -> bool
  val printval : item -> unit
end

signature BTREE =
sig
  structure ItemS : ITEM
  type btree
  val initTree : unit -> btree
  val insert : ItemS.item * btree -> btree
  val find : ItemS.item * btree -> bool
  val print : btree -> unit
end

functor BTree (
  structure Item : ITEM
  ) : BTREE =
struct
  structure ItemS = Item
  datatype btree = Empty
                 | Node of (ItemS.item) * (btree) * (btree)
  fun initTree _ = Empty

  fun insert (p, Empty) = Node(p, Empty, Empty)
    | insert (p, Node(v, ltree, rtree)) =
        if Item.lt(p, v)
          then Node(v, insert(p, ltree), rtree)
          else Node(v, ltree, insert(p, rtree))


  fun find (p, Empty)= false
    | find (p, Node(v, ltree, rtree)) =
        if Item.eq(p, v)
        then true
        else if Item.lt(p, v)
          then find(p, ltree)
          else find(p, rtree)

  (*fun print t = ()*)

  fun print (Empty) = ()
    | print (Node(v, ltree, rtree)) =
      let val _ = print(ltree)
          val _ = Item.printval(v)
          val _ = print(rtree)
      in () end
end

functor Item () : ITEM  =
struct
  type item = int * string
  fun lt ((aint, astring),(bint, bstring)) =
    aint < bint

  fun eq ((aint, astring),(bint, bstring)) =
    (aint = bint) andalso (astring = bstring)

  fun printval (aint, astring) =
    let val outputstr = "(" ^ Int.toString(aint) ^ "," ^ astring ^ ") "
    in print(outputstr) end
end

structure IItem = Item();
structure IBT = BTree(structure Item = IItem);
