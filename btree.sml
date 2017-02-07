signature ITEM =
sig
  type item
  val value : item
  val lt : item * item -> bool
  val eq : item * item -> bool
  val print : item -> unit
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
          val _ = Item.print(v)
          val _ = print(rtree)
      in () end
end
