signature ITEM =
sig
  type item
  val value : item
  val lt : item * item -> bool
  val eq : item * item -> bool
end

signature BTREE =
sig
  type item
  type btree
  val initTree : unit -> btree
  val insert : item * btree -> btree
  val find : item * btree -> bool
  val print : btree -> unit
end

functor BTree (
  structure Item : ITEM
  ) : BTREE =
struct
  type item = Item.item
  datatype btree = Empty
                 | Node of (item) * (btree) * (btree)

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

  fun print t = ()
end
