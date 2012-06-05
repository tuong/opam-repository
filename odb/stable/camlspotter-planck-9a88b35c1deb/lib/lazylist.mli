(** This is a lazy list basically, but its elements and nil can have extra information. *)

type ('elem, 'attr) zlist = ('elem, 'attr) desc lazy_t
and ('elem, 'attr) desc = 
  | Cons of 'elem * 'attr * ('elem, 'attr) zlist  (** cons of element with attribute *)
  | Null of 'attr (** null with attribute *)

(** Constructor functions *)
val null : 'attr -> ('elem, 'attr) zlist
val null_desc : 'attr -> ('elem, 'attr) desc
val cons_desc : 'elem -> 'attr -> ('elem, 'attr) zlist -> ('elem, 'attr) desc

(** Destructors *)
val desc : ('elem, 'attr) zlist -> ('elem, 'attr) desc
val peek : ('elem, 'attr) zlist -> ('elem * 'attr * ('elem, 'attr) zlist) option
val is_null : ('elem, 'attr) zlist -> bool
val attr : ('elem, 'attr) zlist -> 'attr
(** Attribute of the head element/null *)

val to_list : ('elem, 'attr) zlist -> 'elem list
val to_list_with_attrs : ('elem, 'attr) zlist -> ('elem * 'attr) list
(** Conversions to eager list. The attribute at Null is thrown away. *)

val iter : ('elem option -> 'attr -> 'c) -> ('elem, 'attr) zlist -> 'c
val fold_right : ('elem option -> 'attr -> 'c -> 'c) -> ('elem, 'attr) zlist -> 'c -> 'c
val map : ('elem -> 'attr -> 'c * 'd) -> ('attr -> 'd) -> ('elem, 'attr) zlist -> ('c, 'd) zlist

val rev_between : ('elem, 'attr) zlist -> ('elem, 'attr) zlist -> ('elem * 'attr) list
val between : ('elem, 'attr) zlist -> ('elem, 'attr) zlist -> ('elem * 'attr) list
