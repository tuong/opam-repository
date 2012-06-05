* OCaml normal sequence expression in perform, escaped by '\'

  Sequence expressions [e1; e2] now can be written in perform more easily, by escaping [e1] and [e2] by [\]:

  Before:  
    perform
      ...;
      let () = prerr_endline "foobar" in (* BTW, use of 'let _ = ' here is dangerous. You may throw away non unit result! *)
      ...;

  
  Now:
    perform
      ...;
      \ prerr_endline "foobar"; (* Non unit expression is warned *)
      ...;

* Unit bind, [e;] without [<--] sign is more secure. It must have type [unit t] ([t] is the monad type), otherwise it is warned:
     
    module Option = struct
      module Open = sturct
        let bind x f = match x with
          | Some v -> f v
          | None -> None
        let return x = Some x
      end
    end
    open Option.Open

    perform
      ...;
      Some 1;  (* Warning S: this expression should have type unit. *)
      ...;

  To remove the warning, you must define a monad ignore function, which changes the monad content type to [unit]:

    module Option = struct
      module Open = sturct
        let bind x f = match x with
          | Some v -> f v
          | None -> None
        let return x = Some x
      end
      let ignore = function
        | Some v -> Some ()
	| None -> None
    end
    open Option.Open (* [Option.ignore] must not be exposed, so that it does not override the normal [ignore] *)

    perform
      ...;
      Option.ignore (Some 1);  (* The warning is gone. *)
      ...;
