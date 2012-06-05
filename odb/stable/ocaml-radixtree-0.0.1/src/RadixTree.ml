
let rec compare_substr str1 pos1 str2 pos2 =
  match pos1 < String.length str1, pos2 < String.length str2 with
    | true, true -> 
        begin
          match Char.compare str1.[pos1] str2.[pos2] with
            | 0 -> compare_substr str1 (pos1 + 1) str2 (pos2 + 1)
            | n -> n 
        end

    | false, true ->
        -1

    | true, false ->
        1

    | false, false ->
        0

module PString =
struct 
  module MapChar = Map.Make(Char)

  type 'a t = 
    | Node of ('a t MapChar.t) * 'a list
    | Leaf of string * 'a list

  let empty = 
    Node (MapChar.empty, [])

  let add str e t =
    let len = String.length str in

    let node_of_leaf suff lst = 
      if suff = "" then
        Node (MapChar.empty, lst)
      else
        let c = suff.[0] in
        let child = Leaf (String.sub suff 1 ((String.length suff) - 1), lst) in
          Node (MapChar.add c child MapChar.empty, [])
    in

    let rec add' t pos =
      if pos < len then
        begin
          let c = str.[pos] in
            match t with 
              | Node (mp, lst) ->
                  begin
                    let child = 
                      try 
                        add' (MapChar.find c mp) (pos + 1)
                      with Not_found ->
                        let suff = String.sub str (pos + 1) (len - pos - 1) in
                          Leaf (suff, [e])
                    in
                      Node (MapChar.add c child mp, lst)
                  end

              | Leaf (suff, lst) ->
                  begin
                    if compare_substr str pos suff 0 = 0 then
                      Leaf (suff, e :: lst)
                    else
                      add' (node_of_leaf suff lst) pos
                  end
        end
      else
        begin
          match t with 
            | Node (mp, lst) ->
                Node (mp, e :: lst)
            | Leaf (suff, lst) ->
                if suff =  "" then
                  Leaf (suff, e :: lst)
                else 
                  add' (node_of_leaf suff lst) pos
        end
    in
      add' t 0

  let find str t =
    let len = String.length str in

    let rec find' t pos =
      if pos < len then
        begin
          match t with 
            | Node (mp, _) ->
                find' 
                  (MapChar.find str.[pos] mp)
                  (pos + 1)

            | Leaf (suff, hd :: tl) ->
                if compare_substr str pos suff 0 = 0 then
                  hd
                else
                  raise Not_found

            | Leaf (suff, []) ->
                raise Not_found
        end
      else
        begin
          match t with
            | Node (_, hd :: tl) | Leaf ("", hd :: tl) ->
                hd
            | Node (_, []) | Leaf (_, _) ->
                raise Not_found
        end
    in
      find' t 0

  let iter f t =
    let rec iter' k t = 
      match t with 
        | Node (mp, lst) ->
            List.iter (fun e -> f k e) lst;
            MapChar.iter 
              (fun c t ->
                 let k = k ^ (String.make 1 c) in
                   iter' k t)
              mp
        | Leaf (suff, lst) ->
            let k = k ^ suff in
              List.iter (fun e -> f k e) lst
    in
      iter' "" t


  let debug pp_elem t =
    let rec pp_list fmt =
      function
        | [] ->
            ()
        | [hd] ->
            pp_elem fmt hd
        | hd :: tl ->
            Format.fprintf fmt "%a,@ %a"
              pp_elem hd
              pp_list tl
    in

    let rec pp_t fmt =
      function
        | Node (mp, lst) ->
            Format.fprintf fmt "Node(@[<hv>{@[%a@]},@ [@[%a@]]@])"
              (fun fmt ->
                 MapChar.iter 
                   (fun c t ->
                      Format.fprintf fmt "@[%C -> %a@];@ "
                        c pp_t t)) mp
              pp_list lst

        | Leaf (suff, lst) ->
            Format.fprintf fmt "Leaf(@[%S,@ [@[%a@]]@])"
              suff
              pp_list lst
    in
    let fmt =
      Format.err_formatter
    in
      pp_t fmt t;
      Format.pp_print_newline fmt ();
      Format.pp_print_flush fmt ()

end

module IString = 
struct 

  type 'a child =
    | Node of 'a t
    | Leaf of string * 'a list ref
    | Nil 
  and 'a t = 
    {
      mutable lst: 'a list;
      children:    'a child array;
    }

  let create () = 
    {
      lst = [];
      children = Array.create 256 Nil;
    }

  let add t str v =
    let len = String.length str in

    let node_of_leaf suff rlst =
      if suff = "" then
        {(create ()) with lst = !rlst}
      else
        let code = Char.code suff.[0] in
        let child = Leaf (String.sub suff 1 ((String.length suff) - 1), rlst) in
        let t = create () in
          t.children.(code) <- child;
          t
    in

    let rec add' t pos =
      if pos < len then
        begin
          let code = Char.code str.[pos] in
            match t.children.(code) with
              | Node t ->
                  add' t (pos + 1)

              | Leaf (suff, rlst) ->
                  if compare_substr str pos suff 0 = 0 then
                    begin
                      rlst := v :: !rlst 
                    end
                  else
                    begin
                      let t' = node_of_leaf suff rlst in
                        add' t' (pos + 1);
                        t.children.(code) <- Node t'
                    end

              | Nil ->
                  begin
                    let suff = String.sub str (pos + 1) (len - pos - 1) in
                    let t' = Leaf (suff, ref [v]) in
                      t.children.(code) <- t'
                  end
        end
      else
        begin
          t.lst <- v :: t.lst
        end
    in
      add' t 0

  let find t str = 
    let len = String.length str in

    let rec find' t pos =
      if pos < len then
        begin
          let code = Char.code str.[pos] in
            match t.children.(code) with 
              | Node t ->
                  find' t (pos + 1)

              | Leaf (suff, rlst) ->
                  begin
                    match !rlst with 
                      | hd :: tl ->
                          if compare_substr str (pos + 1) suff 0 = 0 then
                            hd
                          else
                            raise Not_found
                      | [] ->
                          raise Not_found
                  end

              | Nil ->
                  raise Not_found 
        end
      else
        begin
          match t.lst with
            | hd :: tl ->
                hd
            | [] ->
                raise Not_found
        end
    in

      find' t 0


  let iter f t =
    let rec iter' k t = 
      List.iter (fun v -> f k v) t.lst;
      Array.iteri 
        (fun code child ->
           let k = k^(String.make 1 (Char.chr code)) in
             match child with 
               | Node t ->
                   iter' k t
               | Leaf (suff, rlst) ->
                   let k = k ^ suff in
                     List.iter (fun v -> f k v) !rlst

               | Nil -> 
                   ())
        t.children
    in
      iter' "" t
end
