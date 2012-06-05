class timed = object
  method! f = 1 (* lex bug "!" was PREFIXOP "!", not BANG *)
end
