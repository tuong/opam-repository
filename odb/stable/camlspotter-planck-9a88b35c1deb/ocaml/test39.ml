let _ = function  'A' | 'B' as c -> ()   (* CORRECT: <'A' | 'B'> as c *) (* WRONG: 'A' | <'B' as c> *)

