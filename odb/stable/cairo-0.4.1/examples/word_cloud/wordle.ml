(* File: wordle.ml

   Copyright (C) 2010

     Christophe Troestler <Christophe.Troestler@umons.ac.be>
     WWW: http://math.umons.ac.be/an/software/

   This library is free software; you can redistribute it and/or modify
   it under the terms of the GNU Lesser General Public License version 3 or
   later as published by the Free Software Foundation, with the special
   exception on linking described in the file LICENSE.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
   LICENSE for more details. *)

(* According to Jonathan Feinberg, the creator of wordle, here's how
   Wordle actually works
   (http://stackoverflow.com/questions/342687/algorithm-to-implement-something-like-wordle):

   Count the words, throw away boring words, and sort by the count,
   descending. Keep the top N words for some N. Assign each word a
   font size proportional to its count.

   Each word "wants" to be somewhere, such as "at some random x
   position in the vertical center". In decreasing order of frequency,
   do this for each word:

   place the word where it wants to be
   while it intersects any of the previously placed words
   move it one step along an ever-increasing spiral

   That's it. The hard part is in doing the intersection-testing
   efficiently, for which I use last-hit caching, hierarchical
   bounding boxes, and a quadtree spatial index (all of which are
   things you can learn more about with some diligent googling).

   See also http://www.research.ibm.com/visual/papers/wordle_final2.pdf
*)
