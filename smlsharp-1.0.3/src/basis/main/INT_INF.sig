include "INTEGER.sig"

signature INT_INF =
sig
  include INTEGER
  val divMod : int * int -> int * int
  val quotRem : int * int -> int * int
  val pow : int * SMLSharp.Int.int -> int
  val log2 : int -> SMLSharp.Int.int
  val orb : int * int -> int
  val xorb : int * int -> int
  val andb : int * int -> int
  val notb : int -> int
  val << : int * SMLSharp.Word.word -> int
  val ~>> : int * SMLSharp.Word.word -> int
end
