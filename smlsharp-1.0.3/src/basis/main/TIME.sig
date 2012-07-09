include "General.smi"
include "RealClass.smi"
include "Real.smi"
include "Real32.smi"
include "Int.smi"
include "IntInf.smi"
include "StringCvt.smi"

signature TIME =
sig
  eqtype time
  exception Time
  val zeroTime : time
  val fromReal : LargeReal.real -> time
  val toReal : time -> LargeReal.real
  val toSeconds : time -> LargeInt.int
  val toMilliseconds : time -> LargeInt.int
  val toMicroseconds : time -> LargeInt.int
  val toNanoseconds : time -> LargeInt.int
  val fromSeconds : LargeInt.int -> time
  val fromMilliseconds : LargeInt.int -> time
  val fromMicroseconds : LargeInt.int -> time
  val fromNanoseconds : LargeInt.int -> time
  val + : time * time -> time
  val - : time * time -> time
  val compare : time * time -> order
  val < : time * time -> bool
  val <= : time * time -> bool
  val > : time * time -> bool
  val >= : time * time -> bool
  val now : unit -> time
  val fmt : int -> time -> string
  val toString : time -> string
  val scan : (char, 'a) StringCvt.reader -> (time, 'a) StringCvt.reader
  val fromString : string -> time option
end
