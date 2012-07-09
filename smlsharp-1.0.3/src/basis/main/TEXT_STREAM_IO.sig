include "CharVector.smi"
include "Substring.smi"
include "STREAM_IO.sig"

signature TEXT_STREAM_IO =
sig
  include STREAM_IO
    where type vector = CharVector.vector
    where type elem = SMLSharp.Char.char
  val inputLine : instream -> (string * instream) option
  val outputSubstr : outstream * substring -> unit
end
