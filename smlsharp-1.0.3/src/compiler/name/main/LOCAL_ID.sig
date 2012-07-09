signature LOCAL_ID =
sig
  eqtype id
  val compare : id * id -> order
  val eq : id * id -> bool
  val generate : unit -> id
  val format_id : id -> SMLFormat.FormatExpression.expression list
  val toString : id -> string
  val toInt : id -> int
  structure Map : ENV sharing type id = Map.key
  structure Set : ORD_SET sharing type id = Set.Key.ord_key
end
