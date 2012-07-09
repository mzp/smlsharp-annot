(**
 * Pretty-printer library for Standard ML.
 * @author YAMATODANI Kiyoshi
 * @copyright 2010, Tohoku University.
 * @version $Id: SMLFormat.sml,v 1.4 2008/02/28 13:08:30 kiyoshiy Exp $
 *)
structure SMLFormat : SMLFORMAT = 
struct

  (***************************************************************************)

  structure FormatExpression = FormatExpression

  structure PrinterParameter = PrinterParameter

  structure BasicFormatters = BasicFormatters

  (***************************************************************************)

  datatype parameter = datatype PrinterParameter.parameter

  (***************************************************************************)

  exception Fail of string

  (***************************************************************************)

  val traceLevel = ref 0
  fun trace phase f arg =
      if !traceLevel = 0
      then f arg
      else
        (
          print ("[SMLFormat] begin " ^ phase ^ "\n");
          f arg
          before print ("[SMLFormat] end " ^ phase ^ "\n")
        )

  fun prettyPrint parameters expressions =
      let
        val parameter = PrinterParameter.convert parameters
      in
        (trace "pretty-print" (PrettyPrinter.format parameter)
         o trace "preprocess" (PreProcessor.preProcess parameter)
         o trace "assocResolve" (AssocResolver.resolve parameter)
         o trace "truncate" (Truncator.truncate parameter))
            (FormatExpression.Guard(NONE, expressions))
      end
        handle PreProcessor.Fail message =>
               raise Fail ("in preprocess:" ^ message)
             | PreProcessor.UnMatchEndOfIndent message =>
               raise Fail message
             | PrettyPrinter.UnMatchEndOfIndent =>
               raise Fail "unmatched EndOfIndent"
             | PrettyPrinter.IndentUnderFlow indent =>
               raise Fail ("indent underflow(" ^ Int.toString indent ^ ")")

  (***************************************************************************)

end;
