(**
 * wrapper of libdl.
 * @author YAMATODANI Kiyoshi
 * @author UENO Katsuhiro
 *)

structure DynamicLink :> sig

  type lib
  datatype scope = LOCAL | GLOBAL
  datatype mode = LAZY | NOW

  val dlopen : string -> lib
  val dlopen' : string * scope * mode -> lib
  val dlsym : lib * string -> unit ptr
  val dlclose : lib -> unit

end =
struct

  infix 6 +
  infix 4 < =
  infix 3 :=
  val op + = SMLSharp.Int.add
  val op < = SMLSharp.Int.lt

  type lib = unit ptr
  datatype scope = LOCAL | GLOBAL
  datatype mode = LAZY | NOW

  val c_dlopen =
      _import "dlopen"
      : __attribute__((no_callback))
        (string, int) -> lib
  val c_dlsym =
      _import "dlsym"
      : __attribute__((no_callback))
        (lib, string) -> unit ptr
  val c_dlerror =
      _import "dlerror"
      : __attribute__((no_callback))
        () -> char ptr
  val c_dlclose =
      _import "dlclose"
      : __attribute__((no_callback))
        lib -> int

  local
    val loaded = ref false
    val RTLD_LAZY = ref 0
    val RTLD_NOW = ref 0
    val RTLD_LOCAL = ref 0
    val RTLD_GLOBAL = ref 0
  in
    fun dlopenMode (scope, mode) =
      let
        val _ =
            if !loaded then () else
            (RTLD_LAZY := SMLSharpRuntime.cconstInt "RTLD_LAZY";
             RTLD_NOW := SMLSharpRuntime.cconstInt "RTLD_NOW";
             RTLD_LOCAL := SMLSharpRuntime.cconstInt "RTLD_LOCAL";
             RTLD_GLOBAL := SMLSharpRuntime.cconstInt "RTLD_GLOBAL";
             loaded := true)
        val scope = case scope of LOCAL => !RTLD_LOCAL | GLOBAL => !RTLD_GLOBAL
        val mode = case mode of LAZY => !RTLD_LAZY | NOW => !RTLD_NOW
      in
        scope + mode
      end
  end (* local *)

  fun dlerror () =
      SMLSharpRuntime.str_new (c_dlerror ())

  fun dlopen' (libname, scope, mode) =
      let
        val lib = c_dlopen (libname, dlopenMode (scope, mode))
      in
        if lib = _NULL
        then raise SMLSharpRuntime.SysErr (dlerror (), NONE)
        else lib
      end

  fun dlopen libname =
      dlopen' (libname, LOCAL, LAZY)

  fun dlclose lib =
      if c_dlclose lib < 0
      then raise SMLSharpRuntime.SysErr (dlerror (), NONE)
      else ()

  fun dlsym (lib, symbol) =
      let
        val ptr = c_dlsym (lib, symbol)
      in
        if lib = _NULL
        then raise SMLSharpRuntime.SysErr (dlerror (), NONE)
        else ptr
      end

end
