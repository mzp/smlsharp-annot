(*
"sharing" structure specification.
It is a derived form "sharing type" specification.

<ul>
  <li>the number of strid connected
    <ul>
      <li>2</li>
      <li>3</li>
    </ul>
  </li>
  <li>the number of type names common among the connected structures
    <ul>
      <li>1</li>
      <li>2</li>
    </ul>
  </li>
</ul>
*)
signature S21 = 
sig
  structure T1 : sig type t1 end
  structure T2 : sig type t1 end
  sharing T1 = T2
end;

signature S32 = 
sig
  structure T1 : sig type t1 datatype t2 = D end
  structure T2 : sig type t1 datatype t2 = D end
  structure T3 : sig type t1 datatype t2 = D end
  sharing T1 = T2 = T3
end;
