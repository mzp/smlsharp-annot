signature S = sig type  ('a, 'b) reader = 'b -> ('a * 'b) end;
structure S : S = struct type ('a, 'b) reader = 'b -> ('a * 'b) end;
