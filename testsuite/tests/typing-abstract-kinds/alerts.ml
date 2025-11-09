(* TEST *)

module Kind_with_alert = struct
  kind_ k [@@alert kind_alert "foo"]
end

module Alert_on_use_in_manifest = struct
  kind_ k = Kind_with_alert.k
end

module Alert_on_use_in_annotation = struct
  type t : Kind_with_alert.k
end

(* CJC XXX this is broken *)
