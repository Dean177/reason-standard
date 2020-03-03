type included_type = string

module ListExtensions = struct
  (** Let binding for {!map} *)
  let (let+) list f = List.map f list
end
