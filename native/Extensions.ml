include Standard_shared

module Option = struct
  include StandardShared.Option

  let (let*) = map
  let (let+) = both
end