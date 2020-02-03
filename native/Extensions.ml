module Option = struct
  include Standard.Option

  let (let*) = map
  let (let+) = both
end

module Result = struct
  include Standard.Option

  let (let*) = map
  let (let+) = both
end