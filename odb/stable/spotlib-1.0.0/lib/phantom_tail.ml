module List = struct
  let unsafe_list x = x
  let to_list v = v
  let to_unknown_list v = v
  let to_array = Array.of_list
  let to_unknown_array = Array.of_list
  let of_unknown_list v = v
  let of_unknown_array = Array.to_list
  let unsafe_of_list v = v
  let unsafe_of_array = Array.to_list
  
  let length = List.length 
  let map = List.map
  let combine = List.combine

  type ('phantom, 'content) t = ('phantom, 'content) ts
end

