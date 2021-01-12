module type DICTIONARY = 
    sig 
      type 'b key  
      type 'a dict
      exception DuplicatedKey of string
        exception FailureKey of string 
      val empty: unit -> 'a dict 
      val insert: 'a dict -> 'b key * 'a-> 'a dict
      val delete: 'a dict -> 'b key -> 'a dict
      val update: 'a dict -> 'b key -> 'b key * 'a -> 'a dict
      val search: 'a dict -> 'b key -> 'b key * 'a
    end;; 

  

module Dict  =  
      struct
        type 'b key = 'b  
        type 'a dict = ('a key * 'a) list
        exception DuplicatedKey of string
        exception FailureKey of string 
        let empty() = [] 
        let insert dict (key, value) = 
          let rec helper = function
          h :: t -> if fst(h) = key then raise (DuplicatedKey "module Dict: insert duplicate")
           else if fst(h) < key && (t = []  || fst(List.hd t) > key) then  h :: (key, value) :: t 
            else h :: helper t
          | [] -> [(key, value)]
        in helper(dict) 

        let rec drop n xs=
          if n<0 then xs
          else
            match (n, xs) with
              (0, h::t) -> t 
             |(_, []) -> xs
             |(n, h::t) -> h :: drop (n-1) t;;

        let delete dict key=
          let rec helper (actual, n) = 
            match actual with 
              h::t -> if fst(h) = key then  drop n dict 
                      else helper (t, n+1)
            |  [] -> raise (FailureKey "module Dict: delete fault key")
          in helper (dict, 0)

          let search dict key =
            let rec helper = function
              h::t -> if fst(h) = key then h
                      else helper t
              | [] -> raise (FailureKey "module Dict: search fault key")
            in helper dict

        let update dict key (newKey, newValue)= 
          let rec helper = function 
            h::t -> if fst(h) = key then (newKey, newValue) :: t
                    else h :: helper t 
            | [] -> raise (FailureKey "module Dict: update fault key") 
          in helper dict          
        end;;
      
let dicti = Dict.empty();;
let dicti = Dict.insert dicti (1, "Karol");;
let dicti = Dict.insert dicti (2, "Weronika");;
let dicti = Dict.insert dicti (3, "Fabian");;
let dicti = Dict.insert dicti (4, "Aneta");;
let dicti = Dict.insert dicti (6, "Tomasz");;
let dicti = Dict.insert dicti (5, "Tester");;
let dicti = Dict.insert dicti (9, "Nina");;
let dicti = Dict.insert dicti (10, "Oliwia");;
let dicti = Dict.insert dicti (8, "Malgorzata");;
let dicti = Dict.insert dicti (7, "Agata");;

let dicti = Dict.delete dicti 3;;
let dicti = Dict.delete dicti 7;;

Dict.search dicti 9;;

let dicti = Dict.update dicti 8 (11, "Karolina");;
Dict.search dicti 11;;