module type QUEUE_FUN= 
    sig
      type 'a queue
      exception Empty of string
      val empty: unit -> 'a queue
      val enqueue: 'a * 'a list -> 'a queue
      val isEmpty: 'a queue -> bool
      val dequeue:  'a queue -> 'a queue
      val first: 'a queue -> 'a
    end;; 

module Queue : QUEUE_FUN =
    struct
      type 'a queue = 'a list
      exception Empty of string
      let empty() = [];;
      let enqueue(e, q) = q @ [e]
      let isEmpty = function
        h::t -> false
        | [] -> true
      let dequeue = function
        h::t -> t
        | [] -> raise (Empty "module Queue': dequeue")
      let first = function
        h::t -> h
        | [] -> raise (Empty "module Queue': first")
    end;;

let q = Queue.empty();; 
Queue.isEmpty q;;  
let q = Queue.enqueue(1, k);; 
let q = Queue.enqueue(2, k);; 
let q = Queue.enqueue(3, k);; 
let q = Queue.enqueue(4, k);; 
let q = Queue.enqueue(5, k);; 
Queue.isEmpty q;; 


Queue.first  q;;
let q = Queue.dequeue q;; 
Queue.first q;;