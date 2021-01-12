module type QUEUE_FUN =
sig
 type 'a queue
 exception Empty of string
 val empty: unit -> 'a queue
 val enqueue: 'a * 'a queue -> 'a queue
 val dequeue: 'a queue -> 'a queue
 val first: 'a queue -> 'a
 val isEmpty: 'a queue -> bool
end;; 
 
module MyQueue : QUEUE_FUN  = 
struct
  type 'a queue =  'a list * 'a list
  exception Empty of string
  let empty() = ([], [])
  let enqueue (e, q) = 
    match q with
      ([], _) -> ([e], [])
      | (fstList, sndList) -> (fstList, sndList @ [e]) 
  let dequeue = function 
      ([], _) -> ([], [])
      | (_::t, sndList) -> if t = [] then (List.rev sndList, [])
                          else (t, sndList)
  let first q = 
    match fst(q) with
      [] -> raise (Empty "module MyQueue: first")
    | h::t -> h
  let isEmpty q = fst(q) = []
end;;
 
let k = MyQueue.empty();;
MyQueue.isEmpty k;; 
let k = MyQueue.enqueue(1, k);;
let k = MyQueue.enqueue(2, k);;
let k = MyQueue.enqueue(3, k);;
let k = MyQueue.enqueue(4, k);;
let k = MyQueue.enqueue(5, k);;
MyQueue.isEmpty k;;
MyQueue.first k;;
let k = MyQueue.dequeue k;; 
MyQueue.first k;;
let k = MyQueue.dequeue k;; 
MyQueue.first k;;
let k = MyQueue.dequeue k;; 
MyQueue.first k;;