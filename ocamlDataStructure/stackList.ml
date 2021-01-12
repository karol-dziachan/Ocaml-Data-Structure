module type STACK_MUT =
    sig
      type 'a t
      exception Empty of string
      val create: unit -> 'a t
      val push: 'a * 'a t -> unit
      val top: 'a t -> 'a
      val pop: 'a t -> unit
      val isEmpty: 'a t -> bool
    end;;
      
module StackMutList = 
struct
  type 'a t = { mutable l : 'a list }
  exception Empty of string
      let create() = {l = [] }
      let push(e, s) = s.l <- e :: s.l
      let top = function
        h::t -> h
        | [] -> raise (Empty "module StackMutList: top")
      let pop s = function
        h:: t -> s.l <- t 
        | [] -> raise (Empty "module StackMutList: pop")
      let isEmpty = function
        h::t -> true
        | [] -> false
      end;; 