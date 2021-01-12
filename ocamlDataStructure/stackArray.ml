module type  MY_STACK = 
sig
  type 'a t
  exception Empty of string
  val size : int
  val create : unit -> 'a t
  val increase : 'a t -> unit
  val push : 'a option * 'a t -> unit
  val top : 'a t -> 'a
  val pop : 'a t -> unit
  val isEmpty : 'a t -> bool
end;;


module MyStack : MY_STACK = 
struct 
type 'a t = {mutable n: int; mutable a: 'a option array;}
exception Empty of string
let size = 5
let increase_factor = 2
let create ()= {n = 0 ; a = Array.make size None}
let increase s = 
  let newSize = size * increase_factor  in
  let newArr = Array.make newSize None 
  in begin 
      for i = 0 to size-1 do 
        newArr.(i) <- s.a.(i)
       done;  
       size = newSize;
       s.a <- newArr
      end
let push(e, s) = 
begin
  if s.n = size then  increase s;
  s.a.(s.n) <- e; 
  s.n <- s.n + 1;
end

let top s = if s.n=0 then raise (Empty "module MyStack: top")
else match s.a.(s.n-1) with
    Some elem -> elem
    | None -> failwith  "module MyStack: top (implementation error!!!)"
let isEmpty s = s.n=0  
let pop s = if s.n = 0 then raise (Empty "module MyStack:pop")
      else s.n <- s.n-1

end;;

let mys = StackMutList.create();;

let testStack = MyStack.create();;
MyStack.isEmpty testStack;;
MyStack.push(Some(1), testStack);;
MyStack.push(Some(2), testStack);;
MyStack.push(Some(3), testStack);;
MyStack.push(Some(4), testStack);;
MyStack.isEmpty testStack;;
MyStack.top testStack;;
MyStack.pop testStack;;
MyStack.top testStack;;
MyStack.pop testStack;;
MyStack.top testStack;;
MyStack.pop testStack;;
MyStack.top testStack;;
MyStack.pop testStack;;
MyStack.isEmpty testStack;;