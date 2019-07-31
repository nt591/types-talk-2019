(* let add2 x y = x +. y

let test = add2 2. *)


(* let sum_list lst = List.fold_left (+) 0 lst *)

(* let sum_list_again nums_to_add =
  let rec inner_function initial_val list = match list with
    | h::t -> inner_function (initial_val + h) t
    | [] -> initial_val
  in inner_function 0 nums_to_add *)



(* let rec map fn list = match list with
  | [] -> []
  | h::t -> fn h :: map fn t *)



(* Variant Types (Algebraic Data Types) *)
type role = Frontend | Backend | Operations
type office_location = Baltimore | San_Francisco

type employee = {
  name: string;
  title: string;
  role: role;
  office: office_location option
}


let employee_works_hard emp = match emp.name with
  | "Russ" -> false
  | _ -> true

let employee_has_snacks employee = match employee.office with
  | Some x -> true
  | None -> false




(* RECURSIVE *)
type 'a tree
  = Node of 'a tree * 'a * 'a tree
  | Leaf

let rec mem x tree = match tree with
  | Leaf -> false
  | Node (left, y, right) -> y = x || mem x left || mem x right



(* type http_response = (int * string)

let log_response resp = match resp with
  | (status, err) when status >= 400 -> "Error: " ^ err
  | (_, msg) -> msg *)



(* BEFORE *)
(* type chatroom_message = {
  status_code: int;
  body: string option;
  error: string option;
  user_id: int option;
  chatroom_id: int option;
  timestamp: int;
}

type message = Chatroom of chatroom_message *)


(* AFTER *)
type user_message = {
  status_code: int;
  body: string;
  user_id: int;
  chatroom_id: int;
  timestamp: int;
}

type guest_message = {
  status_code: int;
  body: string;
  chatroom_id: int;
  timestamp: int;
}

type error = {
  status_code: int;
  chatroom_id: int;
  timestamp: int;
  error: string
}

type message
  = UserMessage of user_message
  | GuestMessage of guest_message
  | Error of error

let log_response_again = function
  | UserMessage {status_code; body; user_id} -> "User " ^ (string_of_int user_id) ^ " - " ^ body
  | GuestMessage {body} -> "Guest - " ^ body
  | Error {status_code; error; chatroom_id} when status_code >= 500 -> "Internal Server Error: " ^ error
  | Error {status_code; error; chatroom_id} -> "Error: " ^ error ^ " - chatroom - " ^ (string_of_int chatroom_id)



type shape =
  | Circle of float
  | Rect of float * float
  | Triangle of float * float

let calculate_area shape = match shape with
  | Circle radius -> radius *. radius *. 3.1415
  | Rect (width, height) -> width *. height
  | Triangle (base, height) -> 0.5 *. base *. height
