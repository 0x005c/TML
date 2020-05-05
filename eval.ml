open Value

exception EvaluationError of Exp.exp ;;
exception UnexpectedError ;;

let get_i v =
  match v with
  | Int i -> i
  | _ -> raise UnexpectedError
;;

let get_f v =
  match v with
  | Float f -> f
  | _ -> raise UnexpectedError
;;

let get_b v =
  match v with
  | Bool b -> b
  | _ -> raise UnexpectedError
;;

let rec lookupE env s =
  match env with
  | [] -> raise (EvaluationError (Exp.Var s))
  | (t,v)::env' -> if String.equal s t then v
                   else lookupE env' s
;;

let rec eval env e =
  match e with
  | Exp.Int i -> Int i
  | Exp.Float f -> Float f
  | Exp.Bool b -> Bool b
  | Exp.Var s -> lookupE env s
  | Exp.Apply (e1,e2) -> apply env e1 e2
  | Exp.Not e -> Bool (not (get_b (eval env e)))
  | Exp.And (e1,e2) -> Bool (get_b (eval env e1) && get_b (eval env e2))
  | Exp.Or (e1,e2) -> Bool (get_b (eval env e1) || get_b (eval env e2))
  | Exp.IAdd (e1,e2) -> Int (get_i (eval env e1) + get_i (eval env e2))
  | Exp.ISub (e1,e2) -> Int (get_i (eval env e1) - get_i (eval env e2))
  | Exp.IMul (e1,e2) -> Int (get_i (eval env e1) * get_i (eval env e2))
  | Exp.IDiv (e1,e2) -> Int (get_i (eval env e1) / get_i (eval env e2))
  | Exp.FAdd (e1,e2) -> Float (get_f (eval env e1) +. get_f (eval env e2))
  | Exp.FSub (e1,e2) -> Float (get_f (eval env e1) -. get_f (eval env e2))
  | Exp.FMul (e1,e2) -> Float (get_f (eval env e1) *. get_f (eval env e2))
  | Exp.FDiv (e1,e2) -> Float (get_f (eval env e1) /. get_f (eval env e2))
  | Exp.Fun (s,e) -> Closure (s,env,e)
and apply env e1 e2 =
  match eval env e1 with
  | Closure (s,cenv,e) -> eval ((s,eval env e2)::cenv) e
  | _ -> raise (EvaluationError (Exp.Apply (e1,e2)))
;;
