open Value

exception EvaluationError ;;

let runtime_error () =
  Printf.eprintf "Evaluation failed";
  exit 1
;;

let get_i v =
  match v with
  | Int i -> i
  | _ -> runtime_error ()
;;

let get_f v =
  match v with
  | Float f -> f
  | _ -> runtime_error ()
;;

let get_b v =
  match v with
  | Bool b -> b
  | _ -> runtime_error ()
;;

let rec lookupE env s =
  match env with
  | [] -> runtime_error ()
  | (t,v)::env' -> if String.equal s t then v
                   else lookupE env' s
;;

let eq v1 v2 =
  match (v1,v2) with
  | (Int v1,Int v2) -> v1==v2
  | (Float v1,Float v2) -> v1==v2
  | (Bool v1,Bool v2) -> v1==v2
  | _ -> false
;;

let rec eval env e =
  match e with
  | Exp.Int i -> Int i
  | Exp.Float f -> Float f
  | Exp.Bool b -> Bool b
  | Exp.Var s -> unlazy env (lookupE env s)
  | Exp.Apply (e1,e2) -> apply env e1 e2
  | Exp.Not e -> Bool (not (get_b (eval env e)))
  | Exp.If (e1,e2,e3) -> if (get_b (eval env e1)) then eval env e2
                                                  else eval env e3
  | Exp.Let (s,e1,e2) -> eval ((s,eval env e1)::env) e2
  | Exp.LetRec (s,e1,e2) -> eval ((s,LazyExp e1)::env) e2
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
  | Exp.Eq (e1,e2) -> Bool (eq (eval env e1) (eval env e2))
  | Exp.Fun (s,e) -> Closure (s,env,e)
  | Exp.Annot (e,_) -> eval env e
and apply env e1 e2 =
  match eval env e1 with
  | Closure (s,cenv,e) -> eval ((s,eval env e2)::cenv) e
  | _ -> runtime_error ()
and unlazy env v =
  match v with
  | LazyExp e -> eval env e
  | _ -> v
;;
