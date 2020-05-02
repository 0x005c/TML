open Exp
open Type

let infer e =
  match e with
  | Exp.Int _ -> Some Type.Int
  | _ -> None
;;
