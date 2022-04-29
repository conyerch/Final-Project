val typeCheck : Typ.t Env.t -> Ast1.program -> unit

exception TypeError of string
