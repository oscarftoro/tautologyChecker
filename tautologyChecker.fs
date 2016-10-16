namespace TautologyChecker
// as seen in ML for the working programmer by L.C. Paulson
module TC =
  let x = 42
  type prop = Atom  of string
            | Neg   of prop
            | Conj  of prop * prop
            | Disj  of prop * prop
            
  //implication p → q is equivalent to (~p) ∧ q
  let implies(p,q) = Disj(Neg p,q)

  //show with enconded precedence: ¬, ∧ , ∨
  let  show arg : string =
    let rec showHlpr precedence arg =
      match arg with
        | Atom a    -> a
        | Neg p     -> "~" + showHlpr 3 p 
        | Conj(p,q) ->
          if precedence > 2 then
            "(" + showHlpr precedence p + " & " + showHlpr precedence q +  ")"
          else showHlpr 2 p + " & " + showHlpr  2 p
          | Disj(p,q) ->
            if precedence > 1 then
              "(" + showHlpr precedence p + " | " + showHlpr precedence  q +  ")"
            else showHlpr 1 p  + " | " + showHlpr 1 q;
    in showHlpr 0 arg


  //evaluate propositions
  let rec evalProp trues arg =
    match arg with
      | (Atom a)    -> List.exists (fun elem -> elem = a) trues
      | (Neg p)     -> not (evalProp trues p)
      | (Conj(p,q)) -> evalProp trues p && evalProp trues q
      | (Disj(p,q)) -> evalProp trues p  || evalProp trues q
    
  //negation normal form (rewrite rules). repeatedly replace:
  //¬¬p by p
  //¬(p ∧ q) by (¬p) ∨ (¬q)
  //¬(p ∨ q) by (¬p) ∧ (¬q)
  let rec nnf arg   =
    match arg with
      | (Atom a)             -> Atom a
      | (Neg (Atom a))       -> Neg (Atom a)
      | (Neg (Neg p))        -> nnf p
      | (Neg (Conj (p,q)))   -> nnf (Disj(Neg p, Neg q))
      | (Neg (Disj (p,q)))   -> nnf (Conj (Neg p, Neg q))
      | (Conj(p,q))          -> Conj(nnf p, nnf q)
      | (Disj(p,q))          -> Disj(nnf p, nnf q);
