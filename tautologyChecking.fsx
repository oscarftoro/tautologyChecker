#r @"./tautologyChecker.exe"
open TautologyChecker.TC
open System

module Checking =
  let rich    = Atom "rich"
  let landed  = Atom "landed" 
  let saintly = Atom "saintly"

  //we assume that if landed then rich
  let assumption1 = implies (landed, rich)
  //we assum ¬(saintly ∧ rich)
  let assumption2 = Neg (Conj(saintly, rich))
  //a plausible conclusion is: landed → ¬(saintly)
  //the landed are not sainlty
  let concl = Disj (Neg (Atom "landed"),
                    Neg (Atom "saintly"))
  //if conclusion follows from the assumptions,
  //then the following proposition is a tautology
  //(landed → rich) ∧ ¬(saintly ∧ rich)) →
  //(landed → ¬ saintly)
  let goal = implies (Conj(assumption1,
                           assumption2),concl)
  
  let pq =  (Conj(rich,saintly), Conj(landed, Neg rich))
[<EntryPoint>]
let main argv =

  printfn "wellcome to entry point!"
  printfn "our goal..."
  let res = show Checking.goal
  printfn "%s" res

  
  printfn "assumption 2 in NNF:"
  printfn "%s" ((nnf >> show) Checking.assumption2)
  
  printfn "test distrib ((rich ∧ saintly) , (landed ∧ ¬rich))"

  printfn "%s" ((distrib >> show) Checking.pq)
  
  0
