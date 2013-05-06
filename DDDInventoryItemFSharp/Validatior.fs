module Validator

open FSharpx.Collections
open FSharpx.Choice
open FSharpx.Validation

let validator predicate error x =
    if predicate x then Choice1Of2 x
    else Choice2Of2 (NonEmptyList.singleton error)

let (==) = LanguagePrimitives.PhysicalEquality
let inline (!=) a b = not (a == b)
let notNull e = validator ((!=) null) e
let notEmptyString e = validator (fun (s:string) -> s.Length > 0) e

/// Given a value, creates a choice 1. (Applicative functor)
let puree = returnM

/// Composes a choice type with a non choice type.
let inline (<?>) a b = lift2 (fun _ z -> z) a (Choice1Of2 b)

/// Composes a non-choice type with a choice type.
let inline (|?>) a b = lift2 (fun z _ -> z) (Choice1Of2 a) b