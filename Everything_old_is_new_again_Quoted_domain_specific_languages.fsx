//code-along to Philip Wadler - Everything is new again: Quoted domain specific languages - Curry On
//www.youtube.com/watch?v=FiflFiZ6pPI
//a lot of the code Mr. Wadler presents cannot be compiled, so I have done the corrections
//types are explicitly specified so that the code is easier to follow along the video
open Microsoft.FSharp.Quotations

//Part I
type Person = {
    name: string
    age: int
}

type DB = {
    db_name: string
    people : Person list
}

let db' = {
    db_name = "People"
    people = [
        { name = "Alex"; age = 40 }
        { name = "Bert"; age = 30 }
        { name = "Cora"; age = 35 }
        { name = "Drew"; age = 60 }
        { name = "Edna"; age = 25 }
        { name = "Fred"; age = 70 }
    ]
}

let youths' = seq {
    for u in db'.people do
        for v in db'.people do
            if u.name = "Alex" && v.age < u.age then
                yield v
}

youths' |> List.ofSeq

let db = <@ db' @>

let youths : Expr<Person seq> = <@
    seq {
        for u in (%db).people do
            for v in (%db).people do
                if u.name = "Alex" && v.age < u.age then
                    yield v
    }
@>
        
//Part II
type Names = string seq

let range : Expr<int * int -> Names> = <@
    fun(a,b) -> seq {
        for w in (%db).people do
            if a <= w.age && w.age < b then
                yield w.name
    }
@>

let run1 : Expr<Names> = (<@ (%range)(30,40) @>)

let satisfies : Expr<(int -> bool) -> Names> = <@
    fun p -> seq {
        for w in (%db).people do
            if p w.age then
                yield w.name
    }
@>

type Predicate =
    | Above of int
    | Below of int
    | And of Predicate * Predicate

let lift (x: 'a) : Expr<'a> = <@ x @>

let rec P (t: Predicate) : Expr<int -> bool> = 
    match t with
    | Above(a) -> <@ fun x -> (%lift(a)) <= x @>
    | Below(a) -> <@ fun x -> x < (%lift(a)) @>
    | And(t,u) -> <@ fun x -> (%P(t))(x) && (%P(u))(x) @>

let run2 : Expr<Names> = <@ (%satisfies) (%P(And(Above(30), Below(40)))) @>

//Part III
let rec P' (t: Predicate) (x: Expr<int>) : Expr<bool> =
    match t with
    | Above(a) -> <@ (%lift(a)) <= (%x) @>
    | Below(a) -> <@(%x) < (%lift(a)) @>
    | And(t,u) -> <@ (%P'(t)(x)) && (%P'(u)(x)) @>

//this guy doesn't work
let satisfies' (p: Expr<int> -> Expr<bool>) : Expr<Names> = <@
    seq {
        for w in (%db).people do
            if (%p(<@ w.age @>)) then // <- WHAT ARE YOU?, CRAZY!?.. attempting to use w inside another quotation
                yield w.name
    }
@>

//satisfies is a 'closed quotation' which is supported in F#
//satisfies' is an open quotation which is not supported in F#, but in other languages such as OCaml
