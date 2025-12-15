import Std.Data.HashMap
import Std.Data.HashSet

-- general-purpose stuff
def String.lines (str: String): List String :=
  str.replace "\r" ""
  |> (String.splitOn · "\n")

def String.split_pair (str: String) (del: String): String × String :=
  match str.splitOn del with
    | hd :: rest => (hd, String.intercalate del rest)
    | _ => ("","")

def List.scan (f: α → β → α) (init: α) (l: List β): List α :=
  l.foldl (λ
    | s::ss, c => (f s c)::s::ss
    | _, _ => panic! "unexpected") [init]
  |> List.reverse

abbrev return_type_of (_: α → β) := β

instance: Monad List where
  pure a := [a]
  bind a f := a.flatMap f

instance: Alternative List where
  failure := []
  orElse l l' := l ++ (l' ())

def List.from_to (n m: Nat): List Nat :=
  (· + n) <$> List.range (m - n + 1)

def String.repeat (str: String) (n: Nat): String :=
  List.range n
  |> Functor.mapConst str
  |> join

def List.local_dedup [BEq α]: List α → List α
  | [] => []
  | first :: rest =>
    rest.foldl (λ(out, prev) curr =>
      if curr == prev
        then (out, prev)
        else (curr :: out, curr)
    ) ([first], first)
    |> Prod.fst
    |> List.reverse

def List.dedup [BEq α] (l: List α): List α :=
  l.foldl (λout curr =>
    if out.contains curr
      then out
      else curr :: out
  ) []
  |> List.reverse

def List.tails (l: List α): List (List α) :=
  l.scan (λs _ => s.tail) l

def List.maximum [Max α] [Inhabited α]  (l: List α): α :=
  l.foldl max (l.head!)

def List.maximumby [Inhabited α] (f: α → α → Ordering) (l: List α): α :=
  l.foldl (λs c =>
    match f s c with
      | .lt => c
      | _ => s
    ) (l.head!)

def List.last [Inhabited α]: List α → α
  | [] => panic! "a proof of non-emptiness would be nice right now"
  | [x] => x
  | _ :: rest => rest.last

partial def List.transpose: List (List α) → List (List α)
  | outerlist => Id.run do
    let mut heads := []
    let mut tails := []
    for innerlist in outerlist do
      match innerlist with
        | [] => return []
        | hd :: tl =>
          heads := hd :: heads
          tails := tl :: tails
    return heads.reverse :: tails.reverse.transpose

def List.with_indices (l: List α): List (α × Nat) :=
  l.zip (List.range l.length)

-- puzzle-specific stuff
abbrev Point := (Int × Int)
def parse (input: String) :=
  input.lines.map (λl =>
    match l.splitOn "," with
      | [x,y] => (x.toInt!, y.toInt!)
      | _ => panic! "station")

def area: Point → Point -> Int
  | (x1,y1), (x2, y2) => (x1 - x2 + 1) * (y1 - y2 + 1)

def rect_contains_point: Point → Point → Point → Bool
  | (x1,y1), (x2, y2), (x3, y3) =>
      min x1 x2 < x3 ∧ x3 < max x1 x2
    ∧ min y1 y2 < y3 ∧ y3 < max y1 y2

abbrev Parms := Unit
def solve (input: return_type_of parse) (_: Parms) :=
  let pairs := do
    let ts ← input.tails
    guard (ts ≠ [])
    if let tile1 :: rest := ts then do
      let tile2 ← rest
      pure (tile1, tile2)
    else panic! "this'll never happen"
  let sorted := pairs.mergeSort (λ(a,b) (c,d) => area a b ≥ area c d)
  let eligible := sorted.filter (λ(a,b) => ¬(input.any (rect_contains_point a b)))
  eligible

def answer (filepath: System.FilePath) (parms: Parms): IO Unit := do
  let start_time ← IO.monoMsNow
  IO.print "file: "
  IO.println filepath
  let raw ← IO.FS.readFile filepath
  let parsed := parse raw
  let solved := solve parsed parms
  IO.println ""
  IO.println solved
  let end_time ← IO.monoMsNow
  let time_delta := end_time - start_time
  IO.println s!"\ntime (ms) = {time_delta}"

--solution isn't correct for part 2
def day: System.FilePath := "day9"
#eval answer (day/"sample.txt") ()
#eval answer (day/"input.txt") ()
