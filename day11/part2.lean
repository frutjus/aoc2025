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

def List.minimum [Min α] [Inhabited α]  (l: List α): α :=
  l.foldl min (l.head!)

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

def List.head_and_tail [Inhabited α] (l: List α): α × List α :=
  match l with
    | x :: xs => (x, xs)
    | _ => panic! "it wasn't supposed to end this way"

def List.init_and_last [Inhabited α] (l: List α): List α × α :=
  let rec aux := λl l' =>
    match l with
      | x :: [x'] => ((x::l').reverse, x')
      | x :: xs => aux xs (x::l')
      | _ => panic! "it wasn't supposed to end this way"
  aux l []

def String.lipo (str: String): String := (str.drop 1).dropRight 1

-- puzzle-specific stuff
def parse (input: String) :=
  input.lines.map (λl =>
    match l.splitOn " " with
      | raw_device :: outputs => (raw_device.dropRight 1, outputs)
      | _ => panic! "whoops")
  |> Std.HashMap.ofList

partial def find_routes
  (device: String)
  (connections: Std.HashMap String (List String))
  (passed_fft: Bool)
  (passed_dac: Bool):
  StateM (Std.HashMap String Nat) Nat := do
  if device == "out" then
    if passed_fft ∧ passed_dac then
      return 1
    else
      return 0
  let mut total_routes := 0
  let passed_fft' := passed_fft || device == "fft"
  let passed_dac' := passed_dac || device == "dac"
  for output in connections[device]! do
    total_routes := total_routes + (← find_routes output connections passed_fft' passed_dac')
  return total_routes

abbrev Parms := Unit
def solve (input: return_type_of parse) (_: Parms) :=
  (find_routes "svr" input false false).run ∅
  |> Prod.fst

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

def day: System.FilePath := "day11"
#eval answer (day/"sample2.txt") ()
-- solution takes too long for part 2
--#eval answer (day/"input.txt") ()
