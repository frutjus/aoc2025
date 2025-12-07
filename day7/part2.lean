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
def parse (input: String): Array (Array ((Nat × Nat) × Char)) :=
  input.lines.with_indices.toArray.map λ(l,r) =>
    l.toList.with_indices.toArray.map λ(v,c) =>
      ((r, c), v)

def solve (input: return_type_of parse) :=
  let timelines: Array (Array Nat) := input.map λrow => row.map λ((r,c),_) =>
    if r == input.size then
      1
    else if input[r+1]![c]!.snd == '^' then
      2
    else
      1
  let (start_row, start_col) := input[0]!.toList.dropWhile (·.snd != 'S')
    |> List.head!
    |> Prod.fst
  timelines

def answer (filepath: System.FilePath): IO Unit := do
  let start_time ← IO.monoMsNow
  IO.print "file: "
  IO.println filepath
  let raw ← IO.FS.readFile filepath
  let parsed := parse raw
  let solved := solve parsed
  IO.println ""
  IO.println solved
  let end_time ← IO.monoMsNow
  let time_delta := end_time - start_time
  IO.println s!"\ntime (ms) = {time_delta}"

def day: System.FilePath := "day7"
#eval answer $ day/"sample.txt"
--#eval answer $ day/"input.txt"
