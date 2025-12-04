-- general-purpose stuff
def String.lines (str: String): List String :=
  str.replace "\r" ""
  |> (String.splitOn · "\n")

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

-- puzzle-specific stuff
def parse (input: String) :=
  input.lines.toArray.map λl =>
    l.toList.toArray

def solve (input: return_type_of parse) :=
  let populated := input.map (Functor.map (match · with | '@' => 1 | _ => 0))
  let index := λi j arr => (Array.getD arr i #[]).getD j 0
  Id.run do
    let mut out := 0
    for hi: i in [0:populated.size] do
      for hj: j in [0:populated[i].size] do
        if populated[i][j] == 0 then continue else
        let mut neighbours := 0
        for neighbour_i in [(i-1):(i+2)] do
          for neighbour_j in [(j-1):(j+2)] do
            if neighbour_i == i ∧ neighbour_j == j then continue else
            let neighbour := index neighbour_i neighbour_j populated
            neighbours := neighbours + neighbour
        if neighbours < 4 then out := out + 1 else continue
    return out

def answer (filepath: System.FilePath): IO Unit := do
  IO.println "file:"
  IO.println filepath
  let raw ← IO.FS.readFile filepath
  let parsed := parse raw
  IO.println "\nparse:"
  IO.println parsed
  let solved := solve parsed
  IO.println "\nsolution:"
  IO.println solved

def day: System.FilePath := "day4"
#eval answer $ day/"sample.txt"
#eval answer $ day/"input.txt"
