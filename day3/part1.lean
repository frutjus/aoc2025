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

def List.from_to (n: Nat) (m: Nat): List Nat :=
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
  input.lines
  |> Functor.map λl =>
    l.toList.map (Char.toNat · - Char.toNat '0')

def solve1 (bank: List Nat) :=
  let tails := List.tails bank |> List.filter (List.length · > 1)
  let left_digit_and_rest := tails.maximumby (compareOn List.head!)
  let left_digit := left_digit_and_rest.head!
  let rest := left_digit_and_rest.tail!
  let right_digit := rest.maximum
  left_digit * 10 + right_digit

def solve (input: return_type_of parse) :=
  input.map solve1 |> List.sum


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

def day: System.FilePath := "day3"
#eval answer $ day/"sample.txt"
#eval answer $ day/"input.txt"
