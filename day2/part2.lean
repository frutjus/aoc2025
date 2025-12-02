-- general-purpose stuff
def String.lines (str : String) : List String :=
  str.replace "\r" ""
  |> (String.splitOn · "\n")

def List.scan (f: α → β → α) (init: α) (l: List β) : List α :=
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

-- puzzle-specific stuff
def parse (input : String) :=
  input.splitOn ","
  |> Functor.map λstr =>
    match str.splitOn "-" with
      | range_start :: range_end :: [] => (range_start, range_end)
      | _ => panic! "unexpected"

def solve (input : return_type_of parse) := (do
  let (raw_rng_start, raw_rng_end) ← input
  -- I guess we'll use the same logic as part 1, but we'll try a bunch of sequence lengths
  -- first we limit our search to ranges of numbers with a divisible number of digits
  let start_digits := raw_rng_start.length
  let end_digits := raw_rng_end.length
  let num_digits ← List.from_to start_digits end_digits
  let seq_len ← List.from_to 2 num_digits
  guard (num_digits % seq_len == 0)
  -- now that we have subdivided the range, figure out the start and end of the sub-range
  let rng_start := if start_digits == num_digits then raw_rng_start else toString (10 ^ (num_digits - 1))
  let rng_end := if end_digits == num_digits then raw_rng_end else toString (10 ^ num_digits - 1)
  -- take the first part of the number
  let start_part := rng_start.take (num_digits / seq_len)
  let end_part := rng_end.take (num_digits / seq_len)
  -- and iterate over all the distinct variations of the first half
  let front_part ← toString <$> List.from_to start_part.toNat! end_part.toNat!
  -- for a given front part, we can simply check if the repeated number
  -- is contained in the current range
  let repeated := (front_part.repeat seq_len).toNat!
  guard (rng_start.toNat! <= repeated ∧ repeated <= rng_end.toNat!)
  pure repeated
  )
  |> List.dedup
  |> List.sum


def answer (filepath : System.FilePath) : IO Unit := do
  IO.println "file:"
  IO.println filepath
  let raw ← IO.FS.readFile filepath
  let parsed := parse raw
  IO.println "\nparse:"
  IO.println parsed
  let solved := solve parsed
  IO.println "\nsolution:"
  IO.println solved

#eval answer "day2/sample.txt"
#eval answer "day2/input.txt"
