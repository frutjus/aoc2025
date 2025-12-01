-- general-purpose stuff
def String.lines (str : String) : List String :=
  str.replace "\r" ""
  |> (String.splitOn · "\n")

def List.scan (f: α → β → α) (init: α) (l: List β) : List α :=
  l.foldl (λ
    | s::ss, c => (f s c)::s::ss
    | _, _ => panic! "unexpected") [init]
  |> List.reverse

-- puzzle-specific stuff
def parse (input : String) :=
  input.lines.map λl =>
    let dir := match l.take 1 with
      | "L" => -1
      | "R" => 1
      | _ => panic! "unexpected"
    let dist := l.drop 1 |> String.toInt!
    dist * dir

def rotate (state: (Int × Int)) (move: Int) : (Int × Int) :=
  let (dial, zero_passes) := state
  let dial' := (dial + move) % 100
  let zero_passes' := zero_passes +
    if move >= 0
      then (dial + move) / 100
      else ((100 - dial) % 100 - move) / 100
  (dial', zero_passes')

def solve (input : List Int) :=
  input.foldl rotate (50, 0)
  |> Prod.snd

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

#eval answer "day1/sample.txt"
#eval answer "day1/input.txt"
