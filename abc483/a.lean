def solve (n : Nat) : Nat :=
  let q := n / 2
  let r := n % 2
  if r = 0 then q else q + 1

def main : IO Unit := do
    let stdin ← IO.getStdin
    let n := (←stdin.getLine) |> String.trimRight |> String.toNat!
    let ans := solve n
    IO.println ans

#guard solve 3 = 2
#guard solve 6 = 3
#guard solve 59 = 30
