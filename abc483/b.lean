def readListNat : IO (List Nat) := do
  let stdin ← IO.getStdin
  let line ← stdin.getLine
  return line.trimRight.splitOn |>.filterMap String.toNat?

def f (cards : List Int) (k : Nat) : List Int :=
  let (left, right) := cards.splitAt k
  (List.map (-·) (List.reverse left)) ++ right

def solve (n : Nat) (ks : List Nat) : List Int :=
  let init := List.map Int.ofNat (List.range' 1 n)
  ks.foldl f init

def main : IO Unit := do
  let nq ← readListNat
  let ks ← readListNat
  let n := nq[0]!
  let ans := solve n ks
  ans.forM IO.println

#guard solve 5 [3,5] = [-5, -4, 1, 2, 3]
#guard solve 1 [1] = [-1]
