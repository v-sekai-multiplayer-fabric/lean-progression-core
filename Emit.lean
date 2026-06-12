import ProgressionCore
open ProgressionCore

def evStr : Event → String
  | .grant i => s!"grant {i}"
  | .sell i p => s!"sell {i} {p}"
  | .buyArt a => s!"buyArt {a}"
  | .train => "train"

def script : List Event :=
  [.grant 7, .grant 7, .sell 7 100, .buyArt 1]
    ++ List.replicate 10 .train
    ++ [.buyArt 2, .buyArt 3, .grant 2, .sell 9 50]

def main : IO Unit := do
  IO.FS.createDirAll "build"
  let (p, _) := replay script
  let items := String.intercalate "|" (p.items.map (fun e => s!"{e.1}:{e.2}"))
  let arts := String.intercalate "|" (p.arts.map toString)
  let lines := script.map evStr
  IO.FS.writeFile "build/progression_script.txt" (String.intercalate "\n" lines ++ "\n")
  IO.FS.writeFile "build/progression_golden.csv"
    s!"credits,affinity,items,arts\n{p.credits},{p.affinity},{items},{arts}\n"
  IO.println s!"final: credits={p.credits} affinity={p.affinity} items=[{items}] arts=[{arts}]"
