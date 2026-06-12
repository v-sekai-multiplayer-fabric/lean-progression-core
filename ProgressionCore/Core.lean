namespace ProgressionCore

/-- Art definitions: id, credit cost, affinity requirement. -/
def artCost : UInt32 → UInt32
  | 1 => 100 | 2 => 250 | _ => 500
def artAffinityReq : UInt32 → UInt32
  | 1 => 10 | 2 => 25 | _ => 40

/-- The profile: a pure value the reducer threads (the `commit_sink` payload). -/
structure Profile where
  credits  : UInt32 := 200
  affinity : UInt32 := 15
  items    : List (UInt32 × UInt32) := []   -- (item id, count)
  arts     : List UInt32 := []
  deriving DecidableEq, Repr, Inhabited

inductive Event
  | grant (item : UInt32)               -- a loot drop lands (inventory_delta)
  | sell (item : UInt32) (price : UInt32)
  | buyArt (art : UInt32)               -- the shop, behind the affinity gate
  | train                               -- affinity rises by one
  deriving DecidableEq, Repr

inductive Effect
  | granted (item : UInt32)
  | sold (item : UInt32) (price : UInt32)
  | learned (art : UInt32)
  | refusedPoor (art : UInt32)          -- not enough credits
  | refusedGate (art : UInt32)          -- affinity below the requirement
  | refusedDup (art : UInt32)
  | refusedNoItem (item : UInt32)
  | trained (to : UInt32)
  deriving DecidableEq, Repr

def countOf (p : Profile) (item : UInt32) : UInt32 :=
  ((p.items.find? (fun e => e.1 == item)).map Prod.snd).getD 0

def addItem (p : Profile) (item : UInt32) (d : UInt32) : Profile :=
  if p.items.any (fun e => e.1 == item) then
    { p with items := p.items.map (fun e => if e.1 == item then (e.1, e.2 + d) else e) }
  else { p with items := p.items ++ [(item, d)] }

def removeItem (p : Profile) (item : UInt32) : Profile :=
  { p with items := (p.items.map (fun e =>
      if e.1 == item then (e.1, e.2 - 1) else e)).filter (fun e => e.2 > 0) }

/-- The pure reducer over the profile (valid transitions only). -/
def step (p : Profile) : Event → Profile × List Effect
  | .grant item => (addItem p item 1, [.granted item])
  | .sell item price =>
    if countOf p item == 0 then (p, [.refusedNoItem item])
    else (removeItem { p with credits := p.credits + price } item, [.sold item price])
  | .buyArt art =>
    if p.arts.contains art then (p, [.refusedDup art])
    else if p.affinity < artAffinityReq art then (p, [.refusedGate art])
    else if p.credits < artCost art then (p, [.refusedPoor art])
    else ({ p with credits := p.credits - artCost art, arts := p.arts ++ [art] }, [.learned art])
  | .train =>
    let a := p.affinity + 1
    ({ p with affinity := a }, [.trained a])

def replay (events : List Event) : Profile × List Effect :=
  events.foldl (fun acc e =>
    let (p, fx) := step acc.1 e
    (p, acc.2 ++ fx)) ({}, [])

end ProgressionCore
