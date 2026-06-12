import ProgressionCore
import Plausible
open ProgressionCore Plausible

-- Fixtures.
#guard (replay [.buyArt 1]).1.arts == [1]                          -- 200cr/15aff buys art 1
#guard (replay [.buyArt 2]).2 == [.refusedGate 2]                  -- aff 15 < 25
#guard (replay [.buyArt 1, .buyArt 1]).2.contains (.refusedDup 1)
#guard (replay [.grant 7, .sell 7 50]).1.credits == 250
#guard (replay [.sell 7 50]).2 == [.refusedNoItem 7]
#guard (replay ([.grant 7, .sell 7 100] ++ List.replicate 10 .train ++ [.buyArt 2])).1.arts == [2]
#guard (replay [.buyArt 1, .buyArt 3]).2.contains (.refusedGate 3)

def eventsOf (ns : List Nat) : List Event :=
  ns.map (fun n => match n % 5 with
    | 0 => .grant (UInt32.ofNat (n % 3 + 1))
    | 1 => .sell (UInt32.ofNat (n % 3 + 1)) (UInt32.ofNat (n % 90))
    | 2 => .buyArt (UInt32.ofNat (n % 3 + 1))
    | _ => .train)

/-- Arts are only learnable at or above their affinity requirement. -/
def gateHolds (ns : List Nat) : Bool :=
  let r := (eventsOf ns).foldl (fun (acc : Profile × Bool) e =>
    let (p', fx) := step acc.1 e
    let ok := fx.all (fun f => match f with
      | .learned a => acc.1.affinity ≥ artAffinityReq a && acc.1.credits ≥ artCost a
      | _ => true)
    (p', acc.2 && ok)) ({}, true)
  r.2

/-- Item counts stay positive and arts stay unique. -/
def invariants (ns : List Nat) : Bool :=
  let p := (replay (eventsOf ns)).1
  p.items.all (fun e => e.2 > 0) && p.arts.eraseDups == p.arts

/-- Credits only move by the priced amounts (no negative wrap). -/
def noWrap (ns : List Nat) : Bool :=
  let r := (eventsOf ns).foldl (fun (acc : Profile × Bool) e =>
    let (p', _) := step acc.1 e
    let ok := match e with
      | .buyArt _ => p'.credits ≤ acc.1.credits
      | .sell _ _ => p'.credits ≥ acc.1.credits
      | _ => true
    (p', acc.2 && ok)) ({}, true)
  r.2

#eval Testable.check (∀ ns : List Nat, gateHolds ns = true)
#eval Testable.check (∀ ns : List Nat, invariants ns = true)
#eval Testable.check (∀ ns : List Nat, noWrap ns = true)

def main : IO Unit := do
  IO.println "progression core: fixtures guarded, properties checked"
