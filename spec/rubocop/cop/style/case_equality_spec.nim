
import
  types

cop :
  suite "CaseEquality":
    var cop = initClassVars()
    it("registers an offense for ===", proc (): void =
      expectOffense("(str \"      Array === var\\n\")".stripIndent))
