
suite "ClassVars",
  var cop = initClassVars()
  it("registers an offense for class variable declaration", proc (): void =
    expectOffense("      class TestClass; @@test = 10; end\n                       ^^^^^^ Replace class var @@test with a class instance var.\n".stripIndent))
  it("does not register an offense for class variable usage", proc (): void =
    expectNoOffenses("@@test.test(20)"))
