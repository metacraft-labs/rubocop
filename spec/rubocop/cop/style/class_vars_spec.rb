# frozen_string_literal: true


RSpec.describe RuboCop::Cop::Style::ClassVars do
  subject(:cop) { described_class.new }

  it 'registers an offense for class variable declaration' do
    expect_offense(<<-RUBY.strip_indent)
      class TestClass; @@test = 10; end
                       ^^^^^^ Replace class var @@test with a class instance var.
    RUBY
  end

  it 'does not register an offense for class variable usage' do
    expect_no_offenses('@@test.test(20)')
  end
end

#suite "ClassVars":
#  var cop = ClassVars()
#
#  test "registers an offense for class variable declaration":
#    expectOffense(stripIndent"""
#      class TestClass; @@test = 10; end
#                       ^^^^^^ Replace class var @@test with a class instance var.
#    """)
  
#  test "does not register an offense for class variable usage":
#    expectNoOffenses("@@test.test(20)")


# parse the code, find the offenses & ast and just call all cops we have for those examples: cops and tests are translated
# later try to translate more infrastrucutre