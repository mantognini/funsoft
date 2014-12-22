package fos

import helper.Loader

import org.scalatest._

class TypecheckerTest extends WordSpec with Matchers {
  private val loader = new Loader(this.info)
  private implicit val parser = loader.parseClass

  private var id = 0
  def testId(): Int = {
    id += 1
    id
  }

  // Check that input parses and typechecks
  def testPass(input: String, expected: Type.Class)(implicit parser: String => Tree) {
    s"accept solo class $expected [$testId]" in {
      CT.clear
      val typ = loader.load(input)
      info(s"expected: $expected")
      assert(typ == expected)
      info("🍺")
    }
  }

  def testPassTogether(inputs: List[String])(implicit parser: String => Tree) {
    s"accept class pack [$testId]" in {
      CT.clear
      loader.loadAll(inputs)
      info("🍺")
    }
  }

  // Test that the input parses but does *not* typecheck
  def testFail(input: String)(implicit parser: String => Tree) {
    s"reject bad code [$testId]" in {
      CT.clear
      val exception = intercept[TypeError] {
        val typ = loader.load(input)
        info(s"unexpected type: $typ")
      }
      info(s"excepted error: ${exception.msg}")
      info("🍺")
    }
  }

  def testLastFail(inputs: List[String])(implicit parser: String => Tree) {
    s"reject batch of bad code [$testId]" in {
      assert(inputs.size >= 1)
      CT.clear
      loader.loadAll(inputs dropRight 1) // Those that should succeed
      val exception = intercept[TypeError] {
        val typ = loader.load(inputs.last)
        info(s"unexpected type: $typ")
      }
      info(s"excepted error: ${exception.msg}")
      info("🍺")
    }
  }

  val pair = """
class Pair extends Object {
    Object fst;
    Object snd;

    Pair(Object fst, Object snd) {
        super();
        this.fst = fst;
        this.snd = snd;
    }

    Pair setfst(Object newfst) {
        return new Pair(newfst, this.snd);
    }
    
    Pair setsnd(Object newsnd) {
        return new Pair(this.fst, newsnd);
    }
}
"""

  val quad = """
class Quad extends Object {
    Pair p;
    Pair q;
    
    Quad(Pair p, Pair q) {
        super();
        this.p = p;
        this.q = q;
    }
    
    Quad set1(Object o) { return new Quad(this.p.setfst(o), this.q); }
    Quad set2(Object o) { return new Quad(this.p.setsnd(o), this.q); }
    Quad set3(Object o) { return new Quad(this.p, this.q.setfst(o)); }
    Quad set4(Object o) { return new Quad(this.p, this.q.setsnd(o)); }
}
"""

  val factory = """
class Factory extends Object {
    Factory() { super(); }
    
    Quad quad(Object a, Object b, Object c, Object d) {
        return new Quad(new Pair(a, b), new Pair(c, d));
    }
    
    Quad foo() {
        return new Factory()
            .quad(new Factory(), new Factory(), new Factory(), new Factory())
            .set1(new Pair(new Factory(), new Factory()));
    }
}
"""

  val myClass = "class MyClass extends Object { MyClass() { super(); } }"
  val mySubClass = "class MySubClass extends MyClass { MySubClass() { super(); } }"
  val mySubClass2 = "class MySubClass2 extends MyClass { Object o; MySubClass2(Object o) { super(); this.o = o; } }"
  val mySubSubClass = "class MySubSubClass extends MySubClass { Object p; MySubSubClass(Object p) { super(); this.p = p; } }"
  val mySubSubClass2 = "class MySubSubClass2 extends MySubClass2 { Object p; MySubSubClass2(Object o, Object p) { super(o); this.p = p; } }"

  val cassetête = """
class Foo extends Object {
    Foo() { super(); }
    
    Object fun() { return new Pair(new Factory(), new Factory()); }
    Pair gun() { return (Pair)(this.fun()); }
    Factory hun() { return (Factory)(this.gun().fst); }
    
    Pair purr(Object pair) { return (Pair)pair; }
    Pair knead() { return this.purr( (Object)(new Factory().foo().p) ); }
    Pair mew() { return this.purr( (Pair)(new Factory().foo()) ); }
}
"""

  "The typechecker" should {

    //** POSITIVE TESTS **//

    // Simple class
    testPass(myClass, "MyClass")

    // Simple inheritance
    testPassTogether(myClass :: mySubClass :: Nil)

    // Simple class with one field
    testPass("class MyClass extends Object { Object o; MyClass(Object o) { super(); this.o = o; } }", "MyClass")

    // Simple inheritance with one field in subclass
    testPassTogether(myClass :: mySubClass2 :: Nil)

    // Two level inheritance, with some fields
    testPassTogether(myClass :: mySubClass :: mySubSubClass :: Nil)
    testPassTogether(myClass :: mySubClass2 :: mySubSubClass2 :: Nil)
    testPassTogether(myClass :: mySubClass :: mySubClass2 :: mySubSubClass :: mySubSubClass2 :: Nil)

    // Test methods
    testPass(pair, "Pair")
    testPassTogether(pair :: quad :: factory :: Nil)
    // TODO add more tests

    // Test casts
    testPassTogether(pair :: quad :: factory :: cassetête :: Nil)

    //** NEGATIVE TESTS **//

    // Cyclic inheritance
    testFail("class MyClass extends MyClass { MyClass() { super(); } }")
    testLastFail(myClass :: mySubClass :: "class MyClass extends MySubClass { Bad() { super(); } }" :: Nil)

    // No parent in context
    testFail(mySubClass)
    testFail(mySubClass2)
    testFail(mySubSubClass)
    testFail(mySubSubClass2)
    testLastFail(myClass :: mySubSubClass :: Nil)
    testLastFail(myClass :: myClass :: Nil) // redef
    testLastFail(myClass :: mySubClass :: mySubSubClass2 :: Nil)

    // Field shadowing
    testLastFail(
      "class A extends Object { Object a; A(Object a) { super(); this.a = a; } }" ::
        "class B extends A { Object a; B(Object a) { super(a); this.a = a; } }" :: Nil)

    // Missing field / extra ctor parameter / arg vs field name mismatch / invalid or missing assignment / bad super call
    testFail("class MyClass extends Object { MyClass(Object o) { super(); } }")
    testFail("class MyClass extends Object { Object p; MyClass(Object o) { super(); } }")
    testFail("class MyClass extends Object { MyClass(Object o) { super(o); } }")
    testFail("class MyClass extends Object { MyClass(Object o) { super(); this.o = o; } }")
    testFail("class MyClass extends Object { Object o; MyClass(Object o) { super(o); } }")
    testFail("class MyClass extends Object { Object o; Object p; MyClass(Object o) { super(); this.o = o; } }")
    testFail("class MyClass extends Object { Object o; Object p; MyClass(Object o) { super(); this.p = p; } }")
    testFail("class MyClass extends Object { Object o; Object p; MyClass(Object o, Object p) { super(); this.p = p; } }")
    testFail("class MyClass extends Object { Object o; Object p; MyClass(Object o, Object p) { super(); this.p = p; this.o = o; } }")

    // TODO test methods
    testLastFail(pair :: """
class Foo extends Object {
        Pair pair;
        Foo(Pair pair) {
            super();
            this.pair = pair;
        }
        
        Pair fun() { 
            return this.pair.fst;
        }
}
""" :: Nil)
  }
}
