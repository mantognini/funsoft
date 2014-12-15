package fos

import scala.util.parsing.combinator.syntactical.StandardTokenParsers
import scala.util.parsing.input._

import scala.collection.mutable.HashMap

/**
 * This object implements a parser for Featherweight Java
 *  check <a href="http://citeseer.ist.psu.edu/igarashi99featherweight.html">
 *  the paper</a>.
 */
object FJ extends StandardTokenParsers {

  lexical.delimiters ++= List(".", "{", "}", ",", "=", ";", "(", ")")
  lexical.reserved ++= List("class", "extends", "super", "return", "new")

  /**
   * <pre>
   *  Program     ::= { ClassDef } Expr
   * </pre>
   */
  def Prog: Parser[Tree] = positioned(
    rep(ClsDef) ~ Expr ^^ { case classes ~ main => Program(classes, main) })

  /**
   * <pre>
   *  ClassDef ::= "class" C extends C "{"
   *                   { FieldDef } ConstructorDef  { MethodDef }
   *               "}"
   *  </pre>
   */
  def ClsDef: Parser[ClassDef] = positioned(
    "class" ~ ident ~ "extends" ~ ident ~ "{" ~
      rep(FldDef) ~ CtorDef ~ rep(MethDef) ~
      "}"
      ^^ { case "class" ~ name ~ "extends" ~ superc ~ "{" ~ fields ~ ctor ~ methods ~ "}" => ClassDef(name, superc, fields, ctor, methods) }
      | failure("illegal start of class definition"))

  /**
   * <pre>
   *  FieldDef       ::= ident ident ";"
   *  </pre>
   */
  def FldDef: Parser[FieldDef] = positioned(
    ident ~ ident <~ ";" ^^ { case tpe ~ name => FieldDef(tpe, name) }
      | failure("illegal start of field definition"))

  /**
   * <pre>
   *  ConstructorDef ::= ident "(" ArgList ")" "{"
   *                       "super" "(" Arguments ")" ";"
   *                       { ident "." ident "=" ident }
   *                     "}"
   *  </pre>
   */
  def CtorDef: Parser[CtrDef] = positioned(
    ident ~ "(" ~ ParamList ~ ")" ~ "{" ~
      "super" ~ "(" ~ VarList ~ ")" ~ ";" ~
      rep(Init) ~
      "}" ^^ { case name ~ "(" ~ params ~ ")" ~ "{" ~ "super" ~ "(" ~ supArgs ~ ")" ~ ";" ~ seq ~ "}" => CtrDef(name, params, supArgs, seq) }
      | failure("illegal start of constructor"))

  /**
   * Initializer ::= ident "." ident "=" ident ";"
   */
  def Init: Parser[Assign] = positioned(
    ident ~ "." ~ ident ~ "=" ~ ident <~ ";" ^^ { case obj ~ "." ~ field ~ "=" ~ value => Assign(obj, field, Var(value)) })

  /**
   * <pre>
   *  ParamList ::= ident ident { "," ident ident }
   *            | empty
   *  </pre>
   */
  def ParamList: Parser[List[FieldDef]] =
    repsep(ident ~ ident ^^ { case tpe ~ name => FieldDef(tpe, name) }, ",")

  /**
   * <pre>
   *  VarList ::= ident ident { "," ident ident }
   *            | epsilon
   *  </pre>
   */
  def VarList: Parser[List[Var]] =
    repsep(ident ^^ { Var(_) }, ",")

  /**
   * <pre>
   *  MethodDef ::= ident ident "(" Params ")" "{" "return" Expr ";" "}"
   *  </pre>
   */
  def MethDef: Parser[MethodDef] = positioned(
    ident ~ ident ~ "(" ~ ParamList ~ ")" ~ "{" ~ "return" ~ Expr ~ ";" ~ "}"
      ^^ { case tpe ~ name ~ "(" ~ params ~ ")" ~ "{" ~ "return" ~ body ~ ";" ~ "}" => MethodDef(tpe, name, params, body) }
      | failure("illegal start of method definition"))

  /**
   * <pre>
   *  Expressions ::= [Expr { "," Expr } ]
   *  </pre>
   */
  def Expressions: Parser[List[Expr]] =
    repsep(Expr, ",")

  /**
   * <pre>
   *  Expr ::= SimpleExpr {"." ident ["(" Expressions ")"]}
   *  </pre>
   */
  def Expr: Parser[Expr] = positioned(
    SimpleExpr ~ rep("." ~> ident ~ opt("(" ~> Expressions <~ ")")) ^^ { case se ~ xs => mkExpression(se, xs) })

  /**
   * <pre>
   *  SimpleExpr ::= ident
   *               | "new" C "(" Expressions ")"
   *               | "(" C ")" Expr
   *               | "(" Expr ")"
   *  </pre>
   */
  def SimpleExpr: Parser[Expr] = positioned(
    ident ^^ { x => Var(x) }
      | "new" ~ ident ~ "(" ~ Expressions ~ ")" ^^ { case "new" ~ name ~ "(" ~ args ~ ")" => New(name, args) }
      | "(" ~ ident ~ ")" ~ Expr ^^ { case "(" ~ tpe ~ ")" ~ obj => Cast(tpe, obj) }
      | "(" ~> Expr <~ ")"
      | failure("illegal start of simple expression"))

  type Output = Tree

  def mkExpression(obj: Expr, rest: List[~[String, Option[List[Expr]]]]) = {
    var t: Expr = obj
    def buildPath(xs: List[~[String, Option[List[Expr]]]]): Unit = xs match {
      case meth ~ Some(args) :: rest =>
        t = Apply(t, meth, args)
        buildPath(rest)
      case field ~ None :: rest =>
        t = Select(t, field)
        buildPath(rest)
      case Nil => ()
    }
    buildPath(rest)
    t
  }

  import Type._

  def eval(t: Tree): Tree = {
    //    PrettyPrinter(t)
    t match {
      case Program(cds, expr) =>
        try {
          cds foreach (cl => typeOf(cl, Nil))
          val typeExpr = typeOf(expr, Nil)
          println("TYPE EXPR: " + typeExpr)
          val evExpr = Evaluate(expr)
          print("EVALUATE TO: ")
          evExpr
        } catch {
          case TypeError(msg) =>
            println("Type Error: " + msg)
            print("The expression will not be evaluated. Expr: ")
            CT.clear
            expr
          case EvaluationException(msg) =>
            println("The expression generate an exception in Java: " + msg)
            CT.clear
            expr

          case e: Throwable =>
            println(e)
            CT.clear
            expr
        }
      case _ =>
        println("The file must start with a class definition")
        System.exit(-1)
        null
    }
  }

  import java.io._

  def main(args: Array[String]): Unit = {
    val inputStream = if (args.length > 0) new FileInputStream(args(0)) else System.in
    val tokens = new lexical.Scanner(StreamReader(new InputStreamReader(inputStream)))
    phrase(Prog)(tokens) match {
      case Success(trees, _) =>
        print(eval(trees))

      case e =>
        println(e)
    }
  }

}
