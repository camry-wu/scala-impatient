package sec16

import scala.xml._

// 1.

// 2.

// 3.

// 4.

// 5.

// 6.

// 7.

// 8.

// 9.

// 10.

object PracTest extends App {
    println ("sec16.PracTest")

    // 1.
    println("------------------------------  practice 1 -------------------------");
    // val a = <fred/>(0)       return a Node <fred/>
    // val b = <fred/>(0)(0)    return a Node <fred/>
    // a.equals(b): true
    // a == b: true

    // 2.
    println("------------------------------  practice 2 -------------------------");
    <ul>
        <li>Opening bracket: [</li>
        <li>Closing bracket: ]</li>
        <li>Opening bracket: {{</li>
        <li>Closing bracket: }}</li>
    </ul>
    // use {{ and }} replace { } in xml code

    // 3.
    println("------------------------------  practice 3 -------------------------");
    // <li>Fred</li> match {
    //     case <li>{Text(t)}</li> => t
    // }
    // this Fred is a Text("Fred")

    // <li>{"Fred"}</li> match {
    //     case <li>{Text(t)}</li> => t
    // }
    // this Fred is a Atom[String]("Fred")

    // 4.
    println("------------------------------  practice 4 -------------------------");
    val root4 = XML.loadFile("./temp/sec16/prac4.html")
    val imglist = root4 \\ "img"
    for (img <- imglist if img.attributes("alt") == null) {
        println(img)
    }

    // 5.
    println("------------------------------  practice 5 -------------------------");
    val imgsrclist = root4 \\ "img" \\ "@src"
    for (src <- imgsrclist) {
        println(src)
    }

    // 6.
    println("------------------------------  practice 6 -------------------------");
    val alist = root4 \\ "a"
    for (a <- alist) {
        print(a.attributes("href"))
        print("\t")
        println(a.child.text)
        println("===")
    }

    // 7.
    println("------------------------------  practice 7 -------------------------");
    import scala.collection.Map
    def map2dl(map: Map[String, String]): Elem = {
        val ret = <dl>{for ((k, v) <- map) yield <dt>{k}</dt><dd>{v}</dd>}</dl>
        ret
    }

    println(map2dl(Map("A" -> "1", "B" -> "2")))

    // 8.
    println("------------------------------  practice 8 -------------------------");
    def dl2map(dl: Elem): Map[String, String] = {
        val dtlist = (dl \ "dt").map(_.text)
        val ddlist = (dl \ "dd").map(_.text)
        val list = dtlist.zip(ddlist)

        list.toMap
    }

    println(dl2map(<dl><dt>A</dt><dd>1</dd><dt>B</dt><dd>2</dd></dl>))

    // 9.
    println("------------------------------  practice 9 -------------------------");
    import scala.xml.transform.RewriteRule
    import scala.xml.transform.RuleTransformer
    val rule1 = new RewriteRule {
        override def transform(n: Node) = n match {
            case e @ <img/> if (e.attributes("alt") == null) => e.asInstanceOf[Elem] % Attribute(null, "alt", "TODO", scala.xml.Null)
            case _ => n
        }
    }
    val root9 = new RuleTransformer(rule1).transform(root4)
    val imglist2 = root9 \\ "img"
    for (img <- imglist2) {
        println(img)
    }

    // 10.
    println("------------------------------  practice 10 -------------------------");
    def trans(src: String, dest: String) {
        val root = XML.loadFile(src)
        val root2 = new RuleTransformer(rule1).transform(root)

        XML.save(dest, root2.head, enc="UTF-8")
    }

    trans("./temp/sec16/prac4.html", "./temp/sec16/prac10.html")
    val root10 = XML.loadFile("./temp/sec16/prac10.html")
    val imglist10 = root10 \\ "img"
    for (img <- imglist10) {
        println(img)
    }
}
