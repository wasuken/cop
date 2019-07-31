object Element{
  def elem(contents: Array[String]): Element =
    new ArrayElement(contents)
  def elem(chr: Char, width: Int, height: Int): Element =
    new UniformElement(chr, width, height)
  def elem(line: String): Element =
    new LineElement(line)
}
abstract class Element{
  def contents: Array[String]
  def height: Int = contents.length
  def width: Int = contents(0).length()
  def above(that: Element): Element = {
    val this1 = this width that.width
    val that1 = that width this.width
    Element.elem(this1.contents ++ that1.contents)
  }
  def beside(that: Element): Element = {
    val this1 = this heighten that.height
    val that1 = that heighten this.height
    Element.elem(
      for(
        (line1, line2) <- this.contents zip that1.contents
      ) yield line1 + line2
    )
  }
  def width(w: Int): Element =
    if (w <= width) this
    else {
      val left = Element.elem(' ', (w - width) / 2, height)
      val right = Element.elem(' ', w - width - left.width, height)
      left beside this beside right
    }
  def heighten(h: Int): Element =
    if (h <= height) this
    else {
      val top = Element.elem(' ', width, (h - height) / 2)
      val bot = Element.elem(' ', width, h - height - top.height)
      top above this above bot
    }
  override def toString(): String = contents mkString "\n"
}

class ArrayElement(val x123: Array[String]) extends Element{
  val contents: Array[String] = x123
}

class LineElement(s: String) extends ArrayElement(Array(s)){
  override def width(): Int = s.length
  override def height(): Int = 1
}

class UniformElement (
  ch: Char,
  override val width: Int,
  override val height: Int
) extends Element {
  private val line = ch.toString() * width
  def contents: Array[String] = Array.fill(height)(line)
}
object Spiral {
  val space = Element.elem(" ")
  val corner = Element.elem("+")
  def spiral(nEdges: Int, direction: Int): Element = {
    if (nEdges == 1)
      Element.elem("+")
    else{
      val sp = spiral(nEdges - 1, (direction * 3) % 4)
      def verticalBar = Element.elem('|', 1, sp.height)
      def horizontalBar = Element.elem('-', sp.width, 1)
      if (direction == 1){
        (corner beside horizontalBar) above (sp beside space)
      }else if(direction == 2){
        (space beside sp) above (horizontalBar beside corner)
      }else
         (verticalBar above corner) beside (space above sp)
    }
  }
}
