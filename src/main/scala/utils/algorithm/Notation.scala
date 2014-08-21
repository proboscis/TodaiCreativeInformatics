package utils.algorithm

/**
 * @author apex
 */
object Notation{
  /**
   * shunting yard algorithm
   * @param src Iterator of characters without spaces or tabs.
   *            use str.replaceAll("""\s+""","").iterator
   * @param priority priority of operators must be specified.
   * @return Seq of Characters in polish notation
   **/
  def InfixToPolish(src:Iterator[Char],priority:Char Map Int = Map('&'->3,'+'->2,'!'->4)):Seq[Char] = {
    val stack = collection.mutable.Stack[Char]()
    val out = collection.mutable.Queue[Char]()
    val tokens = src
    while(tokens.hasNext){
      tokens.next() match {
        case '(' =>stack push '('
        case ')' =>{
          while(stack.top != '('){
            out enqueue stack.pop
          }
          stack.pop()
        }
        case op if priority.contains(op) => {
          while(stack.nonEmpty && stack.top != '(' && priority(stack.top) > priority(op)){
            out enqueue stack.pop()
          }
          stack push op
        }
        case c => out enqueue c
      }
    }
    while(stack.nonEmpty) {
      out enqueue stack.pop
    }
    out
  }
}
