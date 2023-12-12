object CharExt {
  implicit class CharExt(c: Char) {
    def isHash: Boolean = c == '#'
    def isDot: Boolean = c == '.'
    def isQ: Boolean = c == '?'
  }
}
