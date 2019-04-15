
object Reg extends Enumeration {
  val eax, ebx, ecx, edx, esi, edi, esp, ebp, eip, al, ax = Value
  override def toString: String = Value.toString
}
