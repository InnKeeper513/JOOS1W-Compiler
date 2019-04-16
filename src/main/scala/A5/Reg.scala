package A5

object Reg extends Enumeration {
  val eax, ebx, ecx, edx, esi, edi, esp, ebp, eip, al, ax, bl, cl, cx = Value
  override def toString: String = Value.toString
}

