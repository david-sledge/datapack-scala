package scala.data.pack

class IntMagnitudeTooLargeException (val int: java.math.BigInteger)
    extends Exception(s"The mangintude of value 0x${int.toString(16)} is too large to represent in 64-bits.")
