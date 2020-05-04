package scala.data.pack.stream

import Writer._

class WriteStateException(val state: State)
    extends Exception("Invalid state for write operation")
