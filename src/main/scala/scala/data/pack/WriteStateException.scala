package scala.data.pack

import Writer._

class WriteStateException(val state: State)
    extends Exception("Invalid state for write operation")
