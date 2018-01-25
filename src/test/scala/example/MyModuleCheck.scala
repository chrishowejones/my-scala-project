package example

import org.scalacheck.Properties
import org.scalacheck.Prop._

object MyModuleCheck extends Properties("MyModule.abs") {

  property("always positive") = forAll { (n: Int) =>
    if (n > Int.MinValue) MyModule.abs(n) >= 0
    else throws(classOf[IllegalArgumentException])(MyModule.abs(n))

  }

}
