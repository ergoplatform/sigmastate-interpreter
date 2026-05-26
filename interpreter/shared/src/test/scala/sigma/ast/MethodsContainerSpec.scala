package sigma.ast

import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec
import sigma.VersionContext

/** Cross-platform safety net for the JS reflection registry in
  * [[sigma.reflection.ReflectionData]].
  *
  * Most new `SMethod` declarations resolve their Java handler eagerly via
  * `.withIRInfo(MethodCallIrBuilder, javaMethodOf[T, A]("name"))`. On JVM that
  * goes through `java.lang.reflect`; on JS it goes through the hand-written
  * `sigma.reflection` registry. A missing registry entry throws
  * `NoSuchMethodException` the first time the enclosing `lazy val SMethod` is
  * forced — which can be deep in serializer-spec land instead of here.
  *
  * Forcing `container.methods` under every supported `(activatedVersion,
  * ergoTreeVersion)` combination — including the forward-declared
  * [[VersionContext.V7SoftForkVersion]] which sits above
  * `MaxSupportedScriptVersion` — triggers every per-version lazy method table,
  * surfacing missing registry entries here rather than in a downstream consumer.
  */
class MethodsContainerSpec extends AnyPropSpec with Matchers {

  // Range covers V5/V6 era (within MaxSupportedScriptVersion) plus the V7
  // forward-declared version, which CrossVersionProps doesn't reach yet.
  private val versions: Seq[Byte] =
    (0 to VersionContext.V7SoftForkVersion).map(_.toByte)

  private val combinations: Seq[(Byte, Byte)] =
    for {
      activated <- versions
      ergoTree  <- versions if ergoTree <= activated
    } yield (activated, ergoTree)

  property("every MethodsContainer builds its method table on this platform") {
    combinations.foreach { case (activated, ergoTree) =>
      VersionContext.withVersions(activated, ergoTree) {
        val containers =
          if (VersionContext.current.isV3OrLaterErgoTreeVersion) MethodsContainer.methodsV6
          else MethodsContainer.methodsV5

        containers.foreach { container =>
          try container.methods
          catch {
            case t: Throwable =>
              fail(
                s"Failed building method table for ${container.typeName} " +
                  s"under (activated=$activated, ergoTree=$ergoTree): ${t.getMessage}",
                t
              )
          }
        }
      }
    }
  }
}
