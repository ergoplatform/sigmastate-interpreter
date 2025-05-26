package sigma

/** Descriptor of a runtime platform. Can be used to conditionally execute code. */
sealed trait RuntimePlatform {
  /** Platform code (unique for each platform) */
  def code: Int
}

object RuntimePlatform {
  /** Represent JVM platform. */
  case object JVM extends RuntimePlatform {
    val code = 1
  }

  /** Represent JS platform. */
  case object JS extends RuntimePlatform {
    val code = 2
  }
}