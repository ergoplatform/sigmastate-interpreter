package sigma.serialization.generators

import org.ergoplatform.validation.ValidationSpecification

trait TransformerGenerators extends ValidationSpecification {
  self: ObjectGenerators with ConcreteCollectionGenerators =>

}
