package object module1 {
  type TODO

  def TODO: Nothing = throw new Error("Unimplemented")

  implicit class AnySyntax(any: AnyRef) {
    def todo: Nothing = TODO
  }
}
