package domain

case class Grouped[A](
    groupName: String,
    items: List[A]
)
