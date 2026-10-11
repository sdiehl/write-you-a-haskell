class Marker f where {}
class Marker f => Mapped f where { mapIt :: (a -> b) -> f a -> f b }
main=1
