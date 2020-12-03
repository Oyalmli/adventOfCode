
data Orb = Planet Orb Orb | Sat String deriving (Eq, Show)

orb1 = Planet (Sat "A") (Sat "B")
orb2 = Planet (Sat "C") orb1
orb3 = Planet (Sat "D") orb2